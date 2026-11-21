#!/usr/bin/env python3
"""Rewrite project version files to one release version.

fpm and CMake ``project(VERSION)`` store ``major.minor.patch``. A prerelease
suffix is kept in Meson, the citation, Sphinx, and the install snippets.
A version lower than ``CHANGELOG.md``, or equal to it, is refused.
``--annotate-tag`` turns a lightweight ``v<version>`` tag into an annotated tag.
"""

from __future__ import annotations

import argparse
import datetime as dt
import os
import re
import signal
import subprocess
import sys
from pathlib import Path

from version_util import latest_changelog_version, numeric_core, version_sort_key


ROOT = Path(__file__).resolve().parent.parent
VERSION_PATTERN = re.compile(r"^\d+\.\d+\.\d+(?:[-+][0-9A-Za-z.-]+)?$")


def replace_one(path: Path, pattern: str, replacement: str) -> None:
    text = path.read_text(encoding="utf-8")
    updated, count = re.subn(pattern, replacement, text, count=1, flags=re.MULTILINE)
    if count != 1:
        raise ValueError(f"{path.relative_to(ROOT)}: pattern did not match once: {pattern}")
    path.write_text(updated, encoding="utf-8")


def _git_text(args: list[str]) -> str:
    result = subprocess.run(
        ["git", *args],
        cwd=ROOT,
        check=False,
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        return ""
    return result.stdout


def _resolve_git_path(name: str) -> Path | None:
    """Locate a git file from this checkout, including a linked worktree."""
    git_path = _git_text(["rev-parse", "--git-path", name]).strip()
    common = _git_text(["rev-parse", "--git-common-dir"]).strip()
    if not git_path and not common:
        return None
    if git_path:
        path = Path(git_path)
        if not path.is_absolute():
            path = (ROOT / path).resolve()
        return path
    common_path = Path(common)
    if not common_path.is_absolute():
        common_path = (ROOT / common_path).resolve()
    return common_path / name


def _ps_fields(pid: int) -> tuple[int, str, str] | None:
    try:
        result = subprocess.run(
            ["ps", "-o", "ppid=", "-o", "stat=", "-o", "comm=", "-p", str(pid)],
            check=False,
            capture_output=True,
            text=True,
        )
    except OSError:
        return None
    if result.returncode != 0:
        return None
    parts = result.stdout.strip().split(None, 2)
    if len(parts) < 3:
        return None
    try:
        parent = int(parts[0])
    except ValueError:
        return None
    return parent, parts[1], Path(parts[2]).name


def _proc_fields(pid: int) -> tuple[int, str, str] | None:
    """Parent and command name from ``/proc`` when ``ps`` is missing."""
    try:
        text = Path(f"/proc/{pid}/status").read_text(encoding="utf-8", errors="replace")
    except OSError:
        return None
    parent = None
    name = None
    for line in text.splitlines():
        if line.startswith("PPid:"):
            try:
                parent = int(line.split()[1])
            except (IndexError, ValueError):
                return None
        elif line.startswith("Name:"):
            parts = line.split(None, 1)
            if len(parts) == 2:
                name = parts[1].strip()
    if parent is None or not name:
        return None
    return parent, "?", name


def _process_fields(pid: int) -> tuple[int, str, str] | None:
    fields = _ps_fields(pid)
    if fields is not None:
        return fields
    return _proc_fields(pid)


def _find_cog_pid() -> int | None:
    pid = os.getpid()
    for _ in range(8):
        fields = _process_fields(pid)
        if fields is None:
            return None
        parent, _state, name = fields
        if name == "cog":
            return pid
        if parent <= 1 or parent == pid:
            return None
        pid = parent
    return None


def stash_entries() -> list[tuple[str, str, str]]:
    """Return ``(commit, selector, subject)`` for each stash.

    The commit hash is the stash's own ref. The selector is only how
    ``git stash drop`` names that commit, and it is re-read after each drop.
    """
    entries: list[tuple[str, str, str]] = []
    for line in _git_text(["stash", "list", "--format=%H%x09%gd%x09%gs"]).splitlines():
        parts = line.split("\t", 2)
        if len(parts) != 3 or not parts[0] or not parts[1]:
            continue
        entries.append((parts[0], parts[1], parts[2]))
    return entries


def stash_hashes() -> set[str]:
    return {commit for commit, _ref, _subject in stash_entries()}


def _drop_stash_ref(ref: str) -> bool:
    result = subprocess.run(
        ["git", "stash", "drop", "--quiet", ref],
        cwd=ROOT,
        check=False,
        capture_output=True,
        text=True,
    )
    return result.returncode == 0


def _drop_new_cog_stashes(before: set[str]) -> bool:
    """Drop a new ``cog_bump_*`` stash selected by its commit hash."""
    dropped = False
    while True:
        target = None
        for commit, ref, subject in stash_entries():
            if commit in before or "cog_bump_" not in subject:
                continue
            target = ref
            break
        if target is None:
            return dropped
        if not _drop_stash_ref(target):
            return dropped
        dropped = True


def _drop_resolved_cog_stash(before: set[str]) -> bool:
    """Remove a new cog stash commit from the resolved reflog.

    ``before`` is the set of stash commit hashes recorded before cog ran.
    """
    log_path = _resolve_git_path("logs/refs/stash")
    ref_path = _resolve_git_path("refs/stash")
    if log_path is None or not log_path.exists():
        return False
    try:
        lines = log_path.read_text(encoding="utf-8", errors="replace").splitlines()
    except OSError:
        return False
    kept: list[str] = []
    removed = False
    for line in lines:
        body, tab, message = line.partition("\t")
        shas = body.split()
        commit = shas[1] if len(shas) > 1 else ""
        if tab and commit and commit not in before and "cog_bump_" in message:
            removed = True
            continue
        kept.append(line)
    if not removed:
        return False
    try:
        if kept:
            tmp = log_path.with_name(log_path.name + ".tmp")
            tmp.write_text("\n".join(kept) + "\n", encoding="utf-8")
            os.replace(tmp, log_path)
            if ref_path is not None:
                ref_path.write_text(kept[-1].split()[1] + "\n", encoding="utf-8")
        else:
            if ref_path is not None:
                ref_path.unlink(missing_ok=True)
            log_path.unlink(missing_ok=True)
    except OSError:
        return False
    return True


def drop_new_cog_stashes(before: set[str]) -> bool:
    """Drop ``cog_bump_*`` stashes whose commit hash is absent from ``before``.

    Cog has already exited when this runs. A leftover stash with the same
    message stays, because its commit hash was already recorded. Paths come
    from ``git rev-parse`` so a linked worktree updates the common git dir.
    """
    if _drop_new_cog_stashes(before):
        return True
    return _drop_resolved_cog_stash(before)


def annotate_release_tag(version: str) -> int:
    if VERSION_PATTERN.fullmatch(version) is None:
        print(
            "bump_versions.py: version must be a semantic version",
            file=sys.stderr,
        )
        return 2
    tag = f"v{version}"
    kind = subprocess.run(
        ["git", "cat-file", "-t", tag],
        cwd=ROOT,
        check=False,
        capture_output=True,
        text=True,
    )
    if kind.returncode != 0:
        print(f"bump_versions.py: {tag} is missing", file=sys.stderr)
        return 1
    found = kind.stdout.strip()
    if found == "tag":
        return 0
    if found != "commit":
        print(f"bump_versions.py: {tag} is a {found}", file=sys.stderr)
        return 1
    commit = subprocess.run(
        ["git", "rev-parse", "--verify", f"{tag}^{{commit}}"],
        cwd=ROOT,
        check=False,
        capture_output=True,
        text=True,
    )
    if commit.returncode != 0:
        print(f"bump_versions.py: {tag} has no commit", file=sys.stderr)
        return 1
    annotated = subprocess.run(
        [
            "git",
            "tag",
            "-a",
            "-f",
            tag,
            commit.stdout.strip(),
            "-m",
            tag,
            "--no-sign",
        ],
        cwd=ROOT,
        check=False,
    )
    if annotated.returncode != 0:
        return annotated.returncode
    checked = subprocess.run(
        ["git", "cat-file", "-t", tag],
        cwd=ROOT,
        check=False,
        capture_output=True,
        text=True,
    )
    if checked.returncode != 0 or checked.stdout.strip() != "tag":
        print(f"bump_versions.py: {tag} stayed lightweight", file=sys.stderr)
        return 1
    return 0


def refuse(message: str) -> int:
    print(f"bump_versions.py: {message}", file=sys.stderr)
    print(
        "bump_versions.py: no cog_bump stash is created.",
        file=sys.stderr,
    )
    # `ps` may be missing. `/proc` still names cog so the signal lands
    # before cog can stash or print a stash hint.
    cog = _find_cog_pid()
    if cog is not None:
        try:
            os.kill(cog, signal.SIGTERM)
        except OSError:
            pass
    return 1


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("version", nargs="?")
    parser.add_argument("--annotate-tag", action="store_true")
    parser.add_argument("--date", default=dt.date.today().isoformat())
    args = parser.parse_args()
    if args.annotate_tag:
        if not args.version:
            print(
                "bump_versions.py: annotate-tag needs a version",
                file=sys.stderr,
            )
            return 2
        return annotate_release_tag(args.version)
    if not args.version:
        print("bump_versions.py: version is required", file=sys.stderr)
        return 2
    if VERSION_PATTERN.fullmatch(args.version) is None:
        print(
            "bump_versions.py: version must be a semantic version",
            file=sys.stderr,
        )
        return 2
    if re.fullmatch(r"\d{4}-\d{2}-\d{2}", args.date) is None:
        print("bump_versions.py: date must be YYYY-MM-DD", file=sys.stderr)
        return 2
    try:
        requested = version_sort_key(args.version)
        changelog = (ROOT / "CHANGELOG.md").read_text(encoding="utf-8")
        latest = latest_changelog_version(changelog)
        if latest is not None:
            current = version_sort_key(latest)
            if requested < current:
                return refuse(
                    f"{args.version} is lower than {latest} already in CHANGELOG.md"
                )
            if requested == current:
                return refuse(
                    f"{args.version} is already in CHANGELOG.md. "
                    "Tag it with `git tag -a v"
                    f"{args.version}` after the pull request checks are green, "
                    "then `git push --follow-tags`."
                )
    except (OSError, ValueError) as error:
        print(f"bump_versions.py: {error}", file=sys.stderr)
        return 1
    core = numeric_core(args.version)
    tag = f"v{args.version}"
    try:
        replace_one(
            ROOT / "fpm.toml",
            r'^version = "[^"]*"',
            f'version = "{core}"',
        )
        replace_one(
            ROOT / "meson.build",
            r"^  version: '[^']*'",
            f"  version: '{args.version}'",
        )
        replace_one(
            ROOT / "CMakeLists.txt",
            r"^  VERSION \d+\.\d+\.\d+\S*",
            f"  VERSION {core}",
        )
        replace_one(
            ROOT / "CMakeLists.txt",
            r"^#     GIT_TAG        v\S+",
            f"#     GIT_TAG        {tag}",
        )
        replace_one(
            ROOT / "CITATION.cff",
            r"^version: \S+",
            f"version: {args.version}",
        )
        replace_one(
            ROOT / "CITATION.cff",
            r'^date-released: "\d{4}-\d{2}-\d{2}"',
            f'date-released: "{args.date}"',
        )
        replace_one(
            ROOT / "docs/source/conf.py",
            r'^release = "[^"]*"',
            f'release = "{args.version}"',
        )
        replace_one(
            ROOT / "README.md",
            r'tag = "v[^"]*"',
            f'tag = "{tag}"',
        )
        replace_one(
            ROOT / "README.md",
            r"^  GIT_TAG        v\S+",
            f"  GIT_TAG        {tag}",
        )
        replace_one(
            ROOT / "README.md",
            r"^revision = v\S+",
            f"revision = {tag}",
        )
        replace_one(
            ROOT / "docs/orgmode/install.org",
            r"^  GIT_TAG        v\S+",
            f"  GIT_TAG        {tag}",
        )
    except (OSError, ValueError) as error:
        print(f"bump_versions.py: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
