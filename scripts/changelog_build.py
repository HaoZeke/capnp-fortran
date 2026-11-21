#!/usr/bin/env python3
"""Build CHANGELOG.md from fragments at the fpm version.

``towncrier.toml`` has no version field. This command reads ``fpm.toml``
and passes the changelog version to towncrier. fpm stores
``major.minor.patch`` when the changelog version has a prerelease suffix.
The new section's link is a compare link with the other links at the
bottom of ``CHANGELOG.md``. A version already in the file is not rebuilt.
"""

from __future__ import annotations

import argparse
import datetime as dt
import re
import subprocess
import sys
import tomllib
from pathlib import Path

from version_util import latest_changelog_version, numeric_core, version_sort_key


ROOT = Path(__file__).resolve().parent.parent
REPO = "https://github.com/HaoZeke/capnp-fortran"
HEADING = re.compile(r"^## \[([^]]+)\]", re.MULTILINE)


def fpm_version() -> str:
    data = tomllib.loads((ROOT / "fpm.toml").read_text(encoding="utf-8"))
    version = data.get("version")
    if not isinstance(version, str) or not version:
        raise ValueError("fpm.toml has no version")
    return version


def news_fragments() -> list[Path]:
    folder = ROOT / "docs" / "newsfragments"
    return sorted(
        path for path in folder.glob("*.md") if path.name != "README.md"
    )


def section_exists(text: str, version: str) -> bool:
    return re.search(rf"^## \[{re.escape(version)}\]", text, re.MULTILINE) is not None


def next_draft_version(version: str, text: str) -> str:
    """Heading for a draft whose requested section is already recorded."""
    core = numeric_core(version)
    if core != version and not section_exists(text, core):
        return core
    major, minor, patch = (int(part) for part in core.split("."))
    return f"{major}.{minor}.{patch + 1}"


def previous_version(text: str, version: str) -> str | None:
    """Heading a compare link should name.

    A final release skips prerelease headings and links the previous
    final tag. A prerelease keeps the nearest earlier heading.
    """
    final = numeric_core(version) == version
    for found in HEADING.findall(text):
        if found in {"Unreleased", version}:
            continue
        if final and numeric_core(found) != found:
            continue
        return found
    return None


def compare_link(version: str, previous: str | None) -> str:
    if previous is None:
        return f"[{version}]: {REPO}/releases/tag/v{version}"
    return f"[{version}]: {REPO}/compare/v{previous}...v{version}"


def strip_tree_link(text: str, version: str) -> str:
    pattern = re.compile(
        rf"^\[{re.escape(version)}\]: {re.escape(REPO)}/tree/v{re.escape(version)}\n?",
        re.MULTILINE,
    )
    return pattern.sub("", text)


def insert_compare_link(text: str, link: str) -> str:
    if link in text:
        return text
    marker = re.search(
        rf"^(\[[^\]]+\]: {re.escape(REPO)}/)",
        text,
        re.MULTILINE,
    )
    if marker:
        return text[: marker.start()] + link + "\n" + text[marker.start() :]
    suffix = "" if text.endswith("\n") else "\n"
    return text + suffix + "\n" + link + "\n"


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--version", help="version to build; default is fpm.toml")
    parser.add_argument("--date", help="release date, YYYY-MM-DD")
    parser.add_argument("--draft", action="store_true")
    args = parser.parse_args()
    try:
        package_version = fpm_version()
    except (OSError, ValueError, tomllib.TOMLDecodeError) as error:
        print(f"changelog_build.py: {error}", file=sys.stderr)
        return 1
    version = args.version or package_version
    release_date = args.date or dt.date.today().isoformat()
    changelog = ROOT / "CHANGELOG.md"
    text = changelog.read_text(encoding="utf-8")
    latest = latest_changelog_version(text)
    if latest is not None:
        try:
            if version_sort_key(version) < version_sort_key(latest):
                print(
                    f"changelog_build.py: {version} is lower than {latest} "
                    "already in CHANGELOG.md",
                    file=sys.stderr,
                )
                return 1
        except ValueError as error:
            print(f"changelog_build.py: {error}", file=sys.stderr)
            return 1
    core = numeric_core(version)
    if package_version != core:
        print(
            f"fpm.toml is {package_version}, expected {core}",
            file=sys.stderr,
        )
        return 1
    if args.draft and not news_fragments():
        print("No news fragments are pending.")
        return 0
    if args.draft and section_exists(text, version):
        version = next_draft_version(version, text)
    if section_exists(text, version) and not args.draft:
        fragments = news_fragments()
        if fragments:
            names = ", ".join(path.name for path in fragments)
            print(
                f"changelog_build.py: {version} is already in CHANGELOG.md; "
                f"refusing to rebuild ({names})",
                file=sys.stderr,
            )
            return 1
        return 0
    previous = previous_version(text, version)
    link = compare_link(version, previous)
    command = [
        sys.executable,
        "-m",
        "towncrier",
        "build",
        "--version",
        version,
        "--date",
        release_date,
        "--config",
        str(ROOT / "towncrier.toml"),
        (
            "--draft"
            if args.draft
            else "--keep"
            if numeric_core(version) != version
            else "--yes"
        ),
    ]
    result = subprocess.run(command, cwd=ROOT, check=False)
    if result.returncode != 0:
        return result.returncode
    if args.draft:
        print(link)
        return 0
    text = changelog.read_text(encoding="utf-8")
    updated = insert_compare_link(strip_tree_link(text, version), link)
    tree_line = f"{REPO}/tree/v{version}"
    if tree_line in updated:
        print(
            f"changelog_build.py: refused tree link for {version}",
            file=sys.stderr,
        )
        return 1
    changelog.write_text(updated, encoding="utf-8")
    # Towncrier stages the changelog before this link is inserted. Stage the
    # updated file when this directory is a git worktree so the bump commit
    # contains the compare link.
    probe = subprocess.run(
        ["git", "rev-parse", "--is-inside-work-tree"],
        cwd=ROOT,
        check=False,
        capture_output=True,
        text=True,
    )
    if probe.returncode == 0 and probe.stdout.strip() == "true":
        added = subprocess.run(
            ["git", "add", "--", "CHANGELOG.md"],
            cwd=ROOT,
            check=False,
        )
        if added.returncode != 0:
            return added.returncode
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
