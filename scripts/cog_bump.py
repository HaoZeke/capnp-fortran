#!/usr/bin/env python3
"""Run ``cog bump`` after the changelog allows the version.

A version already recorded in ``CHANGELOG.md``, or lower than that
version, is refused here. Cocogitto is not started, so it does not
create a stash and does not print a stash hint.
"""

from __future__ import annotations

import subprocess
import sys
from pathlib import Path

from bump_versions import drop_new_cog_stashes, stash_hashes
from version_util import latest_changelog_version, version_sort_key


ROOT = Path(__file__).resolve().parent.parent


def _version_from_args(args: list[str]) -> str | None:
    for index, arg in enumerate(args):
        if arg == "--version" and index + 1 < len(args):
            return args[index + 1].removeprefix("v")
        if arg.startswith("--version="):
            return arg.split("=", 1)[1].removeprefix("v")
    return None


def _refusal(version: str) -> str | None:
    changelog = (ROOT / "CHANGELOG.md").read_text(encoding="utf-8")
    latest = latest_changelog_version(changelog)
    if latest is None:
        return None
    requested = version_sort_key(version)
    current = version_sort_key(latest)
    if requested < current:
        return f"{version} is lower than {latest} already in CHANGELOG.md"
    if requested == current:
        return (
            f"{version} is already in CHANGELOG.md. "
            f"Tag it with `git tag -a v{version}` after the pull request "
            "checks are green, then `git push --follow-tags`."
        )
    return None


def _refuse(message: str) -> int:
    print(f"cog_bump.py: {message}", file=sys.stderr)
    print("cog_bump.py: cog was not started, so no stash was created.", file=sys.stderr)
    return 1


def _missing_cog(error: OSError) -> int:
    print(f"cog_bump.py: cog is not available ({error})", file=sys.stderr)
    return 1


def _run_cog(args: list[str]) -> subprocess.CompletedProcess[str] | None:
    try:
        return subprocess.run(
            ["cog", "bump", *args],
            cwd=ROOT,
            check=False,
            capture_output=True,
            text=True,
        )
    except OSError as error:
        _missing_cog(error)
        return None


def _stash_hint(line: str) -> bool:
    return "stash@" in line or "been stashed" in line


def _emit(result: subprocess.CompletedProcess[str], dropped: bool) -> int:
    sys.stdout.write(result.stdout)
    if dropped:
        kept = [line for line in result.stderr.splitlines(keepends=True) if not _stash_hint(line)]
        sys.stderr.write("".join(kept))
        print("cog_bump.py: dropped the cog_bump stash.", file=sys.stderr)
    else:
        sys.stderr.write(result.stderr)
    return result.returncode


def _dry_run_version(args: list[str]) -> tuple[int, str | None]:
    result = _run_cog(["--dry-run", *args])
    if result is None:
        return 1, None
    text = result.stdout.strip()
    if text.startswith("v") and " " not in text:
        return result.returncode, text.removeprefix("v")
    sys.stdout.write(result.stdout)
    sys.stderr.write(result.stderr)
    return result.returncode, None


def main() -> int:
    args = sys.argv[1:]
    version = _version_from_args(args)
    if version is None and "--dry-run" not in args:
        code, version = _dry_run_version(args)
        if version is None:
            return code
    if version is not None:
        try:
            message = _refusal(version)
        except (OSError, ValueError) as error:
            print(f"cog_bump.py: {error}", file=sys.stderr)
            return 1
        if message is not None:
            return _refuse(message)
    if "--dry-run" in args:
        result = _run_cog(args)
        if result is None:
            return 1
        return _emit(result, False)
    before = stash_hashes()
    result = _run_cog(args)
    if result is None:
        return 1
    dropped = drop_new_cog_stashes(before)
    return _emit(result, dropped)


if __name__ == "__main__":
    raise SystemExit(main())
