"""Semantic version helpers for the release scripts.

fpm and CMake ``project(VERSION)`` store ``major.minor.patch``. A prerelease
suffix stays on the git tag, in the changelog, and in Meson, the citation,
and Sphinx.
"""

from __future__ import annotations

import re


SEMVER_PATTERN = re.compile(
    r"^(?P<major>\d+)\.(?P<minor>\d+)\.(?P<patch>\d+)"
    r"(?:-(?P<pre>[0-9A-Za-z.-]+))?"
    r"(?:\+(?P<build>[0-9A-Za-z.-]+))?$"
)
HEADING = re.compile(r"^## \[([^]]+)\]", re.MULTILINE)


def numeric_core(version: str) -> str:
    """Return ``major.minor.patch``, dropping a prerelease or build suffix."""
    return re.split(r"[-+]", version, maxsplit=1)[0]


def version_sort_key(version: str) -> tuple:
    """Sort key using Semantic Versioning precedence. Build metadata is ignored."""
    match = SEMVER_PATTERN.fullmatch(version)
    if match is None:
        raise ValueError(f"not a semantic version: {version}")
    numbers = (
        int(match.group("major")),
        int(match.group("minor")),
        int(match.group("patch")),
    )
    pre = match.group("pre")
    if pre is None:
        return (numbers, 1)
    idents: list[tuple[int, int | str]] = []
    for ident in pre.split("."):
        if ident.isdigit():
            idents.append((0, int(ident)))
        else:
            idents.append((1, ident))
    return (numbers, 0, tuple(idents))


def latest_changelog_version(text: str) -> str | None:
    """Return the first released heading in ``CHANGELOG.md`` text."""
    for found in HEADING.findall(text):
        if found != "Unreleased":
            return found
    return None
