#!/usr/bin/env python3
"""Check that project metadata matches a release tag.

fpm and CMake ``project(VERSION)`` are compared to ``major.minor.patch``.
Meson, the citation, and Sphinx keep a prerelease suffix. The README
install pins, the CMake fetch example, and ``docs/orgmode/install.org``
use the full tag.
"""

from __future__ import annotations

import argparse
import re
import sys
import tomllib
from pathlib import Path

from version_util import numeric_core


ROOT = Path(__file__).resolve().parent.parent
VERSION_PATTERN = re.compile(
    r"^v(?P<version>\d+\.\d+\.\d+(?:[-+][0-9A-Za-z.-]+)?)$"
)


def metadata_versions() -> dict[str, str]:
    fpm = tomllib.loads((ROOT / "fpm.toml").read_text(encoding="utf-8"))
    meson_text = (ROOT / "meson.build").read_text(encoding="utf-8")
    cmake_text = (ROOT / "CMakeLists.txt").read_text(encoding="utf-8")
    citation_text = (ROOT / "CITATION.cff").read_text(encoding="utf-8")
    sphinx_text = (ROOT / "docs/source/conf.py").read_text(encoding="utf-8")
    readme_text = (ROOT / "README.md").read_text(encoding="utf-8")
    install_text = (ROOT / "docs/orgmode/install.org").read_text(encoding="utf-8")
    meson = re.search(r"^\s*version:\s*'([^']+)'", meson_text, re.MULTILINE)
    cmake = re.search(
        r"project\(\s*capnp_fortran.*?^\s*VERSION\s+([^\s)]+)",
        cmake_text,
        re.MULTILINE | re.DOTALL,
    )
    citation = re.search(r"^version:\s*(\S+)", citation_text, re.MULTILINE)
    sphinx = re.search(r'^release\s*=\s*"([^"]+)"', sphinx_text, re.MULTILINE)
    readme_fpm = re.search(r'tag = "(v[^"]+)"', readme_text)
    readme_meson = re.search(r"^revision = (v\S+)", readme_text, re.MULTILINE)
    readme_cmake = re.search(r"^  GIT_TAG\s+(v\S+)", readme_text, re.MULTILINE)
    cmake_example = re.search(r"^#\s+GIT_TAG\s+(v\S+)", cmake_text, re.MULTILINE)
    install_cmake = re.search(r"^\s+GIT_TAG\s+(v\S+)", install_text, re.MULTILINE)
    found = {
        "fpm": fpm.get("version"),
        "meson": None if meson is None else meson.group(1),
        "cmake": None if cmake is None else cmake.group(1),
        "citation": None if citation is None else citation.group(1),
        "sphinx": None if sphinx is None else sphinx.group(1),
        "readme_fpm": None if readme_fpm is None else readme_fpm.group(1),
        "readme_meson": None if readme_meson is None else readme_meson.group(1),
        "readme_cmake": None if readme_cmake is None else readme_cmake.group(1),
        "cmake_example": None if cmake_example is None else cmake_example.group(1),
        "install_cmake": None if install_cmake is None else install_cmake.group(1),
    }
    missing = [name for name, version in found.items() if not version]
    if missing:
        raise ValueError("could not read " + ", ".join(missing))
    return {name: str(version) for name, version in found.items()}


def expected_versions(version: str) -> dict[str, str]:
    core = numeric_core(version)
    tag = f"v{version}"
    return {
        "fpm": core,
        "meson": version,
        "cmake": core,
        "citation": version,
        "sphinx": version,
        "readme_fpm": tag,
        "readme_meson": tag,
        "readme_cmake": tag,
        "cmake_example": tag,
        "install_cmake": tag,
    }


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("tag", help="release tag, for example v0.4.0")
    args = parser.parse_args()
    match = VERSION_PATTERN.fullmatch(args.tag)
    if match is None:
        parser.error("tag must begin with v and contain a semantic version")
    version = match.group("version")
    try:
        versions = metadata_versions()
    except (KeyError, ValueError, tomllib.TOMLDecodeError) as error:
        print(f"version metadata error: {error}", file=sys.stderr)
        return 1
    expected = expected_versions(version)
    mismatches = [
        f"{name} is {versions[name]}, expected {expected[name]}"
        for name in versions
        if versions[name] != expected[name]
    ]
    if mismatches:
        print("\n".join(mismatches))
    for name, found in versions.items():
        print(f"{name}={found}")
    if mismatches:
        return 1
    print(f"tag={args.tag}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
