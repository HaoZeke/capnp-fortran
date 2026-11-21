#!/usr/bin/env python3
"""Write the CHANGELOG.md section for a release version."""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parent.parent


def notes_for(version: str) -> str:
    text = (ROOT / "CHANGELOG.md").read_text(encoding="utf-8")
    section = re.search(
        rf"^## \[{re.escape(version)}\] - .*?$",
        text,
        re.MULTILINE,
    )
    if section is None:
        raise ValueError(f"CHANGELOG.md has no section for {version}")
    remainder = text[section.end() :]
    next_section = re.search(r"^## \[", remainder, re.MULTILINE)
    notes = remainder[: next_section.start()] if next_section else remainder
    notes = "\n".join(
        line
        for line in notes.strip().splitlines()
        if not re.match(r"^\[[^\]]+\]: https?://", line)
    )
    return notes.strip() + "\n"


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("version")
    parser.add_argument("-o", "--output", type=Path)
    args = parser.parse_args()
    try:
        notes = notes_for(args.version)
    except ValueError as error:
        print(error, file=sys.stderr)
        return 1
    if args.output is None:
        sys.stdout.write(notes)
    else:
        args.output.write_text(notes, encoding="utf-8")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
