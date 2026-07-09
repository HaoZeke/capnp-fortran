#!/usr/bin/env bash
# Build and run the fpm test suite with gfortran coverage, then summarize
# line coverage for src/ via matching gcov + a small aggregator.
#
# fpm renames objects to src_*.f90 under build/gfortran_*/capnp/; gcovr's
# automatic discovery often mis-attributes those counters. We run gcov on
# each src_*.gcda in that directory and parse the reports.
#
# Usage (repo root):
#   bash scripts/coverage.sh
#   pixi run -e coverage coverage
#
# Outputs under coverage-report/ (or $COVERAGE_OUT_DIR):
#   summary.txt, coverage.xml (Cobertura), index.html, console.txt
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

OUT_DIR="${COVERAGE_OUT_DIR:-coverage-report}"
rm -rf "$OUT_DIR"
mkdir -p "$OUT_DIR"

GCOV_BIN="$(gfortran -print-prog-name=gcov 2>/dev/null || true)"
if [[ -z "${GCOV_BIN}" ]]; then
  GCOV_BIN=gcov
fi

echo "==> clean build tree"
rm -rf build

echo "==> fpm test with --coverage (gcov=$GCOV_BIN)"
fpm test --flag "--coverage -O0 -g" --link-flag "--coverage" "$@"

SAMPLE_GCDA="$(find build -name 'src_*.gcda' 2>/dev/null | head -1 || true)"
if [[ -z "${SAMPLE_GCDA}" ]]; then
  echo "error: no src_*.gcda under build/ after instrumented test" >&2
  exit 1
fi
OBJ_DIR="$(dirname "$SAMPLE_GCDA")"
GCDA_N="$(find "$OBJ_DIR" -name 'src_*.gcda' | wc -l | tr -d ' ')"
echo "==> object dir $OBJ_DIR ($GCDA_N src_*.gcda)"

GCOV_WORK="$(cd "$OUT_DIR" && pwd)/gcov-work"
rm -rf "$GCOV_WORK"
mkdir -p "$GCOV_WORK"

echo "==> gcov on src_*.gcda"
(
  cd "$OBJ_DIR"
  for g in src_*.gcda; do
    # write .gcov next to objects; logs go to absolute GCOV_WORK
    "$GCOV_BIN" -o . "$g" >"$GCOV_WORK/${g%.gcda}.log" 2>&1 || true
  done
  shopt -s nullglob
  for f in *.gcov; do
    if head -1 "$f" | grep -q "Source:src/"; then
      cp "$f" "$GCOV_WORK/"
    fi
  done
)

python3 - "$ROOT" "$OUT_DIR" "$GCOV_WORK" <<'PY'
"""Aggregate gcov text logs + .gcov files into summary / Cobertura / HTML."""
from __future__ import annotations

import html
import re
import sys
from pathlib import Path

root, out_dir, work = map(Path, sys.argv[1:4])
rows: list[tuple[str, int, int, float]] = []

# Prefer structured "Lines executed" lines from gcov logs.
log_re = re.compile(
    r"File '([^']+)'\s*\nLines executed:([0-9.]+)% of ([0-9]+)",
    re.M,
)
for log in sorted(work.glob("*.log")):
    text = log.read_text(errors="replace")
    for m in log_re.finditer(text):
        path, pct_s, total_s = m.group(1), m.group(2), m.group(3)
        if not path.startswith("src/"):
            continue
        total = int(total_s)
        pct = float(pct_s)
        executed = int(round(total * pct / 100.0))
        rows.append((path, total, executed, pct))

# Dedup by path (keep max executed).
by_path: dict[str, tuple[int, int, float]] = {}
for path, total, executed, pct in rows:
    prev = by_path.get(path)
    if prev is None or executed > prev[1]:
        by_path[path] = (total, executed, pct)

rows2 = [(p, *by_path[p]) for p in sorted(by_path)]
if not rows2:
    sys.stderr.write("error: no src/ coverage lines parsed from gcov\n")
    sys.exit(1)

tot_lines = sum(t for _, t, _, _ in rows2)
tot_exec = sum(e for _, _, e, _ in rows2)
tot_pct = 100.0 * tot_exec / tot_lines if tot_lines else 0.0

# summary.txt (lcov-ish table)
lines = [
    "GCC Code Coverage Report (src/ via fpm + gcov)",
    f"Directory: {root}",
    "File                                       Lines    Exec  Cover",
    "-" * 70,
]
for path, total, executed, pct in rows2:
    lines.append(f"{path:42} {total:6d} {executed:6d} {pct:6.1f}%")
lines.append("-" * 70)
lines.append(f"{'TOTAL':42} {tot_lines:6d} {tot_exec:6d} {tot_pct:6.1f}%")
lines.append("")
summary = "\n".join(lines)
(out_dir / "summary.txt").write_text(summary)
(out_dir / "console.txt").write_text(summary)
print(summary)

# Cobertura XML (minimal)
pkgs = {}
for path, total, executed, pct in rows2:
    pkgs[path] = (total, executed, pct)
xml = [
    '<?xml version="1.0" ?>',
    f'<coverage line-rate="{tot_pct/100:.4f}" branch-rate="0" version="gcov" timestamp="0">',
    "  <sources><source>.</source></sources>",
    '  <packages>',
    f'    <package name="src" line-rate="{tot_pct/100:.4f}" branch-rate="0" complexity="0">',
    "      <classes>",
]
for path, total, executed, pct in rows2:
    cname = path.replace("/", ".")
    xml.append(
        f'        <class name="{html.escape(cname)}" filename="{html.escape(path)}" '
        f'line-rate="{pct/100:.4f}" branch-rate="0" complexity="0">'
    )
    xml.append("          <methods/>")
    xml.append("          <lines/>")
    xml.append("        </class>")
xml += [
    "      </classes>",
    "    </package>",
    "  </packages>",
    "</coverage>",
    "",
]
(out_dir / "coverage.xml").write_text("\n".join(xml))

# Simple HTML index
body = ["<html><head><title>capnp-fortran coverage</title></head><body>",
        "<h1>capnp-fortran src/ coverage</h1>",
        f"<p><b>TOTAL</b>: {tot_exec}/{tot_lines} lines ({tot_pct:.1f}%)</p>",
        "<table border='1' cellpadding='4'><tr><th>File</th><th>Lines</th><th>Exec</th><th>Cover</th></tr>"]
for path, total, executed, pct in rows2:
    body.append(
        f"<tr><td>{html.escape(path)}</td><td>{total}</td><td>{executed}</td><td>{pct:.1f}%</td></tr>"
    )
body.append("</table></body></html>")
(out_dir / "index.html").write_text("\n".join(body))
print(f"==> wrote {out_dir}/ ({len(rows2)} src files, {tot_pct:.1f}% lines)")
PY

echo "==> wrote $OUT_DIR/"
