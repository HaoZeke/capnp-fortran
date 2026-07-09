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
#   summary.txt, coverage.lcov (Codecov), coverage.xml (Cobertura),
#   index.html, console.txt
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

OUT_DIR="${COVERAGE_OUT_DIR:-coverage-report}"
rm -rf "$OUT_DIR"
mkdir -p "$OUT_DIR"

# Match gcov to the same toolchain as fpm (pixi env when run via pixi run).
# A system gcov newer than conda-forge gfortran yields empty .gcov bodies.
GFORTRAN_BIN="$(command -v gfortran)"
GCOV_BIN="$(dirname "$GFORTRAN_BIN")/gcov"
if [[ ! -x "$GCOV_BIN" ]]; then
  GCOV_BIN="$(gfortran -print-prog-name=gcov 2>/dev/null || true)"
fi
if [[ -z "${GCOV_BIN}" || ! -x "${GCOV_BIN}" ]]; then
  GCOV_BIN="$(command -v gcov || echo gcov)"
fi

echo "==> clean build tree"
rm -rf build

echo "==> fpm test with --coverage (gfortran=$GFORTRAN_BIN gcov=$GCOV_BIN)"
fpm test --flag "--coverage -O0 -g" --link-flag "--coverage" "$@"

SAMPLE_GCDA="$(find build -name 'src_*.gcda' 2>/dev/null | head -1 || true)"
if [[ -z "${SAMPLE_GCDA}" ]]; then
  echo "error: no src_*.gcda under build/ after instrumented test" >&2
  exit 1
fi
OBJ_DIR="$(dirname "$SAMPLE_GCDA")"
GCDA_N="$(find "$OBJ_DIR" -name 'src_*.gcda' | wc -l | tr -d ' ')"
echo "==> object dir $OBJ_DIR ($GCDA_N src_*.gcda)"

# gcov resolves Source:src/foo.f90 relative to the object directory.
mkdir -p "$OBJ_DIR/src"
for f in "$ROOT"/src/*.f90; do
  ln -sfn "$f" "$OBJ_DIR/src/$(basename "$f")"
done

GCOV_WORK="$(cd "$OUT_DIR" && pwd)/gcov-work"
rm -rf "$GCOV_WORK"
mkdir -p "$GCOV_WORK"

echo "==> gcov on src_*.gcda"
(
  cd "$OBJ_DIR"
  for g in src_*.gcda; do
    "$GCOV_BIN" -o . "$g" >"$GCOV_WORK/${g%.gcda}.log" 2>&1 || true
  done
  shopt -s nullglob
  for f in *.gcov; do
    if grep -q "Source:src/" "$f" 2>/dev/null && [[ "$(wc -l < "$f")" -gt 10 ]]; then
      cp "$f" "$GCOV_WORK/"
    fi
  done
)

python3 - "$ROOT" "$OUT_DIR" "$GCOV_WORK" <<'PY'
"""Aggregate .gcov files into summary, LCOV, Cobertura (per-line), HTML."""
from __future__ import annotations

import html
import re
import sys
import time
from pathlib import Path

root, out_dir, work = map(Path, sys.argv[1:4])

# Parse each .gcov: map source path -> {line_no: hits}
# gcov line forms:
#   "        -:   12: code"  non-executable
#   "        1:   13: code"  hit
#   "    #####:   14: code"  miss
#   "        0:   15: code"  miss (sometimes)
# gcov lines: 9-char count field, ':', line number, ':', text
line_re = re.compile(r"^(.{9}):\s*(\d+):(.*)$")

files: dict[str, dict[int, int]] = {}

for gcov_path in sorted(work.glob("*.gcov")):
    text = gcov_path.read_text(errors="replace")
    source = None
    hits: dict[int, int] = {}
    for raw in text.splitlines():
        # Source path lives on the pseudo-line 0 record.
        if "Source:" in raw:
            source = raw.split("Source:", 1)[1].strip()
            continue
        m = line_re.match(raw)
        if not m or source is None:
            continue
        count_s, lineno_s = m.group(1).strip(), m.group(2)
        lineno = int(lineno_s)
        if lineno == 0:
            continue
        if count_s == "-" or count_s == "":
            continue  # non-executable
        if count_s.startswith("#") or count_s == "=====":
            hits[lineno] = 0
        else:
            digits = re.match(r"(\d+)", count_s)
            hits[lineno] = int(digits.group(1)) if digits else 0
    if source and source.startswith("src/") and hits:
        prev = files.get(source, {})
        for ln, h in hits.items():
            prev[ln] = max(prev.get(ln, 0), h)
        files[source] = prev

if not files:
    # Fallback: summary from logs only (no line detail)
    sys.stderr.write("error: no per-line .gcov data for src/; check gcov workdir\n")
    for p in sorted(work.iterdir()):
        sys.stderr.write(f"  {p.name}\n")
    sys.exit(1)

rows: list[tuple[str, int, int, float]] = []
for path in sorted(files):
    hits = files[path]
    total = len(hits)
    executed = sum(1 for h in hits.values() if h > 0)
    pct = 100.0 * executed / total if total else 0.0
    rows.append((path, total, executed, pct))

tot_lines = sum(t for _, t, _, _ in rows)
tot_exec = sum(e for _, _, e, _ in rows)
tot_pct = 100.0 * tot_exec / tot_lines if tot_lines else 0.0

# summary.txt
lines_out = [
    "GCC Code Coverage Report (src/ via fpm + gcov)",
    f"Directory: {root}",
    "File                                       Lines    Exec  Cover",
    "-" * 70,
]
for path, total, executed, pct in rows:
    lines_out.append(f"{path:42} {total:6d} {executed:6d} {pct:6.1f}%")
lines_out.append("-" * 70)
lines_out.append(f"{'TOTAL':42} {tot_lines:6d} {tot_exec:6d} {tot_pct:6.1f}%")
lines_out.append("")
summary = "\n".join(lines_out)
(out_dir / "summary.txt").write_text(summary)
(out_dir / "console.txt").write_text(summary)
print(summary)

# LCOV (Codecov primary input)
lcov_lines = ["TN:capnp-fortran"]
for path, total, executed, pct in rows:
    hits = files[path]
    lcov_lines.append(f"SF:{path}")
    for ln in sorted(hits):
        lcov_lines.append(f"DA:{ln},{hits[ln]}")
    lcov_lines.append(f"LF:{total}")
    lcov_lines.append(f"LH:{executed}")
    lcov_lines.append("end_of_record")
(out_dir / "coverage.lcov").write_text("\n".join(lcov_lines) + "\n")

# Cobertura with real <line> nodes
ts = int(time.time())
xml = [
    '<?xml version="1.0" ?>',
    f'<coverage line-rate="{tot_pct/100:.4f}" branch-rate="0" version="gcov" timestamp="{ts}">',
    "  <sources><source>.</source></sources>",
    "  <packages>",
    f'    <package name="src" line-rate="{tot_pct/100:.4f}" branch-rate="0" complexity="0">',
    "      <classes>",
]
for path, total, executed, pct in rows:
    cname = path.replace("/", ".")
    xml.append(
        f'        <class name="{html.escape(cname)}" filename="{html.escape(path)}" '
        f'line-rate="{pct/100:.4f}" branch-rate="0" complexity="0">'
    )
    xml.append("          <methods/>")
    xml.append("          <lines>")
    for ln, h in sorted(files[path].items()):
        xml.append(f'            <line number="{ln}" hits="{h}" branch="false"/>')
    xml.append("          </lines>")
    xml.append("        </class>")
xml += [
    "      </classes>",
    "    </package>",
    "  </packages>",
    "</coverage>",
    "",
]
(out_dir / "coverage.xml").write_text("\n".join(xml))

# HTML
body = [
    "<html><head><title>capnp-fortran coverage</title></head><body>",
    "<h1>capnp-fortran src/ coverage</h1>",
    f"<p><b>TOTAL</b>: {tot_exec}/{tot_lines} lines ({tot_pct:.1f}%)</p>",
    "<p>Upload target for Codecov: <code>coverage.lcov</code></p>",
    "<table border='1' cellpadding='4'><tr><th>File</th><th>Lines</th><th>Exec</th><th>Cover</th></tr>",
]
for path, total, executed, pct in rows:
    body.append(
        f"<tr><td>{html.escape(path)}</td><td>{total}</td><td>{executed}</td><td>{pct:.1f}%</td></tr>"
    )
body.append("</table></body></html>")
(out_dir / "index.html").write_text("\n".join(body))

# Sanity: LCOV must have DA: lines
lcov_text = (out_dir / "coverage.lcov").read_text()
if "DA:" not in lcov_text:
    sys.stderr.write("error: coverage.lcov has no DA: entries\n")
    sys.exit(1)
print(f"==> wrote {out_dir}/ ({len(rows)} src files, {tot_pct:.1f}% lines; LCOV DA lines OK)")
PY

echo "==> wrote $OUT_DIR/"
test -s "$OUT_DIR/coverage.lcov"
grep -q '^DA:' "$OUT_DIR/coverage.lcov"
grep -q '^src/' "$OUT_DIR/summary.txt"
