#!/usr/bin/env bash
# Build the RPC differential checker and run it over a seed range and
# the fixed probes.
#
#   pixi run -e interop rpc-diff            # seeds 1-300
#   pixi run -e interop rpc-diff 1 50       # a smaller range
#
# Logs, per-seed first differences and summary.tsv go to
# build-rpc-diff/logs (OUT overrides it). JOBS sets the parallel runs.
# Exit status is 0 only when every seed and probe is SAME.
set -euo pipefail
HERE=$(cd "$(dirname "$0")" && pwd)
ROOT=$(cd "$HERE/../.." && pwd)
FIRST=${1:-1}
LAST=${2:-300}
OUT=${OUT:-$ROOT/build-rpc-diff/logs}
bash "$HERE/build.sh" "$ROOT/build-rpc-diff"
rm -rf "$OUT"
python3 "$HERE/run.py" --bin "$ROOT/build-rpc-diff" --out "$OUT" \
  --jobs "${JOBS:-4}" --probes "$FIRST" "$LAST"
