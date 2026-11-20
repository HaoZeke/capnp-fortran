#!/usr/bin/env bash
# Regenerate test/fixtures/*.cgr.bin with the capnp on PATH.
#
# Each file is a CodeGeneratorRequest (`capnp compile -o-`). The path
# passed to capnp is part of the request, so the commands below are the
# ones that produced the checked-in bytes. Run with --check to compare
# a fresh compile against those files.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
FIX="$ROOT/test/fixtures"
SCHEMA="$ROOT/schema"

if ! command -v capnp >/dev/null 2>&1; then
  echo "gen-cgr: no capnp CLI on PATH" >&2
  exit 1
fi
if ! command -v pkg-config >/dev/null 2>&1 || ! pkg-config --exists capnp; then
  echo "gen-cgr: pkg-config cannot find capnp" >&2
  exit 1
fi
CAPNP_INC="$(pkg-config --variable=includedir capnp)"
if [[ ! -f "$CAPNP_INC/capnp/schema.capnp" ]]; then
  echo "gen-cgr: schema.capnp is not under $CAPNP_INC" >&2
  exit 1
fi

CHECK=0
[[ "${1:-}" == "--check" ]] && CHECK=1

OUT="$FIX"
if [[ $CHECK -eq 1 ]]; then
  OUT=$(mktemp -d)
  trap 'rm -rf "$OUT"' EXIT
fi

compile_root() {
  local name=$1
  shift
  capnp compile -I"$CAPNP_INC" -o- "$@" >"$OUT/${name}.cgr.bin"
}

cd "$ROOT"
for name in adder addressbook dual dynamic holder stream streamer; do
  compile_root "$name" "$SCHEMA/${name}.capnp"
done
compile_root kitchen "$SCHEMA/kitchen.capnp" "$SCHEMA/common.capnp"
compile_root rpc "$SCHEMA/rpc.capnp" "$SCHEMA/rpc-twoparty.capnp"
compile_root rpc-threeparty "$SCHEMA/rpc-threeparty.capnp"
( cd "$SCHEMA" && capnp compile -o- box.capnp >"$OUT/box.cgr.bin" )
( cd "$CAPNP_INC/capnp" && capnp compile -I"$CAPNP_INC" -o- schema.capnp >"$OUT/schema.cgr.bin" )

if [[ $CHECK -eq 1 ]]; then
  rc=0
  for f in adder addressbook box dual dynamic holder kitchen rpc \
           rpc-threeparty schema stream streamer; do
    if cmp -s "$OUT/${f}.cgr.bin" "$FIX/${f}.cgr.bin"; then
      echo "ok   ${f}.cgr.bin matches $(capnp --version)"
    else
      echo "FAIL ${f}.cgr.bin differs from $(capnp --version)" >&2
      rc=1
    fi
  done
  exit $rc
fi

echo "wrote CodeGeneratorRequest fixtures under $FIX with $(capnp --version)"
