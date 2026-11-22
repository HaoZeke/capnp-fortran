#!/usr/bin/env bash
# Regression for the schema-compiler gaps. Each case fails on the base
# emitter and matches Cap'n Proto C++ 1.4.0 after the fix.
set -euo pipefail

ROOT=$(cd "$(dirname "$0")/.." && pwd)
cd "$ROOT"

BIN=$(find build -name capnpc-fortran -type f -printf '%T@ %p\n' | sort -n | tail -1 | cut -d' ' -f2-)
MOD=$(find build -name capnp.mod -printf '%T@ %h\n' | sort -n | tail -1 | cut -d' ' -f2-)
LIB=$(find build -name libcapnp.a -printf '%T@ %p\n' | sort -n | tail -1 | cut -d' ' -f2-)
if [[ -z "${BIN}" || -z "${MOD}" || -z "${LIB}" ]]; then
  echo "codegen parity: build capnpc-fortran first" >&2
  exit 1
fi
BIN=$(cd "$(dirname "$BIN")" && pwd)/$(basename "$BIN")
MOD=$(cd "$MOD" && pwd)
LIB=$(cd "$(dirname "$LIB")" && pwd)/$(basename "$LIB")

FC=${FC:-gfortran}
CXX=${CXX:-g++}
SAN=()
if [[ "${SANITIZE:-0}" == 1 ]]; then
  SAN=(-fsanitize=address,undefined -fno-omit-frame-pointer)
fi
export LD_LIBRARY_PATH=/usr/local/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}

# A prefixed install (a conda or pixi environment) keeps the standard schemas,
# C++ headers and capnp.pc under its own prefix, which neither `capnp` nor the
# system `pkg-config` search by default.
CAPNP_PREFIX=$(cd "$(dirname "$(command -v capnp)")/.." && pwd)
CAPNP_INC=()
if [[ -f "$CAPNP_PREFIX/include/capnp/stream.capnp" ]]; then
  CAPNP_INC=(-I "$CAPNP_PREFIX/include")
fi
export PKG_CONFIG_PATH=$CAPNP_PREFIX/lib/pkgconfig${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}
export LD_LIBRARY_PATH=$CAPNP_PREFIX/lib:$LD_LIBRARY_PATH

work() {
  local d
  d=$(mktemp -d /tmp/codegen-parity.XXXXXX)
  echo "$d"
}

fc() {
  "$FC" -std=f2018 "${SAN[@]}" "$@"
}

cmp_out() {
  local got=$1 exp=$2
  if [[ "$got" != "$exp" ]]; then
    echo "mismatch:" >&2
    echo " got: $got" >&2
    echo " exp: $exp" >&2
    exit 1
  fi
}

case "${1:-all}" in
  all) ;;
  *) echo "unknown gap: $1" >&2; exit 2 ;;
esac
