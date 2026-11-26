#!/bin/sh
# Exit 0 when capnp is on PATH and the three dev schemas are installed.
# CAPNP_PREFIX/include is used when CAPNP_PREFIX is non-empty and that
# directory exists. Otherwise command -v capnp is followed with readlink
# and cd -P, and the include directory is the parent of the resolved bin
# directory with /include appended.
# --include prints the include directory and exits without checking schemas.

p="$(command -v capnp)" || exit 1
if [ -n "${CAPNP_PREFIX:-}" ] && [ -d "${CAPNP_PREFIX}/include" ]; then
  inc="${CAPNP_PREFIX}/include"
else
  while [ -L "$p" ]; do
    t="$(readlink "$p")" || exit 1
    case "$t" in
      /*) p="$t" ;;
      *)
        base="$(dirname "$p")"
        p="${base}/${t}"
        ;;
    esac
  done
  d="$(cd -P "$(dirname "$p")" && pwd -P)" || exit 1
  parent="$(dirname "$d")"
  inc="${parent}/include"
fi
if [ "${1:-}" = "--include" ]; then
  printf '%s\n' "$inc"
  exit 0
fi
test -f "${inc}/capnp/compat/json.capnp" &&
  test -f "${inc}/capnp/c++.capnp" &&
  test -f "${inc}/capnp/stream.capnp"
