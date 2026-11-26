#!/bin/sh
# Exit 0 when capnp is on PATH and the three dev schemas are installed.
bin=$(command -v capnp 2>/dev/null) || exit 1
[ -n "$bin" ] || exit 1
if [ -n "${CAPNP_PREFIX:-}" ]; then
  inc="${CAPNP_PREFIX}/include"
else
  bin=$(realpath "$bin") || exit 1
  inc=$(dirname "$(dirname "$bin")")/include
fi
test -f "${inc}/capnp/compat/json.capnp" &&
  test -f "${inc}/capnp/c++.capnp" &&
  test -f "${inc}/capnp/stream.capnp"
