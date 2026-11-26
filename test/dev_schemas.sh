#!/bin/sh
# Exit 0 when capnp is on PATH and the three dev schemas are installed.
# --include prints the include directory and exits.

# Canonical path of one file. Follow symlinks, then the physical directory.
resolve_bin() {
  _f="$1"
  _n=0
  if command -v readlink >/dev/null 2>&1; then
    while [ -L "$_f" ]; do
      _n=$((_n + 1))
      [ "$_n" -le 40 ] || return 1
      _t=$(readlink "$_f") || return 1
      case "$_t" in
        /*) _f="$_t" ;;
        *) _f="$(dirname "$_f")/$_t" ;;
      esac
    done
  fi
  _d=$(dirname "$_f")
  _b=$(basename "$_f")
  _d=$(cd "$_d" && pwd -P) || return 1
  printf '%s/%s\n' "$_d" "$_b"
}

bin=$(command -v capnp 2>/dev/null) || exit 1
[ -n "$bin" ] || exit 1
if [ -n "${CAPNP_PREFIX:-}" ]; then
  inc="${CAPNP_PREFIX}/include"
else
  bin=$(resolve_bin "$bin") || exit 1
  inc=$(dirname "$(dirname "$bin")")/include
fi
# --include prints that directory for -I and does not check the schema files.
if [ "${1:-}" = --include ]; then
  printf '%s\n' "$inc"
  exit 0
fi
test -f "${inc}/capnp/compat/json.capnp" &&
  test -f "${inc}/capnp/c++.capnp" &&
  test -f "${inc}/capnp/stream.capnp"
