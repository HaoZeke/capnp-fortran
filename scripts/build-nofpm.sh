#!/usr/bin/env bash
# Build and run the full test suite with a bare Fortran compiler, no fpm.
# Exists for environments where fpm is unavailable, e.g. emulated
# big-endian CI (qemu s390x). Run from the repository root; test programs
# read fixtures via paths relative to it.
#
# Under qemu, gfortran can occasionally segfault mid-compile (exit 139).
# Defaults favour reliability: -O0, no -fcheck, and per-unit retries.
set -euo pipefail

FC=${FC:-gfortran}
# Prefer -O0 under emulation; callers may override FFLAGS.
FFLAGS=${FFLAGS:--O0 -g}
OUT=build/nofpm
mkdir -p "$OUT"
RETRIES=${COMPILE_RETRIES:-3}

# Library modules in dependency order.
LIB_SOURCES=(
  src/capnp_kinds.f90
  src/capnp_endian.f90
  src/capnp_pointer.f90
  src/capnp_arena.f90
  src/capnp_message.f90
  src/capnp_serialize.f90
  src/capnp_packed.f90
  src/capnp_union.f90
  src/capnp_stream.f90
  src/capnp_canonical.f90
  src/capnp.f90
  src/capnp_cabi.f90
  src/capnp_posix.f90
  src/capnp_rpc_transport.f90
  src/rpc_capnp.f90
  src/rpc_twoparty_capnp.f90
  src/capnp_rpc.f90
  src/capnp_schema.f90
  src/capnp_dynamic.f90
  src/stream_capnp.f90
)

# Support modules used by test programs, in dependency order.
TEST_MODULES=(
  test/addressbook_schema.f90
  test/generated/common_capnp.f90
  test/generated/addressbook_capnp.f90
  test/generated/kitchen_capnp.f90
  test/generated/adder_capnp.f90
  test/generated/box_capnp.f90
  test/generated/streamer_capnp.f90
  test/generated/dual_capnp.f90
  test/generated/holder_capnp.f90
  test/rpc_servers.f90
  test/rpc_adder_impl.f90
  test/rpc_writer_impl.f90
)

TEST_PROGRAMS=(
  # wire suite (test/tester.f90 + test/test_wire.f90) needs test-drive; fpm only
  test/test_addressbook.f90
  test/test_interop.f90
  test/test_generated.f90
  test/test_kitchen.f90
  test/test_parity.f90
  test/test_canonical.f90
  test/test_rpc.f90
  test/test_rpc_typed.f90
  test/test_dynamic.f90
  test/test_generic.f90
  test/test_stream.f90
  test/test_holder.f90
)

# Compile one unit with retries (qemu/gfortran occasional SIGSEGV).
compile_unit() {
  local f=$1 o=$2
  local attempt=1 rc=0
  while true; do
    echo "compile: $f (attempt $attempt/$RETRIES)"
    set +e
    "$FC" $FFLAGS -J "$OUT" -c "$f" -o "$o"
    rc=$?
    set -e
    if [ "$rc" -eq 0 ]; then
      return 0
    fi
    echo "compile failed: $f rc=$rc" >&2
    if [ "$attempt" -ge "$RETRIES" ]; then
      return "$rc"
    fi
    attempt=$((attempt + 1))
    sleep 1
  done
}

OBJS=()
for f in "${LIB_SOURCES[@]}" "${TEST_MODULES[@]}"; do
  o="$OUT/$(basename "${f%.f90}").o"
  compile_unit "$f" "$o"
  OBJS+=("$o")
done

status=0
for prog in "${TEST_PROGRAMS[@]}"; do
  name=$(basename "${prog%.f90}")
  echo "link: $name"
  set +e
  "$FC" $FFLAGS -I "$OUT" "$prog" "${OBJS[@]}" -o "$OUT/$name"
  rc=$?
  set -e
  if [ "$rc" -ne 0 ]; then
    echo "link failed: $name rc=$rc" >&2
    status=1
    continue
  fi
  echo "== $name"
  if ! "$OUT/$name"; then
    status=1
  fi
done
exit $status
