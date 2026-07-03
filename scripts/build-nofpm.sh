#!/usr/bin/env bash
# Build and run the full test suite with a bare Fortran compiler, no fpm.
# Exists for environments where fpm is unavailable, e.g. emulated
# big-endian CI (qemu s390x). Run from the repository root; test programs
# read fixtures via paths relative to it.
set -euo pipefail

FC=${FC:-gfortran}
FFLAGS=${FFLAGS:--O1 -g -fcheck=bounds}
OUT=build/nofpm
mkdir -p "$OUT"

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
)

# Support modules used by test programs, in dependency order.
TEST_MODULES=(
  test/addressbook_schema.f90
  test/generated/common_capnp.f90
  test/generated/addressbook_capnp.f90
  test/generated/kitchen_capnp.f90
)

TEST_PROGRAMS=(
  test/check.f90
  test/test_addressbook.f90
  test/test_interop.f90
  test/test_generated.f90
  test/test_kitchen.f90
  test/test_parity.f90
  test/test_canonical.f90
)

OBJS=()
for f in "${LIB_SOURCES[@]}" "${TEST_MODULES[@]}"; do
  o="$OUT/$(basename "${f%.f90}").o"
  "$FC" $FFLAGS -J "$OUT" -c "$f" -o "$o"
  OBJS+=("$o")
done

status=0
for prog in "${TEST_PROGRAMS[@]}"; do
  name=$(basename "${prog%.f90}")
  "$FC" $FFLAGS -I "$OUT" "$prog" "${OBJS[@]}" -o "$OUT/$name"
  echo "== $name"
  if ! "$OUT/$name"; then
    status=1
  fi
done
exit $status
