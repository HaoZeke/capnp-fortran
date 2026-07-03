#!/usr/bin/env bash
# Strict standard-conformance gate: every source must parse under
# gfortran -std=f2018 with no extensions. Syntax-only; no objects.
set -euo pipefail

FC=${FC:-gfortran}
OUT=build/stdcheck
mkdir -p "$OUT"

SOURCES=(
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
  app/capnpc_emit.f90
  app/main.f90
  test/addressbook_schema.f90
  test/generated/common_capnp.f90
  test/generated/addressbook_capnp.f90
  test/generated/kitchen_capnp.f90
  test/generated/adder_capnp.f90
  test/generated/box_capnp.f90
  test/rpc_servers.f90
  test/rpc_adder_impl.f90
  test/check.f90
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
  interop/rpc_client.f90
)

# Modules must exist for use-association, so compile (not just parse)
# in dependency order, but discard objects.
status=0
for f in "${SOURCES[@]}"; do
  if ! "$FC" -std=f2018 -Wall -Wextra -fmax-errors=5 -J "$OUT" \
       -c "$f" -o "$OUT/stdcheck.o" 2> "$OUT/err.txt"; then
    echo "== $f"
    cat "$OUT/err.txt"
    status=1
  fi
done
rm -f "$OUT/stdcheck.o"
if [ $status -eq 0 ]; then
  echo "All sources conform to -std=f2018."
fi
exit $status
