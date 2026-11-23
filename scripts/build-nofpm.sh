#!/usr/bin/env bash
# Build the library, the CLI tools, and the test-drive suites with a bare
# Fortran compiler, no fpm, then run selected suites. Exists for
# environments where fpm is unavailable, e.g. emulated big-endian CI
# (qemu s390x). Run from the repository root; test programs read fixtures
# via paths relative to it.
#
# TESTDRIVE_DIR names a test-drive v0.6.1 checkout (default: the one fpm
# fetched under build/dependencies). NOFPM_SUITES lists the tester suites
# to run (default: codecs regress).
#
# Exit status 3 means a suite or test program failed; any other nonzero
# status comes from a compile or link step.
#
# Under qemu, gfortran can occasionally segfault mid-compile (exit 139).
# Defaults favour reliability: -O0, no -fcheck, and per-unit retries.
set -euo pipefail

FC=${FC:-gfortran}
CC=${CC:-gcc}
AR=${AR:-ar}
# Prefer -O0 under emulation; callers may override FFLAGS.
FFLAGS=${FFLAGS:--O0 -g}
OUT=build/nofpm
mkdir -p "$OUT/app" "$OUT/test"
RETRIES=${COMPILE_RETRIES:-3}
TESTDRIVE_DIR=${TESTDRIVE_DIR:-build/dependencies/test-drive}
read -r -a SUITES <<< "${NOFPM_SUITES:-codecs regress}"

if [ ! -f "$TESTDRIVE_DIR/src/testdrive.F90" ]; then
  echo "test-drive sources not found under $TESTDRIVE_DIR; set TESTDRIVE_DIR" >&2
  exit 2
fi

# Library modules in dependency order.
LIB_SOURCES=(
  src/capnp_kinds.f90
  src/capnp_endian.f90
  src/capnp_pointer.f90
  src/capnp_arena.f90
  src/capnp_message.f90
  src/capnp_any.f90
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
  src/rpc_threeparty_capnp.f90
  src/capnp_rpc.f90
  src/capnp_schema.f90
  src/capnp_dynamic.f90
  src/capnp_codec.f90
  src/capnp_text.f90
  src/capnp_json.f90
  src/stream_capnp.f90
)

# Modules shared by the two executables.
APP_MODULES=(
  app/os_errno.f90
  app/capnpc_emit.f90
)

# Test framework, support modules, and test-drive suites, in dependency order.
TEST_MODULES=(
  "$TESTDRIVE_DIR/src/testdrive_version.f90"
  "$TESTDRIVE_DIR/src/testdrive.F90"
  test/addressbook_schema.f90
  test/generated/common_capnp.f90
  test/generated/addressbook_capnp.f90
  test/generated/kitchen_capnp.f90
  test/generated/adder_capnp.f90
  test/generated/box_capnp.f90
  test/generated/streamer_capnp.f90
  test/generated/dual_capnp.f90
  test/generated/holder_capnp.f90
  test/generated/dynamic_capnp.f90
  test/generated/idents_capnp.f90
  test/rpc_servers.f90
  test/rpc_adder_impl.f90
  test/rpc_writer_impl.f90
  test/cli_path.f90
  test/test_wire.f90
  test/test_addressbook.f90
  test/test_canonical.f90
  test/test_codecs.f90
  test/test_dynamic.f90
  test/test_annotated.f90
  test/test_generated.f90
  test/test_generic.f90
  test/test_holder.f90
  test/test_interop.f90
  test/test_kitchen.f90
  test/test_parity.f90
  test/test_rpc.f90
  test/test_rpc_handoff.f90
  test/test_rpc_typed.f90
  test/test_stream.f90
  test/test_codegen_parity.f90
  test/test_idents.f90
  test/test_regress.f90
  test/test_edges.f90
  test/test_limits.f90
  test/test_codegen.f90
  test/test_net.f90
  test/test_rpc_lifetime.f90
  test/test_rpc_peer.f90
)

# Standalone test programs, run after the suites.
TEST_PROGRAMS=(
  test/reentrancy.f90
)

# Run one command with retries (qemu/gfortran occasional SIGSEGV).
with_retries() {
  local attempt=1 rc=0
  while true; do
    set +e
    "$@"
    rc=$?
    set -e
    if [ "$rc" -eq 0 ]; then
      return 0
    fi
    echo "failed rc=$rc: $*" >&2
    if [ "$attempt" -ge "$RETRIES" ]; then
      return "$rc"
    fi
    attempt=$((attempt + 1))
    echo "retry $attempt/$RETRIES" >&2
    sleep 1
  done
}

compile_unit() {
  local f=$1 o=$2
  echo "compile: $f"
  with_retries "$FC" $FFLAGS -J "$OUT" -c "$f" -o "$o"
}

link_exe() {
  local out=$1
  shift
  echo "link: $out"
  with_retries "$FC" $FFLAGS -I "$OUT" "$@" -o "$out"
}

LIB_OBJS=()
for f in "${LIB_SOURCES[@]}"; do
  o="$OUT/$(basename "${f%.*}").o"
  compile_unit "$f" "$o"
  LIB_OBJS+=("$o")
done
# capnp_posix binds to these C helpers for socket calls and errno values.
echo "compile: src/capnp_posix_os.c"
with_retries "$CC" -O0 -g -c src/capnp_posix_os.c -o "$OUT/capnp_posix_os.o"
LIB_OBJS+=("$OUT/capnp_posix_os.o")
# The regress guard tests link helpers against build/**/libcapnp.a.
echo "archive: $OUT/libcapnp.a"
rm -f "$OUT/libcapnp.a"
"$AR" rcs "$OUT/libcapnp.a" "${LIB_OBJS[@]}"

echo "compile: app/os_errno.c"
with_retries "$CC" -O0 -g -c app/os_errno.c -o "$OUT/os_errno_c.o"
APP_OBJS=("$OUT/os_errno_c.o")
for f in "${APP_MODULES[@]}"; do
  o="$OUT/$(basename "${f%.*}").o"
  compile_unit "$f" "$o"
  APP_OBJS+=("$o")
done

# cli_path finds the tools as ../app/<name> beside the tester.
link_exe "$OUT/app/capnpf" app/capnpf.f90 "${APP_OBJS[@]}" "${LIB_OBJS[@]}"
link_exe "$OUT/app/capnpc-fortran" app/main.f90 "${APP_OBJS[@]}" "${LIB_OBJS[@]}"

TEST_OBJS=()
for f in "${TEST_MODULES[@]}"; do
  o="$OUT/$(basename "${f%.*}").o"
  compile_unit "$f" "$o"
  TEST_OBJS+=("$o")
done
link_exe "$OUT/test/tester" test/tester.f90 "${TEST_OBJS[@]}" "${LIB_OBJS[@]}"

status=0
for suite in "${SUITES[@]}"; do
  echo "== tester $suite"
  if ! "$OUT/test/tester" "$suite"; then
    echo "suite failed: $suite" >&2
    status=3
  fi
done

for prog in "${TEST_PROGRAMS[@]}"; do
  name=$(basename "${prog%.f90}")
  link_exe "$OUT/test/$name" "$prog" "${LIB_OBJS[@]}"
  echo "== $name"
  if ! "$OUT/test/$name"; then
    echo "test program failed: $name" >&2
    status=3
  fi
done
exit $status
