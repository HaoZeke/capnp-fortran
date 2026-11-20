#!/usr/bin/env bash
# Build the RPC differential checker: the raw driver, the C++ RpcSystem
# peer and the Fortran peer. Run it inside the interop pixi environment,
# which pins Cap'n Proto 1.4.0:
#
#   pixi run -e interop bash interop/rpc-diff/build.sh [outdir]
#
# outdir defaults to build-rpc-diff/ at the repository root. SRC names
# the checkout whose src/ the Fortran peer is built from, so a peer for
# an older commit can be built from a worktree. FFLAGS overrides the
# Fortran flags.
set -euo pipefail

HERE=$(cd "$(dirname "$0")" && pwd)
ROOT=$(cd "$HERE/../.." && pwd)
SRC=$(cd "${SRC:-$ROOT}" && pwd)
OUT=${1:-$ROOT/build-rpc-diff}
CXX=${CXX:-c++}
CC=${CC:-cc}
FC=${FC:-gfortran}
FFLAGS=${FFLAGS:--O1 -g -fno-omit-frame-pointer}

ver=$(capnp --version)
case "$ver" in
  *" 1.4.0") ;;
  *) echo "build.sh: expected Cap'n Proto 1.4.0, found: $ver" >&2; exit 1 ;;
esac

mkdir -p "$OUT/gen" "$OUT/f"
capinc=$(pkg-config --variable=includedir capnp)
capnp compile --src-prefix="$HERE" -I"$capinc" -oc++:"$OUT/gen" "$HERE/diff.capnp"

libdir=$(pkg-config --variable=libdir capnp-rpc)
cxxflags="-std=c++17 -O1 -g $(pkg-config --cflags capnp-rpc) -I$OUT/gen"
cxxlibs="$(pkg-config --libs capnp-rpc) -Wl,-rpath,$libdir"
# shellcheck disable=SC2086
"$CXX" $cxxflags "$HERE/driver.c++" "$OUT/gen/diff.capnp.c++" -o "$OUT/driver" $cxxlibs
# shellcheck disable=SC2086
"$CXX" $cxxflags "$HERE/peer_cpp.c++" "$OUT/gen/diff.capnp.c++" -o "$OUT/peer_cpp" $cxxlibs

# Library modules in dependency order, as in scripts/build-nofpm.sh.
LIB_SOURCES=(
  capnp_kinds.f90 capnp_endian.f90 capnp_pointer.f90 capnp_arena.f90
  capnp_message.f90 capnp_any.f90 capnp_serialize.f90 capnp_packed.f90
  capnp_union.f90 capnp_stream.f90 capnp_canonical.f90 capnp.f90
  capnp_cabi.f90
  capnp_posix.f90 capnp_rpc_transport.f90 rpc_capnp.f90
  rpc_twoparty_capnp.f90 rpc_threeparty_capnp.f90 capnp_rpc.f90
)
for f in "$SRC"/src/*.f90; do
  b=$(basename "$f")
  case " ${LIB_SOURCES[*]} capnp_schema.f90 capnp_dynamic.f90 stream_capnp.f90 " in
    *" $b "*) ;;
    *) echo "build.sh: $b is not in LIB_SOURCES; add it in dependency order" >&2; exit 1 ;;
  esac
done

objs=()
for f in "${LIB_SOURCES[@]}"; do
  o="$OUT/f/${f%.f90}.o"
  # shellcheck disable=SC2086
  "$FC" $FFLAGS -J"$OUT/f" -c "$SRC/src/$f" -o "$o"
  objs+=("$o")
done
"$CC" -O1 -g -c "$SRC/src/capnp_posix_os.c" -o "$OUT/f/capnp_posix_os.o"
objs+=("$OUT/f/capnp_posix_os.o")
# shellcheck disable=SC2086
"$FC" $FFLAGS -I"$OUT/f" -J"$OUT/f" "$HERE/peer_f.f90" "${objs[@]}" -o "$OUT/peer_f"

echo "built $OUT/driver $OUT/peer_cpp $OUT/peer_f (Fortran from $SRC)"
