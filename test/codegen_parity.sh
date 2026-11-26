#!/usr/bin/env bash
# Regression for the schema-compiler gaps. Each case fails on the base
# emitter and matches Cap'n Proto C++ 1.4.0 after the fix.
set -euo pipefail

START=$PWD
ROOT=$(cd "$(dirname "$0")/.." && pwd)
cd "$ROOT"

# Usage: codegen_parity.sh [g1..g57|all] [build-dir]
# The build directory may also come from CAPNP_BUILD_DIR and must contain one
# complete build. Without one, fpm builds into build-codegen-parity.
CASE=${1:-all}
BUILD_DIR=${2:-${CAPNP_BUILD_DIR:-}}
if [[ -n "$BUILD_DIR" && "$BUILD_DIR" != /* ]]; then
  BUILD_DIR=$START/$BUILD_DIR
fi

FC=${FC:-gfortran}
CXX=${CXX:-g++}

die() {
  echo "codegen parity: $*" >&2
  exit 1
}

# Exit 77 asks the Fortran runner to skip the case. That covers a missing
# C++ toolchain and a capnp install without its standard schemas.
skip_dev() {
  echo "codegen parity: $*" >&2
  exit 77
}

command -v "$FC" > /dev/null || die "$FC not found on PATH"
command -v capnp > /dev/null || skip_dev "capnp is not on PATH"
for tool in "$CXX" pkg-config python3; do
  command -v "$tool" > /dev/null || skip_dev "$tool is not on PATH"
done

TMP_ROOT=$(mktemp -d "${TMPDIR:-/tmp}/codegen-parity.XXXXXX")
trap 'rm -rf "$TMP_ROOT"' EXIT
trap 'exit 130' INT
trap 'exit 143' TERM HUP

join_path() {
  local IFS=:
  echo "$*"
}

# A prefixed install (a conda or pixi environment) keeps the standard schemas,
# C++ headers and capnp.pc under its own prefix, which neither `capnp` nor the
# system `pkg-config` search by default. Some layouts use lib64.
if [[ -z "${CAPNP_PREFIX:-}" ]]; then
  CAPNP_PREFIX=$(cd "$(dirname "$(command -v capnp)")/.." && pwd)
fi
CAPNP_INC=()
if [[ -f "$CAPNP_PREFIX/include/capnp/stream.capnp" ]]; then
  CAPNP_INC=(-I "$CAPNP_PREFIX/include")
fi
PC_DIRS=()
LIB_DIRS=()
PC_DEFS=()
for sub in lib lib64; do
  [[ -d "$CAPNP_PREFIX/$sub" ]] && LIB_DIRS+=("$CAPNP_PREFIX/$sub")
  if [[ -d "$CAPNP_PREFIX/$sub/pkgconfig" ]]; then
    PC_DIRS+=("$CAPNP_PREFIX/$sub/pkgconfig")
    if [[ -f "$CAPNP_PREFIX/$sub/pkgconfig/capnp.pc" ]]; then
      PC_DEFS=(--define-variable=prefix="$CAPNP_PREFIX")
    fi
  fi
done
if ((${#PC_DIRS[@]})); then
  PKG_CONFIG_PATH="$(join_path "${PC_DIRS[@]}")${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}"
  export PKG_CONFIG_PATH
fi
if ((${#LIB_DIRS[@]})); then
  LD_LIBRARY_PATH="$(join_path "${LIB_DIRS[@]}")${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
  export LD_LIBRARY_PATH
fi

# The C++ side of a case links against libcapnp. pkg-config does not quote a
# prefix containing spaces in --cflags or --libs, so the two directories come
# from variables, and only -l, -D and -pthread words are kept from the rest.
CAPNP_PC=()

dev_schemas() {
  [[ -f "$CAPNP_PREFIX/include/capnp/compat/json.capnp" && \
     -f "$CAPNP_PREFIX/include/capnp/c++.capnp" && \
     -f "$CAPNP_PREFIX/include/capnp/stream.capnp" ]]
}

# A distro may install the capnp binary without the standard schemas. An import
# error is a skip only in that case. When the schemas are installed, the same
# error stays a failure.
capnp() {
  local err rc
  err=$(mktemp)
  command capnp "$@" 2>"$err" && rc=0 || rc=$?
  if [[ -s "$err" ]]; then
    cat "$err" >&2
  fi
  if [[ $rc -ne 0 ]] && ! dev_schemas && grep -q -e 'Import failed' -e 'not found in the import path' "$err"; then
    rm -f "$err"
    skip_dev "standard schemas are not installed"
  fi
  rm -f "$err"
  return "$rc"
}

capnp_cxx_flags() {
  local inc lib others word
  if ((${#CAPNP_PC[@]})); then
    return 0
  fi
  pkg-config "${PC_DEFS[@]}" --exists capnp \
    || skip_dev "pkg-config cannot find capnp.pc; install the Cap'n Proto development files or set CAPNP_PREFIX"
  inc=$(pkg-config "${PC_DEFS[@]}" --variable=includedir capnp)
  lib=$(pkg-config "${PC_DEFS[@]}" --variable=libdir capnp)
  others=$(pkg-config "${PC_DEFS[@]}" --cflags-only-other --libs-only-l --libs-only-other capnp)
  CAPNP_PC=(-I"$inc" -L"$lib")
  for word in $others; do
    case "$word" in
      -l* | -D* | -pthread) CAPNP_PC+=("$word") ;;
    esac
  done
}

# Modules a generated schema and its driver need.
REQUIRED_MODS=(capnp capnp_rpc capnp_posix stream_capnp)

# Every artifact comes from one build. A directory given by the caller must
# contain exactly one plugin, one library and one module directory; with several
# (two fpm profiles, a static and a shared Meson build) they could come from
# different builds. Without a directory, the script builds into a tree it owns.
# fpm gets the relative name: it runs from $ROOT, and fpm does not quote an
# absolute --build-dir when it hands the path to git, so spaces in $ROOT break it.
PRIVATE_BUILD_NAME=build-codegen-parity
PRIVATE_BUILD=$ROOT/$PRIVATE_BUILD_NAME
DISCOVER_ERR=

sole() {
  local what=$1 root=$2
  shift 2
  local found
  mapfile -t found < <(find "$root" "$@" 2> /dev/null | sort)
  if ((${#found[@]} != 1)); then
    DISCOVER_ERR="found ${#found[@]} $what under $root, expected one"
    return 1
  fi
  SOLE=${found[0]}
}

# A library built for capnpc-fortran alone has no RPC or POSIX objects.
lib_complete() {
  local lib=$1 flag=-g syms m
  command -v nm > /dev/null || return 0
  [[ "$lib" == *.so ]] && flag=-D
  syms=$(nm "$flag" --defined-only "$lib" 2> /dev/null) || return 1
  for m in capnp_rpc capnp_posix; do
    grep "__${m}_MOD_" <<< "$syms" > /dev/null || return 1
  done
}

discover() {
  local root=$1 bin lib mod m
  DISCOVER_ERR=
  [[ -d "$root" ]] || {
    DISCOVER_ERR="$root is not a directory"
    return 1
  }
  sole "capnpc-fortran executables" "$root" -name capnpc-fortran -type f || return 1
  bin=$SOLE
  sole "libraries" "$root" \( -type f -o -type l \) \
    \( -name libcapnp.a -o -name libcapnp_fortran.a -o -name libcapnp_fortran.so \) || return 1
  lib=$SOLE
  sole "module directories (capnp.mod)" "$root" -name capnp.mod || return 1
  mod=$(dirname "$SOLE")
  for m in "${REQUIRED_MODS[@]}"; do
    [[ -f "$mod/$m.mod" ]] || {
      DISCOVER_ERR="$mod has no $m.mod"
      return 1
    }
  done
  lib_complete "$lib" || {
    DISCOVER_ERR="$lib lacks the capnp_rpc and capnp_posix objects"
    return 1
  }
  BIN=$bin LIB=$lib MOD=$mod
}

# --build-dir needs fpm 0.13. Older releases print an unknown-keyword message
# and exit 0 without building, so the directory is checked after the build.
# The fpm output stays on disk when the build is unusable.
build_private() {
  local attempt log
  log=$(mktemp "${TMPDIR:-/tmp}/codegen-parity-fpm.XXXXXX")
  for attempt in 1 2; do
    # git exports GIT_DIR to rebase exec lines; fpm clones dependencies with git.
    env -u GIT_DIR -u GIT_WORK_TREE fpm build --no-prune --build-dir "$PRIVATE_BUILD_NAME" \
      > "$log" 2>&1 || {
      cat "$log" >&2
      rm -f "$log"
      die "fpm build failed in $PRIVATE_BUILD"
    }
    if [[ ! -d "$PRIVATE_BUILD" ]]; then
      DISCOVER_ERR="fpm did not create $PRIVATE_BUILD (--build-dir needs fpm 0.13 or newer); fpm output is in $log"
      return 1
    fi
    if discover "$PRIVATE_BUILD"; then
      rm -f "$log"
      return 0
    fi
    rm -rf "$PRIVATE_BUILD"
  done
  DISCOVER_ERR="$DISCOVER_ERR; fpm output is in $log"
  return 1
}

BIN= MOD= LIB=
if [[ -n "$BUILD_DIR" ]]; then
  discover "$BUILD_DIR" || die "incomplete build in $BUILD_DIR: $DISCOVER_ERR"
else
  command -v fpm > /dev/null || die "fpm not found; pass a build directory or set CAPNP_BUILD_DIR"
  build_private || die "incomplete build in $PRIVATE_BUILD: $DISCOVER_ERR"
fi
BIN=$(cd "$(dirname "$BIN")" && pwd)/$(basename "$BIN")
MOD=$(cd "$MOD" && pwd)
LIB=$(cd "$(dirname "$LIB")" && pwd)/$(basename "$LIB")
LIBARGS=("$LIB")
if [[ "$LIB" == *.so ]]; then
  LIBARGS=(-L"$(dirname "$LIB")" -lcapnp_fortran -Wl,-rpath,"$(dirname "$LIB")")
fi

# The library says what its drivers need at link time: the gcov runtime for
# an instrumented one, the sanitizer runtimes for a sanitized one.
NM_FLAG=-g
[[ "$LIB" == *.so ]] && NM_FLAG=-D
LIB_SYMS=$(nm "$NM_FLAG" "$LIB" 2> /dev/null) || LIB_SYMS=
has_sym() {
  grep -- "$1" <<< "$LIB_SYMS" > /dev/null
}
SAN=()
SAN_KINDS=
if [[ "${SANITIZE:-0}" == 1 ]]; then
  SAN_KINDS=address,undefined
else
  has_sym __asan_ && SAN_KINDS=address
  has_sym __ubsan_ && SAN_KINDS=${SAN_KINDS:+$SAN_KINDS,}undefined
fi
if [[ -n "$SAN_KINDS" ]]; then
  SAN=(-fsanitize="$SAN_KINDS" -fno-omit-frame-pointer)
fi
COV=()
if has_sym __gcov_init; then
  COV=(--coverage)
fi

work() {
  mktemp -d "$TMP_ROOT/case.XXXXXX"
}

fc() {
  "$FC" -std=f2018 "${SAN[@]}" "${COV[@]}" "$@"
}

run_drv() {
  local rc=0
  DRV_OUT=$(./drv) || rc=$?
  if ((rc != 0)); then
    echo "drv exited with status $rc" >&2
    echo "$DRV_OUT" >&2
    exit 1
  fi
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

# The schema is valid for Cap'n Proto C++ 1.4.0: capnp generates the C++ code
# and g++ accepts it. Each case calls this before it checks the Fortran side.
cxx_ok() {
  local f
  capnp_cxx_flags
  capnp compile "${CAPNP_INC[@]}" -oc++ "$@"
  for f in "$@"; do
    "$CXX" -std=c++17 -fsyntax-only "${CAPNP_PC[@]}" "$f.c++"
  done
}

# No generated line may pass the free-form limit of 132 columns.
no_long_lines() {
  if awk 'length($0) > 132 { bad = 1 } END { exit !bad }' "$@"; then
    echo "generated line passes 132 columns in $*" >&2
    exit 1
  fi
}

g1() {
  local d
  d=$(work)
  cd "$d"
  cat > minmax.capnp << 'EOF'
@0x8111111111111101;
annotation floor @0x8111111111111102 (struct) :Int64;
struct MinMax $floor(-9223372036854775808) {
  i8 @0 :Int8 = -128;
  i16 @1 :Int16 = -32768;
  i32 @2 :Int32 = -2147483648;
  i64 @3 :Int64 = -9223372036854775808;
  z @4 :Float64 = -0;
}
const minI64 :Int64 = -9223372036854775808;
EOF
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" minmax.capnp
  cat > annid.capnp << 'EOF'
@0x8111111111111103;
annotation minid @0x8000000000000000 (struct) :Int32;
EOF
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" annid.capnp
  fc -std=f2018 -c annid_capnp.f90 -I "$MOD" -J .
  cat > ifcid.capnp << 'EOF'
@0x8111111111111104;
interface MinId @0x8000000000000000 {}
EOF
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" ifcid.capnp
  fc -std=f2018 -c ifcid_capnp.f90 -I "$MOD" -J .
  cat > drv.f90 << 'EOF'
program drv
   use capnp
   use minmax_capnp
   implicit none
   type(capnp_message_t), target :: m
   type(min_max_t) :: h
   integer :: err
   real(real64) :: z
   integer(int64) :: bits
   call capnp_message_init_builder(m, err)
   h = min_max_new_root(m, err)
   z = min_max_z_get(h)
   bits = transfer(z, bits)
   print '(i0,1x,i0,1x,i0,1x,i0,1x,i0,1x,z16.16)', &
      min_max_i8_get(h), min_max_i16_get(h), min_max_i32_get(h), &
      min_max_i64_get(h), min_i64, bits
   call capnp_message_free(m)
end program drv
EOF
  fc minmax_capnp.f90 drv.f90 -I "$MOD" -J . "${LIBARGS[@]}" -o drv
  capnp compile "${CAPNP_INC[@]}" -oc++ minmax.capnp
  cat > cpp.cpp << 'EOF'
#include "minmax.capnp.h"
#include <capnp/message.h>
#include <cstdio>
#include <cstring>
int main() {
  capnp::MallocMessageBuilder b;
  auto r = b.initRoot<MinMax>();
  double z = r.getZ();
  unsigned long long bits = 0;
  std::memcpy(&bits, &z, sizeof(z));
  std::printf("%d %d %d %lld %lld %016llx\n",
    (int)r.getI8(), (int)r.getI16(), (int)r.getI32(),
    (long long)r.getI64(), (long long)MIN_I64, bits);
}
EOF
  capnp_cxx_flags
  "$CXX" -std=c++17 cpp.cpp minmax.capnp.c++ "${CAPNP_PC[@]}" -o cppdrv
  local f c
  run_drv
  f=$(tr 'A-F' 'a-f' <<< "$DRV_OUT")
  c=$(./cppdrv | tr 'A-F' 'a-f')
  cmp_out "$f" "$c"
  # -0.0 has the minimum integer of each kind as its bit pattern.
  cat > negzero.capnp << 'EOF'
@0x8111111111111105;
annotation nz32 @0x8111111111111106 (struct) :Float32;
annotation nz64 @0x8111111111111107 (struct) :Float64;
struct NegZero $nz32(-0.0) $nz64(-0.0) {
  x @0 :Int32;
}
EOF
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" negzero.capnp
  cat > drv.f90 << 'EOF'
program drv
   use negzero_capnp
   implicit none
   print '(z8.8,1x,z16.16)', transfer(NEG_ZERO_ANNOTATION_NZ32, 0_int32), &
      transfer(NEG_ZERO_ANNOTATION_NZ64, 0_int64)
end program drv
EOF
  fc negzero_capnp.f90 drv.f90 -I "$MOD" -J . "${LIBARGS[@]}" -o drv
  capnp compile "${CAPNP_INC[@]}" -oc++ negzero.capnp
  cat > cpp.cpp << 'EOF'
#include "negzero.capnp.h"
#include <capnp/schema.h>
#include <cstdint>
#include <cstdio>
#include <cstring>
int main() {
  std::uint32_t b32 = 0;
  std::uint64_t b64 = 0;
  for (auto a : capnp::Schema::from<NegZero>().getProto().getAnnotations()) {
    auto v = a.getValue();
    if (v.isFloat32()) {
      float x = v.getFloat32();
      std::memcpy(&b32, &x, sizeof(x));
    } else if (v.isFloat64()) {
      double x = v.getFloat64();
      std::memcpy(&b64, &x, sizeof(x));
    }
  }
  std::printf("%08x %016llx\n", (unsigned)b32, (unsigned long long)b64);
}
EOF
  "$CXX" -std=c++17 cpp.cpp negzero.capnp.c++ "${CAPNP_PC[@]}" -o cppdrv
  run_drv
  f=$(tr 'A-F' 'a-f' <<< "$DRV_OUT")
  c=$(./cppdrv | tr 'A-F' 'a-f')
  cmp_out "$f" "$c"
  cmp_out "$f" "80000000 8000000000000000"
  echo "g1 ok"
}

g2() {
  local d
  d=$(work)
  cd "$d"
  cat > textesc.capnp << 'EOF'
@0x8222222222222201;
struct Tesc { s @0 :Text = "a\nb"; }
const multi :Text = "line1\nline2";
EOF
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" textesc.capnp
  cat > drv.f90 << 'EOF'
program drv
   use capnp
   use textesc_capnp
   implicit none
   type(capnp_message_t), target :: m
   type(tesc_t) :: h
   character(len=:), allocatable :: s
   integer :: err
   call capnp_message_init_builder(m, err)
   h = tesc_new_root(m, err)
   call tesc_s_get(h, s, err)
   if (err /= 0) error stop 1
   if (index(s, achar(10)) /= 2) error stop 2
   if (s /= 'a'//achar(10)//'b') error stop 3
   if (index(multi, achar(10)) /= 6) error stop 4
   if (multi /= 'line1'//achar(10)//'line2') error stop 5
   deallocate (s)
   print '(a)', 'ok'
   call capnp_message_free(m)
end program drv
EOF
  fc textesc_capnp.f90 drv.f90 -I "$MOD" -J . "${LIBARGS[@]}" -o drv
  run_drv
  cmp_out "$DRV_OUT" "ok"
  echo "g2 ok"
}

g3_g4() {
  local d which=$1
  d=$(work)
  cd "$d"
  cat > deflists.capnp << 'EOF'
@0x8333333333333301;
struct Elem { v @0 :Int32; }
struct DefLists {
  nums @0 :List(Int32) = [10, 20, 30];
  tags @1 :List(Text) = ["alpha", "beta"];
  elems @2 :List(Elem) = [(v = 7)];
}
EOF
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" deflists.capnp
  cat > drv.f90 << 'EOF'
program drv
   use capnp
   use deflists_capnp
   implicit none
   type(capnp_message_t), target :: m1, m2
   type(def_lists_t) :: h, h2
   type(elem_t) :: e
   character(len=:), allocatable :: s
   integer :: err
   call capnp_message_init_builder(m1, err)
   h = def_lists_new_root(m1, err)
   call def_lists_nums_set_elem(h, 0, 999, err)
   call capnp_message_init_builder(m2, err)
   h2 = def_lists_new_root(m2, err)
   call def_lists_tags_get_elem(h2, 0, s, err)
   e = def_lists_elems_get_elem(h2, 0, err)
   print '(i0,1x,a,1x,i0)', def_lists_nums_get_elem(h2, 0, err), s, elem_v_get(e)
   deallocate (s)
   call capnp_message_free(m1)
   call capnp_message_free(m2)
end program drv
EOF
  fc deflists_capnp.f90 drv.f90 -I "$MOD" -J . "${LIBARGS[@]}" -o drv
  capnp compile "${CAPNP_INC[@]}" -oc++ deflists.capnp
  cat > cpp.cpp << 'EOF'
#include "deflists.capnp.h"
#include <capnp/message.h>
#include <cstdio>
int main() {
  capnp::MallocMessageBuilder m1;
  auto d1 = m1.initRoot<DefLists>();
  d1.getNums().set(0, 999);
  capnp::MallocMessageBuilder m2;
  auto d2 = m2.initRoot<DefLists>();
  auto t = d2.getTags()[0];
  std::printf("%d %s %d\n", d2.getNums()[0], t.cStr(), d2.getElems()[0].getV());
}
EOF
  capnp_cxx_flags
  "$CXX" -std=c++17 cpp.cpp deflists.capnp.c++ "${CAPNP_PC[@]}" -o cppdrv
  local f c
  run_drv
  f=$DRV_OUT
  c=$(./cppdrv)
  cmp_out "$f" "$c"
  if [[ "$which" == g3 ]]; then
    # The fresh builder still sees the schema default after the other
    # builder wrote 999 into its own copy.
    [[ "$f" == 10\ * ]]
  fi
  echo "$which ok"
}

g5() {
  local d
  d=$(work)
  cd "$d"
  cat > ifaces.capnp << 'EOF'
@0x8555555555555501;
interface Base { ping @0 () -> (x :Int32); }
interface Sub extends(Base) { pong @0 () -> (y :Int32); }
EOF
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" ifaces.capnp
  grep -q 'extends(base_server_t)' ifaces_capnp.f90
  grep -q 'call base_dispatch' ifaces_capnp.f90
  cat > impl.f90 << 'EOF'
module impl
   use capnp
   use capnp_rpc
   use ifaces_capnp
   implicit none
   integer :: saw = 0
   type, extends(sub_server_t) :: srv_t
   contains
      procedure :: ping => do_ping
      procedure :: pong => do_pong
   end type srv_t
contains
   subroutine do_ping(self, ctx, params, results, err)
      class(srv_t), intent(inout) :: self
      type(rpc_call_ctx_t), intent(inout) :: ctx
      type(base_ping_params_t), intent(in) :: params
      type(base_ping_results_t), intent(in) :: results
      integer, intent(out) :: err
      saw = 1
      err = CAPNP_OK
      call base_ping_results_x_set(results, 7, err)
   end subroutine do_ping
   subroutine do_pong(self, ctx, params, results, err)
      class(srv_t), intent(inout) :: self
      type(rpc_call_ctx_t), intent(inout) :: ctx
      type(sub_pong_params_t), intent(in) :: params
      type(sub_pong_results_t), intent(in) :: results
      integer, intent(out) :: err
      saw = 2
      err = CAPNP_OK
   end subroutine do_pong
end module impl
program drv
   use capnp
   use capnp_rpc
   use ifaces_capnp
   use impl
   implicit none
   type(srv_t) :: srv
   type(rpc_call_ctx_t) :: ctx
   type(capnp_message_t), target :: m
   integer :: err
   call capnp_message_init_builder(m, err)
   ctx%rmsg => m
   ctx%interface_id = base_interface_id
   ctx%method_id = 0
   call srv%dispatch(ctx, err)
   print '(i0)', saw
   call capnp_message_free(m)
end program drv
EOF
  fc ifaces_capnp.f90 impl.f90 -I "$MOD" -J . "${LIBARGS[@]}" -o drv
  run_drv
  cmp_out "$DRV_OUT" "1"
  echo "g5 ok"
}

g6() {
  local d
  d=$(work)
  cd "$d"
  cat > generics.capnp << 'EOF'
@0x8666666666666601;
struct Item { v @0 :Int32; }
struct Result(T, E) {
  union { ok @0 :T; err @1 :E; }
}
struct Five(A, B, C, D, E) {
  a @0 :A; b @1 :B; c @2 :C; d @3 :D; e @4 :E;
}
interface GenIface(T) {
  ident @0 (x :T) -> (y :T);
}
interface GenMethod {
  identity @0 [T] (x :T) -> (y :T);
}
struct Use {
  r @0 :Result(Item, Text);
  f @1 :Five(Text, Text, Text, Text, Item);
  g @2 :GenIface(Text);
}
EOF
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" generics.capnp
  grep -q 'five_text_text_text_text_t' generics_capnp.f90
  capnp compile "${CAPNP_INC[@]}" -oc++ generics.capnp
  grep -q 'template <typename A, typename B, typename C, typename D, typename E>' generics.capnp.h
  cat > drv.f90 << 'EOF'
program drv
   use capnp
   use generics_capnp
   implicit none
   type(capnp_message_t), target :: m
   type(five_text_text_text_text_t) :: f
   type(item_t) :: e
   type(generics_gen_iface_ident_params_text_t) :: p
   type(gen_method_identity_params_t) :: gp
   type(capnp_ptr_t) :: q
   character(len=:), allocatable :: s
   integer :: err
   if (result_item_text_ok_tag /= 0) error stop 1
   if (result_item_text_err_tag /= 1) error stop 2
   call capnp_message_init_builder(m, err)
   f = five_text_text_text_text_new(m, err)
   e = five_text_text_text_text_e_init(f, err)
   call item_v_set(e, 5, err)
   e = five_text_text_text_text_e_get(f, err)
   if (item_v_get(e) /= 5) error stop 3
   p = generics_gen_iface_ident_params_text_new(m, err)
   call generics_gen_iface_ident_params_text_x_set(p, 'hi', err)
   call generics_gen_iface_ident_params_text_x_get(p, s, err)
   if (s /= 'hi') error stop 4
   deallocate (s)
   gp = gen_method_identity_params_new(m, err)
   q = gen_method_identity_params_x_get(gp, err)
   if (err /= 0) error stop 5
   if (q%kind /= CAPNP_PK_NULL) error stop 6
   print '(a)', 'ok'
   call capnp_message_free(m)
end program drv
EOF
  fc generics_capnp.f90 drv.f90 -I "$MOD" -J . "${LIBARGS[@]}" -o drv
  run_drv
  cmp_out "$DRV_OUT" "ok"
  echo "g6 ok"
}

g7() {
  local d
  d=$(work)
  cd "$d"
  mkdir -p dir_a dir_b
  cat > dir_a/dup.capnp << 'EOF'
@0x8a7a7a7a7a7a7a01;
struct FromA { n @0 :Int32; }
EOF
  cat > dir_b/dup.capnp << 'EOF'
@0x8b7b7b7b7b7b7b01;
struct FromB { n @0 :Int32; }
EOF
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" dir_a/dup.capnp dir_b/dup.capnp
  test -f dir_a/dup_capnp.f90
  test -f dir_b/dup_capnp.f90
  grep -q 'type :: from_a_t' dir_a/dup_capnp.f90
  grep -q 'type :: from_b_t' dir_b/dup_capnp.f90
  if grep -q 'from_b_t' dir_a/dup_capnp.f90; then
    echo "dir_a was overwritten" >&2
    exit 1
  fi
  if [[ $(grep -h '^module ' dir_a/dup_capnp.f90 dir_b/dup_capnp.f90 | sort -u | wc -l) != 2 ]]; then
    echo "modules of the same basename share a name" >&2
    exit 1
  fi
  echo "g7 ok"
}

g8() {
  local d
  d=$(work)
  cd "$d"
  cat > caps.capnp << 'EOF'
@0x8888888888888801;
interface Adder { add @0 (a :Int64, b :Int64) -> (sum :Int64); }
struct Box { svc @0 :Adder; }
EOF
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" caps.capnp
  cat > drv.f90 << 'EOF'
module impl
   use capnp
   use capnp_rpc
   use caps_capnp
   implicit none
   integer :: exported = -1
   type, extends(adder_server_t) :: srv_t
   contains
      procedure :: add => do_add
   end type srv_t
contains
   subroutine do_add(self, ctx, params, results, err)
      class(srv_t), intent(inout) :: self
      type(rpc_call_ctx_t), intent(inout) :: ctx
      type(adder_add_params_t), intent(in) :: params
      type(adder_add_results_t), intent(in) :: results
      integer, intent(out) :: err
      type(srv_t), pointer :: p
      allocate (p)
      exported = rpc_ctx_export_cap(ctx, p, err)
   end subroutine do_add
end module impl
program drv
   use capnp
   use capnp_rpc
   use caps_capnp
   use impl
   implicit none
   type(capnp_message_t), target :: m
   type(box_t) :: h
   type(adder_client_t) :: c, g
   type(capnp_ptr_t) :: ctab, cd
   type(srv_t) :: srv
   type(rpc_call_ctx_t) :: ctx
   integer :: err
   call capnp_message_init_builder(m, err)
   h = box_new_root(m, err)
   c%cap%kind = RPC_CAP_IMPORT
   c%cap%id = 11
   call box_svc_set(h, c, err)
   g = box_svc_get(h, err)
   print '(i0)', g%cap%id
   ctab = capnp_new_composite_list(m, 1_int64, 1, 1, err)
   cd = capnp_list_get_struct(ctab, 0, err)
   call capnp_set_u16(cd, 0_int64, 1_int32, err)
   call capnp_set_u32(cd, 4_int64, 7_int64, err)
   c%cap%id = 0
   call box_svc_set(h, c, err)
   g = box_svc_get(h, err, ctab)
   print '(i0,1x,i0)', err, g%cap%id
   ctx%rmsg => m
   ctx%interface_id = adder_interface_id
   ctx%method_id = 0
   call srv%dispatch(ctx, err)
   print '(i0)', exported
   call capnp_message_free(m)
end program drv
EOF
  fc caps_capnp.f90 drv.f90 -I "$MOD" -J . "${LIBARGS[@]}" -o drv
  run_drv
  cmp_out "$DRV_OUT" $'11\n0 7\n0'
  echo "g8 ok"
}

g9() {
  local d
  d=$(work)
  cd "$d"
  cat > surf.capnp << 'EOF'
@0x8999999999999901;
using Stream = import "/capnp/stream.capnp";
annotation myTag @0x8999999999999902 (struct) :Text;
struct S $myTag("hello") {
  n @0 :Int32;
  inner @1 :S;
  nums @2 :List(Int32);
  flags @3 :List(Bool);
  anyS @4 :AnyStruct;
  anyL @5 :AnyList;
  cap @6 :Capability;
}
interface Streamer {
  push @0 (x :Int64) -> stream;
}
EOF
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" surf.capnp
  grep -q 'call rpc_stream_send' surf_capnp.f90
  cat > drv.f90 << 'EOF'
program drv
   use capnp
   use capnp_any
   use surf_capnp
   implicit none
   type(capnp_message_t), target :: m
   type(s_t) :: h, inner, back
   type(any_struct_t) :: a
   type(any_list_t) :: al
   type(capability_client_t) :: cap
   type(capnp_ptr_t) :: lst
   integer :: err
   logical :: fl
   call capnp_message_init_builder(m, err)
   h = s_new_root(m, err)
   if (s_inner_has(h)) error stop 1
   if (surf_s_my_tag /= 'hello') error stop 2
   inner = s_inner_init(h, err)
   call s_n_set(inner, 4, err)
   call s_inner_set(h, inner, err)
   back = s_inner_get(h, err)
   if (s_n_get(back) /= 4) error stop 3
   lst = s_nums_init(h, 2_int64, err)
   call s_nums_set_elem(h, 1, 9, err)
   if (s_nums_get_elem(h, 1, err) /= 9) error stop 4
   lst = s_flags_init(h, 1_int64, err)
   call s_flags_set_elem(h, 0, .true., err)
   fl = s_flags_get_elem(h, 0, err)
   if (.not. fl) error stop 5
   a = s_any_s_get(h, err)
   al = s_any_l_get(h, err)
   cap = s_cap_get(h, err)
   if (err /= 0) error stop 6
   if (surf_my_tag_id /= -8531619134090667774_int64) error stop 7
   print '(a)', 'ok'
   call capnp_message_free(m)
end program drv
EOF
  fc surf_capnp.f90 drv.f90 -I "$MOD" -J . "${LIBARGS[@]}" -o drv
  run_drv
  cmp_out "$DRV_OUT" "ok"
  echo "g9 ok"
}

g10() {
  local d
  d=$(work)
  cd "$d"
  cat > blobann.capnp << 'EOF'
@0x8111111111111111;
annotation mark @0xaaaaaaaaaaaaaaaa(struct) :Data;
struct Host $mark(0x"0102") {
  v @0 :Int32;
}
EOF
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" blobann.capnp
  grep -q 'parameter :: BLOBANN_HOST_MARK(0:1)' blobann_capnp.f90
  grep -q '1, 2' blobann_capnp.f90
  fc -c blobann_capnp.f90 -I "$MOD" -J .
  echo "g10 ok"
}

g11() {
  local d
  d=$(work)
  cd "$d"
  cat > supers.capnp << 'EOF'
@0x8111111111111112;
interface Alpha {
  foo @0 (x :Int32) -> (r :Int32);
}
interface Beta {
  foo @0 (s :Text) -> (t :Text);
  baz @1 () -> ();
}
interface Child extends(Alpha, Beta) {
}
EOF
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" supers.capnp
  python3 - << 'PY'
from pathlib import Path
text = Path("supers_capnp.f90").read_text()
key = "ctx%interface_id == BETA_INTERFACE_ID"
start = text.find(key)
if start < 0:
    raise SystemExit("missing Beta dispatch")
end = text.find("end select", start)
chunk = text[start:end]
if "case (0)" not in chunk:
    raise SystemExit("Beta foo ordinal is not dispatched")
if "beta_foo_params_t" not in chunk or "beta_foo_results_t" not in chunk:
    raise SystemExit("Beta foo does not use Beta types")
if "alpha_foo_params_t" in chunk:
    raise SystemExit("Beta foo wrongly uses Alpha types")
PY
  cat > drv.f90 << 'EOF'
module impl
   use capnp
   use capnp_rpc
   use supers_capnp
   implicit none
   integer :: alpha_hits = 0, beta_hits = 0, baz_hits = 0
   type, extends(child_server_t) :: srv_t
   contains
      procedure :: foo => do_alpha_foo
      procedure :: beta_foo => do_beta_foo
      procedure :: baz => do_beta_baz
   end type srv_t
contains
   subroutine do_alpha_foo(self, ctx, params, results, err)
      class(srv_t), intent(inout) :: self
      type(rpc_call_ctx_t), intent(inout) :: ctx
      type(alpha_foo_params_t), intent(in) :: params
      type(alpha_foo_results_t), intent(in) :: results
      integer, intent(out) :: err
      alpha_hits = alpha_hits + 1
      err = CAPNP_OK
      call alpha_foo_results_r_set(results, alpha_foo_params_x_get(params) + 1_int32, err)
   end subroutine do_alpha_foo

   subroutine do_beta_foo(self, ctx, params, results, err)
      class(srv_t), intent(inout) :: self
      type(rpc_call_ctx_t), intent(inout) :: ctx
      type(beta_foo_params_t), intent(in) :: params
      type(beta_foo_results_t), intent(in) :: results
      integer, intent(out) :: err
      character(len=:), allocatable :: s
      beta_hits = beta_hits + 1
      call beta_foo_params_s_get(params, s, err)
      if (err == CAPNP_OK) call beta_foo_results_t_set(results, 'beta:'//s, err)
   end subroutine do_beta_foo

   subroutine do_beta_baz(self, ctx, params, results, err)
      class(srv_t), intent(inout) :: self
      type(rpc_call_ctx_t), intent(inout) :: ctx
      type(beta_baz_params_t), intent(in) :: params
      type(beta_baz_results_t), intent(in) :: results
      integer, intent(out) :: err
      baz_hits = baz_hits + 1
      err = CAPNP_OK
   end subroutine do_beta_baz
end module impl

program drv
   use capnp
   use capnp_posix
   use capnp_rpc
   use supers_capnp
   use impl
   implicit none
   type(rpc_conn_t), target :: cli, srv_conn
   type(srv_t), target :: srv
   class(rpc_server_t), pointer :: boot
   type(rpc_cap_t) :: root
   type(alpha_client_t) :: acli
   type(beta_client_t) :: bcli
   type(capnp_message_t), target :: m
   type(alpha_foo_params_t) :: ap
   type(alpha_foo_results_t) :: ar
   type(beta_foo_params_t) :: bp
   type(beta_foo_results_t) :: br
   type(beta_baz_params_t) :: zp
   type(beta_baz_results_t) :: zr
   character(len=:), allocatable :: txt
   integer(int64) :: q
   integer :: fa, fb, err, rval
   call px_socketpair(fa, fb, err)
   if (err /= CAPNP_OK) error stop 1
   boot => srv
   call rpc_conn_init(srv_conn, fb, boot)
   boot => null()
   call rpc_conn_init(cli, fa, boot)
   call rpc_bootstrap_send(cli, root, err)
   q = root%id
   call rpc_pump_once(srv_conn, err)
   call rpc_wait(cli, q, err)
   call rpc_result_cap(cli, q, [integer ::], root, err)
   call rpc_finish_send(cli, q, .true., err)
   call rpc_pump_once(srv_conn, err)

   acli%cap = root
   call alpha_foo_begin(cli, acli, m, ap, q, err)
   call alpha_foo_params_x_set(ap, 41_int32, err)
   call rpc_call_send(cli, m, err)
   call rpc_pump_once(srv_conn, err)
   call alpha_foo_wait(cli, q, ar, err)
   rval = alpha_foo_results_r_get(ar)
   call rpc_finish_send(cli, q, .false., err)
   call rpc_pump_once(srv_conn, err)

   bcli%cap = root
   call beta_foo_begin(cli, bcli, m, bp, q, err)
   call beta_foo_params_s_set(bp, 'hi', err)
   call rpc_call_send(cli, m, err)
   call rpc_pump_once(srv_conn, err)
   call beta_foo_wait(cli, q, br, err)
   call beta_foo_results_t_get(br, txt, err)
   call rpc_finish_send(cli, q, .false., err)
   call rpc_pump_once(srv_conn, err)

   call beta_baz_begin(cli, bcli, m, zp, q, err)
   call rpc_call_send(cli, m, err)
   call rpc_pump_once(srv_conn, err)
   call beta_baz_wait(cli, q, zr, err)
   call rpc_finish_send(cli, q, .false., err)
   call rpc_pump_once(srv_conn, err)

   print '(i0,1x,a,1x,i0,1x,i0,1x,i0)', rval, txt, alpha_hits, beta_hits, baz_hits
   deallocate (txt)
   call rpc_conn_close(cli)
   call rpc_conn_close(srv_conn)
end program drv
EOF
  fc supers_capnp.f90 drv.f90 -I "$MOD" -J . "${LIBARGS[@]}" -o drv
  run_drv
  cmp_out "$DRV_OUT" "42 beta:hi 1 1 1"
  echo "g11 ok"
}

g12() {
  local d
  d=$(work)
  cd "$d"
  cat > a.capnp << 'EOF'
@0x8444444444444401;
interface IA { ping @0 () -> (); }
struct SA { any @0 :AnyStruct; lst @1 :AnyList; cap @2 :Capability; }
EOF
  cat > b.capnp << 'EOF'
@0x8444444444444402;
using A = import "a.capnp";
interface IB { pong @0 (a :A.IA) -> (); }
struct SB { any @0 :AnyStruct; lst @1 :AnyList; cap @2 :Capability; a @3 :A.SA; }
EOF
  capnp compile "${CAPNP_INC[@]}" -oc++ a.capnp b.capnp
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" a.capnp b.capnp
  cat > drv.f90 << 'EOF'
program drv
   use capnp
   use capnp_any
   use a_capnp
   use b_capnp
   implicit none
   type(any_struct_t) :: s
   type(any_list_t) :: l
   type(capability_client_t) :: c
   print '(a)', 'ok'
end program drv
EOF
  fc a_capnp.f90 b_capnp.f90 drv.f90 -I "$MOD" -J . "${LIBARGS[@]}" -o drv
  run_drv
  cmp_out "$DRV_OUT" "ok"
  echo "g12 ok"
}

g13() {
  local d
  d=$(work)
  cd "$d"
  cat > an.capnp << 'EOF'
@0x8555555555555501;
using Cxx = import "/capnp/c++.capnp";
using Json = import "/capnp/compat/json.capnp";
struct S {
  f @0 :Int32 $Cxx.name("ff") $Json.name("jf");
}
EOF
  capnp compile "${CAPNP_INC[@]}" -oc++ an.capnp
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" an.capnp
  grep -q "parameter :: AN_S_F_CXX_NAME = 'ff'" an_capnp.f90
  grep -q "parameter :: AN_S_F_JSON_NAME = 'jf'" an_capnp.f90
  fc -c an_capnp.f90 -I "$MOD" -J .
  echo "g13 ok"
}

g14() {
  local d long
  d=$(work)
  cd "$d"
  long=$(printf 'abcdefghij%.0s' {1..20})
  cat > t.capnp << EOF
@0x8777777777777701;
const greet :Text = "café résumé naïve façade";
const long :Text = "$long";
struct S { t @0 :Text = "café résumé naïve façade"; }
EOF
  capnp compile "${CAPNP_INC[@]}" -oc++ t.capnp
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" t.capnp
  if awk 'length($0) > 132 { bad = 1 } END { exit !bad }' t_capnp.f90; then
    echo "generated line passes 132 columns" >&2
    exit 1
  fi
  cat > drv.f90 << EOF
program drv
   use capnp
   use t_capnp
   implicit none
   character(len=*), parameter :: want = 'café résumé naïve façade'
   character(len=*), parameter :: want_long = repeat('abcdefghij', 20)
   type(capnp_message_t), target :: m
   type(s_t) :: h
   character(len=:), allocatable :: got
   integer :: err
   call capnp_message_init_builder(m, err)
   h = s_new_root(m, err)
   call s_t_get(h, got, err)
   if (err /= 0) error stop 1
   print '(l1,l1,l1)', GREET == want, LONG == want_long, got == want
   deallocate (got)
   call capnp_message_free(m)
end program drv
EOF
  fc t_capnp.f90 drv.f90 -I "$MOD" -J . "${LIBARGS[@]}" -o drv
  run_drv
  cmp_out "$DRV_OUT" "TTT"
  echo "g14 ok"
}

g15() {
  local d
  d=$(work)
  cd "$d"
  cat > gen.capnp << 'EOF'
@0x8666666666666601;
interface Base { ping @0 () -> (); }
interface Gen(T) extends(Base) { get @0 () -> (v :T); }
struct Holder { g @0 :Gen(Text); }
EOF
  capnp compile "${CAPNP_INC[@]}" -oc++ gen.capnp
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" gen.capnp
  grep -q 'extends(base_server_t), abstract :: gen_gen_text_server_t' gen_capnp.f90
  fc -c gen_capnp.f90 -I "$MOD" -J .
  echo "g15 ok"
}

g16() {
  local d
  d=$(work)
  cd "$d"
  cat > imp.capnp << 'EOF'
@0x9200000000000002;
struct SessionLoginRequestPayloadData {}
struct SessionLoginResponseData {}
interface UserAuthenticationServiceApi {
  login @0 SessionLoginRequestPayloadData -> SessionLoginResponseData;
}
EOF
  cxx_ok imp.capnp
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" imp.capnp
  no_long_lines imp_capnp.f90
  fc -c imp_capnp.f90 -I "$MOD" -J .
  echo "g16 ok"
}

g17() {
  local d
  d=$(work)
  cd "$d"
  cat > inh_long.capnp << 'EOF'
@0x9100000000000031;
interface VeryLongPrimarySuperInterfaceAlpha { alpha @0 () -> (); }
interface VeryLongSecondarySuperInterfaceBeta {
  executeRemoteDiagnosticCheckOperationNow @0 () -> ();
}
interface CombinedChildServiceImplementation
    extends(VeryLongPrimarySuperInterfaceAlpha, VeryLongSecondarySuperInterfaceBeta) {}
EOF
  cat > f64long.capnp << 'EOF'
@0x9100000000000016;
annotation veryLongAnnotationIdentifierForFloatingPointBounds @0x9100000000000017 (field) :Float64;
annotation veryLongAnnotationIdentifierForSinglePrecisionBounds @0x9100000000000018 (field) :Float32;
struct VeryLongConfigurationContainerRecord {
  veryLongMeasurementThresholdField @0 :Float64
    $veryLongAnnotationIdentifierForFloatingPointBounds(-1.0)
    $veryLongAnnotationIdentifierForSinglePrecisionBounds(-2.5);
}
EOF
  cxx_ok inh_long.capnp f64long.capnp
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" inh_long.capnp f64long.capnp
  no_long_lines inh_long_capnp.f90 f64long_capnp.f90
  fc -c inh_long_capnp.f90 f64long_capnp.f90 -I "$MOD" -J .
  echo "g17 ok"
}

g18() {
  local d
  d=$(work)
  cd "$d"
  if [[ ! -f "$CAPNP_PREFIX/include/capnp/stream.capnp" ]]; then
    skip_dev "standard schemas are not installed"
  fi
  python3 - "$ROOT/app/capnpc_emit.f90" "$CAPNP_PREFIX/include" "$ROOT" << 'EOF'
import re
import sys

src, inc, root = sys.argv[1:]
text = open(src).read()
bad = 0
for name, base in (("bundled_capnp_schema", inc), ("runtime_module_taken", root)):
    m = re.search(r"function %s\b(.*?)end function %s" % (name, name), text, re.S)
    if not m:
        print("FAIL: %s not found" % name, file=sys.stderr)
        sys.exit(1)
    body = m.group(1)
    pairs = re.findall(r"(-?\d+)_int64\b[^\n!]*!\s*(\S+\.capnp)", body)
    if not pairs or len(pairs) != len(re.findall(r"-?\d+_int64", body)):
        print("FAIL: %s lists an id without its schema file" % name, file=sys.stderr)
        bad = 1
    for lit, rel in pairs:
        m = re.search(r"^@0x([0-9a-f]+);", open("%s/%s" % (base, rel)).read(), re.M)
        if int(lit) % 2**64 != int(m.group(1), 16):
            print("FAIL: %s id %s is not the id of %s" % (name, lit, rel), file=sys.stderr)
            bad = 1
sys.exit(bad)
EOF
  cat > strm.capnp << 'EOF'
@0x9100000000000032;
interface AbcdefghijklmnopqrstuvwxyzA {
  abcdefghijklmnopqrstuvwxyzAbc @0 (x :Int32) -> stream;
}
EOF
  cxx_ok strm.capnp
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" strm.capnp
  no_long_lines strm_capnp.f90
  fc -c strm_capnp.f90 -I "$MOD" -J .
  mkdir -p nested/capnp
  cat > nested/capnp/stream.capnp << 'EOF'
@0x9100000000000033;
struct NestedStream { x @0 :Int32; }
EOF
  cxx_ok nested/capnp/stream.capnp
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" nested/capnp/stream.capnp
  if ! grep -q '^module nested__capnp__stream_capnp$' nested/capnp/stream_capnp.f90; then
    echo 'FAIL: nested schema module name was not qualified' >&2
    exit 1
  fi
  fc -c nested/capnp/stream_capnp.f90 -I "$MOD" -J .
  if grep -q '^module stream_capnp$' nested/capnp/stream_capnp.f90; then
    echo 'FAIL: nested schema emitted a runtime module shim' >&2
    exit 1
  fi
  mkdir -p mycapnp
  cat > mycapnp/foo.capnp << 'EOF'
@0x9100000000000034;
struct Foo { x @0 :Int32; }
EOF
  cxx_ok mycapnp/foo.capnp
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" mycapnp/foo.capnp
  if ! grep -q '^module mycapnp__foo_capnp$' mycapnp/foo_capnp.f90; then
    echo 'FAIL: mycapnp schema module name was not qualified' >&2
    exit 1
  fi
  fc -c mycapnp/foo_capnp.f90 -I "$MOD" -J .
  local p
  for p in include vendor; do
    mkdir -p "$p/capnp"
    cp "$CAPNP_PREFIX/include/capnp/stream.capnp" "$p/capnp/stream.capnp"
    cat > imports.capnp << EOF
@0x9100000000000035;
using Stream = import "$p/capnp/stream.capnp";
struct Imports { result @0 :Stream.StreamResult; }
EOF
    capnp compile "${CAPNP_INC[@]}" -o"$BIN" imports.capnp
    if ! grep -Eq '^ *use stream_capnp(,|$)' imports_capnp.f90; then
      echo "FAIL: import of $p/capnp/stream.capnp did not use stream_capnp" >&2
      exit 1
    fi
    fc -c imports_capnp.f90 -I "$MOD" -J .
  done
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" vendor/capnp/stream.capnp
  if ! grep -q '^module stream_capnp$' vendor/capnp/stream_capnp.f90; then
    echo 'FAIL: bundled stream schema was not mapped to its runtime module' >&2
    exit 1
  fi
  echo "g18 ok"
}

g19() {
  local d
  d=$(work)
  cd "$d"
  cat > ov1.capnp << 'EOF'
@0x9300000000000003;
interface Base { foo @0 () -> (); }
interface Sub extends(Base) {
  foo @0 () -> ();
  subFoo @1 () -> ();
}
EOF
  cxx_ok ov1.capnp
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" ov1.capnp
  fc -c ov1_capnp.f90 -I "$MOD" -J .
  grep -q 'deferred :: sub_foo_0$' ov1_capnp.f90
  grep -q 'deferred :: sub_foo$' ov1_capnp.f90
  echo "g19 ok"
}

g20() {
  local d
  d=$(work)
  cd "$d"
  cat > ov2.capnp << 'EOF'
@0x9300000000000004;
interface Base { foo @0 () -> (); }
interface Mid extends(Base) { foo @0 () -> (); }
interface Top extends(Mid) { midFoo @0 () -> (); }
EOF
  cxx_ok ov2.capnp
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" ov2.capnp
  fc -c ov2_capnp.f90 -I "$MOD" -J .
  grep -q 'deferred :: mid_foo$' ov2_capnp.f90
  grep -q 'deferred :: top_mid_foo$' ov2_capnp.f90
  echo "g20 ok"
}

g21() {
  local d
  d=$(work)
  cd "$d"
  cat > a.capnp << 'EOF'
@0x9100000000000005;
annotation tag @0x9100000000000006 (struct, field) :Text;
struct SA $tag("sa") { x @0 :Int32; }
EOF
  cat > b.capnp << 'EOF'
@0x9100000000000007;
using A = import "a.capnp";
annotation tag @0x9100000000000008 (struct, field) :Int32;
struct SB $tag(42) { a @0 :A.SA; }
EOF
  cat > a2.capnp << 'EOF'
@0x9400000000000001;
annotation tag @0x9400000000000002 (struct, field) :Text;
struct S { foo @0 :Int32 $tag("from_a"); }
EOF
  cat > b2.capnp << 'EOF'
@0x9400000000000003;
using A = import "a2.capnp";
struct SFoo $A.tag("from_b") { s @0 :A.S; }
EOF
  cxx_ok a.capnp b.capnp
  cxx_ok a2.capnp b2.capnp
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" a.capnp b.capnp a2.capnp b2.capnp
  cat > drv.f90 << 'EOF'
program drv
   use capnp
   use b_capnp
   use b2_capnp
   implicit none
   print '(i0,1x,a)', B_S_B_TAG, B2_S_FOO_TAG
end program drv
EOF
  fc a_capnp.f90 b_capnp.f90 a2_capnp.f90 b2_capnp.f90 drv.f90 -I "$MOD" -J . \
    "${LIBARGS[@]}" -o drv
  run_drv
  cmp_out "$DRV_OUT" "42 from_b"
  echo "g21 ok"
}

g22() {
  local d
  d=$(work)
  cd "$d"
  cat > a.capnp << 'EOF'
@0x9100000000000020;
interface Gen(T) { get @0 () -> (v :T); }
EOF
  cat > b.capnp << 'EOF'
@0x9100000000000021;
using A = import "a.capnp";
struct SB { g @0 :A.Gen(Text); }
EOF
  cat > c.capnp << 'EOF'
@0x9100000000000022;
using A = import "a.capnp";
using B = import "b.capnp";
struct SC { b @0 :B.SB; g @1 :A.Gen(Text); }
EOF
  cxx_ok a.capnp b.capnp c.capnp
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" a.capnp b.capnp c.capnp
  fc -c a_capnp.f90 b_capnp.f90 c_capnp.f90 -I "$MOD" -J .
  echo "g22 ok"
}

g23() {
  local d
  d=$(work)
  cd "$d"
  cat > pay.capnp << 'EOF'
@0x9300000000000001;
struct Payload {
  id @0 :UInt64;
  cap @1 :Capability;
}
struct CapDescriptor { id @0 :UInt64; cap @1 :Capability; }
interface Svc { call @0 (p :Payload) -> (d :CapDescriptor); }
EOF
  cat > ifc.capnp << 'EOF'
@0x9300000000000010;
interface Svc { ping @0 () -> (); }
struct Msg { x @0 :Int32; }
EOF
  cat > usr.capnp << 'EOF'
@0x9300000000000011;
using I = import "ifc.capnp";
struct Payload { m @0 :I.Msg; }
struct CapDescriptor { x @0 :Int32; }
EOF
  cat > res.capnp << 'EOF'
@0x9300000000000020;
struct Payload { content @0 :Text; c @1 :Capability; }
const capResolve :Int32 = 5;
const capResolveId :Int32 = 6;
interface Svc { go @0 (p :Payload) -> (r :Capability); }
EOF
  cxx_ok pay.capnp ifc.capnp usr.capnp res.capnp
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" pay.capnp ifc.capnp usr.capnp res.capnp
  fc -c pay_capnp.f90 ifc_capnp.f90 usr_capnp.f90 res_capnp.f90 -I "$MOD" -J .
  echo "g23 ok"
}

g24() {
  local d
  d=$(work)
  cd "$d"
  mkdir dir_a dir_b
  cat > msg.capnp << 'EOF'
@0x9100000000000040;
annotation tag @0x9100000000000041 (field) :Int32;
struct Msg {
  union {
    ping @0 :Int32 $tag(100);
    pong @1 :Text;
  }
}
EOF
  cat > route.capnp << 'EOF'
@0x9100000000000042;
annotation get @0x9100000000000043 (field) :Text;
annotation set @0x9100000000000044 (field) :Text;
annotation has @0x9100000000000045 (field) :Text;
annotation init @0x9100000000000046 (field) :Text;
annotation t @0x9100000000000047 (field) :Text;
annotation new @0x9100000000000048 (field) :Text;
annotation dwords @0x9100000000000049 (field) :Text;
annotation pwords @0x910000000000004a (field) :Text;
annotation which @0x910000000000004b (field) :Text;
struct Route {
  struct User { x @0 :Int32; }
  user @0 :Text $get("g") $set("s") $has("h") $init("i") $t("t") $new("n")
    $dwords("d") $pwords("p") $which("w");
}
EOF
  cat > foo.capnp << 'EOF'
@0x910000000000004c;
using Cxx = import "/capnp/c++.capnp";
struct Foo $Cxx.name("CppFoo") {
  const name :Text = "logical-foo";
  x @0 :Int32;
}
EOF
  cat > nf.capnp << 'EOF'
@0x910000000000004d;
using Cxx = import "/capnp/c++.capnp";
struct Outer {
  enum Status $Cxx.name("CppStatus") { ok @0; }
  status @0 :Status $Cxx.name("cppStatus");
}
EOF
  cat > dir_a/ann.capnp << 'EOF'
@0x910000000000004e;
annotation name @0x910000000000004f (field) :Text;
EOF
  cat > dir_b/ann.capnp << 'EOF'
@0x9100000000000050;
annotation name @0x9100000000000051 (field) :Text;
EOF
  cat > use.capnp << 'EOF'
@0x9100000000000052;
using A = import "dir_a/ann.capnp";
using B = import "dir_b/ann.capnp";
struct S { f @0 :Int32 $A.name("af") $B.name("bf"); }
EOF
  cat > foo-bar.capnp << 'EOF'
@0x9100000000000053;
annotation name @0x9100000000000054 (field) :Text;
EOF
  cat > foobar.capnp << 'EOF'
@0x9100000000000055;
annotation name @0x9100000000000056 (field) :Text;
EOF
  cat > use2.capnp << 'EOF'
@0x9100000000000057;
using A = import "foo-bar.capnp";
using B = import "foobar.capnp";
struct S2 { f @0 :Int32 $A.name("af") $B.name("bf"); }
EOF
  cxx_ok msg.capnp route.capnp foo.capnp nf.capnp use.capnp use2.capnp
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" msg.capnp route.capnp foo.capnp nf.capnp \
    use.capnp use2.capnp
  fc -c msg_capnp.f90 route_capnp.f90 foo_capnp.f90 nf_capnp.f90 use_capnp.f90 \
    use2_capnp.f90 -I "$MOD" -J .
  echo "g24 ok"
}

g25() {
  local d
  d=$(work)
  cd "$d"
  cat > a.capnp << 'EOS'
@0xb100000000000001;
struct SvcTag { x @0 :Int32; }
struct SvcList { x @0 :Int32; }
interface SvcCall { ping @0 () -> (); }
struct Outer { interface Inner { ping @0 () -> (); } }
interface Base { hello @0 () -> (); }
interface Other { bye @0 () -> (); }
struct Pt { x @0 :Int32; }
struct Res { y @0 :Int32; }
EOS
  cat > b.capnp << 'EOS'
@0xb100000000000002;
using A = import "a.capnp";
annotation t @0xb100000000000003 (field) :Int32;
annotation clientT @0xb100000000000004 (field) :Int32;
struct Svc {
  tag @0 :A.SvcTag $t(1);
  list @1 :List(A.SvcList) $t(2);
  call @2 :A.SvcCall $clientT(3);
}
interface OuterInner extends(A.Outer.Inner) { pong @0 () -> (); }
struct BaseServer { x @0 :Int32; }
struct OtherServer { x @0 :Int32; }
interface Multi extends(A.Base, A.Other) { m @0 () -> (); }
const ptT :Int32 = 5;
const resT :Int32 = 6;
interface Caller { go @0 A.Pt -> A.Res; }
const svcTagDwords :A.SvcTag = (x = 7);
EOS
  cxx_ok a.capnp b.capnp
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" a.capnp b.capnp
  fc -c a_capnp.f90 b_capnp.f90 -I "$MOD" -J .
  echo "g25 ok"
}

g26() {
  local d
  d=$(work)
  cd "$d"
  cat > s1.capnp << 'EOS'
@0xd100000000000003;
annotation capResolve @0xd100000000000004 (struct) :Int32;
struct Slot { c @0 :Capability; }
EOS
  cat > s2.capnp << 'EOS'
@0xd100000000000001;
annotation resolve @0xd100000000000002 (struct) :Int32;
interface Ping { go @0 () -> (); }
struct Cap $resolve(1) {}
EOS
  cat > s3.capnp << 'EOS'
@0xd100000000000005;
annotation resolve @0xd100000000000006 (struct) :Int32;
struct Plain $resolve(1) { x @0 :Int32; }
EOS
  cxx_ok s1.capnp s2.capnp s3.capnp
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" s1.capnp s2.capnp s3.capnp
  fc -c s1_capnp.f90 s2_capnp.f90 s3_capnp.f90 -I "$MOD" -J .
  echo "g26 ok"
}

g27() {
  local d k prev_a prev_b sa sb n
  d=$(work)
  cd "$d"
  cat > d0.capnp << 'EOS'
@0xd200000000000000;
annotation tag @0xd200000000000100 (field) :Int32;
struct S0 { x @0 :Int32 $tag(1); }
EOS
  for k in $(seq 1 11); do
    if ((k == 1)); then
      prev_a=d0
      prev_b=d0
      sa=S0
      sb=S0
    else
      prev_a=da$((k - 1))
      prev_b=db$((k - 1))
      sa=SA$((k - 1))
      sb=SB$((k - 1))
    fi
    local hex_a hex_b
    hex_a=$(printf '%016x' $((0xd200000000001000 + k)))
    hex_b=$(printf '%016x' $((0xd200000000002000 + k)))
    cat > da$k.capnp << EOS
@0x$hex_a;
using P = import "$prev_a.capnp";
using Q = import "$prev_b.capnp";
struct SA$k { p @0 :P.$sa; q @1 :Q.$sb; }
EOS
    cat > db$k.capnp << EOS
@0x$hex_b;
using P = import "$prev_a.capnp";
using Q = import "$prev_b.capnp";
struct SB$k { p @0 :P.$sa; q @1 :Q.$sb; }
EOS
  done
  cat > top.capnp << 'EOS'
@0xd200000000003000;
using A = import "da11.capnp";
using B = import "db11.capnp";
annotation tag @0xd200000000003001 (field) :Int32;
struct Top { a @0 :A.SA11; b @1 :B.SB11; x @2 :Int32 $tag(2); }
EOS
  local files=(d0.capnp)
  for k in $(seq 1 11); do
    files+=(da$k.capnp db$k.capnp)
  done
  files+=(top.capnp)
  cxx_ok "${files[@]}"
  SECONDS=0
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" "${files[@]}"
  if ((SECONDS > 20)); then
    echo "generating the diamond chain took ${SECONDS}s" >&2
    exit 1
  fi
  n=$(wc -l < top_capnp.f90)
  if ((n > 300)); then
    echo "top_capnp.f90 has $n lines; the output grows with the import depth" >&2
    exit 1
  fi
  fc -c d0_capnp.f90 -I "$MOD" -J .
  for k in $(seq 1 11); do
    fc -c da${k}_capnp.f90 db${k}_capnp.f90 -I "$MOD" -J .
  done
  fc -c top_capnp.f90 -I "$MOD" -J .
  echo "g27 ok"
}

g28() {
  local d
  d=$(work)
  cd "$d"
  mkdir dir_a dir_b dir_c
  cat > dir_a/sub.capnp << 'EOS'
@0xe200000000000011;
struct FromA { y @0 :Int32; }
EOS
  cat > dir_b/sub.capnp << 'EOS'
@0xe200000000000012;
struct FromB { y @0 :Int32; }
EOS
  cat > dir_c/sub.capnp << 'EOS'
@0xe200000000000013;
struct FromC { y @0 :Int32; }
EOS
  cat > dir_a_sub.capnp << 'EOS'
@0xe200000000000099;
struct Flat { z @0 :Int32; }
EOS
  cat > only.capnp << 'EOS'
@0xe200000000000014;
struct Only { w @0 :Int32; }
EOS
  cat > use_all.capnp << 'EOS'
@0xe200000000000098;
using A = import "dir_a/sub.capnp";
using B = import "dir_b/sub.capnp";
using C = import "dir_c/sub.capnp";
using F = import "dir_a_sub.capnp";
using O = import "only.capnp";
struct All { a @0 :A.FromA; b @1 :B.FromB; c @2 :C.FromC; f @3 :F.Flat; o @4 :O.Only; }
EOS
  cat > use_both.capnp << 'EOS'
@0xe200000000000097;
using A = import "dir_a/sub.capnp";
using B = import "dir_b/sub.capnp";
struct Both { a @0 :A.FromA; b @1 :B.FromB; }
EOS
  cxx_ok dir_a/sub.capnp dir_b/sub.capnp dir_c/sub.capnp dir_a_sub.capnp only.capnp \
    use_all.capnp use_both.capnp
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" dir_a/sub.capnp dir_b/sub.capnp dir_c/sub.capnp \
    dir_a_sub.capnp only.capnp use_all.capnp use_both.capnp
  local mods
  mods=$(grep -h '^module ' dir_a/*.f90 dir_b/*.f90 dir_c/*.f90 dir_a_sub_capnp.f90 | sort -u | wc -l)
  if ((mods != 4)); then
    echo "expected four distinct modules for the four colliding schemas, got $mods" >&2
    exit 1
  fi
  grep -q '^module only_capnp$' only_capnp.f90 || {
    echo "a schema whose basename is unique keeps the plain module name" >&2
    exit 1
  }
  grep -q '^module use_all_capnp$' use_all_capnp.f90
  fc -c dir_a/*.f90 dir_b/*.f90 dir_c/*.f90 dir_a_sub_capnp.f90 only_capnp.f90 \
    -I "$MOD" -J .
  fc -c use_all_capnp.f90 use_both_capnp.f90 -I "$MOD" -J .
  echo "g28 ok"
}

g29() {
  local d
  d=$(work)
  cd "$d"
  cat > h.capnp << 'EOS'
@0xf100000000000001;
struct Box(T) { v @0 :T; }
struct Pair(A, B) { a @0 :A; b @1 :B; }
interface Gen(T) { put @0 (x :T) -> (y :T); }
struct TagList(T) { v @0 :T; }
interface Tag(T) { put @0 (x :T) -> (y :T); }
interface Base(T) { get @0 () -> (v :T); }
interface D1 extends(Base(Box(Text))) { a @0 () -> (); }
interface D2 extends(Base(Box(Data))) { b @0 () -> (); }
struct H {
  g1 @0 :Gen(Box(Text));
  g2 @1 :Gen(Box(Data));
  g3 @2 :Gen(Box(Box(Text)));
  g4 @3 :Gen(Box(Box(Data)));
  g5 @4 :Gen(Box(Box(Box(Text))));
  g6 @5 :Gen(Box(Box(Box(Data))));
  l1 @6 :List(Box(Text));
  l2 @7 :List(Box(Data));
  p1 @8 :Pair(Box(Text), Box(Data));
  p2 @9 :Pair(Box(Data), Box(Text));
  p3 @10 :Pair(Box(Text), Box(Text));
  t1 @11 :TagList(TagList(Text));
  t2 @12 :Tag(List(Text));
  t3 @13 :TagList(Text);
}
EOS
  cxx_ok h.capnp
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" h.capnp
  local ty
  for ty in box_text_t box_data_t h_box_box_text_t h_box_box_data_t h_box_box_box_text_t \
    h_box_box_box_data_t h_gen_box_text_client_t h_gen_box_data_client_t \
    h_gen_box_box_text_client_t h_gen_box_box_data_client_t \
    h_gen_box_box_box_text_client_t h_gen_box_box_box_data_client_t \
    pair_box_box_t pair_box_data_box_text_t pair_box_text_box_text_t; do
    grep -q "^   type :: $ty\$" h_capnp.f90 || {
      echo "h_capnp.f90 has no type $ty" >&2
      exit 1
    }
  done
  mkdir own
  cat > own/own.capnp << 'EOS'
@0xf100000000000002;
struct Box(T) { v @0 :T; }
struct BoxText { v @0 :Text; }
struct Own { b @0 :Box(Text); c @1 :BoxText; }
EOS
  (
    cd own
    cxx_ok own.capnp
    capnp compile "${CAPNP_INC[@]}" -o"$BIN" own.capnp
    grep -q '^   type :: box_text_t$' own_capnp.f90
    grep -q '^   type :: box_text_2_t$' own_capnp.f90
    fc -c own_capnp.f90 -I "$MOD" -J .
  )
  cat > drv.f90 << 'EOS'
program drv
   use capnp
   use h_capnp
   implicit none
   type(capnp_message_t), target :: m
   type(box_data_t) :: bd
   type(h_box_box_data_t) :: bbd
   type(box_text_t) :: bt
   integer :: err
   integer(int8), allocatable :: got(:)
   call capnp_message_init_builder(m, err)
   bd = box_data_new(m, err)
   call box_data_v_set(bd, [1_int8, 2_int8, 3_int8], err)
   call box_data_v_get(bd, got, err)
   bbd = h_box_box_data_new(m, err)
   bt = box_text_new(m, err)
   call box_text_v_set(bt, 'hi', err)
   print '(i0,1x,i0,1x,i0,1x,i0,1x,i0)', err, size(got), sum(int(got)), BOX_DATA_PWORDS, &
      H_BOX_BOX_DATA_PWORDS
   deallocate (got)
   call capnp_message_free(m)
end program drv
EOS
  fc h_capnp.f90 drv.f90 -I "$MOD" -J . "${LIBARGS[@]}" -o drv
  run_drv
  cmp_out "$DRV_OUT" "0 3 6 1 1"
  echo "g29 ok"
}

g30() {
  local d
  d=$(work)
  cd "$d"
  mkdir h1 h3 h4 h5 h6
  cat > h1/a.capnp << 'EOS'
@0xb200000000000001;
struct SvcTag { x @0 :Int32; }
const defTag :SvcTag = (x = 7);
EOS
  cat > h1/b.capnp << 'EOS'
@0xb200000000000002;
using A = import "a.capnp";
annotation t @0xb200000000000003 (field) :Int32;
struct Svc { tag @0 :List(A.SvcTag) $t(1); one @1 :A.SvcTag; }
const myTag :A.SvcTag = (x = 9);
const aTags :List(A.SvcTag) = [(x = 1)];
EOS
  cat > h3/a.capnp << 'EOS'
@0xb300000000000001;
struct Outer { interface Inner { ping @0 () -> (); } }
EOS
  cat > h3/b.capnp << 'EOS'
@0xb300000000000002;
using A = import "a.capnp";
struct OuterInner { x @0 :Int32; }
struct Holder { c @0 :A.Outer.Inner; l @1 :List(A.Outer.Inner); }
interface UseIt { take @0 (c :A.Outer.Inner) -> (c2 :A.Outer.Inner); }
EOS
  cat > h4/a.capnp << 'EOS'
@0xb400000000000001;
interface Base1 { one @0 () -> (); }
struct Outer { interface Inner { ping @0 () -> (); } }
EOS
  cat > h4/b.capnp << 'EOS'
@0xb400000000000002;
using A = import "a.capnp";
interface OuterInner { own @0 () -> (); }
interface Both extends(A.Outer.Inner, A.Base1, OuterInner) { both @0 () -> (); }
EOS
  cat > h5/a.capnp << 'EOS'
@0xb500000000000001;
struct SvcReq { x @0 :Int32; }
struct SvcRes { y @0 :Int32; }
EOS
  cat > h5/b.capnp << 'EOS'
@0xb500000000000002;
using A = import "a.capnp";
struct Svc { req @0 :Int32; res @1 :Int32; }
interface Api { call @0 A.SvcReq -> A.SvcRes; }
EOS
  cat > h6/a1.capnp << 'EOS'
@0xb600000000000001;
struct Outer { interface Inner { ping @0 () -> (); } struct Rec { x @0 :Int32; } }
EOS
  cat > h6/a2.capnp << 'EOS'
@0xb600000000000002;
interface OuterInner { pong @0 () -> (); }
struct OuterRec { y @0 :Int32; }
EOS
  cat > h6/b.capnp << 'EOS'
@0xb600000000000003;
using A1 = import "a1.capnp";
using A2 = import "a2.capnp";
struct Both {
  i1 @0 :A1.Outer.Inner;
  i2 @1 :A2.OuterInner;
  r1 @2 :A1.Outer.Rec;
  r2 @3 :A2.OuterRec;
}
EOS
  local sub
  for sub in h1 h3 h4 h5; do
    (
      cd "$sub"
      cxx_ok a.capnp b.capnp
      capnp compile "${CAPNP_INC[@]}" -o"$BIN" a.capnp b.capnp
      fc -c a_capnp.f90 b_capnp.f90 -I "$MOD" -J .
    )
  done
  (
    cd h6
    cxx_ok a1.capnp a2.capnp b.capnp
    capnp compile "${CAPNP_INC[@]}" -o"$BIN" a1.capnp a2.capnp b.capnp
    fc -c a1_capnp.f90 a2_capnp.f90 b_capnp.f90 -I "$MOD" -J .
  )
  echo "g30 ok"
}

g31() {
  local d
  d=$(work)
  cd "$d"
  cat > f0.capnp << 'EOS'
@0xc100000000000001;
interface Svc { req @0 () -> (); }
struct Tag { struct Gen { v @0 :Int32; } }
EOS
  cat > f1.capnp << 'EOS'
@0xc100000000000002;
using F0 = import "f0.capnp";
interface Gen { go @0 () -> (); }
interface Ping extends(F0.Svc, Gen) { ping @0 () -> (); }
struct List { svc @0 :F0.Tag.Gen; }
const tagGenPwords :Int32 = 1;
EOS
  cat > f2.capnp << 'EOS'
@0xc100000000000003;
using F0 = import "f0.capnp";
using F1 = import "f1.capnp";
annotation yT @0xc100000000000004 (field, struct) :Text;
struct PingList { req @0 :F1.List $yT("v"); }
interface Req extends(F0.Svc) { more @0 () -> (); }
const tagGenT :Int32 = 1;
EOS
  cxx_ok f0.capnp f1.capnp f2.capnp
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" f0.capnp f1.capnp f2.capnp
  fc -c f0_capnp.f90 f1_capnp.f90 f2_capnp.f90 -I "$MOD" -J .
  echo "g31 ok"
}

g32() {
  local d
  d=$(work)
  cd "$d"
  cat > a0.capnp << 'EOS'
@0xd200000000000001;
struct CapDescriptor { x @0 :Int32; }
struct Payload { y @0 :Int32; }
EOS
  cat > b.capnp << 'EOS'
@0xd200000000000002;
using A = import "a0.capnp";
interface Svc { go @0 (p :A.Payload) -> (c :A.CapDescriptor); }
EOS
  cat > c.capnp << 'EOS'
@0xd200000000000003;
using B = import "b.capnp";
interface Svc2 { go @0 () -> (); }
struct H { s @0 :B.Svc; }
EOS
  cxx_ok a0.capnp b.capnp c.capnp
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" a0.capnp b.capnp c.capnp
  fc -c a0_capnp.f90 b_capnp.f90 c_capnp.f90 -I "$MOD" -J .
  echo "g32 ok"
}

g33() {
  local d
  d=$(work)
  cd "$d"
  cat > a0.capnp << 'EOS'
@0xd300000000000001;
annotation tagT @0xd300000000000011 (struct) :Int32;
struct Svc $tagT(1) { x @0 :Int32; }
EOS
  cat > a1.capnp << 'EOS'
@0xd300000000000002;
using A0 = import "a0.capnp";
struct One { x @0 :Int32; s @1 :A0.Svc; }
EOS
  cat > a2.capnp << 'EOS'
@0xd300000000000003;
struct SvcTag { x @0 :Int32; }
EOS
  cat > c.capnp << 'EOS'
@0xd300000000000004;
using A1 = import "a1.capnp";
using A2 = import "a2.capnp";
struct C { a @0 :A1.One; s @1 :A2.SvcTag; }
EOS
  cxx_ok a0.capnp a1.capnp a2.capnp c.capnp
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" a0.capnp a1.capnp a2.capnp c.capnp
  fc -c a0_capnp.f90 a1_capnp.f90 a2_capnp.f90 c_capnp.f90 -I "$MOD" -J .
  echo "g33 ok"
}

g34() {
  local d
  d=$(work)
  cd "$d"
  cat > a0.capnp << 'EOS'
@0xd600000000000001;
annotation resolve @0xd600000000000011 (struct) :Int32;
struct Cap $resolve(1) { x @0 :Int32; }
EOS
  cat > a1.capnp << 'EOS'
@0xd600000000000002;
using A0 = import "a0.capnp";
interface Ping { go @0 (c :A0.Cap) -> (); }
struct Holder { c @0 :Capability; k @1 :A0.Cap; }
EOS
  cat > a2.capnp << 'EOS'
@0xd600000000000003;
using A1 = import "a1.capnp";
interface Pong { go @0 (h :A1.Holder) -> (); }
struct Keeper { c @0 :Capability; h @1 :A1.Holder; }
EOS
  cxx_ok a0.capnp a1.capnp a2.capnp
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" a0.capnp a1.capnp a2.capnp
  fc -c a0_capnp.f90 a1_capnp.f90 a2_capnp.f90 -I "$MOD" -J .
  echo "g34 ok"
}

g35() {
  local d
  d=$(work)
  cd "$d"
  cat > a1.capnp << 'EOS'
@0xd500000000000001;
using Cxx = import "/capnp/c++.capnp";
$Cxx.namespace("na1");
struct Item { a @0 :UInt64; }
EOS
  cat > a2.capnp << 'EOS'
@0xd500000000000002;
using Cxx = import "/capnp/c++.capnp";
$Cxx.namespace("na2");
struct Item { a @0 :UInt64; b @1 :UInt64; c @2 :Text; }
EOS
  cat > b.capnp << 'EOS'
@0xd500000000000003;
using A1 = import "a1.capnp";
using A2 = import "a2.capnp";
struct Box(T) { v @0 :T; }
struct H { x @0 :Box(A1.Item); y @1 :Box(A2.Item); }
EOS
  cxx_ok a1.capnp a2.capnp b.capnp
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" a1.capnp a2.capnp b.capnp
  fc -c a1_capnp.f90 a2_capnp.f90 b_capnp.f90 -I "$MOD" -J .
  grep -A3 'function box_item_v_init' b_capnp.f90 | grep -q 'a1_capnp_item_t) :: o' || {
    echo "Box(A1.Item) is not bound to A1.Item" >&2
    exit 1
  }
  grep -q 'a2_capnp_item_t) :: o' b_capnp.f90 || {
    echo "Box(A2.Item) is not bound to A2.Item" >&2
    exit 1
  }
  echo "g35 ok"
}

g36() {
  local d
  d=$(work)
  cd "$d"
  cat > h.capnp << 'EOS'
@0xd400000000000001;
struct Box(T) { val @0 :T; }
struct Outer(U) { inner @0 :Box(Box(U)); }
struct H { t @0 :Outer(Text); d @1 :Outer(Data); }
EOS
  cxx_ok h.capnp
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" h.capnp
  fc -c h_capnp.f90 -I "$MOD" -J .
  grep -A3 'function box_box_text_val_get' h_capnp.f90 | grep -q 'box_text_t) :: o' || {
    echo "Box(Box(Text)).val is not a Box(Text)" >&2
    exit 1
  }
  grep -A3 'function box_box_data_val_get' h_capnp.f90 | grep -q 'box_data_t) :: o' || {
    echo "Box(Box(Data)).val is not a Box(Data)" >&2
    exit 1
  }
  echo "g36 ok"
}

g37() {
  local d
  d=$(work)
  cd "$d"
  mkdir whole each
  for w in whole each; do
    mkdir "$w/dir_a" "$w/dir_b"
    cat > "$w/dir_a/sub.capnp" << 'EOS'
@0xe200000000000011;
struct FromA { y @0 :Int32; }
EOS
    cat > "$w/dir_b/sub.capnp" << 'EOS'
@0xe200000000000012;
struct FromB { y @0 :Int32; }
EOS
    cat > "$w/use_both.capnp" << 'EOS'
@0xe200000000000097;
using A = import "dir_a/sub.capnp";
using B = import "dir_b/sub.capnp";
struct Both { a @0 :A.FromA; b @1 :B.FromB; }
EOS
  done
  (
    cd whole
    cxx_ok dir_a/sub.capnp dir_b/sub.capnp use_both.capnp
    capnp compile "${CAPNP_INC[@]}" -o"$BIN" dir_a/sub.capnp dir_b/sub.capnp use_both.capnp
  )
  (
    cd each
    capnp compile "${CAPNP_INC[@]}" -o"$BIN" dir_a/sub.capnp
    capnp compile "${CAPNP_INC[@]}" -o"$BIN" dir_b/sub.capnp
    capnp compile "${CAPNP_INC[@]}" -o"$BIN" use_both.capnp
    fc -c dir_a/sub_capnp.f90 dir_b/sub_capnp.f90 use_both_capnp.f90 -I "$MOD" -J .
  )
  local f
  for f in dir_a/sub_capnp.f90 dir_b/sub_capnp.f90; do
    if [[ $(grep -m1 '^module ' whole/$f) != "$(grep -m1 '^module ' each/$f)" ]]; then
      echo "$f names its module by the request" >&2
      exit 1
    fi
  done
  if [[ $(grep -m1 '^module ' each/dir_a/sub_capnp.f90) == "$(grep -m1 '^module ' each/dir_b/sub_capnp.f90)" ]]; then
    echo "same-basename files share a module name" >&2
    exit 1
  fi
  if [[ $(grep '^   use ' whole/use_both_capnp.f90 | grep -v 'use capnp') != \
        "$(grep '^   use ' each/use_both_capnp.f90 | grep -v 'use capnp')" ]]; then
    echo "an importer names modules by the request" >&2
    exit 1
  fi
  mkdir solo
  (
    cd solo
    mkdir dir_a
    cp ../whole/dir_a/sub.capnp dir_a/
    capnp compile "${CAPNP_INC[@]}" -o"$BIN" dir_a/sub.capnp
    cat > user.f90 << 'EOS'
module user
   use sub_capnp
   type(from_a_t) :: x
end module user
EOS
    fc -c dir_a/sub_capnp.f90 user.f90 -I "$MOD" -J .
  )
  echo "g37 ok"
}

g38() {
  local d f
  d=$(work)
  cd "$d"
  cat > f0.capnp << 'EOS'
@0xf000000000112b41;
annotation tagTag(field, struct) :Text;
annotation xTag(field, struct) :Text;
interface ListList {
  tagT @0 (a :Int32) -> (b :Text);
  innerTag @1 (a :Int32) -> (b :Text);
}
struct Inner {
  innerTag @0 :ListList;
  innerSvc :group { a @1 :Int32; b @2 :Text; }
}
struct TagInner {
  item @0 :ListList;
}
const c0 :TagInner = ();
EOS
  cat > f1.capnp << 'EOS'
@0xf000000000112b42;
using F0 = import "f0.capnp";
annotation pingSvc(field, struct) :Text;
annotation outer(field, struct) :Text;
struct List {
  outerSvc @0 :Int32;
}
struct Tag {
  reqT @0 :Int32;
  reqSvc :group { a @1 :Int32; b @2 :Text; }
}
interface Ping(T) { put @0 (x :T) -> (y :T); }
struct Inst1 {
  g0 @0 :Ping(F0.TagInner);
}
const innerDwords :Int32 = 1;
const tagInnerDwords :Int32 = 1;
EOS
  cxx_ok f0.capnp f1.capnp
  mkdir whole each
  cp f0.capnp f1.capnp whole/
  cp f0.capnp f1.capnp each/
  (
    cd whole
    capnp compile "${CAPNP_INC[@]}" -o"$BIN" f0.capnp f1.capnp
    fc -c f0_capnp.f90 f1_capnp.f90 -I "$MOD" -J .
  )
  (
    cd each
    for f in f0.capnp f1.capnp; do
      capnp compile "${CAPNP_INC[@]}" -o"$BIN" "$f"
    done
    fc -c f0_capnp.f90 f1_capnp.f90 -I "$MOD" -J .
  )
  echo "g38 ok"
}

g39() {
  local d f
  d=$(work)
  cd "$d"
  cat > f0.capnp << 'EOS'
@0xf000000000139201;
annotation boxTag(field, struct) :Text;
struct ItemPing(T) { v @0 :T; }
struct Svc {
  innerTag @0 :Float64 $boxTag("v");
}
struct Tag {
  itemTag @0 :Svc $boxTag("v");
  xTag @1 :Int32;
  reqTag @2 :Float64;
  pingT @3 :UInt16;
  struct ItemBox { v @0 :Int32; }
  interface TagReq { ping @0 () -> (); }
}
struct ItemInner {
  ping @0 :List(Tag) $boxTag("v");
  itemT @1 :Data;
  outerTag @2 :Svc;
  struct ReqGen { v @0 :Int32; }
  outerT :group { a @3 :Int32; b @4 :Text; }
}
interface Gen(T) { put @0 (x :T) -> (y :T); }
struct Inst0 {
  g0 @0 :ItemPing(Tag.ItemBox);
  g1 @1 :Gen(ItemPing(Text));
}
EOS
  cat > f1.capnp << 'EOS'
@0xf000000000139202;
using F0 = import "f0.capnp";
annotation outerTag(field, struct) :Text;
interface BoxReq extends(F0.Tag.TagReq) {
  pingTag @0 (a :Int32) -> (b :Text);
  tagT @1 (a :Int32) -> (b :Text);
  pingT @2 F0.ItemInner -> F0.ItemInner.ReqGen;
}
struct Item {
  outerT @0 :F0.Tag.TagReq $outerTag("v");
  pingTag @1 :F0.Tag;
  outer @2 :F0.ItemInner $outerTag("v");
  struct BoxReq { v @0 :Int32; }
  interface Req { ping @0 () -> (); }
}
interface Box(T) { put @0 (x :T) -> (y :T); }
struct Inner(T) { v @0 :T; }
struct Req {
  pingT @0 :Float64 $outerTag("v");
  reqT @1 :List(Item.BoxReq);
  svcTag :group { a @2 :Int32; b @3 :Text; }
}
struct Inst1 {
  g0 @0 :Inner(F0.ItemPing(Data));
  g1 @1 :F0.Gen(F0.Tag.ItemBox);
}
EOS
  cat > f2.capnp << 'EOS'
@0xf000000000139203;
using F0 = import "f0.capnp";
using F1 = import "f1.capnp";
annotation tagTag(field, struct) :Text;
struct GenGen {
  innerT @0 :F1.BoxReq;
  tagSvc @1 :Text $tagTag("v");
  pingT @2 :F1.Req;
  svcTag @3 :Bool;
}
struct TagOuter {
  tagTag @0 :F0.Tag.ItemBox;
  reqT @1 :Float64 $tagTag("v");
  pingTag @2 :F1.Item $tagTag("v");
  itemSvc @3 :Int32 $tagTag("v");
  struct PingInner { v @0 :Int32; }
}
interface PingTag {
  xT @0 (a :Int32) -> (b :Text);
  ySvc @1 (a :Int32) -> (b :Text);
  itemTag @2 (a :Int32) -> (b :Text);
}
struct SvcInner {
  box @0 :F0.Tag.ItemBox $tagTag("v");
  reqT @1 :F0.Tag.ItemBox;
  svc @2 :Bool $tagTag("v");
  innerSvc @3 :TagOuter $tagTag("v");
  struct ListSvc { v @0 :Int32; }
}
struct Inst2 {
  g0 @0 :F0.ItemPing(F0.ItemPing(Text));
  g1 @1 :F0.Gen(F1.Inner(Data));
}
const c2 :F0.ItemInner = ();
EOS
  cxx_ok f0.capnp f1.capnp f2.capnp
  mkdir whole each
  cp f0.capnp f1.capnp f2.capnp whole/
  cp f0.capnp f1.capnp f2.capnp each/
  (
    cd whole
    capnp compile "${CAPNP_INC[@]}" -o"$BIN" f0.capnp f1.capnp f2.capnp
    fc -c f0_capnp.f90 f1_capnp.f90 f2_capnp.f90 -I "$MOD" -J .
  )
  (
    cd each
    for f in f0.capnp f1.capnp f2.capnp; do
      capnp compile "${CAPNP_INC[@]}" -o"$BIN" "$f"
    done
    fc -c f0_capnp.f90 f1_capnp.f90 f2_capnp.f90 -I "$MOD" -J .
  )
  echo "g39 ok"
}

g40() {
  local d f
  d=$(work)
  cd "$d"
  cat > f0.capnp << 'EOS'
@0xf000000000139b51;
annotation reqT(field, struct) :Text;
annotation tagT(field, struct) :Text;
interface Ping {
  xTag @0 (a :Int32) -> (b :Text);
  inner @1 (a :Int32) -> (b :Text);
  ping @2 (a :Int32) -> (b :Text);
}
struct ReqTag {
  pingT @0 :Ping;
  xT @1 :Ping;
  innerT @2 :Ping;
  boxT @3 :Ping $tagT("v");
}
EOS
  cat > f1.capnp << 'EOS'
@0xf000000000139b52;
using F0 = import "f0.capnp";
annotation svc(field, struct) :Text;
annotation reqT(field, struct) :Text;
struct Box {
  tagT @0 :F0.ReqTag;
  x @1 :List(F0.ReqTag);
  boxSvc @2 :Int32 $svc("v");
  xSvc :group { a @3 :Int32; b @4 :Text; }
}
struct Gen {
  tagTag @0 :List(Box) $svc("v");
  reqT @1 :UInt16 $reqT("v");
  svc @2 :F0.Ping;
  tagT @3 :Box;
}
interface SvcGen {
  svcTag @0 F0.ReqTag -> Gen;
  ping @1 Box -> Box;
  xT @2 Box -> F0.ReqTag;
}
EOS
  cxx_ok f0.capnp f1.capnp
  mkdir whole each
  cp f0.capnp f1.capnp whole/
  cp f0.capnp f1.capnp each/
  (
    cd whole
    capnp compile "${CAPNP_INC[@]}" -o"$BIN" f0.capnp f1.capnp
    fc -c f0_capnp.f90 f1_capnp.f90 -I "$MOD" -J .
  )
  (
    cd each
    for f in f0.capnp f1.capnp; do
      capnp compile "${CAPNP_INC[@]}" -o"$BIN" "$f"
    done
    fc -c f0_capnp.f90 f1_capnp.f90 -I "$MOD" -J .
  )
  echo "g40 ok"
}

# Compile the schema files in $1.. as one request and one file at a time, and
# check that the generated modules compile in both layouts.
compile_both() {
  local f
  rm -rf whole each
  mkdir whole each
  cp "$@" whole/
  cp "$@" each/
  (
    cd whole
    capnp compile "${CAPNP_INC[@]}" -o"$BIN" "$@"
    fc -c "${@/.capnp/_capnp.f90}" -I "$MOD" -J .
  )
  (
    cd each
    for f in "$@"; do
      capnp compile "${CAPNP_INC[@]}" -o"$BIN" "$f"
    done
    fc -c "${@/.capnp/_capnp.f90}" -I "$MOD" -J .
  )
}

g41() {
  local d
  d=$(work)
  cd "$d"
  cat > f0.capnp << 'EOS'
@0x9000000000000c41;
struct CapDescriptor { x @0 :Int32; }
struct HasCap { c @0 :Capability; }
EOS
  cat > f1.capnp << 'EOS'
@0x9000000000000c42;
using F0 = import "f0.capnp";
struct Wrap { cd @0 :F0.CapDescriptor; }
EOS
  cat > f2.capnp << 'EOS'
@0x9000000000000c43;
using F1 = import "f1.capnp";
interface Svc { m0 @0 () -> (w :F1.Wrap); }
EOS
  cxx_ok f0.capnp f1.capnp f2.capnp
  compile_both f0.capnp f1.capnp f2.capnp
  echo "g41 ok"
}

g42() {
  local d
  d=$(work)
  cd "$d"
  cat > a0.capnp << 'EOS'
@0x90000000000005d1;
struct Keep { v @0 :Int32; }
interface Unused { m0 @0 () -> (); }
EOS
  cat > a1.capnp << 'EOS'
@0x90000000000005d2;
using F0 = import "a0.capnp";
struct Wrap { k @0 :F0.Keep; }
struct CapDescriptor { x @0 :Int32; }
EOS
  cat > b0.capnp << 'EOS'
@0x9000000000001391;
struct Keep { v @0 :Int32; }
struct CapDescriptor { x @0 :Int32; }
EOS
  cat > b1.capnp << 'EOS'
@0x9000000000001392;
using F0 = import "b0.capnp";
struct Wrap { k @0 :F0.Keep; }
interface Svc { m0 @0 () -> (); }
struct Held { c @0 :Capability; }
EOS
  cxx_ok a0.capnp a1.capnp
  cxx_ok b0.capnp b1.capnp
  mkdir pa pb
  cp a0.capnp a1.capnp pa/
  cp b0.capnp b1.capnp pb/
  (cd pa && compile_both a0.capnp a1.capnp)
  (cd pb && compile_both b0.capnp b1.capnp)
  echo "g42 ok"
}

g43() {
  local d
  d=$(work)
  cd "$d"
  cat > f1.capnp << 'EOS'
@0x9000000000002302;
annotation box (struct) :Data;
struct Keep { x @0 :Int32; }
const nodeBox :Text = "a";
EOS
  cat > f2.capnp << 'EOS'
@0x9000000000002303;
using F1 = import "f1.capnp";
struct Node $F1.box(0x"0102") { k @0 :F1.Keep; }
EOS
  cxx_ok f1.capnp f2.capnp
  compile_both f1.capnp f2.capnp
  echo "g43 ok"
}

g44() {
  local d
  d=$(work)
  cd "$d"
  cat > a.capnp << 'EOS'
@0xab00000000000007;
struct AnyStruct { x @0 :Int32; }
struct AnyList { x @0 :Int32; }
struct CapabilityClient { x @0 :Int32; }
EOS
  cat > b.capnp << 'EOS'
@0xab00000000000008;
struct Holder {
  s @0 :AnyStruct;
  l @1 :AnyList;
  c @2 :Capability;
}
EOS
  cxx_ok a.capnp b.capnp
  compile_both a.capnp b.capnp
  echo "g44 ok"
}

g45() {
  local d
  d=$(work)
  cd "$d"
  cat > a.capnp << 'EOS'
@0xab00000000000001;
struct Inner { v @0 :Int32; }
struct S { foo @0 :Text; bar @1 :Inner; nums @2 :List(Int32); ptr @3 :AnyPointer; }
const sFooHas :Int32 = 1;
const sBarSet :Int32 = 2;
const sNumsSetElem :Int32 = 3;
const sPtrHas :Int32 = 4;
const sNumsHas :Int32 = 5;
EOS
  cxx_ok a.capnp
  compile_both a.capnp
  (
    cd whole
    grep -q 'parameter :: S_FOO_HAS = 1' a_capnp.f90
    grep -q 'parameter :: S_BAR_SET = 2' a_capnp.f90
  )
  echo "g45 ok"
}

g46() {
  local d
  d=$(work)
  cd "$d"
  cat > f0.capnp << 'EOS'
@0xf0000000000008d1;
struct Inner(T) { v @0 :T; }
interface InnerY(T) { put @0 (x :T) -> (y :T); }
struct Y { v @0 :Int32; }
struct Inst0 { g0 @0 :Inner(Y); }
EOS
  cxx_ok f0.capnp
  compile_both f0.capnp
  (
    cd whole
    grep -q 'type :: inner_y_t$' f0_capnp.f90
    grep -q 'function inner_y_new(' f0_capnp.f90
    if grep -q 'inner_y_2' f0_capnp.f90; then
      echo "g46: an instantiation was renamed for an interface" >&2
      exit 1
    fi
  )
  echo "g46 ok"
}

g47() {
  local d
  d=$(work)
  cd "$d"
  cat > h.capnp << 'EOS'
@0xf0000000000008d2;
struct Box(T) { v @0 :T; }
struct Pair(A, B) { a @0 :A; b @1 :B; }
struct Five(A, B, C, D, E) { a @0 :A; e @1 :E; }
struct H {
  b @0 :Box(Box(Text));
  d @1 :Box(Box(Data));
  p @2 :Pair(Box(Text), Box(Data));
  l @3 :Box(List(Box(Text)));
  f @4 :Five(Text, Text, Text, Text, Data);
}
EOS
  cxx_ok h.capnp
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" h.capnp
  cat > drv.f90 << 'EOS'
program drv
   use capnp
   use h_capnp
   implicit none
   type(capnp_message_t), target :: m
   type(box_box_t) :: b
   type(box_box_data_t) :: d
   type(pair_box_box_t) :: p
   type(box_list_box_t) :: l
   type(five_text_text_text_text_t) :: f
   integer :: err
   call capnp_message_init_builder(m, err)
   b = box_box_new(m, err)
   if (err /= 0) error stop 1
   d = box_box_data_new(m, err)
   if (err /= 0) error stop 2
   p = pair_box_box_new(m, err)
   if (err /= 0) error stop 3
   l = box_list_box_new(m, err)
   if (err /= 0) error stop 4
   f = five_text_text_text_text_new(m, err)
   if (err /= 0) error stop 5
   print '(a)', 'ok'
   call capnp_message_free(m)
end program drv
EOS
  fc h_capnp.f90 drv.f90 -I "$MOD" -J . "${LIBARGS[@]}" -o drv
  cmp_out "$(./drv)" "ok"
  echo "g47 ok"
}

g48() {
  local d
  d=$(work)
  cd "$d"
  cat > a.capnp << 'EOS'
@0xab00000000000002;
struct Inner { v @0 :Int32; }
struct S { foo @0 :Text; bar @1 :Inner; nums @2 :List(Int32); }
const sFooHas :Inner = (v = 1);
const sBarSet :Inner = (v = 2);
const sNumsSetElem :Inner = (v = 3);
EOS
  cxx_ok a.capnp
  compile_both a.capnp
  (
    cd whole
    local n
    for n in s_foo_has s_bar_set s_nums_set_elem; do
      grep -q "^   function $n(err) result" a_capnp.f90 || {
        echo "g48: the constant accessor $n was renamed" >&2
        exit 1
      }
    done
  )
  echo "g48 ok"
}

g49() {
  local d
  d=$(work)
  cd "$d"
  cat > a.capnp << 'EOS'
@0xab00000000000003;
struct Inner { v @0 :Int32; }
struct S { foo @0 :Text; bar @1 :Inner; }
const sFooHas :Text = "s_foo_has";
const sBarSet :Text = "it's s_bar_set";
const sNote :Inner = (v = 1);
EOS
  cxx_ok a.capnp
  compile_both a.capnp
  (
    cd whole
    grep -q "parameter :: S_FOO_HAS = 's_foo_has'" a_capnp.f90
    grep -q "parameter :: S_BAR_SET = 'it''s s_bar_set'" a_capnp.f90
  )
  echo "g49 ok"
}

g50() {
  local d a b
  d=$(work)
  cd "$d"
  {
    echo '@0xab00000000000004;'
    echo 'struct Inner { v @0 :Int32; }'
    for a in a b c d e f g h i j; do
      for b in a b c d e f g h; do
        echo "struct Q$a$b { foo @0 :Text; }"
        echo "const q${a}${b}FooHas :Inner = (v = 1);"
      done
    done
  } > a.capnp
  cxx_ok a.capnp
  compile_both a.capnp
  (
    cd whole
    grep -q '^   function qaa_foo_has(err) result' a_capnp.f90
    grep -q '^   function qjh_foo_has(err) result' a_capnp.f90
  )
  echo "g50 ok"
}

g51() {
  local d
  d=$(work)
  cd "$d"
  cat > h.capnp << 'EOS'
@0xf0000000000008d3;
struct Item {
  n @0 :Int32;
  xPing :group { a @1 :Int32; }
}
struct ItemX(T) { v @0 :T; }
struct Ping { v @0 :Int32; }
struct H { i @0 :ItemX(Ping); }
EOS
  cxx_ok h.capnp
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" h.capnp
  cat > drv.f90 << 'EOS'
program drv
   use capnp
   use h_capnp
   implicit none
   type(capnp_message_t), target :: m
   type(item_x_ping_t) :: x
   integer :: err
   call capnp_message_init_builder(m, err)
   x = item_x_ping_new(m, err)
   if (err /= 0) error stop 1
   print '(a)', 'ok'
   call capnp_message_free(m)
end program drv
EOS
  fc h_capnp.f90 drv.f90 -I "$MOD" -J . "${LIBARGS[@]}" -o drv
  cmp_out "$(./drv)" "ok"
  echo "g51 ok"
}

# A client of an interface that extends another is accepted by the setter of
# a field of the superclass type, as the C++ setter takes Sub::Client.
g52() {
  local d
  d=$(work)
  cd "$d"
  cat > a.capnp << 'EOS'
@0xf0000000000008e1;
interface Base { a @0 () -> (); }
struct HoldA { b @0 :Base; }
EOS
  cat > b.capnp << 'EOS'
@0xf0000000000008e2;
using A = import "a.capnp";
interface Mid extends(A.Base) { m @0 () -> (); }
interface Sub extends(Mid) { s @0 () -> (); }
struct HoldB { b @0 :A.Base; m @1 :Mid; }
EOS
  cxx_ok a.capnp b.capnp
  cat > cpp.cpp << 'EOS'
#include "a.capnp.h"
#include "b.capnp.h"
#include <capnp/message.h>
void f(HoldA::Builder ha, HoldB::Builder hb) {
  ha.setB(Sub::Client(nullptr));
  hb.setB(Sub::Client(nullptr));
  hb.setM(Sub::Client(nullptr));
}
EOS
  "$CXX" -std=c++17 -fsyntax-only "${CAPNP_PC[@]}" cpp.cpp
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" a.capnp b.capnp
  cat > drv.f90 << 'EOS'
program drv
   use capnp
   use a_capnp
   use b_capnp
   implicit none
   type(capnp_message_t), target :: m
   type(hold_a_t) :: ha
   type(hold_b_t) :: hb
   type(sub_client_t) :: s
   type(capnp_ptr_t) :: q1, q2, q3
   integer :: err
   call capnp_message_init_builder(m, err)
   ha = hold_a_new_root(m, err)
   hb = hold_b_new(m, err)
   s%cap%id = 7_int64
   call hold_a_b_set(ha, s, err)
   if (err /= 0) error stop 1
   call hold_b_b_set(hb, s, err)
   if (err /= 0) error stop 2
   call hold_b_m_set(hb, s, err)
   if (err /= 0) error stop 3
   q1 = capnp_getp(ha%p, 0, err)
   q2 = capnp_getp(hb%p, 0, err)
   q3 = capnp_getp(hb%p, 1, err)
   print '(i0,1x,i0,1x,i0)', q1%capidx, q2%capidx, q3%capidx
   call capnp_message_free(m)
end program drv
EOS
  fc a_capnp.f90 b_capnp.f90 drv.f90 -I "$MOD" -J . "${LIBARGS[@]}" -o drv
  run_drv
  cmp_out "$DRV_OUT" "7 7 7"
  echo "g52 ok"
}

# Selecting a union group zeroes every slot of the group, nested groups and
# inner discriminants included, as the C++ init<Group> accessor does.
g53() {
  local d c
  d=$(work)
  cd "$d"
  cat > gsel.capnp << 'EOS'
@0xf0000000000008e3;
struct Gs {
  pre @0 :UInt8;
  union {
    a :group {
      x @1 :UInt64;
      t @2 :Text;
    }
    b :group {
      f @3 :Bool;
      h @4 :UInt16;
      s @5 :Text;
      inner :union {
        p @6 :Int32;
        q @7 :Data;
      }
      deep :group { d @8 :Float64; }
    }
  }
  post @9 :UInt32;
  c :group {
    cu :union {
      n @10 :Void;
      g :group { gg @11 :UInt8; k @12 :Text; }
    }
  }
}
EOS
  capnp compile "${CAPNP_INC[@]}" -oc++ gsel.capnp
  cat > cpp.cpp << 'EOS'
#include "gsel.capnp.h"
#include <capnp/message.h>
#include <capnp/any.h>
#include <cstdio>
static void fill(Gs::Builder r) {
  auto s = capnp::AnyStruct::Builder(kj::cp(r));
  for (auto& b : s.getDataSection()) b = 0xff;
  auto ps = s.getPointerSection();
  for (unsigned i = 0; i < ps.size(); i++) ps[i].setAs<capnp::Text>("zz");
}
static void dump(Gs::Builder r) {
  auto s = capnp::AnyStruct::Builder(kj::cp(r));
  for (auto b : s.getDataSection()) std::printf("%02x", (unsigned)b);
  std::printf(" ");
  auto ps = s.getPointerSection();
  for (unsigned i = 0; i < ps.size(); i++) std::printf("%c", ps[i].isNull() ? 'N' : 'P');
  std::printf("\n");
}
int main() {
  capnp::MallocMessageBuilder m;
  auto r = m.initRoot<Gs>();
  fill(r); r.initB(); dump(r);
  fill(r); r.initA(); dump(r);
  fill(r); r.getC().getCu().initG(); dump(r);
}
EOS
  capnp_cxx_flags
  "$CXX" -std=c++17 cpp.cpp gsel.capnp.c++ "${CAPNP_PC[@]}" -o cppdrv
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" gsel.capnp
  cat > drv.f90 << 'EOS'
program drv
   use capnp
   use gsel_capnp
   implicit none
   type(capnp_message_t), target :: m
   type(gs_t) :: h
   integer :: err
   call capnp_message_init_builder(m, err)
   h = gs_new_root(m, err)
   call fill(h); call gs_b_select(h, err); call dump(h, err)
   call fill(h); call gs_a_select(h, err); call dump(h, err)
   call fill(h); call gs_c_cu_g_select(h, err); call dump(h, err)
   call capnp_message_free(m)
contains
   subroutine fill(h)
      type(gs_t), intent(in) :: h
      integer :: i, e
      do i = 0, h%p%dwords - 1
         call capnp_set_i64(h%p, int(i, int64)*8_int64, -1_int64, e)
      end do
      do i = 0, h%p%pwords - 1
         call capnp_set_text(h%p, i, 'zz', e)
      end do
   end subroutine fill
   subroutine dump(h, err)
      type(gs_t), intent(in) :: h
      integer, intent(in) :: err
      type(capnp_ptr_t) :: q
      character(len=:), allocatable :: line
      character(len=2) :: hx
      integer :: i, e
      if (err /= 0) error stop 1
      line = ''
      do i = 0, h%p%dwords*8 - 1
         write (hx, '(z2.2)') capnp_get_u8(h%p, int(i, int64))
         line = line//hx
      end do
      line = line//' '
      do i = 0, h%p%pwords - 1
         q = capnp_getp(h%p, i, e)
         if (q%kind == CAPNP_PK_NULL) then
            line = line//'N'
         else
            line = line//'P'
         end if
      end do
      print '(a)', line
   end subroutine dump
end program drv
EOS
  fc gsel_capnp.f90 drv.f90 -I "$MOD" -J . "${LIBARGS[@]}" -o drv
  run_drv
  c=$(./cppdrv)
  cmp_out "$(tr 'A-F' 'a-f' <<< "$DRV_OUT")" "$c"
  echo "g53 ok"
}

# A client of an interface with two superclasses converts to the second
# superclass client and calls that superclass's method. capnpc-c++.c++
# 2447-2451 derives Client from every superclass Client. The base emitter
# has no conversion, so this driver does not compile there.
g54() {
  local d
  d=$(work)
  cd "$d"
  cat > pair.capnp << 'EOF'
@0xf0000000000008f4;
interface Alpha {
  ping @0 () -> (x :Int32);
}
interface Beta {
  pong @0 () -> (y :Int32);
}
interface Child extends(Alpha, Beta) {}
EOF
  cxx_ok pair.capnp
  cat > cpp.cpp << 'EOF'
#include "pair.capnp.h"
static void take_beta(Beta::Client) {}
void f(Child::Client c) { take_beta(c); }
EOF
  "$CXX" -std=c++17 -fsyntax-only "${CAPNP_PC[@]}" cpp.cpp
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" pair.capnp
  grep -q 'function child_as_beta(' pair_capnp.f90
  cat > drv.f90 << 'EOF'
module impl
   use capnp
   use capnp_rpc
   use pair_capnp
   implicit none
   integer :: beta_hits = 0
   type, extends(child_server_t) :: srv_t
   contains
      procedure :: ping => do_ping
      procedure :: pong => do_pong
   end type srv_t
contains
   subroutine do_ping(self, ctx, params, results, err)
      class(srv_t), intent(inout) :: self
      type(rpc_call_ctx_t), intent(inout) :: ctx
      type(alpha_ping_params_t), intent(in) :: params
      type(alpha_ping_results_t), intent(in) :: results
      integer, intent(out) :: err
      err = CAPNP_OK
   end subroutine do_ping
   subroutine do_pong(self, ctx, params, results, err)
      class(srv_t), intent(inout) :: self
      type(rpc_call_ctx_t), intent(inout) :: ctx
      type(beta_pong_params_t), intent(in) :: params
      type(beta_pong_results_t), intent(in) :: results
      integer, intent(out) :: err
      beta_hits = beta_hits + 1
      err = CAPNP_OK
      call beta_pong_results_y_set(results, 9_int32, err)
   end subroutine do_pong
end module impl
program drv
   use capnp
   use capnp_posix
   use capnp_rpc
   use pair_capnp
   use impl
   implicit none
   type(rpc_conn_t), target :: cli, srv_conn
   type(srv_t), target :: srv
   class(rpc_server_t), pointer :: boot
   type(rpc_cap_t) :: root
   type(child_client_t) :: ccli
   type(beta_client_t) :: bcli
   type(capnp_message_t), target :: m
   type(beta_pong_params_t) :: bp
   type(beta_pong_results_t) :: br
   integer(int64) :: q
   integer :: fa, fb, err, y
   call px_socketpair(fa, fb, err)
   if (err /= CAPNP_OK) error stop 1
   boot => srv
   call rpc_conn_init(srv_conn, fb, boot)
   boot => null()
   call rpc_conn_init(cli, fa, boot)
   call rpc_bootstrap_send(cli, root, err)
   q = root%id
   call rpc_pump_once(srv_conn, err)
   call rpc_wait(cli, q, err)
   call rpc_result_cap(cli, q, [integer ::], root, err)
   call rpc_finish_send(cli, q, .true., err)
   call rpc_pump_once(srv_conn, err)
   ccli%cap = root
   bcli = child_as_beta(ccli)
   if (bcli%cap%id /= ccli%cap%id) error stop 2
   call beta_pong_begin(cli, bcli, m, bp, q, err)
   if (err /= CAPNP_OK) error stop 3
   call rpc_call_send(cli, m, err)
   call rpc_pump_once(srv_conn, err)
   call beta_pong_wait(cli, q, br, err)
   y = beta_pong_results_y_get(br)
   call rpc_finish_send(cli, q, .false., err)
   call rpc_pump_once(srv_conn, err)
   print '(i0,1x,i0)', y, beta_hits
   call rpc_conn_close(cli)
   call rpc_conn_close(srv_conn)
end program drv
EOF
  fc pair_capnp.f90 drv.f90 -I "$MOD" -J . "${LIBARGS[@]}" -o drv
  run_drv
  cmp_out "$DRV_OUT" "9 1"
  echo "g54 ok"
}

# Inactive union getters return the kind error. C++ requires which()==arm.
g55() {
  local d c
  d=$(work)
  cd "$d"
  cat > u.capnp << 'EOS'
@0xf0000000000008e4;
enum Hue { red @0; blue @1; }
struct Box { x @0 :Int32; }
interface Ping { ping @0 (); }
struct U {
  union {
    a @0 :Int32 = 7;
    b @1 :Text = "fallback";
    d @2 :Data;
    s @3 :Box;
    l @4 :List(Int32);
    ap @5 :AnyPointer;
    flag @6 :Bool = true;
    ratio @7 :Float64 = 1.5;
    hue @8 :Hue = blue;
    cap @9 :Ping;
    as @10 :AnyStruct;
    al @11 :AnyList;
    ac @12 :Capability;
  }
}
EOS
  capnp compile "${CAPNP_INC[@]}" -oc++ u.capnp
  cat > cpp.cpp << 'EOS'
#include "u.capnp.h"
#include <capnp/message.h>
#include <kj/exception.h>
#include <cstdio>
static int grab_a(U::Builder r, int& v) {
  try { v = r.getA(); return 0; }
  catch (const kj::Exception&) { return 2; }
}
static int grab_b(U::Builder r) {
  try { (void)r.getB(); return 0; }
  catch (const kj::Exception&) { return 2; }
}
int main() {
  capnp::MallocMessageBuilder m;
  auto r = m.initRoot<U>();
  r.setB("hello");
  int v = 0;
  int ea = grab_a(r, v);
  r.setA(9);
  int eb = grab_b(r);
  int eok = grab_a(r, v);
  std::printf("%d %d %d %d\n", ea, eb, eok, v);
}
EOS
  capnp_cxx_flags
  # The interface arm pulls Capability::Server. Static libcapnp-rpc must
  # precede libcapnp.
  local cxxlibs=() w
  for w in "${CAPNP_PC[@]}"; do
    if [[ "$w" == -lcapnp ]]; then
      cxxlibs+=(-lcapnp-rpc -lcapnp -lkj-async)
    else
      cxxlibs+=("$w")
    fi
  done
  # -DKJ_DEBUG stays last so CXXFLAGS cannot compile KJ_IREQUIRE out.
  "$CXX" ${CXXFLAGS-} -std=c++17 cpp.cpp u.capnp.c++ "${cxxlibs[@]}" -o cppdrv -DKJ_DEBUG
  c=$(./cppdrv)
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" u.capnp
  cat > drv.f90 << 'EOS'
program drv
   use capnp
   use capnp_any, only: any_struct_t, any_list_t, capability_client_t
   use u_capnp
   implicit none
   type(capnp_message_t), target :: m
   type(u_t) :: h
   type(box_t) :: bx
   type(capnp_ptr_t) :: q
   type(ping_client_t) :: cli
   type(any_struct_t) :: as
   type(any_list_t) :: al
   type(capability_client_t) :: ac
   integer(int8), allocatable :: bytes(:)
   character(len=:), allocatable :: s
   integer(int32) :: a
   logical :: flag
   real(real64) :: ratio
   integer :: err, ea, eb, eok, ef, er, eh, ed, es, el, eap, ecap, eas, eal, eac
   integer :: hue
   call capnp_message_init_builder(m, err)
   h = u_new_root(m, err)
   if (err /= 0) error stop 1
   call u_b_set(h, 'hello', err)
   if (err /= 0) error stop 2
   a = u_a_get(h, ea)
   flag = u_flag_get(h, ef)
   ratio = u_ratio_get(h, er)
   hue = u_hue_get(h, eh)
   call u_d_get(h, bytes, ed)
   bx = u_s_get(h, es)
   q = u_l_get(h, el)
   q = u_ap_get(h, eap)
   cli = u_cap_get(h, ecap)
   as = u_as_get(h, eas)
   al = u_al_get(h, eal)
   ac = u_ac_get(h, eac)
   if (ea /= 2 .or. ef /= 2 .or. er /= 2 .or. eh /= 2) error stop 3
   if (ed /= 2 .or. es /= 2 .or. el /= 2 .or. eap /= 2) error stop 4
   if (ecap /= 2 .or. eas /= 2 .or. eal /= 2 .or. eac /= 2) error stop 5
   call u_b_get(h, s, err)
   if (err /= 0 .or. s /= 'hello') error stop 6
   call u_a_set(h, 9_int32, err)
   if (err /= 0) error stop 7
   a = u_a_get(h, eok)
   call u_b_get(h, s, eb)
   print '(i0,1x,i0,1x,i0,1x,i0)', ea, eb, eok, a
   a = u_a_get(h)
   if (a /= 9_int32) error stop 8
   call u_b_set(h, 'z', err)
   if (err /= 0) error stop 9
   a = u_a_get(h)
   if (a /= 0_int32) error stop 10
   call capnp_message_free(m)
end program drv
EOS
  fc u_capnp.f90 drv.f90 -I "$MOD" -J . "${LIBARGS[@]}" -o drv
  run_drv
  cmp_out "$DRV_OUT" "$c"
  # A group arm has no reader. Children, and a union nested in the group,
  # follow the enclosing which().
  cat > ug.capnp << 'EOS'
@0xf0000000000008e7;
struct U {
  union {
    g :group {
      n @0 :Int32 = 4;
      t @1 :Text;
      union {
        p @2 :Int32;
        q @3 :Text;
      }
    }
    b @4 :Int32;
  }
}
EOS
  capnp compile "${CAPNP_INC[@]}" -oc++ ug.capnp
  cat > cpp.cpp << 'EOS'
#include "ug.capnp.h"
#include <capnp/message.h>
#include <kj/exception.h>
#include <cstdio>
static int grab_n(U::Builder r, int& v) {
  try { v = r.getG().getN(); return 0; }
  catch (const kj::Exception&) { v = 0; return 2; }
}
static int grab_t(U::Builder r) {
  try { return r.getG().hasT() ? 1 : 0; }
  catch (const kj::Exception&) { return 0; }
}
static int grab_p(U::Builder r, int& v) {
  try { v = r.getG().getP(); return 0; }
  catch (const kj::Exception&) { v = 0; return 2; }
}
static int grab_q(U::Builder r) {
  try { return r.getG().hasQ() ? 1 : 0; }
  catch (const kj::Exception&) { return 0; }
}
int main() {
  capnp::MallocMessageBuilder m;
  auto r = m.initRoot<U>();
  r.initG().setT("hi");
  r.getG().setQ("q");
  r.setB(9);
  int n = 0, p = 0;
  int en = grab_n(r, n);
  int ht = grab_t(r);
  int ep = grab_p(r, p);
  int hq = grab_q(r);
  r.initG();
  r.getG().setN(6);
  r.getG().setP(3);
  int n2 = 0, p2 = 0;
  int en2 = grab_n(r, n2);
  int ht2 = grab_t(r);
  int ep2 = grab_p(r, p2);
  int hq2 = grab_q(r);
  std::printf("%d %d %d %d %d %d %d %d %d %d %d %d\n",
              en, n, ht, ep, p, hq, en2, n2, ht2, ep2, p2, hq2);
}
EOS
  "$CXX" ${CXXFLAGS-} -std=c++17 cpp.cpp ug.capnp.c++ "${CAPNP_PC[@]}" -o cppdrv -DKJ_DEBUG
  c=$(./cppdrv)
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" ug.capnp
  cat > drv.f90 << 'EOS'
program drv
   use capnp
   use ug_capnp
   implicit none
   type(capnp_message_t), target :: m
   type(u_t) :: h
   integer(int32) :: n, p, n2, p2
   integer :: err, en, ep, en2, ep2, ht, hq, ht2, hq2
   call capnp_message_init_builder(m, err)
   h = u_new_root(m, err)
   if (err /= 0) error stop 1
   call u_g_select(h, err)
   if (err /= 0) error stop 2
   call u_g_t_set(h, 'hi', err)
   if (err /= 0) error stop 3
   call u_g_q_set(h, 'q', err)
   if (err /= 0) error stop 4
   call u_b_set(h, 9_int32, err)
   if (err /= 0) error stop 5
   n = u_g_n_get(h, en)
   ht = merge(1, 0, u_g_t_has(h))
   p = u_g_p_get(h, ep)
   hq = merge(1, 0, u_g_q_has(h))
   call u_g_select(h, err)
   if (err /= 0) error stop 6
   call u_g_n_set(h, 6_int32, err)
   if (err /= 0) error stop 7
   call u_g_p_set(h, 3_int32, err)
   if (err /= 0) error stop 8
   n2 = u_g_n_get(h, en2)
   ht2 = merge(1, 0, u_g_t_has(h))
   p2 = u_g_p_get(h, ep2)
   hq2 = merge(1, 0, u_g_q_has(h))
   print '(i0,1x,i0,1x,i0,1x,i0,1x,i0,1x,i0,1x,i0,1x,i0,1x,i0,1x,i0,1x,i0,1x,i0)', &
      en, n, ht, ep, p, hq, en2, n2, ht2, ep2, p2, hq2
   call capnp_message_free(m)
end program drv
EOS
  fc ug_capnp.f90 drv.f90 -I "$MOD" -J . "${LIBARGS[@]}" -o drv
  run_drv
  cmp_out "$DRV_OUT" "$c"
  echo "g55 ok"
}

# Inactive union pointer arms report has false.
g56() {
  local d c
  d=$(work)
  cd "$d"
  cat > u.capnp << 'EOS'
@0xf0000000000008e5;
struct U {
  union {
    a @0 :Int32 = 7;
    b @1 :Text = "fallback";
  }
}
EOS
  capnp compile "${CAPNP_INC[@]}" -oc++ u.capnp
  cat > cpp.cpp << 'EOS'
#include "u.capnp.h"
#include <capnp/message.h>
#include <cstdio>
int main() {
  capnp::MallocMessageBuilder m;
  auto r = m.initRoot<U>();
  r.setB("hello");
  std::printf("%d\n", r.hasB() ? 1 : 0);
  r.setA(9);
  std::printf("%d\n", r.hasB() ? 1 : 0);
  r.setB("x");
  std::printf("%d\n", r.hasB() ? 1 : 0);
}
EOS
  capnp_cxx_flags
  "$CXX" -std=c++17 cpp.cpp u.capnp.c++ "${CAPNP_PC[@]}" -o cppdrv
  c=$(./cppdrv)
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" u.capnp
  cat > drv.f90 << 'EOS'
program drv
   use capnp
   use u_capnp
   implicit none
   type(capnp_message_t), target :: m
   type(u_t) :: h
   integer :: err
   call capnp_message_init_builder(m, err)
   h = u_new_root(m, err)
   if (err /= 0) error stop 1
   call u_b_set(h, 'hello', err)
   if (err /= 0) error stop 2
   print '(i0)', merge(1, 0, u_b_has(h))
   call u_a_set(h, 9_int32, err)
   if (err /= 0) error stop 3
   print '(i0)', merge(1, 0, u_b_has(h))
   call u_b_set(h, 'x', err)
   if (err /= 0) error stop 4
   print '(i0)', merge(1, 0, u_b_has(h))
   call capnp_message_free(m)
end program drv
EOS
  fc u_capnp.f90 drv.f90 -I "$MOD" -J . "${LIBARGS[@]}" -o drv
  run_drv
  cmp_out "$DRV_OUT" "$c"
  echo "g56 ok"
}


# has reads the pointer word. A bad target stays present and get reports it.
g57() {
  local d
  d=$(work)
  cd "$d"
  cat > node.capnp << 'EOS'
@0xf0000000000008e6;
struct Node {
  next @0 :Node;
}
EOS
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" node.capnp
  cat > drv.f90 << 'EOS'
program drv
   use capnp
   use node_capnp
   implicit none
   type(capnp_message_t), target :: m, r
   type(node_t) :: n(0:70), cur, nxt, bad, one, leaf
   integer(int8), allocatable :: bytes(:)
   integer :: i, err, hops
   integer(int64) :: slot, w
   logical :: deep_ok
   call capnp_message_init_builder(m, err)
   do i = 0, 70
      n(i) = node_new(m, err)
      if (err /= 0) error stop 1
   end do
   do i = 0, 69
      call capnp_setp(n(i)%p, 0, n(i+1)%p, err)
      if (err /= 0) error stop 2
   end do
   call capnp_set_root(m, n(0)%p, err)
   if (err /= 0) error stop 3
   call capnp_serialize_bytes(m, bytes, err)
   if (err /= 0) error stop 4
   call capnp_deserialize_bytes(bytes, r, err)
   if (err /= 0) error stop 5
   cur = node_read_root(r, err)
   if (err /= 0) error stop 6
   deep_ok = .false.
   hops = 0
   do i = 1, 80
      if (.not. node_next_has(cur)) error stop 7
      nxt = node_next_get(cur, err)
      if (err /= 0) then
         if (err /= 3) error stop 8
         deep_ok = .true.
         exit
      end if
      hops = hops + 1
      cur = nxt
   end do
   if (.not. deep_ok .or. hops < 60) error stop 9
   call capnp_message_free(m)
   call capnp_message_free(r)
   call capnp_message_init_builder(m, err)
   one = node_new(m, err)
   leaf = node_new(m, err)
   call capnp_setp(one%p, 0, leaf%p, err)
   if (err /= 0) error stop 10
   call capnp_set_root(m, one%p, err)
   call capnp_serialize_bytes(m, bytes, err)
   call capnp_deserialize_bytes(bytes, r, err)
   one = node_read_root(r, err)
   if (.not. node_next_has(one)) error stop 11
   nxt = node_next_get(one, err)
   if (err /= 0) error stop 12
   if (node_next_has(nxt)) error stop 13
   call capnp_message_free(m)
   call capnp_message_free(r)
   call capnp_message_init_builder(m, err)
   bad = node_new_root(m, err)
   if (err /= 0) error stop 14
   slot = bad%p%off + int(bad%p%dwords, int64)*8_int64
   w = wp_make_struct(100000_int32, 1_int32, 0_int32)
   call cp_put_i64(bad%p%msg%segs(bad%p%seg)%bytes, slot, w)
   call capnp_serialize_bytes(m, bytes, err)
   if (err /= 0) error stop 15
   call capnp_deserialize_bytes(bytes, r, err)
   if (err /= 0) error stop 16
   bad = node_read_root(r, err)
   if (err /= 0) error stop 17
   if (.not. node_next_has(bad)) error stop 18
   nxt = node_next_get(bad, err)
   if (err /= 1) error stop 19
   print '(a)', 'ok'
   call capnp_message_free(m)
   call capnp_message_free(r)
end program drv
EOS
  fc node_capnp.f90 drv.f90 -I "$MOD" -J . "${LIBARGS[@]}" -o drv
  run_drv
  cmp_out "$DRV_OUT" "ok"
  echo "g57 ok"
}

case "$CASE" in
  g1) g1 ;;
  g2) g2 ;;
  g3) g3_g4 g3 ;;
  g4) g3_g4 g4 ;;
  g5) g5 ;;
  g6) g6 ;;
  g7) g7 ;;
  g8) g8 ;;
  g9) g9 ;;
  g10) g10 ;;
  g11) g11 ;;
  g12) g12 ;;
  g13) g13 ;;
  g14) g14 ;;
  g15) g15 ;;
  g16) g16 ;;
  g17) g17 ;;
  g18) g18 ;;
  g19) g19 ;;
  g20) g20 ;;
  g21) g21 ;;
  g22) g22 ;;
  g23) g23 ;;
  g24) g24 ;;
  g25) g25 ;;
  g26) g26 ;;
  g27) g27 ;;
  g28) g28 ;;
  g29) g29 ;;
  g30) g30 ;;
  g31) g31 ;;
  g32) g32 ;;
  g33) g33 ;;
  g34) g34 ;;
  g35) g35 ;;
  g36) g36 ;;
  g37) g37 ;;
  g38) g38 ;;
  g39) g39 ;;
  g40) g40 ;;
  g41) g41 ;;
  g42) g42 ;;
  g43) g43 ;;
  g44) g44 ;;
  g45) g45 ;;
  g46) g46 ;;
  g47) g47 ;;
  g48) g48 ;;
  g49) g49 ;;
  g50) g50 ;;
  g51) g51 ;;
  g52) g52 ;;
  g53) g53 ;;
  g54) g54 ;;
  g55) g55 ;;
  g56) g56 ;;
  g57) g57 ;;
  all)
    g1; g2; g3_g4 g3; g3_g4 g4; g5; g6; g7; g8; g9; g10; g11; g12; g13; g14; g15; g16; g17; g18; g19; g20; g21; g22; g23; g24; g25; g26; g27; g28; g29; g30; g31; g32; g33; g34; g35; g36; g37; g38; g39; g40; g41; g42; g43; g44; g45; g46; g47; g48; g49; g50; g51; g52; g53; g54; g55; g56; g57
    ;;
  *)
    echo "usage: $0 g1|g2|g3|g4|g5|g6|g7|g8|g9|g10|g11|g12|g13|g14|g15|g16|g17|g18|g19|g20|g21|g22|g23|g24|g25|g26|g27|g28|g29|g30|g31|g32|g33|g34|g35|g36|g37|g38|g39|g40|g41|g42|g43|g44|g45|g46|g47|g48|g49|g50|g51|g52|g53|g54|g55|g56|g57|all" >&2
    exit 2
    ;;
esac
