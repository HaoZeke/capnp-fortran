#!/usr/bin/env bash
# Regression for the schema-compiler gaps. Each case fails on the base
# emitter and matches Cap'n Proto C++ 1.4.0 after the fix.
set -euo pipefail

START=$PWD
ROOT=$(cd "$(dirname "$0")/.." && pwd)
cd "$ROOT"

# Usage: codegen_parity.sh [g1..g11|g12|g13|all] [build-dir]
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

for tool in capnp "$FC" "$CXX" pkg-config python3; do
  command -v "$tool" > /dev/null || die "$tool not found on PATH (Cap'n Proto 1.4.0 and a Fortran and C++ compiler are required)"
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
capnp_cxx_flags() {
  local inc lib others word
  if ((${#CAPNP_PC[@]})); then
    return 0
  fi
  pkg-config "${PC_DEFS[@]}" --exists capnp \
    || die "pkg-config cannot find capnp.pc; install the Cap'n Proto development files or set CAPNP_PREFIX"
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

g1() {
  local d
  d=$(work)
  cd "$d"
  cat > minmax.capnp << 'EOF'
@0x8111111111111101;
struct MinMax {
  i8 @0 :Int8 = -128;
  i16 @1 :Int16 = -32768;
  i32 @2 :Int32 = -2147483648;
  i64 @3 :Int64 = -9223372036854775808;
  z @4 :Float64 = -0;
}
const minI64 :Int64 = -9223372036854775808;
EOF
  capnp compile "${CAPNP_INC[@]}" -o"$BIN" minmax.capnp
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

g34() {
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

case "$CASE" in
  g1) g1 ;;
  g2) g2 ;;
  g3) g34 g3 ;;
  g4) g34 g4 ;;
  g5) g5 ;;
  g6) g6 ;;
  g7) g7 ;;
  g8) g8 ;;
  g9) g9 ;;
  g10) g10 ;;
  g11) g11 ;;
  g12) g12 ;;
  g13) g13 ;;
  all)
    g1; g2; g34 g3; g34 g4; g5; g6; g7; g8; g9; g10; g11; g12; g13
    ;;
  *)
    echo "usage: $0 g1|g2|g3|g4|g5|g6|g7|g8|g9|g10|g11|g12|g13|all" >&2
    exit 2
    ;;
esac
