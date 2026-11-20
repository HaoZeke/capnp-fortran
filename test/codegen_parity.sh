#!/usr/bin/env bash
# Regression for the schema-compiler gaps. Each case fails on the base
# emitter and matches Cap'n Proto C++ 1.4.0 after the fix.
set -euo pipefail

ROOT=$(cd "$(dirname "$0")/.." && pwd)
cd "$ROOT"

BIN=$(find build -name capnpc-fortran -type f -printf '%T@ %p\n' | sort -n | tail -1 | cut -d' ' -f2-)
MOD=$(find build -name capnp.mod -printf '%T@ %h\n' | sort -n | tail -1 | cut -d' ' -f2-)
LIB=$(find build -name libcapnp.a -printf '%T@ %p\n' | sort -n | tail -1 | cut -d' ' -f2-)
if [[ -z "${BIN}" || -z "${MOD}" || -z "${LIB}" ]]; then
  echo "codegen parity: build capnpc-fortran first" >&2
  exit 1
fi
BIN=$(cd "$(dirname "$BIN")" && pwd)/$(basename "$BIN")
MOD=$(cd "$MOD" && pwd)
LIB=$(cd "$(dirname "$LIB")" && pwd)/$(basename "$LIB")

FC=${FC:-gfortran}
CXX=${CXX:-g++}
SAN=()
if [[ "${SANITIZE:-0}" == 1 ]]; then
  SAN=(-fsanitize=address,undefined -fno-omit-frame-pointer)
fi
export LD_LIBRARY_PATH=/usr/local/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}

work() {
  local d
  d=$(mktemp -d /tmp/codegen-parity.XXXXXX)
  echo "$d"
}

fc() {
  "$FC" -std=f2018 "${SAN[@]}" "$@"
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
  capnp compile -o"$BIN" minmax.capnp
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
  fc minmax_capnp.f90 drv.f90 -I "$MOD" -J . "$LIB" -o drv
  capnp compile -oc++ minmax.capnp
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
  "$CXX" -std=c++17 cpp.cpp minmax.capnp.c++ $(pkg-config --cflags --libs capnp) -o cppdrv
  local f c
  f=$(./drv | tr 'A-F' 'a-f')
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
  capnp compile -o"$BIN" textesc.capnp
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
  fc textesc_capnp.f90 drv.f90 -I "$MOD" -J . "$LIB" -o drv
  cmp_out "$(./drv)" "ok"
  echo "g2 ok"
}

case "${1:-all}" in
  g1) g1 ;;
  g2) g2 ;;
  all) g1; g2 ;;
  *) echo "unknown gap: $1" >&2; exit 2 ;;
esac
