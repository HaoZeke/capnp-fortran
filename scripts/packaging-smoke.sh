#!/usr/bin/env bash
# Consume capnp-fortran the four ways a downstream project can, and fail if
# any of them stops working. Shipping install rules that nothing exercises
# is how a broken Config.cmake or a stale .pc reaches users.
#
#   1. CMake add_subdirectory  (the FetchContent_MakeAvailable path)
#   2. CMake find_package      (against a real staged install)
#   3. pkg-config              (for build systems that do not speak CMake)
#   4. meson subproject        (the .wrap path)
#
# The fpm git-tag route is covered separately; it needs network access.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"

WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
PREFIX="$WORK/prefix"

SMOKE="$ROOT/cmake/fetchcontent_smoke/smoke.f90"

# A .mod file is only readable by the gfortran that wrote it, so every path
# below has to use one compiler. Mixing them fails with "created by a
# different version of GNU Fortran" rather than anything about packaging.
FC="${FC:-$(command -v gfortran)}"
export FC
echo "using FC=$FC ($("$FC" -dumpversion))"

echo "== 1. CMake add_subdirectory (FetchContent path)"
cmake -S "$ROOT/cmake/fetchcontent_smoke" -B "$WORK/sub" \
  -DCMAKE_Fortran_COMPILER="$FC" \
  -DCAPNP_FORTRAN_SOURCE_DIR="$ROOT" -DCMAKE_BUILD_TYPE=Release >"$WORK/sub.log" 2>&1
cmake --build "$WORK/sub" >>"$WORK/sub.log" 2>&1
"$WORK/sub/smoke"

echo "== 2. CMake find_package against a staged install"
cmake -S "$ROOT" -B "$WORK/build" -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_Fortran_COMPILER="$FC" \
  -DCMAKE_INSTALL_PREFIX="$PREFIX" >"$WORK/build.log" 2>&1
cmake --build "$WORK/build" >>"$WORK/build.log" 2>&1
cmake --install "$WORK/build" >>"$WORK/build.log" 2>&1

mkdir -p "$WORK/fp"
cp "$SMOKE" "$WORK/fp/smoke.f90"
cat >"$WORK/fp/CMakeLists.txt" <<'EOF'
cmake_minimum_required(VERSION 3.16)
project(capnp_fortran_find_package_smoke LANGUAGES Fortran)
find_package(capnp_fortran REQUIRED)
add_executable(smoke smoke.f90)
target_link_libraries(smoke PRIVATE capnp_fortran::capnp_fortran)
EOF
cmake -S "$WORK/fp" -B "$WORK/fpb" -DCMAKE_PREFIX_PATH="$PREFIX" \
  -DCMAKE_Fortran_COMPILER="$FC" \
  -DCMAKE_BUILD_TYPE=Release >"$WORK/fp.log" 2>&1
cmake --build "$WORK/fpb" >>"$WORK/fp.log" 2>&1
"$WORK/fpb/smoke"
test -x "$PREFIX/bin/capnpc-fortran"

echo "== 3. pkg-config"
export PKG_CONFIG_PATH="$PREFIX/lib/pkgconfig:$PREFIX/lib64/pkgconfig"
pkg-config --exists capnp-fortran
echo "   version $(pkg-config --modversion capnp-fortran)"
# Cflags must carry the .mod directory or `use capnp` cannot resolve.
# shellcheck disable=SC2046
"$FC" -o "$WORK/pc-smoke" "$SMOKE" $(pkg-config --cflags --libs capnp-fortran)
"$WORK/pc-smoke"

echo "== 4. meson subproject (.wrap path)"
mkdir -p "$WORK/mw/subprojects"
# A local wrap; the published one points at the git tag instead.
ln -s "$ROOT" "$WORK/mw/subprojects/capnp-fortran"
cp "$SMOKE" "$WORK/mw/smoke.f90"
cat >"$WORK/mw/meson.build" <<'EOF'
project('capnp-fortran-wrap-smoke', 'fortran')
dep = dependency('capnp-fortran', fallback: ['capnp-fortran', 'capnp_fortran_dep'])
executable('smoke', 'smoke.f90', dependencies: dep)
EOF
meson setup "$WORK/mwb" "$WORK/mw" >"$WORK/mw.log" 2>&1 || { tail -25 "$WORK/mw.log"; exit 1; }
meson compile -C "$WORK/mwb" >>"$WORK/mw.log" 2>&1 || { tail -25 "$WORK/mw.log"; exit 1; }
"$WORK/mwb/smoke"

echo "ok packaging-smoke"
