# Smoke: build a tiny program against capnp_fortran via add_subdirectory
# (same path as FetchContent_MakeAvailable).
cmake_minimum_required(VERSION 3.16)
project(capnp_fortran_fetchcontent_smoke LANGUAGES Fortran)

set(CAPNP_FORTRAN_BUILD_PLUGIN OFF CACHE BOOL "" FORCE)
set(CAPNP_FORTRAN_INSTALL OFF CACHE BOOL "" FORCE)
add_subdirectory("${CAPNP_FORTRAN_SOURCE_DIR}" "${CMAKE_BINARY_DIR}/capnp_fortran-build")

add_executable(smoke smoke.f90)
target_link_libraries(smoke PRIVATE capnp_fortran::capnp_fortran)
