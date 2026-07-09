# Smoke: build a tiny program against capnp via add_subdirectory (same as FetchContent).
cmake_minimum_required(VERSION 3.16)
project(capnp_fetchcontent_smoke LANGUAGES Fortran)

set(CAPNP_BUILD_PLUGIN OFF CACHE BOOL "" FORCE)
set(CAPNP_INSTALL OFF CACHE BOOL "" FORCE)
add_subdirectory("${CAPNP_SOURCE_DIR}" "${CMAKE_BINARY_DIR}/capnp-build")

add_executable(smoke smoke.f90)
target_link_libraries(smoke PRIVATE capnp::capnp)
