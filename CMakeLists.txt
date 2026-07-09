# capnp-fortran — CMake build for FetchContent / add_subdirectory consumers.
#
# Official Cap'n Proto C++ owns find_package(CapnProto) and CapnProto::capnp.
# This Fortran port deliberately uses a different project / namespace:
#
#   include(FetchContent)
#   FetchContent_Declare(
#     capnp_fortran
#     GIT_REPOSITORY https://github.com/HaoZeke/capnp-fortran.git
#     GIT_TAG        v0.1.0
#   )
#   FetchContent_MakeAvailable(capnp_fortran)
#   target_link_libraries(myapp PRIVATE capnp_fortran::capnp_fortran)
#
# Fortran source modules remain `use capnp` (language API). CMake package name
# is capnp_fortran so it never shadows CapnProtoConfig.cmake.
# Checked-in .f90 sources only (run `pixi run gen` before release if .fypp changes).

cmake_minimum_required(VERSION 3.16)

project(
  capnp_fortran
  VERSION 0.1.0
  DESCRIPTION "Native modern-Fortran Cap'n Proto (wire, packed, canonical, RPC, capnpc-fortran)"
  LANGUAGES Fortran
)

include(GNUInstallDirs)
include(CMakePackageConfigHelpers)

# When pulled via FetchContent / add_subdirectory, default to library only.
if(CMAKE_SOURCE_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR)
  set(_capnp_fortran_is_toplevel ON)
else()
  set(_capnp_fortran_is_toplevel OFF)
endif()

# Prefix CAPNP_FORTRAN_* — Cap'n Proto C++ already uses CAPNP_* (CAPNP_EXECUTABLE, …).
option(CAPNP_FORTRAN_BUILD_PLUGIN "Build the capnpc-fortran schema plugin" ${_capnp_fortran_is_toplevel})
option(CAPNP_FORTRAN_BUILD_SHARED "Build shared library instead of static" OFF)
option(CAPNP_FORTRAN_INSTALL "Install targets and package config" ${_capnp_fortran_is_toplevel})

# Same source set as meson.build (interop tier); order is irrelevant for modern CMake+gfortran.
set(CAPNP_FORTRAN_SOURCES
  src/capnp_kinds.f90
  src/capnp_endian.f90
  src/capnp_pointer.f90
  src/capnp_arena.f90
  src/capnp_message.f90
  src/capnp_packed.f90
  src/capnp_union.f90
  src/capnp_serialize.f90
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
  src/stream_capnp.f90
)

if(CAPNP_FORTRAN_BUILD_SHARED)
  add_library(capnp_fortran SHARED ${CAPNP_FORTRAN_SOURCES})
else()
  add_library(capnp_fortran STATIC ${CAPNP_FORTRAN_SOURCES})
endif()
add_library(capnp_fortran::capnp_fortran ALIAS capnp_fortran)

set(_capnp_fortran_mod_dir "${CMAKE_CURRENT_BINARY_DIR}/fortran_mods")
set_target_properties(capnp_fortran PROPERTIES
  OUTPUT_NAME capnp_fortran
  VERSION ${PROJECT_VERSION}
  SOVERSION ${PROJECT_VERSION_MAJOR}
  Fortran_MODULE_DIRECTORY "${_capnp_fortran_mod_dir}"
  POSITION_INDEPENDENT_CODE ON
  EXPORT_NAME capnp_fortran
)

target_include_directories(capnp_fortran
  PUBLIC
    $<BUILD_INTERFACE:${_capnp_fortran_mod_dir}>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/capnp_fortran>
)

# Free-form F2018; gfortran and typical flang accept these.
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU|LLVMFlang|Flang")
  target_compile_options(capnp_fortran PRIVATE -std=f2018 -ffree-line-length-none)
endif()

if(CAPNP_FORTRAN_BUILD_PLUGIN)
  add_executable(capnpc-fortran
    app/capnpc_emit.f90
    app/main.f90
  )
  target_link_libraries(capnpc-fortran PRIVATE capnp_fortran::capnp_fortran)
  set_target_properties(capnpc-fortran PROPERTIES
    Fortran_MODULE_DIRECTORY "${_capnp_fortran_mod_dir}"
  )
  if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU|LLVMFlang|Flang")
    target_compile_options(capnpc-fortran PRIVATE -std=f2018 -ffree-line-length-none)
  endif()
endif()

if(CAPNP_FORTRAN_INSTALL)
  install(TARGETS capnp_fortran
    EXPORT capnp_fortranTargets
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  )
  if(CAPNP_FORTRAN_BUILD_PLUGIN)
    install(TARGETS capnpc-fortran
      EXPORT capnp_fortranTargets
      RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    )
  endif()
  install(DIRECTORY "${_capnp_fortran_mod_dir}/"
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/capnp_fortran
    FILES_MATCHING PATTERN "*.mod"
  )

  install(EXPORT capnp_fortranTargets
    FILE capnp_fortranTargets.cmake
    NAMESPACE capnp_fortran::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/capnp_fortran
  )

  configure_package_config_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/cmake/capnp_fortranConfig.cmake.in"
    "${CMAKE_CURRENT_BINARY_DIR}/capnp_fortranConfig.cmake"
    INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/capnp_fortran
  )
  write_basic_package_version_file(
    "${CMAKE_CURRENT_BINARY_DIR}/capnp_fortranConfigVersion.cmake"
    VERSION ${PROJECT_VERSION}
    COMPATIBILITY SameMajorVersion
  )
  install(FILES
    "${CMAKE_CURRENT_BINARY_DIR}/capnp_fortranConfig.cmake"
    "${CMAKE_CURRENT_BINARY_DIR}/capnp_fortranConfigVersion.cmake"
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/capnp_fortran
  )
endif()
