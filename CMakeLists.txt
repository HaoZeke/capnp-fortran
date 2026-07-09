# capnp-fortran — CMake build for FetchContent / add_subdirectory consumers.
#
#   include(FetchContent)
#   FetchContent_Declare(
#     capnp
#     GIT_REPOSITORY https://github.com/HaoZeke/capnp-fortran.git
#     GIT_TAG        v0.1.0
#   )
#   FetchContent_MakeAvailable(capnp)
#   target_link_libraries(myapp PRIVATE capnp::capnp)
#
# Fortran modules are published on the target include path (BUILD_INTERFACE).
# Checked-in .f90 sources only (run `pixi run gen` before release if .fypp changes).

cmake_minimum_required(VERSION 3.16)

project(
  capnp
  VERSION 0.1.0
  DESCRIPTION "Native modern-Fortran Cap'n Proto (wire, packed, canonical, RPC, capnpc-fortran)"
  LANGUAGES Fortran
)

include(GNUInstallDirs)
include(CMakePackageConfigHelpers)

# When pulled via FetchContent / add_subdirectory, default to library only.
if(CMAKE_SOURCE_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR)
  set(_capnp_is_toplevel ON)
else()
  set(_capnp_is_toplevel OFF)
endif()

option(CAPNP_BUILD_PLUGIN "Build the capnpc-fortran schema plugin" ${_capnp_is_toplevel})
option(CAPNP_BUILD_SHARED "Build shared library instead of static" OFF)
option(CAPNP_INSTALL "Install targets and package config" ${_capnp_is_toplevel})

# Same source set as meson.build (interop tier); order is irrelevant for modern CMake+gfortran.
set(CAPNP_SOURCES
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

if(CAPNP_BUILD_SHARED)
  add_library(capnp SHARED ${CAPNP_SOURCES})
else()
  add_library(capnp STATIC ${CAPNP_SOURCES})
endif()
add_library(capnp::capnp ALIAS capnp)

set(_capnp_mod_dir "${CMAKE_CURRENT_BINARY_DIR}/fortran_mods")
set_target_properties(capnp PROPERTIES
  OUTPUT_NAME capnp
  VERSION ${PROJECT_VERSION}
  SOVERSION ${PROJECT_VERSION_MAJOR}
  Fortran_MODULE_DIRECTORY "${_capnp_mod_dir}"
  POSITION_INDEPENDENT_CODE ON
)

target_include_directories(capnp
  PUBLIC
    $<BUILD_INTERFACE:${_capnp_mod_dir}>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/capnp>
)

# Free-form F2018; gfortran and typical flang accept these.
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU|LLVMFlang|Flang")
  target_compile_options(capnp PRIVATE -std=f2018 -ffree-line-length-none)
endif()

if(CAPNP_BUILD_PLUGIN)
  add_executable(capnpc-fortran
    app/capnpc_emit.f90
    app/main.f90
  )
  target_link_libraries(capnpc-fortran PRIVATE capnp::capnp)
  set_target_properties(capnpc-fortran PROPERTIES
    Fortran_MODULE_DIRECTORY "${_capnp_mod_dir}"
  )
  if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU|LLVMFlang|Flang")
    target_compile_options(capnpc-fortran PRIVATE -std=f2018 -ffree-line-length-none)
  endif()
endif()

if(CAPNP_INSTALL)
  install(TARGETS capnp
    EXPORT capnpTargets
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  )
  if(CAPNP_BUILD_PLUGIN)
    install(TARGETS capnpc-fortran
      EXPORT capnpTargets
      RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    )
  endif()
  # Install .mod files for consumers of the installed package.
  install(DIRECTORY "${_capnp_mod_dir}/"
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/capnp
    FILES_MATCHING PATTERN "*.mod"
  )

  install(EXPORT capnpTargets
    FILE capnpTargets.cmake
    NAMESPACE capnp::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/capnp
  )

  configure_package_config_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/cmake/capnpConfig.cmake.in"
    "${CMAKE_CURRENT_BINARY_DIR}/capnpConfig.cmake"
    INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/capnp
  )
  write_basic_package_version_file(
    "${CMAKE_CURRENT_BINARY_DIR}/capnpConfigVersion.cmake"
    VERSION ${PROJECT_VERSION}
    COMPATIBILITY SameMajorVersion
  )
  install(FILES
    "${CMAKE_CURRENT_BINARY_DIR}/capnpConfig.cmake"
    "${CMAKE_CURRENT_BINARY_DIR}/capnpConfigVersion.cmake"
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/capnp
  )
endif()
