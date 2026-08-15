project(
  'capnp-fortran',
  ['fortran', 'c'],
  version: '0.2.2',
  license: 'MIT',
  default_options: ['warning_level=2', 'c_std=c11'],
)

# The Fortran runtime, compiled from the checked-in generated .f90 sources
# (the .fypp templates are pre-expanded by `pixi run gen`). Meson scans Fortran
# module dependencies, so source order here is irrelevant.
capnp_fsrc = files(
  'src/capnp_kinds.f90',
  'src/capnp_endian.f90',
  'src/capnp_pointer.f90',
  'src/capnp_arena.f90',
  'src/capnp_message.f90',
  'src/capnp_packed.f90',
  'src/capnp_union.f90',
  'src/capnp_serialize.f90',
  'src/capnp_stream.f90',
  'src/capnp_canonical.f90',
  'src/capnp.f90',
  'src/capnp_cabi.f90',
  'src/capnp_posix.f90',
  'src/capnp_rpc_transport.f90',
  'src/rpc_capnp.f90',
  'src/rpc_twoparty_capnp.f90',
  'src/rpc_threeparty_capnp.f90',
  'src/capnp_rpc.f90',
  'src/capnp_schema.f90',
  'src/capnp_dynamic.f90',
  'src/stream_capnp.f90',
)

capnp_flib = static_library(
  'capnp_fortran',
  capnp_fsrc,
  install: true,
)

# Consumers reach this through `dependency('capnp-fortran')`, which falls
# back to this subproject via a .wrap. Meson tracks the Fortran module
# directory of a linked target itself, so `use capnp` resolves without the
# consumer naming an include path.
capnp_fortran_dep = declare_dependency(link_with: capnp_flib)
meson.override_dependency('capnp-fortran', capnp_fortran_dep)

pkg = import('pkgconfig')
pkg.generate(
  capnp_flib,
  name: 'capnp-fortran',
  description: 'Native modern-Fortran Cap\'n Proto (wire, packed, canonical, RPC)',
  filebase: 'capnp-fortran',
  url: 'https://capnp-fortran.rgoswami.me',
)

# c-capnproto reference runtime. It ships autotools/cmake, but the runtime is
# just three .c files plus its headers, so we compile those directly from a
# vendored clone (see interop/README.md).
fs = import('fs')
# third_party/, not subprojects/: meson sandboxes subprojects/ against direct
# file references, and c-capnproto has no meson build of its own.
ccapnp_lib_dir = 'third_party/c-capnproto/lib'
have_ccapnp = fs.is_dir(ccapnp_lib_dir)

if have_ccapnp
  ccapnp_inc = include_directories(ccapnp_lib_dir)
  ccapnp_lib = static_library(
    'ccapnp',
    files(
      ccapnp_lib_dir / 'capn.c',
      ccapnp_lib_dir / 'capn-malloc.c',
      ccapnp_lib_dir / 'capn-stream.c',
    ),
    include_directories: ccapnp_inc,
    # The reference sources predate -Wall/-Wextra hygiene; do not fail on them.
    c_args: ['-w'],
  )
else
  ccapnp_inc = []
  ccapnp_lib = []
  message(
    'c-capnproto sources not found under ' + ccapnp_lib_dir + '; the golden '
    + 'master tests will be skipped. Fetch with `git clone '
    + 'https://github.com/opensourcerouting/c-capnproto '
    + 'third_party/c-capnproto`.',
  )
endif

# pkg-config only: an optional dependency that falls through to the CMake
# lookup warns about the toolchain state on hosts where CMake cannot
# probe the compilers, and a consumer building with --fatal-meson-warnings
# then fails on a dependency this project does not need.
cmocka_dep = dependency('cmocka', required: false, method: 'pkg-config')

subdir('interop')
