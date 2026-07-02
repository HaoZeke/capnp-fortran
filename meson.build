project(
  'capnp-fortran-interop',
  ['fortran', 'c'],
  version: '0.1.0',
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
)

capnp_flib = static_library('capnp_f', capnp_fsrc)

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

cmocka_dep = dependency('cmocka', required: false)

subdir('interop')
