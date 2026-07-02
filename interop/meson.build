# Golden-master interop executable: links the Fortran runtime static lib, the
# c-capnproto reference static lib, and cmocka. Skipped (with a message) when
# either dependency is unavailable, so a bare `meson setup` still configures.

if not have_ccapnp
  message('interop/golden_master: skipped (c-capnproto sources absent).')
  subdir_done()
endif

if not cmocka_dep.found()
  message('interop/golden_master: skipped (cmocka not found via pkg-config).')
  subdir_done()
endif

golden_master = executable(
  'golden_master',
  'golden_master.c',
  include_directories: ccapnp_inc,
  link_with: [capnp_flib, ccapnp_lib],
  dependencies: cmocka_dep,
  # The main() is C but the objects pull in the Fortran runtime; link with the
  # Fortran driver so libgfortran and friends are resolved automatically.
  link_language: 'fortran',
)

test('golden_master', golden_master)
