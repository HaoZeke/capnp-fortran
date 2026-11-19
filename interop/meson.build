# Golden-master interop executable: links the Fortran runtime static lib, the
# c-capnproto reference static lib, and cmocka. Skipped (with a message) when
# either dependency is unavailable, so a bare `meson setup` still configures.

if have_ccapnp and cmocka_dep.found()
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
elif not have_ccapnp
  message('interop/golden_master: skipped (c-capnproto sources absent).')
else
  message('interop/golden_master: skipped (cmocka not found via pkg-config).')
endif

# --- RPC interop: Fortran vat against a capnp-C++ peer ---------------------

# auto (the default) skips when the peer cannot be built, so a Fortran
# main project still configures. enabled fails that configure. CI passes
# -Drpc_interop=enabled.
rpc_opt = get_option('rpc_interop')
rpc_required = rpc_opt.enabled()
if rpc_opt.disabled()
  message('interop/rpc: skipped (rpc_interop disabled).')
else
  cpp_ok = add_languages('cpp', required: rpc_required, native: false)
  capnp_rpc_dep = dependency('capnp-rpc', required: rpc_required)
  capnp_tool = find_program('capnp', required: rpc_required)
  capnpc_cpp = find_program('capnpc-c++', required: rpc_required)
  bash = find_program('bash', required: rpc_required)

  if cpp_ok and capnp_rpc_dep.found() and capnp_tool.found() and capnpc_cpp.found() and bash.found()
    adder_gen = custom_target(
      'adder-capnp-cpp',
      input: '../schema/adder.capnp',
      output: ['adder.capnp.c++', 'adder.capnp.h'],
      command: [
        capnp_tool, 'compile',
        '--src-prefix=' + meson.project_source_root() / 'schema',
        '-o' + capnpc_cpp.full_path() + ':@OUTDIR@',
        '@INPUT@',
      ],
    )

    rpc_peer_server = executable(
      'rpc_peer_server',
      ['rpc_peer_server.c++', adder_gen],
      dependencies: capnp_rpc_dep,
      override_options: ['cpp_std=c++17'],
    )

    # Fortran callers inline the layout of library types. Relinking the
    # client after a library edit is not enough: the source has to be
    # compiled again. Copying it when a library source changes does that.
    rpc_client_built = custom_target(
      'rpc-client-rebuild',
      input: 'rpc_client.f90',
      output: 'rpc_client_rebuild.f90',
      command: [
        find_program('python3'), '-c',
        'import shutil,sys; shutil.copyfile(sys.argv[1], sys.argv[2])',
        '@INPUT@', '@OUTPUT@',
      ],
      depend_files: capnp_fsrc,
    )

    rpc_client_f = executable(
      'rpc_client_f',
      rpc_client_built,
      link_with: capnp_flib,
    )

    test(
      'rpc_interop_cpp',
      bash,
      args: [
        meson.current_source_dir() / 'run_rpc_interop.sh',
        rpc_peer_server.full_path(),
        rpc_client_f.full_path(),
      ],
      depends: [rpc_peer_server, rpc_client_f],
      timeout: 60,
    )
  else
    message('interop/rpc: skipped (needs a C++ toolchain, capnp-rpc, and the capnp tools).')
  endif
endif
