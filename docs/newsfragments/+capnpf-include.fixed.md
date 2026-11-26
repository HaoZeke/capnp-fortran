`capnpf` quotes the standard include path, resolves `capnp` symlinks with `readlink` and `cd -P`, and falls back to that path when `CAPNP_PREFIX/include` is missing.
