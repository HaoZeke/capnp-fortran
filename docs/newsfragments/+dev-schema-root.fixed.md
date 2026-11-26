Pixi sets `CAPNP_FORTRAN_ROOT` to the repo root for the test task. The dev-schema probe reads that variable and, with it unset, uses `test/dev_schemas.sh` from the working directory.
