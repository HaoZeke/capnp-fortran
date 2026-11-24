`capnpc-fortran` frees the command argument, request buffer, file
name, and message on success and on every error exit, so a leak
checker sees a clean run.
