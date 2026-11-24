`capnpc-fortran` maps a schema under a `capnp/` directory to a library
module only when its file id matches a schema bundled with Cap'n Proto.
A user schema at such a path gets its own module, and no generated shim
takes the name of a module the library provides.
