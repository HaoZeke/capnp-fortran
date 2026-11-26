`capnp_dyn_load` returns `CAPNP_ERR_ARG` when a struct field offset or union
discriminant falls outside the data or pointer section, as Cap'n Proto C++
1.4.0 `SchemaLoader` does.
