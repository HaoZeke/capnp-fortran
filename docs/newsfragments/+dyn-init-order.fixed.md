`capnp_dyn_init_struct` and `capnp_dyn_init_list` select a union member
before checking the field type, the size, the element schema or the
allocation, as Cap'n Proto C++ 1.4.0 `init` does. A failed init therefore
makes that member the active arm while storing nothing.
