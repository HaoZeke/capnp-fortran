`capnp_dyn_load` frees the partly built schema when a load fails, so a
failed load leaves no segments or nodes behind.
