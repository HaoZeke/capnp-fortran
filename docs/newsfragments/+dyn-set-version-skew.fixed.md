`capnp_dyn_setp` stores a struct, or a list of structs, written by an older
or newer version of the schema, as Cap'n Proto C++ 1.4.0 does. It checks the
type by id: `capnp_dyn_getp` stamps a struct with its id and a struct list
with its element id, so a value of another struct type with the same layout
is rejected with `CAPNP_ERR_KIND`, as C++ rejects it. A handle without an id
is checked by layout as before.
