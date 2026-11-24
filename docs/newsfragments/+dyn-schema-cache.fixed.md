Reader pointer lookups store one cached copy of each defaulted field on the schema.
`capnp_dyn_getp`, `capnp_dyn_get_any_struct`, and `capnp_dyn_get_any_list` take
the schema as inout because of that cache. A routine that only holds the schema
as in does not compile against them.
