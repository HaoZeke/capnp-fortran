`capnp_dyn_set_capability` takes an optional interface id. A non-zero id
is stored when that interface extends the field, as Cap'n Proto C++ 1.4.0
`DynamicStruct::Builder::set` does. A mismatch returns `CAPNP_ERR_KIND`
and does not store the capability. An absent or zero id is stored with
no interface check.
