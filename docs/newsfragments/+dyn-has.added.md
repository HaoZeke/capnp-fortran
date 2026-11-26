`capnp_dyn_has` reports field presence. The default mode is
`CAPNP_HAS_NON_NULL` and follows C++: void, bool, numeric, and enum fields
are present, and a selected group is present. `CAPNP_HAS_NON_DEFAULT`
reports a data field whose encoded bits are non-zero. Pointer fields use
the null word in both modes. An inactive union arm is absent.
