`capnp_dyn_setp` accepts a group field, as Cap'n Proto C++ 1.4.0
`DynamicStruct::Builder::set` does. The group becomes the active arm and is
cleared, then the active union member of the value and each other member that
has a value are copied into it. A null active Text, Data, List or struct
member is written as its default, as C++ writes it. Pointers are deep copies,
also within one message. A value of another struct type clears the group and
returns `CAPNP_ERR_KIND`.
