`capnp_dyn_setp` copies a struct, text, data, or list that already
lives in the destination message, as Cap'n Proto C++ 1.4.0
`DynamicStruct::Builder::set` does. A later write through the source
leaves the stored value unchanged.
