`capnpc-fortran` output now matches Cap'n Proto C++ 1.4.0 for the schema
features it used to mishandle. Minimum signed integers and text constants with
control characters keep their values, and list defaults are copied into the
builder and read through the field getter. Generic instantiations keep every
binding, schemas with the same basename in different directories get separate
modules, and servers receive the call context and dispatch inherited interface
ids. Annotations, `Data` annotations and typed `AnyPointer` fields are emitted.
