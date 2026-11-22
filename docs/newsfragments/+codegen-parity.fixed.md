`capnpc-fortran` output now matches Cap'n Proto C++ 1.4.0 for the schema
features it used to mishandle. Minimum signed integers and text constants with
control characters keep their values, and list defaults are copied into the
builder and read through the field getter. Generic instantiations keep every
binding, schemas with the same basename in different directories get separate
modules, and servers receive the call context and dispatch inherited interface
ids. Annotations, `Data` annotations and typed `AnyPointer` fields are emitted. Schemas that
import each other compile when both declare interfaces or `AnyPointer` fields,
because `any_struct_t`, `any_list_t` and `capability_client_t` now come from
the `capnp_any` module under private aliases. Same-named annotations from different files get distinct
parameters, long text literals wrap, and a generic interface may extend a plain
one.
Imported names that the importing module declares itself are renamed on use,
annotation constants never repeat another name of the module, overridden
methods get distinct bindings, generated lines wrap at 132 columns, and
stream procedure names fit 63 characters.
Renamed imports are renamed at every use, helper procedure names stay clear of
annotation constants, diamond import graphs generate in linear time, and generic
arguments that are themselves instantiations stay distinct, including two
imported types of the same name. The `rpc_capnp` symbols, names an importer
re-exports and private helpers no longer meet the names of imported schemas. A
schema in a directory gets a module named from its path, so a run on one file
and a run on all of them agree, and a module of the plain basename re-exports
it while that name is free.
Schemas that compile one file at a time still do. Names written for generic
interface instances and for annotation constants carry the module name, and a
module imported only for them is read with `use ..., only:`, so two files never
declare or import one name twice.
Annotation value constants carry the module tag, the `rpc_capnp` symbols are
private aliases, and a generated procedure that shares a name with a constant
of the module is renamed, so none of them meets an imported name.
Generic instantiations keep the names they had, such as `box_box_t`, and only
an instantiation that would repeat a taken name is spelled out in full.
