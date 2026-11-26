`capnp_dyn_setp` rejects a capability whose interface walk passes 64
visits, as Cap'n Proto C++ 1.4.0 `InterfaceSchema::extends` does. The
counter increments before the identity check.
