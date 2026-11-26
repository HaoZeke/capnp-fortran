A capability whose interface extends a field's interface can be stored in that
field, as in Cap'n Proto C++ 1.4.0. The generated client type of an interface
with superclasses extends the client type of its first superclass, and
interface field setters take `class(<iface>_client_t)`. `capnp_dyn_setp`
checks a capability value that carries an interface id in `schema_id`: an
interface that extends the field's is stored, and an unrelated interface is
rejected with `CAPNP_ERR_KIND`, as C++ rejects it. A value without an
interface id is stored as before.
