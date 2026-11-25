`capnp_dyn_set_text` accepts the same fields as Cap'n Proto C++ 1.4.0: an Enum
field takes the enumerant with that exact name and returns `CAPNP_ERR_ARG` for
an unknown one, a Data field stores the text bytes without the trailing NUL,
and an AnyPointer field stores the text. `capnp_dyn_setp` stores a struct,
list or capability in any AnyPointer field, whatever its declared constraint.
