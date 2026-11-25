`capnp_dyn_init_struct` initializes group and AnyPointer fields as Cap'n Proto
C++ 1.4.0 `init` does. A group becomes the active arm, its discriminant-0 arm
and non-union members are zeroed, and the struct handle is returned. An
AnyPointer field is cleared and the returned handle is null. On a reader, a
frozen view or a list handle, `capnp_dyn_init_struct` and `capnp_dyn_init_list`
write nothing and return an error. A failed `capnp_dyn_init_struct` returns a
null handle.
