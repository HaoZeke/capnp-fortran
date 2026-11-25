`capnp_dyn_init_list` on a Text or Data field allocates a zeroed blob of `n`
bytes and returns it as a byte list, as Cap'n Proto C++ 1.4.0 `init(field,
size)` does. A Text blob has one more byte for its trailing NUL.
