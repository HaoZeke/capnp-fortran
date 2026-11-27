`capnp_dyn_get_text` and `capnp_dyn_get_data` on a builder copy a non-empty
schema default into a null Text or Data field, as Cap'n Proto C++ 1.4.0
does, while a reader returns that default without writing it.
