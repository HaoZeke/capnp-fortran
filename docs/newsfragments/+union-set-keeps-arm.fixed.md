A successful dynamic write or init of a union member now writes only the
discriminant and the new value, as Cap'n Proto C++ 1.4.0 does. The data and
pointer slots of the previous arm keep their bits. `capnp_dyn_select` still
zeroes the previous arm when it switches.
