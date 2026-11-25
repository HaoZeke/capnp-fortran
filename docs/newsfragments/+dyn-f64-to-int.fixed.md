`capnp_dyn_set_f64` now writes integer fields, converting the way Cap'n Proto
C++ 1.4.0 does: the value must be a whole number within the field's range, so
2.0 stores 2 while 1.5, NaN and infinities return `CAPNP_ERR_ARG`. Enum and
Bool fields still return `CAPNP_ERR_KIND`, as in C++.
