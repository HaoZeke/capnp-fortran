Float32 text matches capnp 1.4.0 on glibc: subnormals are printed with
eight digits, an integer token `-0` decodes as +0, and a decimal
literal is parsed as a double and then cast to float32. An integer
token is converted to float32 in one rounding.
