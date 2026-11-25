`capnpf` keeps flat input sizes and offsets in 64-bit integers, so input
above 2 GiB no longer overflows a default integer.
