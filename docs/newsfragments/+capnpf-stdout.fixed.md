`capnpf decode`, `convert`, and `encode` exit 1 when stdout cannot be
written. `--help`, `--version`, and `eval` exit 0 in that case, and a
write interrupted by a signal is retried.
