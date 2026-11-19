`capnpf decode`, `convert`, and `encode` exit 1 when stdout cannot be
written. The help and version options, and text eval, exit 0 in that
case. JSON, binary, and packed eval exit 1. A write interrupted by a
signal is retried.
