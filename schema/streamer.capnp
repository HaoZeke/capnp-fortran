@0xc1a7b2d3e4f50617;
# Streaming coverage: `-> stream` methods return stream.capnp's
# StreamResult; flow control is client-side policy (rpc_stream_t).

interface Writer @0xc1a7b2d3e4f50618 {
  write @0 (chunk :Data) -> stream;
  done @1 () -> (total :UInt64);
}
