!> Umbrella module: `use capnp` exposes the serialization API (wire
!> format, message I/O, packed codec, canonicalization). RPC lives in
!> `capnp_rpc`, `capnp_rpc_transport`, and `capnp_posix`; the C ABI shim
!> lives in `capnp_cabi`; none of these are re-exported here.
module capnp
   use capnp_kinds
   use capnp_endian
   use capnp_pointer
   use capnp_arena
   use capnp_message
   use capnp_serialize
   use capnp_packed
   use capnp_union
   use capnp_stream
   use capnp_canonical
   implicit none
   public
end module capnp
