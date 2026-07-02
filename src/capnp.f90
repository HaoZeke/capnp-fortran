!> Umbrella module: `use capnp` exposes the full public API.
module capnp
   use capnp_kinds
   use capnp_endian
   use capnp_pointer
   use capnp_arena
   use capnp_message
   use capnp_serialize
   use capnp_packed
   implicit none
   public
end module capnp
