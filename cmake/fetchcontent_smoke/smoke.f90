program smoke
   use capnp
   use, intrinsic :: iso_fortran_env, only: int64, int32, int8
   implicit none
   type(capnp_message_t), target :: msg
   type(capnp_ptr_t) :: root
   integer :: err
   integer(int8), allocatable :: bytes(:)

   call capnp_message_init_builder(msg, err)
   if (err /= CAPNP_OK) error stop 'init_builder'
   root = capnp_new_struct(msg, 1, 0, err)
   if (err /= CAPNP_OK) error stop 'new_struct'
   call capnp_set_root(msg, root, err)
   if (err /= CAPNP_OK) error stop 'set_root'
   call capnp_set_i32(root, 0_int64, 42_int32, err)
   if (err /= CAPNP_OK) error stop 'set_i32'
   call capnp_serialize_bytes(msg, bytes, err)
   if (err /= CAPNP_OK) error stop 'serialize'
   if (.not. allocated(bytes)) error stop 'no bytes'
   if (size(bytes) < 8) error stop 'too short'
   print '(a,i0)', 'smoke ok bytes=', size(bytes)
   call capnp_message_free(msg)
end program smoke
