program smoke
   use capnp
   use capnp_rpc, only: rpc_conn_t, rpc_server_t, rpc_conn_init, rpc_conn_alive, &
                        rpc_conn_close
   use capnp_posix, only: px_socketpair, px_close
   use, intrinsic :: iso_fortran_env, only: int64, int32, int8
   implicit none
   type(capnp_message_t), target :: msg
   type(capnp_ptr_t) :: root
   integer :: err
   integer(int8), allocatable :: bytes(:)
   type(rpc_conn_t), target :: conn
   class(rpc_server_t), pointer :: none => null()
   integer :: fa, fb

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

   ! The RPC layer links the C socket shim, so a consumer that only lists
   ! the Fortran sources fails here at link time.
   call px_socketpair(fa, fb, err)
   if (err /= CAPNP_OK) error stop 'socketpair'
   call rpc_conn_init(conn, fa, none)
   if (.not. rpc_conn_alive(conn)) error stop 'rpc_conn_init'
   call rpc_conn_close(conn)
   call px_close(fb)
   print '(a)', 'smoke rpc ok'
end program smoke
