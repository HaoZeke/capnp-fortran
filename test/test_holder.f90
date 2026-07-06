!> Interface-typed struct field: Holder.svc is Echo_client on the wire.
program test_holder
   use capnp
   use holder_capnp
   implicit none
   integer :: nfail = 0
   type(capnp_message_t), target :: msg, rmsg
   type(holder_t) :: h, rh
   type(echo_client_t) :: c, rc
   integer(int8), allocatable :: bytes(:)
   character(len=:), allocatable :: note
   integer :: err

   call capnp_message_init_builder(msg, err)
   h = holder_new_root(msg, err)
   call check_(err == CAPNP_OK, 'holder: new root')
   c%cap%kind = RPC_CAP_IMPORT
   c%cap%id = 11_int64
   call holder_svc_set(h, c, err)
   call check_(err == CAPNP_OK, 'holder: svc set')
   call holder_note_set(h, 'hello-cap', err)
   call check_(err == CAPNP_OK, 'holder: note set')
   call capnp_serialize_bytes(msg, bytes, err)
   call check_(err == CAPNP_OK, 'holder: serialize')

   call capnp_deserialize_bytes(bytes, rmsg, err)
   rh = holder_read_root(rmsg, err)
   rc = holder_svc_get(rh, err)
   call check_(err == CAPNP_OK, 'holder: svc get')
   call check_(rc%cap%kind == RPC_CAP_IMPORT .and. rc%cap%id == 11_int64, &
               'holder: cap index 11 round-trips via typed accessors')
   call holder_note_get(rh, note, err)
   call check_(err == CAPNP_OK .and. note == 'hello-cap', 'holder: note round-trip')

   call capnp_message_free(msg)
   call capnp_message_free(rmsg)
   if (nfail > 0) then
      print '(a,i0,a)', 'FAILED: ', nfail, ' assertion(s)'
      error stop 1
   end if
   print '(a)', 'All holder interface-field tests passed.'
contains
   subroutine check_(cond, name)
      logical, intent(in) :: cond
      character(len=*), intent(in) :: name
      if (.not. cond) then
         nfail = nfail + 1
         print '(a,a)', 'FAIL: ', name
      end if
   end subroutine check_
end program test_holder
