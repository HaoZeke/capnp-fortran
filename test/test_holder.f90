!> Interface-typed struct field: Holder.svc is Echo_client on the wire.
module test_holder
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed, skip_test
   use capnp
   use holder_capnp
   implicit none
   private
   public :: collect_holder

contains

   subroutine collect_holder(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [new_unittest("holder", run_holder)]
   end subroutine collect_holder

   subroutine check_(error, cond, name)
      type(error_type), allocatable, intent(inout) :: error
      logical, intent(in) :: cond
      character(len=*), intent(in) :: name
      if (allocated(error)) return
      call check(error, cond, name)
   end subroutine check_

   subroutine run_holder(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg, rmsg
      type(holder_t) :: h, rh
      type(echo_client_t) :: c, rc
      integer(int8), allocatable :: bytes(:)
      character(len=:), allocatable :: note
      integer :: err

      call capnp_message_init_builder(msg, err)
      h = holder_new_root(msg, err)
      call check_(error, err == CAPNP_OK, 'holder: new root')
      c%cap%kind = RPC_CAP_IMPORT
      c%cap%id = 11_int64
      call holder_svc_set(h, c, err)
      call check_(error, err == CAPNP_OK, 'holder: svc set')
      call holder_note_set(h, 'hello-cap', err)
      call check_(error, err == CAPNP_OK, 'holder: note set')
      call capnp_serialize_bytes(msg, bytes, err)
      call check_(error, err == CAPNP_OK, 'holder: serialize')

      call capnp_deserialize_bytes(bytes, rmsg, err)
      rh = holder_read_root(rmsg, err)
      rc = holder_svc_get(rh, err)
      call check_(error, err == CAPNP_OK, 'holder: svc get')
      call check_(error, rc%cap%kind == RPC_CAP_IMPORT .and. rc%cap%id == 11_int64, &
                  'holder: cap index 11 round-trips via typed accessors')
      call holder_note_get(rh, note, err)
      call check_(error, err == CAPNP_OK .and. note == 'hello-cap', 'holder: note round-trip')

      call capnp_message_free(msg)
      call capnp_message_free(rmsg)
   end subroutine run_holder

end module test_holder
