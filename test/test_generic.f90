!> Generic (parameterized) schemas: branded uses (Box(Text)) get a
!> brand-resolved instantiation with typed accessors; unbound uses keep
!> the AnyPointer degradation, wire compatible either way.
program test_generic
   use capnp
   use box_capnp
   implicit none

   integer :: nfail = 0
   type(capnp_message_t), target :: msg, rmsg
   type(box_use_t) :: use_
   type(box_text_t) :: tb
   type(box_t) :: ab
   integer(int8), allocatable :: bytes(:)
   character(len=:), allocatable :: s
   integer :: err

   call capnp_message_init_builder(msg, err)
   use_ = box_use_new_root(msg, err)
   call check_(err == CAPNP_OK, 'generic: root built')

   tb = box_use_text_box_init(use_, err)
   call check_(err == CAPNP_OK, 'generic: branded box init')
   call box_text_label_set(tb, 'greeting box', err)
   ! T = Text resolved by the brand: a typed accessor, no raw pointers.
   call box_text_value_set(tb, 'boxed hello', err)
   call check_(err == CAPNP_OK, 'generic: typed value set')

   call capnp_serialize_bytes(msg, bytes, err)
   call capnp_deserialize_bytes(bytes, rmsg, err)
   use_ = box_use_read_root(rmsg, err)
   tb = box_use_text_box_get(use_, err)
   call box_text_label_get(tb, s, err)
   call check_(s == 'greeting box', 'generic: label round trip')
   call box_text_value_get(tb, s, err)
   call check_(err == CAPNP_OK .and. s == 'boxed hello', &
               'generic: typed value round trip')

   ! Unbound Box keeps the generic handle and AnyPointer accessors.
   ab = box_use_any_box_get(use_, err)
   call check_(err == CAPNP_OK .and. capnp_ptr_is_null(ab%p), &
               'generic: unbound box absent reads null')

   call capnp_message_free(msg)
   call capnp_message_free(rmsg)

   if (nfail > 0) then
      print '(a,i0,a)', 'FAILED: ', nfail, ' assertion(s)'
      error stop 1
   end if
   print '(a)', 'All generic-schema tests passed.'

contains

   subroutine check_(cond, name)
      logical, intent(in) :: cond
      character(len=*), intent(in) :: name
      if (.not. cond) then
         nfail = nfail + 1
         print '(a,a)', 'FAIL: ', name
      end if
   end subroutine check_

end program test_generic
