!> Generic (parameterized) schemas: type parameters bind to pointer
!> types, so generic fields are AnyPointer slots and the generated code
!> exposes them as pointer accessors (the capnp-c degradation, wire
!> compatible with branded C++ readers).
program test_generic
   use capnp
   use box_capnp
   implicit none

   integer :: nfail = 0
   type(capnp_message_t), target :: msg, rmsg
   type(box_use_t) :: use_
   type(box_t) :: tb
   type(capnp_ptr_t) :: q
   integer(int8), allocatable :: bytes(:)
   character(len=:), allocatable :: s
   integer :: err

   call capnp_message_init_builder(msg, err)
   use_ = box_use_new_root(msg, err)
   call check_(err == CAPNP_OK, 'generic: root built')

   tb = box_use_text_box_init(use_, err)
   call check_(err == CAPNP_OK, 'generic: branded box init')
   call box_label_set(tb, 'greeting box', err)
   ! T = Text: the generic slot is pointer 0; write a text object there.
   call capnp_set_text(tb%p, 0, 'boxed hello', err)
   call check_(err == CAPNP_OK, 'generic: value written through AnyPointer slot')

   call capnp_serialize_bytes(msg, bytes, err)
   call capnp_deserialize_bytes(bytes, rmsg, err)
   use_ = box_use_read_root(rmsg, err)
   tb = box_use_text_box_get(use_, err)
   call box_label_get(tb, s, err)
   call check_(s == 'greeting box', 'generic: label round trip')
   q = box_value_get(tb, err)
   call check_(err == CAPNP_OK .and. q%kind == CAPNP_PK_LIST, &
               'generic: AnyPointer accessor resolves')
   call capnp_get_text(tb%p, 0, s, err)
   call check_(s == 'boxed hello', 'generic: value round trip')

   ! Unbound Box: same accessors, value stays null until set.
   tb = box_use_any_box_get(use_, err)
   call check_(err == CAPNP_OK .and. capnp_ptr_is_null(tb%p), &
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
