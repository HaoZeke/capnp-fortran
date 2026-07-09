!> Generic (parameterized) schemas: branded uses (Box(Text)) get a
!> brand-resolved instantiation with typed accessors, in direct,
!> list-element, list-binding, and nested-brand positions; unbound uses
!> keep the AnyPointer degradation, wire compatible either way.
module test_generic
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed, skip_test
   use capnp
   use box_capnp
   implicit none

   private
   public :: collect_generic

contains

   subroutine collect_generic(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [new_unittest("generic", run_generic)]
   end subroutine collect_generic

   subroutine check_(error, cond, name)
      type(error_type), allocatable, intent(inout) :: error
      logical, intent(in) :: cond
      character(len=*), intent(in) :: name
      if (allocated(error)) return
      call check(error, cond, name)
   end subroutine check_

   subroutine run_generic(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg, rmsg
      type(box_use_t) :: use_
      type(box_text_t) :: tb, eb
      type(box_list_text_t) :: lb
      type(nest_text_t) :: nx
      type(box_t) :: ab
      type(capnp_ptr_t) :: l
      integer(int8), allocatable :: bytes(:)
      character(len=:), allocatable :: s
      integer :: err

      call capnp_message_init_builder(msg, err)
      use_ = box_use_new_root(msg, err)
      call check_(error, err == CAPNP_OK, 'generic: root built')

      tb = box_use_text_box_init(use_, err)
      call check_(error, err == CAPNP_OK, 'generic: branded box init')
      call box_text_label_set(tb, 'greeting box', err)
      ! T = Text resolved by the brand: a typed accessor, no raw pointers.
      call box_text_value_set(tb, 'boxed hello', err)
      call check_(error, err == CAPNP_OK, 'generic: typed value set')

      ! List(Box(Text)): the branded element resolves, so elements come
      ! back as typed handles straight off the list.
      l = box_use_boxes_init(use_, 2_int64, err)
      call check_(error, err == CAPNP_OK, 'generic: branded element list init')
      eb = box_use_boxes_get_elem(use_, 0, err)
      call box_text_value_set(eb, 'one', err)
      eb = box_use_boxes_get_elem(use_, 1, err)
      call box_text_value_set(eb, 'two', err)
      call check_(error, err == CAPNP_OK, 'generic: branded element set')

      ! Box(List(Text)): a list binding substitutes wholesale, so value
      ! gets the full typed List(Text) surface including element helpers.
      lb = box_use_list_box_init(use_, err)
      call box_list_text_label_set(lb, 'list box', err)
      l = box_list_text_value_init(lb, 2_int64, err)
      call box_list_text_value_set_elem(lb, 0, 'alpha', err)
      call box_list_text_value_set_elem(lb, 1, 'beta', err)
      call check_(error, err == CAPNP_OK, 'generic: list-binding value set')

      ! Nest(Text): brands nested inside the generic (Box(T) fields)
      ! resolve through the instantiation's own bindings.
      nx = box_use_nest_init(use_, err)
      eb = nest_text_inner_init(nx, err)
      call box_text_value_set(eb, 'inner hello', err)
      l = nest_text_boxes_init(nx, 1_int64, err)
      eb = nest_text_boxes_get_elem(nx, 0, err)
      call box_text_value_set(eb, 'nested', err)
      call check_(error, err == CAPNP_OK, 'generic: nested brand set')

      call capnp_serialize_bytes(msg, bytes, err)
      call capnp_deserialize_bytes(bytes, rmsg, err)
      use_ = box_use_read_root(rmsg, err)
      tb = box_use_text_box_get(use_, err)
      call box_text_label_get(tb, s, err)
      call check_(error, s == 'greeting box', 'generic: label round trip')
      call box_text_value_get(tb, s, err)
      call check_(error, err == CAPNP_OK .and. s == 'boxed hello', &
                  'generic: typed value round trip')

      eb = box_use_boxes_get_elem(use_, 1, err)
      call box_text_value_get(eb, s, err)
      call check_(error, err == CAPNP_OK .and. s == 'two', &
                  'generic: branded element round trip')

      lb = box_use_list_box_get(use_, err)
      call box_list_text_value_get_elem(lb, 1, s, err)
      call check_(error, err == CAPNP_OK .and. s == 'beta', &
                  'generic: list-binding round trip')

      nx = box_use_nest_get(use_, err)
      eb = nest_text_inner_get(nx, err)
      call box_text_value_get(eb, s, err)
      call check_(error, err == CAPNP_OK .and. s == 'inner hello', &
                  'generic: nested brand inner round trip')
      eb = nest_text_boxes_get_elem(nx, 0, err)
      call box_text_value_get(eb, s, err)
      call check_(error, err == CAPNP_OK .and. s == 'nested', &
                  'generic: nested brand element round trip')

      ! Unbound Box keeps the generic handle and AnyPointer accessors.
      ab = box_use_any_box_get(use_, err)
      call check_(error, err == CAPNP_OK .and. capnp_ptr_is_null(ab%p), &
                  'generic: unbound box absent reads null')

      call capnp_message_free(msg)
      call capnp_message_free(rmsg)
   end subroutine run_generic

end module test_generic
