!> Generated names must be legal Fortran, including a generic struct
!> default and identifiers that would otherwise exceed 63 characters.
module test_idents
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use capnp
   use idents_capnp
   implicit none
   private
   public :: collect_idents

contains

   subroutine collect_idents(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [new_unittest("idents-generic-default", run_idents)]
   end subroutine collect_idents

   subroutine run_idents(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg
      type(hold_data_t) :: h
      type(seed_t) :: body
      character(len=:), allocatable :: s
      integer :: err

      call capnp_message_init_builder(msg, err)
      call check(error, err == CAPNP_OK, 'idents: builder')
      if (allocated(error)) return
      h = hold_data_new(msg, err)
      call check(error, err == CAPNP_OK, 'idents: hold(data)')
      if (allocated(error)) return
      body = hold_data_body_get(h, err)
      call check(error, err == CAPNP_OK, 'idents: default body')
      if (allocated(error)) return
      call seed_text_get(body, s, err)
      call check(error, err == CAPNP_OK .and. s == 'from-brand', &
                 'idents: generic default text')
      call capnp_message_free(msg)
   end subroutine run_idents

end module test_idents
