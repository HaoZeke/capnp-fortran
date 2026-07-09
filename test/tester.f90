!> test-drive entry point for capnp-fortran unit suites.
program tester
   use, intrinsic :: iso_fortran_env, only: error_unit
   use testdrive, only: run_testsuite, new_testsuite, testsuite_type, &
        select_suite, run_selected, get_argument
   use test_wire, only: collect_wire
   use test_addressbook, only: collect_addressbook
   use test_canonical, only: collect_canonical
   use test_dynamic, only: collect_dynamic
   use test_generated, only: collect_generated
   use test_generic, only: collect_generic
   use test_holder, only: collect_holder
   use test_interop, only: collect_interop
   use test_kitchen, only: collect_kitchen
   use test_parity, only: collect_parity
   use test_rpc, only: collect_rpc
   use test_rpc_typed, only: collect_rpc_typed
   use test_stream, only: collect_stream
   implicit none
   integer :: stat, is
   character(len=:), allocatable :: suite_name, test_name
   type(testsuite_type), allocatable :: testsuites(:)
   character(len=*), parameter :: fmt = '("#", *(1x, a))'

   stat = 0
   testsuites = [ &
        new_testsuite("wire", collect_wire), &
        new_testsuite("addressbook", collect_addressbook), &
        new_testsuite("canonical", collect_canonical), &
        new_testsuite("dynamic", collect_dynamic), &
        new_testsuite("generated", collect_generated), &
        new_testsuite("generic", collect_generic), &
        new_testsuite("holder", collect_holder), &
        new_testsuite("interop", collect_interop), &
        new_testsuite("kitchen", collect_kitchen), &
        new_testsuite("parity", collect_parity), &
        new_testsuite("rpc", collect_rpc), &
        new_testsuite("rpc_typed", collect_rpc_typed), &
        new_testsuite("stream", collect_stream) &
        ]

   call get_argument(1, suite_name)
   call get_argument(2, test_name)

   if (allocated(suite_name)) then
      is = select_suite(testsuites, suite_name)
      if (is > 0 .and. is <= size(testsuites)) then
         if (allocated(test_name)) then
            write (error_unit, fmt) "Suite:", testsuites(is)%name
            call run_selected(testsuites(is)%collect, test_name, error_unit, stat)
            if (stat < 0) error stop 1
         else
            write (error_unit, fmt) "Testing:", testsuites(is)%name
            call run_testsuite(testsuites(is)%collect, error_unit, stat)
         end if
      else
         write (error_unit, fmt) "Available testsuites"
         do is = 1, size(testsuites)
            write (error_unit, fmt) "-", testsuites(is)%name
         end do
         error stop 1
      end if
   else
      do is = 1, size(testsuites)
         write (error_unit, fmt) "Testing:", testsuites(is)%name
         call run_testsuite(testsuites(is)%collect, error_unit, stat)
      end do
   end if

   if (stat > 0) then
      write (error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
      error stop 1
   end if
end program tester
