!> test-drive entry point for capnp-fortran unit suites.
program tester
   use, intrinsic :: iso_fortran_env, only: error_unit
   use testdrive, only: run_testsuite, testsuite_type, collect_interface, &
        select_suite, run_selected, get_argument
   use test_wire, only: collect_wire
   use test_addressbook, only: collect_addressbook
   use test_canonical, only: collect_canonical
   use test_codecs, only: collect_codecs, release_codecs_tmp
   use test_dynamic, only: collect_dynamic
   use test_annotated, only: collect_annotated
   use test_generated, only: collect_generated
   use test_generic, only: collect_generic
   use test_holder, only: collect_holder
   use test_interop, only: collect_interop
   use test_kitchen, only: collect_kitchen
   use test_parity, only: collect_parity
   use test_rpc, only: collect_rpc
   use test_rpc_handoff, only: collect_rpc_handoff
   use test_rpc_typed, only: collect_rpc_typed
   use test_stream, only: collect_stream
   use test_codegen_parity, only: collect_codegen_parity
   use test_idents, only: collect_idents
   use test_regress, only: collect_regress, release_regress_tmp
   use test_edges, only: collect_edges, release_edges_tmp
   use test_limits, only: collect_limits, release_limits_tmp
   use cli_path, only: publish_cli_paths
   implicit none
   integer :: stat, is
   character(len=:), allocatable :: suite_name, test_name
   type(testsuite_type), allocatable :: testsuites(:)
   character(len=*), parameter :: fmt = '("#", *(1x, a))'

   call publish_cli_paths()
   stat = 0
   call build_suites(testsuites)

   call get_argument(1, suite_name)
   call get_argument(2, test_name)

   if (allocated(suite_name)) then
      is = select_suite(testsuites, suite_name)
      if (is > 0 .and. is <= size(testsuites)) then
         if (allocated(test_name)) then
            write (error_unit, fmt) "Suite:", testsuites(is)%name
            call run_selected(testsuites(is)%collect, test_name, error_unit, stat)
            if (stat < 0) then
               call release_temps()
               call drop_suites(testsuites)
               call drop_args(suite_name, test_name)
               error stop 1
            end if
         else
            write (error_unit, fmt) "Testing:", testsuites(is)%name
            call run_testsuite(testsuites(is)%collect, error_unit, stat)
         end if
      else
         write (error_unit, fmt) "Available testsuites"
         do is = 1, size(testsuites)
            write (error_unit, fmt) "-", testsuites(is)%name
         end do
         call release_temps()
         call drop_suites(testsuites)
         call drop_args(suite_name, test_name)
         error stop 1
      end if
   else
      do is = 1, size(testsuites)
         write (error_unit, fmt) "Testing:", testsuites(is)%name
         call run_testsuite(testsuites(is)%collect, error_unit, stat)
      end do
   end if

   call release_temps()
   if (stat > 0) then
      write (error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
      call drop_suites(testsuites)
      call drop_args(suite_name, test_name)
      error stop 1
   end if
   call drop_suites(testsuites)
   call drop_args(suite_name, test_name)

contains

   subroutine build_suites(testsuites)
      type(testsuite_type), allocatable, intent(out) :: testsuites(:)
      ! One assignment per suite. An array constructor of these values
      ! leaves the suite-name temporaries allocated on gfortran.
      allocate (testsuites(21))
      call fill_suite(testsuites(1), "wire", collect_wire)
      call fill_suite(testsuites(2), "addressbook", collect_addressbook)
      call fill_suite(testsuites(3), "canonical", collect_canonical)
      call fill_suite(testsuites(4), "codecs", collect_codecs)
      call fill_suite(testsuites(5), "dynamic", collect_dynamic)
      call fill_suite(testsuites(6), "annotated", collect_annotated)
      call fill_suite(testsuites(7), "generated", collect_generated)
      call fill_suite(testsuites(8), "generic", collect_generic)
      call fill_suite(testsuites(9), "holder", collect_holder)
      call fill_suite(testsuites(10), "interop", collect_interop)
      call fill_suite(testsuites(11), "kitchen", collect_kitchen)
      call fill_suite(testsuites(12), "parity", collect_parity)
      call fill_suite(testsuites(13), "rpc", collect_rpc)
      call fill_suite(testsuites(14), "rpc_handoff", collect_rpc_handoff)
      call fill_suite(testsuites(15), "rpc_typed", collect_rpc_typed)
      call fill_suite(testsuites(16), "stream", collect_stream)
      call fill_suite(testsuites(17), "codegen_parity", collect_codegen_parity)
      call fill_suite(testsuites(18), "idents", collect_idents)
      call fill_suite(testsuites(19), "regress", collect_regress)
      call fill_suite(testsuites(20), "edges", collect_edges)
      call fill_suite(testsuites(21), "limits", collect_limits)
   end subroutine build_suites

   subroutine fill_suite(suite, name, collect)
      type(testsuite_type), intent(out) :: suite
      character(len=*), intent(in) :: name
      procedure(collect_interface) :: collect
      suite%name = name
      suite%collect => collect
   end subroutine fill_suite

   subroutine release_temps()
      call release_codecs_tmp()
      call release_regress_tmp()
      call release_edges_tmp()
      call release_limits_tmp()
   end subroutine release_temps

   subroutine drop_suites(testsuites)
      type(testsuite_type), allocatable, intent(inout) :: testsuites(:)
      if (allocated(testsuites)) deallocate (testsuites)
   end subroutine drop_suites

   subroutine drop_args(suite_name, test_name)
      character(len=:), allocatable, intent(inout) :: suite_name, test_name
      if (allocated(suite_name)) deallocate (suite_name)
      if (allocated(test_name)) deallocate (test_name)
   end subroutine drop_args
end program tester
