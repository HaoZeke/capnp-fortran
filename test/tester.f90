!> test-drive entry point for capnp-fortran unit suites.
program tester
   use, intrinsic :: iso_fortran_env, only: error_unit
   use testdrive, only: run_testsuite, testsuite_type, &
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
   use test_codegen, only: collect_codegen
   use test_rpc_lifetime, only: collect_rpc_lifetime
   use test_net, only: collect_net
   use test_rpc_peer, only: collect_rpc_peer
   use rpc_servers, only: free_minted_adders
   use test_regress, only: collect_regress, release_regress_tmp
   use test_edges, only: collect_edges, release_edges_tmp
   use test_limits, only: collect_limits, release_limits_tmp
   use test_schema_validate, only: collect_schema_validate
   use test_membrane_redirect, only: collect_membrane_redirect
   use cli_path, only: publish_cli_paths
   implicit none
   integer :: stat, is
   character(len=:), allocatable :: suite_name, test_name
   type(testsuite_type), allocatable :: testsuites(:)
   character(len=*), parameter :: fmt = '("#", *(1x, a))'

   call publish_cli_paths()
   stat = 0
   ! Each name is allocated in the element that owns it. A constructor
   ! of new_testsuite() leaks those strings on this compiler.
   allocate (testsuites(27))
   testsuites(1)%name = "wire"
   testsuites(1)%collect => collect_wire
   testsuites(2)%name = "addressbook"
   testsuites(2)%collect => collect_addressbook
   testsuites(3)%name = "canonical"
   testsuites(3)%collect => collect_canonical
   testsuites(4)%name = "dynamic"
   testsuites(4)%collect => collect_dynamic
   testsuites(5)%name = "annotated"
   testsuites(5)%collect => collect_annotated
   testsuites(6)%name = "generated"
   testsuites(6)%collect => collect_generated
   testsuites(7)%name = "generic"
   testsuites(7)%collect => collect_generic
   testsuites(8)%name = "holder"
   testsuites(8)%collect => collect_holder
   testsuites(9)%name = "interop"
   testsuites(9)%collect => collect_interop
   testsuites(10)%name = "kitchen"
   testsuites(10)%collect => collect_kitchen
   testsuites(11)%name = "parity"
   testsuites(11)%collect => collect_parity
   testsuites(12)%name = "rpc"
   testsuites(12)%collect => collect_rpc
   testsuites(13)%name = "rpc_handoff"
   testsuites(13)%collect => collect_rpc_handoff
   testsuites(14)%name = "rpc_typed"
   testsuites(14)%collect => collect_rpc_typed
   testsuites(15)%name = "stream"
   testsuites(15)%collect => collect_stream
   testsuites(16)%name = "codegen_parity"
   testsuites(16)%collect => collect_codegen_parity
   testsuites(17)%name = "idents"
   testsuites(17)%collect => collect_idents
   testsuites(18)%name = "codegen"
   testsuites(18)%collect => collect_codegen
   testsuites(19)%name = "rpc_lifetime"
   testsuites(19)%collect => collect_rpc_lifetime
   testsuites(20)%name = "net"
   testsuites(20)%collect => collect_net
   testsuites(21)%name = "rpc_peer"
   testsuites(21)%collect => collect_rpc_peer
   testsuites(22)%name = "codecs"
   testsuites(22)%collect => collect_codecs
   testsuites(23)%name = "regress"
   testsuites(23)%collect => collect_regress
   testsuites(24)%name = "edges"
   testsuites(24)%collect => collect_edges
   testsuites(25)%name = "limits"
   testsuites(25)%collect => collect_limits
   testsuites(26)%name = "schema_validate"
   testsuites(26)%collect => collect_schema_validate
   testsuites(27)%name = "membrane_redirect"
   testsuites(27)%collect => collect_membrane_redirect

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
               call release_runner()
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
         call release_runner()
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
      call release_runner()
      error stop 1
   end if
   call release_runner()
   call free_minted_adders()
contains
   subroutine release_runner()
      if (allocated(testsuites)) deallocate (testsuites)
      if (allocated(suite_name)) deallocate (suite_name)
      if (allocated(test_name)) deallocate (test_name)
   end subroutine release_runner

   subroutine release_temps()
      call release_codecs_tmp()
      call release_regress_tmp()
      call release_edges_tmp()
      call release_limits_tmp()
   end subroutine release_temps
end program tester
