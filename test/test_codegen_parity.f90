!> Schema compiler parity with Cap'n Proto C++ 1.4.0.
module test_codegen_parity
   use testdrive, only: new_unittest, unittest_type, error_type, check
   implicit none
   private
   public :: collect_codegen_parity
contains
   subroutine collect_codegen_parity(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [new_unittest("g1_min_signed", test_g1)]
   end subroutine collect_codegen_parity

   subroutine test_g1(error)
      type(error_type), allocatable, intent(out) :: error
      integer :: stat, cs
      call execute_command_line("bash test/codegen_parity.sh g1", &
                                exitstat=stat, cmdstat=cs)
      call check(error, cs == 0 .and. stat == 0, "g1")
   end subroutine test_g1
end module test_codegen_parity
