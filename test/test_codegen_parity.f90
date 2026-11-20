!> Schema compiler parity with Cap'n Proto C++ 1.4.0.
module test_codegen_parity
   use testdrive, only: new_unittest, unittest_type, error_type, check
   implicit none
   private
   public :: collect_codegen_parity
contains
   subroutine collect_codegen_parity(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [ &
         new_unittest("g1_min_signed", test_g1), &
         new_unittest("g2_text_escape", test_g2), &
         new_unittest("g3_default_list_copy", test_g3), &
         new_unittest("g4_get_elem_default", test_g4), &
         new_unittest("g5_extends", test_g5) &
         ]
   end subroutine collect_codegen_parity

   subroutine run_gap(error, gap)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), intent(in) :: gap
      integer :: stat, cs
      call execute_command_line("bash test/codegen_parity.sh "//gap, &
                                exitstat=stat, cmdstat=cs)
      call check(error, cs == 0 .and. stat == 0, gap)
   end subroutine run_gap

   subroutine test_g1(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, "g1")
   end subroutine test_g1

   subroutine test_g2(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, "g2")
   end subroutine test_g2

   subroutine test_g3(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, "g3")
   end subroutine test_g3

   subroutine test_g4(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, "g4")
   end subroutine test_g4

   subroutine test_g5(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, "g5")
   end subroutine test_g5
end module test_codegen_parity
