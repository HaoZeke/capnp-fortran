!> Generator regressions: multi-file generics, suffixed names, annotation
!> stems, and checked-in modules that drift from the plugin.
module test_codegen
   use testdrive, only: new_unittest, unittest_type, error_type, check, skip_test
   implicit none
   private
   public :: collect_codegen

contains

   subroutine collect_codegen(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [new_unittest("codegen-multifile-generics", run_multifile), &
                   new_unittest("codegen-suffix-clamp", run_suffix), &
                   new_unittest("codegen-annotation-stem", run_annot)]
   end subroutine collect_codegen

   subroutine run_multifile(error)
      type(error_type), allocatable, intent(out) :: error
      call run_mode(error, 'multifile', 'codegen: two generic files')
   end subroutine run_multifile

   subroutine run_suffix(error)
      type(error_type), allocatable, intent(out) :: error
      call run_mode(error, 'suffix', 'codegen: suffixed names fit')
   end subroutine run_suffix

   subroutine run_annot(error)
      type(error_type), allocatable, intent(out) :: error
      call run_mode(error, 'annot', 'codegen: annotation stem')
   end subroutine run_annot

   subroutine run_mode(error, mode, what)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), intent(in) :: mode, what
      integer :: stat, cs
      call execute_command_line('bash test/codegen_check.sh '//mode, &
                                exitstat=stat, cmdstat=cs)
      if (cs /= 0) then
         call skip_test(error, 'codegen helper could not start')
         return
      end if
      call check(error, stat == 0, what)
   end subroutine run_mode

end module test_codegen
