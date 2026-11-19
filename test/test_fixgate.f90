program test_fixgate
   use, intrinsic :: iso_fortran_env, only: real64, real32, int64
   use capnp
   use capnp_codec
   implicit none
   integer :: nfail
   nfail = 0
   call case_fmt()
   if (nfail /= 0) error stop 1
contains
   subroutine bad()
      nfail = nfail + 1
   end subroutine bad

   subroutine case_fmt()
      character(len=:), allocatable :: s
      call capnp_fmt_f64(1.25_real64, s)
      if (.not. allocated(s)) call bad()
      if (index(s, "1.25") /= 1) call bad()
   end subroutine case_fmt

end program
