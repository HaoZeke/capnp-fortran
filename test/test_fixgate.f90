program test_fixgate
   use, intrinsic :: iso_fortran_env, only: real64, real32, int64
   use capnp
   use capnp_codec
   implicit none
   integer :: nfail
   nfail = 0
   call case_fmt()
   call case_f32()
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


   subroutine case_f32()
      real(real32) :: y
      integer :: err
      call capnp_parse_f32("1.5", y, err)
      if (err /= 0) call bad()
      if (abs(y - 1.5_real32) > 1.0e-6_real32) call bad()
   end subroutine case_f32

end program
