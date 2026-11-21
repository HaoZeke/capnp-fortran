program test_fixgate
   use, intrinsic :: iso_fortran_env, only: real64, real32, int64
   use capnp
   use capnp_codec
   use capnp_json
   implicit none
   integer :: nfail
   nfail = 0
   call case_fmt()
   call case_f32()
   call case_oct()
   call case_esc()
   call case_span()
   call case_real()
   call case_fits()
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


   subroutine case_oct()
      integer(int64) :: v
      integer :: err
      call capnp_parse_i64("010", v, err, allow_octal=.true.)
      if (err /= 0 .or. v /= 8_int64) call bad()
   end subroutine case_oct


   subroutine case_esc()
      character(len=:), allocatable :: out
      integer :: err
      call capnp_cunescape("\q", out, err)
      if (err == 0) call bad()
      call capnp_cunescape("\x41", out, err)
      if (err /= 0 .or. out /= "A") call bad()
   end subroutine case_esc


   subroutine case_span()
      integer :: beg, to, err
      call capnp_json_span('{"a":1} {"b":2}', 1, beg, to, err)
      if (err /= 0 .or. beg < 1 .or. to < beg) call bad()
   end subroutine case_span


   subroutine case_real()
      integer(int64) :: iv
      integer :: err
      call capnp_f64_to_i64(-9223372036854775808.0_real64, iv, err)
      if (err /= 0) call bad()
      if (iv /= -9223372036854775807_int64 - 1_int64) call bad()
   end subroutine case_real


   subroutine case_fits()
      type(capnp_ptr_t) :: p
      p = capnp_ptr_t()
      if (capnp_list_fits(p, 2)) call bad()
   end subroutine case_fits

end program
