!> Shared scalar formatting for the text and JSON codecs.
!>
!> Float text matches KJ's DoubleToBuffer / FloatToBuffer (snprintf %.*g,
!> then one extra retry, then the redundant exponent '+' stripped). Integer
!> text is plain decimal. Data and Text escapes match kj::encodeCEscape.
module capnp_codec
   use, intrinsic :: iso_c_binding, only: c_char, c_double, c_float, c_int, &
      c_loc, c_null_char, c_ptr, c_size_t, c_associated
   use, intrinsic :: ieee_arithmetic, only: ieee_is_nan, ieee_is_finite, &
      ieee_quiet_nan, ieee_value
   use capnp_kinds, only: int8, int16, int32, int64, real32, real64, CAPNP_OK, CAPNP_ERR_ARG
   implicit none
   private

   public :: capnp_fmt_f64, capnp_fmt_f32
   public :: capnp_fmt_i64, capnp_fmt_u64
   public :: capnp_parse_f64, capnp_parse_i64, capnp_parse_u64
   public :: capnp_cescape_text, capnp_cescape_data, capnp_cunescape
   public :: capnp_as_i8, capnp_as_i16, capnp_as_i32u
   public :: capnp_u_of_i8, capnp_u_of_i16, capnp_u_of_i32
   public :: capnp_quiet_nan

   interface
      function c_snprintf_d(buf, n, fmt, prec, val) bind(c, name="snprintf") &
         result(rc)
         import c_char, c_int, c_size_t, c_double
         character(kind=c_char), intent(out) :: buf(*)
         integer(c_size_t), value :: n
         character(kind=c_char), intent(in) :: fmt(*)
         integer(c_int), value :: prec
         real(c_double), value :: val
         integer(c_int) :: rc
      end function c_snprintf_d
      function c_strtod(nptr, endptr) bind(c, name="strtod") result(v)
         import c_double, c_ptr, c_char
         character(kind=c_char), intent(in) :: nptr(*)
         type(c_ptr), intent(out) :: endptr
         real(c_double) :: v
      end function c_strtod
      function c_strtof(nptr, endptr) bind(c, name="strtof") result(v)
         import c_float, c_ptr, c_char
         character(kind=c_char), intent(in) :: nptr(*)
         type(c_ptr), intent(out) :: endptr
         real(c_float) :: v
      end function c_strtof
   end interface

contains

   function capnp_quiet_nan() result(x)
      real(real64) :: x
      x = ieee_value(0.0_real64, ieee_quiet_nan)
   end function capnp_quiet_nan

   subroutine capnp_fmt_f64(x, s)
      real(real64), intent(in) :: x
      character(len=:), allocatable, intent(out) :: s
      real(real64) :: y
      integer :: err
      if (ieee_is_nan(x)) then
         s = 'nan'
         return
      end if
      if (.not. ieee_is_finite(x)) then
         if (neg_bit64(x)) then
            s = '-inf'
         else
            s = 'inf'
         end if
         return
      end if
      call fmt_g(real(x, c_double), 15, s)
      call capnp_parse_f64(s, y, err)
      if (err /= CAPNP_OK .or. .not. f64_same(y, x)) &
         call fmt_g(real(x, c_double), 17, s)
   end subroutine capnp_fmt_f64

   subroutine capnp_fmt_f32(x, s)
      real(real32), intent(in) :: x
      character(len=:), allocatable, intent(out) :: s
      real(c_float) :: y
      integer :: err
      if (ieee_is_nan(x)) then
         s = 'nan'
         return
      end if
      if (.not. ieee_is_finite(x)) then
         if (neg_bit32(x)) then
            s = '-inf'
         else
            s = 'inf'
         end if
         return
      end if
      call fmt_g(real(x, c_double), 6, s)
      call parse_f32(s, y, err)
      if (err /= 0 .or. .not. f32_same(real(y, real32), x)) &
         call fmt_g(real(x, c_double), 8, s)
   end subroutine capnp_fmt_f32

   subroutine capnp_fmt_i64(v, s)
      integer(int64), intent(in) :: v
      character(len=:), allocatable, intent(out) :: s
      integer(int64) :: mag
      character(len=:), allocatable :: body
      if (v == int(z'8000000000000000', int64)) then
         s = '-9223372036854775808'
         return
      end if
      if (v < 0_int64) then
         mag = -v
         call capnp_fmt_u64(mag, body)
         s = '-'//body
      else
         call capnp_fmt_u64(v, s)
      end if
   end subroutine capnp_fmt_i64

   subroutine capnp_fmt_u64(bits, s)
      integer(int64), intent(in) :: bits
      character(len=:), allocatable, intent(out) :: s
      character(len=20) :: rev
      integer(int64) :: x, q
      integer :: d, n
      if (bits == 0_int64) then
         s = '0'
         return
      end if
      x = bits
      n = 0
      do while (x /= 0_int64)
         call u64_divmod10(x, q, d)
         n = n + 1
         rev(n:n) = achar(iachar('0') + d)
         x = q
      end do
      allocate (character(len=n) :: s)
      do d = 1, n
         s(d:d) = rev(n - d + 1:n - d + 1)
      end do
   end subroutine capnp_fmt_u64

   subroutine capnp_parse_f64(tok, x, err)
      character(len=*), intent(in) :: tok
      real(real64), intent(out) :: x
      integer, intent(out) :: err
      character(kind=c_char), allocatable, target :: cs(:)
      type(c_ptr) :: endp
      real(c_double) :: y
      integer :: i, n
      err = CAPNP_OK
      x = 0.0_real64
      n = len(tok)
      if (n == 0) then
         err = CAPNP_ERR_ARG
         return
      end if
      allocate (cs(n + 1))
      do i = 1, n
         cs(i) = tok(i:i)
      end do
      cs(n + 1) = c_null_char
      y = c_strtod(cs, endp)
      x = real(y, real64)
      if (.not. c_associated(endp, c_loc(cs(n + 1)))) err = CAPNP_ERR_ARG
   end subroutine capnp_parse_f64

   subroutine capnp_parse_i64(tok, v, err)
      character(len=*), intent(in) :: tok
      integer(int64), intent(out) :: v
      integer, intent(out) :: err
      integer(int64) :: bits
      logical :: neg
      character(len=:), allocatable :: body
      call split_sign(tok, neg, body, err)
      if (err /= CAPNP_OK) return
      call parse_u64_body(body, bits, err)
      if (err /= CAPNP_OK) return
      if (neg) then
         if (bits == int(z'8000000000000000', int64)) then
            v = bits
         else if (iand(bits, int(z'8000000000000000', int64)) /= 0_int64) then
            err = CAPNP_ERR_ARG
         else
            v = -bits
         end if
      else
         if (iand(bits, int(z'8000000000000000', int64)) /= 0_int64) then
            err = CAPNP_ERR_ARG
         else
            v = bits
         end if
      end if
   end subroutine capnp_parse_i64

   subroutine capnp_parse_u64(tok, v, err)
      character(len=*), intent(in) :: tok
      integer(int64), intent(out) :: v
      integer, intent(out) :: err
      logical :: neg
      character(len=:), allocatable :: body
      call split_sign(tok, neg, body, err)
      if (err /= CAPNP_OK) return
      if (neg) then
         err = CAPNP_ERR_ARG
         return
      end if
      call parse_u64_body(body, v, err)
   end subroutine capnp_parse_u64

   subroutine capnp_cescape_text(str, out)
      character(len=*), intent(in) :: str
      character(len=:), allocatable, intent(out) :: out
      integer :: i
      out = ''
      do i = 1, len(str)
         call escape_byte(iachar(str(i:i)), .false., out)
      end do
   end subroutine capnp_cescape_text

   subroutine capnp_cescape_data(bytes, out)
      integer(int8), intent(in) :: bytes(:)
      character(len=:), allocatable, intent(out) :: out
      integer :: i
      out = ''
      do i = 1, size(bytes)
         call escape_byte(capnp_u_of_i8(bytes(i)), .true., out)
      end do
   end subroutine capnp_cescape_data

   !> Decode C-style escapes (octal, hex, and \\u/\\U) into a byte string.
   subroutine capnp_cunescape(src, out, err)
      character(len=*), intent(in) :: src
      character(len=:), allocatable, intent(out) :: out
      integer, intent(out) :: err
      integer :: i, n, d, cp, nhex, v
      err = CAPNP_OK
      out = ''
      n = len(src)
      i = 1
      do while (i <= n)
         if (src(i:i) /= '\') then
            out = out//src(i:i)
            i = i + 1
            cycle
         end if
         i = i + 1
         if (i > n) then
            err = CAPNP_ERR_ARG
            return
         end if
         select case (src(i:i))
         case ('a'); out = out//achar(7)
         case ('b'); out = out//achar(8)
         case ('f'); out = out//achar(12)
         case ('n'); out = out//achar(10)
         case ('r'); out = out//achar(13)
         case ('t'); out = out//achar(9)
         case ('v'); out = out//achar(11)
         case ("'"); out = out//"'"
         case ('"'); out = out//'"'
         case ('\'); out = out//'\'
         case ('0':'7')
            v = iachar(src(i:i)) - iachar('0')
            i = i + 1
            nhex = 0
            do while (nhex < 2 .and. i <= n)
               d = oct_digit(src(i:i))
               if (d < 0) exit
               v = v*8 + d
               i = i + 1
               nhex = nhex + 1
            end do
            if (v > 255) then
               err = CAPNP_ERR_ARG
               return
            end if
            out = out//achar(v)
            cycle
         case ('x')
            i = i + 1
            v = 0
            nhex = 0
            do while (i <= n)
               d = hex_digit(src(i:i))
               if (d < 0) exit
               v = v*16 + d
               i = i + 1
               nhex = nhex + 1
            end do
            if (nhex == 0 .or. v > 255) then
               err = CAPNP_ERR_ARG
               return
            end if
            out = out//achar(v)
            cycle
         case ('u', 'U')
            nhex = 4
            if (src(i:i) == 'U') nhex = 8
            i = i + 1
            cp = 0
            if (i + nhex - 1 > n) then
               err = CAPNP_ERR_ARG
               return
            end if
            do d = 1, nhex
               v = hex_digit(src(i:i))
               if (v < 0) then
                  err = CAPNP_ERR_ARG
                  return
               end if
               cp = cp*16 + v
               i = i + 1
            end do
            call append_cp(out, cp, err)
            if (err /= CAPNP_OK) return
            cycle
         case default
            out = out//src(i:i)
         end select
         i = i + 1
      end do
   end subroutine capnp_cunescape

   pure function capnp_as_i8(u) result(v)
      integer, intent(in) :: u
      integer(int8) :: v
      integer :: x
      x = iand(u, 255)
      if (x >= 128) then
         v = int(x - 256, int8)
      else
         v = int(x, int8)
      end if
   end function capnp_as_i8

   pure function capnp_as_i16(u) result(v)
      integer, intent(in) :: u
      integer(int16) :: v
      integer :: x
      x = iand(u, 65535)
      if (x >= 32768) then
         v = int(x - 65536, int16)
      else
         v = int(x, int16)
      end if
   end function capnp_as_i16

   pure function capnp_as_i32u(u) result(v)
      integer(int64), intent(in) :: u
      integer(int32) :: v
      integer(int64) :: x
      x = iand(u, 4294967295_int64)
      if (x > 2147483647_int64) then
         v = int(x - 4294967296_int64, int32)
      else
         v = int(x, int32)
      end if
   end function capnp_as_i32u

   pure function capnp_u_of_i8(s) result(u)
      integer(int8), intent(in) :: s
      integer :: u
      u = iand(int(s, int32), 255)
   end function capnp_u_of_i8

   pure function capnp_u_of_i16(s) result(u)
      integer(int16), intent(in) :: s
      integer :: u
      u = iand(int(s, int32), 65535)
   end function capnp_u_of_i16

   pure function capnp_u_of_i32(s) result(u)
      integer(int32), intent(in) :: s
      integer(int64) :: u
      u = iand(int(s, int64), 4294967295_int64)
   end function capnp_u_of_i32

   subroutine fmt_g(val, prec, s)
      real(c_double), intent(in) :: val
      integer, intent(in) :: prec
      character(len=:), allocatable, intent(out) :: s
      character(kind=c_char) :: buf(64), fmt(6)
      integer(c_int) :: rc
      integer :: i, n
      character(len=64) :: tmp
      fmt(1) = c_char_'%'
      fmt(2) = c_char_'.'
      fmt(3) = c_char_'*'
      fmt(4) = c_char_'g'
      fmt(5) = c_null_char
      fmt(6) = c_char_' '
      rc = c_snprintf_d(buf, 64_c_size_t, fmt, int(prec, c_int), val)
      n = 0
      do i = 1, 63
         if (buf(i) == c_null_char) exit
         if (buf(i) == c_char_'+') cycle
         n = n + 1
         tmp(n:n) = buf(i)
      end do
      if (rc < 0 .or. n == 0) then
         s = '0'
      else
         s = tmp(1:n)
      end if
   end subroutine fmt_g

   subroutine parse_f32(tok, x, err)
      character(len=*), intent(in) :: tok
      real(c_float), intent(out) :: x
      integer, intent(out) :: err
      character(kind=c_char), allocatable, target :: cs(:)
      type(c_ptr) :: endp
      integer :: i, n
      err = CAPNP_OK
      n = len(tok)
      allocate (cs(n + 1))
      do i = 1, n
         cs(i) = tok(i:i)
      end do
      cs(n + 1) = c_null_char
      x = c_strtof(cs, endp)
      if (.not. c_associated(endp, c_loc(cs(n + 1)))) err = CAPNP_ERR_ARG
   end subroutine parse_f32

   pure function neg_bit64(x) result(y)
      real(real64), intent(in) :: x
      logical :: y
      y = iand(transfer(x, 0_int64), int(z'8000000000000000', int64)) /= 0_int64
   end function neg_bit64

   pure function neg_bit32(x) result(y)
      real(real32), intent(in) :: x
      logical :: y
      y = iand(transfer(x, 0_int32), int(z'80000000', int32)) /= 0_int32
   end function neg_bit32

   pure function f64_same(a, b) result(y)
      real(real64), intent(in) :: a, b
      logical :: y
      integer(int64) :: ba, bb
      ba = transfer(a, 0_int64)
      bb = transfer(b, 0_int64)
      if (ba == bb) then
         y = .true.
      else
         y = iand(ba, int(z'7fffffffffffffff', int64)) == 0_int64 .and. &
             iand(bb, int(z'7fffffffffffffff', int64)) == 0_int64
      end if
   end function f64_same

   pure function f32_same(a, b) result(y)
      real(real32), intent(in) :: a, b
      logical :: y
      integer(int32) :: ba, bb
      ba = transfer(a, 0_int32)
      bb = transfer(b, 0_int32)
      if (ba == bb) then
         y = .true.
      else
         y = iand(ba, int(z'7fffffff', int32)) == 0_int32 .and. &
             iand(bb, int(z'7fffffff', int32)) == 0_int32
      end if
   end function f32_same

   subroutine u64_divmod10(bits, q, r)
      integer(int64), intent(in) :: bits
      integer(int64), intent(out) :: q
      integer, intent(out) :: r
      integer(int64) :: hi, lo, rest, hi_div, lo_div
      hi = iand(shiftr(bits, 32), 4294967295_int64)
      lo = iand(bits, 4294967295_int64)
      hi_div = hi/10_int64
      rest = mod(hi, 10_int64)*4294967296_int64 + lo
      lo_div = rest/10_int64
      r = int(mod(rest, 10_int64))
      q = ior(shiftl(hi_div, 32), lo_div)
   end subroutine u64_divmod10

   subroutine u64_mul_add(x, mul, d, err)
      integer(int64), intent(inout) :: x
      integer(int64), intent(in) :: mul
      integer, intent(in) :: d
      integer, intent(inout) :: err
      integer(int64) :: hi, lo, t, m
      if (err /= CAPNP_OK) return
      hi = iand(shiftr(x, 32), 4294967295_int64)
      lo = iand(x, 4294967295_int64)
      t = lo*mul + int(d, int64)
      m = hi*mul + shiftr(t, 32)
      if (m > 4294967295_int64) then
         err = CAPNP_ERR_ARG
         return
      end if
      x = ior(shiftl(m, 32), iand(t, 4294967295_int64))
   end subroutine u64_mul_add

   subroutine split_sign(tok, neg, body, err)
      character(len=*), intent(in) :: tok
      logical, intent(out) :: neg
      character(len=:), allocatable, intent(out) :: body
      integer, intent(out) :: err
      err = CAPNP_OK
      neg = .false.
      if (len(tok) == 0) then
         err = CAPNP_ERR_ARG
         body = ''
         return
      end if
      if (tok(1:1) == '+') then
         body = tok(2:)
      else if (tok(1:1) == '-') then
         neg = .true.
         body = tok(2:)
      else
         body = tok
      end if
      if (len(body) == 0) err = CAPNP_ERR_ARG
   end subroutine split_sign

   subroutine parse_u64_body(body, bits, err)
      character(len=*), intent(in) :: body
      integer(int64), intent(out) :: bits
      integer, intent(out) :: err
      integer :: i, d
      logical :: hex
      integer(int64) :: mul
      err = CAPNP_OK
      bits = 0_int64
      i = 1
      hex = .false.
      if (len(body) >= 2) then
         if (body(1:1) == '0' .and. (body(2:2) == 'x' .or. body(2:2) == 'X')) then
            hex = .true.
            i = 3
         end if
      end if
      if (i > len(body)) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (hex) then
         mul = 16_int64
      else
         mul = 10_int64
      end if
      do while (i <= len(body))
         if (hex) then
            d = hex_digit(body(i:i))
         else
            d = dec_digit(body(i:i))
         end if
         if (d < 0) then
            err = CAPNP_ERR_ARG
            return
         end if
         call u64_mul_add(bits, mul, d, err)
         if (err /= CAPNP_OK) return
         i = i + 1
      end do
   end subroutine parse_u64_body

   subroutine escape_byte(ub0, binary, out)
      integer, intent(in) :: ub0
      logical, intent(in) :: binary
      character(len=:), allocatable, intent(inout) :: out
      integer :: ub
      character(len=1) :: d1, d2, d3
      ub = iand(ub0, 255)
      select case (ub)
      case (7); out = out//'\a'
      case (8); out = out//'\b'
      case (12); out = out//'\f'
      case (10); out = out//'\n'
      case (13); out = out//'\r'
      case (9); out = out//'\t'
      case (11); out = out//'\v'
      case (39); out = out//"\'"
      case (34); out = out//'\"'
      case (92); out = out//'\\'
      case default
         if (ub < 32 .or. ub == 127 .or. (binary .and. ub > 127)) then
            d1 = achar(iachar('0') + ub/64)
            d2 = achar(iachar('0') + mod(ub/8, 8))
            d3 = achar(iachar('0') + mod(ub, 8))
            out = out//'\'//d1//d2//d3
         else
            out = out//achar(ub)
         end if
      end select
   end subroutine escape_byte

   subroutine append_cp(out, cp, err)
      character(len=:), allocatable, intent(inout) :: out
      integer, intent(in) :: cp
      integer, intent(inout) :: err
      integer :: c
      if (err /= CAPNP_OK) return
      c = cp
      if (c < 0) then
         err = CAPNP_ERR_ARG
         return
      else if (c <= 127) then
         out = out//achar(c)
      else if (c <= 2047) then
         out = out//achar(ior(192, ishft(c, -6)))//achar(ior(128, iand(c, 63)))
      else if (c <= 65535) then
         out = out//achar(ior(224, ishft(c, -12)))// &
               achar(ior(128, iand(ishft(c, -6), 63)))//achar(ior(128, iand(c, 63)))
      else if (c <= 1114111) then
         out = out//achar(ior(240, ishft(c, -18)))// &
               achar(ior(128, iand(ishft(c, -12), 63)))// &
               achar(ior(128, iand(ishft(c, -6), 63)))//achar(ior(128, iand(c, 63)))
      else
         err = CAPNP_ERR_ARG
      end if
   end subroutine append_cp

   pure function dec_digit(c) result(d)
      character(len=1), intent(in) :: c
      integer :: d
      if (c >= '0' .and. c <= '9') then
         d = iachar(c) - iachar('0')
      else
         d = -1
      end if
   end function dec_digit

   pure function oct_digit(c) result(d)
      character(len=1), intent(in) :: c
      integer :: d
      if (c >= '0' .and. c <= '7') then
         d = iachar(c) - iachar('0')
      else
         d = -1
      end if
   end function oct_digit

   pure function hex_digit(c) result(d)
      character(len=1), intent(in) :: c
      integer :: d
      if (c >= '0' .and. c <= '9') then
         d = iachar(c) - iachar('0')
      else if (c >= 'a' .and. c <= 'f') then
         d = iachar(c) - iachar('a') + 10
      else if (c >= 'A' .and. c <= 'F') then
         d = iachar(c) - iachar('A') + 10
      else
         d = -1
      end if
   end function hex_digit

end module capnp_codec
