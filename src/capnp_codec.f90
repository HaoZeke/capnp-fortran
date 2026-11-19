!> Shared scalar formatting, escaping, schema defaults, and builder allocation
!> for the text and JSON codecs.
!>
!> Float text matches KJ's DoubleToBuffer / FloatToBuffer (snprintf %.*g,
!> then one extra retry, then the redundant exponent '+' stripped). Integer
!> text is plain decimal, hex, or (for Cap'n Proto text literals) octal.
!> Data and Text escapes match kj::encodeCEscape / kj::decodeCEscape.
module capnp_codec
   use, intrinsic :: iso_c_binding, only: c_char, c_double, c_float, &
      c_loc, c_null_char, c_ptr, c_associated
   use, intrinsic :: ieee_arithmetic, only: ieee_is_nan, ieee_is_finite, &
      ieee_quiet_nan, ieee_value
   use capnp
   use capnp_schema
   use capnp_dynamic, only: capnp_dyn_schema_t
   implicit none
   private

   public :: capnp_fmt_f64, capnp_fmt_f32
   public :: capnp_fmt_i64, capnp_fmt_u64
   public :: capnp_parse_f64, capnp_parse_f32, capnp_parse_i64, capnp_parse_u64
   public :: capnp_cescape_text, capnp_cescape_data, capnp_cunescape
   public :: capnp_as_i8, capnp_as_i16, capnp_as_i32u
   public :: capnp_u_of_i8, capnp_u_of_i16, capnp_u_of_i32
   public :: capnp_quiet_nan
   public :: capnp_codec_find_id, capnp_codec_find_field

   interface
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
      call fmt_g(x, 15, s)
      call capnp_parse_f64(s, y, err)
      if (err /= CAPNP_OK .or. .not. f64_same(y, x)) &
         call fmt_g(x, 17, s)
   end subroutine capnp_fmt_f64

   subroutine capnp_fmt_f32(x, s)
      real(real32), intent(in) :: x
      character(len=:), allocatable, intent(out) :: s
      real(real32) :: y
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
      call fmt_g(real(x, real64), 6, s)
      call capnp_parse_f32(s, y, err)
      if (err /= CAPNP_OK .or. .not. f32_same(y, x)) &
         call fmt_g(real(x, real64), 8, s)
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

   subroutine capnp_parse_f32(tok, x, err)
      character(len=*), intent(in) :: tok
      real(real32), intent(out) :: x
      integer, intent(out) :: err
      character(kind=c_char), allocatable, target :: cs(:)
      type(c_ptr) :: endp
      real(c_float) :: y
      integer :: i, n
      err = CAPNP_OK
      x = 0.0_real32
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
      y = c_strtof(cs, endp)
      x = real(y, real32)
      if (.not. c_associated(endp, c_loc(cs(n + 1)))) err = CAPNP_ERR_ARG
   end subroutine capnp_parse_f32

   subroutine capnp_parse_i64(tok, v, err, allow_octal)
      character(len=*), intent(in) :: tok
      integer(int64), intent(out) :: v
      integer, intent(out) :: err
      logical, intent(in), optional :: allow_octal
      integer(int64) :: bits
      logical :: neg, oct
      character(len=:), allocatable :: body
      oct = .false.
      if (present(allow_octal)) oct = allow_octal
      call split_sign(tok, neg, body, err)
      if (err /= CAPNP_OK) return
      call parse_u64_body(body, bits, oct, err)
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

   subroutine capnp_parse_u64(tok, v, err, allow_octal)
      character(len=*), intent(in) :: tok
      integer(int64), intent(out) :: v
      integer, intent(out) :: err
      logical, intent(in), optional :: allow_octal
      logical :: neg, oct
      character(len=:), allocatable :: body
      oct = .false.
      if (present(allow_octal)) oct = allow_octal
      call split_sign(tok, neg, body, err)
      if (err /= CAPNP_OK) return
      if (neg) then
         err = CAPNP_ERR_ARG
         return
      end if
      call parse_u64_body(body, v, oct, err)
   end subroutine capnp_parse_u64

   subroutine capnp_cescape_text(str, out)
      character(len=*), intent(in) :: str
      character(len=:), allocatable, intent(out) :: out
      character(len=:), allocatable :: buf
      integer :: i, m
      if (len(str) == 0) then
         out = ''
         return
      end if
      allocate (character(len=len(str)*4) :: buf)
      m = 0
      do i = 1, len(str)
         call escape_byte(iachar(str(i:i)), .false., buf, m)
      end do
      out = buf(1:m)
   end subroutine capnp_cescape_text

   subroutine capnp_cescape_data(bytes, out)
      integer(int8), intent(in) :: bytes(:)
      character(len=:), allocatable, intent(out) :: out
      character(len=:), allocatable :: buf
      integer :: i, m
      if (size(bytes) == 0) then
         out = ''
         return
      end if
      allocate (character(len=size(bytes)*4) :: buf)
      m = 0
      do i = 1, size(bytes)
         call escape_byte(capnp_u_of_i8(bytes(i)), .true., buf, m)
      end do
      out = buf(1:m)
   end subroutine capnp_cescape_data

   !> Decode C-style escapes (1-3 octal digits and \xHH) into a byte string.
   subroutine capnp_cunescape(src, out, err)
      character(len=*), intent(in) :: src
      character(len=:), allocatable, intent(out) :: out
      integer, intent(out) :: err
      character(len=:), allocatable :: buf
      integer :: i, n, m, d, d2, noct, v
      err = CAPNP_OK
      n = len(src)
      if (n == 0) then
         out = ''
         return
      end if
      allocate (character(len=n) :: buf)
      m = 0
      i = 1
      do while (i <= n)
         if (src(i:i) /= '\') then
            m = m + 1
            buf(m:m) = src(i:i)
            i = i + 1
            cycle
         end if
         i = i + 1
         if (i > n) then
            err = CAPNP_ERR_ARG
            out = ''
            return
         end if
         select case (src(i:i))
         case ('a'); m = m + 1; buf(m:m) = achar(7)
         case ('b'); m = m + 1; buf(m:m) = achar(8)
         case ('f'); m = m + 1; buf(m:m) = achar(12)
         case ('n'); m = m + 1; buf(m:m) = achar(10)
         case ('r'); m = m + 1; buf(m:m) = achar(13)
         case ('t'); m = m + 1; buf(m:m) = achar(9)
         case ('v'); m = m + 1; buf(m:m) = achar(11)
         case ("'"); m = m + 1; buf(m:m) = "'"
         case ('"'); m = m + 1; buf(m:m) = '"'
         case ('?'); m = m + 1; buf(m:m) = '?'
         case ('\'); m = m + 1; buf(m:m) = '\'
         case ('0':'7')
            v = iachar(src(i:i)) - iachar('0')
            i = i + 1
            noct = 0
            do while (noct < 2 .and. i <= n)
               d = oct_digit(src(i:i))
               if (d < 0) exit
               v = v*8 + d
               i = i + 1
               noct = noct + 1
            end do
            if (v > 255) then
               err = CAPNP_ERR_ARG
               out = ''
               return
            end if
            m = m + 1
            buf(m:m) = achar(v)
            cycle
         case ('x')
            if (i + 2 > n) then
               err = CAPNP_ERR_ARG
               out = ''
               return
            end if
            d = hex_digit(src(i + 1:i + 1))
            d2 = hex_digit(src(i + 2:i + 2))
            if (d < 0 .or. d2 < 0) then
               err = CAPNP_ERR_ARG
               out = ''
               return
            end if
            m = m + 1
            buf(m:m) = achar(d*16 + d2)
            i = i + 3
            cycle
         case default
            err = CAPNP_ERR_ARG
            out = ''
            return
         end select
         i = i + 1
      end do
      out = buf(1:m)
   end subroutine capnp_cunescape

   ! --- Schema, brand, default, and builder helpers ---------------------

   function capnp_codec_find_id(schema, id) result(idx)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer(int64), intent(in) :: id
      integer :: idx, i
      idx = 0
      if (.not. schema%loaded) return
      do i = 1, size(schema%nodes)
         if (node_id(schema%nodes(i)) == id) then
            idx = i
            return
         end if
      end do
   end function capnp_codec_find_id

   subroutine capnp_codec_find_field(schema, idx, name, f, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      character(len=*), intent(in) :: name
      type(capnp_ptr_t), intent(out) :: f
      integer, intent(out) :: err
      type(capnp_ptr_t) :: fl
      character(len=:), allocatable :: nm
      integer :: i, n
      err = CAPNP_OK
      fl = node_struct_fields(schema%nodes(idx), err)
      if (err /= CAPNP_OK) return
      n = int(capnp_list_len(fl))
      do i = 0, n - 1
         f = capnp_list_get_struct(fl, i, err)
         if (err /= CAPNP_OK) return
         call field_name(f, nm, err)
         if (err /= CAPNP_OK) return
         if (nm == name) return
      end do
      err = CAPNP_ERR_ARG
   end subroutine capnp_codec_find_field

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
      real(real64), intent(in) :: val
      integer, intent(in) :: prec
      character(len=:), allocatable, intent(out) :: s
      character(len=64) :: raw, fspec, digits, epart
      character(len=:), allocatable :: sign_s
      integer :: i, m, dot, ep, exp10, aexp, ios
      if (neg_bit64(val)) then
         sign_s = '-'
      else
         sign_s = ''
      end if
      if (val == 0.0_real64) then
         s = sign_s//'0'
         return
      end if
      write (fspec, '(a,i0,a)') '(RN, ES32.', prec - 1, 'E4)'
      write (raw, fspec) abs(val)
      raw = adjustl(raw)
      ep = index(raw, 'E')
      dot = index(raw, '.')
      m = 0
      do i = 1, ep - 1
         if (i == dot) cycle
         m = m + 1
         digits(m:m) = raw(i:i)
      end do
      exp10 = 0
      read (raw(ep + 1:), *, iostat=ios) exp10
      if (ios /= 0) then
         s = '0'
         return
      end if
      do while (m > 1 .and. digits(m:m) == '0')
         m = m - 1
      end do
      if (exp10 >= -4 .and. exp10 < prec) then
         if (exp10 >= m - 1) then
            s = sign_s//digits(1:m)//repeat('0', exp10 - (m - 1))
         else if (exp10 >= 0) then
            s = sign_s//digits(1:exp10 + 1)//'.'//digits(exp10 + 2:m)
         else
            s = sign_s//'0.'//repeat('0', -exp10 - 1)//digits(1:m)
         end if
      else
         if (m == 1) then
            s = sign_s//digits(1:1)
         else
            s = sign_s//digits(1:1)//'.'//digits(2:m)
         end if
         aexp = abs(exp10)
         if (aexp < 100) then
            write (epart, '(i2.2)') aexp
         else
            write (epart, '(i0)') aexp
         end if
         if (exp10 < 0) then
            s = s//'e-'//trim(epart)
         else
            s = s//'e'//trim(epart)
         end if
      end if
   end subroutine fmt_g

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

   subroutine parse_u64_body(body, bits, allow_octal, err)
      character(len=*), intent(in) :: body
      integer(int64), intent(out) :: bits
      logical, intent(in) :: allow_octal
      integer, intent(out) :: err
      integer :: i, d, base
      integer(int64) :: mul
      err = CAPNP_OK
      bits = 0_int64
      i = 1
      base = 10
      if (len(body) >= 2) then
         if (body(1:1) == '0' .and. (body(2:2) == 'x' .or. body(2:2) == 'X')) then
            base = 16
            i = 3
         else if (allow_octal .and. body(1:1) == '0') then
            base = 8
            i = 2
         end if
      end if
      if (i > len(body)) then
         err = CAPNP_ERR_ARG
         return
      end if
      mul = int(base, int64)
      do while (i <= len(body))
         select case (base)
         case (16); d = hex_digit(body(i:i))
         case (8); d = oct_digit(body(i:i))
         case default; d = dec_digit(body(i:i))
         end select
         if (d < 0) then
            err = CAPNP_ERR_ARG
            return
         end if
         call u64_mul_add(bits, mul, d, err)
         if (err /= CAPNP_OK) return
         i = i + 1
      end do
   end subroutine parse_u64_body

   subroutine escape_byte(ub0, binary, buf, m)
      integer, intent(in) :: ub0
      logical, intent(in) :: binary
      character(len=*), intent(inout) :: buf
      integer, intent(inout) :: m
      integer :: ub
      ub = iand(ub0, 255)
      select case (ub)
      case (7); buf(m + 1:m + 2) = '\a'; m = m + 2
      case (8); buf(m + 1:m + 2) = '\b'; m = m + 2
      case (12); buf(m + 1:m + 2) = '\f'; m = m + 2
      case (10); buf(m + 1:m + 2) = '\n'; m = m + 2
      case (13); buf(m + 1:m + 2) = '\r'; m = m + 2
      case (9); buf(m + 1:m + 2) = '\t'; m = m + 2
      case (11); buf(m + 1:m + 2) = '\v'; m = m + 2
      case (39); buf(m + 1:m + 2) = "\'"; m = m + 2
      case (34); buf(m + 1:m + 2) = '\"'; m = m + 2
      case (92); buf(m + 1:m + 2) = '\\'; m = m + 2
      case default
         if (ub < 32 .or. ub == 127 .or. (binary .and. ub > 127)) then
            buf(m + 1:m + 1) = '\'
            buf(m + 2:m + 2) = achar(iachar('0') + ub/64)
            buf(m + 3:m + 3) = achar(iachar('0') + mod(ub/8, 8))
            buf(m + 4:m + 4) = achar(iachar('0') + mod(ub, 8))
            m = m + 4
         else
            m = m + 1
            buf(m:m) = achar(ub)
         end if
      end select
   end subroutine escape_byte

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
