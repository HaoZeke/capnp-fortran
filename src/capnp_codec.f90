!> Shared scalar formatting, escaping, schema defaults, builder allocation,
!> and generic brand resolution for the text and JSON codecs.
!>
!> Float text matches KJ's DoubleToBuffer / FloatToBuffer (snprintf %.*g,
!> then one extra retry, then the redundant exponent '+' stripped). Integer
!> text is plain decimal, hex, or (for Cap'n Proto text literals) octal.
!> Data and Text escapes match kj::encodeCEscape / kj::decodeCEscape.
module capnp_codec
   use, intrinsic :: ieee_arithmetic, only: ieee_is_nan, ieee_is_finite, &
      ieee_quiet_nan, ieee_value
   use capnp
   use capnp_schema
   use capnp_dynamic, only: capnp_dyn_schema_t, capnp_dyn_brand_t, capnp_dyn_load_brand
   implicit none
   private

   public :: capnp_fmt_f64, capnp_fmt_f32
   public :: capnp_fmt_i64, capnp_fmt_u64
   public :: capnp_parse_f64, capnp_parse_f32, capnp_parse_i64, capnp_parse_u64
   public :: capnp_cescape_text, capnp_cescape_data, capnp_cunescape
   public :: capnp_as_i8, capnp_as_i16, capnp_as_i32u
   public :: capnp_u_of_i8, capnp_u_of_i16, capnp_u_of_i32
   public :: capnp_u64_to_f64, capnp_f64_to_u64, capnp_f64_to_i64
   public :: capnp_quiet_nan
   public :: capnp_dec_digit, capnp_oct_digit, capnp_hex_digit
   public :: capnp_codec_find_id, capnp_codec_find_field, capnp_codec_field_present
   public :: capnp_codec_new_struct, capnp_codec_alloc_list, capnp_codec_elem_esize
   public :: capnp_list_fits
   public :: capnp_def_bool, capnp_def_i8, capnp_def_i16, capnp_def_i32, capnp_def_i64
   public :: capnp_def_u8, capnp_def_u16, capnp_def_u32, capnp_def_f32, capnp_def_f64
   public :: capnp_put_signed, capnp_put_unsigned
   public :: capnp_brand_env_t, capnp_brand_enter, capnp_brand_resolve

   type :: brand_param_t
      logical :: bound = .false.
      type(capnp_ptr_t) :: typ
   end type brand_param_t

   type :: brand_scope_t
      integer(int64) :: id = 0_int64
      type(brand_param_t), allocatable :: params(:)
   end type brand_scope_t

   type :: capnp_brand_env_t
      type(brand_scope_t), allocatable :: scopes(:)
   end type capnp_brand_env_t

   integer, parameter :: FLT_BAD = 0, FLT_DEC = 1, FLT_HEX = 2, FLT_INF = 3, FLT_NAN = 4

   !> IEEE binary64 and binary32 layouts for the hex and NaN paths.
   type :: float_fmt_t
      integer :: prec, emin, emax, mbits
   end type float_fmt_t
   type(float_fmt_t), parameter :: FMT64 = float_fmt_t(53, -1022, 1023, 52)
   type(float_fmt_t), parameter :: FMT32 = float_fmt_t(24, -126, 127, 23)

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
      ! glibc strtof sets ERANGE for every subnormal, so the C++ printer
      ! rejects the six-digit form and reprints with eight digits. A libc
      ! that does not set ERANGE can keep the shorter text.
      if (err /= CAPNP_OK .or. .not. f32_same(y, x) .or. f32_subnormal(x)) &
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

   !> The whole token as C-locale strtod reads it, which is what KJ's
   !> NoLocaleStrtod gives: the radix is '.' whatever the process locale.
   !> A decimal goes through Fortran list input, which never follows the
   !> C locale and rounds as the C library does.
   subroutine capnp_parse_f64(tok, x, err)
      character(len=*), intent(in) :: tok
      real(real64), intent(out) :: x
      integer, intent(out) :: err
      integer :: form, a, c, ios
      logical :: neg
      integer(int64) :: bits
      err = CAPNP_OK
      x = 0.0_real64
      call scan_c_float(tok, form, neg, a, c)
      select case (form)
      case (FLT_DEC)
         read (tok(a:), *, iostat=ios) x
         if (ios /= 0) then
            x = 0.0_real64
            err = CAPNP_ERR_ARG
         end if
         return
      case (FLT_HEX)
         bits = hex_float_bits(tok(c:), FMT64)
      case (FLT_INF)
         bits = inf_bits(FMT64)
      case (FLT_NAN)
         bits = nan_bits(tok(c:), FMT64)
      case default
         err = CAPNP_ERR_ARG
         return
      end select
      if (neg) bits = ibset(bits, 63)
      x = transfer(bits, x)
   end subroutine capnp_parse_f64

   !> capnp_parse_f64 rounded once to binary32, as strtof does.
   subroutine capnp_parse_f32(tok, x, err)
      character(len=*), intent(in) :: tok
      real(real32), intent(out) :: x
      integer, intent(out) :: err
      integer :: form, a, c, ios
      logical :: neg
      integer(int64) :: bits
      integer(int32) :: b32
      err = CAPNP_OK
      x = 0.0_real32
      call scan_c_float(tok, form, neg, a, c)
      select case (form)
      case (FLT_DEC)
         read (tok(a:), *, iostat=ios) x
         if (ios /= 0) then
            x = 0.0_real32
            err = CAPNP_ERR_ARG
         end if
         return
      case (FLT_HEX)
         bits = hex_float_bits(tok(c:), FMT32)
      case (FLT_INF)
         bits = inf_bits(FMT32)
      case (FLT_NAN)
         bits = nan_bits(tok(c:), FMT32)
      case default
         err = CAPNP_ERR_ARG
         return
      end select
      b32 = int(bits, int32)
      if (neg) b32 = ibset(b32, 31)
      x = transfer(b32, x)
   end subroutine capnp_parse_f32

   !> Classify tok by the C-locale strtod grammar. a is the first byte
   !> after leading C whitespace, c the first byte after the sign.
   pure subroutine scan_c_float(tok, form, neg, a, c)
      character(len=*), intent(in) :: tok
      integer, intent(out) :: form, a, c
      logical, intent(out) :: neg
      integer :: i, n
      form = FLT_BAD
      neg = .false.
      n = len(tok)
      i = 1
      do while (i <= n)
         if (index(' '//achar(9)//achar(10)//achar(11)//achar(12)//achar(13), tok(i:i)) == 0) exit
         i = i + 1
      end do
      a = i
      c = i
      if (i > n) return
      if (tok(i:i) == '+' .or. tok(i:i) == '-') then
         neg = tok(i:i) == '-'
         i = i + 1
      end if
      c = i
      if (i > n) return
      if (ascii_ieq(tok(i:), 'inf') .or. ascii_ieq(tok(i:), 'infinity')) then
         form = FLT_INF
      else if (n - i >= 2) then
         if (ascii_ieq(tok(i:i + 2), 'nan')) then
            if (nan_tail_ok(tok(i + 3:))) form = FLT_NAN
            return
         end if
      end if
      if (form /= FLT_BAD) return
      if (n - i >= 1) then
         if (tok(i:i) == '0') then
            ! One character remains when i == n. The next index is past the token.
            if (i < n) then
               if (tok(i + 1:i + 1) == 'x' .or. tok(i + 1:i + 1) == 'X') then
                  if (digits_ok(tok(i + 2:), .true.)) form = FLT_HEX
                  return
               end if
            end if
         end if
      end if
      if (digits_ok(tok(i:), .false.)) form = FLT_DEC
   end subroutine scan_c_float

   !> Empty, or "(" n-char-sequence ")" with the sequence in [0-9A-Za-z_].
   pure function nan_tail_ok(s) result(ok)
      character(len=*), intent(in) :: s
      logical :: ok
      integer :: i
      ok = len(s) == 0
      if (ok) return
      if (len(s) < 2) return
      if (s(1:1) /= '(' .or. s(len(s):len(s)) /= ')') return
      do i = 2, len(s) - 1
         if (.not. nchar(s(i:i))) return
      end do
      ok = .true.
   end function nan_tail_ok

   pure function nchar(ch) result(y)
      character(len=1), intent(in) :: ch
      logical :: y
      y = (ch >= '0' .and. ch <= '9') .or. (ch >= 'a' .and. ch <= 'z') .or. &
          (ch >= 'A' .and. ch <= 'Z') .or. ch == '_'
   end function nchar

   !> One or more digits with at most one '.', then an optional exponent
   !> with at least one decimal digit: e for decimal, p for hex.
   pure function digits_ok(s, hex) result(ok)
      character(len=*), intent(in) :: s
      logical, intent(in) :: hex
      logical :: ok
      integer :: i, n, nd
      logical :: dot
      ok = .false.
      n = len(s)
      i = 1
      nd = 0
      dot = .false.
      do while (i <= n)
         if (s(i:i) == '.') then
            if (dot) return
            dot = .true.
         else if (hex .and. capnp_hex_digit(s(i:i)) >= 0) then
            nd = nd + 1
         else if (.not. hex .and. capnp_dec_digit(s(i:i)) >= 0) then
            nd = nd + 1
         else
            exit
         end if
         i = i + 1
      end do
      if (nd == 0) return
      if (i > n) then
         ok = .true.
         return
      end if
      if (hex) then
         if (s(i:i) /= 'p' .and. s(i:i) /= 'P') return
      else
         if (s(i:i) /= 'e' .and. s(i:i) /= 'E') return
      end if
      i = i + 1
      if (i > n) return
      if (s(i:i) == '+' .or. s(i:i) == '-') i = i + 1
      if (i > n) return
      do while (i <= n)
         if (capnp_dec_digit(s(i:i)) < 0) return
         i = i + 1
      end do
      ok = .true.
   end function digits_ok

   pure function ascii_ieq(s, lower) result(y)
      character(len=*), intent(in) :: s, lower
      logical :: y
      integer :: i, c
      y = .false.
      if (len(s) /= len(lower)) return
      do i = 1, len(s)
         c = iachar(s(i:i))
         if (c >= iachar('A') .and. c <= iachar('Z')) c = c + 32
         if (c /= iachar(lower(i:i))) return
      end do
      y = .true.
   end function ascii_ieq

   pure function inf_bits(f) result(bits)
      type(float_fmt_t), intent(in) :: f
      integer(int64) :: bits
      bits = shiftl(int(2*f%emax + 1, int64), f%mbits)
   end function inf_bits

   !> A quiet NaN. glibc puts the number in "nan(n)" into the low mantissa
   !> bits when the whole sequence reads as one strtoull base-0 integer.
   pure function nan_bits(s, f) result(bits)
      character(len=*), intent(in) :: s
      type(float_fmt_t), intent(in) :: f
      integer(int64) :: bits, hi, lo, mant
      integer :: i, n, base, d
      bits = ibset(inf_bits(f), f%mbits - 1)
      n = len(s) - 1
      if (n < 4) return
      i = 5
      base = 10
      if (s(i:i) == '0') then
         base = 8
         if (i + 1 <= n) then
            if (s(i + 1:i + 1) == 'x' .or. s(i + 1:i + 1) == 'X') then
               if (i + 2 > n) return
               if (capnp_hex_digit(s(i + 2:i + 2)) < 0) return
               base = 16
               i = i + 2
            end if
         end if
      end if
      hi = 0_int64
      lo = 0_int64
      do while (i <= n)
         d = capnp_hex_digit(s(i:i))
         if (d < 0 .or. d >= base) return
         lo = lo*base + d
         hi = hi*base + shiftr(lo, 32)
         lo = iand(lo, 4294967295_int64)
         if (hi > 4294967295_int64) hi = 2_int64**40
         i = i + 1
      end do
      if (hi > 4294967295_int64) then
         mant = -1_int64
      else
         mant = ior(shiftl(hi, 32), lo)
      end if
      bits = ior(bits, iand(mant, shiftl(1_int64, f%mbits - 1) - 1_int64))
   end function nan_bits

   !> Magnitude bits of a hex float "0x..." already checked by digits_ok,
   !> rounded to nearest even. The first 15 significant hex digits are
   !> kept; the rest only decide a tie.
   pure function hex_float_bits(s, f) result(bits)
      character(len=*), intent(in) :: s
      type(float_fmt_t), intent(in) :: f
      integer(int64) :: bits, mant, e2, pexp, shift, m, rem, half, eres, q
      integer :: i, n, d, nd, b
      logical :: frac, sticky, eneg
      n = len(s)
      mant = 0_int64
      e2 = 0_int64
      nd = 0
      frac = .false.
      sticky = .false.
      i = 3
      do while (i <= n)
         if (s(i:i) == '.') then
            frac = .true.
            i = i + 1
            cycle
         end if
         d = capnp_hex_digit(s(i:i))
         if (d < 0) exit
         if (nd == 0 .and. d == 0) then
            if (frac) e2 = e2 - 4_int64
         else if (nd < 15) then
            mant = mant*16_int64 + int(d, int64)
            nd = nd + 1
            if (frac) e2 = e2 - 4_int64
         else
            if (d /= 0) sticky = .true.
            if (.not. frac) e2 = e2 + 4_int64
         end if
         i = i + 1
      end do
      pexp = 0_int64
      eneg = .false.
      if (i <= n) then
         i = i + 1
         if (s(i:i) == '+' .or. s(i:i) == '-') then
            eneg = s(i:i) == '-'
            i = i + 1
         end if
         do while (i <= n)
            if (pexp < 1000000000000_int64) pexp = pexp*10_int64 + int(capnp_dec_digit(s(i:i)), int64)
            i = i + 1
         end do
         if (eneg) pexp = -pexp
      end if
      bits = 0_int64
      if (mant == 0_int64) return
      e2 = e2 + pexp
      b = 63 - leadz(mant)
      if (b + e2 > f%emax) then
         bits = inf_bits(f)
         return
      end if
      q = int(f%emin - (f%prec - 1), int64)
      shift = max(int(b - (f%prec - 1), int64), q - e2)
      if (shift <= 0_int64) then
         m = shiftl(mant, int(-shift))
      else if (shift > 62_int64) then
         return
      else
         m = shiftr(mant, int(shift))
         rem = mant - shiftl(m, int(shift))
         half = shiftl(1_int64, int(shift) - 1)
         if (rem > half .or. (rem == half .and. (sticky .or. btest(m, 0)))) m = m + 1_int64
      end if
      eres = e2 + shift
      if (m == shiftl(1_int64, f%prec)) then
         m = shiftr(m, 1)
         eres = eres + 1_int64
      end if
      if (m < shiftl(1_int64, f%prec - 1)) then
         bits = m
         return
      end if
      if (eres + f%prec - 1 > f%emax) then
         bits = inf_bits(f)
         return
      end if
      bits = ior(shiftl(eres + int(f%prec - 1 + f%emax, int64), f%mbits), &
                 m - shiftl(1_int64, f%prec - 1))
   end function hex_float_bits

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
      call parse_u64_body(body, v, oct, err)
      if (err /= CAPNP_OK) return
      if (neg .and. v /= 0_int64) err = CAPNP_ERR_ARG
   end subroutine capnp_parse_u64

   subroutine capnp_cescape_text(str, out, err)
      character(len=*), intent(in) :: str
      character(len=:), allocatable, intent(out) :: out
      integer, intent(out), optional :: err
      character(len=:), allocatable :: buf
      integer :: i, m, stat, local
      local = CAPNP_OK
      out = ''
      if (len(str) == 0) then
         if (present(err)) err = local
         return
      end if
      if (int(len(str), int64)*4_int64 > int(huge(m), int64)) then
         local = CAPNP_ERR_ALLOC
         if (present(err)) err = local
         return
      end if
      allocate (character(len=len(str)*4) :: buf, stat=stat)
      if (stat /= 0) then
         local = CAPNP_ERR_ALLOC
         if (present(err)) err = local
         return
      end if
      m = 0
      do i = 1, len(str)
         call escape_byte(iachar(str(i:i)), .false., buf, m)
      end do
      if (m == 0) then
         out = ''
      else
         out = buf(1:m)
      end if
      if (present(err)) err = local
   end subroutine capnp_cescape_text

   subroutine capnp_cescape_data(bytes, out, err)
      integer(int8), intent(in) :: bytes(:)
      character(len=:), allocatable, intent(out) :: out
      integer, intent(out), optional :: err
      character(len=:), allocatable :: buf
      integer :: i, n, m, lb, stat, local
      local = CAPNP_OK
      out = ''
      n = size(bytes)
      if (n == 0) then
         if (present(err)) err = local
         return
      end if
      if (int(n, int64)*4_int64 > int(huge(m), int64)) then
         local = CAPNP_ERR_ALLOC
         if (present(err)) err = local
         return
      end if
      allocate (character(len=n*4) :: buf, stat=stat)
      if (stat /= 0) then
         local = CAPNP_ERR_ALLOC
         if (present(err)) err = local
         return
      end if
      m = 0
      lb = lbound(bytes, 1)
      do i = 0, n - 1
         call escape_byte(capnp_u_of_i8(bytes(lb + i)), .true., buf, m)
      end do
      if (m == 0) then
         out = ''
      else
         out = buf(1:m)
      end if
      if (present(err)) err = local
   end subroutine capnp_cescape_data

   !> Decode C-style escapes (1-3 octal digits and \xHH) into a byte string.
   subroutine capnp_cunescape(src, out, err)
      character(len=*), intent(in) :: src
      character(len=:), allocatable, intent(out) :: out
      integer, intent(out) :: err
      character(len=:), allocatable :: buf
      integer :: i, n, m, d, d2, noct, v, stat
      err = CAPNP_OK
      n = len(src)
      if (n == 0) then
         out = ''
         return
      end if
      allocate (character(len=n) :: buf, stat=stat)
      if (stat /= 0) then
         err = CAPNP_ERR_ALLOC
         out = ''
         return
      end if
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
               d = capnp_oct_digit(src(i:i))
               if (d < 0) exit
               v = v*8 + d
               i = i + 1
               noct = noct + 1
            end do
            v = iand(v, 255)
            m = m + 1
            buf(m:m) = achar(v)
            cycle
         case ('x')
            if (i + 2 > n) then
               err = CAPNP_ERR_ARG
               out = ''
               return
            end if
            d = capnp_hex_digit(src(i + 1:i + 1))
            d2 = capnp_hex_digit(src(i + 2:i + 2))
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
         if (len(nm) == len(name)) then
            if (nm == name) return
         end if
      end do
      err = CAPNP_ERR_ARG
   end subroutine capnp_codec_find_field

   function capnp_codec_field_present(f, p, err) result(y)
      type(capnp_ptr_t), intent(in) :: f, p
      integer, intent(out) :: err
      logical :: y
      type(capnp_ptr_t) :: t, q
      integer :: tw
      err = CAPNP_OK
      y = .false.
      if (field_which(f) == FIELD_GROUP) then
         y = .true.
         return
      end if
      t = field_slot_type(f, err)
      if (err /= CAPNP_OK) return
      tw = type_which(t)
      select case (tw)
      case (TYPE_TEXT, TYPE_DATA, TYPE_LIST, TYPE_STRUCT, TYPE_INTERFACE, TYPE_ANY_POINTER)
         if (p%kind == CAPNP_PK_NULL) then
            y = .false.
         else
            q = capnp_getp(p, int(field_slot_offset(f)), err)
            y = err == CAPNP_OK .and. q%kind /= CAPNP_PK_NULL
         end if
      case default
         y = .true.
      end select
   end function capnp_codec_field_present

   function capnp_codec_new_struct(p, dw, pw, err) result(q)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(in) :: dw, pw
      integer, intent(out) :: err
      type(capnp_ptr_t) :: q
      type(capnp_message_t), pointer :: m
      m => p%msg
      q = capnp_new_struct(m, dw, pw, err)
   end function capnp_codec_new_struct

   function capnp_codec_alloc_list(parent, et, n, schema, err, env) result(lst)
      type(capnp_ptr_t), intent(in) :: parent, et
      integer, intent(in) :: n
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(out) :: err
      type(capnp_brand_env_t), intent(in), optional :: env
      type(capnp_ptr_t) :: lst, rt
      type(capnp_message_t), pointer :: m
      integer :: tw, idx, dw, pw, es
      m => parent%msg
      err = CAPNP_OK
      rt = et
      if (present(env)) rt = capnp_brand_resolve(env, et)
      tw = type_which(rt)
      if (tw == TYPE_STRUCT) then
         idx = capnp_codec_find_id(schema, type_type_id(rt))
         if (idx == 0) then
            err = CAPNP_ERR_ARG
            return
         end if
         dw = node_struct_data_words(schema%nodes(idx))
         pw = node_struct_pointer_count(schema%nodes(idx))
         lst = capnp_new_composite_list(m, int(n, int64), dw, pw, err)
      else
         es = capnp_codec_elem_esize(tw)
         if (es < 0) then
            err = CAPNP_ERR_KIND
            return
         end if
         lst = capnp_new_list(m, es, int(n, int64), err)
      end if
   end function capnp_codec_alloc_list

   pure function capnp_codec_elem_esize(tw) result(es)
      integer, intent(in) :: tw
      integer :: es
      select case (tw)
      case (TYPE_VOID); es = CAPNP_SZ_VOID
      case (TYPE_BOOL); es = CAPNP_SZ_BIT
      case (TYPE_INT8, TYPE_UINT8); es = CAPNP_SZ_BYTE
      case (TYPE_INT16, TYPE_UINT16, TYPE_ENUM); es = CAPNP_SZ_TWO
      case (TYPE_INT32, TYPE_UINT32, TYPE_FLOAT32); es = CAPNP_SZ_FOUR
      case (TYPE_INT64, TYPE_UINT64, TYPE_FLOAT64); es = CAPNP_SZ_EIGHT
      case (TYPE_TEXT, TYPE_DATA, TYPE_LIST, TYPE_ANY_POINTER, TYPE_INTERFACE)
         es = CAPNP_SZ_PTR
      case default; es = -1
      end select
   end function capnp_codec_elem_esize

   !> True when a wire list can be read as expect (a CAPNP_SZ_* code).
   !> Composite is the code for a struct list. Matches the reader checks
   !> in layout.c++: a data element may be wider, a pointer list needs a
   !> pointer, and a bit list upgrades to nothing else.
   pure function capnp_list_fits(p, expect) result(ok)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(in) :: expect
      logical :: ok
      integer :: wb, eb, wp, ep
      ok = .false.
      if (p%kind /= CAPNP_PK_LIST) return
      if (p%esize == CAPNP_SZ_COMPOSITE) then
         select case (expect)
         case (CAPNP_SZ_VOID, CAPNP_SZ_COMPOSITE)
            ok = .true.
         case (CAPNP_SZ_BYTE, CAPNP_SZ_TWO, CAPNP_SZ_FOUR, CAPNP_SZ_EIGHT)
            ok = p%dwords >= 1
         case (CAPNP_SZ_PTR)
            ok = p%pwords >= 1
         end select
         return
      end if
      if (p%esize == CAPNP_SZ_BIT .and. expect /= CAPNP_SZ_BIT) return
      wb = fit_bits(p%esize)
      eb = fit_bits(expect)
      wp = 0
      if (p%esize == CAPNP_SZ_PTR) wp = 1
      ep = 0
      if (expect == CAPNP_SZ_PTR) ep = 1
      ok = eb <= wb .and. ep <= wp
   end function capnp_list_fits

   pure function fit_bits(esize) result(bits)
      integer, intent(in) :: esize
      integer :: bits
      select case (esize)
      case (CAPNP_SZ_BIT); bits = 1
      case (CAPNP_SZ_BYTE); bits = 8
      case (CAPNP_SZ_TWO); bits = 16
      case (CAPNP_SZ_FOUR); bits = 32
      case (CAPNP_SZ_EIGHT); bits = 64
      case default; bits = 0
      end select
   end function fit_bits

   function capnp_brand_resolve(env, t) result(rt)
      type(capnp_brand_env_t), intent(in) :: env
      type(capnp_ptr_t), intent(in) :: t
      type(capnp_ptr_t) :: rt
      integer(int64) :: sid
      integer :: pidx, i, step
      logical :: hit
      rt = t
      if (.not. allocated(env%scopes)) return
      do step = 1, 16
         if (type_which(rt) /= TYPE_ANY_POINTER) return
         if (type_anyptr_which(rt) /= ANYPTR_PARAMETER) return
         sid = type_param_scope_id(rt)
         pidx = type_param_index(rt) + 1
         hit = .false.
         do i = 1, size(env%scopes)
            if (env%scopes(i)%id /= sid) cycle
            if (allocated(env%scopes(i)%params)) then
               if (pidx >= 1 .and. pidx <= size(env%scopes(i)%params)) then
                  if (env%scopes(i)%params(pidx)%bound) then
                     rt = env%scopes(i)%params(pidx)%typ
                     hit = .true.
                  end if
               end if
            end if
            exit
         end do
         if (.not. hit) return
      end do
   end function capnp_brand_resolve

   subroutine capnp_brand_enter(parent_env, t, child_env, err)
      type(capnp_brand_env_t), intent(in) :: parent_env
      type(capnp_ptr_t), intent(in) :: t
      type(capnp_brand_env_t), intent(out) :: child_env
      integer, intent(out) :: err
      type(capnp_dyn_brand_t) :: b
      type(brand_scope_t) :: entry
      type(brand_scope_t), allocatable :: grown(:)
      integer :: i, j, k, nb, nsc, pos
      err = CAPNP_OK
      if (allocated(parent_env%scopes)) then
         nsc = size(parent_env%scopes)
         allocate (child_env%scopes(nsc))
         child_env%scopes = parent_env%scopes
      else
         allocate (child_env%scopes(0))
      end if
      call capnp_dyn_load_brand(t, b, err)
      if (err /= CAPNP_OK) return
      if (.not. allocated(b%scopes)) return
      do i = 1, size(b%scopes)
         if (b%scopes(i)%inherited) cycle
         entry%id = b%scopes(i)%id
         if (allocated(entry%params)) deallocate (entry%params)
         if (allocated(b%scopes(i)%bindings)) then
            nb = size(b%scopes(i)%bindings)
         else
            nb = 0
         end if
         allocate (entry%params(nb))
         do j = 1, nb
            if (b%scopes(i)%bindings(j)%unbound) then
               entry%params(j)%bound = .false.
            else
               entry%params(j)%bound = .true.
               entry%params(j)%typ = capnp_brand_resolve(parent_env, b%scopes(i)%bindings(j)%type)
            end if
         end do
         pos = 0
         do k = 1, size(child_env%scopes)
            if (child_env%scopes(k)%id == entry%id) then
               pos = k
               exit
            end if
         end do
         if (pos > 0) then
            child_env%scopes(pos) = entry
         else
            nsc = size(child_env%scopes)
            allocate (grown(nsc + 1))
            if (nsc > 0) grown(1:nsc) = child_env%scopes
            grown(nsc + 1) = entry
            call move_alloc(grown, child_env%scopes)
         end if
      end do
   end subroutine capnp_brand_enter

   function capnp_def_bool(dv) result(d)
      type(capnp_ptr_t), intent(in) :: dv
      logical :: d
      d = .false.
      if (dv%kind /= CAPNP_PK_NULL) d = value_bool(dv)
   end function capnp_def_bool

   function capnp_def_i8(dv) result(d)
      type(capnp_ptr_t), intent(in) :: dv
      integer(int8) :: d
      d = 0_int8
      if (dv%kind /= CAPNP_PK_NULL) d = value_i8(dv)
   end function capnp_def_i8

   function capnp_def_i16(dv) result(d)
      type(capnp_ptr_t), intent(in) :: dv
      integer(int16) :: d
      d = 0_int16
      if (dv%kind /= CAPNP_PK_NULL) d = value_i16(dv)
   end function capnp_def_i16

   function capnp_def_i32(dv) result(d)
      type(capnp_ptr_t), intent(in) :: dv
      integer(int32) :: d
      d = 0_int32
      if (dv%kind /= CAPNP_PK_NULL) d = value_i32(dv)
   end function capnp_def_i32

   function capnp_def_i64(dv) result(d)
      type(capnp_ptr_t), intent(in) :: dv
      integer(int64) :: d
      d = 0_int64
      if (dv%kind /= CAPNP_PK_NULL) d = value_i64(dv)
   end function capnp_def_i64

   function capnp_def_u8(dv) result(d)
      type(capnp_ptr_t), intent(in) :: dv
      integer(int16) :: d
      d = 0_int16
      if (dv%kind /= CAPNP_PK_NULL) d = value_u8(dv)
   end function capnp_def_u8

   function capnp_def_u16(dv) result(d)
      type(capnp_ptr_t), intent(in) :: dv
      integer(int32) :: d
      d = 0_int32
      if (dv%kind /= CAPNP_PK_NULL) d = value_u16(dv)
   end function capnp_def_u16

   function capnp_def_u32(dv) result(d)
      type(capnp_ptr_t), intent(in) :: dv
      integer(int64) :: d
      d = 0_int64
      if (dv%kind /= CAPNP_PK_NULL) d = value_u32(dv)
   end function capnp_def_u32

   function capnp_def_f32(dv) result(d)
      type(capnp_ptr_t), intent(in) :: dv
      real(real32) :: d
      d = 0.0_real32
      if (dv%kind /= CAPNP_PK_NULL) d = value_f32(dv)
   end function capnp_def_f32

   function capnp_def_f64(dv) result(d)
      type(capnp_ptr_t), intent(in) :: dv
      real(real64) :: d
      d = 0.0_real64
      if (dv%kind /= CAPNP_PK_NULL) d = value_f64(dv)
   end function capnp_def_f64

   subroutine capnp_put_signed(p, off, tw, iv, in_list, ix, dv, err)
      type(capnp_ptr_t), intent(in) :: p, dv
      integer(int64), intent(in) :: off, iv, ix
      integer, intent(in) :: tw
      logical, intent(in) :: in_list
      integer, intent(out) :: err
      integer(int64) :: stored
      logical :: clamped
      err = CAPNP_OK
      stored = iv
      clamped = .false.
      select case (tw)
      case (TYPE_INT8)
         if (iv > 127_int64) then
            stored = 127_int64
            clamped = .true.
         else if (iv < -128_int64) then
            stored = -128_int64
            clamped = .true.
         end if
         if (in_list) then
            call capnp_list_set_i8(p, ix, int(stored, int8), err)
         else
            call capnp_set_i8(p, off, int(stored, int8), err, default=capnp_def_i8(dv))
         end if
      case (TYPE_INT16)
         if (iv > 32767_int64) then
            stored = 32767_int64
            clamped = .true.
         else if (iv < -32768_int64) then
            stored = -32768_int64
            clamped = .true.
         end if
         if (in_list) then
            call capnp_list_set_i16(p, ix, int(stored, int16), err)
         else
            call capnp_set_i16(p, off*2_int64, int(stored, int16), err, default=capnp_def_i16(dv))
         end if
      case (TYPE_INT32)
         if (iv > 2147483647_int64) then
            stored = 2147483647_int64
            clamped = .true.
         else if (iv < -2147483648_int64) then
            stored = -2147483648_int64
            clamped = .true.
         end if
         if (in_list) then
            call capnp_list_set_i32(p, ix, int(stored, int32), err)
         else
            call capnp_set_i32(p, off*4_int64, int(stored, int32), err, default=capnp_def_i32(dv))
         end if
      case (TYPE_INT64)
         if (in_list) then
            call capnp_list_set_i64(p, ix, iv, err)
         else
            call capnp_set_i64(p, off*8_int64, iv, err, default=capnp_def_i64(dv))
         end if
      case default
         err = CAPNP_ERR_KIND
      end select
      if (err == CAPNP_OK .and. clamped) err = CAPNP_ERR_ARG
   end subroutine capnp_put_signed

   subroutine capnp_put_unsigned(p, off, tw, uv, in_list, ix, dv, err)
      type(capnp_ptr_t), intent(in) :: p, dv
      integer(int64), intent(in) :: off, uv, ix
      integer, intent(in) :: tw
      logical, intent(in) :: in_list
      integer, intent(out) :: err
      integer(int64) :: stored
      logical :: clamped
      err = CAPNP_OK
      stored = uv
      clamped = .false.
      select case (tw)
      case (TYPE_UINT8)
         if (uv < 0_int64) then
            stored = 0_int64
            clamped = .true.
         else if (uv > 255_int64) then
            stored = 255_int64
            clamped = .true.
         end if
         if (in_list) then
            call capnp_list_set_i8(p, ix, capnp_as_i8(int(stored)), err)
         else
            call capnp_set_u8(p, off, int(stored, int16), err, default=capnp_def_u8(dv))
         end if
      case (TYPE_UINT16)
         if (uv < 0_int64) then
            stored = 0_int64
            clamped = .true.
         else if (uv > 65535_int64) then
            stored = 65535_int64
            clamped = .true.
         end if
         if (in_list) then
            call capnp_list_set_i16(p, ix, capnp_as_i16(int(stored)), err)
         else
            call capnp_set_u16(p, off*2_int64, int(stored, int32), err, default=capnp_def_u16(dv))
         end if
      case (TYPE_UINT32)
         if (uv < 0_int64) then
            stored = 0_int64
            clamped = .true.
         else if (uv > 4294967295_int64) then
            stored = 4294967295_int64
            clamped = .true.
         end if
         if (in_list) then
            call capnp_list_set_i32(p, ix, capnp_as_i32u(stored), err)
         else
            call capnp_set_u32(p, off*4_int64, stored, err, default=capnp_def_u32(dv))
         end if
      case (TYPE_UINT64)
         if (in_list) then
            call capnp_list_set_i64(p, ix, uv, err)
         else
            call capnp_set_i64(p, off*8_int64, uv, err, default=capnp_def_i64(dv))
         end if
      case default
         err = CAPNP_ERR_KIND
      end select
      if (err == CAPNP_OK .and. clamped) err = CAPNP_ERR_ARG
   end subroutine capnp_put_unsigned

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

   pure function capnp_u64_to_f64(bits) result(x)
      integer(int64), intent(in) :: bits
      real(real64) :: x
      integer(int64) :: half
      if (iand(bits, int(z'8000000000000000', int64)) == 0_int64) then
         x = real(bits, real64)
      else
         half = ior(iand(shiftr(bits, 1), int(z'7fffffffffffffff', int64)), iand(bits, 1_int64))
         x = real(half, real64)*2.0_real64
      end if
   end function capnp_u64_to_f64

   subroutine capnp_f64_to_u64(x, uv, err)
      real(real64), intent(in) :: x
      integer(int64), intent(out) :: uv
      integer, intent(out) :: err
      real(real64), parameter :: two63 = 9223372036854775808.0_real64
      real(real64), parameter :: max_u64_f64 = 18446744073709549568.0_real64
      err = CAPNP_OK
      uv = 0_int64
      if (.not. ieee_is_finite(x)) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (x < 0.0_real64 .or. x > max_u64_f64 .or. abs(x - aint(x)) > 0.0_real64) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (x < two63) then
         uv = int(x, int64)
      else
         uv = ieor(int(x - two63, int64), int(z'8000000000000000', int64))
      end if
   end subroutine capnp_f64_to_u64

   subroutine capnp_f64_to_i64(x, iv, err)
      real(real64), intent(in) :: x
      integer(int64), intent(out) :: iv
      integer, intent(out) :: err
      real(real64), parameter :: min_i64_f64 = -9223372036854775808.0_real64
      real(real64), parameter :: max_i64_f64 = 9223372036854774784.0_real64
      err = CAPNP_OK
      iv = 0_int64
      if (.not. ieee_is_finite(x)) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (x < min_i64_f64 .or. x > max_i64_f64 .or. abs(x - aint(x)) > 0.0_real64) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (.not. (x > min_i64_f64)) then
         iv = int(z'8000000000000000', int64)
      else
         iv = int(x, int64)
      end if
   end subroutine capnp_f64_to_i64

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
      if (iand(transfer(val, 0_int64), int(z'7fffffffffffffff', int64)) == 0_int64) then
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
      do while (m > 1)
         if (digits(m:m) /= '0') exit
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

   pure function f32_subnormal(x) result(y)
      real(real32), intent(in) :: x
      logical :: y
      integer(int32) :: bits
      bits = iand(transfer(x, 0_int32), int(z'7fffffff', int32))
      y = bits > 0_int32 .and. bits < int(z'00800000', int32)
   end function f32_subnormal

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
      ! Text integers wrap the magnitude mod 2^64, then the target type
      ! range-checks the result. A value past 2^64-1 keeps the low 64 bits.
      m = iand(hi*mul + shiftr(t, 32), 4294967295_int64)
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
         case (16); d = capnp_hex_digit(body(i:i))
         case (8); d = capnp_oct_digit(body(i:i))
         case default; d = capnp_dec_digit(body(i:i))
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

   pure function capnp_dec_digit(c) result(d)
      character(len=1), intent(in) :: c
      integer :: d
      if (c >= '0' .and. c <= '9') then
         d = iachar(c) - iachar('0')
      else
         d = -1
      end if
   end function capnp_dec_digit

   pure function capnp_oct_digit(c) result(d)
      character(len=1), intent(in) :: c
      integer :: d
      if (c >= '0' .and. c <= '7') then
         d = iachar(c) - iachar('0')
      else
         d = -1
      end if
   end function capnp_oct_digit

   pure function capnp_hex_digit(c) result(d)
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
   end function capnp_hex_digit

end module capnp_codec
