!> Little-endian byte assembly. All wire access goes through these helpers so
!> the library never depends on host byte order or unaligned word transfers.
!> Buffers are 0-based byte arrays; offsets are byte offsets.
module capnp_endian
   use capnp_kinds, only: int8, int16, int32, int64, real32, real64
   implicit none
   private

   public :: cp_u8, cp_i8b
   public :: cp_get_i8, cp_get_i16, cp_get_i32, cp_get_i64
   public :: cp_put_i8, cp_put_i16, cp_put_i32, cp_put_i64
   public :: cp_get_f32, cp_get_f64, cp_put_f32, cp_put_f64
   public :: cp_f32_bits, cp_f64_bits, cp_bits_f32, cp_bits_f64

contains

   !> Zero-extend a storage byte to 0..255.
   elemental function cp_u8(b) result(u)
      integer(int8), intent(in) :: b
      integer(int32) :: u
      u = iand(int(b, int32), 255_int32)
   end function cp_u8

   !> Wrap 0..255 into a storage byte without overflow.
   elemental function cp_i8b(u) result(b)
      integer(int32), intent(in) :: u
      integer(int8) :: b
      integer(int32) :: v
      v = iand(u, 255_int32)
      if (v > 127_int32) v = v - 256_int32
      b = int(v, int8)
   end function cp_i8b

   pure function cp_get_i8(buf, off) result(v)
      integer(int8), intent(in) :: buf(0:)
      integer(int64), intent(in) :: off
      integer(int8) :: v
      v = buf(off)
   end function cp_get_i8

   pure function cp_get_i16(buf, off) result(v)
      integer(int8), intent(in) :: buf(0:)
      integer(int64), intent(in) :: off
      integer(int16) :: v
      integer(int32) :: u
      u = ior(cp_u8(buf(off)), shiftl(cp_u8(buf(off + 1)), 8))
      if (u > 32767_int32) u = u - 65536_int32
      v = int(u, int16)
   end function cp_get_i16

   pure function cp_get_i32(buf, off) result(v)
      integer(int8), intent(in) :: buf(0:)
      integer(int64), intent(in) :: off
      integer(int32) :: v
      integer(int64) :: u
      integer :: i
      u = 0_int64
      do i = 3, 0, -1
         u = ior(shiftl(u, 8), int(cp_u8(buf(off + i)), int64))
      end do
      if (u > 2147483647_int64) u = u - 4294967296_int64
      v = int(u, int32)
   end function cp_get_i32

   pure function cp_get_i64(buf, off) result(v)
      integer(int8), intent(in) :: buf(0:)
      integer(int64), intent(in) :: off
      integer(int64) :: v
      integer :: i
      v = 0_int64
      do i = 7, 0, -1
         v = ior(shiftl(v, 8), int(cp_u8(buf(off + i)), int64))
      end do
   end function cp_get_i64

   pure subroutine cp_put_i8(buf, off, v)
      integer(int8), intent(inout) :: buf(0:)
      integer(int64), intent(in) :: off
      integer(int8), intent(in) :: v
      buf(off) = v
   end subroutine cp_put_i8

   pure subroutine cp_put_i16(buf, off, v)
      integer(int8), intent(inout) :: buf(0:)
      integer(int64), intent(in) :: off
      integer(int16), intent(in) :: v
      integer(int32) :: u
      integer :: i
      u = iand(int(v, int32), 65535_int32)
      do i = 0, 1
         buf(off + i) = cp_i8b(iand(shiftr(u, 8*i), 255_int32))
      end do
   end subroutine cp_put_i16

   pure subroutine cp_put_i32(buf, off, v)
      integer(int8), intent(inout) :: buf(0:)
      integer(int64), intent(in) :: off
      integer(int32), intent(in) :: v
      integer :: i
      do i = 0, 3
         buf(off + i) = cp_i8b(int(iand(shiftr(int(v, int64), 8*i), 255_int64), int32))
      end do
   end subroutine cp_put_i32

   pure subroutine cp_put_i64(buf, off, v)
      integer(int8), intent(inout) :: buf(0:)
      integer(int64), intent(in) :: off
      integer(int64), intent(in) :: v
      integer :: i
      do i = 0, 7
         buf(off + i) = cp_i8b(int(iand(shiftr(v, 8*i), 255_int64), int32))
      end do
   end subroutine cp_put_i64

   pure function cp_f32_bits(x) result(b)
      real(real32), intent(in) :: x
      integer(int32) :: b
      b = transfer(x, b)
   end function cp_f32_bits

   pure function cp_bits_f32(b) result(x)
      integer(int32), intent(in) :: b
      real(real32) :: x
      x = transfer(b, x)
   end function cp_bits_f32

   pure function cp_f64_bits(x) result(b)
      real(real64), intent(in) :: x
      integer(int64) :: b
      b = transfer(x, b)
   end function cp_f64_bits

   pure function cp_bits_f64(b) result(x)
      integer(int64), intent(in) :: b
      real(real64) :: x
      x = transfer(b, x)
   end function cp_bits_f64

   pure function cp_get_f32(buf, off) result(x)
      integer(int8), intent(in) :: buf(0:)
      integer(int64), intent(in) :: off
      real(real32) :: x
      x = cp_bits_f32(cp_get_i32(buf, off))
   end function cp_get_f32

   pure function cp_get_f64(buf, off) result(x)
      integer(int8), intent(in) :: buf(0:)
      integer(int64), intent(in) :: off
      real(real64) :: x
      x = cp_bits_f64(cp_get_i64(buf, off))
   end function cp_get_f64

   pure subroutine cp_put_f32(buf, off, x)
      integer(int8), intent(inout) :: buf(0:)
      integer(int64), intent(in) :: off
      real(real32), intent(in) :: x
      call cp_put_i32(buf, off, cp_f32_bits(x))
   end subroutine cp_put_f32

   pure subroutine cp_put_f64(buf, off, x)
      integer(int8), intent(inout) :: buf(0:)
      integer(int64), intent(in) :: off
      real(real64), intent(in) :: x
      call cp_put_i64(buf, off, cp_f64_bits(x))
   end subroutine cp_put_f64

end module capnp_endian
