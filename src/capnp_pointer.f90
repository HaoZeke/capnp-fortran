!> Encode and decode 64-bit wire pointer words. Pure bit manipulation only;
!> no segment or arena knowledge lives here.
!>
!> Layouts (bit 0 = least significant):
!>   struct: kind(0-1)=0 | offset(2-31, signed) | dwords(32-47) | pwords(48-63)
!>   list:   kind(0-1)=1 | offset(2-31, signed) | esize(32-34)  | count(35-63)
!>   far:    kind(0-1)=2 | two(2) | pad word offset(3-31) | segment id(32-63)
!>   cap:    kind(0-1)=3 | zero(2-31) | capability index(32-63)
module capnp_pointer
   use capnp_kinds, only: int32, int64
   implicit none
   private

   public :: wp_kind, wp_offset, wp_struct_dwords, wp_struct_pwords
   public :: wp_list_esize, wp_list_count
   public :: wp_far_two, wp_far_off, wp_far_seg
   public :: wp_cap_index
   public :: wp_make_struct, wp_make_list, wp_make_far, wp_make_cap

contains

   pure function wp_kind(w) result(k)
      integer(int64), intent(in) :: w
      integer :: k
      k = int(ibits(w, 0, 2))
   end function wp_kind

   !> Signed 30-bit word offset shared by struct and list pointers.
   pure function wp_offset(w) result(off)
      integer(int64), intent(in) :: w
      integer(int32) :: off
      integer(int64) :: u
      u = ibits(w, 2, 30)
      if (u >= 536870912_int64) u = u - 1073741824_int64
      off = int(u, int32)
   end function wp_offset

   pure function wp_struct_dwords(w) result(c)
      integer(int64), intent(in) :: w
      integer(int32) :: c
      c = int(ibits(w, 32, 16), int32)
   end function wp_struct_dwords

   pure function wp_struct_pwords(w) result(d)
      integer(int64), intent(in) :: w
      integer(int32) :: d
      d = int(ibits(w, 48, 16), int32)
   end function wp_struct_pwords

   pure function wp_list_esize(w) result(c)
      integer(int64), intent(in) :: w
      integer :: c
      c = int(ibits(w, 32, 3))
   end function wp_list_esize

   !> Element count, or content word count (tag excluded) for composite lists.
   pure function wp_list_count(w) result(n)
      integer(int64), intent(in) :: w
      integer(int64) :: n
      n = ibits(w, 35, 29)
   end function wp_list_count

   pure function wp_far_two(w) result(two)
      integer(int64), intent(in) :: w
      logical :: two
      two = ibits(w, 2, 1) /= 0_int64
   end function wp_far_two

   !> Word offset of the landing pad within the target segment (unsigned).
   pure function wp_far_off(w) result(off)
      integer(int64), intent(in) :: w
      integer(int64) :: off
      off = ibits(w, 3, 29)
   end function wp_far_off

   pure function wp_far_seg(w) result(seg)
      integer(int64), intent(in) :: w
      integer(int64) :: seg
      seg = ibits(w, 32, 32)
   end function wp_far_seg

   pure function wp_cap_index(w) result(idx)
      integer(int64), intent(in) :: w
      integer(int64) :: idx
      idx = ibits(w, 32, 32)
   end function wp_cap_index

   pure function wp_make_struct(off, dwords, pwords) result(w)
      integer(int32), intent(in) :: off, dwords, pwords
      integer(int64) :: w
      w = shiftl(iand(int(off, int64), 1073741823_int64), 2)
      w = ior(w, shiftl(iand(int(dwords, int64), 65535_int64), 32))
      w = ior(w, shiftl(iand(int(pwords, int64), 65535_int64), 48))
   end function wp_make_struct

   pure function wp_make_list(off, esize, count) result(w)
      integer(int32), intent(in) :: off, esize
      integer(int64), intent(in) :: count
      integer(int64) :: w
      w = ior(1_int64, shiftl(iand(int(off, int64), 1073741823_int64), 2))
      w = ior(w, shiftl(iand(int(esize, int64), 7_int64), 32))
      w = ior(w, shiftl(iand(count, 536870911_int64), 35))
   end function wp_make_list

   pure function wp_make_far(two, off, seg) result(w)
      logical, intent(in) :: two
      integer(int64), intent(in) :: off, seg
      integer(int64) :: w
      w = 2_int64
      if (two) w = ior(w, 4_int64)
      w = ior(w, shiftl(iand(off, 536870911_int64), 3))
      w = ior(w, shiftl(iand(seg, 4294967295_int64), 32))
   end function wp_make_far

   pure function wp_make_cap(idx) result(w)
      integer(int64), intent(in) :: idx
      integer(int64) :: w
      w = ior(3_int64, shiftl(iand(idx, 4294967295_int64), 32))
   end function wp_make_cap

end module capnp_pointer
