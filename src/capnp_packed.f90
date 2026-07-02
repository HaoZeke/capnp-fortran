!> Packed encoding, per https://capnproto.org/encoding.html#packing
!>
!> Each word gets a tag byte whose bit i marks byte i as nonzero; only the
!> nonzero bytes follow. Two escapes:
!>   tag 0x00: one count byte follows, the number of additional zero words.
!>   tag 0xff: the eight literal bytes follow, then one count byte, the
!>             number of following words copied verbatim.
module capnp_packed
   use capnp_kinds
   use capnp_endian, only: cp_u8, cp_i8b
   implicit none
   private

   public :: capnp_pack, capnp_unpack

contains

   !> Pack a word-aligned buffer. The literal-run heuristic after an 0xff tag
   !> matches the C++ encoder: include following words that contain no zero
   !> byte (capped at 255).
   subroutine capnp_pack(inb, outb, err)
      integer(int8), intent(in) :: inb(0:)
      integer(int8), allocatable, intent(out) :: outb(:)
      integer, intent(out) :: err
      integer(int8), allocatable :: buf(:)
      integer(int64) :: nwords, w, run, opos, j
      integer :: tag, nz, k
      err = CAPNP_OK
      if (mod(size(inb, kind=int64), 8_int64) /= 0_int64) then
         err = CAPNP_ERR_ARG
         return
      end if
      nwords = size(inb, kind=int64)/8_int64
      ! Worst case: tag + 8 bytes per word, plus one count byte per 0xff run.
      allocate (buf(0:10_int64*max(nwords, 1_int64) - 1))
      opos = 0_int64
      w = 0_int64
      do while (w < nwords)
         tag = 0
         nz = 0
         do k = 0, 7
            if (inb(w*8 + k) /= 0_int8) then
               tag = ibset(tag, k)
               nz = nz + 1
            end if
         end do
         buf(opos) = cp_i8b(tag)
         opos = opos + 1
         do k = 0, 7
            if (btest(tag, k)) then
               buf(opos) = inb(w*8 + k)
               opos = opos + 1
            end if
         end do
         w = w + 1
         if (tag == 0) then
            run = 0_int64
            do while (w + run < nwords .and. run < 255_int64)
               if (any(inb((w + run)*8:(w + run)*8 + 7) /= 0_int8)) exit
               run = run + 1
            end do
            buf(opos) = cp_i8b(int(run, int32))
            opos = opos + 1
            w = w + run
         else if (nz == 8) then
            run = 0_int64
            do while (w + run < nwords .and. run < 255_int64)
               if (any(inb((w + run)*8:(w + run)*8 + 7) == 0_int8)) exit
               run = run + 1
            end do
            buf(opos) = cp_i8b(int(run, int32))
            opos = opos + 1
            do j = 0_int64, run*8_int64 - 1_int64
               buf(opos + j) = inb(w*8 + j)
            end do
            opos = opos + run*8_int64
            w = w + run
         end if
      end do
      allocate (outb(0:max(opos, 1_int64) - 1))
      if (opos > 0_int64) outb(0:opos - 1) = buf(0:opos - 1)
      if (opos == 0_int64) then
         deallocate (outb)
         allocate (outb(0))
      end if
   end subroutine capnp_pack

   !> Unpack into a word-aligned buffer.
   subroutine capnp_unpack(inb, outb, err)
      integer(int8), intent(in) :: inb(0:)
      integer(int8), allocatable, intent(out) :: outb(:)
      integer, intent(out) :: err
      integer(int8), allocatable :: buf(:)
      integer(int64) :: ipos, opos, cap, n, j
      integer :: tag, k, cnt
      err = CAPNP_OK
      n = size(inb, kind=int64)
      cap = max(8_int64*n, 64_int64)
      allocate (buf(0:cap - 1))
      buf = 0_int8
      ipos = 0_int64
      opos = 0_int64
      do while (ipos < n)
         tag = cp_u8(inb(ipos))
         ipos = ipos + 1
         call ensure(buf, cap, opos + 8_int64)
         do k = 0, 7
            if (btest(tag, k)) then
               if (ipos >= n) then
                  err = CAPNP_ERR_PACKED
                  return
               end if
               buf(opos + k) = inb(ipos)
               ipos = ipos + 1
            else
               buf(opos + k) = 0_int8
            end if
         end do
         opos = opos + 8
         if (tag == 0) then
            if (ipos >= n) then
               err = CAPNP_ERR_PACKED
               return
            end if
            cnt = cp_u8(inb(ipos))
            ipos = ipos + 1
            call ensure(buf, cap, opos + int(cnt, int64)*8_int64)
            buf(opos:opos + int(cnt, int64)*8_int64 - 1) = 0_int8
            opos = opos + int(cnt, int64)*8_int64
         else if (tag == 255) then
            if (ipos >= n) then
               err = CAPNP_ERR_PACKED
               return
            end if
            cnt = cp_u8(inb(ipos))
            ipos = ipos + 1
            if (ipos + int(cnt, int64)*8_int64 > n) then
               err = CAPNP_ERR_PACKED
               return
            end if
            call ensure(buf, cap, opos + int(cnt, int64)*8_int64)
            do j = 0_int64, int(cnt, int64)*8_int64 - 1_int64
               buf(opos + j) = inb(ipos + j)
            end do
            ipos = ipos + int(cnt, int64)*8_int64
            opos = opos + int(cnt, int64)*8_int64
         end if
      end do
      allocate (outb(0:max(opos, 1_int64) - 1))
      if (opos > 0_int64) outb(0:opos - 1) = buf(0:opos - 1)
      if (opos == 0_int64) then
         deallocate (outb)
         allocate (outb(0))
      end if
   end subroutine capnp_unpack

   subroutine ensure(buf, cap, need)
      integer(int8), allocatable, intent(inout) :: buf(:)
      integer(int64), intent(inout) :: cap
      integer(int64), intent(in) :: need
      integer(int8), allocatable :: tmp(:)
      if (need <= cap) return
      do while (cap < need)
         cap = cap*2_int64
      end do
      allocate (tmp(0:cap - 1))
      tmp = 0_int8
      tmp(0:size(buf, kind=int64) - 1) = buf
      call move_alloc(tmp, buf)
   end subroutine ensure

end module capnp_packed
