!> Stream framing: the segment table followed by segment contents, per
!> https://capnproto.org/encoding.html#serialization-over-a-stream
!>
!>   u32          segment count - 1
!>   u32 x count  segment sizes in words
!>   (u32 pad to an 8-byte boundary when count is even)
!>   segments, concatenated
module capnp_serialize
   use capnp_kinds
   use capnp_endian, only: cp_get_i32, cp_put_i32
   use capnp_arena, only: capnp_message_t, capnp_segment_t
   implicit none
   private

   public :: capnp_serialize_bytes, capnp_deserialize_bytes
   public :: capnp_write_file, capnp_read_file

contains

   !> Frame a message into a flat byte buffer.
   subroutine capnp_serialize_bytes(msg, bytes, err)
      type(capnp_message_t), intent(in) :: msg
      integer(int8), allocatable, intent(out) :: bytes(:)
      integer, intent(out) :: err
      integer(int64) :: header_bytes, total, pos, n
      integer :: i
      err = CAPNP_OK
      if (msg%nsegs < 1) then
         err = CAPNP_ERR_ARG
         return
      end if
      header_bytes = (int(1 + msg%nsegs, int64)*4_int64 + 7_int64)/8_int64*8_int64
      total = header_bytes
      do i = 1, msg%nsegs
         total = total + msg%segs(i)%len
      end do
      allocate (bytes(0:total - 1))
      bytes = 0_int8
      call cp_put_i32(bytes, 0_int64, u32_wire(int(msg%nsegs - 1, int64)))
      do i = 1, msg%nsegs
         call cp_put_i32(bytes, int(i, int64)*4_int64, &
                         u32_wire(msg%segs(i)%len/CAPNP_WORD_BYTES))
      end do
      pos = header_bytes
      do i = 1, msg%nsegs
         n = msg%segs(i)%len
         if (n > 0_int64) bytes(pos:pos + n - 1) = msg%segs(i)%bytes(0:n - 1)
         pos = pos + n
      end do
   end subroutine capnp_serialize_bytes

   !> Parse a framed byte buffer into a reader message (segments copied).
   subroutine capnp_deserialize_bytes(bytes, msg, err, traversal_words, depth_limit)
      integer(int8), intent(in) :: bytes(0:)
      type(capnp_message_t), intent(out) :: msg
      integer, intent(out) :: err
      integer(int64), intent(in), optional :: traversal_words
      integer, intent(in), optional :: depth_limit
      integer(int64) :: nsegs64, header_bytes, pos, n
      integer :: i, nsegs
      err = CAPNP_OK
      if (present(traversal_words)) msg%traversal_words = traversal_words
      if (present(depth_limit)) msg%depth_limit = depth_limit
      if (size(bytes, kind=int64) < 8_int64) then
         err = CAPNP_ERR_FRAMING
         return
      end if
      nsegs64 = wire_u32(cp_get_i32(bytes, 0_int64)) + 1_int64
      if (nsegs64 < 1_int64 .or. nsegs64 > 512_int64) then
         err = CAPNP_ERR_FRAMING
         return
      end if
      nsegs = int(nsegs64)
      header_bytes = ((1_int64 + nsegs64)*4_int64 + 7_int64)/8_int64*8_int64
      if (size(bytes, kind=int64) < header_bytes) then
         err = CAPNP_ERR_FRAMING
         return
      end if
      msg%is_builder = .false.
      msg%nsegs = nsegs
      allocate (msg%segs(nsegs))
      pos = header_bytes
      do i = 1, nsegs
         n = wire_u32(cp_get_i32(bytes, int(i, int64)*4_int64))*CAPNP_WORD_BYTES
         if (pos + n > size(bytes, kind=int64)) then
            err = CAPNP_ERR_FRAMING
            return
         end if
         allocate (msg%segs(i)%bytes(0:max(n, 8_int64) - 1))
         msg%segs(i)%bytes = 0_int8
         if (n > 0_int64) msg%segs(i)%bytes(0:n - 1) = bytes(pos:pos + n - 1)
         msg%segs(i)%len = n
         pos = pos + n
      end do
   end subroutine capnp_deserialize_bytes

   subroutine capnp_write_file(path, bytes, err)
      character(len=*), intent(in) :: path
      integer(int8), intent(in) :: bytes(:)
      integer, intent(out) :: err
      integer :: unit, ios
      err = CAPNP_OK
      open (newunit=unit, file=path, access='stream', form='unformatted', &
            status='replace', action='write', iostat=ios)
      if (ios /= 0) then
         err = CAPNP_ERR_IO
         return
      end if
      write (unit, iostat=ios) bytes
      close (unit)
      if (ios /= 0) err = CAPNP_ERR_IO
   end subroutine capnp_write_file

   subroutine capnp_read_file(path, bytes, err)
      character(len=*), intent(in) :: path
      integer(int8), allocatable, intent(out) :: bytes(:)
      integer, intent(out) :: err
      integer :: unit, ios
      integer(int64) :: sz
      err = CAPNP_OK
      open (newunit=unit, file=path, access='stream', form='unformatted', &
            status='old', action='read', iostat=ios)
      if (ios /= 0) then
         err = CAPNP_ERR_IO
         return
      end if
      inquire (unit=unit, size=sz)
      allocate (bytes(0:max(sz, 1_int64) - 1))
      if (sz > 0_int64) read (unit, iostat=ios) bytes(0:sz - 1)
      close (unit)
      if (ios /= 0) err = CAPNP_ERR_IO
   end subroutine capnp_read_file

   !> Unsigned u32 wire value from a signed int32 container.
   pure function wire_u32(v) result(u)
      integer(int32), intent(in) :: v
      integer(int64) :: u
      u = iand(int(v, int64), 4294967295_int64)
   end function wire_u32

   !> Signed int32 container for an unsigned u32 wire value.
   pure function u32_wire(u) result(v)
      integer(int64), intent(in) :: u
      integer(int32) :: v
      integer(int64) :: x
      x = iand(u, 4294967295_int64)
      if (x > 2147483647_int64) x = x - 4294967296_int64
      v = int(x, int32)
   end function u32_wire

end module capnp_serialize
