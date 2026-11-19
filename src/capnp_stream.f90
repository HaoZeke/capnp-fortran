!> Convenience compositions: packed serialization and message file I/O.
module capnp_stream
   use capnp_kinds
   use capnp_arena, only: capnp_message_t, capnp_message_free
   use capnp_serialize, only: capnp_serialize_bytes, capnp_deserialize_bytes, &
                              capnp_write_file, capnp_read_file
   use capnp_packed, only: capnp_pack
   implicit none
   private

   public :: capnp_serialize_packed_bytes, capnp_deserialize_packed_bytes
   public :: capnp_write_message, capnp_read_message
   public :: capnp_write_message_packed, capnp_read_message_packed
   public :: capnp_read_message_unit, capnp_read_message_packed_unit
   public :: capnp_add_segment_words, capnp_packed_room

   !> Run-length state carried between packed words pulled off a stream.
   type :: pk_source_t
      integer :: zero_left = 0
      integer :: raw_left = 0
   end type pk_source_t

contains

   subroutine capnp_serialize_packed_bytes(msg, bytes, err)
      type(capnp_message_t), intent(in) :: msg
      integer(int8), allocatable, intent(out) :: bytes(:)
      integer, intent(out) :: err
      integer(int8), allocatable :: flat(:)
      call capnp_serialize_bytes(msg, flat, err)
      if (err /= CAPNP_OK) return
      call capnp_pack(flat, bytes, err)
   end subroutine capnp_serialize_packed_bytes

   subroutine capnp_deserialize_packed_bytes(bytes, msg, err, traversal_words, &
                                             depth_limit, options)
      use capnp_endian, only: cp_get_i32
      integer(int8), intent(in) :: bytes(0:)
      type(capnp_message_t), intent(inout) :: msg
      integer, intent(out) :: err
      integer(int64), intent(in), optional :: traversal_words
      integer, intent(in), optional :: depth_limit
      type(capnp_reader_options_t), intent(in), optional :: options
      type(pk_source_t) :: st
      integer(int8) :: w8(0:7)
      integer(int8) :: hdr(0:2055) ! 257 words; 511 segments need 256
      integer(int8), allocatable :: flat(:)
      integer(int64) :: ipos, nsegs64, header_words, total_words, nwords, limit, i, n
      integer :: ios
      err = CAPNP_OK
      call capnp_message_free(msg)
      if (size(bytes, kind=int64) == 0_int64) then
         err = CAPNP_ERR_FRAMING
         return
      end if
      limit = reader_word_limit(traversal_words, options)
      if (limit < 0_int64) then
         err = CAPNP_ERR_TRAVERSAL
         return
      end if
      ipos = 0_int64
      call unpack_word_from_bytes(bytes, ipos, st, w8, err)
      if (err /= CAPNP_OK) return
      if (run_pending(st)) then
         err = CAPNP_ERR_PACKED
         return
      end if
      nsegs64 = iand(int(cp_get_i32(w8, 0_int64), int64), 4294967295_int64) + 1_int64
      if (nsegs64 < 1_int64 .or. nsegs64 > 511_int64) then
         err = CAPNP_ERR_FRAMING
         return
      end if
      header_words = ((1_int64 + nsegs64)*4_int64 + 7_int64)/8_int64
      hdr(0:7) = w8
      do i = 2_int64, header_words
         call unpack_word_from_bytes(bytes, ipos, st, w8, err)
         if (err /= CAPNP_OK) return
         hdr((i - 1_int64)*8_int64:(i - 1_int64)*8_int64 + 7_int64) = w8
      end do
      if (run_pending(st)) then
         err = CAPNP_ERR_PACKED
         return
      end if
      total_words = 0_int64
      do i = 1_int64, nsegs64
         n = iand(int(cp_get_i32(hdr, i*4_int64), int64), 4294967295_int64)
         call capnp_add_segment_words(total_words, n, err, limit)
         if (err /= CAPNP_OK) return
      end do
      nwords = header_words + total_words
      allocate (flat(0:nwords*CAPNP_WORD_BYTES - 1), stat=ios)
      if (ios /= 0) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      flat = 0_int8
      flat(0:header_words*CAPNP_WORD_BYTES - 1) = hdr(0:header_words*CAPNP_WORD_BYTES - 1)
      do i = header_words + 1_int64, nwords
         call unpack_word_from_bytes(bytes, ipos, st, w8, err)
         if (err /= CAPNP_OK) return
         flat((i - 1_int64)*8_int64:(i - 1_int64)*8_int64 + 7_int64) = w8
      end do
      if (run_pending(st)) then
         err = CAPNP_ERR_PACKED
         return
      end if
      call capnp_deserialize_bytes(flat, msg, err, traversal_words, depth_limit, options)
   end subroutine capnp_deserialize_packed_bytes

   subroutine capnp_write_message(path, msg, err)
      character(len=*), intent(in) :: path
      type(capnp_message_t), intent(in) :: msg
      integer, intent(out) :: err
      integer(int8), allocatable :: bytes(:)
      call capnp_serialize_bytes(msg, bytes, err)
      if (err /= CAPNP_OK) return
      call capnp_write_file(path, bytes, err)
   end subroutine capnp_write_message

   subroutine capnp_read_message(path, msg, err, traversal_words, depth_limit, options)
      character(len=*), intent(in) :: path
      type(capnp_message_t), intent(inout) :: msg
      integer, intent(out) :: err
      integer(int64), intent(in), optional :: traversal_words
      integer, intent(in), optional :: depth_limit
      type(capnp_reader_options_t), intent(in), optional :: options
      integer(int8), allocatable :: bytes(:)
      integer(int64) :: limit
      call capnp_read_file(path, bytes, err)
      if (err /= CAPNP_OK) return
      if (size(bytes, kind=int64) == 0_int64) then
         err = CAPNP_ERR_FRAMING
         return
      end if
      limit = reader_word_limit(traversal_words, options)
      call check_stream_framing(bytes, limit, err)
      if (err /= CAPNP_OK) return
      call capnp_deserialize_bytes(bytes, msg, err, traversal_words, depth_limit, options)
   end subroutine capnp_read_message

   !> Segment count and total size limits of the C++ stream reader
   !> (`InputStreamMessageReader`), applied before any body is parsed.
   !> `firstWord[0] < 511` rejects a count of 512 or more. A table that
   !> is cut short is left for the deserializer to report.
   subroutine check_stream_framing(bytes, traversal_words, err)
      use capnp_endian, only: cp_get_i32
      integer(int8), intent(in) :: bytes(0:)
      integer(int64), intent(in), optional :: traversal_words
      integer, intent(out) :: err
      integer(int64) :: nsegs64, header_bytes, total, limit, i
      err = CAPNP_OK
      limit = CAPNP_DEFAULT_TRAVERSAL_WORDS
      if (present(traversal_words)) limit = traversal_words
      if (size(bytes, kind=int64) == 0_int64) then
         err = CAPNP_ERR_FRAMING
         return
      end if
      if (size(bytes, kind=int64) < 4_int64) return
      nsegs64 = iand(int(cp_get_i32(bytes, 0_int64), int64), 4294967295_int64) + 1_int64
      if (nsegs64 > 511_int64) then
         err = CAPNP_ERR_FRAMING
         return
      end if
      header_bytes = ((1_int64 + nsegs64)*4_int64 + 7_int64)/8_int64*8_int64
      if (size(bytes, kind=int64) < header_bytes) return
      total = 0_int64
      do i = 1_int64, nsegs64
         total = total + iand(int(cp_get_i32(bytes, i*4_int64), int64), 4294967295_int64)
      end do
      if (total > limit) err = CAPNP_ERR_TRAVERSAL
   end subroutine check_stream_framing

   subroutine capnp_write_message_packed(path, msg, err)
      character(len=*), intent(in) :: path
      type(capnp_message_t), intent(in) :: msg
      integer, intent(out) :: err
      integer(int8), allocatable :: bytes(:)
      call capnp_serialize_packed_bytes(msg, bytes, err)
      if (err /= CAPNP_OK) return
      call capnp_write_file(path, bytes, err)
   end subroutine capnp_write_message_packed

   subroutine capnp_read_message_packed(path, msg, err, traversal_words, depth_limit, options)
      character(len=*), intent(in) :: path
      type(capnp_message_t), intent(inout) :: msg
      integer, intent(out) :: err
      integer(int64), intent(in), optional :: traversal_words
      integer, intent(in), optional :: depth_limit
      type(capnp_reader_options_t), intent(in), optional :: options
      integer(int8), allocatable :: bytes(:)
      call capnp_read_file(path, bytes, err)
      if (err /= CAPNP_OK) return
      call capnp_deserialize_packed_bytes(bytes, msg, err, traversal_words, depth_limit, options)
   end subroutine capnp_read_message_packed

   !> Read exactly one framed message from an open stream-access unit,
   !> consuming only that message's bytes (capn_init_fp parity). Several
   !> messages written back-to-back on one stream read in sequence.
   subroutine capnp_read_message_unit(unit, msg, err, traversal_words, depth_limit, options)
      use capnp_endian, only: cp_get_i32
      integer, intent(in) :: unit
      type(capnp_message_t), intent(inout) :: msg
      integer, intent(out) :: err
      integer(int64), intent(in), optional :: traversal_words
      integer, intent(in), optional :: depth_limit
      type(capnp_reader_options_t), intent(in), optional :: options
      integer(int8) :: head(0:3)
      integer(int8), allocatable :: table(:), body(:), whole(:)
      integer(int64) :: nsegs64, header_bytes, total, limit, i, n
      integer :: ios
      err = CAPNP_OK
      limit = reader_word_limit(traversal_words, options)
      read (unit, iostat=ios) head
      if (ios /= 0) then
         err = CAPNP_ERR_IO
         return
      end if
      nsegs64 = iand(int(cp_get_i32(head, 0_int64), int64), 4294967295_int64) + 1_int64
      if (nsegs64 < 1_int64 .or. nsegs64 > 511_int64) then
         err = CAPNP_ERR_FRAMING
         return
      end if
      header_bytes = ((1_int64 + nsegs64)*4_int64 + 7_int64)/8_int64*8_int64
      allocate (table(0:header_bytes - 5))
      read (unit, iostat=ios) table
      if (ios /= 0) then
         err = CAPNP_ERR_FRAMING
         return
      end if
      total = 0_int64
      do i = 1_int64, nsegs64
         ! Size entries start at byte 4 of the header; table lacks the head word.
         n = iand(int(cp_get_i32(table, (i - 1_int64)*4_int64), int64), 4294967295_int64)
         call capnp_add_segment_words(total, n, err, limit)
         if (err /= CAPNP_OK) return
      end do
      if (total > limit) then
         err = CAPNP_ERR_TRAVERSAL
         return
      end if
      total = total*CAPNP_WORD_BYTES
      call stream_body_fits(unit, total, err)
      if (err /= CAPNP_OK) return
      allocate (whole(0:header_bytes + total - 1), stat=ios)
      if (ios /= 0) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      whole(0:3) = head
      whole(4:header_bytes - 1) = table
      if (total > 0_int64) then
         allocate (body(0:total - 1), stat=ios)
         if (ios /= 0) then
            err = CAPNP_ERR_ALLOC
            return
         end if
         read (unit, iostat=ios) body
         if (ios /= 0) then
            err = CAPNP_ERR_FRAMING
            return
         end if
         whole(header_bytes:) = body
      end if
      call capnp_deserialize_bytes(whole, msg, err, traversal_words, depth_limit, options)
   end subroutine capnp_read_message_unit

   !> Read exactly one packed framed message from an open stream-access
   !> unit (C++ PackedMessageReader parity). The packed encoding is
   !> self-delimiting per word, so consumption stops at this message's
   !> last byte and packed messages written back-to-back read in sequence.
   subroutine capnp_read_message_packed_unit(unit, msg, err, traversal_words, depth_limit, options)
      use capnp_endian, only: cp_get_i32
      integer, intent(in) :: unit
      type(capnp_message_t), intent(inout) :: msg
      integer, intent(out) :: err
      integer(int64), intent(in), optional :: traversal_words
      integer, intent(in), optional :: depth_limit
      type(capnp_reader_options_t), intent(in), optional :: options
      type(pk_source_t) :: st
      integer(int8) :: w8(0:7)
      integer(int8) :: hdr(0:2055) ! 257 words; 511 segments need 256
      integer(int8), allocatable :: flat(:)
      integer(int64) :: nsegs64, header_words, total_words, nwords, limit, i, n
      integer :: ios
      err = CAPNP_OK
      limit = reader_word_limit(traversal_words, options)
      call unpack_word_from_unit(unit, st, w8, err)
      if (err /= CAPNP_OK) return
      if (run_pending(st)) then
         err = CAPNP_ERR_PACKED
         return
      end if
      nsegs64 = iand(int(cp_get_i32(w8, 0_int64), int64), 4294967295_int64) + 1_int64
      if (nsegs64 < 1_int64 .or. nsegs64 > 511_int64) then
         err = CAPNP_ERR_FRAMING
         return
      end if
      header_words = ((1_int64 + nsegs64)*4_int64 + 7_int64)/8_int64
      hdr(0:7) = w8
      do i = 2_int64, header_words
         call unpack_word_from_unit(unit, st, w8, err)
         if (err /= CAPNP_OK) return
         hdr((i - 1_int64)*8_int64:(i - 1_int64)*8_int64 + 7_int64) = w8
      end do
      if (run_pending(st)) then
         err = CAPNP_ERR_PACKED
         return
      end if
      total_words = 0_int64
      do i = 1_int64, nsegs64
         n = iand(int(cp_get_i32(hdr, i*4_int64), int64), 4294967295_int64)
         call capnp_add_segment_words(total_words, n, err, limit)
         if (err /= CAPNP_OK) return
      end do
      if (total_words > limit) then
         err = CAPNP_ERR_TRAVERSAL
         return
      end if
      call packed_stream_fits(unit, st, total_words, err)
      if (err /= CAPNP_OK) return
      nwords = header_words + total_words
      allocate (flat(0:nwords*CAPNP_WORD_BYTES - 1), stat=ios)
      if (ios /= 0) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      flat = 0_int8
      flat(0:header_words*CAPNP_WORD_BYTES - 1) = hdr(0:header_words*CAPNP_WORD_BYTES - 1)
      do i = header_words + 1_int64, nwords
         call unpack_word_from_unit(unit, st, w8, err)
         if (err /= CAPNP_OK) return
         flat((i - 1_int64)*8_int64:(i - 1_int64)*8_int64 + 7_int64) = w8
      end do
      if (run_pending(st)) then
         err = CAPNP_ERR_PACKED
         return
      end if
      call capnp_deserialize_bytes(flat, msg, err, traversal_words, depth_limit, options)
   end subroutine capnp_read_message_packed_unit

   !> True when a zero or literal run extends past the read it started in
   !> (C++ PackedInputStream bounds each read by its own end).
   pure logical function run_pending(st)
      type(pk_source_t), intent(in) :: st
      run_pending = st%zero_left > 0 .or. st%raw_left > 0
   end function run_pending

   !> Pull one unpacked word off a byte buffer of packed bytes, honouring
   !> zero-run and literal-run tags across calls.
   subroutine unpack_word_from_bytes(bytes, ipos, st, w8, err)
      integer(int8), intent(in) :: bytes(0:)
      integer(int64), intent(inout) :: ipos
      type(pk_source_t), intent(inout) :: st
      integer(int8), intent(out) :: w8(0:7)
      integer, intent(out) :: err
      integer(int8) :: tag, cnt
      integer(int64) :: n
      integer :: i
      err = CAPNP_OK
      w8 = 0_int8
      n = size(bytes, kind=int64)
      if (st%zero_left > 0) then
         st%zero_left = st%zero_left - 1
         return
      end if
      if (st%raw_left > 0) then
         if (ipos + 8_int64 > n) then
            err = CAPNP_ERR_PACKED
            return
         end if
         w8 = bytes(ipos:ipos + 7_int64)
         ipos = ipos + 8_int64
         st%raw_left = st%raw_left - 1
         return
      end if
      if (ipos >= n) then
         err = CAPNP_ERR_FRAMING
         return
      end if
      tag = bytes(ipos)
      ipos = ipos + 1_int64
      if (tag == 0_int8) then
         if (ipos >= n) then
            err = CAPNP_ERR_PACKED
            return
         end if
         cnt = bytes(ipos)
         ipos = ipos + 1_int64
         st%zero_left = int(iand(int(cnt), 255))
      else if (tag == -1_int8) then
         if (ipos + 9_int64 > n) then
            err = CAPNP_ERR_PACKED
            return
         end if
         w8 = bytes(ipos:ipos + 7_int64)
         ipos = ipos + 8_int64
         cnt = bytes(ipos)
         ipos = ipos + 1_int64
         st%raw_left = int(iand(int(cnt), 255))
      else
         do i = 0, 7
            if (btest(tag, i)) then
               if (ipos >= n) then
                  err = CAPNP_ERR_PACKED
                  return
               end if
               w8(i) = bytes(ipos)
               ipos = ipos + 1_int64
            end if
         end do
      end if
   end subroutine unpack_word_from_bytes

   !> Pull one unpacked word off a stream of packed bytes, honouring
   !> zero-run and literal-run tags across calls.
   subroutine unpack_word_from_unit(unit, st, w8, err)
      integer, intent(in) :: unit
      type(pk_source_t), intent(inout) :: st
      integer(int8), intent(out) :: w8(0:7)
      integer, intent(out) :: err
      integer(int8) :: tag, cnt, b
      integer :: ios, i
      err = CAPNP_OK
      w8 = 0_int8
      if (st%zero_left > 0) then
         st%zero_left = st%zero_left - 1
         return
      end if
      if (st%raw_left > 0) then
         read (unit, iostat=ios) w8
         if (ios /= 0) err = CAPNP_ERR_PACKED
         st%raw_left = st%raw_left - 1
         return
      end if
      read (unit, iostat=ios) tag
      if (ios /= 0) then
         err = CAPNP_ERR_IO
         return
      end if
      if (tag == 0_int8) then
         read (unit, iostat=ios) cnt
         if (ios /= 0) then
            err = CAPNP_ERR_PACKED
            return
         end if
         st%zero_left = int(iand(int(cnt), 255))
      else if (tag == -1_int8) then
         read (unit, iostat=ios) w8
         if (ios /= 0) then
            err = CAPNP_ERR_PACKED
            return
         end if
         read (unit, iostat=ios) cnt
         if (ios /= 0) then
            err = CAPNP_ERR_PACKED
            return
         end if
         st%raw_left = int(iand(int(cnt), 255))
      else
         do i = 0, 7
            if (btest(tag, i)) then
               read (unit, iostat=ios) b
               if (ios /= 0) then
                  err = CAPNP_ERR_PACKED
                  return
               end if
               w8(i) = b
            end if
         end do
      end if
   end subroutine unpack_word_from_unit

   !> Add one segment's word count, rejecting a total above the reader
   !> traversal limit before any body allocation. limit overrides the
   !> default eight mebiword budget; omitting it keeps that default.
   pure subroutine capnp_add_segment_words(total, n, err, limit)
      integer(int64), intent(inout) :: total
      integer(int64), intent(in) :: n
      integer, intent(out) :: err
      integer(int64), intent(in), optional :: limit
      integer(int64) :: cap
      err = CAPNP_OK
      cap = CAPNP_DEFAULT_TRAVERSAL_WORDS
      if (present(limit)) cap = limit
      if (n < 0_int64) then
         err = CAPNP_ERR_FRAMING
         return
      end if
      if (cap < 0_int64 .or. n > cap .or. total > cap - n) then
         err = CAPNP_ERR_TRAVERSAL
         return
      end if
      total = total + n
   end subroutine capnp_add_segment_words

   !> Traversal budget from an explicit word count, else reader options,
   !> else the eight mebiword default. Explicit wins, matching deserialize.
   pure function reader_word_limit(traversal_words, options) result(limit)
      integer(int64), intent(in), optional :: traversal_words
      type(capnp_reader_options_t), intent(in), optional :: options
      integer(int64) :: limit
      limit = CAPNP_DEFAULT_TRAVERSAL_WORDS
      if (present(options)) limit = options%traversal_words
      if (present(traversal_words)) limit = traversal_words
   end function reader_word_limit

   !> Most words still available from a packed tail. zero_left words are
   !> already tagged. raw_left words still cost 8 bytes each. Every
   !> remaining pair of bytes expands to at most 256 zero words.
   pure subroutine capnp_packed_room(rem_bytes, zero_left, raw_left, room, err)
      integer(int64), intent(in) :: rem_bytes
      integer, intent(in) :: zero_left, raw_left
      integer(int64), intent(out) :: room
      integer, intent(out) :: err
      integer(int64) :: rem
      err = CAPNP_OK
      room = 0_int64
      if (rem_bytes < 0_int64 .or. zero_left < 0 .or. raw_left < 0) then
         err = CAPNP_ERR_PACKED
         return
      end if
      rem = rem_bytes
      room = int(zero_left, int64)
      if (raw_left > 0) then
         if (rem < int(raw_left, int64)*8_int64) then
            err = CAPNP_ERR_PACKED
            return
         end if
         room = room + int(raw_left, int64)
         rem = rem - int(raw_left, int64)*8_int64
      end if
      room = room + (rem/2_int64)*256_int64
   end subroutine capnp_packed_room

   !> Reject a body longer than the bytes still in a sized file. Pipes
   !> report no size; the traversal cap is the bound in that case.
   subroutine stream_body_fits(unit, need, err)
      integer, intent(in) :: unit
      integer(int64), intent(in) :: need
      integer, intent(out) :: err
      integer(int64) :: fsz, fpos
      integer :: ios
      err = CAPNP_OK
      if (need < 0_int64) then
         err = CAPNP_ERR_FRAMING
         return
      end if
      inquire (unit=unit, size=fsz, pos=fpos, iostat=ios)
      if (ios /= 0) return
      if (fsz < 1_int64 .or. fpos < 1_int64) return
      if (need > fsz - fpos + 1_int64) err = CAPNP_ERR_FRAMING
   end subroutine stream_body_fits

   subroutine packed_stream_fits(unit, st, total_words, err)
      integer, intent(in) :: unit
      type(pk_source_t), intent(in) :: st
      integer(int64), intent(in) :: total_words
      integer, intent(out) :: err
      integer(int64) :: fsz, fpos, rem, room
      integer :: ios
      err = CAPNP_OK
      inquire (unit=unit, size=fsz, pos=fpos, iostat=ios)
      if (ios /= 0) return
      if (fsz < 1_int64 .or. fpos < 1_int64) return
      rem = fsz - fpos + 1_int64
      call capnp_packed_room(rem, st%zero_left, st%raw_left, room, err)
      if (err /= CAPNP_OK) return
      if (total_words > room) err = CAPNP_ERR_FRAMING
   end subroutine packed_stream_fits

end module capnp_stream
