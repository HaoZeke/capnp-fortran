!> Framed Cap'n Proto messages over a stream socket: the standard segment
!> table framing (as capnp_serialize) sent and received on a file
!> descriptor. This is the byte layer of the two-party vat network.
module capnp_rpc_transport
   use capnp_kinds
   use capnp_endian, only: cp_get_i32
   use capnp_arena, only: capnp_message_t, capnp_message_free
   use capnp_serialize, only: capnp_serialize_bytes, capnp_deserialize_bytes
   use capnp_posix, only: px_send_all, px_recv_all
   implicit none
   private

   public :: rpc_send_message, rpc_recv_message

contains

   subroutine rpc_send_message(fd, msg, err)
      integer, intent(in) :: fd
      type(capnp_message_t), intent(in) :: msg
      integer, intent(out) :: err
      integer(int8), allocatable :: bytes(:)
      call capnp_serialize_bytes(msg, bytes, err)
      if (err /= CAPNP_OK) return
      call px_send_all(fd, bytes, err)
   end subroutine rpc_send_message

   !> Read exactly one framed message off the socket (blocking). EOF or a
   !> short read surfaces as CAPNP_ERR_IO. Mirrors Cap'n Proto C++ 1.4.0
   !> AsyncMessageReader: firstWord[0] < 511 (at most 511 segments) and
   !> total segment words <= traversal_words (default
   !> CAPNP_DEFAULT_TRAVERSAL_WORDS) checked before allocating the body.
   subroutine rpc_recv_message(fd, msg, err, traversal_words)
      integer, intent(in) :: fd
      type(capnp_message_t), intent(inout) :: msg
      integer, intent(out) :: err
      integer(int64), intent(in), optional :: traversal_words
      integer(int8), target :: hdr(0:2047)
      integer(int8), allocatable, target :: whole(:)
      integer(int64) :: raw_count, nsegs64, header_bytes, total_words, total_bytes
      integer(int64) :: limit, i, n
      integer :: astat
      call capnp_message_free(msg)
      limit = CAPNP_DEFAULT_TRAVERSAL_WORDS
      if (present(traversal_words)) then
         if (traversal_words < 0_int64) then
            err = CAPNP_ERR_ARG
            return
         end if
         limit = traversal_words
      end if
      call px_recv_all(fd, hdr(0:7), err)
      if (err /= CAPNP_OK) return
      raw_count = iand(int(cp_get_i32(hdr, 0_int64), int64), 4294967295_int64)
      if (raw_count >= 511_int64) then
         err = CAPNP_ERR_FRAMING
         return
      end if
      nsegs64 = raw_count + 1_int64
      header_bytes = ((1_int64 + nsegs64)*4_int64 + 7_int64)/8_int64*8_int64
      if (header_bytes > 8_int64) then
         call px_recv_all(fd, hdr(8:header_bytes - 1_int64), err)
         if (err /= CAPNP_OK) return
      end if
      total_words = 0_int64
      do i = 1_int64, nsegs64
         n = iand(int(cp_get_i32(hdr, i*4_int64), int64), 4294967295_int64)
         total_words = total_words + n
      end do
      if (total_words > limit) then
         err = CAPNP_ERR_TRAVERSAL
         return
      end if
      total_bytes = total_words*CAPNP_WORD_BYTES
      allocate (whole(0:header_bytes + total_bytes - 1_int64), stat=astat)
      if (astat /= 0) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      whole(0:header_bytes - 1_int64) = hdr(0:header_bytes - 1_int64)
      if (total_bytes > 0_int64) then
         call px_recv_all(fd, whole(header_bytes:), err)
         if (err /= CAPNP_OK) return
      end if
      call capnp_deserialize_bytes(whole, msg, err, traversal_words=limit)
   end subroutine rpc_recv_message

end module capnp_rpc_transport
