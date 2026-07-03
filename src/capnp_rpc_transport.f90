!> Framed Cap'n Proto messages over a stream socket: the standard segment
!> table framing (as capnp_serialize) sent and received on a file
!> descriptor. This is the byte layer of the two-party vat network.
module capnp_rpc_transport
   use capnp_kinds
   use capnp_endian, only: cp_get_i32
   use capnp_arena, only: capnp_message_t
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
   !> short read surfaces as CAPNP_ERR_IO.
   subroutine rpc_recv_message(fd, msg, err)
      integer, intent(in) :: fd
      type(capnp_message_t), intent(inout) :: msg
      integer, intent(out) :: err
      integer(int8) :: head(0:3)
      integer(int8), allocatable :: rest(:), whole(:)
      integer(int64) :: nsegs64, header_bytes, total, i, n
      call px_recv_all(fd, head, err)
      if (err /= CAPNP_OK) return
      nsegs64 = iand(int(cp_get_i32(head, 0_int64), int64), 4294967295_int64) + 1_int64
      if (nsegs64 < 1_int64 .or. nsegs64 > 512_int64) then
         err = CAPNP_ERR_FRAMING
         return
      end if
      header_bytes = ((1_int64 + nsegs64)*4_int64 + 7_int64)/8_int64*8_int64
      allocate (rest(0:header_bytes - 5_int64))
      call px_recv_all(fd, rest, err)
      if (err /= CAPNP_OK) return
      total = 0_int64
      do i = 1_int64, nsegs64
         ! Size entries start at header byte 4; rest lacks the first word.
         n = iand(int(cp_get_i32(rest, (i - 1_int64)*4_int64), int64), 4294967295_int64)
         total = total + n*CAPNP_WORD_BYTES
      end do
      allocate (whole(0:header_bytes + total - 1_int64))
      whole(0:3) = head
      whole(4:header_bytes - 1) = rest
      if (total > 0_int64) then
         block
            integer(int8), allocatable :: body(:)
            allocate (body(0:total - 1_int64))
            call px_recv_all(fd, body, err)
            if (err /= CAPNP_OK) return
            whole(header_bytes:) = body
         end block
      end if
      call capnp_deserialize_bytes(whole, msg, err)
   end subroutine rpc_recv_message

end module capnp_rpc_transport
