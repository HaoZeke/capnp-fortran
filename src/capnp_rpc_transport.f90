!> Framed Cap'n Proto messages over a stream socket: the standard segment
!> table framing (as capnp_serialize) sent and received on a file
!> descriptor. This is the byte layer of the two-party vat network.
module capnp_rpc_transport
   use capnp_kinds
   use capnp_endian, only: cp_get_i32
   use capnp_arena, only: capnp_message_t, capnp_message_free
   use capnp_serialize, only: capnp_serialize_bytes, capnp_deserialize_bytes
   use capnp_posix, only: PX_BAD_FD, px_send_all, px_recv_all, px_poll_in, &
                         px_close, px_shutdown_wr, px_socketpair
   implicit none
   private

   public :: rpc_send_message, rpc_recv_message
   public :: rpc_byte_stream_t, rpc_fd_stream_t
   public :: rpc_fd_stream_init, rpc_socketpair_streams

   type, abstract :: rpc_byte_stream_t
   contains
      procedure(rpc_stream_io_ifc), deferred :: send_all
      procedure(rpc_stream_io_ifc), deferred :: recv_all
      procedure(rpc_stream_poll_ifc), deferred :: poll_in
      procedure(rpc_stream_close_ifc), deferred :: close
   end type rpc_byte_stream_t

   type, extends(rpc_byte_stream_t) :: rpc_fd_stream_t
      integer :: fd = PX_BAD_FD
      logical :: owns_fd = .false.
   contains
      procedure :: send_all => fd_send_all
      procedure :: recv_all => fd_recv_all
      procedure :: poll_in => fd_poll_in
      procedure :: close => fd_close
   end type rpc_fd_stream_t

   abstract interface
      subroutine rpc_stream_io_ifc(self, bytes, err)
         import :: rpc_byte_stream_t, int8
         class(rpc_byte_stream_t), intent(inout) :: self
         integer(int8), intent(inout), target :: bytes(:)
         integer, intent(out) :: err
      end subroutine rpc_stream_io_ifc

      subroutine rpc_stream_poll_ifc(self, timeout_ms, readable, err)
         import :: rpc_byte_stream_t
         class(rpc_byte_stream_t), intent(inout) :: self
         integer, intent(in) :: timeout_ms
         logical, intent(out) :: readable
         integer, intent(out) :: err
      end subroutine rpc_stream_poll_ifc

      subroutine rpc_stream_close_ifc(self)
         import :: rpc_byte_stream_t
         class(rpc_byte_stream_t), intent(inout) :: self
      end subroutine rpc_stream_close_ifc
   end interface

   interface rpc_send_message
      module procedure rpc_send_message_fd
      module procedure rpc_send_message_stream
   end interface rpc_send_message

   interface rpc_recv_message
      module procedure rpc_recv_message_fd
      module procedure rpc_recv_message_stream
   end interface rpc_recv_message

contains

   subroutine rpc_fd_stream_init(stream, fd, owns_fd)
      type(rpc_fd_stream_t), intent(out) :: stream
      integer, intent(in) :: fd
      logical, intent(in), optional :: owns_fd
      stream%fd = fd
      stream%owns_fd = .false.
      if (present(owns_fd)) stream%owns_fd = owns_fd
   end subroutine rpc_fd_stream_init

   subroutine rpc_socketpair_streams(a, b, err)
      type(rpc_fd_stream_t), intent(out) :: a, b
      integer, intent(out) :: err
      integer :: fda, fdb
      call px_socketpair(fda, fdb, err)
      if (err /= CAPNP_OK) return
      call rpc_fd_stream_init(a, fda, .true.)
      call rpc_fd_stream_init(b, fdb, .true.)
   end subroutine rpc_socketpair_streams

   subroutine fd_send_all(self, bytes, err)
      class(rpc_fd_stream_t), intent(inout) :: self
      integer(int8), intent(inout), target :: bytes(:)
      integer, intent(out) :: err
      call px_send_all(self%fd, bytes, err)
   end subroutine fd_send_all

   subroutine fd_recv_all(self, bytes, err)
      class(rpc_fd_stream_t), intent(inout) :: self
      integer(int8), intent(inout), target :: bytes(:)
      integer, intent(out) :: err
      call px_recv_all(self%fd, bytes, err)
   end subroutine fd_recv_all

   subroutine fd_poll_in(self, timeout_ms, readable, err)
      class(rpc_fd_stream_t), intent(inout) :: self
      integer, intent(in) :: timeout_ms
      logical, intent(out) :: readable
      integer, intent(out) :: err
      call px_poll_in(self%fd, timeout_ms, readable, err)
   end subroutine fd_poll_in

   subroutine fd_close(self)
      class(rpc_fd_stream_t), intent(inout) :: self
      if (self%fd /= PX_BAD_FD .and. self%owns_fd) then
         call px_shutdown_wr(self%fd)
         call px_close(self%fd)
      end if
      self%fd = PX_BAD_FD
   end subroutine fd_close

   subroutine rpc_send_message_fd(fd, msg, err)
      integer, intent(in) :: fd
      type(capnp_message_t), intent(in) :: msg
      integer, intent(out) :: err
      type(rpc_fd_stream_t) :: stream
      call rpc_fd_stream_init(stream, fd)
      call rpc_send_message_stream(stream, msg, err)
   end subroutine rpc_send_message_fd

   subroutine rpc_send_message_stream(stream, msg, err)
      class(rpc_byte_stream_t), intent(inout) :: stream
      type(capnp_message_t), intent(in) :: msg
      integer, intent(out) :: err
      integer(int8), allocatable :: bytes(:)
      call capnp_serialize_bytes(msg, bytes, err)
      if (err /= CAPNP_OK) return
      call stream%send_all(bytes, err)
   end subroutine rpc_send_message_stream

   !> Read exactly one framed message off the socket (blocking). EOF or a
   !> short read surfaces as CAPNP_ERR_IO. Mirrors Cap'n Proto C++ 1.4.0
   !> AsyncMessageReader: firstWord[0] < 511 (at most 511 segments) and
   !> total segment words <= traversal_words (default
   !> CAPNP_DEFAULT_TRAVERSAL_WORDS) checked before allocating the body.
   subroutine rpc_recv_message_fd(fd, msg, err, traversal_words)
      integer, intent(in) :: fd
      type(capnp_message_t), intent(inout) :: msg
      integer, intent(out) :: err
      integer(int64), intent(in), optional :: traversal_words
      type(rpc_fd_stream_t) :: stream
      call rpc_fd_stream_init(stream, fd)
      call rpc_recv_message_stream(stream, msg, err, traversal_words)
   end subroutine rpc_recv_message_fd

   subroutine rpc_recv_message_stream(stream, msg, err, traversal_words)
      class(rpc_byte_stream_t), intent(inout) :: stream
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
      call stream%recv_all(hdr(0:7), err)
      if (err /= CAPNP_OK) return
      raw_count = iand(int(cp_get_i32(hdr, 0_int64), int64), 4294967295_int64)
      if (raw_count >= 511_int64) then
         err = CAPNP_ERR_FRAMING
         return
      end if
      nsegs64 = raw_count + 1_int64
      header_bytes = ((1_int64 + nsegs64)*4_int64 + 7_int64)/8_int64*8_int64
      if (header_bytes > 8_int64) then
         call stream%recv_all(hdr(8:header_bytes - 1_int64), err)
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
         call stream%recv_all(whole(header_bytes:), err)
         if (err /= CAPNP_OK) return
      end if
      call capnp_deserialize_bytes(whole, msg, err, traversal_words=limit)
   end subroutine rpc_recv_message_stream

end module capnp_rpc_transport
