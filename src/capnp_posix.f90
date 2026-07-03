!> Thin bind(c) surface over the POSIX socket API: no C sources, only
!> interfaces into libc. Enough for the two-party RPC transport: stream
!> sockets (TCP and Unix-domain), socketpair loopbacks for tests, and
!> poll-based readiness.
module capnp_posix
   use iso_c_binding
   use capnp_kinds, only: int8, int64, CAPNP_OK, CAPNP_ERR_IO
   implicit none
   private

   public :: px_socketpair, px_close, px_shutdown_wr
   public :: px_send_all, px_recv_all, px_recv_some
   public :: px_tcp_listen, px_tcp_accept, px_tcp_connect
   public :: px_poll_in

   integer, parameter, public :: PX_BAD_FD = -1

   ! Linux ABI constants (x86_64 and aarch64 share these).
   integer(c_int), parameter :: AF_UNIX = 1
   integer(c_int), parameter :: AF_INET = 2
   integer(c_int), parameter :: SOCK_STREAM = 1
   integer(c_int), parameter :: SHUT_WR = 1
   integer(c_int), parameter :: SOL_SOCKET = 1
   integer(c_int), parameter :: SO_REUSEADDR = 2
   integer(c_short), parameter :: POLLIN = 1_c_short

   type, bind(c) :: c_pollfd
      integer(c_int) :: fd
      integer(c_short) :: events
      integer(c_short) :: revents
   end type c_pollfd

   !> struct sockaddr_in, fixed 16 bytes.
   type, bind(c) :: c_sockaddr_in
      integer(c_short) :: sin_family
      integer(c_int16_t) :: sin_port ! big-endian on the wire
      integer(c_int32_t) :: sin_addr
      integer(c_int8_t) :: sin_zero(8)
   end type c_sockaddr_in

   interface
      function c_socket(domain, type_, protocol) bind(c, name='socket') result(fd)
         import :: c_int
         integer(c_int), value :: domain, type_, protocol
         integer(c_int) :: fd
      end function c_socket

      function c_socketpair(domain, type_, protocol, sv) bind(c, name='socketpair') result(rc)
         import :: c_int
         integer(c_int), value :: domain, type_, protocol
         integer(c_int), intent(out) :: sv(2)
         integer(c_int) :: rc
      end function c_socketpair

      function c_close(fd) bind(c, name='close') result(rc)
         import :: c_int
         integer(c_int), value :: fd
         integer(c_int) :: rc
      end function c_close

      function c_shutdown(fd, how) bind(c, name='shutdown') result(rc)
         import :: c_int
         integer(c_int), value :: fd, how
         integer(c_int) :: rc
      end function c_shutdown

      function c_send(fd, buf, n, flags) bind(c, name='send') result(sent)
         import :: c_int, c_size_t, c_ptr, c_long
         integer(c_int), value :: fd, flags
         type(c_ptr), value :: buf
         integer(c_size_t), value :: n
         integer(c_long) :: sent
      end function c_send

      function c_recv(fd, buf, n, flags) bind(c, name='recv') result(got)
         import :: c_int, c_size_t, c_ptr, c_long
         integer(c_int), value :: fd, flags
         type(c_ptr), value :: buf
         integer(c_size_t), value :: n
         integer(c_long) :: got
      end function c_recv

      function c_bind(fd, addr, addrlen) bind(c, name='bind') result(rc)
         import :: c_int, c_ptr
         integer(c_int), value :: fd
         type(c_ptr), value :: addr
         integer(c_int), value :: addrlen
         integer(c_int) :: rc
      end function c_bind

      function c_listen(fd, backlog) bind(c, name='listen') result(rc)
         import :: c_int
         integer(c_int), value :: fd, backlog
         integer(c_int) :: rc
      end function c_listen

      function c_accept(fd, addr, addrlen) bind(c, name='accept') result(cfd)
         import :: c_int, c_ptr
         integer(c_int), value :: fd
         type(c_ptr), value :: addr, addrlen
         integer(c_int) :: cfd
      end function c_accept

      function c_connect(fd, addr, addrlen) bind(c, name='connect') result(rc)
         import :: c_int, c_ptr
         integer(c_int), value :: fd
         type(c_ptr), value :: addr
         integer(c_int), value :: addrlen
         integer(c_int) :: rc
      end function c_connect

      function c_setsockopt(fd, level, optname, optval, optlen) &
         bind(c, name='setsockopt') result(rc)
         import :: c_int, c_ptr
         integer(c_int), value :: fd, level, optname
         type(c_ptr), value :: optval
         integer(c_int), value :: optlen
         integer(c_int) :: rc
      end function c_setsockopt

      function c_poll(fds, nfds, timeout_ms) bind(c, name='poll') result(nready)
         import :: c_int, c_ptr, c_long
         type(c_ptr), value :: fds
         integer(c_long), value :: nfds
         integer(c_int), value :: timeout_ms
         integer(c_int) :: nready
      end function c_poll
   end interface

contains

   !> Connected AF_UNIX stream pair; the loopback transport for tests.
   subroutine px_socketpair(fd_a, fd_b, err)
      integer, intent(out) :: fd_a, fd_b, err
      integer(c_int) :: sv(2)
      err = CAPNP_OK
      fd_a = PX_BAD_FD
      fd_b = PX_BAD_FD
      if (c_socketpair(AF_UNIX, SOCK_STREAM, 0_c_int, sv) /= 0_c_int) then
         err = CAPNP_ERR_IO
         return
      end if
      fd_a = int(sv(1))
      fd_b = int(sv(2))
   end subroutine px_socketpair

   subroutine px_close(fd)
      integer, intent(in) :: fd
      integer(c_int) :: rc
      if (fd >= 0) rc = c_close(int(fd, c_int))
   end subroutine px_close

   !> Half-close the write side; the peer's reads then see EOF.
   subroutine px_shutdown_wr(fd)
      integer, intent(in) :: fd
      integer(c_int) :: rc
      if (fd >= 0) rc = c_shutdown(int(fd, c_int), SHUT_WR)
   end subroutine px_shutdown_wr

   !> Write the whole buffer, looping over short sends.
   subroutine px_send_all(fd, bytes, err)
      integer, intent(in) :: fd
      integer(int8), intent(in), target :: bytes(:)
      integer, intent(out) :: err
      integer(int64) :: off, n
      integer(c_long) :: sent
      err = CAPNP_OK
      n = size(bytes, kind=int64)
      off = 0_int64
      do while (off < n)
         sent = c_send(int(fd, c_int), c_loc(bytes(lbound(bytes, 1) + off)), &
                       int(n - off, c_size_t), 0_c_int)
         if (sent <= 0_c_long) then
            err = CAPNP_ERR_IO
            return
         end if
         off = off + int(sent, int64)
      end do
   end subroutine px_send_all

   !> Read exactly size(bytes) bytes, looping over short reads. EOF before
   !> the buffer fills is an error.
   subroutine px_recv_all(fd, bytes, err)
      integer, intent(in) :: fd
      integer(int8), intent(out), target :: bytes(:)
      integer, intent(out) :: err
      integer(int64) :: off, n
      integer(c_long) :: got
      err = CAPNP_OK
      n = size(bytes, kind=int64)
      off = 0_int64
      do while (off < n)
         got = c_recv(int(fd, c_int), c_loc(bytes(lbound(bytes, 1) + off)), &
                      int(n - off, c_size_t), 0_c_int)
         if (got <= 0_c_long) then
            err = CAPNP_ERR_IO
            return
         end if
         off = off + int(got, int64)
      end do
   end subroutine px_recv_all

   !> One recv: up to size(bytes); got = 0 means EOF.
   subroutine px_recv_some(fd, bytes, got, err)
      integer, intent(in) :: fd
      integer(int8), intent(out), target :: bytes(:)
      integer(int64), intent(out) :: got
      integer, intent(out) :: err
      integer(c_long) :: r
      err = CAPNP_OK
      got = 0_int64
      r = c_recv(int(fd, c_int), c_loc(bytes(lbound(bytes, 1))), &
                 int(size(bytes, kind=int64), c_size_t), 0_c_int)
      if (r < 0_c_long) then
         err = CAPNP_ERR_IO
         return
      end if
      got = int(r, int64)
   end subroutine px_recv_some

   !> Listening TCP socket on 127.0.0.1:port (port 0 picks a free one;
   !> callers using 0 must discover the port out of band).
   subroutine px_tcp_listen(port, fd, err)
      integer, intent(in) :: port
      integer, intent(out) :: fd, err
      type(c_sockaddr_in), target :: sa
      integer(c_int), target :: one
      integer(c_int) :: sfd
      err = CAPNP_OK
      fd = PX_BAD_FD
      sfd = c_socket(AF_INET, SOCK_STREAM, 0_c_int)
      if (sfd < 0_c_int) then
         err = CAPNP_ERR_IO
         return
      end if
      one = 1_c_int
      if (c_setsockopt(sfd, SOL_SOCKET, SO_REUSEADDR, c_loc(one), 4_c_int) /= 0) continue
      sa%sin_family = int(AF_INET, c_short)
      sa%sin_port = htons16(port)
      sa%sin_addr = htonl_loopback()
      sa%sin_zero = 0_c_int8_t
      if (c_bind(sfd, c_loc(sa), 16_c_int) /= 0_c_int) then
         err = CAPNP_ERR_IO
         call px_close(int(sfd))
         return
      end if
      if (c_listen(sfd, 8_c_int) /= 0_c_int) then
         err = CAPNP_ERR_IO
         call px_close(int(sfd))
         return
      end if
      fd = int(sfd)
   end subroutine px_tcp_listen

   subroutine px_tcp_accept(listen_fd, fd, err)
      integer, intent(in) :: listen_fd
      integer, intent(out) :: fd, err
      integer(c_int) :: cfd
      err = CAPNP_OK
      fd = PX_BAD_FD
      cfd = c_accept(int(listen_fd, c_int), c_null_ptr, c_null_ptr)
      if (cfd < 0_c_int) then
         err = CAPNP_ERR_IO
         return
      end if
      fd = int(cfd)
   end subroutine px_tcp_accept

   subroutine px_tcp_connect(port, fd, err)
      integer, intent(in) :: port
      integer, intent(out) :: fd, err
      type(c_sockaddr_in), target :: sa
      integer(c_int) :: sfd
      err = CAPNP_OK
      fd = PX_BAD_FD
      sfd = c_socket(AF_INET, SOCK_STREAM, 0_c_int)
      if (sfd < 0_c_int) then
         err = CAPNP_ERR_IO
         return
      end if
      sa%sin_family = int(AF_INET, c_short)
      sa%sin_port = htons16(port)
      sa%sin_addr = htonl_loopback()
      sa%sin_zero = 0_c_int8_t
      if (c_connect(sfd, c_loc(sa), 16_c_int) /= 0_c_int) then
         err = CAPNP_ERR_IO
         call px_close(int(sfd))
         return
      end if
      fd = int(sfd)
   end subroutine px_tcp_connect

   !> Block until fd is readable or timeout_ms elapses; readable out.
   subroutine px_poll_in(fd, timeout_ms, readable, err)
      integer, intent(in) :: fd, timeout_ms
      logical, intent(out) :: readable
      integer, intent(out) :: err
      type(c_pollfd), target :: p
      integer(c_int) :: n
      err = CAPNP_OK
      readable = .false.
      p%fd = int(fd, c_int)
      p%events = POLLIN
      p%revents = 0_c_short
      n = c_poll(c_loc(p), 1_c_long, int(timeout_ms, c_int))
      if (n < 0_c_int) then
         err = CAPNP_ERR_IO
         return
      end if
      readable = iand(int(p%revents), int(POLLIN)) /= 0
   end subroutine px_poll_in

   !> Port to network byte order in a signed int16 container.
   pure function htons16(port) result(v)
      integer, intent(in) :: port
      integer(c_int16_t) :: v
      integer :: hi, lo, u
      hi = iand(port/256, 255)
      lo = iand(port, 255)
      u = lo*256 + hi
      if (u > 32767) u = u - 65536
      v = int(u, c_int16_t)
   end function htons16

   !> 127.0.0.1 in network byte order.
   pure function htonl_loopback() result(v)
      integer(c_int32_t) :: v
      ! Bytes on the wire: 7f 00 00 01; little-endian container holds
      ! 0x0100007f.
      v = int(16777343, c_int32_t)
   end function htonl_loopback

end module capnp_posix
