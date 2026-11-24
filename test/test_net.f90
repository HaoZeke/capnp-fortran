!> TCP connect completion. A hard error such as ENETUNREACH is not a
!> connect that is still in progress.
module test_net
   use testdrive, only: unittest_type, error_type, check, skip_test
   use capnp, only: CAPNP_OK, int8
   use capnp_posix
   implicit none
   private
   public :: collect_net

contains

   subroutine collect_net(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      allocate (testsuite(3))
      testsuite(1)%name = "connect-completes"
      testsuite(1)%test => run_connect_ok
      testsuite(2)%name = "connect-unreach"
      testsuite(2)%test => run_connect_unreach
      testsuite(3)%name = "send-to-closed-peer"
      testsuite(3)%test => run_send_closed
   end subroutine collect_net

   subroutine check_(error, cond, name)
      type(error_type), allocatable, intent(inout) :: error
      logical, intent(in) :: cond
      character(len=*), intent(in) :: name
      if (allocated(error)) return
      call check(error, cond, name)
   end subroutine check_

   !> EINPROGRESS is completed with poll and SO_ERROR. A listener on
   !> loopback accepts the connect.
   subroutine run_connect_ok(error)
      type(error_type), allocatable, intent(out) :: error
      integer :: lfd, port, fd, acc, err
      call px_tcp_listen(0, lfd, err)
      call px_tcp_bound_port(lfd, port, err)
      call check_(error, err == CAPNP_OK .and. port > 0, 'connect: listener')
      if (allocated(error)) return
      call px_tcp_connect_host('127.0.0.1', port, fd, err, timeout_ms=1000)
      call check_(error, err == CAPNP_OK .and. fd /= PX_BAD_FD, &
                  'connect: in-progress connect completes')
      call px_tcp_accept(lfd, acc, err)
      call px_close(acc)
      call px_close(fd)
      call px_close(lfd)
   end subroutine run_connect_ok

   !> ENETUNREACH must not be reported as a successful connect. The
   !> check runs in a network namespace, where that is what connect
   !> returns and SO_ERROR stays zero.
   subroutine run_connect_unreach(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=8) :: flag
      character(len=4096) :: bin
      integer :: stat, cs, fd, err
      call get_environment_variable('CAPNP_IN_NETNS', flag)
      if (flag == '1') then
         call px_tcp_connect_host('192.0.2.1', 9, fd, err, timeout_ms=200)
         call check_(error, err /= CAPNP_OK .and. fd == PX_BAD_FD, &
                     'connect: unreachable network is an error')
         if (err == CAPNP_OK) call px_close(fd)
         return
      end if
      call check_(error, .not. source_has_find(), &
                  'connect: the check does not search the build tree')
      if (allocated(error)) return
      call get_command_argument(0, bin)
      ! The shell can start `unshare` and still return nonzero when user
      ! namespaces are blocked. cmdstat stays zero in that case, so probe
      ! `unshare -n -r` and skip unless the probe itself exits zero.
      stat = 0
      cs = 0
      call execute_command_line( &
         'unshare -n -r true >/dev/null 2>/dev/null', &
         exitstat=stat, cmdstat=cs)
      if (cs /= 0 .or. stat /= 0) then
         call skip_test(error, 'unshare -n -r is not available')
         return
      end if
      stat = 0
      cs = 0
      call execute_command_line( &
         'unshare -n -r env CAPNP_IN_NETNS=1 "' // trim(bin) // &
         '" net connect-unreach >/dev/null 2>/dev/null', &
         exitstat=stat, cmdstat=cs)
      if (cs /= 0) then
         call skip_test(error, 'unshare is not available')
         return
      end if
      call check_(error, stat == 0, 'connect: unreachable network is an error')
   end subroutine run_connect_unreach

   !> A write to a peer that has gone away is an I/O error, not SIGPIPE.
   !> KJ ignores the signal, so a C++ peer sees the same write fail and
   !> carries on (async-unix.c++ 218-229).
   subroutine run_send_closed(error)
      type(error_type), allocatable, intent(out) :: error
      integer :: a, b, err, k
      integer(int8) :: bytes(64)
      call px_socketpair(a, b, err)
      call check_(error, err == CAPNP_OK, 'send: socketpair')
      if (allocated(error)) return
      call px_close(b)
      bytes = 0_int8
      do k = 1, 2
         call px_send_all(a, bytes, err)
      end do
      call check_(error, err /= CAPNP_OK, 'send: a closed peer is an I/O error')
      call px_close(a)
   end subroutine run_send_closed

   function source_has_find() result(found)
      logical :: found
      integer :: u, ios
      character(len=240) :: line
      found = .false.
      open (newunit=u, file='test/test_net.f90', status='old', action='read', iostat=ios)
      if (ios /= 0) return
      do
         read (u, '(a)', iostat=ios) line
         if (ios /= 0) exit
         if (index(line, 'find' // ' build') > 0) found = .true.
      end do
      close (u)
   end function source_has_find

end module test_net
