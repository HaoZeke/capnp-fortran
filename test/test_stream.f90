!> `-> stream` methods: windowed client-side flow control over the
!> ordinary Call/Return wire traffic, plus the streaming error rule
!> (first failure surfaces at finish; later sends fail fast).
module test_stream
   use iso_c_binding, only: c_int, c_loc, c_ptr
   use testdrive, only: unittest_type, error_type, check, test_failed, skip_test
   use capnp
   use streamer_capnp
   use capnp_posix
   use capnp_rpc
   use rpc_writer_impl
   implicit none

   private
   public :: collect_stream

   integer(c_int), parameter :: c_sol_socket = 1_c_int
   integer(c_int), parameter :: c_so_sndbuf = 7_c_int

   interface
      function c_setsockopt(fd, level, optname, optval, optlen) &
         bind(c, name='setsockopt') result(rc)
         import :: c_int, c_ptr
         integer(c_int), value :: fd, level, optname
         type(c_ptr), value :: optval
         integer(c_int), value :: optlen
         integer(c_int) :: rc
      end function c_setsockopt
      function c_getsockopt(fd, level, optname, optval, optlen) &
         bind(c, name='getsockopt') result(rc)
         import :: c_int, c_ptr
         integer(c_int), value :: fd, level, optname
         type(c_ptr), value :: optval, optlen
         integer(c_int) :: rc
      end function c_getsockopt
   end interface

   type, extends(writer_server_t) :: hold_writer_t
   contains
      procedure :: write => hold_write
      procedure :: done => hold_done
   end type hold_writer_t

contains

   subroutine collect_stream(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      allocate (testsuite(2))
      testsuite(1)%name = "stream"
      testsuite(1)%test => run_stream
      testsuite(2)%name = "stream_window"
      testsuite(2)%test => run_stream_window
   end subroutine collect_stream

   subroutine check_(error, cond, name)
      type(error_type), allocatable, intent(inout) :: error
      logical, intent(in) :: cond
      character(len=*), intent(in) :: name
      if (allocated(error)) return
      call check(error, cond, name)
   end subroutine check_

   subroutine run_stream(error)
      type(error_type), allocatable, intent(out) :: error
      type(rpc_conn_t), target :: cli, srv
      type(my_writer_t), target :: impl
      class(rpc_server_t), pointer :: boot
      type(writer_client_t) :: client
      type(rpc_stream_t) :: stream
      type(capnp_message_t), target :: m
      type(writer_write_params_t) :: wparams
      type(writer_done_params_t) :: dparams
      type(writer_done_results_t) :: dresults
      type(payload_t) :: rawpl
      type(capnp_ptr_t) :: s
      integer(int8), allocatable :: chunk(:)
      integer(int64) :: qid, sent
      integer :: fda, fdb, err, i

      call px_socketpair(fda, fdb, err)
      boot => impl
      call rpc_conn_init(srv, fdb, boot)
      boot => null()
      call rpc_conn_init(cli, fda, boot)

      call rpc_bootstrap_send(cli, client%cap, err)
      call rpc_pump_once(srv, err)
      call check_(error, err == CAPNP_OK, 'stream: bootstrap')

      ! Five chunks through a 2-byte window. Each call is larger than the
      ! window. The first two stay in flight, and later sends retire the
      ! oldest first. The server drains until quiet after each client
      ! action so that retirement can read the Return.
      call rpc_stream_init(stream, window=2)
      sent = 0_int64
      do i = 1, 5
         allocate (chunk(i*3))
         chunk = int(i, int8)
         call writer_write_begin(cli, client, m, wparams, qid, err)
         call writer_write_params_chunk_set(wparams, chunk, err)
         call rpc_stream_send(cli, stream, m, qid, err)
         call check_(error, err == CAPNP_OK, 'stream: send accepted')
         sent = sent + size(chunk, kind=int64)
         deallocate (chunk)
         call drain(srv)
      end do
      call rpc_stream_finish(cli, stream, err)
      call check_(error, err == CAPNP_OK, 'stream: window drains clean')
      call drain(srv)

      call writer_done_begin(cli, client, m, dparams, qid, err)
      call rpc_call_send(cli, m, err)
      call drain(srv)
      call writer_done_wait(cli, qid, dresults, err)
      call check_(error, err == CAPNP_OK, 'stream: done returns')
      call check_(error, writer_done_results_total_get(dresults) == sent, &
                  'stream: total equals bytes sent')

      ! Error propagation: a stream call to a bad method ordinal fails at
      ! finish, and later sends fail fast without touching the wire.
      call rpc_stream_init(stream, window=4)
      call rpc_call_begin(cli, client%cap, WRITER_INTERFACE_ID, 99, m, rawpl, qid, err)
      s = capnp_new_struct(m, 0, 1, err)
      call payload_content_set(rawpl, s, err)
      call rpc_stream_send(cli, stream, m, qid, err)
      call check_(error, err == CAPNP_OK, 'stream: bad call sent')
      call drain(srv)
      call rpc_stream_finish(cli, stream, err)
      call check_(error, err == RPC_ERR_EXCEPTION, 'stream: failure surfaces at finish')
      call drain(srv)
      call writer_write_begin(cli, client, m, wparams, qid, err)
      call rpc_stream_send(cli, stream, m, qid, err)
      call check_(error, err == RPC_ERR_EXCEPTION, 'stream: post-failure send fails fast')

      ! A 2-byte window is below the call size. The controller stays
      ! ready while in-flight bytes are within the largest body, so the
      ! second send leaves the first call outstanding. The third retires it.
      call rpc_stream_init(stream, window=2)
      call writer_write_begin(cli, client, m, wparams, qid, err)
      allocate (chunk(8))
      chunk = 1_int8
      call writer_write_params_chunk_set(wparams, chunk, err)
      call rpc_stream_send(cli, stream, m, qid, err)
      sent = qid
      call drain(srv)
      call writer_write_begin(cli, client, m, wparams, qid, err)
      call writer_write_params_chunk_set(wparams, chunk, err)
      call rpc_stream_send(cli, stream, m, qid, err)
      call check_(error, cli%questions(int(sent))%used, &
                  'stream: one message of slack keeps the first call')
      call check_(error, stream%nout == 2, 'stream: two calls are in flight')
      call drain(srv)
      call writer_write_begin(cli, client, m, wparams, qid, err)
      call writer_write_params_chunk_set(wparams, chunk, err)
      call rpc_stream_send(cli, stream, m, qid, err)
      deallocate (chunk)
      call check_(error, .not. cli%questions(int(sent))%used, &
                  'stream: the third send retires the oldest call')
      call check_(error, stream%nout == 2, 'stream: one call retired, two remain')
      call drain(srv)
      call rpc_stream_finish(cli, stream, err)
      call check_(error, err == CAPNP_OK, 'stream: retired window finishes')
      call drain(srv)

      call rpc_stream_init(stream, window=65536)
      call writer_write_begin(cli, client, m, wparams, qid, err)
      call rpc_stream_send(cli, stream, m, qid, err)
      sent = qid
      call rpc_stream_cancel(cli, stream, err)
      call check_(error, err == CAPNP_OK .and. cli%questions(int(sent))%canceled, &
                  'stream: cancel reserves the question')
      call writer_write_begin(cli, client, m, wparams, qid, err)
      call rpc_stream_send(cli, stream, m, qid, err)
      call check_(error, err == RPC_ERR_EXCEPTION .and. .not. cli%questions(int(qid))%used, &
                  'stream: send after cancel fails and releases its question')
      call drain(srv)
      call rpc_pump_once(cli, err)
      call check_(error, .not. cli%questions(int(sent))%used, &
                  'stream: the canceled return frees the question')

      call rpc_conn_close(cli)
      call rpc_conn_close(srv)
   contains
      !> Handle every message already on the wire, then stop.
      subroutine drain(conn)
         type(rpc_conn_t), intent(inout), target :: conn
         logical :: handled
         integer :: derr
         do
            call rpc_pump_poll(conn, 50, handled, derr)
            if (derr /= CAPNP_OK .or. .not. handled) exit
         end do
      end subroutine drain
   end subroutine run_stream

   !> Same socket send buffer as the C++ probe (setsockopt 8192, reported
   !> back as 16384 here). The server reads each call and does not
   !> answer. The send count is the C++ window formula; on this host
   !> that is 115 calls of 1 byte and 5 calls of 4000 bytes.
   subroutine run_stream_window(error)
      type(error_type), allocatable, intent(out) :: error
      call window_case(error, 1)
      if (allocated(error)) return
      call window_case(error, 4000)
   end subroutine run_stream_window

   subroutine window_case(error, nbytes)
      type(error_type), allocatable, intent(inout) :: error
      integer, intent(in) :: nbytes
      type(rpc_conn_t), target :: cli, srv
      type(hold_writer_t), target :: impl
      class(rpc_server_t), pointer :: boot
      type(writer_client_t) :: client
      type(rpc_stream_t) :: stream
      type(capnp_message_t), target :: m
      type(writer_write_params_t) :: wparams
      integer(int8), allocatable :: chunk(:)
      integer(int64) :: qid, first, body, inflight, maxs
      integer :: fda, fdb, err, snd, nsent, expect
      logical :: handled
      character(len=120) :: label
      if (allocated(error)) return
      call px_socketpair(fda, fdb, err)
      call check_(error, err == CAPNP_OK, 'window: socketpair')
      if (allocated(error)) return
      call set_sndbuf(fda, 8192, snd, err)
      call check_(error, err == CAPNP_OK .and. snd > 0, 'window: SO_SNDBUF')
      if (allocated(error)) then
         call px_close(fda)
         call px_close(fdb)
         return
      end if
      boot => impl
      call rpc_conn_init(srv, fdb, boot)
      boot => null()
      call rpc_conn_init(cli, fda, boot)
      call rpc_bootstrap_send(cli, client%cap, err)
      call rpc_pump_poll(srv, 1000, handled, err)
      call check_(error, err == CAPNP_OK .and. handled, 'window: bootstrap')
      if (allocated(error)) then
         call rpc_conn_close(cli)
         call rpc_conn_close(srv)
         return
      end if
      call rpc_stream_init(stream)
      allocate (chunk(nbytes))
      chunk = 1_int8
      body = -1_int64
      inflight = 0_int64
      maxs = 0_int64
      first = -1_int64
      nsent = 0
      do
         if (nsent > 0) then
            if (.not. (inflight <= maxs .or. &
                       inflight < int(stream%window, int64) + maxs)) exit
         end if
         if (nsent >= 1000000) exit
         call writer_write_begin(cli, client, m, wparams, qid, err)
         if (err == CAPNP_OK) call writer_write_params_chunk_set(wparams, chunk, err)
         if (err /= CAPNP_OK) then
            call check_(error, .false., 'window: begin')
            exit
         end if
         if (body < 0_int64) then
            body = capnp_message_nbytes(m)
            first = qid
         end if
         call check_(error, capnp_message_nbytes(m) == body, 'window: stable body')
         if (allocated(error)) exit
         call rpc_stream_send(cli, stream, m, qid, err)
         if (err /= CAPNP_OK) then
            call check_(error, .false., 'window: send')
            exit
         end if
         call rpc_pump_poll(srv, 1000, handled, err)
         if (err /= CAPNP_OK .or. .not. handled) then
            call check_(error, .false., 'window: server read')
            exit
         end if
         if (body > maxs) maxs = body
         inflight = inflight + body
         nsent = nsent + 1
         if (nsent == 1) then
            call check_(error, stream%window == snd, 'window: window is SO_SNDBUF')
            if (allocated(error)) exit
         end if
      end do
      if (.not. allocated(error)) then
         expect = cpp_send_count(int(stream%window, int64), body)
         write (label, '(a,i0,a,i0,a,i0,a,i0,a,i0)') 'window: nbytes ', nbytes, &
            ' body ', body, ' sent ', nsent, ' expected ', expect, ' snd ', snd
         call check_(error, stream%window == snd, 'window: socket buffer sticks')
         call check_(error, stream%nout == nsent, 'window: every send is in flight')
         call check_(error, stream%used_bytes == inflight, 'window: body bytes in flight')
         call check_(error, nsent == expect, label)
         call rpc_deferred_return(srv, first, err)
         call check_(error, err == CAPNP_OK, 'window: oldest return')
      end if
      if (.not. allocated(error)) then
         call writer_write_begin(cli, client, m, wparams, qid, err)
         if (err == CAPNP_OK) call writer_write_params_chunk_set(wparams, chunk, err)
         if (err == CAPNP_OK) call rpc_stream_send(cli, stream, m, qid, err)
         call check_(error, err == CAPNP_OK, 'window: send at the limit')
         call check_(error, .not. cli%questions(int(first))%used, &
                     'window: the limit retires the oldest call')
         call check_(error, stream%nout == nsent, 'window: depth stays at the limit')
      end if
      deallocate (chunk)
      call rpc_conn_close(cli)
      call rpc_conn_close(srv)
   end subroutine window_case

   pure function cpp_send_count(window, msg) result(n)
      integer(int64), intent(in) :: window, msg
      integer :: n
      integer(int64) :: inflight, maxs
      inflight = 0_int64
      maxs = 0_int64
      n = 0
      if (msg <= 0_int64) return
      do
         if (n > 0) then
            if (.not. (inflight <= maxs .or. inflight < window + maxs)) exit
         end if
         if (msg > maxs) maxs = msg
         inflight = inflight + msg
         n = n + 1
         if (n >= 1000000) exit
      end do
   end function cpp_send_count

   subroutine set_sndbuf(fd, bytes, actual, err)
      integer, intent(in) :: fd, bytes
      integer, intent(out) :: actual, err
      integer(c_int), target :: buf, optlen
      err = CAPNP_OK
      actual = -1
      buf = int(bytes, c_int)
      if (c_setsockopt(int(fd, c_int), c_sol_socket, c_so_sndbuf, &
                       c_loc(buf), 4_c_int) /= 0_c_int) then
         err = CAPNP_ERR_IO
         return
      end if
      buf = 0_c_int
      optlen = 4_c_int
      if (c_getsockopt(int(fd, c_int), c_sol_socket, c_so_sndbuf, &
                       c_loc(buf), c_loc(optlen)) /= 0_c_int .or. buf <= 0_c_int) then
         err = CAPNP_ERR_IO
         return
      end if
      actual = int(buf)
   end subroutine set_sndbuf

   subroutine hold_write(self, ctx, params, results, err)
      class(hold_writer_t), intent(inout) :: self
      type(rpc_call_ctx_t), intent(inout) :: ctx
      type(writer_write_params_t), intent(in) :: params
      type(stream_result_t), intent(in) :: results
      integer, intent(out) :: err
      associate (unused_self => self, unused_params => params, unused_results => results)
      end associate
      err = CAPNP_OK
      call rpc_ctx_defer(ctx)
   end subroutine hold_write

   subroutine hold_done(self, ctx, params, results, err)
      class(hold_writer_t), intent(inout) :: self
      type(rpc_call_ctx_t), intent(inout) :: ctx
      type(writer_done_params_t), intent(in) :: params
      type(writer_done_results_t), intent(in) :: results
      integer, intent(out) :: err
      associate (unused_self => self, unused_ctx => ctx, unused_params => params, &
                 unused_results => results)
      end associate
      err = CAPNP_OK
   end subroutine hold_done

end module test_stream
