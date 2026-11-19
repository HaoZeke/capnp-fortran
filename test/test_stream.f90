!> `-> stream` methods: windowed client-side flow control over the
!> ordinary Call/Return wire traffic, plus the streaming error rule
!> (first failure surfaces at finish; later sends fail fast).
module test_stream
   use testdrive, only: unittest_type, error_type, check, test_failed, skip_test
   use capnp
   use streamer_capnp
   use capnp_posix
   use capnp_rpc
   use rpc_writer_impl
   implicit none

   private
   public :: collect_stream

contains

   subroutine collect_stream(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      allocate (testsuite(1))
      testsuite(1)%name = "stream"
      testsuite(1)%test => run_stream
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

      ! Five chunks through a 2-byte window. The first call is sent even
      ! though its frame is larger than the window; every later call
      ! retires the oldest before it is sent. The server drains until
      ! quiet after each client action so that retirement can read the
      ! Return.
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

      ! A 2-byte window retires before the second send, so the first
      ! question is finished rather than left in flight beside the new one.
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
      deallocate (chunk)
      call check_(error, .not. cli%questions(int(sent))%used, &
                  'stream: byte window retires the first call')
      call check_(error, stream%nout == 1, 'stream: only the new call is in flight')
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

end module test_stream
