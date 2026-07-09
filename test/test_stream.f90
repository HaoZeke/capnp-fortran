!> `-> stream` methods: windowed client-side flow control over the
!> ordinary Call/Return wire traffic, plus the streaming error rule
!> (first failure surfaces at finish; later sends fail fast).
module test_stream
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed, skip_test
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
      testsuite = [new_unittest("stream", run_stream)]
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

      ! Five chunks of growing size through a window of 2. The server
      ! drains until quiet after each client action: retiring a window
      ! slot emits a Finish alongside the next Call, so fixed one-pump
      ! accounting would starve.
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
