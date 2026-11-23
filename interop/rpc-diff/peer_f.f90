!> Fortran peer for the RPC differential checker.
!> Usage: peer_f <port>
!> Serves diff.capnp Target as its bootstrap, the same contract as
!> peer_cpp.c++. slow defers its answer until the driver's Callback.ping
!> returns, as the C++ server returns a promise.
module diff_peer
   use capnp
   use rpc_capnp
   use capnp_rpc
   implicit none
   private

   public :: target_srv_t, g_conn, g_cb, TARGET_IFACE, CALLBACK_IFACE
   public :: settle_slow

   integer(int64), parameter :: TARGET_IFACE = -3314929971793887215_int64   ! 0xd1ff00c0ffee0011
   integer(int64), parameter :: CALLBACK_IFACE = -3314929971793887214_int64 ! 0xd1ff00c0ffee0012

   type(rpc_conn_t), pointer :: g_conn => null()
   type(rpc_cap_t) :: g_cb

   !> A deferred slow call and the ping question it waits on.
   type :: slow_wait_t
      integer(int64) :: answer = -1_int64
      integer(int64) :: ping = -1_int64
      logical :: make = .false.
   end type slow_wait_t
   type(slow_wait_t), allocatable :: g_slow(:)

   type, extends(rpc_server_t) :: target_srv_t
      integer(int64) :: base = 0_int64
      class(rpc_server_t), pointer :: me => null()
   contains
      procedure :: dispatch => target_dispatch
   end type target_srv_t

contains

   recursive subroutine target_dispatch(self, ctx, err)
      class(target_srv_t), intent(inout) :: self
      type(rpc_call_ctx_t), intent(inout) :: ctx
      integer, intent(out) :: err
      type(capnp_ptr_t) :: s
      integer(int64) :: n
      integer :: idx, i
      err = CAPNP_OK
      if (ctx%interface_id /= TARGET_IFACE) then
         call rpc_ctx_unimplemented_interface(ctx, 'diff.capnp:Target', err)
         return
      end if
      n = capnp_get_i64(ctx%params, 0_int64)
      select case (ctx%method_id)
      case (0)
         call set_n(ctx, n + self%base, err)
      case (1)
         call rpc_ctx_fail(ctx, 'SRVEXC', err)
      case (2)
         s = capnp_new_struct(ctx%rmsg, 0, 1, err)
         if (err /= CAPNP_OK) return
         call put_new_target(ctx, s, 0, n, err)
         if (err == CAPNP_OK) call payload_content_set(ctx%results, s, err)
      case (3)
         call slow_call(ctx, n, .false., err)
      case (7)
         call slow_call(ctx, n, .true., err)
      case (4)
         idx = rpc_ctx_export_cap(ctx, self%me, err)
         if (err /= CAPNP_OK) return
         s = capnp_new_struct(ctx%rmsg, 0, 1, err)
         if (err == CAPNP_OK) call capnp_setp(s, 0, rpc_make_cap_ptr(ctx%rmsg, idx), err)
         if (err == CAPNP_OK) call payload_content_set(ctx%results, s, err)
      case (5)
         call reflect_driver(ctx, err)
      case (6)
         s = capnp_new_struct(ctx%rmsg, 0, 20, err)
         if (err /= CAPNP_OK) return
         do i = 0, int(min(max(n, 0_int64), 20_int64)) - 1
            call put_new_target(ctx, s, i, int(i, int64), err)
            if (err /= CAPNP_OK) return
         end do
         call payload_content_set(ctx%results, s, err)
      case default
         call rpc_ctx_unimplemented_method(ctx, 'diff.capnp:Target', TARGET_IFACE, err)
      end select
   end subroutine target_dispatch

   subroutine set_n(ctx, n, err)
      type(rpc_call_ctx_t), intent(inout) :: ctx
      integer(int64), intent(in) :: n
      integer, intent(out) :: err
      type(capnp_ptr_t) :: s
      s = capnp_new_struct(ctx%rmsg, 1, 0, err)
      if (err /= CAPNP_OK) return
      call capnp_set_i64(s, 0_int64, n, err)
      if (err == CAPNP_OK) call payload_content_set(ctx%results, s, err)
   end subroutine set_n

   !> Pointer field `field` of `s` gets a new owned Target with `base`.
   subroutine put_new_target(ctx, s, field, base, err)
      type(rpc_call_ctx_t), intent(inout) :: ctx
      type(capnp_ptr_t), intent(in) :: s
      integer, intent(in) :: field
      integer(int64), intent(in) :: base
      integer, intent(out) :: err
      type(target_srv_t), pointer :: t
      class(rpc_server_t), pointer :: sp
      integer :: idx
      allocate (t)
      t%base = base
      t%me => t
      sp => t
      idx = rpc_ctx_export_cap(ctx, sp, err, owned=.true.)
      if (err /= CAPNP_OK) then
         deallocate (t)
         return
      end if
      call capnp_setp(s, field, rpc_make_cap_ptr(ctx%rmsg, idx), err)
   end subroutine put_new_target

   !> The driver's Callback in the results, written as C++ writes an
   !> import of the calling connection: receiverHosted.
   subroutine reflect_driver(ctx, err)
      type(rpc_call_ctx_t), intent(inout) :: ctx
      integer, intent(out) :: err
      type(capnp_ptr_t) :: ctab, s
      type(cap_descriptor_t) :: cd
      ctab = payload_cap_table_init(ctx%results, 1_int64, err)
      if (err /= CAPNP_OK) return
      cd%p = capnp_list_get_struct(ctab, 0, err)
      if (err /= CAPNP_OK) return
      call cap_descriptor_receiver_hosted_set(cd, g_cb%id, err)
      if (err /= CAPNP_OK) return
      s = capnp_new_struct(ctx%rmsg, 0, 1, err)
      if (err == CAPNP_OK) call capnp_setp(s, 0, rpc_make_cap_ptr(ctx%rmsg, 0), err)
      if (err == CAPNP_OK) call payload_content_set(ctx%results, s, err)
   end subroutine reflect_driver

   !> Ping the driver. Once it returns, settle_slow answers with its n,
   !> or with a new Target whose base is that n when `make` is set.
   subroutine slow_call(ctx, n, make, err)
      type(rpc_call_ctx_t), intent(inout) :: ctx
      integer(int64), intent(in) :: n
      logical, intent(in) :: make
      integer, intent(out) :: err
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(capnp_ptr_t) :: s
      integer(int64) :: q
      type(message_t) :: msg
      type(call_t) :: c
      call rpc_call_begin(g_conn, g_cb, CALLBACK_IFACE, 0, m, params, q, err)
      if (err /= CAPNP_OK) return
      ! C++ generated code sets this hint when the results cannot hold
      ! capabilities, as ping's cannot.
      msg = message_read_root(m, err)
      if (err == CAPNP_OK) c = message_call_get(msg, err)
      if (err == CAPNP_OK) call call_no_promise_pipelining_set(c, .true., err)
      if (err /= CAPNP_OK) return
      s = capnp_new_struct(m, 1, 0, err)
      if (err == CAPNP_OK) call capnp_set_i64(s, 0_int64, n, err)
      if (err == CAPNP_OK) call payload_content_set(params, s, err)
      if (err /= CAPNP_OK) return
      call rpc_call_send(g_conn, m, err)
      if (err /= CAPNP_OK) return
      call rpc_ctx_defer(ctx)
      if (.not. allocated(g_slow)) allocate (g_slow(0))
      g_slow = [g_slow, slow_wait_t(ctx%answer_id, q, make)]
   end subroutine slow_call

   !> Answer each deferred slow call whose ping has returned, oldest
   !> first. The ping is finished before the answer, as in C++, where
   !> the response goes before the server's promise resolves.
   recursive subroutine settle_slow(conn)
      type(rpc_conn_t), intent(inout), target :: conn
      type(rpc_call_ctx_t), pointer :: ctx
      type(capnp_ptr_t) :: s, content
      type(slow_wait_t) :: w
      integer(int64) :: got
      integer :: i, err, e, ping_type
      if (.not. allocated(g_slow)) return
      i = 1
      do while (i <= size(g_slow))
         if (.not. rpc_question_returned(conn, g_slow(i)%ping)) then
            i = i + 1
            cycle
         end if
         w = g_slow(i)
         g_slow = [g_slow(:i - 1), g_slow(i + 1:)]
         call rpc_result_content(conn, w%ping, content, err, ping_type)
         if (err == CAPNP_OK) got = capnp_get_i64(content, 0_int64)
         call rpc_finish_send(conn, w%ping, .false., e)
         ctx => rpc_deferred_ctx(conn, w%answer)
         if (.not. associated(ctx)) cycle
         if (err == CAPNP_OK) then
            if (w%make) then
               s = capnp_new_struct(ctx%rmsg, 0, 1, err)
               if (err == CAPNP_OK) call put_new_target(ctx, s, 0, got, err)
               if (err == CAPNP_OK) call payload_content_set(ctx%results, s, err)
            else
               call set_n(ctx, got, err)
            end if
         end if
         if (err == RPC_ERR_EXCEPTION) then
            call rpc_deferred_return(conn, w%answer, e, failed=.true., &
                                     reason='remote exception: '//rpc_conn_reason(conn), &
                                     exc_type=ping_type)
         else
            call rpc_deferred_return(conn, w%answer, e, failed=err /= CAPNP_OK)
         end if
      end do
   end subroutine settle_slow

end module diff_peer

program peer_f
   use capnp
   use capnp_posix
   use capnp_rpc
   use diff_peer
   implicit none
   type(rpc_conn_t), target :: conn
   type(target_srv_t), pointer :: boot
   class(rpc_server_t), pointer :: bootp
   character(len=16) :: arg
   integer(int64) :: q
   integer :: port, lfd, fd, err

   call get_command_argument(1, arg)
   read (arg, *) port
   call px_tcp_listen(port, lfd, err)
   if (err /= CAPNP_OK) error stop 'peer_f: listen failed'
   print '(a)', 'READY'
   flush (6)
   call px_tcp_accept(lfd, fd, err)
   if (err /= CAPNP_OK) error stop 'peer_f: accept failed'
   call px_close(lfd)

   allocate (boot)
   boot%me => boot
   bootp => boot
   call rpc_conn_init(conn, fd, bootp)
   g_conn => conn

   call rpc_bootstrap_send(conn, g_cb, err)
   if (err == CAPNP_OK) then
      q = g_cb%id
      call rpc_wait(conn, q, err)
      if (err == CAPNP_OK) call rpc_result_cap(conn, q, [integer ::], g_cb, err)
      call rpc_finish_send(conn, q, .true., err)
   end if
   do
      call rpc_pump_once(conn, err)
      if (err /= CAPNP_OK) exit
      call settle_slow(conn)
   end do
   call rpc_conn_close(conn)
   deallocate (boot)
end program peer_f
