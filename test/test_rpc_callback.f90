!> A capability hosted by the calling vat, passed in call parameters
!> (rpc_call_export_cap): the callee keeps it and calls back into the
!> caller, and releasing it frees the caller's export.
module test_rpc_callback
   use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_loc
   use testdrive, only: unittest_type, error_type, check
   use capnp
   use rpc_capnp
   use capnp_posix
   use capnp_rpc
   implicit none
   private

   public :: collect_rpc_callback

   integer(int64), parameter :: KEEPER_IFACE = 515151_int64
   integer(int64), parameter :: ECHO_IFACE = 525252_int64

   !> Hosted by the caller: answers echo(n) with n + 1 and counts calls.
   type, extends(rpc_server_t) :: echo_srv_t
      integer :: calls = 0
   contains
      procedure :: dispatch => echo_dispatch
   end type echo_srv_t

   !> Hosted by the callee: keep(cap) remembers the capability it was given.
   type, extends(rpc_server_t) :: keeper_srv_t
      type(rpc_cap_t) :: kept
      logical :: have = .false.
   contains
      procedure :: dispatch => keeper_dispatch
   end type keeper_srv_t

   !> The callee's connection (used by its dispatch) and the caller's.
   type(rpc_conn_t), target :: callee, cli_g

contains

   subroutine collect_rpc_callback(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      allocate (testsuite(2))
      testsuite(1)%name = "capability in call parameters is called back"
      testsuite(1)%test => run_callback
      testsuite(2)%name = "connected and accepted TCP sockets have TCP_NODELAY"
      testsuite(2)%test => run_nodelay
   end subroutine collect_rpc_callback

   subroutine echo_dispatch(self, ctx, err)
      class(echo_srv_t), intent(inout) :: self
      type(rpc_call_ctx_t), intent(inout) :: ctx
      integer, intent(out) :: err
      type(capnp_ptr_t) :: r
      integer(int32) :: n
      err = CAPNP_OK
      if (ctx%interface_id /= ECHO_IFACE .or. ctx%method_id /= 0) then
         err = CAPNP_ERR_ARG
         return
      end if
      n = capnp_get_i32(ctx%params, 0_int64)
      self%calls = self%calls + 1
      r = capnp_new_struct(ctx%rmsg, 1, 0, err)
      if (err == CAPNP_OK) call capnp_set_i32(r, 0_int64, n + 1_int32, err)
      if (err == CAPNP_OK) call payload_content_set(ctx%results, r, err)
   end subroutine echo_dispatch

   !> keep(cap): the capability in pointer 0 of the parameters becomes an
   !> import of the callee's connection.
   subroutine keeper_dispatch(self, ctx, err)
      class(keeper_srv_t), intent(inout) :: self
      type(rpc_call_ctx_t), intent(inout) :: ctx
      integer, intent(out) :: err
      type(capnp_ptr_t) :: q, r
      err = CAPNP_OK
      if (ctx%interface_id /= KEEPER_IFACE .or. ctx%method_id /= 0) then
         err = CAPNP_ERR_ARG
         return
      end if
      q = capnp_getp(ctx%params, 0, err)
      if (err /= CAPNP_OK) return
      if (q%kind /= CAPNP_PK_CAP) then
         err = CAPNP_ERR_KIND
         return
      end if
      call rpc_ctx_keep_param_cap(callee, ctx, q%capidx, self%kept, err)
      if (err /= CAPNP_OK) return
      self%have = .true.
      r = capnp_new_struct(ctx%rmsg, 0, 0, err)
      if (err == CAPNP_OK) call payload_content_set(ctx%results, r, err)
   end subroutine keeper_dispatch

   subroutine run_callback(error)
      type(error_type), allocatable, intent(out) :: error
      type(keeper_srv_t), target :: keeper
      type(echo_srv_t), target :: echo
      class(rpc_server_t), pointer :: boot, ep
      type(rpc_cap_t) :: kcap
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(capnp_ptr_t) :: s, content
      integer(int64) :: qid
      integer :: fda, fdb, err, eid, i
      integer(int32) :: got
      logical :: handled, ok
      call px_socketpair(fda, fdb, err)
      call check(error, err == CAPNP_OK, 'socketpair')
      if (allocated(error)) return
      boot => keeper
      call rpc_conn_init(callee, fdb, boot)
      boot => null()
      call rpc_conn_init(cli_g, fda, boot)
      call rpc_bootstrap_send(cli_g, kcap, err)
      call rpc_pump_once(callee, err)
      call rpc_wait(cli_g, kcap%id, err)
      call rpc_result_cap(cli_g, kcap%id, [integer ::], kcap, err)
      call check(error, err == CAPNP_OK, 'bootstrap settled')
      if (allocated(error)) return

      ! keep(echo): the echo server rides in the parameters.
      call rpc_call_begin(cli_g, kcap, KEEPER_IFACE, 0, m, params, qid, err)
      s = capnp_new_struct(m, 0, 1, err)
      call capnp_setp(s, 0, rpc_make_cap_ptr(m, 0), err)
      call payload_content_set(params, s, err)
      ep => echo
      call rpc_call_export_cap(cli_g, m, ep, err)
      call check(error, err == CAPNP_OK, 'rpc_call_export_cap')
      if (allocated(error)) return
      call rpc_call_export_cap(cli_g, m, ep, err)
      call check(error, err == CAPNP_ERR_ARG, 'a second capability in one call is refused')
      if (allocated(error)) return
      call rpc_call_send(cli_g, m, err)
      call pump_until(.false., qid, ok)
      call check(error, ok .and. keeper%have, 'callee kept the capability')
      if (allocated(error)) return
      call rpc_finish_send(cli_g, qid, .false., err)
      eid = -1
      do i = 0, size(cli_g%exports) - 1
         if (cli_g%exports(i)%used) then
            if (associated(cli_g%exports(i)%srv, ep)) eid = i
         end if
      end do
      call check(error, eid >= 0 .and. keeper%kept%kind == RPC_CAP_IMPORT, &
                 'the caller exports the echo server and the callee holds an import of it')
      if (allocated(error)) return

      ! The callee calls back into the caller, three times.
      do i = 1, 3
         call rpc_call_begin(callee, keeper%kept, ECHO_IFACE, 0, m, params, qid, err)
         s = capnp_new_struct(m, 1, 0, err)
         call capnp_set_i32(s, 0_int64, int(10 * i, int32), err)
         call payload_content_set(params, s, err)
         call rpc_call_send(callee, m, err)
         call pump_until(.true., qid, ok)
         call check(error, ok, 'callback returned (caller: '//trim(rpc_conn_reason(cli_g))// &
                    '; callee: '//trim(rpc_conn_reason(callee))//'; echo calls '//achar(48 + echo%calls)//')')
         if (allocated(error)) return
         call rpc_result_content(callee, qid, content, err)
         got = capnp_get_i32(content, 0_int64)
         call check(error, err == CAPNP_OK .and. got == int(10 * i + 1, int32), 'callback answered n + 1')
         if (allocated(error)) return
         call rpc_finish_send(callee, qid, .false., err)
      end do
      call check(error, echo%calls == 3, 'the caller-hosted server ran three times')
      if (allocated(error)) return

      ! Releasing it frees the caller's export.
      call rpc_release_send(callee, keeper%kept, err)
      call check(error, err == CAPNP_OK, 'release sent')
      do i = 1, 5
         call rpc_pump_poll(cli_g, 20, handled, err)
         if (cli_g%exports(eid)%refcount <= 0) exit
      end do
      call check(error, cli_g%exports(eid)%refcount <= 0, 'release reached the caller')
      call rpc_conn_close(callee)
      call rpc_conn_close(cli_g)
   end subroutine run_callback

   !> Pumps both connections until question `qid` of the caller (or, with
   !> by_callee, of the callee) has returned, for at most two seconds.
   subroutine pump_until(by_callee, qid, ok)
      logical, intent(in) :: by_callee
      integer(int64), intent(in) :: qid
      logical, intent(out) :: ok
      logical :: handled
      integer :: k, e
      ok = .false.
      do k = 1, 200
         call rpc_pump_poll(cli_g, 5, handled, e)
         call rpc_pump_poll(callee, 5, handled, e)
         if (by_callee) then
            ok = rpc_question_returned(callee, qid)
         else
            ok = rpc_question_returned(cli_g, qid)
         end if
         if (ok) then
            ok = .true.
            return
         end if
      end do
   end subroutine pump_until

   subroutine run_nodelay(error)
      type(error_type), allocatable, intent(out) :: error
      interface
         function c_getsockopt(fd, level, name, val, len) bind(c, name='getsockopt') result(rc)
            import :: c_int, c_ptr
            integer(c_int), value :: fd, level, name
            type(c_ptr), value :: val, len
            integer(c_int) :: rc
         end function c_getsockopt
      end interface
      integer(c_int), parameter :: IPPROTO_TCP = 6_c_int, TCP_NODELAY = 1_c_int
      integer(c_int), target :: v, vlen
      integer :: lfd, cfd, afd, port, err
      call px_tcp_listen(0, lfd, err)
      call check(error, err == CAPNP_OK, 'listen')
      if (allocated(error)) return
      call px_tcp_bound_port(lfd, port, err)
      call px_tcp_connect_host('127.0.0.1', port, cfd, err)
      call check(error, err == CAPNP_OK, 'connect')
      if (allocated(error)) return
      call px_tcp_accept(lfd, afd, err)
      call check(error, err == CAPNP_OK, 'accept')
      if (allocated(error)) return
      v = 0; vlen = 4
      call check(error, c_getsockopt(int(cfd, c_int), IPPROTO_TCP, TCP_NODELAY, c_loc(v), c_loc(vlen)) == 0 &
                 .and. v /= 0, 'connected socket has TCP_NODELAY')
      if (allocated(error)) return
      v = 0; vlen = 4
      call check(error, c_getsockopt(int(afd, c_int), IPPROTO_TCP, TCP_NODELAY, c_loc(v), c_loc(vlen)) == 0 &
                 .and. v /= 0, 'accepted socket has TCP_NODELAY')
      call px_close(cfd)
      call px_close(afd)
      call px_close(lfd)
   end subroutine run_nodelay

end module test_rpc_callback
