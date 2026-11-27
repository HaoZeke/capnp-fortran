!> Membrane redirect: a policy may hand a call to another capability.
!> Pass-through results stay wrapped. Redirect results stay as the
!> target capability returned them (capnp membrane.h lines 69-71).
module test_membrane_redirect
   use testdrive, only: unittest_type, error_type, check
   use capnp
   use rpc_capnp
   use capnp_posix
   use capnp_rpc
   implicit none
   private
   public :: collect_membrane_redirect

   type, extends(rpc_server_t) :: plus_srv_t
      integer :: add0 = 1
      integer :: add1 = 100
      integer :: add2 = 1000
      integer :: hits(0:3) = 0
      type(plus_srv_t), pointer :: child => null()
   contains
      procedure :: dispatch => plus_dispatch
   end type plus_srv_t

   type, extends(rpc_membrane_policy_t) :: redir_policy_t
      class(rpc_server_t), pointer :: alt => null()
      type(rpc_cap_t) :: alt_cap
      logical :: use_cap = .false.
   contains
      procedure :: inbound_call => redir_check
      procedure :: outbound_call => redir_check
      procedure :: inbound_redirect => redir_go
      procedure :: outbound_redirect => redir_go
   end type redir_policy_t

   type, extends(rpc_membrane_reason_policy_t) :: reason_policy_t
      class(rpc_server_t), pointer :: alt => null()
      type(rpc_cap_t) :: alt_cap
      logical :: use_cap = .false.
      logical :: deny1 = .false.
   contains
      procedure :: inbound_call => reason_check
      procedure :: outbound_call => reason_check
      procedure :: inbound_redirect => reason_go
      procedure :: outbound_redirect => reason_go
   end type reason_policy_t

contains

   subroutine collect_membrane_redirect(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      allocate (testsuite(8))
      testsuite(1)%name = "membrane-redirect-inbound"
      testsuite(1)%test => run_inbound
      testsuite(2)%name = "membrane-redirect-outbound-server"
      testsuite(2)%test => run_outbound_server
      testsuite(3)%name = "membrane-redirect-outbound-client"
      testsuite(3)%test => run_outbound_client
      testsuite(4)%name = "membrane-redirect-after-revoke"
      testsuite(4)%test => run_after_revoke
      testsuite(5)%name = "membrane-redirect-revoked-chain"
      testsuite(5)%test => run_revoked_chain
      testsuite(6)%name = "membrane-redirect-chain-deny"
      testsuite(6)%test => run_chain_deny
      testsuite(7)%name = "membrane-redirect-chain-revoked"
      testsuite(7)%test => run_chain_revoked
      testsuite(8)%name = "membrane-redirect-chain-server"
      testsuite(8)%test => run_chain_server
   end subroutine collect_membrane_redirect

   subroutine check_(error, cond, name)
      type(error_type), allocatable, intent(inout) :: error
      logical, intent(in) :: cond
      character(len=*), intent(in) :: name
      if (allocated(error)) return
      call check(error, cond, name)
   end subroutine check_

   !> Stop after a fixed number of polls so a missing Return fails the test.
   subroutine wait_bounded(conn, qid, err)
      type(rpc_conn_t), intent(inout) :: conn
      integer(int64), intent(in) :: qid
      integer, intent(out) :: err
      integer :: i, q
      logical :: handled
      err = CAPNP_OK
      if (.not. allocated(conn%questions)) then
         err = CAPNP_ERR_BOUNDS
         return
      end if
      if (qid < 0_int64 .or. qid >= int(size(conn%questions), int64)) then
         err = CAPNP_ERR_BOUNDS
         return
      end if
      q = int(qid)
      if (.not. conn%questions(q)%used) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (conn%questions(q)%canceled) then
         conn%abort_reason = 'call canceled'
         err = RPC_ERR_EXCEPTION
         return
      end if
      do i = 1, 40
         if (conn%dead) then
            err = RPC_ERR_DEAD
            return
         end if
         if (conn%questions(q)%returned) return
         call rpc_pump_poll(conn, 25, handled, err)
         if (err /= CAPNP_OK) return
      end do
      if (.not. conn%questions(q)%returned) err = CAPNP_ERR_BOUNDS
   end subroutine wait_bounded

   subroutine plus_dispatch(self, ctx, err)
      class(plus_srv_t), intent(inout) :: self
      type(rpc_call_ctx_t), intent(inout) :: ctx
      integer, intent(out) :: err
      type(capnp_ptr_t) :: s
      integer(int64) :: n, add
      class(rpc_server_t), pointer :: sp
      integer :: idx
      err = CAPNP_OK
      if (ctx%method_id >= 0 .and. ctx%method_id <= 3) &
         self%hits(ctx%method_id) = self%hits(ctx%method_id) + 1
      if (ctx%method_id == 3) then
         if (.not. associated(self%child)) then
            err = CAPNP_ERR_ARG
            return
         end if
         s = capnp_new_struct(ctx%rmsg, 1, 1, err)
         if (err /= CAPNP_OK) return
         call capnp_set_i64(s, 0_int64, 0_int64, err)
         sp => self%child
         idx = rpc_ctx_export_cap(ctx, sp, err)
         if (err == CAPNP_OK) call capnp_setp(s, 0, rpc_make_cap_ptr(ctx%rmsg, idx), err)
         if (err == CAPNP_OK) call payload_content_set(ctx%results, s, err)
         return
      end if
      n = capnp_get_i64(ctx%params, 0_int64)
      select case (ctx%method_id)
      case (0); add = int(self%add0, int64)
      case (1); add = int(self%add1, int64)
      case (2); add = int(self%add2, int64)
      case default
         err = CAPNP_ERR_ARG
         return
      end select
      if (ctx%method_id == 1 .and. associated(self%child)) then
         s = capnp_new_struct(ctx%rmsg, 1, 1, err)
         if (err /= CAPNP_OK) return
         call capnp_set_i64(s, 0_int64, n + add, err)
         sp => self%child
         idx = rpc_ctx_export_cap(ctx, sp, err)
         if (err == CAPNP_OK) call capnp_setp(s, 0, rpc_make_cap_ptr(ctx%rmsg, idx), err)
         if (err == CAPNP_OK) call payload_content_set(ctx%results, s, err)
         return
      end if
      s = capnp_new_struct(ctx%rmsg, 1, 0, err)
      if (err /= CAPNP_OK) return
      call capnp_set_i64(s, 0_int64, n + add, err)
      if (err == CAPNP_OK) call payload_content_set(ctx%results, s, err)
   end subroutine plus_dispatch

   subroutine redir_check(self, interface_id, method_id, err)
      class(redir_policy_t), intent(inout) :: self
      integer(int64), intent(in) :: interface_id
      integer, intent(in) :: method_id
      integer, intent(out) :: err
      associate (unused_self => self)
      end associate
      associate (unused_interface_id => interface_id)
      end associate
      if (method_id == 2) then
         err = RPC_ERR_EXCEPTION
      else
         err = CAPNP_OK
      end if
   end subroutine redir_check

   subroutine redir_go(self, interface_id, method_id, server, cap)
      class(redir_policy_t), intent(inout) :: self
      integer(int64), intent(in) :: interface_id
      integer, intent(in) :: method_id
      class(rpc_server_t), pointer, intent(inout) :: server
      type(rpc_cap_t), intent(inout) :: cap
      server => null()
      cap = rpc_cap_t()
      if (method_id /= 1) return
      if (interface_id < 0_int64) return
      if (self%use_cap) then
         cap = self%alt_cap
      else if (associated(self%alt)) then
         server => self%alt
      end if
   end subroutine redir_go

   subroutine reason_check(self, interface_id, method_id, err)
      class(reason_policy_t), intent(inout) :: self
      integer(int64), intent(in) :: interface_id
      integer, intent(in) :: method_id
      integer, intent(out) :: err
      associate (unused_interface_id => interface_id)
      end associate
      if (method_id == 2 .or. (self%deny1 .and. method_id == 1)) then
         self%reject_reason = 'nope denied'
         err = RPC_ERR_EXCEPTION
      else
         err = CAPNP_OK
      end if
   end subroutine reason_check

   subroutine reason_go(self, interface_id, method_id, server, cap)
      class(reason_policy_t), intent(inout) :: self
      integer(int64), intent(in) :: interface_id
      integer, intent(in) :: method_id
      class(rpc_server_t), pointer, intent(inout) :: server
      type(rpc_cap_t), intent(inout) :: cap
      server => null()
      cap = rpc_cap_t()
      if (method_id /= 1) return
      if (interface_id < 0_int64) return
      if (self%use_cap) then
         cap = self%alt_cap
      else if (associated(self%alt)) then
         server => self%alt
      end if
   end subroutine reason_go

   subroutine run_call(srv, method, n, value, err, staged)
      class(rpc_server_t), intent(inout) :: srv
      integer, intent(in) :: method
      integer(int64), intent(in) :: n
      integer(int64), intent(out) :: value
      integer, intent(out) :: err
      class(*), pointer, intent(out), optional :: staged
      type(capnp_message_t), target :: rmsg, pmsg
      type(message_t) :: root
      type(return_t) :: ret
      type(rpc_call_ctx_t) :: ctx
      type(capnp_ptr_t) :: q
      value = -1_int64
      if (present(staged)) staged => null()
      call capnp_message_init_builder(pmsg, err)
      if (err /= CAPNP_OK) return
      ctx%params = capnp_new_struct(pmsg, 1, 0, err)
      if (err == CAPNP_OK) call capnp_set_i64(ctx%params, 0_int64, n, err)
      if (err == CAPNP_OK) call capnp_message_init_builder(rmsg, err)
      if (err == CAPNP_OK) root = message_new_root(rmsg, err)
      if (err == CAPNP_OK) ret = message_return_init(root, err)
      if (err == CAPNP_OK) ctx%results = return_results_init(ret, err)
      ctx%rmsg => rmsg
      ctx%interface_id = 1_int64
      ctx%method_id = method
      if (err == CAPNP_OK) call srv%dispatch(ctx, err)
      if (present(staged)) then
         if (allocated(ctx%staged)) then
            if (ctx%nexp > 0) staged => ctx%staged(0)%s
         end if
      end if
      if (err == CAPNP_OK) then
         q = payload_content_get(ctx%results, err)
         if (err == CAPNP_OK) value = capnp_get_i64(q, 0_int64)
      end if
      call capnp_message_free(rmsg)
      call capnp_message_free(pmsg)
   end subroutine run_call

   subroutine release_wrapped(p)
      class(*), pointer, intent(inout) :: p
      if (.not. associated(p)) return
      select type (p)
      type is (rpc_membrane_server_t)
         if (p%owned) then
            p%inner_owned = .false.
            deallocate (p)
         end if
      end select
   end subroutine release_wrapped

   subroutine exercise_direct(reverse, do_revoke, error)
      logical, intent(in) :: reverse, do_revoke
      type(error_type), allocatable, intent(inout) :: error
      type(plus_srv_t), target :: inner, alt, pass_child, redir_child
      type(rpc_membrane_server_t), target :: wrap
      type(redir_policy_t), target :: policy_store
      class(rpc_server_t), pointer :: ip, ap, called
      class(rpc_membrane_policy_t), pointer :: pp
      class(*), pointer :: raw
      integer(int64) :: value, value2
      integer :: err, err2, live0
      logical :: is_wrap
      live0 = rpc_membrane_owned_live()
      alt%add1 = 7
      alt%child => redir_child
      inner%child => pass_child
      ip => inner
      ap => alt
      pp => policy_store
      policy_store%alt => ap
      call rpc_membrane_server_init(wrap, ip, pp, reverse=reverse)
      call run_call(wrap, 0, 5_int64, value, err)
      call check_(error, err == CAPNP_OK .and. value == 6_int64 .and. inner%hits(0) == 1, &
                  'redirect: method 0 adds 1')
      call run_call(wrap, 1, 5_int64, value, err)
      call check_(error, err == CAPNP_OK .and. value == 12_int64 .and. inner%hits(1) == 0, &
                  'redirect: method 1 adds 7')
      call run_call(wrap, 2, 5_int64, value, err)
      call check_(error, err == RPC_ERR_EXCEPTION .and. inner%hits(2) == 0, &
                  'redirect: method 2 rejected')

      raw => null()
      call run_call(wrap, 3, 5_int64, value, err, raw)
      is_wrap = .false.
      called => null()
      select type (raw)
      type is (rpc_membrane_server_t)
         is_wrap = associated(raw%policy, pp) .and. (raw%reverse .eqv. reverse)
         called => raw
      end select
      value2 = -1_int64
      err2 = CAPNP_OK
      if (associated(called)) call run_call(called, 2, 5_int64, value2, err2)
      call release_wrapped(raw)
      call check_(error, err == CAPNP_OK .and. is_wrap .and. err2 == RPC_ERR_EXCEPTION &
                  .and. pass_child%hits(2) == 0, &
                  'redirect: pass-through result capability stays wrapped')

      raw => null()
      call run_call(wrap, 1, 5_int64, value, err, raw)
      is_wrap = .false.
      select type (raw)
      type is (rpc_membrane_server_t)
         is_wrap = .true.
      end select
      called => null()
      select type (raw)
      class is (rpc_server_t)
         called => raw
      end select
      value2 = -1_int64
      err2 = CAPNP_ERR_ARG
      if (associated(called)) call run_call(called, 2, 5_int64, value2, err2)
      call check_(error, err == CAPNP_OK .and. value == 12_int64 .and. .not. is_wrap &
                  .and. associated(raw, redir_child) .and. err2 == CAPNP_OK &
                  .and. value2 == 1005_int64, &
                  'redirect: result capability stays outside the membrane')

      if (do_revoke) then
         call rpc_membrane_revoke(policy_store, 'membrane revoked')
         call run_call(wrap, 0, 5_int64, value, err)
         call check_(error, err == RPC_ERR_EXCEPTION .and. inner%hits(0) == 1, &
                     'redirect: revoked pass-through rejected')
         call run_call(wrap, 1, 5_int64, value, err)
         call check_(error, err == CAPNP_OK .and. value == 12_int64 .and. alt%hits(1) == 3, &
                     'redirect: revoked redirect runs')
         call run_call(wrap, 2, 5_int64, value, err)
         call check_(error, err == RPC_ERR_EXCEPTION .and. inner%hits(2) == 0 &
                     .and. alt%hits(2) == 0, &
                     'redirect: revoked deny rejected')
      end if
      call check_(error, rpc_membrane_owned_live() == live0, 'redirect: owned wrappers freed')
   end subroutine exercise_direct

   subroutine run_inbound(error)
      type(error_type), allocatable, intent(out) :: error
      call exercise_direct(.false., .true., error)
   end subroutine run_inbound

   subroutine run_outbound_server(error)
      type(error_type), allocatable, intent(out) :: error
      call exercise_direct(.true., .false., error)
   end subroutine run_outbound_server

   subroutine vat_call(cli, srv, cap, method, n, pump, value, err)
      type(rpc_conn_t), intent(inout) :: cli, srv
      type(rpc_cap_t), intent(in) :: cap
      integer, intent(in) :: method
      integer(int64), intent(in) :: n
      logical, intent(in) :: pump
      integer(int64), intent(out) :: value
      integer, intent(out) :: err
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(capnp_ptr_t) :: s, content
      integer(int64) :: qid
      value = -1_int64
      call rpc_call_begin(cli, cap, 1_int64, method, m, params, qid, err)
      if (err /= CAPNP_OK) return
      s = capnp_new_struct(m, 1, 0, err)
      if (err == CAPNP_OK) call capnp_set_i64(s, 0_int64, n, err)
      if (err == CAPNP_OK) call payload_content_set(params, s, err)
      if (err == CAPNP_OK) call rpc_call_send(cli, m, err)
      if (err /= CAPNP_OK) return
      if (pump) call rpc_pump_once(srv, err)
      if (err /= CAPNP_OK) return
      call wait_bounded(cli, qid, err)
      if (err /= CAPNP_OK) return
      call rpc_result_content(cli, qid, content, err)
      if (err == CAPNP_OK) value = capnp_get_i64(content, 0_int64)
      call rpc_finish_send(cli, qid, .false., err)
      if (pump .and. err == CAPNP_OK) call rpc_pump_once(srv, err)
   end subroutine vat_call

   subroutine vat_call_cap(cli, srv, cap, method, n, pump, value, out, err)
      type(rpc_conn_t), intent(inout) :: cli, srv
      type(rpc_cap_t), intent(in) :: cap
      integer, intent(in) :: method
      integer(int64), intent(in) :: n
      logical, intent(in) :: pump
      integer(int64), intent(out) :: value
      type(rpc_cap_t), intent(out) :: out
      integer, intent(out) :: err
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(capnp_ptr_t) :: s, content
      integer(int64) :: qid
      value = -1_int64
      out = rpc_cap_t()
      call rpc_call_begin(cli, cap, 1_int64, method, m, params, qid, err)
      if (err /= CAPNP_OK) return
      s = capnp_new_struct(m, 1, 0, err)
      if (err == CAPNP_OK) call capnp_set_i64(s, 0_int64, n, err)
      if (err == CAPNP_OK) call payload_content_set(params, s, err)
      if (err == CAPNP_OK) call rpc_call_send(cli, m, err)
      if (err /= CAPNP_OK) return
      if (pump) call rpc_pump_once(srv, err)
      if (err /= CAPNP_OK) return
      call wait_bounded(cli, qid, err)
      if (err /= CAPNP_OK) return
      call rpc_result_content(cli, qid, content, err)
      if (err == CAPNP_OK) value = capnp_get_i64(content, 0_int64)
      if (err == CAPNP_OK) call rpc_result_cap(cli, qid, [0], out, err)
      call rpc_finish_send(cli, qid, .true., err)
      if (pump .and. err == CAPNP_OK) call rpc_pump_once(srv, err)
   end subroutine vat_call_cap

   subroutine run_outbound_client(error)
      type(error_type), allocatable, intent(out) :: error
      type(rpc_conn_t), target :: cli, srv
      type(plus_srv_t), target :: remote, remote_child, alt, local_child
      type(redir_policy_t), target :: policy_store
      class(rpc_server_t), pointer :: boot, sp
      class(rpc_membrane_policy_t), pointer :: pp
      type(rpc_cap_t) :: root, wrapped, returned, local_cap
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      integer(int64) :: value, qid
      integer :: fa, fb, err
      alt%add1 = 7
      alt%child => local_child
      remote%child => remote_child
      call px_socketpair(fa, fb, err)
      call check_(error, err == CAPNP_OK, 'redirect client: socketpair')
      if (allocated(error)) return
      boot => remote
      call rpc_conn_init(srv, fb, boot)
      boot => null()
      call rpc_conn_init(cli, fa, boot)
      if (.not. allocated(cli%exports)) then
         call check_(error, .false., 'redirect client: export table')
         call rpc_conn_close(cli)
         call rpc_conn_close(srv)
         return
      end if
      if (size(cli%exports) < 1) then
         call check_(error, .false., 'redirect client: export slot')
         call rpc_conn_close(cli)
         call rpc_conn_close(srv)
         return
      end if
      sp => alt
      cli%exports(0)%used = .true.
      cli%exports(0)%refcount = 1
      cli%exports(0)%owned = .false.
      cli%exports(0)%srv => sp
      local_cap = rpc_cap_t()
      local_cap%kind = RPC_CAP_LOCAL
      local_cap%id = 0_int64

      call rpc_bootstrap_send(cli, root, err)
      call check_(error, err == CAPNP_OK, 'redirect client: bootstrap send')
      if (err == CAPNP_OK) call rpc_pump_once(srv, err)
      if (err == CAPNP_OK) call wait_bounded(cli, root%id, err)
      qid = root%id
      if (err == CAPNP_OK) call rpc_result_cap(cli, qid, [integer ::], root, err)
      if (err == CAPNP_OK) call rpc_finish_send(cli, qid, .true., err)
      if (err == CAPNP_OK) call rpc_pump_once(srv, err)
      call check_(error, err == CAPNP_OK .and. root%kind == RPC_CAP_IMPORT, &
                  'redirect client: import settled')

      pp => policy_store
      policy_store%alt => sp
      wrapped = rpc_membrane_cap(root, pp, reverse=.true.)
      call vat_call(cli, srv, wrapped, 0, 5_int64, .true., value, err)
      call check_(error, err == CAPNP_OK .and. value == 6_int64 .and. remote%hits(0) == 1, &
                  'redirect client: outbound method 0 adds 1')
      call vat_call_cap(cli, srv, wrapped, 1, 5_int64, .false., value, returned, err)
      call check_(error, err == CAPNP_OK .and. value == 12_int64 .and. remote%hits(1) == 0 &
                  .and. .not. associated(returned%membrane), &
                  'redirect client: outbound method 1 adds 7')
      call vat_call(cli, srv, returned, 2, 5_int64, .false., value, err)
      call check_(error, err == CAPNP_OK .and. value == 1005_int64 .and. local_child%hits(2) == 1, &
                  'redirect client: returned capability stays outside the membrane')

      call vat_call_cap(cli, srv, wrapped, 3, 5_int64, .true., value, returned, err)
      call check_(error, err == CAPNP_OK .and. associated(returned%membrane, pp) &
                  .and. returned%membrane_reverse, &
                  'redirect client: pass-through result capability is wrapped')
      call rpc_call_begin(cli, returned, 1_int64, 2, m, params, qid, err)
      call check_(error, err == RPC_ERR_EXCEPTION .and. remote_child%hits(2) == 0, &
                  'redirect client: wrapped result rejects method 2')

      policy_store%use_cap = .true.
      policy_store%alt_cap = local_cap
      call vat_call(cli, srv, wrapped, 1, 5_int64, .false., value, err)
      call check_(error, err == CAPNP_OK .and. value == 12_int64 .and. remote%hits(1) == 0, &
                  'redirect client: outbound call can retarget a vat client')
      call rpc_conn_close(cli)
      call rpc_conn_close(srv)
   end subroutine run_outbound_client

   subroutine vat_probe(cli, srv, cap, method, n, pump, value, reason, etype, err)
      type(rpc_conn_t), intent(inout) :: cli, srv
      type(rpc_cap_t), intent(in) :: cap
      integer, intent(in) :: method
      integer(int64), intent(in) :: n
      logical, intent(in) :: pump
      integer(int64), intent(out) :: value
      character(len=:), allocatable, intent(out) :: reason
      integer, intent(out) :: etype
      integer, intent(out) :: err
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(capnp_ptr_t) :: s, content
      integer(int64) :: qid
      integer :: rerr
      value = -1_int64
      reason = ''
      etype = -1
      call rpc_call_begin(cli, cap, 1_int64, method, m, params, qid, err)
      if (err /= CAPNP_OK) then
         reason = rpc_conn_reason(cli)
         return
      end if
      s = capnp_new_struct(m, 1, 0, err)
      if (err == CAPNP_OK) call capnp_set_i64(s, 0_int64, n, err)
      if (err == CAPNP_OK) call payload_content_set(params, s, err)
      if (err == CAPNP_OK) call rpc_call_send(cli, m, err)
      if (err /= CAPNP_OK) return
      if (pump) call rpc_pump_once(srv, err)
      if (err /= CAPNP_OK) return
      call wait_bounded(cli, qid, err)
      if (err /= CAPNP_OK) return
      call rpc_result_content(cli, qid, content, err, etype)
      reason = rpc_conn_reason(cli)
      rerr = err
      if (err == CAPNP_OK) value = capnp_get_i64(content, 0_int64)
      call rpc_finish_send(cli, qid, .false., err)
      if (pump) call rpc_pump_once(srv, err)
      if (rerr /= CAPNP_OK) err = rerr
   end subroutine vat_probe

   subroutine settle_root(cli, srv, root, err)
      type(rpc_conn_t), intent(inout) :: cli, srv
      type(rpc_cap_t), intent(inout) :: root
      integer, intent(out) :: err
      integer(int64) :: qid
      call rpc_bootstrap_send(cli, root, err)
      if (err == CAPNP_OK) call rpc_pump_once(srv, err)
      if (err == CAPNP_OK) call wait_bounded(cli, root%id, err)
      qid = root%id
      if (err == CAPNP_OK) call rpc_result_cap(cli, qid, [integer ::], root, err)
      if (err == CAPNP_OK) call rpc_finish_send(cli, qid, .true., err)
      if (err == CAPNP_OK) call rpc_pump_once(srv, err)
   end subroutine settle_root

   subroutine run_server_reasons(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cli, srv
      type(plus_srv_t), target :: inner, alt
      type(rpc_membrane_server_t), target :: wrap
      type(redir_policy_t), target :: policy_store
      class(rpc_server_t), pointer :: boot, ip, ap
      class(rpc_membrane_policy_t), pointer :: pp
      type(rpc_cap_t) :: root
      character(len=:), allocatable :: reason
      integer(int64) :: value
      integer :: fa, fb, err, etype
      alt%add1 = 7
      ip => inner
      ap => alt
      pp => policy_store
      policy_store%alt => ap
      call rpc_membrane_server_init(wrap, ip, pp)
      call px_socketpair(fa, fb, err)
      call check_(error, err == CAPNP_OK, 'revoke server: socketpair')
      if (allocated(error)) return
      boot => wrap
      call rpc_conn_init(srv, fb, boot)
      boot => null()
      call rpc_conn_init(cli, fa, boot)
      call settle_root(cli, srv, root, err)
      call check_(error, err == CAPNP_OK .and. root%kind == RPC_CAP_IMPORT, &
                  'revoke server: import settled')
      call rpc_membrane_revoke(policy_store, 'membrane revoked')
      call vat_probe(cli, srv, root, 1, 5_int64, .true., value, reason, etype, err)
      call check_(error, err == CAPNP_OK .and. value == 12_int64 .and. alt%hits(1) == 1 &
                  .and. inner%hits(1) == 0, &
                  'revoke server: redirect runs')
      call vat_probe(cli, srv, root, 0, 5_int64, .true., value, reason, etype, err)
      call check_(error, err == RPC_ERR_EXCEPTION .and. reason == 'membrane revoked' &
                  .and. etype == EXCEPTION_TYPE_DISCONNECTED_E .and. inner%hits(0) == 0, &
                  'revoke server: pass-through reason')
      call vat_probe(cli, srv, root, 2, 5_int64, .true., value, reason, etype, err)
      call check_(error, err == RPC_ERR_EXCEPTION .and. reason == 'method raised error' &
                  .and. etype == EXCEPTION_TYPE_FAILED_E .and. inner%hits(2) == 0 &
                  .and. alt%hits(2) == 0, &
                  'revoke server: denied method')
      call rpc_conn_close(cli)
      call rpc_conn_close(srv)
   end subroutine run_server_reasons

   subroutine run_client_reasons(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cli, srv
      type(plus_srv_t), target :: remote, alt
      type(reason_policy_t), target :: policy_store
      type(redir_policy_t), target :: plain_store
      class(rpc_server_t), pointer :: boot, ap
      class(rpc_membrane_policy_t), pointer :: pp
      type(rpc_cap_t) :: root, wrapped, plain_wrapped
      character(len=:), allocatable :: reason
      integer(int64) :: value
      integer :: fa, fb, err, etype
      alt%add1 = 7
      ap => alt
      call px_socketpair(fa, fb, err)
      call check_(error, err == CAPNP_OK, 'revoke client: socketpair')
      if (allocated(error)) return
      boot => remote
      call rpc_conn_init(srv, fb, boot)
      boot => null()
      call rpc_conn_init(cli, fa, boot)
      call settle_root(cli, srv, root, err)
      call check_(error, err == CAPNP_OK .and. root%kind == RPC_CAP_IMPORT, &
                  'revoke client: import settled')
      pp => plain_store
      plain_store%alt => ap
      plain_wrapped = rpc_membrane_cap(root, pp, reverse=.true.)
      call rpc_membrane_revoke(plain_store, 'membrane revoked')
      call vat_probe(cli, srv, plain_wrapped, 2, 5_int64, .false., value, reason, etype, err)
      call check_(error, err == RPC_ERR_EXCEPTION .and. reason == '' &
                  .and. remote%hits(2) == 0, &
                  'revoke client: plain policy deny reason')
      pp => policy_store
      policy_store%alt => ap
      wrapped = rpc_membrane_cap(root, pp, reverse=.true.)
      call rpc_membrane_revoke(policy_store, 'membrane revoked')
      call vat_probe(cli, srv, wrapped, 2, 5_int64, .false., value, reason, etype, err)
      call check_(error, err == RPC_ERR_EXCEPTION .and. reason == 'nope denied' &
                  .and. remote%hits(2) == 0, &
                  'revoke client: denied method')
      call vat_probe(cli, srv, wrapped, 0, 5_int64, .false., value, reason, etype, err)
      call check_(error, err == RPC_ERR_EXCEPTION .and. reason == 'membrane revoked' &
                  .and. remote%hits(0) == 0, &
                  'revoke client: pass-through reason')
      call vat_probe(cli, srv, wrapped, 1, 5_int64, .false., value, reason, etype, err)
      call check_(error, err == CAPNP_OK .and. value == 12_int64 .and. alt%hits(1) == 1 &
                  .and. remote%hits(1) == 0, &
                  'revoke client: redirect runs')
      call rpc_conn_close(cli)
      call rpc_conn_close(srv)
   end subroutine run_client_reasons

   subroutine run_after_revoke(error)
      type(error_type), allocatable, intent(out) :: error
      call exercise_direct(.true., .true., error)
      if (allocated(error)) return
      call run_server_reasons(error)
      if (allocated(error)) return
      call run_client_reasons(error)
   end subroutine run_after_revoke

   !> Eight redirect hops land on a revoked policy. That policy still
   !> forwards its redirect (membrane.c++ lines 428 and 459).
   subroutine run_revoked_chain(error)
      type(error_type), allocatable, intent(out) :: error
      type(rpc_conn_t), target :: cli, srv
      type(plus_srv_t), target :: remote, alt
      type(redir_policy_t), target :: hop(0:8)
      class(rpc_server_t), pointer :: boot, ap
      class(rpc_membrane_policy_t), pointer :: pp
      type(rpc_cap_t) :: root, wrapped, link
      integer(int64) :: value
      integer :: fa, fb, err, i
      alt%add1 = 7
      ap => alt
      hop(8)%alt => ap
      call px_socketpair(fa, fb, err)
      call check_(error, err == CAPNP_OK, 'chain: socketpair')
      if (allocated(error)) return
      boot => remote
      call rpc_conn_init(srv, fb, boot)
      boot => null()
      call rpc_conn_init(cli, fa, boot)
      call settle_root(cli, srv, root, err)
      call check_(error, err == CAPNP_OK .and. root%kind == RPC_CAP_IMPORT, &
                  'chain: import settled')
      if (allocated(error)) then
         call rpc_conn_close(cli)
         call rpc_conn_close(srv)
         return
      end if
      do i = 8, 1, -1
         pp => hop(i)
         link = rpc_membrane_cap(root, pp, reverse=.true.)
         hop(i - 1)%use_cap = .true.
         hop(i - 1)%alt_cap = link
      end do
      pp => hop(0)
      wrapped = rpc_membrane_cap(root, pp, reverse=.true.)
      do i = 0, 8
         call rpc_membrane_revoke(hop(i), 'membrane revoked')
      end do
      call vat_call(cli, srv, wrapped, 1, 5_int64, .false., value, err)
      call check_(error, err == CAPNP_OK .and. value == 12_int64 .and. alt%hits(1) == 1 &
                  .and. remote%hits(1) == 0, &
                  'chain: revoked redirect at hop cap runs')
      call rpc_conn_close(cli)
      call rpc_conn_close(srv)
   end subroutine run_revoked_chain

   subroutine run_chain_deny(error)
      type(error_type), allocatable, intent(out) :: error
      call run_cap_tail(1, error)
   end subroutine run_chain_deny

   subroutine run_chain_revoked(error)
      type(error_type), allocatable, intent(out) :: error
      call run_cap_tail(2, error)
   end subroutine run_chain_revoked

   subroutine run_chain_server(error)
      type(error_type), allocatable, intent(out) :: error
      call run_cap_tail(3, error)
   end subroutine run_chain_server

   !> Hop 8 names another membrane. That membrane denies method 1,
   !> is revoked with no redirect, or names a local server.
   subroutine run_cap_tail(kind, error)
      integer, intent(in) :: kind
      type(error_type), allocatable, intent(out) :: error
      type(rpc_conn_t), target :: cli, srv
      type(plus_srv_t), target :: remote, alt
      type(redir_policy_t), target :: hop(0:8), tail
      type(reason_policy_t), target :: deny
      class(rpc_server_t), pointer :: boot, ap
      class(rpc_membrane_policy_t), pointer :: pp
      type(rpc_cap_t) :: root, wrapped, link
      character(len=:), allocatable :: reason
      integer(int64) :: value
      integer :: fa, fb, err, etype, i
      alt%add1 = 7
      ap => alt
      if (kind == 1) deny%deny1 = .true.
      if (kind == 3) tail%alt => ap
      call px_socketpair(fa, fb, err)
      call check_(error, err == CAPNP_OK, 'chain cap: socketpair')
      if (allocated(error)) return
      boot => remote
      call rpc_conn_init(srv, fb, boot)
      boot => null()
      call rpc_conn_init(cli, fa, boot)
      call settle_root(cli, srv, root, err)
      call check_(error, err == CAPNP_OK .and. root%kind == RPC_CAP_IMPORT, &
                  'chain cap: import settled')
      if (allocated(error)) then
         call rpc_conn_close(cli)
         call rpc_conn_close(srv)
         return
      end if
      do i = 8, 1, -1
         pp => hop(i)
         link = rpc_membrane_cap(root, pp, reverse=.true.)
         hop(i - 1)%use_cap = .true.
         hop(i - 1)%alt_cap = link
      end do
      if (kind == 1) then
         pp => deny
      else
         pp => tail
      end if
      link = rpc_membrane_cap(root, pp, reverse=.true.)
      hop(8)%use_cap = .true.
      hop(8)%alt_cap = link
      pp => hop(0)
      wrapped = rpc_membrane_cap(root, pp, reverse=.true.)
      do i = 0, 8
         call rpc_membrane_revoke(hop(i), 'membrane revoked')
      end do
      if (kind == 2) call rpc_membrane_revoke(tail, 'tail revoked')
      if (kind == 3) call rpc_membrane_revoke(tail, 'membrane revoked')
      if (kind == 3) then
         call vat_call(cli, srv, wrapped, 1, 5_int64, .false., value, err)
         call check_(error, err == CAPNP_OK .and. value == 12_int64 &
                     .and. alt%hits(1) == 1 .and. remote%hits(1) == 0, &
                     'chain cap: tail redirect runs')
      else
         call vat_probe(cli, srv, wrapped, 1, 5_int64, .true., value, reason, etype, err)
         if (kind == 1) then
            call check_(error, err == RPC_ERR_EXCEPTION .and. reason == 'nope denied' &
                        .and. remote%hits(1) == 0 .and. alt%hits(1) == 0, &
                        'chain cap: deny stops the call')
         else
            call check_(error, err == RPC_ERR_EXCEPTION .and. reason == 'tail revoked' &
                        .and. remote%hits(1) == 0 .and. alt%hits(1) == 0, &
                        'chain cap: revoked tail stops the call')
         end if
      end if
      call rpc_conn_close(cli)
      call rpc_conn_close(srv)
   end subroutine run_cap_tail

end module test_membrane_redirect
