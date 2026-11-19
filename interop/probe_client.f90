!> Local Echo and Worker hosted by the probe client.
module probe_servers
   use capnp
   use rpc_capnp
   use capnp_rpc
   implicit none
   public :: echo_t, worker_t, ECHO_IFACE, WORKER_IFACE

   integer(int64), parameter :: ECHO_IFACE = -5689160577013907439_int64
   integer(int64), parameter :: WORKER_IFACE = -5689160577013907438_int64

   type, extends(rpc_server_t) :: worker_t
      integer(int32) :: base = 40_int32
   contains
      procedure :: dispatch => worker_dispatch
   end type worker_t

   type, extends(rpc_server_t) :: echo_t
      type(worker_t), pointer :: worker => null()
   contains
      procedure :: dispatch => echo_dispatch
   end type echo_t

contains

   subroutine echo_dispatch(self, ctx, err)
      class(echo_t), intent(inout) :: self
      type(rpc_call_ctx_t), intent(inout) :: ctx
      integer, intent(out) :: err
      type(capnp_ptr_t) :: s
      class(rpc_server_t), pointer :: sp
      integer :: idx
      err = CAPNP_OK
      if (ctx%interface_id /= ECHO_IFACE) then
         err = CAPNP_ERR_ARG
         return
      end if
      select case (ctx%method_id)
      case (0)
         s = capnp_new_struct(ctx%rmsg, 1, 0, err)
         if (err /= CAPNP_OK) return
         call capnp_set_i32(s, 0_int64, capnp_get_i32(ctx%params, 0_int64), err)
         if (err == CAPNP_OK) call payload_content_set(ctx%results, s, err)
      case (1)
         err = CAPNP_ERR_ARG
      case (2)
         if (.not. associated(self%worker)) then
            err = CAPNP_ERR_ARG
            return
         end if
         sp => self%worker
         idx = rpc_ctx_export_cap(ctx, sp, err)
         if (err /= CAPNP_OK) return
         s = capnp_new_struct(ctx%rmsg, 0, 1, err)
         if (err /= CAPNP_OK) return
         call capnp_setp(s, 0, rpc_make_cap_ptr(ctx%rmsg, idx), err)
         if (err == CAPNP_OK) call payload_content_set(ctx%results, s, err)
      case default
         err = CAPNP_ERR_ARG
      end select
   end subroutine echo_dispatch

   subroutine worker_dispatch(self, ctx, err)
      class(worker_t), intent(inout) :: self
      type(rpc_call_ctx_t), intent(inout) :: ctx
      integer, intent(out) :: err
      type(capnp_ptr_t) :: s
      integer(int32) :: n
      err = CAPNP_OK
      if (ctx%interface_id /= WORKER_IFACE .or. ctx%method_id /= 0) then
         err = CAPNP_ERR_ARG
         return
      end if
      n = capnp_get_i32(ctx%params, 0_int64)
      s = capnp_new_struct(ctx%rmsg, 1, 0, err)
      if (err /= CAPNP_OK) return
      call capnp_set_i32(s, 0_int64, self%base + n, err)
      if (err == CAPNP_OK) call payload_content_set(ctx%results, s, err)
   end subroutine worker_dispatch

end module probe_servers

!> Fortran client for the C++ 1.4.0 probe server.
!> One scenario name on the command line. A failure prints the values
!> the peer actually returned and exits non-zero.
program probe_client
   use capnp
   use rpc_capnp
   use capnp_posix
   use capnp_rpc
   use probe_servers
   implicit none

   integer(int64), parameter :: BOX_IFACE = -5689160577013907436_int64
   integer, parameter :: WAIT_TRIES = 80

   type(rpc_conn_t), target :: conn
   type(echo_t), target :: echo
   type(worker_t), target :: worker
   class(rpc_server_t), pointer :: nosrv, esp
   type(rpc_cap_t) :: boot
   character(len=16) :: arg
   character(len=32) :: scen
   integer :: port, fd, err, i, nfail

   nfail = 0
   nosrv => null()
   worker%base = 40_int32
   echo%worker => worker
   port = 43118
   scen = 'bootstrap-ok'
   call get_command_argument(1, arg)
   if (len_trim(arg) > 0) read (arg, *) port
   call get_command_argument(2, scen)

   fd = PX_BAD_FD
   do i = 1, 50
      call px_tcp_connect(port, fd, err)
      if (err == CAPNP_OK) exit
      call execute_command_line('sleep 0.2')
   end do
   if (fd == PX_BAD_FD) then
      print '(a)', 'CONNECT_FAIL'
      error stop 1
   end if
   call rpc_conn_init(conn, fd, nosrv)
   esp => echo
   conn%exports(0)%used = .true.
   conn%exports(0)%refcount = 1
   conn%exports(0)%srv => esp

   call do_bootstrap()
   if (nfail > 0) call finish_program()

   select case (trim(scen))
   case ('bootstrap-ok')
      call scenario_bootstrap()
   case ('local-keep')
      call scenario_local(.false.)
   case ('local-drop')
      call scenario_local(.true.)
   case ('remote-reuse')
      call scenario_reuse()
   case ('same-remote')
      call scenario_same()
   case ('finish-drop')
      call scenario_finish_drop()
   case ('embargo-pipeline')
      call scenario_embargo_pipeline()
   case ('local-return-cap')
      call scenario_local_return_cap()
   case ('tail-ok-finish')
      call scenario_tail_ok()
   case ('tail-fail')
      call scenario_tail_fail()
   case ('tail-worker')
      call scenario_tail_worker()
   case default
      nfail = nfail + 1
      print '(a,a)', 'UNKNOWN_SCENARIO ', trim(scen)
   end select

   call finish_program()

contains

   subroutine finish_program()
      call rpc_conn_close(conn)
      if (nfail > 0) error stop 1
      print '(a)', 'PROBE_OK'
      stop
   end subroutine finish_program

   subroutine check_(cond, name)
      logical, intent(in) :: cond
      character(len=*), intent(in) :: name
      if (.not. cond) then
         nfail = nfail + 1
         print '(a,a,a,l1,a,a)', 'FAIL ', name, ' dead=', conn%dead, &
            ' reason=', trim(rpc_conn_reason(conn))
      end if
   end subroutine check_

   subroutine wait_back(qid, ok)
      integer(int64), intent(in) :: qid
      logical, intent(out) :: ok
      integer :: n, e, q
      logical :: handled
      ok = .false.
      if (qid < 0_int64 .or. qid >= int(size(conn%questions), int64)) return
      q = int(qid)
      do n = 1, WAIT_TRIES
         if (conn%dead) return
         if (conn%questions(q)%returned) then
            ok = .true.
            return
         end if
         call rpc_pump_poll(conn, 50, handled, e)
         if (e /= CAPNP_OK .and. e /= RPC_ERR_DEAD) return
      end do
   end subroutine wait_back

   subroutine drain(rounds)
      integer, intent(in) :: rounds
      integer :: n, e
      logical :: handled
      do n = 1, rounds
         if (conn%dead) return
         call rpc_pump_poll(conn, 30, handled, e)
         if (.not. handled) return
      end do
   end subroutine drain

   subroutine do_bootstrap()
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      integer(int64) :: q
      logical :: back
      integer :: e
      call rpc_bootstrap_send(conn, boot, e)
      call check_(e == CAPNP_OK, 'bootstrap sent')
      if (nfail > 0) return
      call wait_back(boot%id, back)
      call check_(back, 'bootstrap returned')
      if (.not. back) return
      call rpc_result_cap(conn, boot%id, [integer ::], boot, e)
      call check_(e == CAPNP_OK .and. boot%kind == RPC_CAP_IMPORT, 'bootstrap cap')
      if (nfail > 0) return
      ! One server is shared by every scenario. Drop what the previous
      ! connection left behind before this one measures live objects.
      call call_begin_box(11, m, params, q, e)
      call empty_params(m, params, e)
      call rpc_call_send(conn, m, e)
      call wait_back(q, back)
      call check_(back .and. .not. conn%dead, 'begin')
      call rpc_finish_send(conn, q, .false., e)
   end subroutine do_bootstrap

   subroutine call_begin_box(method, m, params, qid, e)
      integer, intent(in) :: method
      type(capnp_message_t), intent(inout), target :: m
      type(payload_t), intent(out) :: params
      integer(int64), intent(out) :: qid
      integer, intent(out) :: e
      call rpc_call_begin(conn, boot, BOX_IFACE, method, m, params, qid, e)
   end subroutine call_begin_box

   subroutine empty_params(m, params, e)
      type(capnp_message_t), intent(inout), target :: m
      type(payload_t), intent(inout) :: params
      integer, intent(out) :: e
      call payload_content_set(params, capnp_new_struct(m, 0, 0, e), e)
   end subroutine empty_params

   subroutine read_i32(qid, value, e)
      integer(int64), intent(in) :: qid
      integer(int32), intent(out) :: value
      integer, intent(out) :: e
      type(capnp_ptr_t) :: content
      value = -1_int32
      call rpc_result_content(conn, qid, content, e)
      if (e /= CAPNP_OK) return
      value = capnp_get_i32(content, 0_int64)
   end subroutine read_i32

   subroutine read_i64(qid, value, e)
      integer(int64), intent(in) :: qid
      integer(int64), intent(out) :: value
      integer, intent(out) :: e
      type(capnp_ptr_t) :: content
      value = -1_int64
      call rpc_result_content(conn, qid, content, e)
      if (e /= CAPNP_OK) return
      value = capnp_get_i64(content, 0_int64)
   end subroutine read_i64

   subroutine scenario_bootstrap()
      integer(int64) :: q, n
      integer :: e
      logical :: back
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      call call_begin_box(11, m, params, q, e)
      call empty_params(m, params, e)
      call rpc_call_send(conn, m, e)
      call wait_back(q, back)
      call check_(back .and. .not. conn%dead, 'begin')
      call rpc_finish_send(conn, q, .false., e)
      call call_begin_box(2, m, params, q, e)
      call empty_params(m, params, e)
      call rpc_call_send(conn, m, e)
      call wait_back(q, back)
      call read_i64(q, n, e)
      print '(a,l1,a,i0,a,i0,a,l1,a,a)', 'BOOTSTRAP ok=', back, ' err=', e, &
         ' live=', n, ' dead=', conn%dead, ' reason=', trim(rpc_conn_reason(conn))
      call check_(back .and. e == CAPNP_OK .and. n == 0_int64 .and. .not. conn%dead, &
                  'bootstrap live is 0')
      call rpc_finish_send(conn, q, .false., e)
   end subroutine scenario_bootstrap

   subroutine give_peer()
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(capnp_ptr_t) :: s, ctab
      type(cap_descriptor_t) :: cd
      integer(int64) :: q
      integer :: e
      logical :: back
      call call_begin_box(3, m, params, q, e)
      s = capnp_new_struct(m, 0, 1, e)
      call capnp_setp(s, 0, rpc_make_cap_ptr(m, 0), e)
      call payload_content_set(params, s, e)
      ctab = payload_cap_table_init(params, 1_int64, e)
      cd%p = capnp_list_get_struct(ctab, 0, e)
      call cap_descriptor_sender_hosted_set(cd, 0_int64, e)
      call rpc_call_send(conn, m, e)
      call wait_back(q, back)
      call check_(back .and. e == CAPNP_OK .and. .not. conn%dead, 'setPeer')
      call rpc_finish_send(conn, q, .false., e)
   end subroutine give_peer

   subroutine scenario_local(drop_ref)
      logical, intent(in) :: drop_ref
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(capnp_ptr_t) :: s
      type(rpc_cap_t) :: promised
      integer(int64) :: qprom, qres, qcall
      integer :: e
      integer(int32) :: val
      logical :: back
      call give_peer()
      if (nfail > 0) return
      call call_begin_box(8, m, params, qprom, e)
      call empty_params(m, params, e)
      call rpc_call_send(conn, m, e)
      call wait_back(qprom, back)
      call rpc_result_cap(conn, qprom, [0], promised, e)
      call check_(back .and. e == CAPNP_OK, 'promise of peer')
      if (nfail > 0) return
      call rpc_finish_send(conn, qprom, .true., e)
      call call_begin_box(9, m, params, qres, e)
      s = capnp_new_struct(m, 1, 0, e)
      call capnp_set_bool(s, 0_int64, drop_ref, e)
      call payload_content_set(params, s, e)
      call rpc_call_send(conn, m, e)
      call wait_back(qres, back)
      call check_(back, 'resolvePeer returned')
      call rpc_finish_send(conn, qres, .false., e)
      call drain(8)
      call rpc_call_begin(conn, promised, ECHO_IFACE, 0, m, params, qcall, e)
      s = capnp_new_struct(m, 1, 0, e)
      call capnp_set_i32(s, 0_int64, 7_int32, e)
      call payload_content_set(params, s, e)
      call rpc_call_send(conn, m, e)
      call wait_back(qcall, back)
      call read_i32(qcall, val, e)
      if (drop_ref) then
         print '(a,l1,a,i0,a,i0,a,l1,a,a)', 'LOCAL_DROP ok=', back, ' err=', e, &
            ' val=', val, ' dead=', conn%dead, ' reason=', trim(rpc_conn_reason(conn))
      else
         print '(a,l1,a,i0,a,i0,a,l1,a,a)', 'LOCAL_KEEP ok=', back, ' err=', e, &
            ' val=', val, ' dead=', conn%dead, ' reason=', trim(rpc_conn_reason(conn))
      end if
      call check_(back .and. e == CAPNP_OK .and. val == 7_int32 .and. .not. conn%dead, &
                  'local echo returned 7')
   end subroutine scenario_local

   subroutine one_cycle(tag, live_after)
      character(len=*), intent(in) :: tag
      integer(int64), intent(out) :: live_after
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(rpc_cap_t) :: promised
      integer(int64) :: qprom, qres, qlive
      integer :: e
      logical :: back
      live_after = -1_int64
      call call_begin_box(0, m, params, qprom, e)
      call empty_params(m, params, e)
      call rpc_call_send(conn, m, e)
      call wait_back(qprom, back)
      call rpc_result_cap(conn, qprom, [0], promised, e)
      call check_(back .and. e == CAPNP_OK, tag // ' promise')
      if (nfail > 0) return
      call rpc_finish_send(conn, qprom, .true., e)
      call call_begin_box(1, m, params, qres, e)
      call empty_params(m, params, e)
      call rpc_call_send(conn, m, e)
      call wait_back(qres, back)
      call check_(back, tag // ' resolveNow')
      call rpc_finish_send(conn, qres, .false., e)
      call drain(4)
      call rpc_release_send(conn, promised, e)
      call call_begin_box(2, m, params, qlive, e)
      call empty_params(m, params, e)
      call rpc_call_send(conn, m, e)
      call wait_back(qlive, back)
      call read_i64(qlive, live_after, e)
      print '(a,a,l1,a,i0,a,i0,a,l1,a,a)', tag, ' ok=', back, ' err=', e, &
         ' live=', live_after, ' dead=', conn%dead, ' reason=', &
         trim(rpc_conn_reason(conn))
      call rpc_finish_send(conn, qlive, .false., e)
      call check_(back .and. e == CAPNP_OK .and. live_after == 0_int64 .and. &
                  .not. conn%dead, tag // ' live is 0')
   end subroutine one_cycle

   subroutine scenario_reuse()
      integer(int64) :: n1, n2
      call one_cycle('REUSE1', n1)
      if (nfail > 0) return
      call one_cycle('REUSE2', n2)
   end subroutine scenario_reuse

   subroutine scenario_same()
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(rpc_cap_t) :: a, b
      integer(int64) :: q, qlive, n
      integer :: e
      logical :: back
      call call_begin_box(6, m, params, q, e)
      call empty_params(m, params, e)
      call rpc_call_send(conn, m, e)
      call wait_back(q, back)
      call rpc_result_cap(conn, q, [0], a, e)
      call check_(back .and. e == CAPNP_OK, 'same a')
      if (nfail > 0) return
      call rpc_result_cap(conn, q, [1], b, e)
      call check_(e == CAPNP_OK, 'same b')
      print '(a,i0,a,i0,a,i0,a,i0)', 'SAME ids kind/id a=', a%kind, ',', a%id, &
         ' b=', b%kind, ',', b%id
      call rpc_finish_send(conn, q, .true., e)
      call rpc_release_send(conn, a, e)
      call rpc_release_send(conn, b, e)
      call call_begin_box(2, m, params, qlive, e)
      call empty_params(m, params, e)
      call rpc_call_send(conn, m, e)
      call wait_back(qlive, back)
      call read_i64(qlive, n, e)
      print '(a,l1,a,i0,a,i0,a,l1,a,a)', 'SAME ok=', back, ' err=', e, &
         ' live=', n, ' dead=', conn%dead, ' reason=', trim(rpc_conn_reason(conn))
      call check_(back .and. e == CAPNP_OK .and. n == 0_int64 .and. .not. conn%dead, &
                  'same-remote live is 0')
   end subroutine scenario_same

   subroutine scenario_finish_drop()
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(rpc_cap_t) :: cap
      integer(int64) :: q, qlive, n
      integer :: e
      logical :: back
      call call_begin_box(7, m, params, q, e)
      call empty_params(m, params, e)
      call rpc_call_send(conn, m, e)
      call wait_back(q, back)
      call rpc_result_cap(conn, q, [0], cap, e)
      call check_(back .and. e == CAPNP_OK, 'oneRemote cap')
      if (nfail > 0) return
      call rpc_finish_send(conn, q, .false., e)
      call rpc_release_send(conn, cap, e)
      call call_begin_box(2, m, params, qlive, e)
      call empty_params(m, params, e)
      call rpc_call_send(conn, m, e)
      call wait_back(qlive, back)
      call read_i64(qlive, n, e)
      print '(a,l1,a,i0,a,i0,a,l1,a,a)', 'FINISH_DROP ok=', back, ' err=', e, &
         ' live=', n, ' dead=', conn%dead, ' reason=', trim(rpc_conn_reason(conn))
      call check_(back .and. e == CAPNP_OK .and. n == 0_int64 .and. .not. conn%dead, &
                  'finish retain false then release stays up')
   end subroutine scenario_finish_drop

   subroutine scenario_embargo_pipeline()
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(capnp_ptr_t) :: s
      type(rpc_cap_t) :: promised, piped
      integer(int64) :: qprom, qearly, qres, qwork, qping
      integer :: e
      integer(int32) :: val
      logical :: back
      call give_peer()
      if (nfail > 0) return
      call call_begin_box(8, m, params, qprom, e)
      call empty_params(m, params, e)
      call rpc_call_send(conn, m, e)
      call wait_back(qprom, back)
      call rpc_result_cap(conn, qprom, [0], promised, e)
      call check_(back .and. e == CAPNP_OK, 'embargo promise')
      if (nfail > 0) return
      call rpc_finish_send(conn, qprom, .true., e)
      call rpc_call_begin(conn, promised, ECHO_IFACE, 0, m, params, qearly, e)
      s = capnp_new_struct(m, 1, 0, e)
      call capnp_set_i32(s, 0_int64, 1_int32, e)
      call payload_content_set(params, s, e)
      call rpc_call_send(conn, m, e)
      call call_begin_box(9, m, params, qres, e)
      s = capnp_new_struct(m, 1, 0, e)
      call capnp_set_bool(s, 0_int64, .false., e)
      call payload_content_set(params, s, e)
      call rpc_call_send(conn, m, e)
      call wait_back(qres, back)
      call check_(back, 'embargo resolve returned')
      if (nfail > 0) return
      call rpc_call_begin(conn, promised, ECHO_IFACE, 2, m, params, qwork, e)
      call empty_params(m, params, e)
      call rpc_call_send(conn, m, e)
      piped = rpc_pipeline_cap(conn, qwork, [0])
      call rpc_call_begin(conn, piped, WORKER_IFACE, 0, m, params, qping, e)
      s = capnp_new_struct(m, 1, 0, e)
      call capnp_set_i32(s, 0_int64, 2_int32, e)
      call payload_content_set(params, s, e)
      call rpc_call_send(conn, m, e)
      call wait_back(qping, back)
      call read_i32(qping, val, e)
      print '(a,l1,a,i0,a,i0,a,l1,a,a)', 'EMBARGO_PIPE ok=', back, ' err=', e, &
         ' val=', val, ' dead=', conn%dead, ' reason=', trim(rpc_conn_reason(conn))
      call check_(back .and. e == CAPNP_OK .and. val == 42_int32 .and. .not. conn%dead, &
                  'embargo pipeline ping is 42')
   end subroutine scenario_embargo_pipeline

   subroutine scenario_local_return_cap()
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(capnp_ptr_t) :: s
      type(rpc_cap_t) :: promised, got
      integer(int64) :: qprom, qres, qwork, qping
      integer :: e
      integer(int32) :: val
      logical :: back
      call give_peer()
      if (nfail > 0) return
      call call_begin_box(8, m, params, qprom, e)
      call empty_params(m, params, e)
      call rpc_call_send(conn, m, e)
      call wait_back(qprom, back)
      call rpc_result_cap(conn, qprom, [0], promised, e)
      call check_(back .and. e == CAPNP_OK, 'local-return promise')
      if (nfail > 0) return
      call rpc_finish_send(conn, qprom, .true., e)
      call call_begin_box(9, m, params, qres, e)
      s = capnp_new_struct(m, 1, 0, e)
      call capnp_set_bool(s, 0_int64, .false., e)
      call payload_content_set(params, s, e)
      call rpc_call_send(conn, m, e)
      call wait_back(qres, back)
      call drain(6)
      call rpc_call_begin(conn, promised, ECHO_IFACE, 2, m, params, qwork, e)
      call empty_params(m, params, e)
      call rpc_call_send(conn, m, e)
      call wait_back(qwork, back)
      call rpc_result_cap(conn, qwork, [0], got, e)
      print '(a,l1,a,i0,a,i0,a,i0,a,l1,a,a)', 'LOCAL_RET_CAP ok=', back, &
         ' err=', e, ' kind=', got%kind, ' id=', got%id, ' dead=', conn%dead, &
         ' reason=', trim(rpc_conn_reason(conn))
      call check_(back .and. e == CAPNP_OK .and. .not. conn%dead, 'local worker cap')
      if (nfail > 0) return
      call rpc_call_begin(conn, got, WORKER_IFACE, 0, m, params, qping, e)
      s = capnp_new_struct(m, 1, 0, e)
      call capnp_set_i32(s, 0_int64, 2_int32, e)
      call payload_content_set(params, s, e)
      call rpc_call_send(conn, m, e)
      call wait_back(qping, back)
      call read_i32(qping, val, e)
      print '(a,l1,a,i0,a,i0,a,l1,a,a)', 'LOCAL_RET_CALL ok=', back, ' err=', e, &
         ' val=', val, ' dead=', conn%dead, ' reason=', trim(rpc_conn_reason(conn))
      call check_(back .and. e == CAPNP_OK .and. val == 42_int32 .and. .not. conn%dead, &
                  'local worker ping is 42')
   end subroutine scenario_local_return_cap

   subroutine scenario_tail_ok()
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(capnp_ptr_t) :: s
      integer(int64) :: q, qlive, n
      integer :: e
      integer(int32) :: val
      logical :: back
      call give_peer()
      if (nfail > 0) return
      call call_begin_box(4, m, params, q, e)
      s = capnp_new_struct(m, 1, 0, e)
      call capnp_set_i32(s, 0_int64, 7_int32, e)
      call payload_content_set(params, s, e)
      call rpc_call_send(conn, m, e)
      call wait_back(q, back)
      call drain(8)
      call read_i32(q, val, e)
      print '(a,l1,a,i0,a,i0,a,l1,a,a)', 'TAIL_OK ok=', back, ' err=', e, &
         ' val=', val, ' dead=', conn%dead, ' reason=', trim(rpc_conn_reason(conn))
      call check_(back .and. e == CAPNP_OK .and. val == 7_int32 .and. .not. conn%dead, &
                  'tail echo is 7 after Finish')
      if (conn%dead) return
      call call_begin_box(2, m, params, qlive, e)
      call empty_params(m, params, e)
      call rpc_call_send(conn, m, e)
      call wait_back(qlive, back)
      call read_i64(qlive, n, e)
      print '(a,l1,a,i0,a,i0,a,l1)', 'TAIL_OK_FOLLOW ok=', back, ' err=', e, &
         ' live=', n, ' dead=', conn%dead
      call check_(back .and. .not. conn%dead, 'tail echo left the connection up')
   end subroutine scenario_tail_ok

   subroutine scenario_tail_fail()
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      integer(int64) :: q, qlive
      integer(int32) :: ignored
      integer :: e
      logical :: back
      call give_peer()
      if (nfail > 0) return
      call call_begin_box(5, m, params, q, e)
      call empty_params(m, params, e)
      call rpc_call_send(conn, m, e)
      call wait_back(q, back)
      call drain(8)
      call read_i32(q, ignored, e)
      print '(a,l1,a,i0,a,l1,a,a)', 'TAIL_FAIL ok=', back, ' err=', e, &
         ' dead=', conn%dead, ' reason=', trim(rpc_conn_reason(conn))
      call check_(back .and. e == RPC_ERR_EXCEPTION .and. .not. conn%dead, &
                  'tail failure is an exception')
      if (conn%dead) return
      call call_begin_box(2, m, params, qlive, e)
      call empty_params(m, params, e)
      call rpc_call_send(conn, m, e)
      call wait_back(qlive, back)
      print '(a,l1,a,l1,a,a)', 'TAIL_FAIL_FOLLOW ok=', back, ' dead=', conn%dead, &
         ' reason=', trim(rpc_conn_reason(conn))
      call check_(back .and. .not. conn%dead, 'tail failure left the connection up')
   end subroutine scenario_tail_fail

   subroutine scenario_tail_worker()
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(capnp_ptr_t) :: s
      type(rpc_cap_t) :: got
      integer(int64) :: q, qping
      integer :: e
      integer(int32) :: val
      logical :: back
      call give_peer()
      if (nfail > 0) return
      call call_begin_box(10, m, params, q, e)
      call empty_params(m, params, e)
      call rpc_call_send(conn, m, e)
      call wait_back(q, back)
      call drain(8)
      call rpc_result_cap(conn, q, [0], got, e)
      print '(a,l1,a,i0,a,i0,a,i0,a,l1,a,a)', 'TAIL_WORKER ok=', back, &
         ' err=', e, ' kind=', got%kind, ' id=', got%id, ' dead=', conn%dead, &
         ' reason=', trim(rpc_conn_reason(conn))
      call check_(back .and. e == CAPNP_OK .and. .not. conn%dead, 'tail worker cap')
      if (nfail > 0) return
      call rpc_call_begin(conn, got, WORKER_IFACE, 0, m, params, qping, e)
      s = capnp_new_struct(m, 1, 0, e)
      call capnp_set_i32(s, 0_int64, 2_int32, e)
      call payload_content_set(params, s, e)
      call rpc_call_send(conn, m, e)
      call wait_back(qping, back)
      call read_i32(qping, val, e)
      print '(a,l1,a,i0,a,i0,a,l1,a,a)', 'TAIL_WORKER_PING ok=', back, &
         ' err=', e, ' val=', val, ' dead=', conn%dead, ' reason=', &
         trim(rpc_conn_reason(conn))
      call check_(back .and. e == CAPNP_OK .and. val == 42_int32 .and. .not. conn%dead, &
                  'tail worker ping is 42')
   end subroutine scenario_tail_worker

end program probe_client
