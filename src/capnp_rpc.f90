!> Two-party Cap'n Proto RPC, level 1: bootstrap, calls on imported and
!> promised (pipelined) capabilities, returns with capability tables,
!> finish/release bookkeeping, disembargo echo, abort, and the
!> spec-mandated `unimplemented` reply for level 3+ messages.
!>
!> The vat is single-threaded and message-driven: `rpc_pump_once`
!> processes exactly one incoming message, so a process can run both
!> sides of a socketpair deterministically (the tests do), and a plain
!> server is `do while (ok); call rpc_pump_once(conn, err); end do`.
module capnp_rpc
   use capnp
   use rpc_capnp
   use capnp_posix, only: PX_BAD_FD, px_close, px_shutdown_wr
   use capnp_rpc_transport, only: rpc_send_message, rpc_recv_message
   implicit none
   private

   public :: rpc_conn_t, rpc_server_t, rpc_cap_t, rpc_call_ctx_t
   public :: rpc_conn_init, rpc_conn_close
   public :: rpc_bootstrap_send, rpc_wait, rpc_result_content, rpc_result_cap
   public :: rpc_call_begin, rpc_call_send, rpc_pipeline_cap
   public :: rpc_finish_send, rpc_release_send
   public :: rpc_pump_once, rpc_ctx_export_cap, rpc_make_cap_ptr
   public :: rpc_conn_alive, rpc_conn_reason, rpc_cap_is_settled
   public :: RPC_CAP_NONE, RPC_CAP_IMPORT, RPC_CAP_PIPELINE
   public :: RPC_ERR_EXCEPTION, RPC_ERR_DEAD
   public :: RPC_PERSISTENT_IFACE, RPC_PERSISTENT_SAVE

   integer, parameter :: RPC_CAP_NONE = 0
   integer, parameter :: RPC_CAP_IMPORT = 1
   integer, parameter :: RPC_CAP_PIPELINE = 2

   !> Level 2 persistence: capnp/persistent.capnp's Persistent interface
   !> (id 0xc8cb212fcd9f5691) as a signed container, and its save method.
   !> A capability opts in by answering this interface id in dispatch;
   !> SturdyRef contents are application-defined, per the spec.
   integer(int64), parameter :: RPC_PERSISTENT_IFACE = -3978049356654750063_int64
   integer, parameter :: RPC_PERSISTENT_SAVE = 0

   !> Remote raised an exception; reason text is on the connection.
   integer, parameter :: RPC_ERR_EXCEPTION = 100
   !> Connection aborted or closed.
   integer, parameter :: RPC_ERR_DEAD = 101

   integer, parameter :: MAXQ = 64   ! outstanding questions/answers
   integer, parameter :: MAXE = 64   ! live exports
   integer, parameter :: MAXOPS = 8  ! pipeline ops per capability
   integer, parameter :: MAXCAPS = 16 ! caps per payload

   !> Client-side reference to a remote capability.
   type :: rpc_cap_t
      integer :: kind = RPC_CAP_NONE
      integer(int64) :: id = 0_int64 ! import id, or question id for pipelines
      integer :: nops = 0
      integer :: ops(0:MAXOPS - 1) = 0 ! getPointerField indices
   end type rpc_cap_t

   !> Unlimited-polymorphic holder so ctx and conn can reference servers
   !> without a circular type dependency.
   type :: rpc_srv_ref_t
      class(*), pointer :: s => null()
   end type rpc_srv_ref_t

   !> One incoming call, handed to a capability server's dispatch.
   type :: rpc_call_ctx_t
      integer(int64) :: interface_id = 0_int64
      integer :: method_id = 0
      type(capnp_ptr_t) :: params            ! resolved params content
      type(capnp_message_t), pointer :: rmsg => null() ! Return being built
      type(payload_t) :: results
      integer :: nexp = 0
      type(rpc_srv_ref_t) :: staged(0:MAXCAPS - 1) ! servers for the capTable
      integer :: staged_eids(0:MAXCAPS - 1) = -1   ! filled when the table flushes
   end type rpc_call_ctx_t

   !> A capability implementation hosted by this vat.
   type, abstract :: rpc_server_t
   contains
      procedure(rpc_dispatch_ifc), deferred :: dispatch
   end type rpc_server_t

   abstract interface
      subroutine rpc_dispatch_ifc(self, ctx, err)
         import :: rpc_server_t, rpc_call_ctx_t
         class(rpc_server_t), intent(inout) :: self
         type(rpc_call_ctx_t), intent(inout) :: ctx
         integer, intent(out) :: err
      end subroutine rpc_dispatch_ifc
   end interface

   type :: rpc_export_slot_t
      logical :: used = .false.
      integer :: refcount = 0
      class(*), pointer :: srv => null()
   end type rpc_export_slot_t

   type :: rpc_question_slot_t
      logical :: used = .false.
      logical :: returned = .false.
      type(capnp_message_t) :: retmsg ! the Return message, kept until finish
   end type rpc_question_slot_t

   type :: rpc_answer_slot_t
      logical :: used = .false.
      logical :: has_results = .false.
      type(capnp_message_t) :: retmsg ! copy of the Return we sent
      integer :: nexp = 0
      integer :: exports(0:MAXCAPS - 1) = -1
   end type rpc_answer_slot_t

   type :: rpc_conn_t
      integer :: fd = PX_BAD_FD
      logical :: dead = .false.
      character(len=:), allocatable :: abort_reason
      class(rpc_server_t), pointer :: bootstrap_srv => null()
      type(rpc_export_slot_t) :: exports(0:MAXE - 1)
      type(rpc_question_slot_t) :: questions(0:MAXQ - 1)
      type(rpc_answer_slot_t) :: answers(0:MAXQ - 1)
      integer(int64) :: next_qid = 0_int64
   end type rpc_conn_t

contains

   ! ------------------------------------------------------------------
   ! Lifecycle
   ! ------------------------------------------------------------------

   subroutine rpc_conn_init(conn, fd, bootstrap)
      type(rpc_conn_t), intent(inout) :: conn
      integer, intent(in) :: fd
      class(rpc_server_t), pointer, intent(in) :: bootstrap
      conn%fd = fd
      conn%dead = .false.
      conn%bootstrap_srv => bootstrap
   end subroutine rpc_conn_init

   subroutine rpc_conn_close(conn)
      type(rpc_conn_t), intent(inout) :: conn
      integer :: i
      call px_shutdown_wr(conn%fd)
      call px_close(conn%fd)
      conn%fd = PX_BAD_FD
      conn%dead = .true.
      do i = 0, MAXQ - 1
         if (conn%questions(i)%used) call capnp_message_free(conn%questions(i)%retmsg)
         if (conn%answers(i)%used) call capnp_message_free(conn%answers(i)%retmsg)
         conn%questions(i) = rpc_question_slot_t()
         conn%answers(i) = rpc_answer_slot_t()
      end do
   end subroutine rpc_conn_close

   !> Inquiry getters over connection state, so callers need not touch
   !> components.
   pure function rpc_conn_alive(conn) result(alive)
      type(rpc_conn_t), intent(in) :: conn
      logical :: alive
      alive = .not. conn%dead .and. conn%fd /= PX_BAD_FD
   end function rpc_conn_alive

   !> The abort/exception reason last recorded on the connection, or ''.
   function rpc_conn_reason(conn) result(reason)
      type(rpc_conn_t), intent(in) :: conn
      character(len=:), allocatable :: reason
      if (allocated(conn%abort_reason)) then
         reason = conn%abort_reason
      else
         reason = ''
      end if
   end function rpc_conn_reason

   pure function rpc_cap_is_settled(cap) result(settled)
      type(rpc_cap_t), intent(in) :: cap
      logical :: settled
      settled = cap%kind == RPC_CAP_IMPORT
   end function rpc_cap_is_settled

   ! ------------------------------------------------------------------
   ! Client side
   ! ------------------------------------------------------------------

   function alloc_qid(conn) result(qid)
      type(rpc_conn_t), intent(inout) :: conn
      integer(int64) :: qid
      integer :: i, k
      qid = -1_int64
      do i = 0, MAXQ - 1
         k = int(mod(conn%next_qid + int(i, int64), int(MAXQ, int64)))
         if (.not. conn%questions(k)%used) then
            conn%questions(k)%used = .true.
            conn%questions(k)%returned = .false.
            conn%next_qid = int(k + 1, int64)
            qid = int(k, int64)
            return
         end if
      end do
   end function alloc_qid

   !> Ask the peer for its bootstrap capability. The returned cap is a
   !> pipeline on the new question; calls on it work immediately, and
   !> rpc_wait + rpc_result_cap turn it into a settled import.
   subroutine rpc_bootstrap_send(conn, cap, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(rpc_cap_t), intent(out) :: cap
      integer, intent(out) :: err
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(bootstrap_t) :: b
      integer(int64) :: qid
      err = CAPNP_OK
      qid = alloc_qid(conn)
      if (qid < 0_int64) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      call capnp_message_init_builder(m, err)
      if (err /= CAPNP_OK) return
      msg = message_new_root(m, err)
      b = message_bootstrap_init(msg, err)
      call bootstrap_question_id_set(b, qid, err)
      if (err == CAPNP_OK) call rpc_send_message(conn%fd, m, err)
      call capnp_message_free(m)
      cap%kind = RPC_CAP_PIPELINE
      cap%id = qid
      cap%nops = 0
   end subroutine rpc_bootstrap_send

   !> Begin a call: builds the Call skeleton and hands back the params
   !> payload; set the content, then rpc_call_send.
   subroutine rpc_call_begin(conn, target, interface_id, method_id, m, params, qid, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(rpc_cap_t), intent(in) :: target
      integer(int64), intent(in) :: interface_id
      integer, intent(in) :: method_id
      type(capnp_message_t), intent(inout), target :: m
      type(payload_t), intent(out) :: params
      integer(int64), intent(out) :: qid
      integer, intent(out) :: err
      type(message_t) :: msg
      type(call_t) :: c
      type(message_target_t) :: tgt
      type(promised_answer_t) :: pa
      type(promised_answer_op_t) :: op
      type(capnp_ptr_t) :: ops
      integer :: i
      err = CAPNP_OK
      qid = alloc_qid(conn)
      if (qid < 0_int64) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      call capnp_message_init_builder(m, err)
      if (err /= CAPNP_OK) return
      msg = message_new_root(m, err)
      c = message_call_init(msg, err)
      call call_question_id_set(c, qid, err)
      call call_interface_id_set(c, interface_id, err)
      call call_method_id_set(c, method_id, err)
      call call_send_results_to_caller_set(c, err)
      tgt = call_target_init(c, err)
      select case (target%kind)
      case (RPC_CAP_IMPORT)
         call message_target_imported_cap_set(tgt, target%id, err)
      case (RPC_CAP_PIPELINE)
         pa = message_target_promised_answer_init(tgt, err)
         call promised_answer_question_id_set(pa, target%id, err)
         ops = promised_answer_transform_init(pa, int(target%nops, int64), err)
         do i = 0, target%nops - 1
            op%p = capnp_list_get_struct(ops, i, err)
            call promised_answer_op_get_pointer_field_set(op, target%ops(i), err)
         end do
      case default
         err = CAPNP_ERR_ARG
         return
      end select
      params = call_params_init(c, err)
   end subroutine rpc_call_begin

   subroutine rpc_call_send(conn, m, err)
      type(rpc_conn_t), intent(inout) :: conn
      type(capnp_message_t), intent(inout) :: m
      integer, intent(out) :: err
      call rpc_send_message(conn%fd, m, err)
      call capnp_message_free(m)
   end subroutine rpc_call_send

   !> A capability reached by pointer-field hops into a question's
   !> not-yet-returned results: promise pipelining.
   function rpc_pipeline_cap(qid, field_indices) result(cap)
      integer(int64), intent(in) :: qid
      integer, intent(in) :: field_indices(:)
      type(rpc_cap_t) :: cap
      integer :: i
      cap%kind = RPC_CAP_PIPELINE
      cap%id = qid
      cap%nops = min(size(field_indices), MAXOPS)
      do i = 1, cap%nops
         cap%ops(i - 1) = field_indices(i)
      end do
   end function rpc_pipeline_cap

   !> Pump the connection until question qid has returned.
   subroutine rpc_wait(conn, qid, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: qid
      integer, intent(out) :: err
      err = CAPNP_OK
      do
         if (conn%dead) then
            err = RPC_ERR_DEAD
            return
         end if
         if (conn%questions(int(qid))%returned) exit
         call rpc_pump_once(conn, err)
         if (err /= CAPNP_OK) return
      end do
   end subroutine rpc_wait

   !> Results content of a returned question. Errors with
   !> RPC_ERR_EXCEPTION (reason in conn%abort_reason) on exception
   !> returns.
   subroutine rpc_result_content(conn, qid, content, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: qid
      type(capnp_ptr_t), intent(out) :: content
      integer, intent(out) :: err
      type(message_t) :: msg
      type(return_t) :: r
      type(payload_t) :: pl
      type(exception_t) :: ex
      integer :: q
      err = CAPNP_OK
      q = int(qid)
      if (q < 0 .or. q >= MAXQ) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (.not. conn%questions(q)%returned) then
         err = CAPNP_ERR_ARG
         return
      end if
      msg = message_read_root(conn%questions(q)%retmsg, err)
      if (err /= CAPNP_OK) return
      r = message_return_get(msg, err)
      select case (return_which(r))
      case (RETURN_RESULTS_TAG)
         pl = return_results_get(r, err)
         content = payload_content_get(pl, err)
      case (RETURN_EXCEPTION_TAG)
         ex = return_exception_get(r, err)
         call exception_reason_get(ex, conn%abort_reason, err)
         err = RPC_ERR_EXCEPTION
      case default
         err = CAPNP_ERR_KIND
      end select
   end subroutine rpc_result_content

   !> Follow pointer-field hops from a returned question's content to a
   !> capability, resolving it through the answer's capTable into a
   !> settled import.
   subroutine rpc_result_cap(conn, qid, field_indices, cap, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: qid
      integer, intent(in) :: field_indices(:)
      type(rpc_cap_t), intent(out) :: cap
      integer, intent(out) :: err
      type(message_t) :: msg
      type(return_t) :: r
      type(payload_t) :: pl
      type(capnp_ptr_t) :: p, ctab
      type(cap_descriptor_t) :: cd
      integer :: i
      cap = rpc_cap_t()
      call rpc_result_content(conn, qid, p, err)
      if (err /= CAPNP_OK) return
      do i = 1, size(field_indices)
         p = capnp_getp(p, field_indices(i), err)
         if (err /= CAPNP_OK) return
      end do
      if (p%kind /= CAPNP_PK_CAP) then
         err = CAPNP_ERR_KIND
         return
      end if
      msg = message_read_root(conn%questions(int(qid))%retmsg, err)
      r = message_return_get(msg, err)
      pl = return_results_get(r, err)
      ctab = payload_cap_table_get(pl, err)
      if (err /= CAPNP_OK) return
      if (p%capidx < 0_int64 .or. p%capidx >= capnp_list_len(ctab)) then
         err = CAPNP_ERR_BOUNDS
         return
      end if
      cd%p = capnp_list_get_struct(ctab, int(p%capidx), err)
      if (err /= CAPNP_OK) return
      if (cap_descriptor_which(cd) /= CAP_DESCRIPTOR_SENDER_HOSTED_TAG) then
         err = CAPNP_ERR_KIND
         return
      end if
      cap%kind = RPC_CAP_IMPORT
      cap%id = cap_descriptor_sender_hosted_get(cd)
   end subroutine rpc_result_cap

   !> Tell the peer we are done with a question. retain_caps=.true. keeps
   !> capabilities we imported from the results alive.
   subroutine rpc_finish_send(conn, qid, retain_caps, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: qid
      logical, intent(in) :: retain_caps
      integer, intent(out) :: err
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(finish_t) :: f
      call capnp_message_init_builder(m, err)
      if (err /= CAPNP_OK) return
      msg = message_new_root(m, err)
      f = message_finish_init(msg, err)
      call finish_question_id_set(f, qid, err)
      call finish_release_result_caps_set(f, .not. retain_caps, err)
      if (err == CAPNP_OK) call rpc_send_message(conn%fd, m, err)
      call capnp_message_free(m)
      if (conn%questions(int(qid))%used) then
         call capnp_message_free(conn%questions(int(qid))%retmsg)
         conn%questions(int(qid)) = rpc_question_slot_t()
      end if
   end subroutine rpc_finish_send

   !> Drop refcount on an imported capability.
   subroutine rpc_release_send(conn, cap, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(rpc_cap_t), intent(in) :: cap
      integer, intent(out) :: err
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(release_t) :: rel
      err = CAPNP_OK
      if (cap%kind /= RPC_CAP_IMPORT) return
      call capnp_message_init_builder(m, err)
      if (err /= CAPNP_OK) return
      msg = message_new_root(m, err)
      rel = message_release_init(msg, err)
      call release_id_set(rel, cap%id, err)
      call release_reference_count_set(rel, 1_int64, err)
      if (err == CAPNP_OK) call rpc_send_message(conn%fd, m, err)
      call capnp_message_free(m)
   end subroutine rpc_release_send

   ! ------------------------------------------------------------------
   ! Server-side helpers
   ! ------------------------------------------------------------------

   !> Stage a server for the call's results capTable; returns the cap
   !> index for the content pointer. Export ids are allocated when the
   !> table flushes at send time.
   function rpc_ctx_export_cap(ctx, srv, err) result(idx)
      type(rpc_call_ctx_t), intent(inout) :: ctx
      class(rpc_server_t), pointer, intent(in) :: srv
      integer, intent(out) :: err
      integer :: idx
      err = CAPNP_OK
      idx = -1
      if (ctx%nexp >= MAXCAPS) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      ctx%staged(ctx%nexp)%s => srv
      idx = ctx%nexp
      ctx%nexp = ctx%nexp + 1
   end function rpc_ctx_export_cap

   !> Capability pointer handle for content slots: index into the
   !> payload's capTable.
   function rpc_make_cap_ptr(m, idx) result(p)
      type(capnp_message_t), intent(in), target :: m
      integer, intent(in) :: idx
      type(capnp_ptr_t) :: p
      p%kind = CAPNP_PK_CAP
      p%capidx = int(idx, int64)
      p%msg => m
   end function rpc_make_cap_ptr

   function export_server(conn, srv) result(eid)
      type(rpc_conn_t), intent(inout) :: conn
      class(*), pointer, intent(in) :: srv
      integer :: eid, i
      ! Reuse an existing export of the same server.
      do i = 0, MAXE - 1
         if (conn%exports(i)%used) then
            if (associated(conn%exports(i)%srv, srv)) then
               conn%exports(i)%refcount = conn%exports(i)%refcount + 1
               eid = i
               return
            end if
         end if
      end do
      do i = 0, MAXE - 1
         if (.not. conn%exports(i)%used) then
            conn%exports(i)%used = .true.
            conn%exports(i)%refcount = 1
            conn%exports(i)%srv => srv
            eid = i
            return
         end if
      end do
      eid = -1
   end function export_server

   ! ------------------------------------------------------------------
   ! Message pump
   ! ------------------------------------------------------------------

   !> Receive and handle exactly one message. Blocking.
   subroutine rpc_pump_once(conn, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer, intent(out) :: err
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      err = CAPNP_OK
      if (conn%dead) then
         err = RPC_ERR_DEAD
         return
      end if
      call rpc_recv_message(conn%fd, m, err)
      if (err /= CAPNP_OK) then
         conn%dead = .true.
         err = RPC_ERR_DEAD
         return
      end if
      msg = message_read_root(m, err)
      if (err /= CAPNP_OK) return
      select case (message_which(msg))
      case (MESSAGE_BOOTSTRAP_TAG)
         call handle_bootstrap(conn, msg, err)
      case (MESSAGE_CALL_TAG)
         call handle_call(conn, msg, err)
      case (MESSAGE_RETURN_TAG)
         call handle_return(conn, m, msg, err)
         return ! handle_return takes ownership of m
      case (MESSAGE_FINISH_TAG)
         call handle_finish(conn, msg, err)
      case (MESSAGE_RELEASE_TAG)
         call handle_release(conn, msg, err)
      case (MESSAGE_DISEMBARGO_TAG)
         call handle_disembargo(conn, msg, err)
      case (MESSAGE_ABORT_TAG)
         call handle_abort(conn, msg, err)
      case (MESSAGE_UNIMPLEMENTED_TAG)
         ! Peer did not understand something we sent; nothing to do at
         ! level 1 (we only send level 1 messages).
      case default
         ! Level 3+ (provide/accept/join) and obsolete messages: reply
         ! unimplemented, echoing the message, per the spec.
         call send_unimplemented(conn, msg, err)
      end select
      call capnp_message_free(m)
   end subroutine rpc_pump_once

   subroutine handle_bootstrap(conn, msg, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(message_t), intent(in) :: msg
      integer, intent(out) :: err
      type(bootstrap_t) :: b
      type(rpc_call_ctx_t) :: ctx
      type(capnp_message_t), target :: rm
      type(message_t) :: rmsg
      type(return_t) :: r
      integer(int64) :: qid
      integer :: idx
      b = message_bootstrap_get(msg, err)
      if (err /= CAPNP_OK) return
      qid = bootstrap_question_id_get(b)
      call capnp_message_init_builder(rm, err)
      if (err /= CAPNP_OK) return
      rmsg = message_new_root(rm, err)
      r = message_return_init(rmsg, err)
      call return_answer_id_set(r, qid, err)
      if (.not. associated(conn%bootstrap_srv)) then
         call fill_exception(r, 'no bootstrap capability', err)
      else
         ctx%results = return_results_init(r, err)
         idx = rpc_ctx_export_cap(ctx, conn%bootstrap_srv, err)
         if (err == CAPNP_OK) then
            call payload_content_set(ctx%results, rpc_make_cap_ptr(rm, idx), err)
            call flush_cap_table(conn, ctx, err)
         end if
      end if
      call finish_answer(conn, qid, rm, ctx, err)
   end subroutine handle_bootstrap

   subroutine handle_call(conn, msg, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(message_t), intent(in) :: msg
      integer, intent(out) :: err
      type(call_t) :: c
      type(payload_t) :: params
      type(rpc_call_ctx_t) :: ctx
      type(capnp_message_t), target :: rm
      type(message_t) :: rmsg
      type(return_t) :: r
      integer(int64) :: qid
      integer :: derr, eid
      logical :: dispatched
      c = message_call_get(msg, err)
      if (err /= CAPNP_OK) return
      qid = call_question_id_get(c)
      call capnp_message_init_builder(rm, err)
      if (err /= CAPNP_OK) return
      rmsg = message_new_root(rm, err)
      r = message_return_init(rmsg, err)
      call return_answer_id_set(r, qid, err)
      call resolve_target(conn, c, eid, err)
      if (err /= CAPNP_OK .or. eid < 0) then
         call fill_exception(r, 'no such capability', err)
         call finish_answer(conn, qid, rm, ctx, err)
         return
      end if
      ctx%interface_id = call_interface_id_get(c)
      ctx%method_id = int(call_method_id_get(c))
      params = call_params_get(c, err)
      ctx%params = payload_content_get(params, err)
      ctx%rmsg => rm
      ctx%results = return_results_init(r, err)
      dispatched = .false.
      derr = CAPNP_ERR_KIND
      select type (s => conn%exports(eid)%srv)
      class is (rpc_server_t)
         call s%dispatch(ctx, derr)
         dispatched = .true.
      end select
      if (.not. dispatched .or. derr /= CAPNP_OK) then
         ! Rebuild the union as an exception return.
         call fill_exception(r, 'method raised error', err)
      else
         call flush_cap_table(conn, ctx, err)
      end if
      call finish_answer(conn, qid, rm, ctx, err)
   end subroutine handle_call

   !> Find the export id a Call's target names: a direct export, or a
   !> promised answer resolved through a stored answer's capTable.
   !> eid = -1 when the target does not resolve.
   subroutine resolve_target(conn, c, out_eid, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(call_t), intent(in) :: c
      integer, intent(out) :: out_eid
      integer, intent(out) :: err
      type(message_target_t) :: tgt
      type(promised_answer_t) :: pa
      type(promised_answer_op_t) :: op
      type(capnp_ptr_t) :: ops, p, ctab
      type(message_t) :: amsg
      type(return_t) :: ar
      type(payload_t) :: apl
      type(cap_descriptor_t) :: cd
      integer(int64) :: aqid, eid
      integer :: i, q
      out_eid = -1
      tgt = call_target_get(c, err)
      if (err /= CAPNP_OK) return
      select case (message_target_which(tgt))
      case (MESSAGE_TARGET_IMPORTED_CAP_TAG)
         eid = message_target_imported_cap_get(tgt)
         if (eid < 0_int64 .or. eid >= int(MAXE, int64)) then
            err = CAPNP_ERR_BOUNDS
            return
         end if
         if (conn%exports(int(eid))%used) out_eid = int(eid)
      case (MESSAGE_TARGET_PROMISED_ANSWER_TAG)
         pa = message_target_promised_answer_get(tgt, err)
         if (err /= CAPNP_OK) return
         aqid = promised_answer_question_id_get(pa)
         q = int(aqid)
         if (q < 0 .or. q >= MAXQ) then
            err = CAPNP_ERR_BOUNDS
            return
         end if
         if (.not. conn%answers(q)%has_results) return
         amsg = message_read_root(conn%answers(q)%retmsg, err)
         ar = message_return_get(amsg, err)
         apl = return_results_get(ar, err)
         p = payload_content_get(apl, err)
         if (err /= CAPNP_OK) return
         ops = promised_answer_transform_get(pa, err)
         if (err /= CAPNP_OK) return
         do i = 0, int(capnp_list_len(ops)) - 1
            op%p = capnp_list_get_struct(ops, i, err)
            if (err /= CAPNP_OK) return
            if (promised_answer_op_which(op) == PROMISED_ANSWER_OP_GET_POINTER_FIELD_TAG) then
               p = capnp_getp(p, int(promised_answer_op_get_pointer_field_get(op)), err)
               if (err /= CAPNP_OK) return
            end if
         end do
         if (p%kind /= CAPNP_PK_CAP) return
         ctab = payload_cap_table_get(apl, err)
         if (err /= CAPNP_OK) return
         if (p%capidx < 0_int64 .or. p%capidx >= capnp_list_len(ctab)) return
         cd%p = capnp_list_get_struct(ctab, int(p%capidx), err)
         if (cap_descriptor_which(cd) /= CAP_DESCRIPTOR_SENDER_HOSTED_TAG) return
         eid = cap_descriptor_sender_hosted_get(cd)
         if (eid >= 0_int64 .and. eid < int(MAXE, int64)) then
            if (conn%exports(int(eid))%used) out_eid = int(eid)
         end if
      end select
   end subroutine resolve_target

   !> Allocate export ids for the staged servers and write the results
   !> capTable.
   subroutine flush_cap_table(conn, ctx, err)
      type(rpc_conn_t), intent(inout) :: conn
      type(rpc_call_ctx_t), intent(inout) :: ctx
      integer, intent(out) :: err
      type(capnp_ptr_t) :: ctab
      type(cap_descriptor_t) :: cd
      integer :: i, eid
      err = CAPNP_OK
      if (ctx%nexp == 0) return
      ctab = payload_cap_table_init(ctx%results, int(ctx%nexp, int64), err)
      if (err /= CAPNP_OK) return
      do i = 0, ctx%nexp - 1
         eid = export_server(conn, ctx%staged(i)%s)
         if (eid < 0) then
            err = CAPNP_ERR_ALLOC
            return
         end if
         ctx%staged_eids(i) = eid
         cd%p = capnp_list_get_struct(ctab, i, err)
         if (err /= CAPNP_OK) return
         call cap_descriptor_sender_hosted_set(cd, int(eid, int64), err)
         if (err /= CAPNP_OK) return
      end do
   end subroutine flush_cap_table

   !> Send the Return and stash a copy in the answers table so pipelined
   !> calls can resolve against it until Finish arrives.
   subroutine finish_answer(conn, qid, rm, ctx, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: qid
      type(capnp_message_t), intent(inout) :: rm
      type(rpc_call_ctx_t), intent(in) :: ctx
      integer, intent(out) :: err
      integer(int8), allocatable :: bytes(:)
      integer :: q, i
      call rpc_send_message(conn%fd, rm, err)
      q = int(qid)
      if (q >= 0 .and. q < MAXQ .and. err == CAPNP_OK) then
         if (conn%answers(q)%used) call capnp_message_free(conn%answers(q)%retmsg)
         call capnp_serialize_bytes(rm, bytes, err)
         if (err == CAPNP_OK) then
            call capnp_deserialize_bytes(bytes, conn%answers(q)%retmsg, err)
            conn%answers(q)%used = .true.
            conn%answers(q)%has_results = .true.
            conn%answers(q)%nexp = ctx%nexp
            do i = 0, ctx%nexp - 1
               conn%answers(q)%exports(i) = ctx%staged_eids(i)
            end do
         end if
      end if
      call capnp_message_free(rm)
   end subroutine finish_answer

   subroutine handle_return(conn, m, msg, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(capnp_message_t), intent(inout) :: m
      type(message_t), intent(in) :: msg
      integer, intent(out) :: err
      type(return_t) :: r
      integer(int64) :: qid
      integer(int8), allocatable :: bytes(:)
      integer :: q
      r = message_return_get(msg, err)
      if (err /= CAPNP_OK) return
      qid = return_answer_id_get(r)
      q = int(qid)
      if (q < 0 .or. q >= MAXQ) then
         err = CAPNP_ERR_BOUNDS
         call capnp_message_free(m)
         return
      end if
      ! Own the Return bytes in the question slot.
      call capnp_serialize_bytes(m, bytes, err)
      if (err == CAPNP_OK) then
         if (conn%questions(q)%used .and. conn%questions(q)%returned) &
            call capnp_message_free(conn%questions(q)%retmsg)
         call capnp_deserialize_bytes(bytes, conn%questions(q)%retmsg, err)
         conn%questions(q)%returned = .true.
      end if
      call capnp_message_free(m)
   end subroutine handle_return

   subroutine handle_finish(conn, msg, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(message_t), intent(in) :: msg
      integer, intent(out) :: err
      type(finish_t) :: f
      integer :: q, i, eid
      f = message_finish_get(msg, err)
      if (err /= CAPNP_OK) return
      q = int(finish_question_id_get(f))
      if (q < 0 .or. q >= MAXQ) return
      if (.not. conn%answers(q)%used) return
      if (finish_release_result_caps_get(f)) then
         do i = 0, conn%answers(q)%nexp - 1
            eid = conn%answers(q)%exports(i)
            if (eid >= 0 .and. eid < MAXE) call drop_export(conn, eid, 1)
         end do
      end if
      call capnp_message_free(conn%answers(q)%retmsg)
      conn%answers(q) = rpc_answer_slot_t()
   end subroutine handle_finish

   subroutine handle_release(conn, msg, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(message_t), intent(in) :: msg
      integer, intent(out) :: err
      type(release_t) :: rel
      integer(int64) :: eid
      rel = message_release_get(msg, err)
      if (err /= CAPNP_OK) return
      eid = release_id_get(rel)
      if (eid >= 0_int64 .and. eid < int(MAXE, int64)) &
         call drop_export(conn, int(eid), int(release_reference_count_get(rel)))
   end subroutine handle_release

   subroutine drop_export(conn, eid, count)
      type(rpc_conn_t), intent(inout) :: conn
      integer, intent(in) :: eid, count
      if (.not. conn%exports(eid)%used) return
      conn%exports(eid)%refcount = conn%exports(eid)%refcount - count
      if (conn%exports(eid)%refcount <= 0) then
         conn%exports(eid)%used = .false.
         conn%exports(eid)%srv => null()
         conn%exports(eid)%refcount = 0
      end if
   end subroutine drop_export

   !> senderLoopback disembargoes come back as receiverLoopback with the
   !> same id and target, per the level 1 embargo protocol.
   subroutine handle_disembargo(conn, msg, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(message_t), intent(in) :: msg
      integer, intent(out) :: err
      type(disembargo_t) :: d, rd
      type(message_target_t) :: tgt, rtgt
      type(capnp_message_t), target :: rm
      type(message_t) :: rmsg
      d = message_disembargo_get(msg, err)
      if (err /= CAPNP_OK) return
      if (disembargo_context_which(d) /= DISEMBARGO_CONTEXT_SENDER_LOOPBACK_TAG) then
         err = CAPNP_OK ! receiverLoopback handled by waiters; nothing here
         return
      end if
      call capnp_message_init_builder(rm, err)
      if (err /= CAPNP_OK) return
      rmsg = message_new_root(rm, err)
      rd = message_disembargo_init(rmsg, err)
      call disembargo_context_receiver_loopback_set( &
         rd, disembargo_context_sender_loopback_get(d), err)
      tgt = disembargo_target_get(d, err)
      rtgt = disembargo_target_init(rd, err)
      if (message_target_which(tgt) == MESSAGE_TARGET_IMPORTED_CAP_TAG) then
         call message_target_imported_cap_set(rtgt, &
                                              message_target_imported_cap_get(tgt), err)
      end if
      if (err == CAPNP_OK) call rpc_send_message(conn%fd, rm, err)
      call capnp_message_free(rm)
   end subroutine handle_disembargo

   subroutine handle_abort(conn, msg, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(message_t), intent(in) :: msg
      integer, intent(out) :: err
      type(exception_t) :: ex
      ex = message_abort_get(msg, err)
      if (err == CAPNP_OK) call exception_reason_get(ex, conn%abort_reason, err)
      conn%dead = .true.
      err = RPC_ERR_DEAD
   end subroutine handle_abort

   !> Echo an incomprehensible message back inside Message.unimplemented.
   subroutine send_unimplemented(conn, msg, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(message_t), intent(in) :: msg
      integer, intent(out) :: err
      type(capnp_message_t), target :: rm
      type(capnp_ptr_t) :: c, root
      call capnp_message_init_builder(rm, err)
      if (err /= CAPNP_OK) return
      root = capnp_new_struct(rm, MESSAGE_DWORDS, MESSAGE_PWORDS, err)
      call capnp_set_root(rm, root, err)
      call capnp_set_which(root, 0, MESSAGE_UNIMPLEMENTED_TAG, err)
      c = capnp_copy(rm, msg%p, err)
      if (err == CAPNP_OK) call capnp_setp(root, 0, c, err)
      if (err == CAPNP_OK) call rpc_send_message(conn%fd, rm, err)
      call capnp_message_free(rm)
   end subroutine send_unimplemented

   subroutine fill_exception(r, reason, err)
      type(return_t), intent(in) :: r
      character(len=*), intent(in) :: reason
      integer, intent(out) :: err
      type(exception_t) :: ex
      ex = return_exception_init(r, err)
      if (err /= CAPNP_OK) return
      call exception_type_set(ex, EXCEPTION_TYPE_FAILED, err)
      call exception_reason_set(ex, reason, err)
   end subroutine fill_exception

end module capnp_rpc
