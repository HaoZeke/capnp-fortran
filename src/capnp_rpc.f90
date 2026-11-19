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
   use capnp_any, only: capability_client_t
   use rpc_capnp
   ! JoinKeyPart / JoinResult are defined by the network layer, not by
   ! rpc.capnp; this vat speaks the two-party one.
   ! One network layer, not two. rpc-twoparty.capnp declares the
   ! third-party ids empty, because a two-party connection has no third
   ! vat to name; rpc-threeparty.capnp gives them content and carries the
   ! same join keys, and both modules declare the same names, so a vat
   ! speaks one or the other.
   use rpc_threeparty_capnp, only: provision_id_t, recipient_id_t, &
                                   provision_id_nonce_get, &
                                   recipient_id_nonce_get, &
                                   join_key_part_t, join_result_t, &
                                   join_key_part_new, &
                                   join_key_part_join_id_get, &
                                   join_key_part_join_id_set, &
                                   join_key_part_part_count_get, &
                                   join_key_part_part_count_set, &
                                   join_key_part_part_num_get, &
                                   join_key_part_part_num_set, &
                                   join_result_new, join_result_join_id_set, &
                                   join_result_succeeded_set, join_result_cap_set, &
                                   join_result_succeeded_get, join_result_cap_get, &
                                   third_party_cap_id_t, third_party_cap_id_new, &
                                   third_party_cap_id_vat_get, &
                                   third_party_cap_id_vat_init, &
                                   third_party_cap_id_nonce_get, &
                                   third_party_cap_id_nonce_set, &
                                   provision_id_new, provision_id_nonce_set, &
                                   recipient_id_new, recipient_id_nonce_set, &
                                   recipient_id_vat_init, &
                                   vat_id_t, vat_id_host_get, vat_id_port_get, &
                                   vat_id_host_set, vat_id_port_set
   use capnp_posix, only: PX_BAD_FD, px_tcp_connect_host
   use capnp_rpc_transport, only: rpc_send_message, rpc_recv_message, &
                                 rpc_byte_stream_t, rpc_fd_stream_t, &
                                 rpc_fd_stream_init
   implicit none
   private

   public :: rpc_conn_t, rpc_server_t, rpc_cap_t, rpc_call_ctx_t
   public :: rpc_membrane_policy_t, rpc_membrane_server_t
   public :: rpc_membrane_cap, rpc_membrane_server_init, rpc_membrane_revoke
   public :: rpc_membrane_owned_live
   public :: rpc_conn_init, rpc_conn_init_stream, rpc_conn_close
   public :: rpc_bootstrap_send, rpc_wait, rpc_result_content, rpc_result_cap
   public :: rpc_call_begin, rpc_call_send, rpc_pipeline_cap
   public :: rpc_finish_send, rpc_release_send
   public :: rpc_request_cancellation
   public :: rpc_pump_once, rpc_pump_poll, rpc_serve
   public :: rpc_ctx_export_cap, rpc_make_cap_ptr
   public :: rpc_conn_alive, rpc_conn_reason, rpc_cap_is_settled
   public :: rpc_stream_t, rpc_stream_init, rpc_stream_send, rpc_stream_finish
   public :: rpc_stream_cancel
   public :: rpc_pending_provisions, rpc_embargoed_accepts
   public :: rpc_vat_t, rpc_conn_set_vat
   public :: rpc_provide_send, rpc_accept_send, rpc_disembargo_provide_send
   public :: rpc_join_send, rpc_join_wait
   public :: rpc_introduction_t, rpc_pending_introductions, MAXHOST
   public :: rpc_introduction_done, rpc_write_third_party_cap
   public :: rpc_introduction_dial
   public :: RPC_CAP_NONE, RPC_CAP_IMPORT, RPC_CAP_PIPELINE, RPC_CAP_LOCAL
   public :: RPC_ERR_EXCEPTION, RPC_ERR_DEAD
   public :: RPC_PERSISTENT_IFACE, RPC_PERSISTENT_SAVE
   public :: rpc_persistent_save_begin, rpc_persistent_save_wait

   integer, parameter :: RPC_CAP_NONE = 0
   integer, parameter :: RPC_CAP_IMPORT = 1
   integer, parameter :: RPC_CAP_PIPELINE = 2
   !> An export this vat hosts. Calls run locally and are not sent.
   integer, parameter :: RPC_CAP_LOCAL = 3

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
   !> Question ids with this bit set are pipeline-only hints. A Return
   !> for one is ignored, matching ExportTable::isHigh in rpc.c++.
   integer(int64), parameter :: QID_HIGH_BIT = 2147483648_int64
   integer, parameter :: HIBUCKETS = 64 ! overflow-answer hash buckets
   integer, parameter :: MAXE = 64   ! live exports
   integer, parameter :: MAXOPS = 8  ! pipeline ops per capability
   integer, parameter :: MAXCAPS = 16 ! caps per payload
   integer, parameter :: MAXPROV = 32       ! capabilities held for a third vat
   integer, parameter :: MAXINTRO = 8       ! introductions handed to us
   !> Long enough for a hostname; a longer one is refused, not truncated.
   integer, parameter :: MAXHOST = 64
   integer, parameter :: MAXJOINS = 8       ! concurrent joins (level 4)
   integer, parameter :: MAXJOINPARTS = 16  ! capabilities per join
   integer, parameter :: IMP_ABSENT = 0
   integer, parameter :: IMP_HOSTED = 1
   integer, parameter :: IMP_PROMISE = 2
   integer, parameter :: IMP_REMOTE = 3
   integer, parameter :: IMP_LOCAL = 4
   integer, parameter :: IMP_EMBARGO = 5
   integer, parameter :: IMP_BROKEN = 6

   !> Policy shared by every wrapper belonging to one membrane. Reusing
   !> the same object gives all wrapped capabilities one revocation
   !> switch, matching capnp-C++ MembranePolicy identity.
   type, abstract :: rpc_membrane_policy_t
      logical :: revoked = .false.
      character(len=:), allocatable :: revoke_reason
   contains
      procedure(rpc_membrane_check_ifc), deferred :: inbound_call
      procedure(rpc_membrane_check_ifc), deferred :: outbound_call
   end type rpc_membrane_policy_t

   !> Client-side reference to a remote capability.
   type :: rpc_cap_t
      integer :: kind = RPC_CAP_NONE
      integer(int64) :: id = 0_int64 ! import id, or question id for pipelines
      integer :: nops = 0
      integer :: ops(0:MAXOPS - 1) = 0 ! getPointerField indices
      class(rpc_membrane_policy_t), pointer :: membrane => null()
      logical :: membrane_reverse = .false.
   end type rpc_cap_t

   !> Unlimited-polymorphic holder so ctx and conn can reference servers
   !> without a circular type dependency.
   type :: rpc_srv_ref_t
      class(*), pointer :: s => null()
      !> True when this vat allocated the server and must free it.
      logical :: owned = .false.
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

   !> Server capability crossing a membrane. Calls are checked in the
   !> crossing direction and capabilities staged in results are wrapped
   !> recursively in the same membrane.
   type, extends(rpc_server_t) :: rpc_membrane_server_t
      class(rpc_server_t), pointer :: inner => null()
      class(rpc_membrane_policy_t), pointer :: policy => null()
      logical :: reverse = .false.
      logical :: owned = .false.
      !> Live export slots and provisions holding this owned wrapper.
      integer :: refs = 0
   contains
      procedure :: dispatch => membrane_dispatch
      final :: membrane_owned_final
   end type rpc_membrane_server_t

   !> Owned wrappers allocated by a membrane. Tests use this to see that
   !> unwrapping frees the wrapper instead of leaving it behind.
   integer :: g_membrane_owned_live = 0

   abstract interface
      subroutine rpc_dispatch_ifc(self, ctx, err)
         import :: rpc_server_t, rpc_call_ctx_t
         class(rpc_server_t), intent(inout) :: self
         type(rpc_call_ctx_t), intent(inout) :: ctx
         integer, intent(out) :: err
      end subroutine rpc_dispatch_ifc

      subroutine rpc_membrane_check_ifc(self, interface_id, method_id, err)
         import :: rpc_membrane_policy_t, int64
         class(rpc_membrane_policy_t), intent(inout) :: self
         integer(int64), intent(in) :: interface_id
         integer, intent(in) :: method_id
         integer, intent(out) :: err
      end subroutine rpc_membrane_check_ifc

      !> Return allow=.false. to refuse a third-party dial. A null hook
      !> denies every recorded introduction.
      subroutine rpc_dial_allow_ifc(host, port, allow)
         character(len=*), intent(in) :: host
         integer, intent(in) :: port
         logical, intent(out) :: allow
      end subroutine rpc_dial_allow_ifc
   end interface

   type :: rpc_export_slot_t
      logical :: used = .false.
      integer :: refcount = 0
      class(*), pointer :: srv => null()
      !> True when dropping the last reference must deallocate srv.
      logical :: owned = .false.
      !> Dispatches of this server that have been entered and not left.
      !> drop_export will not deallocate while this is positive.
      integer :: dispatching = 0
   end type rpc_export_slot_t

   type :: rpc_question_slot_t
      logical :: used = .false.
      logical :: returned = .false.
      !> Finish arrived before Return. The id stays reserved so a later
      !> call cannot observe this question's Return.
      logical :: canceled = .false.
      !> The Call asked for sendResultsTo.yourself.
      logical :: tail_call = .false.
      !> The call was delivered to a local capability, not sent on the wire.
      logical :: local_call = .false.
      !> Caps in retmsg are exports this vat hosts, not imports.
      logical :: local_results = .false.
      !> Set when the call target is a local export, before send.
      integer :: local_export = -1
      type(capnp_message_t) :: retmsg ! the Return message, kept until finish
      class(rpc_membrane_policy_t), pointer :: membrane => null()
      logical :: membrane_reverse = .false.
   end type rpc_question_slot_t

   type :: rpc_answer_slot_t
      logical :: used = .false.
      logical :: has_results = .false.
      !> Set only for a sendResultsTo.yourself call whose results are
      !> held for a later takeFromOtherQuestion.
      logical :: redirected = .false.
      type(capnp_message_t) :: retmsg ! copy of the Return we sent
      integer :: nexp = 0
      integer :: exports(0:MAXCAPS - 1) = -1
   end type rpc_answer_slot_t

   !> One inbound answer whose question id does not fit in `answers`.
   !> The node stays until Finish so a capability in the results can
   !> still be called. Nodes hang off a hash bucket, the same shape as
   !> the high-id map in C++ ImportTable, so a lookup does not walk
   !> every overflow answer.
   type :: rpc_hi_node_t
      integer(int64) :: qid = -1_int64
      type(rpc_answer_slot_t) :: slot
      type(rpc_hi_node_t), pointer :: next => null()
   end type rpc_hi_node_t

   !> One hash bucket. A pointer component cannot be an explicit-shape
   !> array, so the head lives in a scalar wrapper.
   type :: rpc_hi_head_t
      type(rpc_hi_node_t), pointer :: head => null()
   end type rpc_hi_head_t

   !> One import id this connection has been shown.
   !>
   !> A senderPromise stays unresolved until Resolve. A later call on
   !> that id follows a remote resolution. A resolution back to a
   !> capability this vat hosts, after a call on the promise, issues an
   !> embargo id and waits for receiverLoopback.
   type :: rpc_import_node_t
      integer(int64) :: id = -1_int64
      integer :: state = IMP_ABSENT
      logical :: received_call = .false.
      integer(int64) :: resolved_id = -1_int64
      integer(int64) :: embargo_id = -1_int64
      !> Remote references this vat still owes a Release for.
      integer :: refs = 0
      logical :: released = .false.
      character(len=:), allocatable :: reason
      type(rpc_import_node_t), pointer :: next => null()
   end type rpc_import_node_t

   !> A call held until an embargo lifts, then delivered locally.
   type :: rpc_queued_call_t
      type(capnp_message_t) :: msg
      integer(int64) :: qid = -1_int64
      type(rpc_queued_call_t), pointer :: next => null()
   end type rpc_queued_call_t

   !> A pipeline call waiting on a local question that has not returned.
   type :: rpc_pipe_wait_t
      integer(int64) :: target_qid = -1_int64
      integer(int64) :: call_qid = -1_int64
      type(capnp_message_t) :: msg
      type(rpc_pipe_wait_t), pointer :: next => null()
   end type rpc_pipe_wait_t

   !> An embargo id this vat has sent in Disembargo.senderLoopback.
   type :: rpc_embargo_node_t
      integer(int64) :: id = -1_int64
      integer(int64) :: promise_id = -1_int64
      logical :: open = .false.
      type(rpc_queued_call_t), pointer :: queued => null()
      type(rpc_embargo_node_t), pointer :: next => null()
   end type rpc_embargo_node_t

   !> A capability promised to a third vat (level 3), awaiting its Accept.
   !>
   !> The introducer told us to expect someone, and the nonce is the whole
   !> of the arrangement. Matching on it alone is what lets the recipient
   !> claim the capability without us having to trust her account of who
   !> sent her.
   type :: rpc_provision_slot_t
      logical :: used = .false.
      integer(int64) :: nonce = 0_int64
      !> The capability itself, not this connection's id for it: the
      !> recipient may well arrive on another connection, where that id
      !> means nothing.
      class(*), pointer :: srv => null()
      integer :: eid = -1
      !> Export on `host` that the provision pins. Accept must not drop
      !> the capability while this pin is held, and must not free the
      !> wrapper when the recipient's own export is released.
      integer :: source_eid = -1
      type(rpc_conn_t), pointer :: host => null()
      !> The connection that sent Accept. Its question and export ids are
      !> not ids on `host`.
      type(rpc_conn_t), pointer :: recipient => null()
      !> The introducer's Provide question, which is how a later
      !> Disembargo names this arrangement (rpc.capnp,
      !> Disembargo.context.provide).
      integer(int64) :: qid = -1_int64
      !> An embargoed Accept has claimed the capability but must not be
      !> answered until the introducer lifts the embargo, so the slot
      !> outlives the claim and carries the answer to send.
      logical :: embargoed = .false.
      integer(int64) :: accept_qid = -1_int64
   end type rpc_provision_slot_t

   !> An introduction we have been handed but not yet picked up.
   !>
   !> Level 3: a payload can name a capability hosted by a third vat, as
   !> a `thirdPartyHosted` CapDescriptor carrying where to go and a vine.
   !> The vine is an ordinary import through the introducer, so calls
   !> work before we ever reach the third party; that is the fallback the
   !> spec gives level 1 and 2 receivers, and it is why the vine must not
   !> be released until the pickup succeeds. Connecting is the network
   !> layer's job, so the vat records the introduction and hands it over.
   type :: rpc_introduction_t
      logical :: used = .false.
      integer(int64) :: nonce = 0_int64
      integer(int64) :: vine_id = -1_int64
      integer :: port = 0
      character(len=MAXHOST) :: host = ''
      integer :: host_len = 0
   end type rpc_introduction_t

   !> One in-flight Join (level 4), keyed by the sender's joinId.
   !>
   !> A Join asks whether several capabilities are the same object. The
   !> parts arrive as separate questions, so the answer to any one of them
   !> is only known once all `part_count` have been seen: the receiver
   !> waits for the whole set, compares, then answers every part
   !> (rpc-twoparty.capnp, JoinResult).
   type :: rpc_join_slot_t
      logical :: used = .false.
      integer(int64) :: join_id = -1_int64
      integer :: part_count = 0
      integer :: nseen = 0
      logical :: seen(0:MAXJOINPARTS - 1) = .false.
      integer(int64) :: qids(0:MAXJOINPARTS - 1) = -1_int64
      integer :: eids(0:MAXJOINPARTS - 1) = -1
   end type rpc_join_slot_t

   !> Client-side flow control for `-> stream` methods. The window is a
   !> byte budget, matching RpcFlowController::DEFAULT_WINDOW_SIZE. The
   !> first call is sent even when it is larger than the window; the next
   !> waits until acknowledged bytes bring the in-flight total back under
   !> it. After any stream call fails, later sends fail immediately.
   type :: rpc_stream_t
      integer :: window = 65536
      integer :: nout = 0
      integer :: used_bytes = 0
      integer :: sizes(0:63) = 0
      integer(int64) :: qids(0:63) = -1_int64
      logical :: failed = .false.
   end type rpc_stream_t

   !> What a vat knows across all its connections.
   !>
   !> A level 3 handoff is arranged on one connection and claimed on
   !> another: the introducer sends `Provide` over its own, and the
   !> recipient arrives on hers. Holding the arrangement on the
   !> connection would make it claimable only by the introducer, which is
   !> no handoff at all. Connections given no vat get one to themselves,
   !> which is what a two-party deployment wants.
   type :: rpc_vat_t
      type(rpc_provision_slot_t) :: provisions(0:MAXPROV - 1)
   end type rpc_vat_t

   type :: rpc_conn_t
      integer :: fd = PX_BAD_FD
      class(rpc_byte_stream_t), pointer :: transport => null()
      type(rpc_fd_stream_t) :: own_transport
      logical :: dead = .false.
      character(len=:), allocatable :: abort_reason
      class(rpc_server_t), pointer :: bootstrap_srv => null()
      type(rpc_export_slot_t) :: exports(0:MAXE - 1)
      type(rpc_question_slot_t) :: questions(0:MAXQ - 1)
      type(rpc_answer_slot_t) :: answers(0:MAXQ - 1)
      !> Inbound answers with question ids at or above MAXQ, hashed.
      !> The bucket count is fixed. The node count is not: C++ 1.4.0
      !> stores the same answers in an unbounded hash map.
      type(rpc_hi_head_t) :: hi_buckets(0:HIBUCKETS - 1)
      !> Imports named by cap descriptors in Returns we have accepted.
      type(rpc_import_node_t), pointer :: imports => null()
      !> Embargo ids issued by a reflected Resolve. receiverLoopback is
      !> accepted only for an id on this list.
      type(rpc_embargo_node_t), pointer :: embargoes => null()
      type(rpc_pipe_wait_t), pointer :: pipe_wait => null()
      integer(int64) :: next_embargo = 0_int64
      !> Shared with this vat's other connections when one is attached;
      !> otherwise `own_vat` below.
      type(rpc_vat_t), pointer :: vat => null()
      type(rpc_vat_t) :: own_vat
      type(rpc_introduction_t) :: introductions(0:MAXINTRO - 1)
      type(rpc_join_slot_t) :: joins(0:MAXJOINS - 1)
      integer(int64) :: next_qid = 0_int64
      !> Null denies every dial. A hook must allow the host before connect.
      procedure(rpc_dial_allow_ifc), pointer, nopass :: dial_allow => null()
      !> Connect timeout for an introduction dial, in milliseconds.
      integer :: dial_timeout_ms = 10000
   end type rpc_conn_t

contains

   ! ------------------------------------------------------------------
   ! Lifecycle
   ! ------------------------------------------------------------------

   subroutine rpc_conn_init(conn, fd, bootstrap)
      type(rpc_conn_t), intent(inout), target :: conn
      integer, intent(in) :: fd
      class(rpc_server_t), pointer, intent(in) :: bootstrap
      if (associated(conn%transport)) call rpc_conn_close(conn)
      conn%fd = fd
      call rpc_fd_stream_init(conn%own_transport, fd, .true.)
      conn%transport => conn%own_transport
      conn%dead = .false.
      if (allocated(conn%abort_reason)) deallocate (conn%abort_reason)
      conn%bootstrap_srv => bootstrap
      conn%own_vat = rpc_vat_t()
      conn%introductions = rpc_introduction_t()
      conn%joins = rpc_join_slot_t()
      conn%next_qid = 0_int64
      conn%next_embargo = 0_int64
      conn%dial_allow => null()
      conn%dial_timeout_ms = 10000
      ! Its own vat until told otherwise, which is what a two-party
      ! deployment wants.
      conn%vat => conn%own_vat
   end subroutine rpc_conn_init

   !> Initialize RPC over an application-defined byte stream. The stream
   !> must outlive the connection and its close binding defines ownership.
   subroutine rpc_conn_init_stream(conn, stream, bootstrap)
      type(rpc_conn_t), intent(inout), target :: conn
      class(rpc_byte_stream_t), intent(inout), target :: stream
      class(rpc_server_t), pointer, intent(in) :: bootstrap
      if (associated(conn%transport)) call rpc_conn_close(conn)
      conn%fd = PX_BAD_FD
      select type (stream)
      type is (rpc_fd_stream_t)
         conn%fd = stream%fd
      end select
      conn%transport => stream
      conn%dead = .false.
      if (allocated(conn%abort_reason)) deallocate (conn%abort_reason)
      conn%bootstrap_srv => bootstrap
      conn%own_vat = rpc_vat_t()
      conn%introductions = rpc_introduction_t()
      conn%joins = rpc_join_slot_t()
      conn%next_qid = 0_int64
      conn%next_embargo = 0_int64
      conn%dial_allow => null()
      conn%dial_timeout_ms = 10000
      conn%vat => conn%own_vat
   end subroutine rpc_conn_init_stream

   subroutine rpc_conn_close(conn)
      type(rpc_conn_t), intent(inout), target :: conn
      integer :: i
      if (associated(conn%transport)) call conn%transport%close()
      conn%transport => null()
      conn%fd = PX_BAD_FD
      conn%dead = .true.
      ! The peer will not Release what it still imports. An export whose
      ! dispatch is on the stack keeps that call's reference and is freed
      ! when the call returns. Every other owned server is freed here.
      do i = 0, MAXE - 1
         if (.not. conn%exports(i)%used) cycle
         if (conn%exports(i)%dispatching > 0) then
            conn%exports(i)%refcount = conn%exports(i)%dispatching
         else
            call drop_export(conn, i, conn%exports(i)%refcount)
         end if
      end do
      do i = 0, MAXQ - 1
         if (conn%questions(i)%used) call capnp_message_free(conn%questions(i)%retmsg)
         if (conn%answers(i)%used) call capnp_message_free(conn%answers(i)%retmsg)
         call question_slot_clear(conn%questions(i))
         call answer_slot_clear(conn%answers(i))
      end do
      call free_hi_answers(conn)
      if (associated(conn%vat)) call release_hosted_provisions(conn)
      do i = 0, MAXE - 1
         if (conn%exports(i)%used) call drop_export(conn, i, conn%exports(i)%refcount)
      end do
      call free_pipe_waits(conn)
      call clear_joins(conn)
   end subroutine rpc_conn_close

   !> Inquiry getters over connection state, so callers need not touch
   !> components.
   pure function rpc_conn_alive(conn) result(alive)
      type(rpc_conn_t), intent(in) :: conn
      logical :: alive
      alive = .not. conn%dead .and. associated(conn%transport)
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
   ! Membranes
   ! ------------------------------------------------------------------

   !> Wrap a client capability for use outside a membrane. Set reverse
   !> when the original capability is outside and the result is for use
   !> inside.
   function rpc_membrane_cap(cap, policy, reverse) result(wrapped)
      type(rpc_cap_t), intent(in) :: cap
      class(rpc_membrane_policy_t), pointer, intent(in) :: policy
      logical, intent(in), optional :: reverse
      type(rpc_cap_t) :: wrapped
      wrapped = cap
      wrapped%membrane => policy
      wrapped%membrane_reverse = .false.
      if (present(reverse)) wrapped%membrane_reverse = reverse
   end function rpc_membrane_cap

   !> Initialize a server wrapper. `inner` and `policy` must outlive the
   !> wrapper and every result capability exported through it.
   subroutine rpc_membrane_server_init(wrapper, inner, policy, reverse)
      type(rpc_membrane_server_t), intent(out) :: wrapper
      class(rpc_server_t), pointer, intent(in) :: inner
      class(rpc_membrane_policy_t), pointer, intent(in) :: policy
      logical, intent(in), optional :: reverse
      wrapper%inner => inner
      wrapper%policy => policy
      wrapper%reverse = .false.
      wrapper%owned = .false.
      if (present(reverse)) wrapper%reverse = reverse
   end subroutine rpc_membrane_server_init

   !> Revoke every client and server wrapper sharing this policy.
   subroutine rpc_membrane_revoke(policy, reason)
      class(rpc_membrane_policy_t), intent(inout) :: policy
      character(len=*), intent(in), optional :: reason
      policy%revoked = .true.
      if (present(reason)) then
         policy%revoke_reason = reason
      else
         policy%revoke_reason = 'membrane revoked'
      end if
   end subroutine rpc_membrane_revoke

   subroutine membrane_check(policy, reverse, interface_id, method_id, err)
      class(rpc_membrane_policy_t), pointer, intent(in) :: policy
      logical, intent(in) :: reverse
      integer(int64), intent(in) :: interface_id
      integer, intent(in) :: method_id
      integer, intent(out) :: err
      if (.not. associated(policy)) then
         err = CAPNP_ERR_ARG
      else if (policy%revoked) then
         err = RPC_ERR_EXCEPTION
      else if (reverse) then
         call policy%outbound_call(interface_id, method_id, err)
      else
         call policy%inbound_call(interface_id, method_id, err)
      end if
   end subroutine membrane_check

   !> A nested membrane calls this again through the inner server.
   recursive subroutine membrane_dispatch(self, ctx, err)
      class(rpc_membrane_server_t), intent(inout) :: self
      type(rpc_call_ctx_t), intent(inout) :: ctx
      integer, intent(out) :: err
      integer :: i
      if (.not. associated(self%inner) .or. .not. associated(self%policy)) then
         err = CAPNP_ERR_ARG
         return
      end if
      call membrane_check(self%policy, self%reverse, ctx%interface_id, &
                          ctx%method_id, err)
      if (err /= CAPNP_OK) return
      call self%inner%dispatch(ctx, err)
      if (err /= CAPNP_OK) return
      do i = 0, ctx%nexp - 1
         call membrane_wrap_server_ref(ctx%staged(i)%s, self%policy, &
                                       self%reverse, err)
         if (err /= CAPNP_OK) then
            call discard_unexported_wrappers(ctx)
            return
         end if
      end do
   end subroutine membrane_dispatch

   !> Wrap a result capability crossing the membrane. A wrapper crossing
   !> back through the same policy is unwrapped instead of doubled.
   subroutine membrane_wrap_server_ref(srv, policy, reverse, err)
      class(*), pointer, intent(inout) :: srv
      class(rpc_membrane_policy_t), pointer, intent(in) :: policy
      logical, intent(in) :: reverse
      integer, intent(out) :: err
      type(rpc_membrane_server_t), pointer :: wrapper
      class(rpc_server_t), pointer :: inner
      err = CAPNP_OK
      select type (existing => srv)
      type is (rpc_membrane_server_t)
         if (associated(existing%policy, policy) .and. &
             existing%reverse .neqv. reverse) then
            inner => existing%inner
            ! This pointer is the only holder of an owned wrapper that
            ! was never exported. Dropping it on the floor leaks the
            ! wrapper, and a later use of that memory is not the inner.
            if (existing%owned .and. existing%refs <= 0) deallocate (existing)
            srv => inner
            return
         end if
      end select
      select type (typed => srv)
      class is (rpc_server_t)
         inner => typed
      class default
         err = CAPNP_ERR_KIND
         return
      end select
      allocate (wrapper)
      call rpc_membrane_server_init(wrapper, inner, policy, reverse)
      wrapper%owned = .true.
      wrapper%refs = 0
      g_membrane_owned_live = g_membrane_owned_live + 1
      srv => wrapper
   end subroutine membrane_wrap_server_ref

   subroutine membrane_owned_final(self)
      type(rpc_membrane_server_t), intent(inout) :: self
      if (self%owned) g_membrane_owned_live = g_membrane_owned_live - 1
   end subroutine membrane_owned_final

   function rpc_membrane_owned_live() result(n)
      integer :: n
      n = g_membrane_owned_live
   end function rpc_membrane_owned_live

   !> One holder of an owned membrane. Unowned servers are the caller's.
   subroutine retain_server(srv)
      class(*), pointer, intent(in) :: srv
      if (.not. associated(srv)) return
      select type (srv)
      type is (rpc_membrane_server_t)
         if (srv%owned) srv%refs = srv%refs + 1
      end select
   end subroutine retain_server

   !> Drop one holder. The last holder frees the wrapper.
   subroutine release_server(srv)
      class(*), pointer, intent(inout) :: srv
      if (.not. associated(srv)) return
      select type (srv)
      type is (rpc_membrane_server_t)
         if (srv%owned) then
            srv%refs = srv%refs - 1
            if (srv%refs <= 0) deallocate (srv)
         end if
      end select
      srv => null()
   end subroutine release_server

   !> Free an owned membrane wrapper that never reached an export slot.
   !> The inner server is left alone: it may be a caller-owned object.
   subroutine discard_unexported_wrapper(srv)
      class(*), pointer, intent(inout) :: srv
      if (.not. associated(srv)) return
      select type (srv)
      type is (rpc_membrane_server_t)
         if (srv%owned .and. srv%refs <= 0) then
            deallocate (srv)
            srv => null()
         end if
      end select
   end subroutine discard_unexported_wrapper

   subroutine discard_unexported_wrappers(ctx)
      type(rpc_call_ctx_t), intent(inout) :: ctx
      integer :: i
      do i = 0, ctx%nexp - 1
         if (ctx%staged_eids(i) < 0) call discard_unexported_wrapper(ctx%staged(i)%s)
      end do
   end subroutine discard_unexported_wrappers

   !> Drop exports this call already published and free wrappers that
   !> never got an id, so a failed flush does not leave them behind.
   subroutine rollback_staged(conn, ctx)
      type(rpc_conn_t), intent(inout) :: conn
      type(rpc_call_ctx_t), intent(inout) :: ctx
      integer :: i
      do i = 0, ctx%nexp - 1
         if (ctx%staged_eids(i) >= 0) then
            call drop_export(conn, ctx%staged_eids(i), 1)
            ctx%staged_eids(i) = -1
         else
            call discard_unexported_wrapper(ctx%staged(i)%s)
         end if
      end do
      ctx%nexp = 0
   end subroutine rollback_staged

   !> Drop both pins a provision took in handle_provide.
   subroutine release_provision_pin(slot)
      type(rpc_provision_slot_t), intent(inout) :: slot
      class(*), pointer :: held
      if (associated(slot%host) .and. slot%source_eid >= 0 .and. &
          slot%source_eid < MAXE) then
         call drop_export(slot%host, slot%source_eid, 1)
      end if
      held => slot%srv
      slot%srv => null()
      call release_server(held)
      slot%host => null()
      slot%source_eid = -1
   end subroutine release_provision_pin

   !> Provisions this connection introduced, and embargoed Accepts whose
   !> answer was going to be sent here. Either side closing ends the
   !> arrangement. A closed recipient is not answered: its transport is
   !> already gone, and a later Finish must not send through it.
   subroutine release_hosted_provisions(conn)
      type(rpc_conn_t), intent(inout), target :: conn
      integer :: i, ferr
      do i = 0, MAXPROV - 1
         if (.not. conn%vat%provisions(i)%used) cycle
         if (associated(conn%vat%provisions(i)%host, conn)) then
            call abandon_provision(conn%vat%provisions(i), ferr)
         else if (associated(conn%vat%provisions(i)%recipient, conn)) then
            call release_provision_pin(conn%vat%provisions(i))
            conn%vat%provisions(i) = rpc_provision_slot_t()
         end if
      end do
   end subroutine release_hosted_provisions

   !> Drop a provision. An embargoed Accept is answered on the recipient
   !> connection, and that connection's export is the one released.
   subroutine abandon_provision(slot, err)
      type(rpc_provision_slot_t), intent(inout) :: slot
      integer, intent(out) :: err
      type(rpc_conn_t), pointer :: recip
      integer :: ae
      integer(int64) :: aq
      err = CAPNP_OK
      recip => null()
      ae = -1
      aq = -1_int64
      if (slot%embargoed) then
         recip => slot%recipient
         ae = slot%eid
         aq = slot%accept_qid
      end if
      call release_provision_pin(slot)
      slot = rpc_provision_slot_t()
      if (.not. associated(recip)) return
      ! The recipient already closed. Its exports go with that close,
      ! and there is no transport left to answer.
      if (.not. rpc_conn_alive(recip)) return
      if (ae >= 0) call drop_export(recip, ae, 1)
      call send_answer_exception(recip, aq, 'provide finished', err)
   end subroutine abandon_provision

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
            conn%questions(k)%canceled = .false.
            conn%questions(k)%tail_call = .false.
            conn%questions(k)%local_call = .false.
            conn%questions(k)%local_results = .false.
            conn%questions(k)%local_export = -1
            conn%questions(k)%membrane => null()
            conn%questions(k)%membrane_reverse = .false.
            conn%next_qid = int(k + 1, int64)
            qid = int(k, int64)
            return
         end if
      end do
   end function alloc_qid

   !> Ask the peer for its bootstrap capability. The returned cap is a
   !> pipeline on the new question; calls on it work immediately, and
   !> rpc_wait + rpc_result_cap turn it into a settled import.
   !> `Provide`: ask the peer to hold `imported_cap` for a third vat.
   !>
   !> This is the introducer's half of a handoff. The nonce is the whole
   !> of the arrangement: the recipient presents it in an `Accept`, and
   !> the host matches on it alone, so it never has to take the
   !> recipient's word for who sent her. `qid` comes back because a later
   !> `Disembargo.provide` names it.
   subroutine rpc_provide_send(conn, imported_cap, host, port, nonce, qid, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: imported_cap, nonce
      character(len=*), intent(in) :: host
      integer, intent(in) :: port
      integer(int64), intent(out) :: qid
      integer, intent(out) :: err
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(provide_t) :: pv
      type(message_target_t) :: tgt
      type(recipient_id_t) :: rid
      type(vat_id_t) :: vat
      err = CAPNP_OK
      qid = alloc_qid(conn)
      if (qid < 0_int64) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      call capnp_message_init_builder(m, err)
      if (err == CAPNP_OK) msg = message_new_root(m, err)
      if (err == CAPNP_OK) pv = message_provide_init(msg, err)
      if (err == CAPNP_OK) call provide_question_id_set(pv, qid, err)
      if (err == CAPNP_OK) tgt = provide_target_init(pv, err)
      if (err == CAPNP_OK) call message_target_imported_cap_set(tgt, imported_cap, err)
      if (err == CAPNP_OK) rid = recipient_id_new(m, err)
      if (err == CAPNP_OK) call recipient_id_nonce_set(rid, nonce, err)
      if (err == CAPNP_OK) vat = recipient_id_vat_init(rid, err)
      if (err == CAPNP_OK) call vat_id_host_set(vat, host, err)
      if (err == CAPNP_OK) call vat_id_port_set(vat, port, err)
      if (err == CAPNP_OK) call provide_recipient_set(pv, rid%p, err)
      if (err == CAPNP_OK) call rpc_send_message(conn%transport, m, err)
      call capnp_message_free(m)
      if (err /= CAPNP_OK) call release_question(conn, qid)
   end subroutine rpc_provide_send

   !> `Accept`: claim a capability a third vat provided for us. The
   !> answer to `qid` carries the capability.
   subroutine rpc_accept_send(conn, nonce, embargo, qid, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: nonce
      logical, intent(in) :: embargo
      integer(int64), intent(out) :: qid
      integer, intent(out) :: err
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(accept_t) :: ac
      type(provision_id_t) :: pid
      err = CAPNP_OK
      qid = alloc_qid(conn)
      if (qid < 0_int64) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      call capnp_message_init_builder(m, err)
      if (err == CAPNP_OK) msg = message_new_root(m, err)
      if (err == CAPNP_OK) ac = message_accept_init(msg, err)
      if (err == CAPNP_OK) call accept_question_id_set(ac, qid, err)
      if (err == CAPNP_OK .and. embargo) call accept_embargo_set(ac, .true., err)
      if (err == CAPNP_OK) pid = provision_id_new(m, err)
      if (err == CAPNP_OK) call provision_id_nonce_set(pid, nonce, err)
      if (err == CAPNP_OK) call accept_provision_set(ac, pid%p, err)
      if (err == CAPNP_OK) call rpc_send_message(conn%transport, m, err)
      call capnp_message_free(m)
      if (err /= CAPNP_OK) call release_question(conn, qid)
   end subroutine rpc_accept_send

   !> Lift the embargo on the Accept this vat arranged with `Provide`.
   subroutine rpc_disembargo_provide_send(conn, provide_qid, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: provide_qid
      integer, intent(out) :: err
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(disembargo_t) :: d
      call capnp_message_init_builder(m, err)
      if (err /= CAPNP_OK) return
      msg = message_new_root(m, err)
      if (err /= CAPNP_OK) return
      d = message_disembargo_init(msg, err)
      if (err /= CAPNP_OK) return
      call disembargo_context_provide_set(d, provide_qid, err)
      if (err == CAPNP_OK) call rpc_send_message(conn%transport, m, err)
      call capnp_message_free(m)
   end subroutine rpc_disembargo_provide_send

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
      if (err == CAPNP_OK) msg = message_new_root(m, err)
      if (err == CAPNP_OK) b = message_bootstrap_init(msg, err)
      if (err == CAPNP_OK) call bootstrap_question_id_set(b, qid, err)
      if (err == CAPNP_OK) call rpc_send_message(conn%transport, m, err)
      call capnp_message_free(m)
      if (err /= CAPNP_OK) then
         call release_question(conn, qid)
         cap = rpc_cap_t()
         return
      end if
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
      if (associated(target%membrane)) then
         call membrane_check(target%membrane, target%membrane_reverse, &
                             interface_id, method_id, err)
         if (err /= CAPNP_OK) then
            if (target%membrane%revoked .and. &
                allocated(target%membrane%revoke_reason)) &
               conn%abort_reason = target%membrane%revoke_reason
            qid = -1_int64
            return
         end if
      end if
      qid = alloc_qid(conn)
      if (qid < 0_int64) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      conn%questions(int(qid))%membrane => target%membrane
      conn%questions(int(qid))%membrane_reverse = target%membrane_reverse
      call capnp_message_init_builder(m, err)
      if (err == CAPNP_OK) msg = message_new_root(m, err)
      if (err == CAPNP_OK) c = message_call_init(msg, err)
      if (err == CAPNP_OK) call call_question_id_set(c, qid, err)
      if (err == CAPNP_OK) call call_interface_id_set(c, interface_id, err)
      if (err == CAPNP_OK) call call_method_id_set(c, method_id, err)
      if (err == CAPNP_OK) call call_send_results_to_caller_set(c, err)
      if (err == CAPNP_OK) tgt = call_target_init(c, err)
      if (err == CAPNP_OK) then
         select case (target%kind)
         case (RPC_CAP_IMPORT)
            call message_target_imported_cap_set(tgt, target%id, err)
         case (RPC_CAP_LOCAL)
            ! The call never leaves this vat. The imported-cap slot is
            ! only here so the message is well formed until send.
            call message_target_imported_cap_set(tgt, target%id, err)
            conn%questions(int(qid))%local_export = int(target%id)
            conn%questions(int(qid))%local_call = .true.
         case (RPC_CAP_PIPELINE)
            pa = message_target_promised_answer_init(tgt, err)
            if (err == CAPNP_OK) call promised_answer_question_id_set(pa, target%id, err)
            if (err == CAPNP_OK) ops = promised_answer_transform_init(pa, int(target%nops, int64), err)
            if (err == CAPNP_OK) then
               do i = 0, target%nops - 1
                  op%p = capnp_list_get_struct(ops, i, err)
                  if (err /= CAPNP_OK) exit
                  call promised_answer_op_get_pointer_field_set(op, target%ops(i), err)
                  if (err /= CAPNP_OK) exit
               end do
            end if
         case default
            err = CAPNP_ERR_ARG
         end select
      end if
      if (err == CAPNP_OK) params = call_params_init(c, err)
      if (err /= CAPNP_OK) then
         call capnp_message_free(m)
         call release_question(conn, qid)
         qid = -1_int64
      end if
   end subroutine rpc_call_begin

   subroutine rpc_call_send(conn, m, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(capnp_message_t), intent(inout) :: m
      integer, intent(out) :: err
      integer(int64) :: qid
      type(message_t) :: msg
      type(call_t) :: c
      type(message_target_t) :: tgt
      integer :: rerr
      qid = call_question_of(m)
      if (qid >= 0_int64 .and. qid < int(MAXQ, int64)) then
         msg = message_read_root(m, rerr)
         if (rerr == CAPNP_OK .and. message_which(msg) == MESSAGE_CALL_TAG) then
            c = message_call_get(msg, rerr)
            if (rerr == CAPNP_OK) then
               conn%questions(int(qid))%tail_call = &
                  call_send_results_to_which(c) == CALL_SEND_RESULTS_TO_YOURSELF_TAG
            end if
         end if
      end if
      ! A call on a capability this vat hosts never goes on the wire.
      if (qid >= 0_int64 .and. qid < int(MAXQ, int64)) then
         if (conn%questions(int(qid))%local_export >= 0) then
            call fulfill_local_call(conn, conn%questions(int(qid))%local_export, &
                                    m, qid, err)
            call capnp_message_free(m)
            return
         end if
      end if
      ! A pipeline on a local question is resolved here, or queued until
      ! that question's Return is stored.
      msg = message_read_root(m, rerr)
      if (rerr == CAPNP_OK .and. message_which(msg) == MESSAGE_CALL_TAG) then
         c = message_call_get(msg, rerr)
         if (rerr == CAPNP_OK) tgt = call_target_get(c, rerr)
         if (rerr == CAPNP_OK) then
            if (message_target_which(tgt) == MESSAGE_TARGET_PROMISED_ANSWER_TAG) then
               block
                  type(promised_answer_t) :: pa
                  integer(int64) :: tqid
                  integer :: tq
                  pa = message_target_promised_answer_get(tgt, rerr)
                  if (rerr == CAPNP_OK) then
                     tqid = promised_answer_question_id_get(pa)
                     if (tqid >= 0_int64 .and. tqid < int(MAXQ, int64)) then
                        tq = int(tqid)
                        if (conn%questions(tq)%local_call .or. &
                            conn%questions(tq)%local_results) then
                           if (conn%questions(tq)%returned) then
                              call dispatch_local_pipeline(conn, m, qid, tqid, err)
                           else
                              call enqueue_pipeline(conn, m, qid, tqid, err)
                           end if
                           call capnp_message_free(m)
                           return
                        end if
                     end if
                  end if
               end block
            end if
         end if
      end if
      call rpc_send_message(conn%transport, m, err)
      if (err /= CAPNP_OK) then
         call release_unsent_call_caps(conn, m)
         if (qid >= 0_int64 .and. qid < int(MAXQ, int64)) then
            call question_slot_clear(conn%questions(int(qid)))
         end if
      end if
      call capnp_message_free(m)
   end subroutine rpc_call_send

   !> Run a saved Call against a local export and store the Return on
   !> the question. Nothing is written to the peer. A local answer can
   !> start another local call before this one returns.
   recursive subroutine fulfill_local_call(conn, export_id, callmsg, qid, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer, intent(in) :: export_id
      type(capnp_message_t), intent(inout), target :: callmsg
      integer(int64), intent(in) :: qid
      integer, intent(out) :: err
      type(capnp_message_t), target :: rm
      type(message_t) :: cmsg, rmsg
      type(call_t) :: c
      type(return_t) :: r
      type(payload_t) :: params
      type(rpc_call_ctx_t) :: ctx
      integer :: derr
      logical :: dispatched
      integer(int8), allocatable :: bytes(:)
      err = CAPNP_OK
      if (qid < 0_int64 .or. qid >= int(MAXQ, int64)) then
         err = CAPNP_ERR_BOUNDS
         return
      end if
      if (conn%questions(int(qid))%canceled) then
         call fail_pipe_waits(conn, qid, err)
         return
      end if
      call capnp_message_init_builder(rm, err)
      if (err /= CAPNP_OK) return
      rmsg = message_new_root(rm, err)
      if (err /= CAPNP_OK) then
         call capnp_message_free(rm)
         return
      end if
      r = message_return_init(rmsg, err)
      if (err == CAPNP_OK) call return_answer_id_set(r, qid, err)
      cmsg = message_read_root(callmsg, err)
      if (err /= CAPNP_OK) then
         call capnp_message_free(rm)
         return
      end if
      c = message_call_get(cmsg, err)
      params = call_params_get(c, err)
      if (export_id < 0 .or. export_id >= MAXE .or. &
          .not. conn%exports(export_id)%used) then
         call fill_exception(r, 'no such capability', err)
      else
         ctx%interface_id = call_interface_id_get(c)
         ctx%method_id = int(call_method_id_get(c))
         ctx%params = payload_content_get(params, err)
         ctx%rmsg => rm
         ctx%results = return_results_init(r, err)
         dispatched = .false.
         derr = CAPNP_ERR_KIND
         select type (s => conn%exports(export_id)%srv)
         class is (rpc_server_t)
            call s%dispatch(ctx, derr)
            dispatched = .true.
         end select
         if (.not. dispatched .or. derr /= CAPNP_OK) then
            call discard_unexported_wrappers(ctx)
            call fill_exception(r, 'method raised error', err)
         else
            call flush_cap_table(conn, ctx, err)
            if (err /= CAPNP_OK) then
               call rollback_staged(conn, ctx)
               call fill_exception(r, 'export failed', err)
            end if
         end if
      end if
      call capnp_serialize_bytes(rm, bytes, err)
      if (err == CAPNP_OK) then
         call capnp_deserialize_bytes(bytes, conn%questions(int(qid))%retmsg, err)
         conn%questions(int(qid))%returned = .true.
         conn%questions(int(qid))%local_call = .true.
      end if
      call capnp_message_free(rm)
      if (err == CAPNP_OK .and. conn%questions(int(qid))%returned) &
         call drain_pipe_waits(conn, qid, err)
   end subroutine fulfill_local_call

   !> Run pipeline calls that were waiting on a local question.
   recursive subroutine drain_pipe_waits(conn, target_qid, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: target_qid
      integer, intent(out) :: err
      type(rpc_pipe_wait_t), pointer :: node, prev, nxt
      err = CAPNP_OK
      prev => null()
      node => conn%pipe_wait
      do while (associated(node))
         nxt => node%next
         if (node%target_qid == target_qid) then
            if (associated(prev)) then
               prev%next => nxt
            else
               conn%pipe_wait => nxt
            end if
            call dispatch_local_pipeline(conn, node%msg, node%call_qid, &
                                         target_qid, err)
            call capnp_message_free(node%msg)
            deallocate (node)
         else
            prev => node
         end if
         node => nxt
         if (err /= CAPNP_OK) exit
      end do
   end subroutine drain_pipe_waits

   !> A canceled local question will not produce a result. Waiting
   !> pipelines fail with that fact instead of sitting on the list.
   subroutine fail_pipe_waits(conn, target_qid, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: target_qid
      integer, intent(out) :: err
      type(rpc_pipe_wait_t), pointer :: node, prev, nxt
      err = CAPNP_OK
      prev => null()
      node => conn%pipe_wait
      do while (associated(node))
         nxt => node%next
         if (node%target_qid == target_qid) then
            if (associated(prev)) then
               prev%next => nxt
            else
               conn%pipe_wait => nxt
            end if
            call reject_local_question(conn, node%call_qid, 'call canceled', err)
            call capnp_message_free(node%msg)
            deallocate (node)
         else
            prev => node
         end if
         node => nxt
      end do
   end subroutine fail_pipe_waits

   subroutine reject_local_question(conn, qid, reason, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: qid
      character(len=*), intent(in) :: reason
      integer, intent(out) :: err
      type(capnp_message_t), target :: rm
      type(message_t) :: rmsg
      type(return_t) :: r
      integer(int8), allocatable :: bytes(:)
      err = CAPNP_OK
      if (qid < 0_int64 .or. qid >= int(MAXQ, int64)) return
      if (conn%questions(int(qid))%returned) return
      call capnp_message_init_builder(rm, err)
      if (err /= CAPNP_OK) return
      rmsg = message_new_root(rm, err)
      if (err /= CAPNP_OK) then
         call capnp_message_free(rm)
         return
      end if
      r = message_return_init(rmsg, err)
      if (err == CAPNP_OK) call return_answer_id_set(r, qid, err)
      if (err == CAPNP_OK) call fill_exception(r, reason, err)
      if (err == CAPNP_OK) call capnp_serialize_bytes(rm, bytes, err)
      if (err == CAPNP_OK) then
         call capnp_deserialize_bytes(bytes, conn%questions(int(qid))%retmsg, err)
         conn%questions(int(qid))%returned = .true.
         conn%questions(int(qid))%local_call = .true.
      end if
      call capnp_message_free(rm)
   end subroutine reject_local_question

   subroutine enqueue_pipeline(conn, m, call_qid, target_qid, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(capnp_message_t), intent(inout) :: m
      integer(int64), intent(in) :: call_qid, target_qid
      integer, intent(out) :: err
      type(rpc_pipe_wait_t), pointer :: node
      integer(int8), allocatable :: bytes(:)
      allocate (node)
      node%call_qid = call_qid
      node%target_qid = target_qid
      node%next => conn%pipe_wait
      call capnp_serialize_bytes(m, bytes, err)
      if (err == CAPNP_OK) call capnp_deserialize_bytes(bytes, node%msg, err)
      if (err /= CAPNP_OK) then
         deallocate (node)
         return
      end if
      conn%pipe_wait => node
      if (call_qid >= 0_int64 .and. call_qid < int(MAXQ, int64)) &
         conn%questions(int(call_qid))%local_call = .true.
      err = CAPNP_OK
   end subroutine enqueue_pipeline

   !> Turn a pipeline call into a local call on the export the target
   !> question returned. An exception on that question fails the pipeline.
   recursive subroutine dispatch_local_pipeline(conn, callmsg, call_qid, target_qid, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(capnp_message_t), intent(inout), target :: callmsg
      integer(int64), intent(in) :: call_qid, target_qid
      integer, intent(out) :: err
      type(message_t) :: cmsg, tmsg
      type(call_t) :: c
      type(message_target_t) :: tgt
      type(promised_answer_t) :: pa
      type(promised_answer_op_t) :: op
      type(return_t) :: tr
      type(payload_t) :: pl
      type(capnp_ptr_t) :: ops, p, ctab
      type(cap_descriptor_t) :: cd
      integer(int8), allocatable :: bytes(:)
      integer(int64) :: eid
      integer :: i, tq
      err = CAPNP_OK
      if (target_qid < 0_int64 .or. target_qid >= int(MAXQ, int64)) then
         call fulfill_local_call(conn, -1, callmsg, call_qid, err)
         return
      end if
      tq = int(target_qid)
      tmsg = message_read_root(conn%questions(tq)%retmsg, err)
      if (err /= CAPNP_OK) return
      tr = message_return_get(tmsg, err)
      if (err /= CAPNP_OK) return
      if (return_which(tr) /= RETURN_RESULTS_TAG) then
         if (call_qid < 0_int64 .or. call_qid >= int(MAXQ, int64)) return
         call capnp_serialize_bytes(conn%questions(tq)%retmsg, bytes, err)
         if (err /= CAPNP_OK) return
         call capnp_deserialize_bytes(bytes, conn%questions(int(call_qid))%retmsg, err)
         if (err /= CAPNP_OK) return
         conn%questions(int(call_qid))%returned = .true.
         conn%questions(int(call_qid))%local_call = .true.
         return
      end if
      pl = return_results_get(tr, err)
      p = payload_content_get(pl, err)
      if (err /= CAPNP_OK) return
      cmsg = message_read_root(callmsg, err)
      if (err /= CAPNP_OK) return
      c = message_call_get(cmsg, err)
      tgt = call_target_get(c, err)
      pa = message_target_promised_answer_get(tgt, err)
      if (err /= CAPNP_OK) return
      ops = promised_answer_transform_get(pa, err)
      if (err /= CAPNP_OK) return
      do i = 0, int(capnp_list_len(ops)) - 1
         op%p = capnp_list_get_struct(ops, i, err)
         if (err /= CAPNP_OK) return
         select case (promised_answer_op_which(op))
         case (PROMISED_ANSWER_OP_NOOP_TAG)
         case (PROMISED_ANSWER_OP_GET_POINTER_FIELD_TAG)
            p = capnp_getp(p, int(promised_answer_op_get_pointer_field_get(op)), err)
            if (err /= CAPNP_OK) return
         case default
            call abort_conn(conn, 'Unsupported pipeline op.', err)
            return
         end select
      end do
      eid = -1_int64
      if (p%kind == CAPNP_PK_CAP) then
         ctab = payload_cap_table_get(pl, err)
         if (err == CAPNP_OK) then
            if (p%capidx >= 0_int64 .and. p%capidx < capnp_list_len(ctab)) then
               cd%p = capnp_list_get_struct(ctab, int(p%capidx), err)
               if (err == CAPNP_OK) then
                  select case (cap_descriptor_which(cd))
                  case (CAP_DESCRIPTOR_SENDER_HOSTED_TAG)
                     eid = cap_descriptor_sender_hosted_get(cd)
                  case (CAP_DESCRIPTOR_SENDER_PROMISE_TAG)
                     eid = cap_descriptor_sender_promise_get(cd)
                  end select
               end if
            end if
         end if
      end if
      if (conn%dead) return
      call fulfill_local_call(conn, int(eid), callmsg, call_qid, err)
   end subroutine dispatch_local_pipeline

   subroutine free_pipe_waits(conn)
      type(rpc_conn_t), intent(inout) :: conn
      type(rpc_pipe_wait_t), pointer :: node, nxt
      node => conn%pipe_wait
      do while (associated(node))
         nxt => node%next
         call capnp_message_free(node%msg)
         deallocate (node)
         node => nxt
      end do
      conn%pipe_wait => null()
   end subroutine free_pipe_waits

   !> Question id carried by a Call, or -1 when the message is not one.
   function call_question_of(m) result(qid)
      type(capnp_message_t), intent(inout), target :: m
      integer(int64) :: qid
      type(message_t) :: msg
      type(call_t) :: c
      integer :: rerr
      qid = -1_int64
      msg = message_read_root(m, rerr)
      if (rerr /= CAPNP_OK) return
      if (message_which(msg) /= MESSAGE_CALL_TAG) return
      c = message_call_get(msg, rerr)
      if (rerr /= CAPNP_OK) return
      qid = call_question_id_get(c)
   end function call_question_of

   !> A Call that never reached the peer must not leave the export refs
   !> its cap table was holding. senderHosted, senderPromise and the
   !> vine of a thirdPartyHosted descriptor are one ref each.
   subroutine release_unsent_call_caps(conn, m)
      type(rpc_conn_t), intent(inout) :: conn
      type(capnp_message_t), intent(inout), target :: m
      type(message_t) :: msg
      type(call_t) :: c
      type(payload_t) :: params
      type(capnp_ptr_t) :: ctab
      type(cap_descriptor_t) :: cd
      type(third_party_cap_descriptor_t) :: tp
      integer(int64) :: n, i, id
      integer :: rerr
      msg = message_read_root(m, rerr)
      if (rerr /= CAPNP_OK) return
      if (message_which(msg) /= MESSAGE_CALL_TAG) return
      c = message_call_get(msg, rerr)
      if (rerr /= CAPNP_OK) return
      params = call_params_get(c, rerr)
      if (rerr /= CAPNP_OK) return
      ctab = payload_cap_table_get(params, rerr)
      if (rerr /= CAPNP_OK) return
      n = capnp_list_len(ctab)
      do i = 0_int64, n - 1_int64
         cd%p = capnp_list_get_struct(ctab, int(i), rerr)
         if (rerr /= CAPNP_OK) cycle
         id = -1_int64
         select case (cap_descriptor_which(cd))
         case (CAP_DESCRIPTOR_SENDER_HOSTED_TAG)
            id = cap_descriptor_sender_hosted_get(cd)
         case (CAP_DESCRIPTOR_SENDER_PROMISE_TAG)
            id = cap_descriptor_sender_promise_get(cd)
         case (CAP_DESCRIPTOR_THIRD_PARTY_HOSTED_TAG)
            tp = cap_descriptor_third_party_hosted_get(cd, rerr)
            if (rerr == CAPNP_OK) id = third_party_cap_descriptor_vine_id_get(tp)
         end select
         if (id >= 0_int64 .and. id < int(MAXE, int64)) &
            call drop_export(conn, int(id), 1)
      end do
   end subroutine release_unsent_call_caps

   !> Begin Persistent.save using the generic
   !> Persistent<AnyPointer, AnyPointer> wire layout. `owner` is copied to
   !> SaveParams.sealFor when present; a null owner requests an unsealed
   !> reference, whose acceptance is realm policy.
   subroutine rpc_persistent_save_begin(conn, target, m, owner, qid, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(rpc_cap_t), intent(in) :: target
      type(capnp_message_t), intent(inout), target :: m
      type(capnp_ptr_t), intent(in), optional :: owner
      integer(int64), intent(out) :: qid
      integer, intent(out) :: err
      type(payload_t) :: params
      type(capnp_ptr_t) :: save_params
      call rpc_call_begin(conn, target, RPC_PERSISTENT_IFACE, &
                          RPC_PERSISTENT_SAVE, m, params, qid, err)
      if (err /= CAPNP_OK) return
      save_params = capnp_new_struct(m, 0, 1, err)
      if (err == CAPNP_OK .and. present(owner)) call capnp_setp(save_params, 0, owner, err)
      if (err == CAPNP_OK) call payload_content_set(params, save_params, err)
      if (err /= CAPNP_OK) then
         call capnp_message_free(m)
         call release_question(conn, qid)
         qid = -1_int64
      end if
   end subroutine rpc_persistent_save_begin

   !> Wait for Persistent.save and return SaveResults.sturdyRef. The
   !> pointer remains owned by the question until rpc_finish_send.
   subroutine rpc_persistent_save_wait(conn, qid, sturdy_ref, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: qid
      type(capnp_ptr_t), intent(out) :: sturdy_ref
      integer, intent(out) :: err
      type(capnp_ptr_t) :: results
      call rpc_wait(conn, qid, err)
      if (err /= CAPNP_OK) return
      call rpc_result_content(conn, qid, results, err)
      if (err /= CAPNP_OK) return
      sturdy_ref = capnp_getp(results, 0, err)
   end subroutine rpc_persistent_save_wait

   !> A capability reached by pointer-field hops into a question's
   !> not-yet-returned results: promise pipelining.
   function rpc_pipeline_cap(conn, qid, field_indices) result(cap)
      type(rpc_conn_t), intent(in) :: conn
      integer(int64), intent(in) :: qid
      integer, intent(in) :: field_indices(:)
      type(rpc_cap_t) :: cap
      integer :: i, q
      cap%kind = RPC_CAP_PIPELINE
      cap%id = qid
      cap%nops = min(size(field_indices), MAXOPS)
      do i = 1, cap%nops
         cap%ops(i - 1) = field_indices(i)
      end do
      ! Same inheritance as rpc_result_cap: a revoke must stop pipelined
      ! calls, not only calls on the settled import.
      q = int(qid)
      if (q >= 0 .and. q < MAXQ) then
         if (conn%questions(q)%used) then
            cap%membrane => conn%questions(q)%membrane
            cap%membrane_reverse = conn%questions(q)%membrane_reverse
         end if
      end if
   end function rpc_pipeline_cap

   !> Send every part of a two-party Join. The receiver waits for all
   !> parts before answering, so callers should send the complete set
   !> before waiting on any returned question id.
   subroutine rpc_join_send(conn, caps, join_id, qids, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(rpc_cap_t), intent(in) :: caps(:)
      integer(int64), intent(in) :: join_id
      integer(int64), intent(out) :: qids(:)
      integer, intent(out) :: err
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(join_t) :: jn
      type(message_target_t) :: tgt
      type(promised_answer_t) :: pa
      type(promised_answer_op_t) :: op
      type(join_key_part_t) :: kp
      type(capnp_ptr_t) :: ops
      integer :: i, k
      err = CAPNP_OK
      if (size(caps) < 1 .or. size(caps) > MAXJOINPARTS .or. &
          size(qids) < size(caps)) then
         err = CAPNP_ERR_ARG
         return
      end if
      qids = -1_int64
      do i = 1, size(caps)
         qids(i) = alloc_qid(conn)
         if (qids(i) < 0_int64) then
            err = CAPNP_ERR_ALLOC
            call finish_sent_joins(conn, qids, i - 1)
            return
         end if
         call capnp_message_init_builder(m, err)
         if (err == CAPNP_OK) msg = message_new_root(m, err)
         if (err == CAPNP_OK) jn = message_join_init(msg, err)
         if (err == CAPNP_OK) call join_question_id_set(jn, qids(i), err)
         if (err == CAPNP_OK) tgt = join_target_init(jn, err)
         if (err == CAPNP_OK) then
            select case (caps(i)%kind)
            case (RPC_CAP_IMPORT)
               call message_target_imported_cap_set(tgt, caps(i)%id, err)
            case (RPC_CAP_PIPELINE)
               pa = message_target_promised_answer_init(tgt, err)
               if (err == CAPNP_OK) call promised_answer_question_id_set(pa, caps(i)%id, err)
               if (err == CAPNP_OK) ops = promised_answer_transform_init(pa, int(caps(i)%nops, int64), err)
               if (err == CAPNP_OK) then
                  do k = 0, caps(i)%nops - 1
                     op%p = capnp_list_get_struct(ops, k, err)
                     if (err /= CAPNP_OK) exit
                     call promised_answer_op_get_pointer_field_set(op, caps(i)%ops(k), err)
                     if (err /= CAPNP_OK) exit
                  end do
               end if
            case default
               err = CAPNP_ERR_ARG
            end select
         end if
         if (err == CAPNP_OK) kp = join_key_part_new(m, err)
         if (err == CAPNP_OK) call join_key_part_join_id_set(kp, join_id, err)
         if (err == CAPNP_OK) call join_key_part_part_count_set(kp, size(caps), err)
         if (err == CAPNP_OK) call join_key_part_part_num_set(kp, i - 1, err)
         if (err == CAPNP_OK) call join_key_part_set(jn, kp%p, err)
         if (err == CAPNP_OK) call rpc_send_message(conn%transport, m, err)
         call capnp_message_free(m)
         if (err /= CAPNP_OK) then
            call release_question(conn, qids(i))
            qids(i) = -1_int64
            call finish_sent_joins(conn, qids, i - 1)
            return
         end if
      end do
   end subroutine rpc_join_send

   !> Finish Join parts that already left the vat when a later part fails.
   subroutine finish_sent_joins(conn, qids, nsent)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: qids(:)
      integer, intent(in) :: nsent
      integer :: i, ferr
      do i = 1, nsent
         if (qids(i) >= 0_int64) call rpc_finish_send(conn, qids(i), .false., ferr)
      end do
   end subroutine finish_sent_joins

   !> Give back every question a failed Join wait still holds.
   subroutine release_join_questions(conn, qids)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: qids(:)
      integer :: i, q, ferr
      logical :: gone
      gone = conn%dead .or. .not. associated(conn%transport)
      do i = 1, size(qids)
         q = int(qids(i))
         if (q < 0 .or. q >= MAXQ) cycle
         if (.not. conn%questions(q)%used) cycle
         if (gone) then
            if (conn%questions(q)%returned) &
               call capnp_message_free(conn%questions(q)%retmsg)
            call question_slot_clear(conn%questions(q))
         else if (conn%questions(q)%returned) then
            call rpc_finish_send(conn, qids(i), .false., ferr)
         else
            call rpc_request_cancellation(conn, qids(i), ferr)
         end if
      end do
   end subroutine release_join_questions

   subroutine clear_joins(conn)
      type(rpc_conn_t), intent(inout) :: conn
      integer :: i
      do i = 0, MAXJOINS - 1
         conn%joins(i) = rpc_join_slot_t()
      end do
   end subroutine clear_joins

   !> Collect a complete Join result set. `joined` is the single
   !> capability carried by a successful set; a failed set returns
   !> `succeeded=.false.` and RPC_CAP_NONE.
   subroutine rpc_join_wait(conn, qids, joined, succeeded, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: qids(:)
      type(rpc_cap_t), intent(out) :: joined
      logical, intent(out) :: succeeded
      integer, intent(out) :: err
      type(capnp_ptr_t) :: content
      type(capability_client_t) :: jcap
      type(join_result_t) :: jr
      type(rpc_cap_t) :: candidate
      integer :: i, ferr, ncap
      joined = rpc_cap_t()
      succeeded = size(qids) > 0
      ncap = 0
      err = CAPNP_OK
      do i = 1, size(qids)
         call rpc_wait(conn, qids(i), err)
         if (err /= CAPNP_OK) exit
         call rpc_result_content(conn, qids(i), content, err)
         if (err /= CAPNP_OK) exit
         jr%p = content
         if (.not. join_result_succeeded_get(jr)) succeeded = .false.
         jcap = join_result_cap_get(jr, err)
         if (err /= CAPNP_OK) exit
         if (jcap%kind /= 0) then
            call rpc_result_cap(conn, qids(i), [0], candidate, err)
            if (err /= CAPNP_OK) exit
            joined = candidate
            ncap = ncap + 1
         end if
      end do
      if (err /= CAPNP_OK) then
         call release_join_questions(conn, qids)
         return
      end if
      if (succeeded .and. ncap /= 1) then
         err = CAPNP_ERR_KIND
         call release_join_questions(conn, qids)
         return
      end if
      if (.not. succeeded) joined = rpc_cap_t()
      do i = 1, size(qids)
         call rpc_finish_send(conn, qids(i), succeeded .and. ncap == 1, ferr)
         if (ferr /= CAPNP_OK .and. err == CAPNP_OK) err = ferr
      end do
   end subroutine rpc_join_wait

   !> Pump the connection until question qid has returned.
   recursive subroutine rpc_wait(conn, qid, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: qid
      integer, intent(out) :: err
      integer :: q
      err = CAPNP_OK
      if (qid < 0_int64 .or. qid >= int(MAXQ, int64)) then
         err = CAPNP_ERR_BOUNDS
         return
      end if
      q = int(qid)
      if (.not. conn%questions(q)%used) then
         err = CAPNP_ERR_ARG
         return
      end if
      ! Cancellation already told the caller there will be no result.
      ! Waiting here would block until a Return the caller discarded.
      if (conn%questions(q)%canceled) then
         conn%abort_reason = 'call canceled'
         err = RPC_ERR_EXCEPTION
         return
      end if
      do
         if (conn%dead) then
            err = RPC_ERR_DEAD
            return
         end if
         if (conn%questions(q)%returned) exit
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
      case (RETURN_CANCELED_TAG)
         conn%abort_reason = 'call canceled'
         err = RPC_ERR_EXCEPTION
      case (RETURN_RESULTS_SENT_ELSEWHERE_TAG)
         ! Only meaningful for sendResultsTo.yourself, which this vat
         ! never requests.
         conn%abort_reason = 'results sent elsewhere (unrequested tail call)'
         err = RPC_ERR_EXCEPTION
      case (RETURN_TAKE_FROM_OTHER_QUESTION_TAG)
         block
            integer(int64) :: other
            type(message_t) :: held
            type(return_t) :: hr
            type(rpc_answer_slot_t), pointer :: slot
            logical :: found
            other = return_take_from_other_question_get(r)
            call answer_find(conn, other, slot, found)
            ! .and. and .or. always evaluate both sides, so a missing
            ! slot must be rejected before it is dereferenced.
            if (err /= CAPNP_OK .or. .not. found) then
               call abort_conn(conn, &
                               '`Return.takeFromOtherQuestion` had invalid answer ID.', &
                               err)
               return
            end if
            if (.not. associated(slot) .or. .not. slot%used) then
               call abort_conn(conn, &
                               '`Return.takeFromOtherQuestion` had invalid answer ID.', &
                               err)
               return
            end if
            if (.not. slot%redirected .or. .not. slot%has_results) then
               call abort_conn(conn, &
                               '`Return.takeFromOtherQuestion` referenced a call that ' // &
                               'did not use `sendResultsTo.yourself`.', err)
               return
            end if
            held = message_read_root(slot%retmsg, err)
            if (err /= CAPNP_OK) return
            hr = message_return_get(held, err)
            if (err /= CAPNP_OK) return
            if (return_which(hr) /= RETURN_RESULTS_TAG) then
               call abort_conn(conn, &
                               '`Return.takeFromOtherQuestion` referenced a call that ' // &
                               'did not use `sendResultsTo.yourself`.', err)
               return
            end if
            pl = return_results_get(hr, err)
            content = payload_content_get(pl, err)
         end block
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
      select case (cap_descriptor_which(cd))
      case (CAP_DESCRIPTOR_SENDER_HOSTED_TAG)
         cap%id = cap_descriptor_sender_hosted_get(cd)
         if (conn%questions(int(qid))%local_call .or. &
             conn%questions(int(qid))%local_results) then
            cap%kind = RPC_CAP_LOCAL
         else
            cap%kind = RPC_CAP_IMPORT
         end if
      case (CAP_DESCRIPTOR_SENDER_PROMISE_TAG)
         ! A promise import: the spec permits continuing to address the
         ! promise id after (or without) a Resolve; the exporter keeps
         ! forwarding until we Release it.
         cap%id = cap_descriptor_sender_promise_get(cd)
         if (conn%questions(int(qid))%local_call .or. &
             conn%questions(int(qid))%local_results) then
            cap%kind = RPC_CAP_LOCAL
         else
            cap%kind = RPC_CAP_IMPORT
         end if
      case default
         err = CAPNP_ERR_KIND
      end select
      if (err == CAPNP_OK) then
         cap%membrane => conn%questions(int(qid))%membrane
         cap%membrane_reverse = conn%questions(int(qid))%membrane_reverse
      end if
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
      integer :: q
      err = CAPNP_OK
      if (qid < 0_int64 .or. qid >= int(MAXQ, int64)) then
         err = CAPNP_ERR_BOUNDS
         return
      end if
      q = int(qid)
      if (.not. conn%questions(q)%used) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (.not. conn%questions(q)%local_call) then
         call capnp_message_init_builder(m, err)
         if (err /= CAPNP_OK) return
         msg = message_new_root(m, err)
         f = message_finish_init(msg, err)
         call finish_question_id_set(f, qid, err)
         call finish_release_result_caps_set(f, .not. retain_caps, err)
         call finish_require_early_cancellation_workaround_set(f, .false., err)
         if (err == CAPNP_OK) call rpc_send_message(conn%transport, m, err)
         call capnp_message_free(m)
      end if
      if (.not. retain_caps) call drop_finished_caps(conn, q)
      if (conn%questions(q)%used) then
         call capnp_message_free(conn%questions(q)%retmsg)
         call question_slot_clear(conn%questions(q))
      end if
   end subroutine rpc_finish_send

   !> Finish(releaseResultCaps) is the release. A later rpc_release_send
   !> must not send a second Release for the same result caps.
   subroutine drop_finished_caps(conn, q)
      type(rpc_conn_t), intent(inout), target :: conn
      integer, intent(in) :: q
      type(message_t) :: msg
      type(return_t) :: r
      type(payload_t) :: pl
      type(capnp_ptr_t) :: ctab
      type(cap_descriptor_t) :: cd
      integer :: i, n, ierr
      integer(int64) :: id
      logical :: local
      if (.not. conn%questions(q)%returned) return
      msg = message_read_root(conn%questions(q)%retmsg, ierr)
      if (ierr /= CAPNP_OK) return
      if (message_which(msg) /= MESSAGE_RETURN_TAG) return
      r = message_return_get(msg, ierr)
      if (ierr /= CAPNP_OK) return
      if (return_which(r) /= RETURN_RESULTS_TAG) return
      pl = return_results_get(r, ierr)
      if (ierr /= CAPNP_OK) return
      ctab = payload_cap_table_get(pl, ierr)
      if (ierr /= CAPNP_OK) return
      local = conn%questions(q)%local_call .or. conn%questions(q)%local_results
      n = int(capnp_list_len(ctab))
      do i = 0, n - 1
         cd%p = capnp_list_get_struct(ctab, i, ierr)
         if (ierr /= CAPNP_OK) cycle
         id = -1_int64
         select case (cap_descriptor_which(cd))
         case (CAP_DESCRIPTOR_SENDER_HOSTED_TAG)
            id = cap_descriptor_sender_hosted_get(cd)
         case (CAP_DESCRIPTOR_SENDER_PROMISE_TAG)
            id = cap_descriptor_sender_promise_get(cd)
         end select
         if (id < 0_int64) cycle
         if (local) then
            if (id >= 0_int64 .and. id < int(MAXE, int64)) &
               call drop_export(conn, int(id), 1)
         end if
      end do
   end subroutine drop_finished_caps

   !> Request cancellation by sending Finish before a Return. Modern
   !> peers receive requireEarlyCancellationWorkaround=false, matching
   !> Cap'n Proto 1.0 and newer. Whether delivered application code is
   !> interruptible remains the callee's static method policy.
   subroutine rpc_request_cancellation(conn, qid, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: qid
      integer, intent(out) :: err
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(finish_t) :: f
      integer :: q
      err = CAPNP_OK
      if (qid < 0_int64 .or. qid >= int(MAXQ, int64)) then
         err = CAPNP_ERR_BOUNDS
         return
      end if
      q = int(qid)
      if (.not. conn%questions(q)%used) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (.not. conn%questions(q)%local_call) then
         call capnp_message_init_builder(m, err)
         if (err /= CAPNP_OK) return
         msg = message_new_root(m, err)
         f = message_finish_init(msg, err)
         call finish_question_id_set(f, qid, err)
         call finish_release_result_caps_set(f, .true., err)
         call finish_require_early_cancellation_workaround_set(f, .false., err)
         if (err == CAPNP_OK) call rpc_send_message(conn%transport, m, err)
         call capnp_message_free(m)
         if (err /= CAPNP_OK) return
      end if
      if (.not. conn%questions(q)%used) return
      if (conn%questions(q)%returned) then
         call capnp_message_free(conn%questions(q)%retmsg)
         call question_slot_clear(conn%questions(q))
      else
         ! Reserve the id until the matching Return arrives. Reusing it
         ! earlier delivers that Return to whatever call takes the id.
         conn%questions(q)%canceled = .true.
      end if
   end subroutine rpc_request_cancellation

   !> Clear a question slot.
   !>
   !> An empty structure constructor says this more briefly, but a
   !> component of derived type is where compilers disagree about whether
   !> default initialisation carries through: flang rejects
   !> `rpc_question_slot_t()` for lacking a value for `retmsg` where
   !> gfortran accepts it. Assigning the fields is unambiguous, and
   !> callers free `retmsg` first, which already restores it.
   subroutine question_slot_clear(slot)
      type(rpc_question_slot_t), intent(inout) :: slot
      slot%used = .false.
      slot%returned = .false.
      slot%canceled = .false.
      slot%tail_call = .false.
      slot%local_call = .false.
      slot%local_results = .false.
      slot%local_export = -1
      slot%membrane => null()
      slot%membrane_reverse = .false.
   end subroutine question_slot_clear

   !> Clear an answer slot; see question_slot_clear.
   subroutine answer_slot_clear(slot)
      type(rpc_answer_slot_t), intent(inout) :: slot
      slot%used = .false.
      slot%has_results = .false.
      slot%redirected = .false.
      slot%nexp = 0
      slot%exports = -1
   end subroutine answer_slot_clear

   !> Drop every overflow answer. Export refcounts are the export
   !> table's job; this only frees the saved Return messages.
   subroutine free_hi_answers(conn)
      type(rpc_conn_t), intent(inout) :: conn
      type(rpc_hi_node_t), pointer :: node, nxt
      integer :: b
      do b = 0, HIBUCKETS - 1
         node => conn%hi_buckets(b)%head
         do while (associated(node))
            nxt => node%next
            if (node%slot%used) call capnp_message_free(node%slot%retmsg)
            deallocate (node)
            node => nxt
         end do
         conn%hi_buckets(b)%head => null()
      end do
   end subroutine free_hi_answers

   !> Validate CapDescriptor rows of a payload cap table.
   subroutine note_payload_imports(conn, pl, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(payload_t), intent(in) :: pl
      integer, intent(out) :: err
      type(capnp_ptr_t) :: ctab
      type(cap_descriptor_t) :: cd
      integer :: i, n, ierr
      err = CAPNP_OK
      if (pl%p%kind /= CAPNP_PK_STRUCT) return
      ctab = payload_cap_table_get(pl, ierr)
      if (ierr /= CAPNP_OK) return
      n = int(capnp_list_len(ctab))
      do i = 0, n - 1
         cd%p = capnp_list_get_struct(ctab, i, ierr)
         if (ierr /= CAPNP_OK) cycle
         select case (cap_descriptor_which(cd))
         case (CAP_DESCRIPTOR_NONE_TAG)
         case (CAP_DESCRIPTOR_SENDER_HOSTED_TAG)
         case (CAP_DESCRIPTOR_SENDER_PROMISE_TAG)
         case (CAP_DESCRIPTOR_RECEIVER_HOSTED_TAG)
         case (CAP_DESCRIPTOR_RECEIVER_ANSWER_TAG)
         case (CAP_DESCRIPTOR_THIRD_PARTY_HOSTED_TAG)
         case default
            call abort_conn(conn, 'unknown CapDescriptor type', err)
            return
         end select
      end do
   end subroutine note_payload_imports

   pure function hi_bucket(qid) result(b)
      integer(int64), intent(in) :: qid
      integer :: b
      integer(int64) :: x
      x = ieor(qid, shiftr(qid, 33))
      b = int(iand(x, int(HIBUCKETS - 1, int64)))
   end function hi_bucket

   !> Locate the answer slot for qid. Low ids always resolve to the
   !> fixed table. Higher ids resolve only when a node was claimed.
   subroutine answer_find(conn, qid, slot, found)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: qid
      type(rpc_answer_slot_t), pointer, intent(out) :: slot
      logical, intent(out) :: found
      type(rpc_hi_node_t), pointer :: node
      found = .false.
      slot => null()
      if (qid < 0_int64) return
      if (qid < int(MAXQ, int64)) then
         slot => conn%answers(int(qid))
         found = .true.
         return
      end if
      node => conn%hi_buckets(hi_bucket(qid))%head
      do while (associated(node))
         if (node%qid == qid) then
            slot => node%slot
            found = .true.
            return
         end if
         node => node%next
      end do
   end subroutine answer_find

   !> Add an overflow node for a question id the fixed table cannot hold.
   subroutine answer_add_hi(conn, qid, slot)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: qid
      type(rpc_answer_slot_t), pointer, intent(out) :: slot
      type(rpc_hi_node_t), pointer :: node
      integer :: b
      allocate (node)
      node%qid = qid
      b = hi_bucket(qid)
      node%next => conn%hi_buckets(b)%head
      conn%hi_buckets(b)%head => node
      slot => node%slot
   end subroutine answer_add_hi

   !> Unlink one overflow answer after its Finish. The caller has
   !> already freed the saved message.
   subroutine answer_drop_hi(conn, qid)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: qid
      type(rpc_hi_node_t), pointer :: node, prev
      integer :: b
      prev => null()
      b = hi_bucket(qid)
      node => conn%hi_buckets(b)%head
      do while (associated(node))
         if (node%qid == qid) then
            if (associated(prev)) then
               prev%next => node%next
            else
               conn%hi_buckets(b)%head => node%next
            end if
            deallocate (node)
            return
         end if
         prev => node
         node => node%next
      end do
   end subroutine answer_drop_hi


   !> Drop refcount on an imported capability.
   subroutine rpc_release_send(conn, cap, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(rpc_cap_t), intent(in) :: cap
      integer, intent(out) :: err
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(release_t) :: rel
      err = CAPNP_OK
      if (cap%kind == RPC_CAP_LOCAL) then
         if (cap%id >= 0_int64 .and. cap%id < int(MAXE, int64)) &
            call drop_export(conn, int(cap%id), 1)
         return
      end if
      if (cap%kind /= RPC_CAP_IMPORT) return
      call capnp_message_init_builder(m, err)
      if (err /= CAPNP_OK) return
      msg = message_new_root(m, err)
      rel = message_release_init(msg, err)
      call release_id_set(rel, cap%id, err)
      call release_reference_count_set(rel, 1_int64, err)
      if (err == CAPNP_OK) call rpc_send_message(conn%transport, m, err)
      call capnp_message_free(m)
   end subroutine rpc_release_send

   ! ------------------------------------------------------------------
   ! Streaming (client side)
   ! ------------------------------------------------------------------

   subroutine rpc_stream_init(stream, window)
      type(rpc_stream_t), intent(out) :: stream
      integer, intent(in), optional :: window
      if (present(window)) stream%window = max(1, window)
   end subroutine rpc_stream_init

   !> Bytes on the wire for one message: framing header plus segment
   !> bodies. This is what the stream window counts.
   function framed_message_bytes(m) result(n)
      type(capnp_message_t), intent(in) :: m
      integer :: n
      integer(int64) :: total
      integer :: i
      total = ((1_int64 + int(m%nsegs, int64))*4_int64 + 7_int64)/8_int64*8_int64
      if (allocated(m%segs)) then
         do i = 1, m%nsegs
            total = total + m%segs(i)%len
         end do
      end if
      if (total > int(huge(n), int64)) then
         n = huge(n)
      else
         n = int(total)
      end if
   end function framed_message_bytes

   !> Send a prepared stream call (from a generated _begin) without
   !> waiting for its return. Blocks only when the window is full, and
   !> then just long enough to retire the oldest outstanding call.
   subroutine rpc_stream_send(conn, stream, m, qid, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(rpc_stream_t), intent(inout) :: stream
      type(capnp_message_t), intent(inout) :: m
      integer(int64), intent(in) :: qid
      integer, intent(out) :: err
      integer :: bytes
      err = CAPNP_OK
      if (stream%failed) then
         call capnp_message_free(m)
         call release_question(conn, qid)
         err = RPC_ERR_EXCEPTION
         return
      end if
      do while (stream%nout > 0 .and. &
                (stream%nout >= 64 .or. stream%used_bytes >= stream%window))
         call retire_oldest(conn, stream, err)
         if (err /= CAPNP_OK) then
            call capnp_message_free(m)
            call release_question(conn, qid)
            return
         end if
      end do
      bytes = framed_message_bytes(m)
      call rpc_call_send(conn, m, err)
      if (err /= CAPNP_OK) then
         call release_question(conn, qid)
         return
      end if
      stream%qids(stream%nout) = qid
      stream%sizes(stream%nout) = bytes
      stream%used_bytes = stream%used_bytes + bytes
      stream%nout = stream%nout + 1
   end subroutine rpc_stream_send

   !> Wait for every outstanding stream call. Call-level failures mark
   !> the stream failed but the window still drains fully; the first
   !> failure is what finish reports. Only a dead connection aborts the
   !> drain.
   subroutine rpc_stream_finish(conn, stream, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(rpc_stream_t), intent(inout) :: stream
      integer, intent(out) :: err
      integer :: first_err
      err = CAPNP_OK
      first_err = CAPNP_OK
      do while (stream%nout > 0)
         call retire_oldest(conn, stream, err)
         if (err == RPC_ERR_DEAD) return
         if (err /= CAPNP_OK .and. first_err == CAPNP_OK) first_err = err
      end do
      err = first_err
      if (stream%failed .and. err == CAPNP_OK) err = RPC_ERR_EXCEPTION
   end subroutine rpc_stream_finish

   !> Request cancellation of every outstanding stream call and discard
   !> the local window. This is useful for abandoning a producer; a
   !> remote method that does not opt into cancellation may still run to
   !> completion, as in capnp-C++.
   subroutine rpc_stream_cancel(conn, stream, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(rpc_stream_t), intent(inout) :: stream
      integer, intent(out) :: err
      integer :: i, cerr
      err = CAPNP_OK
      do i = 0, stream%nout - 1
         call rpc_request_cancellation(conn, stream%qids(i), cerr)
         if (cerr /= CAPNP_OK .and. err == CAPNP_OK) err = cerr
      end do
      stream%nout = 0
      stream%qids = -1_int64
      stream%sizes = 0
      stream%used_bytes = 0
      stream%failed = .true.
   end subroutine rpc_stream_cancel

   subroutine retire_oldest(conn, stream, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(rpc_stream_t), intent(inout) :: stream
      integer, intent(out) :: err
      type(capnp_ptr_t) :: content
      integer(int64) :: qid
      integer :: i, werr, bytes
      err = CAPNP_OK
      if (stream%nout == 0) return
      qid = stream%qids(0)
      bytes = stream%sizes(0)
      do i = 0, stream%nout - 2
         stream%qids(i) = stream%qids(i + 1)
         stream%sizes(i) = stream%sizes(i + 1)
      end do
      stream%nout = stream%nout - 1
      stream%used_bytes = stream%used_bytes - bytes
      call rpc_wait(conn, qid, err)
      if (err /= CAPNP_OK) return
      call rpc_result_content(conn, qid, content, werr)
      if (werr == RPC_ERR_EXCEPTION) then
         stream%failed = .true.
         err = RPC_ERR_EXCEPTION
      else if (werr /= CAPNP_OK) then
         err = werr
      end if
      call rpc_finish_send(conn, qid, .false., werr)
   end subroutine retire_oldest

   !> Return an unused question slot allocated by a _begin whose message
   !> was never sent.
   subroutine release_question(conn, qid)
      type(rpc_conn_t), intent(inout) :: conn
      integer(int64), intent(in) :: qid
      integer :: q
      q = int(qid)
      if (q >= 0 .and. q < MAXQ) then
         if (conn%questions(q)%used .and. .not. conn%questions(q)%returned) &
            call question_slot_clear(conn%questions(q))
      end if
   end subroutine release_question

   ! ------------------------------------------------------------------
   ! Server-side helpers
   ! ------------------------------------------------------------------

   !> Stage a server for the call's results capTable; returns the cap
   !> index for the content pointer. Export ids are allocated when the
   !> table flushes at send time.
   function rpc_ctx_export_cap(ctx, srv, err, owned) result(idx)
      type(rpc_call_ctx_t), intent(inout) :: ctx
      class(rpc_server_t), pointer, intent(in) :: srv
      integer, intent(out) :: err
      logical, intent(in), optional :: owned
      integer :: idx
      err = CAPNP_OK
      idx = -1
      if (ctx%nexp >= MAXCAPS) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      ctx%staged(ctx%nexp)%s => srv
      ctx%staged(ctx%nexp)%owned = .false.
      if (present(owned)) ctx%staged(ctx%nexp)%owned = owned
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

   function export_server(conn, srv, owned) result(eid)
      type(rpc_conn_t), intent(inout) :: conn
      class(*), pointer, intent(in) :: srv
      logical, intent(in), optional :: owned
      integer :: eid, i
      logical :: own
      own = .false.
      if (present(owned)) own = owned
      ! Reuse an existing export of the same server.
      do i = 0, MAXE - 1
         if (conn%exports(i)%used) then
            if (associated(conn%exports(i)%srv, srv)) then
               conn%exports(i)%refcount = conn%exports(i)%refcount + 1
               if (own) conn%exports(i)%owned = .true.
               eid = i
               return
            end if
         end if
      end do
      do i = 0, MAXE - 1
         if (.not. conn%exports(i)%used) then
            conn%exports(i)%used = .true.
            conn%exports(i)%refcount = 1
            call retain_server(srv)
            conn%exports(i)%srv => srv
            conn%exports(i)%owned = own
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
   !> A server's dispatch may call back into the pump, so this is recursive.
   recursive subroutine rpc_pump_once(conn, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer, intent(out) :: err
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      err = CAPNP_OK
      if (conn%dead) then
         err = RPC_ERR_DEAD
         return
      end if
      call rpc_recv_message(conn%transport, m, err)
      if (err /= CAPNP_OK) then
         ! No Return is coming for an abandoned Join, so the slot must
         ! not wait for a Finish that the dead peer will never send.
         call capnp_message_free(m)
         call clear_joins(conn)
         conn%dead = .true.
         err = RPC_ERR_DEAD
         return
      end if
      msg = message_read_root(m, err)
      if (err /= CAPNP_OK) then
         call capnp_message_free(m)
         return
      end if
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
         call handle_unimplemented(conn, msg, err)
      case (MESSAGE_PROVIDE_TAG)
         call handle_provide(conn, msg, err)
      case (MESSAGE_ACCEPT_TAG)
         call handle_accept(conn, msg, err)
      case (MESSAGE_JOIN_TAG)
         call handle_join(conn, msg, err)
      case (MESSAGE_RESOLVE_TAG)
         ! Promise resolution. Replying unimplemented is the
         ! spec-defined signal that this vat does not adopt
         ! resolutions: the sender must keep forwarding calls
         ! addressed to the promise, which it does until Release.
         call send_unimplemented(conn, msg, err)
      case default
         ! The obsolete save/delete messages. Reply unimplemented,
         ! echoing the message, per the spec.
         call send_unimplemented(conn, msg, err)
      end select
      call capnp_message_free(m)
   end subroutine rpc_pump_once

   !> Poll-then-pump: handle at most one message if one is readable
   !> within timeout_ms. handled=.false. on timeout. The multiplexing
   !> building block: a single thread can round-robin several
   !> connections without blocking on any one of them.
   recursive subroutine rpc_pump_poll(conn, timeout_ms, handled, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer, intent(in) :: timeout_ms
      logical, intent(out) :: handled
      integer, intent(out) :: err
      logical :: readable
      err = CAPNP_OK
      handled = .false.
      if (conn%dead) then
         err = RPC_ERR_DEAD
         return
      end if
      call conn%transport%poll_in(timeout_ms, readable, err)
      if (err /= CAPNP_OK .or. .not. readable) return
      call rpc_pump_once(conn, err)
      handled = err == CAPNP_OK
   end subroutine rpc_pump_poll

   !> Blocking server loop: pump until the peer disconnects. A clean
   !> peer shutdown returns CAPNP_OK; aborts surface their reason via
   !> rpc_conn_reason.
   recursive subroutine rpc_serve(conn, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer, intent(out) :: err
      err = CAPNP_OK
      do
         call rpc_pump_once(conn, err)
         if (err /= CAPNP_OK) exit
      end do
      if (err == RPC_ERR_DEAD .and. .not. allocated(conn%abort_reason)) err = CAPNP_OK
   end subroutine rpc_serve

   !> Tell the peer this connection is done and stop using it. The reason
   !> is the text a later rpc_conn_reason returns.
   subroutine abort_conn(conn, reason, err)
      type(rpc_conn_t), intent(inout), target :: conn
      character(len=*), intent(in) :: reason
      integer, intent(out) :: err
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(exception_t) :: ex
      integer :: e
      conn%abort_reason = reason
      conn%dead = .true.
      err = CAPNP_ERR_BOUNDS
      if (.not. associated(conn%transport)) return
      call capnp_message_init_builder(m, e)
      if (e /= CAPNP_OK) return
      msg = message_new_root(m, e)
      if (e /= CAPNP_OK) return
      ex = message_abort_init(msg, e)
      if (e /= CAPNP_OK) return
      call exception_type_set(ex, EXCEPTION_TYPE_FAILED_E, e)
      if (e /= CAPNP_OK) return
      call exception_reason_set(ex, reason, e)
      if (e /= CAPNP_OK) return
      call rpc_send_message(conn%transport, m, e)
      call capnp_message_free(m)
   end subroutine abort_conn

   !> True when qid can still be answered. A duplicate, a negative id,
   !> or an id already sitting in a Join set aborts the connection
   !> instead of publishing another export. Ids past the fixed table
   !> are stored on an overflow list until Finish.
   function claim_question(conn, qid, err) result(ok)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: qid
      integer, intent(out) :: err
      logical :: ok
      integer :: i, k
      type(rpc_answer_slot_t), pointer :: slot
      logical :: found
      ok = .false.
      if (qid < 0_int64) then
         call abort_conn(conn, &
                         'questionId is outside the supported range', err)
         return
      end if
      call answer_find(conn, qid, slot, found)
      ! A missing overflow slot is not in use. .and. would still read it.
      if (found) then
         if (slot%used) then
            call abort_conn(conn, 'questionId is already in use', err)
            return
         end if
      end if
      do i = 0, MAXJOINS - 1
         if (.not. conn%joins(i)%used) cycle
         do k = 0, conn%joins(i)%part_count - 1
            if (conn%joins(i)%seen(k) .and. conn%joins(i)%qids(k) == qid) then
               call abort_conn(conn, 'questionId is already in use', err)
               return
            end if
         end do
      end do
      if (.not. found) call answer_add_hi(conn, qid, slot)
      slot%used = .true.
      slot%has_results = .false.
      slot%redirected = .false.
      slot%nexp = 0
      slot%exports = -1
      ok = .true.
      err = CAPNP_OK
   end function claim_question

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
      if (.not. claim_question(conn, qid, err)) return
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
         if (err /= CAPNP_OK) then
            ! Exports added by the failed flush are already dropped.
            call release_unflushed(conn, ctx)
            call rollback_staged(conn, ctx)
            call fill_exception(r, 'export failed', err)
         end if
      end if
      call finish_answer(conn, qid, rm, ctx, err)
   end subroutine handle_bootstrap

   recursive subroutine handle_call(conn, msg, err)
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
      if (.not. claim_question(conn, qid, err)) return
      if (call_send_results_to_which(c) == CALL_SEND_RESULTS_TO_THIRD_PARTY_TAG .or. &
          (call_send_results_to_which(c) /= CALL_SEND_RESULTS_TO_CALLER_TAG .and. &
           call_send_results_to_which(c) /= CALL_SEND_RESULTS_TO_YOURSELF_TAG)) then
         ! Level 3 third-party tail calls are in the schema. Cap'n Proto
         ! C++ 1.4.0 does not implement them and aborts the connection.
         call abort_conn(conn, "Unsupported `Call.sendResultsTo`.", err)
         return
      end if
      call capnp_message_init_builder(rm, err)
      if (err /= CAPNP_OK) return
      rmsg = message_new_root(rm, err)
      r = message_return_init(rmsg, err)
      call return_answer_id_set(r, qid, err)
      ! The cap table describes the message, not the dispatch: a call
      ! this vat cannot route still told us where a third party's
      ! capability lives.
      params = call_params_get(c, err)
      if (err == CAPNP_OK) call note_introductions(conn, params, err)
      if (err == CAPNP_OK) call note_payload_imports(conn, params, err)
      if (conn%dead .or. err /= CAPNP_OK) then
         call capnp_message_free(rm)
         return
      end if

      call resolve_target(conn, c, eid, err)
      if (conn%dead) then
         call capnp_message_free(rm)
         return
      end if
      block
         type(message_target_t) :: tgt
         integer :: terr
         tgt = call_target_get(c, terr)
         if (terr == CAPNP_OK) then
            if (message_target_which(tgt) == MESSAGE_TARGET_IMPORTED_CAP_TAG .and. &
                eid < 0) then
               call capnp_message_free(rm)
               call abort_conn(conn, &
                               'Message target is not a current export ID.', err)
               return
            end if
         end if
      end block
      if (err /= CAPNP_OK .or. eid < 0) then
         call release_unflushed(conn, ctx)
         call fill_exception(r, 'no such capability', err)
         call finish_answer(conn, qid, rm, ctx, err)
         return
      end if
      ctx%interface_id = call_interface_id_get(c)
      ctx%method_id = int(call_method_id_get(c))
      ctx%params = payload_content_get(params, err)
      ctx%rmsg => rm
      ctx%results = return_results_init(r, err)
      ! The call keeps one reference so Release or close cannot destroy
      ! the server between entering dispatch and leaving it.
      conn%exports(eid)%refcount = conn%exports(eid)%refcount + 1
      conn%exports(eid)%dispatching = conn%exports(eid)%dispatching + 1
      dispatched = .false.
      derr = CAPNP_ERR_KIND
      select type (s => conn%exports(eid)%srv)
      class is (rpc_server_t)
         call s%dispatch(ctx, derr)
         dispatched = .true.
      end select
      if (.not. dispatched .or. derr /= CAPNP_OK) then
         ! An exception return does not publish the results cap table.
         ! Owned servers staged for that table are freed here, once.
         ! A membrane that never received an export id is freed too.
         call release_unflushed(conn, ctx)
         call discard_unexported_wrappers(ctx)
         call fill_exception(r, 'method raised error', err)
      else
         call flush_cap_table(conn, ctx, err)
         if (err /= CAPNP_OK) then
            ! Exports added by the failed flush are already dropped.
            ! Owned servers with no export slot are freed here, once.
            call release_unflushed(conn, ctx)
            call rollback_staged(conn, ctx)
            call fill_exception(r, 'export failed', err)
         end if
      end if
      conn%exports(eid)%dispatching = conn%exports(eid)%dispatching - 1
      call drop_export(conn, eid, 1)
      call finish_answer(conn, qid, rm, ctx, err, &
                         elsewhere=call_send_results_to_which(c) == &
                         CALL_SEND_RESULTS_TO_YOURSELF_TAG)
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
      out_eid = -1
      tgt = call_target_get(c, err)
      if (err /= CAPNP_OK) return
      call resolve_message_target(conn, tgt, out_eid, err)
   end subroutine resolve_target

   !> Resolve a MessageTarget to a local export index, or -1 when it names
   !> nothing this vat hosts. Shared by Call and Join, which address
   !> capabilities the same way.
   subroutine resolve_message_target(conn, tgt, out_eid, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(message_target_t), intent(in) :: tgt
      integer, intent(out) :: out_eid
      integer, intent(out) :: err
      type(promised_answer_t) :: pa
      type(promised_answer_op_t) :: op
      type(capnp_ptr_t) :: ops, p, ctab
      type(message_t) :: amsg
      type(return_t) :: ar
      type(payload_t) :: apl
      type(cap_descriptor_t) :: cd
      integer(int64) :: aqid, eid
      integer :: i
      type(rpc_answer_slot_t), pointer :: aslot
      logical :: afound
      out_eid = -1
      err = CAPNP_OK
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
         call answer_find(conn, aqid, aslot, afound)
         if (.not. afound) return
         if (.not. aslot%has_results) return
         amsg = message_read_root(aslot%retmsg, err)
         ar = message_return_get(amsg, err)
         apl = return_results_get(ar, err)
         p = payload_content_get(apl, err)
         if (err /= CAPNP_OK) return
         ops = promised_answer_transform_get(pa, err)
         if (err /= CAPNP_OK) return
         do i = 0, int(capnp_list_len(ops)) - 1
            op%p = capnp_list_get_struct(ops, i, err)
            if (err /= CAPNP_OK) return
            select case (promised_answer_op_which(op))
            case (PROMISED_ANSWER_OP_NOOP_TAG)
            case (PROMISED_ANSWER_OP_GET_POINTER_FIELD_TAG)
               p = capnp_getp(p, int(promised_answer_op_get_pointer_field_get(op)), err)
               if (err /= CAPNP_OK) return
            case default
               call abort_conn(conn, 'Unsupported pipeline op.', err)
               return
            end select
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
      case default
         call abort_conn(conn, 'Unknown message target type.', err)
      end select
   end subroutine resolve_message_target

   ! ------------------------------------------------------------------
   ! Level 3: introductions handed to us
   ! ------------------------------------------------------------------

   !> Record every `thirdPartyHosted` entry in an incoming cap table.
   !>
   !> The descriptor says where the capability really lives and hands us
   !> a vine, an ordinary import through the introducer. Calls on the
   !> vine work right away, which is the fallback the spec gives
   !> receivers that cannot reach a third party; the vine must therefore
   !> outlive the pickup. Dialling the third vat belongs to the network
   !> layer, so the arrangement is recorded and handed over rather than
   !> acted on here.
   subroutine note_introductions(conn, pl, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(payload_t), intent(in) :: pl
      integer, intent(out) :: err
      type(capnp_ptr_t) :: ctab, idp
      type(cap_descriptor_t) :: cd
      type(third_party_cap_descriptor_t) :: tp
      type(third_party_cap_id_t) :: id
      type(vat_id_t) :: vat
      character(len=:), allocatable :: host
      integer :: i, j, n
      err = CAPNP_OK
      if (pl%p%kind /= CAPNP_PK_STRUCT) return
      ctab = payload_cap_table_get(pl, err)
      if (err /= CAPNP_OK) then
         err = CAPNP_OK
         return
      end if
      n = int(capnp_list_len(ctab))
      do i = 0, n - 1
         cd%p = capnp_list_get_struct(ctab, i, err)
         if (err /= CAPNP_OK) cycle
         if (cap_descriptor_which(cd) /= CAP_DESCRIPTOR_THIRD_PARTY_HOSTED_TAG) cycle
         tp = cap_descriptor_third_party_hosted_get(cd, err)
         if (err /= CAPNP_OK .or. tp%p%kind /= CAPNP_PK_STRUCT) cycle
         idp = third_party_cap_descriptor_id_get(tp, err)
         if (err /= CAPNP_OK .or. idp%kind /= CAPNP_PK_STRUCT) cycle
         id%p = idp
         vat = third_party_cap_id_vat_get(id, err)
         if (err /= CAPNP_OK .or. vat%p%kind /= CAPNP_PK_STRUCT) cycle
         call vat_id_host_get(vat, host, err)
         if (err /= CAPNP_OK .or. .not. allocated(host)) cycle
         ! A host that will not fit is refused rather than truncated: a
         ! truncated address names a different vat.
         if (len(host) > MAXHOST) cycle

         do j = 0, MAXINTRO - 1
            if (conn%introductions(j)%used) cycle
            conn%introductions(j)%used = .true.
            conn%introductions(j)%nonce = third_party_cap_id_nonce_get(id)
            conn%introductions(j)%vine_id = &
               third_party_cap_descriptor_vine_id_get(tp)
            conn%introductions(j)%port = int(vat_id_port_get(vat))
            conn%introductions(j)%host = host
            conn%introductions(j)%host_len = len(host)
            exit
         end do
      end do
      err = CAPNP_OK
   end subroutine note_introductions

   !> Introductions handed to us and not yet picked up.
   function rpc_pending_introductions(conn, out) result(n)
      type(rpc_conn_t), intent(in) :: conn
      type(rpc_introduction_t), intent(out), optional :: out(:)
      integer :: n, i
      n = 0
      do i = 0, MAXINTRO - 1
         if (.not. conn%introductions(i)%used) cycle
         if (present(out)) then
            if (n < size(out)) out(n + 1) = conn%introductions(i)
         end if
         n = n + 1
      end do
   end function rpc_pending_introductions

   !> Open the connection named by an outstanding third-party
   !> introduction. The returned connection is ready for rpc_accept_send;
   !> successful acceptance must be followed by rpc_introduction_done on
   !> the introducing connection so its vine can be released.
   subroutine rpc_introduction_dial(conn, nonce, introduced, bootstrap, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: nonce
      type(rpc_conn_t), intent(inout), target :: introduced
      class(rpc_server_t), pointer, intent(in) :: bootstrap
      integer, intent(out) :: err
      integer :: fd, i
      logical :: allow
      err = CAPNP_ERR_BOUNDS
      do i = 0, MAXINTRO - 1
         if (.not. conn%introductions(i)%used) cycle
         if (conn%introductions(i)%nonce /= nonce) cycle
         allow = .false.
         if (associated(conn%dial_allow)) then
            call conn%dial_allow( &
               conn%introductions(i)%host(1:conn%introductions(i)%host_len), &
               conn%introductions(i)%port, allow)
         end if
         if (.not. allow) then
            err = CAPNP_ERR_ARG
            return
         end if
         call px_tcp_connect_host( &
            conn%introductions(i)%host(1:conn%introductions(i)%host_len), &
            conn%introductions(i)%port, fd, err, timeout_ms=conn%dial_timeout_ms)
         if (err /= CAPNP_OK) return
         call rpc_conn_init(introduced, fd, bootstrap)
         return
      end do
   end subroutine rpc_introduction_dial

   !> Finish the pickup for `nonce`: releases the vine, which the sender
   !> treats as the signal to close its Provide. Call this only once the
   !> third party has actually handed the capability over; releasing
   !> early drops the fallback path with nothing in its place.
   subroutine rpc_introduction_done(conn, nonce, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: nonce
      integer, intent(out) :: err
      type(rpc_cap_t) :: vine
      integer :: i
      err = CAPNP_ERR_BOUNDS
      do i = 0, MAXINTRO - 1
         if (.not. conn%introductions(i)%used) cycle
         if (conn%introductions(i)%nonce /= nonce) cycle
         vine%kind = RPC_CAP_IMPORT
         vine%id = conn%introductions(i)%vine_id
         call rpc_release_send(conn, vine, err)
         conn%introductions(i) = rpc_introduction_t()
         return
      end do
   end subroutine rpc_introduction_done

   !> Write a `thirdPartyHosted` descriptor into `cd`: where the
   !> recipient should go, which pending Provide to claim once there, and
   !> the vine we export so calls work in the meantime.
   subroutine rpc_write_third_party_cap(cd, host, port, nonce, vine_id, err)
      type(cap_descriptor_t), intent(in) :: cd
      character(len=*), intent(in) :: host
      integer, intent(in) :: port
      integer(int64), intent(in) :: nonce, vine_id
      integer, intent(out) :: err
      type(third_party_cap_descriptor_t) :: tp
      type(third_party_cap_id_t) :: id
      type(vat_id_t) :: vat
      tp = cap_descriptor_third_party_hosted_init(cd, err)
      if (err /= CAPNP_OK) return
      call third_party_cap_descriptor_vine_id_set(tp, vine_id, err)
      if (err /= CAPNP_OK) return
      id = third_party_cap_id_new(cd%p%msg, err)
      if (err /= CAPNP_OK) return
      call third_party_cap_id_nonce_set(id, nonce, err)
      if (err /= CAPNP_OK) return
      vat = third_party_cap_id_vat_init(id, err)
      if (err /= CAPNP_OK) return
      call vat_id_host_set(vat, host, err)
      if (err /= CAPNP_OK) return
      call vat_id_port_set(vat, port, err)
      if (err /= CAPNP_OK) return
      call third_party_cap_descriptor_id_set(tp, id%p, err)
   end subroutine rpc_write_third_party_cap

   !> Level 3: hold a capability for whoever presents this nonce.
   !>
   !> The answer is an empty Return: the introducer is not waiting for a
   !> value, only for confirmation that the arrangement is recorded.
   subroutine handle_provide(conn, msg, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(message_t), intent(in) :: msg
      integer, intent(out) :: err
      type(provide_t) :: pv
      type(message_target_t) :: tgt
      type(recipient_id_t) :: rid
      type(capnp_ptr_t) :: rp
      integer(int64) :: qid, nonce
      integer :: eid, i

      pv = message_provide_get(msg, err)
      if (err /= CAPNP_OK) return
      qid = provide_question_id_get(pv)
      if (.not. claim_question(conn, qid, err)) return

      tgt = provide_target_get(pv, err)
      if (err /= CAPNP_OK) return
      call resolve_message_target(conn, tgt, eid, err)
      if (err /= CAPNP_OK) return
      if (eid < 0) then
         call send_answer_exception(conn, qid, 'provide: no such capability', err)
         return
      end if

      rp = provide_recipient_get(pv, err)
      if (err /= CAPNP_OK) return
      if (rp%kind /= CAPNP_PK_STRUCT) then
         call send_answer_exception(conn, qid, 'provide: no recipient', err)
         return
      end if
      rid%p = rp
      nonce = recipient_id_nonce_get(rid)

      do i = 0, MAXPROV - 1
         if (.not. conn%vat%provisions(i)%used) then
            conn%vat%provisions(i)%used = .true.
            conn%vat%provisions(i)%nonce = nonce
            conn%vat%provisions(i)%srv => conn%exports(eid)%srv
            conn%vat%provisions(i)%eid = eid
            conn%vat%provisions(i)%source_eid = eid
            conn%vat%provisions(i)%host => conn
            conn%vat%provisions(i)%qid = qid
            conn%vat%provisions(i)%embargoed = .false.
            ! Two pins. The export count keeps the slot from being
            ! reused while the arrangement is open. The object reference
            ! keeps an owned wrapper alive even if a later Release
            ! consumes every export ref, including this one.
            conn%exports(eid)%refcount = conn%exports(eid)%refcount + 1
            call retain_server(conn%exports(eid)%srv)
            call send_empty_answer(conn, qid, err)
            return
         end if
      end do
      call send_answer_exception(conn, qid, 'provide: table full', err)
   end subroutine handle_provide

   !> Level 3: claim a capability a third vat provided for us.
   !>
   !> A nonce is single-use. Leaving it claimable would let anyone who
   !> learns it take the capability again later.
   subroutine handle_accept(conn, msg, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(message_t), intent(in) :: msg
      integer, intent(out) :: err
      type(accept_t) :: ac
      type(provision_id_t) :: pid
      type(capnp_ptr_t) :: pp
      integer(int64) :: qid, nonce
      integer :: i, eid, new_eid

      ac = message_accept_get(msg, err)
      if (err /= CAPNP_OK) return
      qid = accept_question_id_get(ac)
      if (.not. claim_question(conn, qid, err)) return

      pp = accept_provision_get(ac, err)
      if (err /= CAPNP_OK) return
      if (pp%kind /= CAPNP_PK_STRUCT) then
         call send_answer_exception(conn, qid, 'accept: no provision', err)
         return
      end if
      pid%p = pp
      nonce = provision_id_nonce_get(pid)

      eid = -1
      do i = 0, MAXPROV - 1
         if (conn%vat%provisions(i)%used .and. .not. conn%vat%provisions(i)%embargoed &
             .and. conn%vat%provisions(i)%nonce == nonce) then
            ! Export it here: the id the introducer used belongs to its
            ! own connection and means nothing on this one. A new slot
            ! takes its own reference on an owned membrane.
            new_eid = export_server(conn, conn%vat%provisions(i)%srv)
            if (new_eid < 0) then
               call send_answer_exception(conn, qid, 'accept: export table full', err)
               return
            end if
            eid = new_eid
            conn%vat%provisions(i)%eid = new_eid
            if (accept_embargo_get(ac)) then
               ! Claimed, but the Return waits: the recipient has calls
               ! in flight through the introducer, and answering now
               ! would let one sent straight to us overtake them. The
               ! introducer lifts it with Disembargo.provide. The pin
               ! stays until that Return is sent. The answer and the
               ! export live on this connection, not the introducer's.
               conn%vat%provisions(i)%embargoed = .true.
               conn%vat%provisions(i)%accept_qid = qid
               conn%vat%provisions(i)%recipient => conn
            else
               call release_provision_pin(conn%vat%provisions(i))
               conn%vat%provisions(i) = rpc_provision_slot_t()
            end if
            exit
         end if
      end do
      if (eid < 0) then
         call send_answer_exception(conn, qid, 'accept: no such provision', err)
         return
      end if
      if (accept_embargo_get(ac)) then
         err = CAPNP_OK
         return
      end if
      call send_accepted_cap(conn, qid, eid, err)
   end subroutine handle_accept

   !> Answer an Accept with the capability it claimed.
   subroutine send_accepted_cap(conn, qid, eid, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: qid
      integer, intent(in) :: eid
      integer, intent(out) :: err
      type(capnp_message_t), target :: rm
      type(message_t) :: rmsg
      type(return_t) :: r
      type(rpc_call_ctx_t) :: ctx
      type(cap_descriptor_t) :: cd
      type(capnp_ptr_t) :: ctab
      call capnp_message_init_builder(rm, err)
      if (err /= CAPNP_OK) return
      rmsg = message_new_root(rm, err)
      if (err /= CAPNP_OK) return
      r = message_return_init(rmsg, err)
      if (err /= CAPNP_OK) return
      call return_answer_id_set(r, qid, err)
      if (err /= CAPNP_OK) return
      ctx%results = return_results_init(r, err)
      if (err /= CAPNP_OK) return
      ctab = payload_cap_table_init(ctx%results, 1_int64, err)
      if (err /= CAPNP_OK) return
      cd%p = capnp_list_get_struct(ctab, 0, err)
      if (err /= CAPNP_OK) return
      call cap_descriptor_sender_hosted_set(cd, int(eid, int64), err)
      if (err /= CAPNP_OK) return
      call payload_content_set(ctx%results, rpc_make_cap_ptr(rm, 0), err)
      if (err /= CAPNP_OK) return
      ! export_server already counted this result. Finish with
      ! releaseResultCaps drops that reference, and so does a failed
      ! send. The id was not staged, so record it the way a Join result
      ! records an export it did not flush.
      ctx%nexp = 1
      ctx%staged_eids(0) = eid
      call finish_answer(conn, qid, rm, ctx, err)
   end subroutine send_accepted_cap

   !> Disembargo.provide: the introducer lifts the embargo on the Accept
   !> it arranged, naming its own Provide question.
   subroutine release_provide_embargo(conn, provide_qid, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: provide_qid
      integer, intent(out) :: err
      type(rpc_conn_t), pointer :: recip
      integer(int64) :: qid
      integer :: i, eid
      err = CAPNP_OK
      do i = 0, MAXPROV - 1
         if (.not. conn%vat%provisions(i)%used) cycle
         if (.not. conn%vat%provisions(i)%embargoed) cycle
         if (conn%vat%provisions(i)%qid /= provide_qid) cycle
         ! Only the introducer that arranged the Provide may lift it.
         ! The recipient naming the same question id must not.
         if (.not. associated(conn%vat%provisions(i)%host, conn)) cycle
         qid = conn%vat%provisions(i)%accept_qid
         eid = conn%vat%provisions(i)%eid
         recip => conn%vat%provisions(i)%recipient
         call release_provision_pin(conn%vat%provisions(i))
         conn%vat%provisions(i) = rpc_provision_slot_t()
         if (.not. associated(recip)) recip => conn
         call send_accepted_cap(recip, qid, eid, err)
         return
      end do
      ! A Disembargo naming nothing we hold is the sender's problem, not
      ! a reason to disturb this connection.
   end subroutine release_provide_embargo

   !> Accepts claimed but still embargoed, awaiting Disembargo.provide.
   function rpc_embargoed_accepts(conn) result(n)
      type(rpc_conn_t), intent(in) :: conn
      integer :: n, i
      n = 0
      do i = 0, MAXPROV - 1
         if (conn%vat%provisions(i)%used .and. conn%vat%provisions(i)%embargoed) n = n + 1
      end do
   end function rpc_embargoed_accepts

   !> Answer a question with empty results.
   subroutine send_empty_answer(conn, qid, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: qid
      integer, intent(out) :: err
      type(capnp_message_t), target :: rm
      type(message_t) :: rmsg
      type(return_t) :: r
      type(rpc_call_ctx_t) :: ctx
      call capnp_message_init_builder(rm, err)
      if (err /= CAPNP_OK) return
      rmsg = message_new_root(rm, err)
      if (err /= CAPNP_OK) return
      r = message_return_init(rmsg, err)
      if (err /= CAPNP_OK) return
      call return_answer_id_set(r, qid, err)
      if (err /= CAPNP_OK) return
      ctx%results = return_results_init(r, err)
      if (err /= CAPNP_OK) return
      call finish_answer(conn, qid, rm, ctx, err)
   end subroutine send_empty_answer

   !> Answer a question with an exception.
   subroutine send_answer_exception(conn, qid, reason, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: qid
      character(len=*), intent(in) :: reason
      integer, intent(out) :: err
      type(capnp_message_t), target :: rm
      type(message_t) :: rmsg
      type(return_t) :: r
      type(rpc_call_ctx_t) :: ctx
      call capnp_message_init_builder(rm, err)
      if (err /= CAPNP_OK) return
      rmsg = message_new_root(rm, err)
      if (err /= CAPNP_OK) return
      r = message_return_init(rmsg, err)
      if (err /= CAPNP_OK) return
      call return_answer_id_set(r, qid, err)
      if (err /= CAPNP_OK) return
      call fill_exception(r, reason, err)
      if (err /= CAPNP_OK) return
      call finish_answer(conn, qid, rm, ctx, err)
   end subroutine send_answer_exception

   !> Nonces of capabilities held for a third vat.
   function rpc_pending_provisions(conn) result(n)
      type(rpc_conn_t), intent(in) :: conn
      integer :: n, i
      n = 0
      do i = 0, MAXPROV - 1
         if (conn%vat%provisions(i)%used) n = n + 1
      end do
   end function rpc_pending_provisions

   !> Share level 3 arrangements with this vat's other connections. Call
   !> after rpc_conn_init and before any Provide; `vat` must outlive the
   !> connection.
   subroutine rpc_conn_set_vat(conn, vat)
      type(rpc_conn_t), intent(inout), target :: conn
      type(rpc_vat_t), pointer, intent(in) :: vat
      if (associated(vat)) then
         conn%vat => vat
      else
         conn%vat => conn%own_vat
      end if
   end subroutine rpc_conn_set_vat

   !> Join (level 4): does this set of capabilities name one object?
   !>
   !> Each part is its own question addressed to one of the candidates. A
   !> part cannot be answered on arrival, because the answer depends on
   !> the whole set, so parts accumulate against their joinId and every
   !> question is answered once the last one lands.
   !>
   !> On a two-party network the comparison is decidable locally: both
   !> ends of the connection can only name capabilities this vat exports,
   !> so equal export ids mean the same object. A vat bridging to another
   !> network would instead forward the join onward, which needs a
   !> three-party layer this transport does not have (rpc-twoparty.capnp:
   !> "Never used, because there is no third party").
   subroutine handle_join(conn, msg, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(message_t), intent(in) :: msg
      integer, intent(out) :: err
      type(join_t) :: j
      type(message_target_t) :: tgt
      type(join_key_part_t) :: kp
      type(capnp_ptr_t) :: kpp
      integer(int64) :: qid, jid
      integer :: slot, pnum, pcount, eid

      j = message_join_get(msg, err)
      if (err /= CAPNP_OK) return
      qid = join_question_id_get(j)
      if (.not. claim_question(conn, qid, err)) return

      kpp = join_key_part_get(j, err)
      if (err /= CAPNP_OK) return
      if (kpp%kind /= CAPNP_PK_STRUCT) then
         ! No JoinKeyPart means we cannot tell which set this belongs to.
         call send_join_failure(conn, qid, 0_int64, err)
         return
      end if
      kp%p = kpp
      jid = join_key_part_join_id_get(kp)
      pcount = int(join_key_part_part_count_get(kp))
      pnum = int(join_key_part_part_num_get(kp))

      if (pcount <= 0 .or. pcount > MAXJOINPARTS .or. &
          pnum < 0 .or. pnum >= pcount) then
         call send_join_failure(conn, qid, jid, err)
         return
      end if

      tgt = join_target_get(j, err)
      if (err /= CAPNP_OK) return
      call resolve_message_target(conn, tgt, eid, err)
      if (err /= CAPNP_OK) return

      call join_slot_find(conn, jid, pcount, slot)
      if (slot < 0) then
         ! Join table full, or a conflicting partCount for this joinId.
         call send_join_failure(conn, qid, jid, err)
         return
      end if

      if (conn%joins(slot)%seen(pnum)) then
         ! Duplicate part: the sender must not reuse a partNum before the
         ! set completes.
         call send_join_failure(conn, qid, jid, err)
         return
      end if
      conn%joins(slot)%seen(pnum) = .true.
      conn%joins(slot)%qids(pnum) = qid
      conn%joins(slot)%eids(pnum) = eid
      conn%joins(slot)%nseen = conn%joins(slot)%nseen + 1

      if (conn%joins(slot)%nseen == pcount) call join_complete(conn, slot, err)
   end subroutine handle_join

   !> Find the slot holding joinId, or claim a free one. Returns -1 when
   !> the table is full or partCount disagrees with the parts already in.
   subroutine join_slot_find(conn, jid, pcount, slot)
      type(rpc_conn_t), intent(inout) :: conn
      integer(int64), intent(in) :: jid
      integer, intent(in) :: pcount
      integer, intent(out) :: slot
      integer :: i
      slot = -1
      do i = 0, MAXJOINS - 1
         if (conn%joins(i)%used .and. conn%joins(i)%join_id == jid) then
            if (conn%joins(i)%part_count /= pcount) return
            slot = i
            return
         end if
      end do
      do i = 0, MAXJOINS - 1
         if (.not. conn%joins(i)%used) then
            conn%joins(i) = rpc_join_slot_t()
            conn%joins(i)%used = .true.
            conn%joins(i)%join_id = jid
            conn%joins(i)%part_count = pcount
            slot = i
            return
         end if
      end do
   end subroutine join_slot_find

   !> The set is complete: compare the targets and answer every part.
   subroutine join_complete(conn, slot, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer, intent(in) :: slot
      integer, intent(out) :: err
      logical :: same
      integer :: i, first_eid
      first_eid = conn%joins(slot)%eids(0)
      ! An unresolved part names nothing we host, so the set cannot be
      ! proven equal.
      same = first_eid >= 0
      if (same) then
         do i = 1, conn%joins(slot)%part_count - 1
            if (conn%joins(slot)%eids(i) /= first_eid) then
               same = .false.
               exit
            end if
         end do
      end if
      ! Exactly one result carries the joined capability, per JoinResult.
      do i = 0, conn%joins(slot)%part_count - 1
         call send_join_result(conn, conn%joins(slot)%qids(i), &
                               conn%joins(slot)%join_id, same, &
                               same .and. i == 0, first_eid, err)
         if (err /= CAPNP_OK) exit
      end do
      conn%joins(slot) = rpc_join_slot_t()
   end subroutine join_complete

   !> Answer one Join question with a JoinResult payload.
   subroutine send_join_result(conn, qid, jid, succeeded, with_cap, eid, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: qid, jid
      logical, intent(in) :: succeeded, with_cap
      integer, intent(in) :: eid
      integer, intent(out) :: err
      type(capnp_message_t), target :: rm
      type(message_t) :: rmsg
      type(return_t) :: r
      type(rpc_call_ctx_t) :: ctx
      type(join_result_t) :: jr
      type(capnp_ptr_t) :: ctab
      type(cap_descriptor_t) :: cd
      call capnp_message_init_builder(rm, err)
      if (err /= CAPNP_OK) return
      rmsg = message_new_root(rm, err)
      if (err /= CAPNP_OK) return
      r = message_return_init(rmsg, err)
      if (err /= CAPNP_OK) return
      call return_answer_id_set(r, qid, err)
      if (err /= CAPNP_OK) return
      ctx%results = return_results_init(r, err)
      if (err /= CAPNP_OK) return
      jr = join_result_new(rm, err)
      if (err /= CAPNP_OK) return
      call join_result_join_id_set(jr, jid, err)
      if (err /= CAPNP_OK) return
      call join_result_succeeded_set(jr, succeeded, err)
      if (err /= CAPNP_OK) return
      if (with_cap .and. eid >= 0) then
         ! The export id is already known, so the capTable is written
         ! directly rather than through the staging path, which exists to
         ! turn server pointers into ids. The receiver gains a reference,
         ! so the refcount rises with it.
         ctab = payload_cap_table_init(ctx%results, 1_int64, err)
         if (err /= CAPNP_OK) return
         cd%p = capnp_list_get_struct(ctab, 0, err)
         if (err /= CAPNP_OK) return
         call cap_descriptor_sender_hosted_set(cd, int(eid, int64), err)
         if (err /= CAPNP_OK) return
         conn%exports(eid)%refcount = conn%exports(eid)%refcount + 1
         ! Finish(releaseResultCaps) drops this reference. The cap was
         ! not staged, so record it the way a flushed export is recorded.
         ctx%nexp = 1
         ctx%staged_eids(0) = eid
         call join_result_cap_set(jr, rpc_make_cap_ptr(rm, 0), err)
         if (err /= CAPNP_OK) return
      end if
      call payload_content_set(ctx%results, jr%p, err)
      if (err /= CAPNP_OK) return
      call finish_answer(conn, qid, rm, ctx, err)
   end subroutine send_join_result

   !> A join we cannot evaluate: answer this part with succeeded = false.
   subroutine send_join_failure(conn, qid, jid, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: qid, jid
      integer, intent(out) :: err
      call send_join_result(conn, qid, jid, .false., .false., -1, err)
   end subroutine send_join_failure

   !> Free owned servers staged for a return that will not publish them.
   !>
   !> Dispatch errors and exception returns leave these out of the export
   !> table. A flush that fails rolls its new exports back before this
   !> runs, so a server still in the table was exported earlier. The same
   !> pointer staged twice is freed once. A pointer already exported keeps
   !> that export and is freed when its refcount drops to zero.
   subroutine release_unflushed(conn, ctx)
      type(rpc_conn_t), intent(inout) :: conn
      type(rpc_call_ctx_t), intent(inout) :: ctx
      integer :: i, j
      class(*), pointer :: p
      logical :: exported
      do i = 0, ctx%nexp - 1
         if (.not. associated(ctx%staged(i)%s)) cycle
         do j = 0, i - 1
            if (associated(ctx%staged(j)%s, ctx%staged(i)%s)) then
               if (ctx%staged(i)%owned) ctx%staged(j)%owned = .true.
               ctx%staged(i)%s => null()
               ctx%staged(i)%owned = .false.
               exit
            end if
         end do
      end do
      do i = 0, ctx%nexp - 1
         if (.not. ctx%staged(i)%owned) cycle
         if (.not. associated(ctx%staged(i)%s)) cycle
         p => ctx%staged(i)%s
         exported = .false.
         do j = 0, MAXE - 1
            if (conn%exports(j)%used .and. associated(conn%exports(j)%srv, p)) then
               conn%exports(j)%owned = .true.
               exported = .true.
               exit
            end if
         end do
         if (.not. exported) deallocate (ctx%staged(i)%s)
         ctx%staged(i)%s => null()
         ctx%staged(i)%owned = .false.
      end do
   end subroutine release_unflushed

   !> Drop the exports `flush_cap_table` added and restore every export
   !> that was already live.
   !>
   !> Cap'n Proto 1.4.0 answers a failure while building the results cap
   !> table with an exception Return (`RpcCallContext::sendErrorReturn`,
   !> and `fromException` on the bootstrap path) and does not publish
   !> that table. The peer therefore has no export id to Release. An
   !> owned server whose only export came from this flush is released
   !> once. An export that already existed keeps the refcount and owned
   !> flag it had on entry; its server stays.
   subroutine rollback_flushed_exports(conn, ctx, base_used, base_ref, base_owned)
      type(rpc_conn_t), intent(inout) :: conn
      type(rpc_call_ctx_t), intent(inout) :: ctx
      logical, intent(in) :: base_used(0:)
      integer, intent(in) :: base_ref(0:)
      logical, intent(in) :: base_owned(0:)
      class(*), pointer :: p
      integer :: i, j, delta
      do j = 0, MAXE - 1
         if (.not. base_used(j)) then
            if (.not. conn%exports(j)%used) cycle
            p => conn%exports(j)%srv
            do i = 0, ctx%nexp - 1
               if (associated(p)) then
                  if (associated(ctx%staged(i)%s, p)) then
                     ctx%staged(i)%s => null()
                     ctx%staged(i)%owned = .false.
                  end if
               end if
               if (ctx%staged_eids(i) == j) ctx%staged_eids(i) = -1
            end do
            call drop_export(conn, j, conn%exports(j)%refcount)
         else if (conn%exports(j)%used) then
            delta = conn%exports(j)%refcount - base_ref(j)
            if (delta > 0) call drop_export(conn, j, delta)
            if (conn%exports(j)%used) conn%exports(j)%owned = base_owned(j)
            do i = 0, ctx%nexp - 1
               if (ctx%staged_eids(i) == j) ctx%staged_eids(i) = -1
            end do
         end if
      end do
   end subroutine rollback_flushed_exports

   !> Allocate export ids for the staged servers and write the results
   !> capTable. On failure the exports this flush added are dropped and
   !> every pre-existing export is restored to its earlier refcount.
   subroutine flush_cap_table(conn, ctx, err)
      type(rpc_conn_t), intent(inout) :: conn
      type(rpc_call_ctx_t), intent(inout) :: ctx
      integer, intent(out) :: err
      type(capnp_ptr_t) :: ctab
      type(cap_descriptor_t) :: cd
      logical :: base_used(0:MAXE - 1)
      integer :: base_ref(0:MAXE - 1)
      logical :: base_owned(0:MAXE - 1)
      integer :: i, eid, j
      err = CAPNP_OK
      if (ctx%nexp == 0) return
      do j = 0, MAXE - 1
         base_used(j) = conn%exports(j)%used
         base_ref(j) = conn%exports(j)%refcount
         base_owned(j) = conn%exports(j)%owned
      end do
      ctab = payload_cap_table_init(ctx%results, int(ctx%nexp, int64), err)
      if (err /= CAPNP_OK) return
      do i = 0, ctx%nexp - 1
         eid = export_server(conn, ctx%staged(i)%s, ctx%staged(i)%owned)
         if (eid < 0) then
            call rollback_flushed_exports(conn, ctx, base_used, base_ref, base_owned)
            err = CAPNP_ERR_ALLOC
            return
         end if
         ctx%staged_eids(i) = eid
         cd%p = capnp_list_get_struct(ctab, i, err)
         if (err /= CAPNP_OK) then
            call rollback_flushed_exports(conn, ctx, base_used, base_ref, base_owned)
            return
         end if
         call cap_descriptor_sender_hosted_set(cd, int(eid, int64), err)
         if (err /= CAPNP_OK) then
            call rollback_flushed_exports(conn, ctx, base_used, base_ref, base_owned)
            return
         end if
      end do
   end subroutine flush_cap_table

   !> Return for sendResultsTo.yourself: the results stay in the answer
   !> slot and the caller is told they were delivered elsewhere.
   subroutine send_results_elsewhere(conn, qid, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: qid
      integer, intent(out) :: err
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(return_t) :: r
      call capnp_message_init_builder(m, err)
      if (err /= CAPNP_OK) return
      msg = message_new_root(m, err)
      if (err /= CAPNP_OK) return
      r = message_return_init(msg, err)
      if (err /= CAPNP_OK) return
      call return_answer_id_set(r, qid, err)
      if (err /= CAPNP_OK) return
      call return_results_sent_elsewhere_set(r, err)
      if (err == CAPNP_OK) call rpc_send_message(conn%transport, m, err)
      call capnp_message_free(m)
   end subroutine send_results_elsewhere

   !> Send the Return and stash a copy in the answers table so pipelined
   !> calls can resolve against it until Finish arrives. `elsewhere`
   !> holds the results and tells the caller they were sent elsewhere.
   !> That hold is the only answer a later takeFromOtherQuestion may take.
   subroutine finish_answer(conn, qid, rm, ctx, err, elsewhere)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: qid
      type(capnp_message_t), intent(inout) :: rm
      type(rpc_call_ctx_t), intent(in) :: ctx
      integer, intent(out) :: err
      logical, intent(in), optional :: elsewhere
      integer(int8), allocatable :: bytes(:)
      integer :: i
      logical :: hold, found
      type(rpc_answer_slot_t), pointer :: slot
      type(message_t) :: heldmsg
      type(return_t) :: heldr
      integer :: herr
      logical :: capless
      hold = .false.
      capless = .false.
      err = CAPNP_OK
      if (present(elsewhere)) hold = elsewhere
      if (qid < 0_int64) then
         do i = 0, ctx%nexp - 1
            if (ctx%staged_eids(i) >= 0) call drop_export(conn, ctx%staged_eids(i), 1)
         end do
         call capnp_message_free(rm)
         call abort_conn(conn, 'questionId is outside the supported range', err)
         return
      end if
      call answer_find(conn, qid, slot, found)
      if (.not. found) call answer_add_hi(conn, qid, slot)
      if (slot%has_results) then
         ! A question that already has a Return must not drop the
         ! exports of that earlier answer.
         do i = 0, ctx%nexp - 1
            if (ctx%staged_eids(i) >= 0) call drop_export(conn, ctx%staged_eids(i), 1)
         end do
         call capnp_message_free(rm)
         call abort_conn(conn, 'questionId is already in use', err)
         return
      end if
      ! yourself results stay here until takeFromOtherQuestion. C++ sends
      ! resultsSentElsewhere from sendRedirectReturn, not from the call.
      if (.not. hold) then
         heldmsg = message_read_root(rm, herr)
         if (herr == CAPNP_OK) then
            if (message_which(heldmsg) == MESSAGE_RETURN_TAG) then
               heldr = message_return_get(heldmsg, herr)
               ! Cap-less results need no Finish (rpc.c++ sendReturn,
               ! noFinishNeeded when the response has no capabilities).
               if (herr == CAPNP_OK) then
                  if (return_which(heldr) == RETURN_RESULTS_TAG .and. ctx%nexp == 0) then
                     call return_no_finish_needed_set(heldr, .true., err)
                     if (err == CAPNP_OK) capless = .true.
                  end if
               end if
            end if
         end if
         ! Close from inside dispatch drops the transport. The server
         ! is freed when this call returns, and there is no peer left.
         if (err == CAPNP_OK) then
            if (.not. associated(conn%transport)) then
               err = CAPNP_ERR_IO
            else
               call rpc_send_message(conn%transport, rm, err)
            end if
         end if
      end if
      if (err /= CAPNP_OK) then
         ! The peer never saw the exports this answer published.
         do i = 0, ctx%nexp - 1
            if (ctx%staged_eids(i) >= 0) call drop_export(conn, ctx%staged_eids(i), 1)
         end do
         call capnp_message_free(rm)
         return
      end if
      if (capless) then
         call capnp_message_free(rm)
         call answer_slot_clear(slot)
         if (qid >= int(MAXQ, int64)) call answer_drop_hi(conn, qid)
         return
      end if
      call capnp_serialize_bytes(rm, bytes, err)
      if (err == CAPNP_OK) then
         call capnp_deserialize_bytes(bytes, slot%retmsg, err)
         slot%used = .true.
         slot%has_results = .true.
         slot%redirected = hold
         slot%nexp = ctx%nexp
         do i = 0, ctx%nexp - 1
            slot%exports(i) = ctx%staged_eids(i)
         end do
      end if
      call capnp_message_free(rm)
   end subroutine finish_answer

   !> Copy a yourself answer into the question that asked to take it,
   !> then tell the peer the redirect is complete.
   subroutine adopt_redirected(conn, q, other, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer, intent(in) :: q
      integer(int64), intent(in) :: other
      integer, intent(out) :: err
      type(rpc_answer_slot_t), pointer :: slot
      logical :: found
      integer(int8), allocatable :: bytes(:)
      integer :: i, eid
      call answer_find(conn, other, slot, found)
      if (.not. found .or. .not. associated(slot) .or. .not. slot%used) then
         call abort_conn(conn, &
                         '`Return.takeFromOtherQuestion` had invalid answer ID.', err)
         return
      end if
      if (.not. slot%redirected .or. .not. slot%has_results) then
         call abort_conn(conn, &
                         '`Return.takeFromOtherQuestion` referenced a call that ' // &
                         'did not use `sendResultsTo.yourself`.', err)
         return
      end if
      call capnp_serialize_bytes(slot%retmsg, bytes, err)
      if (err /= CAPNP_OK) return
      call capnp_deserialize_bytes(bytes, conn%questions(q)%retmsg, err)
      if (err /= CAPNP_OK) return
      conn%questions(q)%returned = .true.
      conn%questions(q)%local_results = .true.
      ! The question now holds the caps. Finish of the yourself call
      ! still drops the answer's own refs.
      do i = 0, slot%nexp - 1
         eid = slot%exports(i)
         if (eid >= 0 .and. eid < MAXE) then
            if (conn%exports(eid)%used) &
               conn%exports(eid)%refcount = conn%exports(eid)%refcount + 1
         end if
      end do
      slot%redirected = .false.
      call send_results_elsewhere(conn, other, err)
   end subroutine adopt_redirected

   !> A canceled question still owes resultsSentElsewhere when the
   !> takeFrom names a yourself answer. A missing answer is not an abort.
   subroutine release_redirected(conn, other, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: other
      integer, intent(out) :: err
      type(rpc_answer_slot_t), pointer :: slot
      logical :: found
      err = CAPNP_OK
      call answer_find(conn, other, slot, found)
      if (.not. found) return
      if (.not. associated(slot) .or. .not. slot%used) return
      if (.not. slot%redirected .or. .not. slot%has_results) return
      slot%redirected = .false.
      call send_results_elsewhere(conn, other, err)
   end subroutine release_redirected

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
      if (err /= CAPNP_OK) then
         call capnp_message_free(m)
         return
      end if
      qid = return_answer_id_get(r)
      ! Pipeline-only question ids carry the high bit. C++ ignores a
      ! Return for those rather than treating the id as unknown.
      if (iand(qid, QID_HIGH_BIT) /= 0_int64) then
         err = CAPNP_OK
         call capnp_message_free(m)
         return
      end if
      if (qid < 0_int64 .or. qid >= int(MAXQ, int64)) then
         call capnp_message_free(m)
         call abort_conn(conn, 'Invalid question ID in Return message.', err)
         return
      end if
      q = int(qid)
      if (.not. conn%questions(q)%used) then
         call capnp_message_free(m)
         call abort_conn(conn, 'Invalid question ID in Return message.', err)
         return
      end if
      ! A Return racing with a Finish for a canceled question is legal.
      ! The id was kept reserved so it could not be reused; the Return
      ! ends that reservation and is not delivered to a later call.
      if (conn%questions(q)%canceled) then
         err = CAPNP_OK
         if (return_which(r) == RETURN_TAKE_FROM_OTHER_QUESTION_TAG) then
            call release_redirected(conn, return_take_from_other_question_get(r), err)
         end if
         call capnp_message_free(m)
         if (.not. conn%dead) call question_slot_clear(conn%questions(q))
         return
      end if
      ! A second Return must not replace the result already stored.
      if (conn%questions(q)%returned) then
         call capnp_message_free(m)
         call abort_conn(conn, 'Duplicate Return.', err)
         return
      end if
      if (return_which(r) == RETURN_CANCELED_TAG) then
         call capnp_message_free(m)
         call abort_conn(conn, &
                         'Return message falsely claims call was canceled.', err)
         return
      end if
      if (return_which(r) == RETURN_RESULTS_SENT_ELSEWHERE_TAG .and. &
          .not. conn%questions(q)%tail_call) then
         call capnp_message_free(m)
         call abort_conn(conn, &
                         '`Return` had `resultsSentElsewhere` but this was ' // &
                         'not a tail call.', err)
         return
      end if
      if (conn%questions(q)%tail_call .and. &
          return_which(r) == RETURN_RESULTS_TAG) then
         call capnp_message_free(m)
         call abort_conn(conn, &
                         'Tail call `Return` must set `resultsSentElsewhere`, ' // &
                         'not `results`.', err)
         return
      end if
      if (conn%questions(q)%tail_call .and. &
          return_which(r) == RETURN_EXCEPTION_TAG) then
         call capnp_message_free(m)
         call abort_conn(conn, &
                         'Tail call `Return` must set `resultsSentElsewhere`, ' // &
                         'not `exception`.', err)
         return
      end if
      if (return_which(r) == RETURN_TAKE_FROM_OTHER_QUESTION_TAG) then
         call adopt_redirected(conn, q, return_take_from_other_question_get(r), err)
         call capnp_message_free(m)
         return
      end if
      if (return_which(r) /= RETURN_RESULTS_TAG .and. &
          return_which(r) /= RETURN_EXCEPTION_TAG .and. &
          return_which(r) /= RETURN_RESULTS_SENT_ELSEWHERE_TAG) then
         call capnp_message_free(m)
         call abort_conn(conn, "Unknown 'Return' type.", err)
         return
      end if
      if (return_which(r) == RETURN_RESULTS_TAG) then
         block
            type(payload_t) :: rpl
            integer :: ierr
            rpl = return_results_get(r, ierr)
            if (ierr == CAPNP_OK) call note_introductions(conn, rpl, ierr)
            if (ierr == CAPNP_OK) call note_payload_imports(conn, rpl, ierr)
            if (ierr /= CAPNP_OK) then
               call capnp_message_free(m)
               err = ierr
               return
            end if
         end block
      end if
      ! Own the Return bytes in the question slot.
      call capnp_serialize_bytes(m, bytes, err)
      if (err == CAPNP_OK) then
         call capnp_deserialize_bytes(bytes, conn%questions(q)%retmsg, err)
         conn%questions(q)%returned = .true.
      end if
      call capnp_message_free(m)
   end subroutine handle_return

   !> Finish of a Provide question drops the arrangement, including one
   !> whose question id never fit in the answer table.
   subroutine release_finished_provide(conn, qid, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: qid
      integer, intent(out) :: err
      integer :: i
      err = CAPNP_OK
      if (.not. associated(conn%vat)) return
      do i = 0, MAXPROV - 1
         if (.not. conn%vat%provisions(i)%used) cycle
         if (conn%vat%provisions(i)%qid /= qid) cycle
         if (.not. associated(conn%vat%provisions(i)%host, conn)) cycle
         call abandon_provision(conn%vat%provisions(i), err)
         return
      end do
   end subroutine release_finished_provide

   subroutine handle_finish(conn, msg, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(message_t), intent(in) :: msg
      integer, intent(out) :: err
      type(finish_t) :: f
      integer(int64) :: qid
      integer :: i, k, eid, jerr
      type(rpc_answer_slot_t), pointer :: jslot
      logical :: jfound
      f = message_finish_get(msg, err)
      if (err /= CAPNP_OK) return
      qid = finish_question_id_get(f)
      call release_finished_provide(conn, qid, err)
      do i = 0, MAXJOINS - 1
         if (.not. conn%joins(i)%used) cycle
         if (any(conn%joins(i)%qids(0:conn%joins(i)%part_count - 1) == qid)) then
            ! One Finish cancels the whole set. Every other part that
            ! has already arrived, including a later one, is answered
            ! with an exception. The finished part itself is not.
            do k = 0, conn%joins(i)%part_count - 1
               if (.not. conn%joins(i)%seen(k)) cycle
               if (conn%joins(i)%qids(k) == qid) cycle
               call send_answer_exception(conn, conn%joins(i)%qids(k), &
                                          'join finished', jerr)
               if (jerr /= CAPNP_OK) err = jerr
            end do
            ! The finished part is not answered. Drop the reservation
            ! so the question id can be used again.
            if (qid >= 0_int64) then
               call answer_find(conn, qid, jslot, jfound)
               if (jfound) then
                  if (jslot%used .and. .not. jslot%has_results) then
                     call answer_slot_clear(jslot)
                     if (qid >= int(MAXQ, int64)) call answer_drop_hi(conn, qid)
                  end if
               end if
            end if
            conn%joins(i) = rpc_join_slot_t()
            return
         end if
      end do
      if (qid < 0_int64) return
      call answer_find(conn, qid, jslot, jfound)
      if (.not. jfound) return
      if (.not. jslot%used) return
      if (finish_release_result_caps_get(f)) then
         do i = 0, jslot%nexp - 1
            eid = jslot%exports(i)
            if (eid >= 0 .and. eid < MAXE) call drop_export(conn, eid, 1)
         end do
      end if
      call capnp_message_free(jslot%retmsg)
      call answer_slot_clear(jslot)
      if (qid >= int(MAXQ, int64)) call answer_drop_hi(conn, qid)
   end subroutine handle_finish

   subroutine handle_release(conn, msg, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(message_t), intent(in) :: msg
      integer, intent(out) :: err
      type(release_t) :: rel
      integer(int64) :: eid, nref
      integer :: id
      rel = message_release_get(msg, err)
      if (err /= CAPNP_OK) return
      eid = release_id_get(rel)
      nref = release_reference_count_get(rel)
      ! A Release of more refs than the export holds is a protocol
      ! error. Applying it would wrap the subtraction and free the
      ! object while a provision may still name it.
      if (eid < 0_int64 .or. eid >= int(MAXE, int64) .or. nref < 0_int64) then
         call abort_conn(conn, 'Tried to release invalid export ID.', err)
         return
      end if
      id = int(eid)
      if (.not. conn%exports(id)%used) then
         call abort_conn(conn, 'Tried to release invalid export ID.', err)
         return
      end if
      if (nref > int(conn%exports(id)%refcount, int64)) then
         call abort_conn(conn, &
                         "Tried to drop export's refcount below zero.", err)
         return
      end if
      if (nref == 0_int64) return
      call drop_export(conn, id, int(nref))
   end subroutine handle_release

   subroutine drop_export(conn, eid, count)
      type(rpc_conn_t), intent(inout) :: conn
      integer, intent(in) :: eid, count
      integer :: left
      class(*), pointer :: held
      if (eid < 0 .or. eid >= MAXE) return
      if (.not. conn%exports(eid)%used) return
      if (count <= 0 .or. count > conn%exports(eid)%refcount) return
      left = conn%exports(eid)%refcount - count
      ! A dispatch still on the stack owns one reference per entry.
      if (left < conn%exports(eid)%dispatching) left = conn%exports(eid)%dispatching
      conn%exports(eid)%refcount = left
      if (conn%exports(eid)%refcount <= 0) then
         held => conn%exports(eid)%srv
         ! Membranes use their own refcount. Any other owned export is
         ! freed here so close drops a server the peer never Releases.
         select type (held)
         type is (rpc_membrane_server_t)
         class default
            if (conn%exports(eid)%owned .and. associated(held)) deallocate (held)
         end select
         conn%exports(eid)%used = .false.
         conn%exports(eid)%owned = .false.
         conn%exports(eid)%srv => null()
         conn%exports(eid)%refcount = 0
         conn%exports(eid)%dispatching = 0
         call release_server(held)
      end if
   end subroutine drop_export

   !> receiverLoopback is accepted only for an embargo id this vat issued.
   !> senderLoopback aborts. A locally hosted target does not point back
   !> at the sender, and an unknown export id is rejected on its own.
   subroutine handle_disembargo(conn, msg, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(message_t), intent(in) :: msg
      integer, intent(out) :: err
      type(disembargo_t) :: d
      type(message_target_t) :: tgt
      integer :: which
      integer(int64) :: embargo_id
      type(rpc_embargo_node_t), pointer :: emb
      d = message_disembargo_get(msg, err)
      if (err /= CAPNP_OK) return
      which = disembargo_context_which(d)
      if (which == DISEMBARGO_CONTEXT_PROVIDE_TAG) then
         call release_provide_embargo(conn, disembargo_context_provide_get(d), err)
         return
      end if
      if (which == DISEMBARGO_CONTEXT_RECEIVER_LOOPBACK_TAG) then
         embargo_id = disembargo_context_receiver_loopback_get(d)
         emb => conn%embargoes
         do while (associated(emb))
            if (emb%open .and. emb%id == embargo_id) exit
            emb => emb%next
         end do
         if (.not. associated(emb)) then
            call abort_conn(conn, &
                            "Invalid embargo ID in 'Disembargo.context.receiverLoopback'.", &
                            err)
            return
         end if
         emb%open = .false.
         err = CAPNP_OK
         return
      end if
      if (which /= DISEMBARGO_CONTEXT_SENDER_LOOPBACK_TAG) then
         call abort_conn(conn, 'Unimplemented Disembargo type.', err)
         return
      end if
      ! senderLoopback names one of our exports. C++ echoes it only when
      ! that export is an import pointing back at the sender. A locally
      ! hosted capability does not, and an id we never exported is a
      ! different error from getMessageTarget.
      tgt = disembargo_target_get(d, err)
      if (err /= CAPNP_OK) return
      if (message_target_which(tgt) == MESSAGE_TARGET_IMPORTED_CAP_TAG) then
         embargo_id = message_target_imported_cap_get(tgt)
         if (embargo_id < 0_int64 .or. embargo_id >= int(MAXE, int64)) then
            call abort_conn(conn, 'Message target is not a current export ID.', err)
            return
         end if
         if (.not. conn%exports(int(embargo_id))%used) then
            call abort_conn(conn, 'Message target is not a current export ID.', err)
            return
         end if
      end if
      call abort_conn(conn, &
                      "'Disembargo' of type 'senderLoopback' sent to an " // &
                      "object that does not point back to the sender.", err)
   end subroutine handle_disembargo

   !> Unimplemented(Resolve) releases the export the Resolve named.
   !> Any other unimplemented message aborts. Leaving a Call, Bootstrap,
   !> or Return unanswered would block rpc_wait forever.
   subroutine handle_unimplemented(conn, msg, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(message_t), intent(in) :: msg
      integer, intent(out) :: err
      type(message_t) :: inner
      type(resolve_t) :: rv
      type(cap_descriptor_t) :: cd
      type(third_party_cap_descriptor_t) :: tp
      integer(int64) :: id
      integer :: ierr
      inner = message_unimplemented_get(msg, err)
      if (err /= CAPNP_OK) return
      if (message_which(inner) == MESSAGE_RESOLVE_TAG) then
         rv = message_resolve_get(inner, ierr)
         if (ierr /= CAPNP_OK) return
         if (resolve_which(rv) /= RESOLVE_CAP_TAG) return
         cd = resolve_cap_get(rv, ierr)
         if (ierr /= CAPNP_OK) return
         id = -1_int64
         select case (cap_descriptor_which(cd))
         case (CAP_DESCRIPTOR_SENDER_HOSTED_TAG)
            id = cap_descriptor_sender_hosted_get(cd)
         case (CAP_DESCRIPTOR_SENDER_PROMISE_TAG)
            id = cap_descriptor_sender_promise_get(cd)
         case (CAP_DESCRIPTOR_RECEIVER_HOSTED_TAG, CAP_DESCRIPTOR_RECEIVER_ANSWER_TAG)
            err = CAPNP_OK
            return
         case (CAP_DESCRIPTOR_THIRD_PARTY_HOSTED_TAG)
            tp = cap_descriptor_third_party_hosted_get(cd, ierr)
            if (ierr /= CAPNP_OK) return
            id = third_party_cap_descriptor_vine_id_get(tp)
         case (CAP_DESCRIPTOR_NONE_TAG)
            err = CAPNP_OK
            return
         case default
            call abort_conn(conn, 'unknown CapDescriptor type', err)
            return
         end select
         if (id < 0_int64 .or. id >= int(MAXE, int64) .or. &
             .not. conn%exports(int(id))%used) then
            call abort_conn(conn, 'Tried to release invalid export ID.', err)
            return
         end if
         call drop_export(conn, int(id), 1)
         err = CAPNP_OK
         return
      end if
      call abort_conn(conn, &
                      'Peer did not implement required RPC message type.', err)
   end subroutine handle_unimplemented

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
      if (err == CAPNP_OK) call rpc_send_message(conn%transport, rm, err)
      call capnp_message_free(rm)
   end subroutine send_unimplemented

   subroutine fill_exception(r, reason, err)
      type(return_t), intent(in) :: r
      character(len=*), intent(in) :: reason
      integer, intent(out) :: err
      type(exception_t) :: ex
      ex = return_exception_init(r, err)
      if (err /= CAPNP_OK) return
      call exception_type_set(ex, EXCEPTION_TYPE_FAILED_E, err)
      call exception_reason_set(ex, reason, err)
   end subroutine fill_exception

end module capnp_rpc
