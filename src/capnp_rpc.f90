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
   public :: rpc_membrane_owned_live, rpc_shared_owners_live
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
   integer, parameter :: IMP_ANSWER = 7

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
      logical :: inner_owned = .false.
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
      !> References the peer may release on the wire.
      integer :: refcount = 0
      !> Internal holds such as dispatch, reflected resolution, and Provide.
      integer :: pins = 0
      class(*), pointer :: srv => null()
      !> True when dropping the last reference must deallocate srv.
      logical :: owned = .false.
      !> True when another connection's export also owns srv after a
      !> handoff. srv is deallocated when the last of them is dropped.
      logical :: shared = .false.
      !> Dispatches of this server that have been entered and not left.
      !> drop_export will not deallocate while this is positive.
      integer :: dispatching = 0
   end type rpc_export_slot_t

   !> Owners of a server that a handoff exported on two connections.
   type :: rpc_shared_owner_t
      class(*), pointer :: srv => null()
      integer :: owners = 0
      type(rpc_shared_owner_t), pointer :: next => null()
   end type rpc_shared_owner_t

   type(rpc_shared_owner_t), pointer :: g_shared_owners => null()

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
      !> Application import handle used to form an outgoing Call.
      integer(int64) :: import_handle = -1_int64
      !> Import handles bound to retmsg's cap table when Return arrived.
      integer :: nresult_handles = 0
      !> Both are indexed from 0 and sized to the Return's capTable.
      integer(int64), allocatable :: result_handles(:)
      !> Result caps whose reference an introduction pickup has released.
      logical, allocatable :: result_released(:)
      type(capnp_message_t) :: retmsg ! the Return message, kept until finish
      class(rpc_membrane_policy_t), pointer :: membrane => null()
      logical :: membrane_reverse = .false.
   end type rpc_question_slot_t

   type :: rpc_answer_slot_t
      logical :: used = .false.
      logical :: has_results = .false.
      !> Finish arrived while dispatch was still producing this answer.
      logical :: finished = .false.
      !> Set only for a sendResultsTo.yourself call whose results are
      !> held for a later takeFromOtherQuestion.
      logical :: redirected = .false.
      type(capnp_message_t) :: retmsg ! copy of the Return we sent
      integer :: nexp = 0
      integer :: exports(0:MAXCAPS - 1) = -1
      !> Set once the answer has pinned each result export. Finish
      !> drops those pins. The wire refcount is a separate count.
      logical :: exports_pinned = .false.
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
      !> The peer's wire id. `id` may be a private local alias after the
      !> peer reuses an id that still names a reflected local capability.
      integer(int64) :: wire_id = -1_int64
      integer :: state = IMP_ABSENT
      logical :: received_call = .false.
      integer(int64) :: resolved_id = -1_int64
      integer(int64) :: answer_qid = -1_int64
      integer, allocatable :: answer_ops(:)
      integer(int64) :: embargo_id = -1_int64
      !> Remote references this vat still owes a Release for.
      integer :: refs = 0
      !> Local handles remaining on a resolved promise node.
      integer :: local_refs = 0
      logical :: released = .false.
      !> receiverHosted resolution kept one extra ref on the export.
      logical :: pinned = .false.
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

   type :: rpc_answer_wait_t
      integer(int64) :: promise_id = -1_int64
      integer(int64) :: call_qid = -1_int64
      type(capnp_message_t) :: msg
      type(rpc_answer_wait_t), pointer :: next => null()
   end type rpc_answer_wait_t

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
      integer(int64) :: vine_handle = -1_int64
      logical :: has_handle = .false.
      !> The vine of a Return result is that result's reference. `qid` and
      !> `capidx` name the result until Finish settles who owns it.
      integer(int64) :: qid = -1_int64
      integer :: capidx = -1
      !> False once Finish(releaseResultCaps) has released the vine.
      logical :: holds_vine = .true.
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
      type(rpc_answer_wait_t), pointer :: answer_wait => null()
      integer(int64) :: next_embargo = 0_int64
      integer(int64) :: next_import_handle = -2_int64
      !> Shared with this vat's other connections when one is attached;
      !> otherwise `own_vat` below.
      type(rpc_vat_t), pointer :: vat => null()
      type(rpc_vat_t) :: own_vat
      type(rpc_introduction_t), allocatable :: introductions(:)
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
      if (allocated(conn%introductions)) deallocate (conn%introductions)
      allocate (conn%introductions(0:MAXINTRO - 1))
      conn%introductions = rpc_introduction_t()
      conn%joins = rpc_join_slot_t()
      conn%next_qid = 0_int64
      conn%next_embargo = 0_int64
      conn%next_import_handle = -2_int64
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
      if (allocated(conn%introductions)) deallocate (conn%introductions)
      allocate (conn%introductions(0:MAXINTRO - 1))
      conn%introductions = rpc_introduction_t()
      conn%joins = rpc_join_slot_t()
      conn%next_qid = 0_int64
      conn%next_embargo = 0_int64
      conn%next_import_handle = -2_int64
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
      ! Drop answer-pipeline pins before the wire refcounts. A peer that
      ! already Released an export leaves only that pin, and the server
      ! is freed here. A remaining wire refcount is dropped below.
      call unpin_all_answers(conn)
      ! The peer will not Release what it still imports. Internal pins keep
      ! a dispatch, reflected capability, or provision alive independently
      ! of the wire reference count.
      do i = 0, MAXE - 1
         if (.not. conn%exports(i)%used) cycle
         call drop_export(conn, i, conn%exports(i)%refcount)
      end do
      do i = 0, MAXQ - 1
         if (conn%questions(i)%used) call capnp_message_free(conn%questions(i)%retmsg)
         if (conn%answers(i)%used) call capnp_message_free(conn%answers(i)%retmsg)
         call question_slot_clear(conn%questions(i))
         call answer_slot_clear(conn%answers(i))
      end do
      call free_hi_answers(conn)
      call free_imports(conn)
      if (associated(conn%vat)) call release_hosted_provisions(conn)
      call free_pipe_waits(conn)
      call free_answer_waits(conn)
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
      logical :: direction
      wrapped = cap
      direction = .false.
      if (present(reverse)) direction = reverse
      if (associated(cap%membrane, policy)) then
         if (cap%membrane_reverse .neqv. direction) then
            wrapped%membrane => null()
            wrapped%membrane_reverse = .false.
         end if
         return
      end if
      wrapped%membrane => policy
      wrapped%membrane_reverse = direction
   end function rpc_membrane_cap

   !> Initialize a server wrapper. `inner` and `policy` must outlive the
   !> wrapper and every result capability exported through it.
   subroutine rpc_membrane_server_init(wrapper, inner, policy, reverse, inner_owned)
      type(rpc_membrane_server_t), intent(out) :: wrapper
      class(rpc_server_t), pointer, intent(in) :: inner
      class(rpc_membrane_policy_t), pointer, intent(in) :: policy
      logical, intent(in), optional :: reverse
      logical, intent(in), optional :: inner_owned
      wrapper%inner => inner
      wrapper%policy => policy
      wrapper%reverse = .false.
      wrapper%owned = .false.
      wrapper%inner_owned = .false.
      if (present(reverse)) wrapper%reverse = reverse
      if (present(inner_owned)) wrapper%inner_owned = inner_owned
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
      type :: staged_ptr_t
         class(*), pointer :: p => null()
      end type staged_ptr_t
      type(staged_ptr_t) :: orig(0:MAXCAPS - 1)
      integer :: i, j
      logical :: dup
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
         orig(i)%p => ctx%staged(i)%s
      end do
      do i = 0, ctx%nexp - 1
         dup = .false.
         do j = 0, i - 1
            if (associated(orig(j)%p, orig(i)%p)) then
               ctx%staged(i)%s => ctx%staged(j)%s
               if (ctx%staged(i)%owned) then
                  ctx%staged(j)%owned = .true.
                  select type (w => ctx%staged(j)%s)
                  type is (rpc_membrane_server_t)
                     if (w%owned) w%inner_owned = .true.
                  end select
               end if
               dup = .true.
               exit
            end if
         end do
         if (dup) cycle
         call membrane_wrap_server_ref(ctx%staged(i)%s, self%policy, &
                                       self%reverse, err, ctx%staged(i)%owned)
         if (err /= CAPNP_OK) then
            call discard_unexported_wrappers(ctx)
            return
         end if
      end do
   end subroutine membrane_dispatch

   !> Wrap a result capability crossing the membrane. A wrapper crossing
   !> back through the same policy is unwrapped instead of doubled.
   subroutine membrane_wrap_server_ref(srv, policy, reverse, err, inner_owned)
      class(*), pointer, intent(inout) :: srv
      class(rpc_membrane_policy_t), pointer, intent(in) :: policy
      logical, intent(in) :: reverse
      integer, intent(out) :: err
      logical, intent(in), optional :: inner_owned
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
            if (existing%owned .and. existing%refs <= 0) then
               existing%inner_owned = .false.
               deallocate (existing)
            end if
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
      call rpc_membrane_server_init(wrapper, inner, policy, reverse, inner_owned)
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

   !> Free an owned membrane wrapper and any owned inner server it holds.
   recursive subroutine free_owned_membrane(w)
      type(rpc_membrane_server_t), pointer, intent(inout) :: w
      class(rpc_server_t), pointer :: inner
      if (.not. associated(w)) return
      if (w%inner_owned .and. associated(w%inner)) then
         inner => w%inner
         w%inner => null()
         select type (inner)
         type is (rpc_membrane_server_t)
            if (inner%owned) then
               inner%refs = inner%refs - 1
               if (inner%refs <= 0) call free_owned_membrane(inner)
            else
               deallocate (inner)
            end if
         class default
            deallocate (inner)
         end select
      else
         w%inner => null()
      end if
      deallocate (w)
      w => null()
   end subroutine free_owned_membrane

   !> Drop one holder. The last holder frees the wrapper.
   subroutine release_server(srv)
      class(*), pointer, intent(inout) :: srv
      type(rpc_membrane_server_t), pointer :: w
      if (.not. associated(srv)) return
      select type (srv)
      type is (rpc_membrane_server_t)
         if (srv%owned) then
            srv%refs = srv%refs - 1
            if (srv%refs <= 0) then
               w => srv
               call free_owned_membrane(w)
            end if
         end if
      end select
      srv => null()
   end subroutine release_server

   !> Free an owned membrane wrapper that never reached an export slot.
   !> An owned inner server is freed with it; caller-owned inners stay.
   subroutine discard_unexported_wrapper(srv)
      class(*), pointer, intent(inout) :: srv
      type(rpc_membrane_server_t), pointer :: w
      if (.not. associated(srv)) return
      select type (srv)
      type is (rpc_membrane_server_t)
         if (srv%owned .and. srv%refs <= 0) then
            w => srv
            call free_owned_membrane(w)
            srv => null()
         end if
      end select
   end subroutine discard_unexported_wrapper

   subroutine discard_unexported_wrappers(ctx)
      type(rpc_call_ctx_t), intent(inout) :: ctx
      integer :: i, j
      class(*), pointer :: p
      do i = 0, ctx%nexp - 1
         if (ctx%staged_eids(i) >= 0) cycle
         if (.not. associated(ctx%staged(i)%s)) cycle
         p => ctx%staged(i)%s
         do j = i + 1, ctx%nexp - 1
            if (ctx%staged_eids(j) < 0 .and. associated(ctx%staged(j)%s, p)) then
               ctx%staged(j)%s => null()
               ctx%staged(j)%owned = .false.
            end if
         end do
         call discard_unexported_wrapper(ctx%staged(i)%s)
      end do
   end subroutine discard_unexported_wrappers

   !> Drop exports this call already published, free owned servers that
   !> never reached the export table, and discard unexported wrappers.
   subroutine rollback_staged(conn, ctx)
      type(rpc_conn_t), intent(inout) :: conn
      type(rpc_call_ctx_t), intent(inout) :: ctx
      integer :: i, j, eid
      class(*), pointer :: p
      logical :: was_owned, freed_owned
      do i = 0, ctx%nexp - 1
         eid = ctx%staged_eids(i)
         if (eid >= 0 .and. eid < MAXE) then
            p => conn%exports(eid)%srv
            was_owned = conn%exports(eid)%owned
            if (.not. was_owned .and. associated(p)) then
               select type (p)
               type is (rpc_membrane_server_t)
                  was_owned = p%owned
               end select
            end if
            call drop_export(conn, eid, 1)
            freed_owned = was_owned .and. .not. conn%exports(eid)%used
            if (freed_owned .and. associated(p)) then
               do j = 0, ctx%nexp - 1
                  if (associated(ctx%staged(j)%s, p)) then
                     ctx%staged(j)%s => null()
                     ctx%staged(j)%owned = .false.
                  end if
               end do
            end if
            ctx%staged_eids(i) = -1
         end if
      end do
      call release_unflushed(conn, ctx)
      call discard_unexported_wrappers(ctx)
      ctx%nexp = 0
   end subroutine rollback_staged

   !> Drop the internal provision pin without changing wire references.
   subroutine release_provision_pin(slot)
      type(rpc_provision_slot_t), intent(inout) :: slot
      if (associated(slot%host) .and. slot%source_eid >= 0 .and. &
          slot%source_eid < MAXE) then
         call drop_export_pin(slot%host, slot%source_eid)
      end if
      slot%srv => null()
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
            conn%questions(k)%import_handle = -1_int64
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
      integer(int64) :: target_id, wire_id
      logical :: broken
      character(len=:), allocatable :: breason
      err = CAPNP_OK
      call import_follow(conn, imported_cap, target_id, broken, breason)
      if (broken) then
         if (allocated(breason)) conn%abort_reason = breason
         err = RPC_ERR_EXCEPTION
         qid = -1_int64
         return
      end if
      call import_wire_id(conn, target_id, wire_id)
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
      if (err == CAPNP_OK) call message_target_imported_cap_set(tgt, wire_id, err)
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
      integer(int64) :: target_id, wire_id
      logical :: broken
      character(len=:), allocatable :: breason
      err = CAPNP_OK
      target_id = target%id
      if (target%kind == RPC_CAP_IMPORT) then
         call import_follow(conn, target%id, target_id, broken, breason)
         if (broken) then
            if (allocated(breason)) conn%abort_reason = breason
            err = RPC_ERR_EXCEPTION
            qid = -1_int64
            return
         end if
      end if
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
      if (target%kind == RPC_CAP_IMPORT) then
         conn%questions(int(qid))%import_handle = target_id
         call import_wire_id(conn, target_id, wire_id)
         target_id = wire_id
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
            call message_target_imported_cap_set(tgt, target_id, err)
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
      integer(int64) :: qid, target_id
      type(message_t) :: msg
      type(call_t) :: c
      type(message_target_t) :: tgt
      type(rpc_import_node_t), pointer :: node
      logical :: found
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
      ! An embargoed or locally resolved promise is not called on the wire.
      ! C++ queues those calls on the local capability until the embargo lifts.
      msg = message_read_root(m, rerr)
      if (rerr == CAPNP_OK .and. message_which(msg) == MESSAGE_CALL_TAG) then
         c = message_call_get(msg, rerr)
         if (rerr == CAPNP_OK) tgt = call_target_get(c, rerr)
         if (rerr == CAPNP_OK) then
            if (message_target_which(tgt) == MESSAGE_TARGET_IMPORTED_CAP_TAG) then
               target_id = message_target_imported_cap_get(tgt)
               if (qid >= 0_int64 .and. qid < int(MAXQ, int64)) then
                  if (conn%questions(int(qid))%import_handle /= -1_int64) &
                     target_id = conn%questions(int(qid))%import_handle
               end if
               call import_find(conn, target_id, node, found)
               if (found) then
                  if (node%state == IMP_ANSWER) then
                     call enqueue_answer_pipeline(conn, m, qid, node%id, err)
                     call capnp_message_free(m)
                     return
                  else if (node%state == IMP_EMBARGO .or. node%state == IMP_LOCAL) then
                     call deliver_local_promise(conn, node, m, qid, err)
                     call capnp_message_free(m)
                     return
                  end if
               end if
            end if
         end if
      end if
      call note_sent_promise(conn, m, qid)
      call rpc_send_message(conn%transport, m, err)
      if (err /= CAPNP_OK) then
         call release_unsent_call_caps(conn, m)
         if (qid >= 0_int64) call release_question(conn, qid)
      end if
      call capnp_message_free(m)
   end subroutine rpc_call_send

   !> Hold a call made on an embargoed promise, or run it on the local
   !> capability once the embargo has lifted.
   subroutine deliver_local_promise(conn, node, m, qid, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(rpc_import_node_t), intent(in) :: node
      type(capnp_message_t), intent(inout), target :: m
      integer(int64), intent(in) :: qid
      integer, intent(out) :: err
      if (node%state == IMP_EMBARGO) then
         call enqueue_embargo_call(conn, node, m, qid, err)
      else
         call fulfill_local_call(conn, int(node%resolved_id), m, qid, err)
      end if
   end subroutine deliver_local_promise

   subroutine enqueue_embargo_call(conn, node, m, qid, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(rpc_import_node_t), intent(in) :: node
      type(capnp_message_t), intent(inout) :: m
      integer(int64), intent(in) :: qid
      integer, intent(out) :: err
      type(rpc_embargo_node_t), pointer :: emb
      type(rpc_queued_call_t), pointer :: q, tail
      integer(int8), allocatable :: bytes(:)
      emb => conn%embargoes
      do while (associated(emb))
         if (emb%open .and. emb%promise_id == node%id) exit
         emb => emb%next
      end do
      if (.not. associated(emb)) then
         call fulfill_local_call(conn, int(node%resolved_id), m, qid, err)
         return
      end if
      allocate (q)
      q%qid = qid
      q%next => null()
      call capnp_serialize_bytes(m, bytes, err)
      if (err == CAPNP_OK) call capnp_deserialize_bytes(bytes, q%msg, err)
      if (err /= CAPNP_OK) then
         deallocate (q)
         return
      end if
      if (.not. associated(emb%queued)) then
         emb%queued => q
      else
         tail => emb%queued
         do while (associated(tail%next))
            tail => tail%next
         end do
         tail%next => q
      end if
      if (qid >= 0_int64 .and. qid < int(MAXQ, int64)) &
         conn%questions(int(qid))%local_call = .true.
      err = CAPNP_OK
   end subroutine enqueue_embargo_call

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
      if (export_id < 0 .or. export_id >= MAXE) then
         call fill_exception(r, 'no such capability', err)
      else if (.not. conn%exports(export_id)%used) then
         call fill_exception(r, 'no such capability', err)
      else
         ctx%interface_id = call_interface_id_get(c)
         ctx%method_id = int(call_method_id_get(c))
         ctx%params = payload_content_get(params, err)
         ctx%rmsg => rm
         ctx%results = return_results_init(r, err)
         dispatched = .false.
         derr = CAPNP_ERR_KIND
         ! Match wire dispatch: keep the export alive while application code
         ! pumps the connection and may receive Release or close it.
         call pin_export(conn, export_id)
         conn%exports(export_id)%dispatching = conn%exports(export_id)%dispatching + 1
         select type (s => conn%exports(export_id)%srv)
         class is (rpc_server_t)
            call s%dispatch(ctx, derr)
            dispatched = .true.
         end select
         conn%exports(export_id)%dispatching = conn%exports(export_id)%dispatching - 1
         call drop_export_pin(conn, export_id)
         if (conn%dead .or. .not. associated(conn%transport)) then
            call rollback_staged(conn, ctx)
            call capnp_message_free(rm)
            err = RPC_ERR_DEAD
            return
         end if
         if (.not. dispatched .or. derr /= CAPNP_OK) then
            call rollback_staged(conn, ctx)
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
      type(rpc_pipe_wait_t), pointer :: node, prev, nxt, waiting, tail
      err = CAPNP_OK
      ! Detach all eligible calls before entering any one of them. A
      ! nested local call can itself drain `pipe_wait`, so retaining a
      ! predecessor in the connection list across dispatch is unsafe.
      waiting => null()
      tail => null()
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
            node%next => null()
            if (associated(tail)) then
               tail%next => node
            else
               waiting => node
            end if
            tail => node
         else
            prev => node
         end if
         node => nxt
      end do
      node => waiting
      do while (associated(node))
         nxt => node%next
         if (err == CAPNP_OK) &
            call dispatch_local_pipeline(conn, node%msg, node%call_qid, &
                                         target_qid, err)
         call capnp_message_free(node%msg)
         deallocate (node)
         node => nxt
      end do
   end subroutine drain_pipe_waits

   !> A canceled local question will not produce a result. Waiting
   !> pipelines fail with that fact instead of sitting on the list.
   recursive subroutine fail_pipe_waits(conn, target_qid, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: target_qid
      integer, intent(out) :: err
      type(rpc_pipe_wait_t), pointer :: node, prev, nxt, waiting, tail
      err = CAPNP_OK
      waiting => null()
      tail => null()
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
            node%next => null()
            if (associated(tail)) then
               tail%next => node
            else
               waiting => node
            end if
            tail => node
         else
            prev => node
         end if
         node => nxt
      end do
      node => waiting
      do while (associated(node))
         nxt => node%next
         if (err == CAPNP_OK) &
            call reject_local_question(conn, node%call_qid, 'call canceled', err)
         call capnp_message_free(node%msg)
         deallocate (node)
         node => nxt
      end do
   end subroutine fail_pipe_waits

   recursive subroutine reject_local_question(conn, qid, reason, err)
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
      if (err == CAPNP_OK .and. conn%questions(int(qid))%returned) &
         call drain_pipe_waits(conn, qid, err)
   end subroutine reject_local_question

   subroutine enqueue_pipeline(conn, m, call_qid, target_qid, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(capnp_message_t), intent(inout) :: m
      integer(int64), intent(in) :: call_qid, target_qid
      integer, intent(out) :: err
      type(rpc_pipe_wait_t), pointer :: node, tail
      integer(int8), allocatable :: bytes(:)
      allocate (node)
      node%call_qid = call_qid
      node%target_qid = target_qid
      node%next => null()
      call capnp_serialize_bytes(m, bytes, err)
      if (err == CAPNP_OK) call capnp_deserialize_bytes(bytes, node%msg, err)
      if (err /= CAPNP_OK) then
         deallocate (node)
         return
      end if
      if (.not. associated(conn%pipe_wait)) then
         conn%pipe_wait => node
      else
         tail => conn%pipe_wait
         do while (associated(tail%next))
            tail => tail%next
         end do
         tail%next => node
      end if
      if (call_qid >= 0_int64 .and. call_qid < int(MAXQ, int64)) &
         conn%questions(int(call_qid))%local_call = .true.
      err = CAPNP_OK
   end subroutine enqueue_pipeline

   subroutine enqueue_answer_pipeline(conn, m, call_qid, promise_id, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(capnp_message_t), intent(inout) :: m
      integer(int64), intent(in) :: call_qid, promise_id
      integer, intent(out) :: err
      type(rpc_answer_wait_t), pointer :: node, tail
      integer(int8), allocatable :: bytes(:)

      allocate (node)
      node%call_qid = call_qid
      node%promise_id = promise_id
      node%next => null()
      call capnp_serialize_bytes(m, bytes, err)
      if (err == CAPNP_OK) call capnp_deserialize_bytes(bytes, node%msg, err)
      if (err /= CAPNP_OK) then
         deallocate (node)
         return
      end if
      if (.not. associated(conn%answer_wait)) then
         conn%answer_wait => node
      else
         tail => conn%answer_wait
         do while (associated(tail%next))
            tail => tail%next
         end do
         tail%next => node
      end if
      if (call_qid >= 0_int64 .and. call_qid < int(MAXQ, int64)) &
         conn%questions(int(call_qid))%local_call = .true.
      err = CAPNP_OK
   end subroutine enqueue_answer_pipeline

   recursive subroutine drain_answer_pipeline(conn, promise_id, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: promise_id
      integer, intent(out) :: err
      type(rpc_answer_wait_t), pointer :: node, prev, nxt, waiting, tail

      err = CAPNP_OK
      waiting => null()
      tail => null()
      prev => null()
      node => conn%answer_wait
      do while (associated(node))
         nxt => node%next
         if (node%promise_id == promise_id) then
            if (associated(prev)) then
               prev%next => nxt
            else
               conn%answer_wait => nxt
            end if
            node%next => null()
            if (associated(tail)) then
               tail%next => node
            else
               waiting => node
            end if
            tail => node
         else
            prev => node
         end if
         node => nxt
      end do
      node => waiting
      do while (associated(node))
         nxt => node%next
         if (err == CAPNP_OK) call rpc_call_send(conn, node%msg, err)
         if (err /= CAPNP_OK) call capnp_message_free(node%msg)
         deallocate (node)
         node => nxt
      end do
   end subroutine drain_answer_pipeline

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
         call drain_pipe_waits(conn, call_qid, err)
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

   subroutine free_answer_waits(conn)
      type(rpc_conn_t), intent(inout) :: conn
      type(rpc_answer_wait_t), pointer :: node, nxt
      node => conn%answer_wait
      do while (associated(node))
         nxt => node%next
         call capnp_message_free(node%msg)
         deallocate (node)
         node => nxt
      end do
      conn%answer_wait => null()
   end subroutine free_answer_waits

   subroutine drain_embargo_queue(conn, emb, export_id, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(rpc_embargo_node_t), intent(inout) :: emb
      integer, intent(in) :: export_id
      integer, intent(out) :: err
      type(rpc_queued_call_t), pointer :: q, nxt
      err = CAPNP_OK
      q => emb%queued
      emb%queued => null()
      do while (associated(q))
         nxt => q%next
         call fulfill_local_call(conn, export_id, q%msg, q%qid, err)
         call capnp_message_free(q%msg)
         deallocate (q)
         q => nxt
         if (err /= CAPNP_OK) exit
      end do
      do while (associated(q))
         nxt => q%next
         call capnp_message_free(q%msg)
         deallocate (q)
         q => nxt
      end do
   end subroutine drain_embargo_queue

   !> Forget a pipelined call that is still waiting on its target
   !> question, so a canceled call is not dispatched later.
   subroutine drop_pipe_wait(conn, qid)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: qid
      type(rpc_pipe_wait_t), pointer :: node, prev, nxt
      prev => null()
      node => conn%pipe_wait
      do while (associated(node))
         nxt => node%next
         if (node%call_qid == qid) then
            if (associated(prev)) then
               prev%next => nxt
            else
               conn%pipe_wait => nxt
            end if
            call capnp_message_free(node%msg)
            deallocate (node)
         else
            prev => node
         end if
         node => nxt
      end do
   end subroutine drop_pipe_wait

   subroutine drop_queued_call(conn, qid)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: qid
      type(rpc_embargo_node_t), pointer :: emb
      type(rpc_queued_call_t), pointer :: q, prev, nxt
      emb => conn%embargoes
      do while (associated(emb))
         prev => null()
         q => emb%queued
         do while (associated(q))
            if (q%qid == qid) then
               nxt => q%next
               if (associated(prev)) then
                  prev%next => nxt
               else
                  emb%queued => nxt
               end if
               call capnp_message_free(q%msg)
               deallocate (q)
               return
            end if
            prev => q
            q => q%next
         end do
         emb => emb%next
      end do
   end subroutine drop_queued_call

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
               block
                  integer(int64) :: target_id, wire_id
                  logical :: broken
                  character(len=:), allocatable :: breason
                  call import_follow(conn, caps(i)%id, target_id, broken, breason)
                  if (broken) then
                     if (allocated(breason)) conn%abort_reason = breason
                     err = RPC_ERR_EXCEPTION
                  else
                     call import_wire_id(conn, target_id, wire_id)
                     call message_target_imported_cap_set(tgt, wire_id, err)
                  end if
               end block
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
      type(rpc_import_node_t), pointer :: inode
      integer :: i
      logical :: found
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
            if (result_handle(conn%questions(int(qid)), int(p%capidx)) /= -1_int64) then
               cap%id = result_handle(conn%questions(int(qid)), int(p%capidx))
            else
               call import_find_wire(conn, cap%id, inode, found)
               if (found) cap%id = inode%id
            end if
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
            if (result_handle(conn%questions(int(qid)), int(p%capidx)) /= -1_int64) then
               cap%id = result_handle(conn%questions(int(qid)), int(p%capidx))
            else
               call import_find_wire(conn, cap%id, inode, found)
               if (found) cap%id = inode%id
            end if
         end if
      case (CAP_DESCRIPTOR_THIRD_PARTY_HOSTED_TAG)
         ! C++ receiveCap imports the vine and returns that client
         ! (rpc.c++ 1628-1630). A local result never carries this
         ! descriptor.
         if (conn%questions(int(qid))%local_call .or. &
             conn%questions(int(qid))%local_results) then
            err = CAPNP_ERR_KIND
         else
            block
               type(third_party_cap_descriptor_t) :: tp
               integer(int64) :: vine
               tp = cap_descriptor_third_party_hosted_get(cd, err)
               if (err == CAPNP_OK) then
                  vine = third_party_cap_descriptor_vine_id_get(tp)
                  cap%kind = RPC_CAP_IMPORT
                  cap%id = vine
                  if (result_handle(conn%questions(int(qid)), int(p%capidx)) /= -1_int64) then
                     cap%id = result_handle(conn%questions(int(qid)), int(p%capidx))
                  else
                     call import_find_live(conn, vine, inode, found)
                     if (.not. found) call import_find_wire(conn, vine, inode, found)
                     if (found) cap%id = inode%id
                  end if
               end if
            end block
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
   !> capabilities we imported from the results alive. After Resolve has
   !> already Released a result promise, releaseResultCaps stays false
   !> and the question Releases the capability the promise resolved to.
   subroutine rpc_finish_send(conn, qid, retain_caps, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: qid
      logical, intent(in) :: retain_caps
      integer, intent(out) :: err
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(finish_t) :: f
      integer :: q, relerr
      logical :: release_flag, explicit_release, ignored
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
      explicit_release = .false.
      if (.not. retain_caps .and. conn%questions(q)%returned .and. &
          .not. conn%questions(q)%local_call .and. &
          .not. conn%questions(q)%local_results) then
         call scan_result_imports(conn, q, explicit_release, .false., err)
         if (err /= CAPNP_OK) return
         if (allocated(conn%questions(q)%result_released)) then
            if (any(conn%questions(q)%result_released)) explicit_release = .true.
         end if
      end if
      release_flag = .not. retain_caps
      if (explicit_release) release_flag = .false.
      ! C++ sets releaseResultCaps only while the Return is outstanding
      ! (rpc.c++ ~QuestionRef). A Return with no capabilities to release
      ! needs none.
      if (conn%questions(q)%returned .and. .not. result_has_caps(conn, q)) release_flag = .false.
      if (.not. conn%questions(q)%local_call) then
         call capnp_message_init_builder(m, err)
         if (err /= CAPNP_OK) return
         msg = message_new_root(m, err)
         f = message_finish_init(msg, err)
         call finish_question_id_set(f, qid, err)
         call finish_release_result_caps_set(f, release_flag, err)
         call finish_require_early_cancellation_workaround_set(f, .false., err)
         if (err == CAPNP_OK) call rpc_send_message(conn%transport, m, err)
         call capnp_message_free(m)
      end if
      if (.not. retain_caps) then
         if (explicit_release) then
            call scan_result_imports(conn, q, ignored, .true., relerr)
            if (err == CAPNP_OK) err = relerr
         else
            call drop_finished_caps(conn, q)
         end if
      end if
      call settle_result_introductions(conn, qid, retain_caps)
      if (conn%questions(q)%used) then
         call capnp_message_free(conn%questions(q)%retmsg)
         call question_slot_clear(conn%questions(q))
      end if
   end subroutine rpc_finish_send

   !> True when the Return of question `q` is a result with at least one
   !> capability descriptor.
   logical function result_has_caps(conn, q) result(has)
      type(rpc_conn_t), intent(inout) :: conn
      integer, intent(in) :: q
      type(message_t) :: msg
      type(return_t) :: r
      type(payload_t) :: pl
      type(capnp_ptr_t) :: ctab
      integer :: ierr
      has = .false.
      if (conn%questions(q)%retmsg%nsegs < 1) return
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
      has = capnp_list_len(ctab) > 0
   end function result_has_caps

   !> After Finish a retained result vine belongs to the application
   !> until the pickup. A released one is gone, so the pickup must not
   !> release it again.
   subroutine settle_result_introductions(conn, qid, retain_caps)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: qid
      logical, intent(in) :: retain_caps
      integer :: i
      do i = lbound(conn%introductions, 1), ubound(conn%introductions, 1)
         if (.not. conn%introductions(i)%used) cycle
         if (conn%introductions(i)%qid /= qid) cycle
         conn%introductions(i)%qid = -1_int64
         conn%introductions(i)%capidx = -1
         if (.not. retain_caps) then
            conn%introductions(i)%holds_vine = .false.
            conn%introductions(i)%has_handle = .false.
         end if
      end do
   end subroutine settle_result_introductions

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
         case (CAP_DESCRIPTOR_THIRD_PARTY_HOSTED_TAG)
            block
               type(third_party_cap_descriptor_t) :: tp
               integer :: verr
               tp = cap_descriptor_third_party_hosted_get(cd, verr)
               if (verr == CAPNP_OK .and. tp%p%kind == CAPNP_PK_STRUCT) &
                  id = third_party_cap_descriptor_vine_id_get(tp)
            end block
         end select
         if (id < 0_int64) cycle
         if (local) then
            if (id >= 0_int64 .and. id < int(MAXE, int64)) &
               call drop_export(conn, int(id), 1)
         else if (result_was_released(conn%questions(q), i)) then
            cycle
         else
            if (result_handle(conn%questions(q), i) /= -1_int64) then
               call forget_import_handle(conn, result_handle(conn%questions(q), i))
            else
               call forget_import_ref(conn, id)
            end if
         end if
      end do
   end subroutine drop_finished_caps

   subroutine forget_import_handle(conn, handle)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: handle
      type(rpc_import_node_t), pointer :: node
      logical :: found
      call import_find(conn, handle, node, found)
      if (.not. found) return
      if (node%refs <= 0 .or. node%released) return
      node%refs = node%refs - 1
      if (node%refs == 0) node%released = .true.
   end subroutine forget_import_handle

   subroutine forget_import_ref(conn, id)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: id
      type(rpc_import_node_t), pointer :: node
      logical :: found
      call import_find_live(conn, id, node, found)
      if (.not. found) return
      node%refs = node%refs - 1
      if (node%refs == 0) node%released = .true.
   end subroutine forget_import_ref

   pure logical function result_was_released(slot, i)
      type(rpc_question_slot_t), intent(in) :: slot
      integer, intent(in) :: i
      result_was_released = .false.
      if (.not. allocated(slot%result_released)) return
      if (i < 0 .or. i >= size(slot%result_released)) return
      result_was_released = slot%result_released(i)
   end function result_was_released

   !> Import handle bound to result cap `i`, or -1 when none was bound.
   pure function result_handle(slot, i) result(h)
      type(rpc_question_slot_t), intent(in) :: slot
      integer, intent(in) :: i
      integer(int64) :: h
      h = -1_int64
      if (.not. allocated(slot%result_handles)) return
      if (i < 0 .or. i >= min(slot%nresult_handles, size(slot%result_handles))) return
      h = slot%result_handles(i)
   end function result_handle

   !> Walk result imports recorded for question `q`. `any_released` is
   !> true when one of them has already sent Release. With `do_release`,
   !> each import is released through `rpc_release_send`, which follows
   !> a resolved promise onto its replacement.
   subroutine scan_result_imports(conn, q, any_released, do_release, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer, intent(in) :: q
      logical, intent(out) :: any_released
      logical, intent(in) :: do_release
      integer, intent(out) :: err
      type(message_t) :: msg
      type(return_t) :: r
      type(payload_t) :: pl
      type(capnp_ptr_t) :: ctab
      type(cap_descriptor_t) :: cd
      type(rpc_import_node_t), pointer :: node
      type(rpc_cap_t) :: cap
      integer :: i, n, ierr
      integer(int64) :: id
      logical :: found
      any_released = .false.
      err = CAPNP_OK
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
         case (CAP_DESCRIPTOR_THIRD_PARTY_HOSTED_TAG)
            block
               type(third_party_cap_descriptor_t) :: tp
               integer :: verr
               tp = cap_descriptor_third_party_hosted_get(cd, verr)
               if (verr == CAPNP_OK .and. tp%p%kind == CAPNP_PK_STRUCT) &
                  id = third_party_cap_descriptor_vine_id_get(tp)
            end block
         end select
         if (id < 0_int64) cycle
         if (result_was_released(conn%questions(q), i)) cycle
         found = .false.
         node => null()
         if (result_handle(conn%questions(q), i) /= -1_int64) then
            call import_find(conn, result_handle(conn%questions(q), i), node, found)
         else
            call import_find_live(conn, id, node, found)
            if (.not. found) call import_find_wire(conn, id, node, found)
         end if
         if (.not. found) cycle
         if (node%released) any_released = .true.
         if (do_release) then
            cap%kind = RPC_CAP_IMPORT
            cap%id = node%id
            call rpc_release_send(conn, cap, err)
            if (err /= CAPNP_OK) return
         end if
      end do
   end subroutine scan_result_imports

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
      ! Once Return has arrived this is an ordinary releasing Finish, which
      ! knows about result references a pickup has already released.
      if (conn%questions(q)%returned .and. .not. conn%questions(q)%local_call) then
         call rpc_finish_send(conn, qid, .false., err)
         return
      end if
      if (conn%questions(q)%local_call) then
         call drop_queued_call(conn, qid)
         call drop_pipe_wait(conn, qid)
      else
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
         call drop_finished_caps(conn, q)
         call capnp_message_free(conn%questions(q)%retmsg)
         call question_slot_clear(conn%questions(q))
      else if (conn%questions(q)%local_call) then
         call fail_pipe_waits(conn, qid, err)
         if (err /= CAPNP_OK) return
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
      slot%import_handle = -1_int64
      slot%nresult_handles = 0
      if (allocated(slot%result_handles)) deallocate (slot%result_handles)
      if (allocated(slot%result_released)) deallocate (slot%result_released)
      slot%membrane => null()
      slot%membrane_reverse = .false.
   end subroutine question_slot_clear

   !> Clear an answer slot; see question_slot_clear.
   subroutine answer_slot_clear(slot)
      type(rpc_answer_slot_t), intent(inout) :: slot
      slot%used = .false.
      slot%has_results = .false.
      slot%finished = .false.
      slot%redirected = .false.
      slot%nexp = 0
      slot%exports = -1
      slot%exports_pinned = .false.
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

   subroutine free_imports(conn)
      type(rpc_conn_t), intent(inout) :: conn
      type(rpc_import_node_t), pointer :: node, nxt
      type(rpc_embargo_node_t), pointer :: emb, emb_next
      type(rpc_queued_call_t), pointer :: q, qn
      node => conn%imports
      do while (associated(node))
         nxt => node%next
         if (node%pinned .and. node%resolved_id >= 0_int64 .and. &
             node%resolved_id < int(MAXE, int64)) then
            call drop_export_pin(conn, int(node%resolved_id))
         end if
         deallocate (node)
         node => nxt
      end do
      conn%imports => null()
      emb => conn%embargoes
      do while (associated(emb))
         emb_next => emb%next
         q => emb%queued
         do while (associated(q))
            qn => q%next
            call capnp_message_free(q%msg)
            deallocate (q)
            q => qn
         end do
         deallocate (emb)
         emb => emb_next
      end do
      conn%embargoes => null()
      conn%next_embargo = 0_int64
   end subroutine free_imports

   subroutine import_find(conn, id, node, found)
      type(rpc_conn_t), intent(in) :: conn
      integer(int64), intent(in) :: id
      type(rpc_import_node_t), pointer, intent(out) :: node
      logical, intent(out) :: found
      node => conn%imports
      found = .false.
      do while (associated(node))
         if (node%id == id) then
            found = .true.
            return
         end if
         node => node%next
      end do
      node => null()
   end subroutine import_find

   !> The import currently holding references to a peer wire id. After a
   !> reflected resolution the id may also name a released local alias, so
   !> `import_find_wire` alone can select the wrong node.
   subroutine import_find_live(conn, wire_id, node, found)
      type(rpc_conn_t), intent(in) :: conn
      integer(int64), intent(in) :: wire_id
      type(rpc_import_node_t), pointer, intent(out) :: node
      logical, intent(out) :: found
      node => conn%imports
      found = .false.
      do while (associated(node))
         if (node%wire_id == wire_id .and. node%refs > 0 .and. &
             .not. node%released) then
            found = .true.
            return
         end if
         node => node%next
      end do
      node => null()
   end subroutine import_find_live

   subroutine import_find_wire(conn, wire_id, node, found)
      type(rpc_conn_t), intent(in) :: conn
      integer(int64), intent(in) :: wire_id
      type(rpc_import_node_t), pointer, intent(out) :: node
      logical, intent(out) :: found
      node => conn%imports
      found = .false.
      do while (associated(node))
         if (node%wire_id == wire_id) then
            found = .true.
            return
         end if
         node => node%next
      end do
      node => null()
   end subroutine import_find_wire

   !> Remember an import id the first time a cap descriptor names it.
   !> A later descriptor for the same id does not change a promise into
   !> a settled import or the reverse.
   subroutine import_note(conn, id, promise)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: id
      logical, intent(in) :: promise
      type(rpc_import_node_t), pointer :: node
      logical :: found
      if (id < 0_int64) return
      call import_find(conn, id, node, found)
      if (found) return
      allocate (node)
      node%id = id
      node%wire_id = id
      node%refs = 1
      node%released = .false.
      if (promise) then
         node%state = IMP_PROMISE
      else
         node%state = IMP_HOSTED
      end if
      node%next => conn%imports
      conn%imports => node
   end subroutine import_note

   !> A new delivery of an id we already know is another remote reference.
   subroutine import_receive(conn, id, promise, handle)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: id
      logical, intent(in) :: promise
      integer(int64), intent(out), optional :: handle
      type(rpc_import_node_t), pointer :: node
      logical :: found
      call import_find_live(conn, id, node, found)
      if (found) then
         node%refs = node%refs + 1
         if (present(handle)) handle = node%id
         return
      end if
      call import_find(conn, id, node, found)
      if (.not. found) then
         call import_note(conn, id, promise)
         if (present(handle)) handle = id
         return
      end if
      ! A released id can be handed out again. The old node keeps its own
      ! handle: a handle recorded before the Release or Finish that freed
      ! the id may still be in use, and reviving the node in place would
      ! make that stale handle name the new import. Every reuse gets a new
      ! node and a new handle.
      if (node%released .or. node%refs <= 0) then
         allocate (node)
         node%id = conn%next_import_handle
         conn%next_import_handle = conn%next_import_handle - 1_int64
         node%wire_id = id
         node%refs = 1
         node%released = .false.
         if (promise) then
            node%state = IMP_PROMISE
         else
            node%state = IMP_HOSTED
         end if
         node%next => conn%imports
         conn%imports => node
         if (present(handle)) handle = node%id
         return
      end if
      node%refs = node%refs + 1
      node%released = .false.
      if (present(handle)) handle = node%id
   end subroutine import_receive

   !> Record senderHosted and senderPromise rows of a payload cap table.
   !> `record` is false for a call's params: those caps are the sender's
   !> exports, and counting them here would invent imports. An unknown
   !> descriptor aborts either way.
   subroutine note_payload_imports(conn, pl, record, err, handles, nhandles)
      type(rpc_conn_t), intent(inout), target :: conn
      type(payload_t), intent(in) :: pl
      logical, intent(in) :: record
      integer, intent(out) :: err
      integer(int64), intent(out), optional :: handles(0:)
      integer, intent(out), optional :: nhandles
      type(capnp_ptr_t) :: ctab
      type(cap_descriptor_t) :: cd
      integer :: i, n, ierr
      integer(int64) :: h
      err = CAPNP_OK
      if (present(handles)) handles = -1_int64
      if (present(nhandles)) nhandles = 0
      if (pl%p%kind /= CAPNP_PK_STRUCT) return
      ctab = payload_cap_table_get(pl, ierr)
      if (ierr /= CAPNP_OK) return
      n = int(capnp_list_len(ctab))
      if (present(handles) .and. present(nhandles)) &
         nhandles = min(n, size(handles))
      do i = 0, n - 1
         cd%p = capnp_list_get_struct(ctab, i, ierr)
         if (ierr /= CAPNP_OK) cycle
         h = -1_int64
         select case (cap_descriptor_which(cd))
         case (CAP_DESCRIPTOR_NONE_TAG)
         case (CAP_DESCRIPTOR_SENDER_HOSTED_TAG)
            if (record) call import_receive(conn, &
                                            cap_descriptor_sender_hosted_get(cd), .false., h)
         case (CAP_DESCRIPTOR_SENDER_PROMISE_TAG)
            if (record) call import_receive(conn, &
                                            cap_descriptor_sender_promise_get(cd), .true., h)
         case (CAP_DESCRIPTOR_RECEIVER_HOSTED_TAG)
         case (CAP_DESCRIPTOR_RECEIVER_ANSWER_TAG)
         case (CAP_DESCRIPTOR_THIRD_PARTY_HOSTED_TAG)
            ! C++ imports the vine for a result the same way it imports
            ! senderHosted (rpc.c++ receiveCap, 1628-1630). Call params
            ! stay on the introduction record: `record` is false there,
            ! and `rpc_introduction_done` releases that vine itself.
            if (record) then
               block
                  type(third_party_cap_descriptor_t) :: tp
                  integer(int64) :: vine
                  integer :: verr
                  tp = cap_descriptor_third_party_hosted_get(cd, verr)
                  if (verr == CAPNP_OK .and. tp%p%kind == CAPNP_PK_STRUCT) then
                     vine = third_party_cap_descriptor_vine_id_get(tp)
                     if (vine >= 0_int64) call import_receive(conn, vine, .false., h)
                  end if
               end block
            end if
         case default
            call abort_conn(conn, 'unknown CapDescriptor type', err)
            return
         end select
         if (present(handles) .and. h /= -1_int64) then
            if (i < size(handles)) handles(i) = h
         end if
      end do
   end subroutine note_payload_imports

   !> Follow remote resolutions. A broken promise sets broken and does
   !> not produce an id to call.
   subroutine import_follow(conn, id, out_id, broken, reason)
      type(rpc_conn_t), intent(in) :: conn
      integer(int64), intent(in) :: id
      integer(int64), intent(out) :: out_id
      logical, intent(out) :: broken
      character(len=:), allocatable, intent(out) :: reason
      type(rpc_import_node_t), pointer :: node
      logical :: found
      integer :: step
      broken = .false.
      out_id = id
      do step = 1, 8
         call import_find(conn, out_id, node, found)
         if (.not. found) return
         select case (node%state)
         case (IMP_REMOTE)
            out_id = node%resolved_id
         case (IMP_BROKEN)
            broken = .true.
            if (allocated(node%reason)) reason = node%reason
            return
         case default
            return
         end select
      end do
   end subroutine import_follow

   subroutine import_wire_id(conn, handle, wire_id)
      type(rpc_conn_t), intent(in) :: conn
      integer(int64), intent(in) :: handle
      integer(int64), intent(out) :: wire_id
      type(rpc_import_node_t), pointer :: node
      logical :: found
      wire_id = handle
      call import_find(conn, handle, node, found)
      if (found) wire_id = node%wire_id
   end subroutine import_wire_id

   subroutine note_sent_promise(conn, m, qid)
      type(rpc_conn_t), intent(inout) :: conn
      type(capnp_message_t), intent(inout), target :: m
      integer(int64), intent(in), optional :: qid
      type(message_t) :: msg
      type(call_t) :: c
      type(message_target_t) :: tgt
      type(rpc_import_node_t), pointer :: node
      logical :: found
      integer :: rerr
      integer(int64) :: id
      msg = message_read_root(m, rerr)
      if (rerr /= CAPNP_OK) return
      if (message_which(msg) /= MESSAGE_CALL_TAG) return
      c = message_call_get(msg, rerr)
      if (rerr /= CAPNP_OK) return
      tgt = call_target_get(c, rerr)
      if (rerr /= CAPNP_OK) return
      if (message_target_which(tgt) /= MESSAGE_TARGET_IMPORTED_CAP_TAG) return
      id = message_target_imported_cap_get(tgt)
      if (present(qid)) then
         if (qid >= 0_int64 .and. qid < int(MAXQ, int64)) then
            if (conn%questions(int(qid))%import_handle /= -1_int64) &
               id = conn%questions(int(qid))%import_handle
         end if
      end if
      call import_find(conn, id, node, found)
      if (.not. found) return
      if (node%state == IMP_PROMISE) node%received_call = .true.
   end subroutine note_sent_promise

   subroutine send_sender_loopback(conn, promise_id, embargo_id, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: promise_id, embargo_id
      integer, intent(out) :: err
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(disembargo_t) :: d
      type(message_target_t) :: tgt
      call capnp_message_init_builder(m, err)
      if (err /= CAPNP_OK) return
      msg = message_new_root(m, err)
      if (err /= CAPNP_OK) then
         call capnp_message_free(m)
         return
      end if
      d = message_disembargo_init(msg, err)
      if (err == CAPNP_OK) call disembargo_context_sender_loopback_set(d, embargo_id, err)
      if (err == CAPNP_OK) tgt = disembargo_target_init(d, err)
      if (err == CAPNP_OK) call message_target_imported_cap_set(tgt, promise_id, err)
      if (err == CAPNP_OK) call rpc_send_message(conn%transport, m, err)
      call capnp_message_free(m)
   end subroutine send_sender_loopback

   !> Echo of senderLoopback: receiverLoopback, target importedCap.
   !> writeTarget on an ImportClient (rpc.c++ 896-899, 3508).
   subroutine send_receiver_loopback_import(conn, embargo_id, import_id, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: embargo_id, import_id
      integer, intent(out) :: err
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(disembargo_t) :: d
      type(message_target_t) :: tgt
      call capnp_message_init_builder(m, err)
      if (err /= CAPNP_OK) return
      msg = message_new_root(m, err)
      if (err /= CAPNP_OK) then
         call capnp_message_free(m)
         return
      end if
      d = message_disembargo_init(msg, err)
      if (err == CAPNP_OK) call disembargo_context_receiver_loopback_set(d, embargo_id, err)
      if (err == CAPNP_OK) tgt = disembargo_target_init(d, err)
      if (err == CAPNP_OK) call message_target_imported_cap_set(tgt, import_id, err)
      if (err == CAPNP_OK) call rpc_send_message(conn%transport, m, err)
      call capnp_message_free(m)
   end subroutine send_receiver_loopback_import

   !> Echo of senderLoopback aimed at a pipeline client. writeTarget
   !> copies the promisedAnswer (rpc.c++ PipelineClient, 948-953).
   subroutine send_receiver_loopback_answer(conn, embargo_id, src, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: embargo_id
      type(promised_answer_t), intent(in) :: src
      integer, intent(out) :: err
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(disembargo_t) :: d
      type(message_target_t) :: tgt
      type(promised_answer_t) :: pa
      type(promised_answer_op_t) :: op, src_op
      type(capnp_ptr_t) :: ops, new_ops
      integer(int64) :: qid
      integer :: i, n
      qid = promised_answer_question_id_get(src)
      ops = promised_answer_transform_get(src, err)
      if (err /= CAPNP_OK) return
      n = int(capnp_list_len(ops))
      call capnp_message_init_builder(m, err)
      if (err /= CAPNP_OK) return
      msg = message_new_root(m, err)
      if (err /= CAPNP_OK) then
         call capnp_message_free(m)
         return
      end if
      d = message_disembargo_init(msg, err)
      if (err == CAPNP_OK) call disembargo_context_receiver_loopback_set(d, embargo_id, err)
      if (err == CAPNP_OK) tgt = disembargo_target_init(d, err)
      if (err == CAPNP_OK) pa = message_target_promised_answer_init(tgt, err)
      if (err == CAPNP_OK) call promised_answer_question_id_set(pa, qid, err)
      if (err == CAPNP_OK) new_ops = promised_answer_transform_init(pa, int(n, int64), err)
      if (err == CAPNP_OK) then
         do i = 0, n - 1
            src_op%p = capnp_list_get_struct(ops, i, err)
            if (err /= CAPNP_OK) exit
            op%p = capnp_list_get_struct(new_ops, i, err)
            if (err /= CAPNP_OK) exit
            select case (promised_answer_op_which(src_op))
            case (PROMISED_ANSWER_OP_NOOP_TAG)
               call promised_answer_op_noop_set(op, err)
            case (PROMISED_ANSWER_OP_GET_POINTER_FIELD_TAG)
               call promised_answer_op_get_pointer_field_set(op, &
                  promised_answer_op_get_pointer_field_get(src_op), err)
            case default
               err = CAPNP_ERR_KIND
            end select
            if (err /= CAPNP_OK) exit
         end do
      end if
      if (err == CAPNP_OK) call rpc_send_message(conn%transport, m, err)
      call capnp_message_free(m)
   end subroutine send_receiver_loopback_answer

   subroutine resolve_remote(conn, node, new_id, as_promise, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(rpc_import_node_t), intent(inout) :: node
      integer(int64), intent(in) :: new_id
      logical, intent(in) :: as_promise
      integer, intent(out) :: err
      type(rpc_import_node_t), pointer :: other
      logical :: found, held
      integer :: nheld
      integer(int64) :: new_handle
      err = CAPNP_OK
      ! The application still holds this promise when refs remain. The
      ! promise import itself is dropped now; the replacement is what a
      ! later rpc_release_send releases.
      held = node%refs > 0 .and. .not. node%released
      nheld = merge(node%refs, 0, held)
      call release_all_import(conn, node, err)
      if (err /= CAPNP_OK) return
      node%state = IMP_REMOTE
      node%local_refs = nheld
      call import_receive(conn, new_id, as_promise, new_handle)
      node%resolved_id = new_handle
      if (.not. held) then
         call import_find(conn, new_handle, other, found)
         if (found) call release_one_import(conn, other, err)
         return
      end if
      if (.not. node%received_call) return
      call import_find(conn, new_handle, other, found)
      if (found) then
         if (other%state == IMP_PROMISE) other%received_call = .true.
      end if
   end subroutine resolve_remote

   !> `pipelined` marks a capability taken from one of our answers. The
   !> answer's pipeline holds it until Finish, so a pin-only export is
   !> still live there (rpc.c++ 1604-1615).
   subroutine resolve_reflected(conn, node, local_id, err, pipelined)
      type(rpc_conn_t), intent(inout), target :: conn
      type(rpc_import_node_t), intent(inout), target :: node
      integer(int64), intent(in) :: local_id
      integer, intent(out) :: err
      logical, intent(in), optional :: pipelined
      type(rpc_embargo_node_t), pointer :: emb
      integer(int64) :: eid
      logical :: held, valid, via_answer
      integer :: nheld
      err = CAPNP_OK
      via_answer = .false.
      if (present(pipelined)) via_answer = pipelined
      held = node%refs > 0 .and. .not. node%released
      nheld = merge(node%refs, 0, held)
      ! receiveCap addRefs a receiverHosted export (rpc.c++ 1595-1600).
      ! A pin-only export counts as missing: once the wire refcount drops
      ! to 0, C++ has erased the id from its table. A missing export is a
      ! broken capability, so no Disembargo follows (rpc.c++ 1171-1181).
      valid = .false.
      if (local_id >= 0_int64 .and. local_id < int(MAXE, int64)) then
         valid = conn%exports(int(local_id))%used .and. &
                 (via_answer .or. conn%exports(int(local_id))%refcount > 0)
      end if
      if (.not. valid) then
         call release_all_import(conn, node, err)
         if (err /= CAPNP_OK) return
         node%resolved_id = -1_int64
         node%state = IMP_BROKEN
         node%reason = "invalid 'receiverHosted' export ID"
         return
      end if
      node%local_refs = nheld
      node%resolved_id = local_id
      if (held) then
         call pin_export(conn, int(local_id))
         node%pinned = .true.
      end if
      if (.not. held .or. .not. node%received_call) then
         node%state = IMP_LOCAL
         call release_all_import(conn, node, err)
         return
      end if
      eid = conn%next_embargo
      conn%next_embargo = eid + 1_int64
      allocate (emb)
      emb%id = eid
      emb%promise_id = node%id
      emb%open = .true.
      emb%next => conn%embargoes
      conn%embargoes => emb
      node%state = IMP_EMBARGO
      node%embargo_id = eid
      call send_sender_loopback(conn, node%wire_id, eid, err)
      ! C++ drops the promise ImportClient after the Disembargo is sent.
      if (err == CAPNP_OK) call release_all_import(conn, node, err)
   end subroutine resolve_reflected

   subroutine release_resolve_cap(conn, cd, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(cap_descriptor_t), intent(in) :: cd
      integer, intent(out) :: err
      type(third_party_cap_descriptor_t) :: tp
      integer(int64) :: id
      integer :: ierr
      err = CAPNP_OK
      id = -1_int64
      select case (cap_descriptor_which(cd))
      case (CAP_DESCRIPTOR_SENDER_HOSTED_TAG)
         id = cap_descriptor_sender_hosted_get(cd)
      case (CAP_DESCRIPTOR_SENDER_PROMISE_TAG)
         id = cap_descriptor_sender_promise_get(cd)
      case (CAP_DESCRIPTOR_THIRD_PARTY_HOSTED_TAG)
         tp = cap_descriptor_third_party_hosted_get(cd, ierr)
         if (ierr == CAPNP_OK) id = third_party_cap_descriptor_vine_id_get(tp)
      end select
      if (id >= 0_int64) call send_release(conn, id, 1_int64, err)
   end subroutine release_resolve_cap

   !> Fulfill a promise import. An unknown id is ignored. CapDescriptor
   !> none and a Resolve for a settled import abort the connection.
   subroutine handle_resolve(conn, msg, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(message_t), intent(in) :: msg
      integer, intent(out) :: err
      type(resolve_t) :: rv
      type(cap_descriptor_t) :: cd
      type(exception_t) :: ex
      type(rpc_import_node_t), pointer :: node
      logical :: found
      integer(int64) :: pid
      character(len=:), allocatable :: reason
      err = CAPNP_OK
      rv = message_resolve_get(msg, err)
      if (err /= CAPNP_OK) return
      pid = resolve_promise_id_get(rv)
      if (resolve_which(rv) == RESOLVE_EXCEPTION_TAG) then
         call import_find_wire(conn, pid, node, found)
         if (.not. found) return
         if (node%state /= IMP_PROMISE) then
            if (node%state == IMP_HOSTED .and. .not. node%released) then
               call abort_conn(conn, "Got 'Resolve' for a non-promise import.", err)
            end if
            return
         end if
         ex = resolve_exception_get(rv, err)
         if (err /= CAPNP_OK) return
         call exception_reason_get(ex, reason, err)
         call release_all_import(conn, node, err)
         if (err /= CAPNP_OK) return
         node%state = IMP_BROKEN
         if (allocated(reason)) node%reason = reason
         if (.not. allocated(node%reason)) node%reason = 'promise rejected'
         return
      end if
      if (resolve_which(rv) /= RESOLVE_CAP_TAG) then
         call abort_conn(conn, "Unknown 'Resolve' type.", err)
         return
      end if
      cd = resolve_cap_get(rv, err)
      if (err /= CAPNP_OK) return
      if (cap_descriptor_which(cd) == CAP_DESCRIPTOR_NONE_TAG) then
         call abort_conn(conn, "'Resolve' contained 'CapDescriptor.none'.", err)
         return
      end if
      call import_find_wire(conn, pid, node, found)
      if (.not. found) then
         call release_resolve_cap(conn, cd, err)
         return
      end if
      ! A second Resolve arrives after the promise import has been
      ! superseded. C++ has already erased that import and ignores it.
      ! A hosted import that was never a promise is still an error.
      if (node%state /= IMP_PROMISE) then
         if (node%state == IMP_HOSTED .and. .not. node%released) then
            call abort_conn(conn, "Got 'Resolve' for a non-promise import.", err)
         else
            call release_resolve_cap(conn, cd, err)
         end if
         return
      end if
      select case (cap_descriptor_which(cd))
      case (CAP_DESCRIPTOR_SENDER_HOSTED_TAG)
         call resolve_remote(conn, node, cap_descriptor_sender_hosted_get(cd), .false., err)
      case (CAP_DESCRIPTOR_SENDER_PROMISE_TAG)
         call resolve_remote(conn, node, cap_descriptor_sender_promise_get(cd), .true., err)
      case (CAP_DESCRIPTOR_RECEIVER_HOSTED_TAG)
         call resolve_reflected(conn, node, cap_descriptor_receiver_hosted_get(cd), err)
      case (CAP_DESCRIPTOR_THIRD_PARTY_HOSTED_TAG)
         block
            type(third_party_cap_descriptor_t) :: tp
            tp = cap_descriptor_third_party_hosted_get(cd, err)
            if (err /= CAPNP_OK) return
            ! Level 1 uses the vine. C++ imports it and does not abort.
            call resolve_remote(conn, node, &
                                third_party_cap_descriptor_vine_id_get(tp), .false., err)
         end block
      case (CAP_DESCRIPTOR_RECEIVER_ANSWER_TAG)
         call resolve_receiver_answer(conn, node, cd, err)
      case default
         call abort_conn(conn, "Unknown 'Resolve' type.", err)
      end select
   end subroutine handle_resolve

   !> Resolve a promise to a capability inside one of our answers, the
   !> way receiveCap reads `answer.pipeline` (rpc.c++ 1604-1624). Only an
   !> answer that is still active and has results can supply one; any
   !> other answer, and a walk that reaches a null pointer, gives a broken
   !> capability. An unknown op or a walk through a pointer that is not a
   !> struct makes C++ 1.4.0 abort the connection, so it does here too.
   subroutine resolve_receiver_answer(conn, node, cd, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(rpc_import_node_t), intent(inout), target :: node
      type(cap_descriptor_t), intent(in) :: cd
      integer, intent(out) :: err
      type(promised_answer_t) :: pa
      type(promised_answer_op_t) :: op
      type(capnp_ptr_t) :: ops, p, ctab
      type(message_t) :: amsg
      type(return_t) :: ar
      type(payload_t) :: apl
      type(cap_descriptor_t) :: acd
      type(rpc_answer_slot_t), pointer :: slot
      logical :: active
      integer(int64) :: aqid
      integer :: i, n
      integer, allocatable :: fields(:)
      character(len=:), allocatable :: reason
      character(len=*), parameter :: not_struct = "Schema mismatch: Message contains " // &
                                     "non-struct pointer where struct pointer was expected."
      character(len=*), parameter :: not_cap = "Schema mismatch: Message contains " // &
                                     "non-capability pointer where capability pointer was expected."
      err = CAPNP_OK
      reason = "invalid 'receiverAnswer'"
      pa = cap_descriptor_receiver_answer_get(cd, err)
      if (err /= CAPNP_OK) return
      aqid = promised_answer_question_id_get(pa)
      ops = promised_answer_transform_get(pa, err)
      if (err /= CAPNP_OK) return
      n = int(capnp_list_len(ops))
      allocate (fields(n))
      do i = 0, n - 1
         op%p = capnp_list_get_struct(ops, i, err)
         if (err /= CAPNP_OK) return
         select case (promised_answer_op_which(op))
         case (PROMISED_ANSWER_OP_NOOP_TAG)
            fields(i + 1) = -1
         case (PROMISED_ANSWER_OP_GET_POINTER_FIELD_TAG)
            fields(i + 1) = int(promised_answer_op_get_pointer_field_get(op))
         case default
            call abort_conn(conn, 'Unsupported pipeline op.', err)
            return
         end select
      end do
      call answer_find(conn, aqid, slot, active)
      if (active) active = associated(slot)
      if (active) active = slot%used .and. .not. slot%finished
      if (active .and. .not. slot%has_results) then
         node%state = IMP_ANSWER
         node%answer_qid = aqid
         if (allocated(node%answer_ops)) deallocate (node%answer_ops)
         allocate (node%answer_ops(n))
         node%answer_ops = fields
         return
      end if
      if (active) then
         amsg = message_read_root(slot%retmsg, err)
         if (err == CAPNP_OK) ar = message_return_get(amsg, err)
         active = err == CAPNP_OK
         if (active) active = return_which(ar) == RETURN_RESULTS_TAG
         err = CAPNP_OK
      end if
      if (active) then
         apl = return_results_get(ar, err)
         if (err == CAPNP_OK) p = payload_content_get(apl, err)
         if (err /= CAPNP_OK) return
         do i = 1, n
            if (fields(i) < 0) cycle
            ! A null pointer reads as the default struct, whose fields
            ! are null too.
            if (p%kind == CAPNP_PK_NULL) cycle
            if (p%kind /= CAPNP_PK_STRUCT) then
               call abort_conn(conn, not_struct, err)
               return
            end if
            p = capnp_getp(p, fields(i), err)
            if (err /= CAPNP_OK) then
               call abort_conn(conn, not_struct, err)
               return
            end if
         end do
         if (p%kind == CAPNP_PK_NULL) then
            reason = 'Called null capability.'
         else if (p%kind /= CAPNP_PK_CAP) then
            call abort_conn(conn, not_cap, err)
            return
         else
            ctab = payload_cap_table_get(apl, err)
            if (err == CAPNP_OK .and. p%capidx >= 0_int64 .and. &
                p%capidx < capnp_list_len(ctab)) then
               acd%p = capnp_list_get_struct(ctab, int(p%capidx), err)
               if (err == CAPNP_OK) then
                  select case (cap_descriptor_which(acd))
                  case (CAP_DESCRIPTOR_SENDER_HOSTED_TAG)
                     call resolve_reflected(conn, node, &
                                            cap_descriptor_sender_hosted_get(acd), err, &
                                            pipelined=.true.)
                     return
                  case (CAP_DESCRIPTOR_SENDER_PROMISE_TAG)
                     call resolve_reflected(conn, node, &
                                            cap_descriptor_sender_promise_get(acd), err, &
                                            pipelined=.true.)
                     return
                  case (CAP_DESCRIPTOR_RECEIVER_HOSTED_TAG)
                     call resolve_reflected(conn, node, &
                                            cap_descriptor_receiver_hosted_get(acd), err, &
                                            pipelined=.true.)
                     return
                  end select
               end if
            end if
            err = CAPNP_OK
         end if
      end if
      call release_all_import(conn, node, err)
      if (err /= CAPNP_OK) return
      node%state = IMP_BROKEN
      node%reason = reason
   end subroutine resolve_receiver_answer

   subroutine settle_answer_promises(conn, qid, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: qid
      integer, intent(out) :: err
      type(rpc_import_node_t), pointer :: node
      type(rpc_answer_slot_t), pointer :: slot
      type(message_t) :: msg
      type(return_t) :: ret
      type(payload_t) :: pl
      type(capnp_ptr_t) :: content, ctab
      type(cap_descriptor_t) :: cd
      integer :: i
      logical :: found

      err = CAPNP_OK
      call answer_find(conn, qid, slot, found)
      if (.not. found) return
      if (.not. associated(slot)) return
      if (.not. slot%has_results) return
      msg = message_read_root(slot%retmsg, err)
      if (err == CAPNP_OK) ret = message_return_get(msg, err)
      if (err == CAPNP_OK) pl = return_results_get(ret, err)
      if (err == CAPNP_OK) content = payload_content_get(pl, err)
      if (err /= CAPNP_OK) return

      node => conn%imports
      do while (associated(node))
         if (node%state == IMP_ANSWER .and. node%answer_qid == qid) then
            do i = 1, size(node%answer_ops)
               if (node%answer_ops(i) < 0) cycle
               if (content%kind == CAPNP_PK_NULL) cycle
               if (content%kind /= CAPNP_PK_STRUCT) then
                  call abort_conn(conn, 'Schema mismatch: Message contains non-struct pointer ' // &
                                  'where struct pointer was expected.', err)
                  return
               end if
               content = capnp_getp(content, node%answer_ops(i), err)
               if (err /= CAPNP_OK) return
            end do
            if (content%kind /= CAPNP_PK_CAP) then
               call release_all_import(conn, node, err)
               if (err /= CAPNP_OK) return
               node%state = IMP_BROKEN
               node%reason = 'Called null capability.'
            else
               ctab = payload_cap_table_get(pl, err)
               if (err /= CAPNP_OK) return
               if (content%capidx < 0_int64 .or. content%capidx >= capnp_list_len(ctab)) then
                  call release_all_import(conn, node, err)
                  if (err /= CAPNP_OK) return
                  node%state = IMP_BROKEN
                  node%reason = "invalid 'receiverAnswer'"
               else
                  cd%p = capnp_list_get_struct(ctab, int(content%capidx), err)
                  if (err /= CAPNP_OK) return
                  select case (cap_descriptor_which(cd))
                  case (CAP_DESCRIPTOR_SENDER_HOSTED_TAG)
                     call resolve_reflected(conn, node, &
                                            cap_descriptor_sender_hosted_get(cd), err, &
                                            pipelined=.true.)
                     if (err /= CAPNP_OK) return
                  case (CAP_DESCRIPTOR_SENDER_PROMISE_TAG)
                     call resolve_reflected(conn, node, &
                                            cap_descriptor_sender_promise_get(cd), err, &
                                            pipelined=.true.)
                     if (err /= CAPNP_OK) return
                  case (CAP_DESCRIPTOR_RECEIVER_HOSTED_TAG)
                     call resolve_reflected(conn, node, &
                                            cap_descriptor_receiver_hosted_get(cd), err, &
                                            pipelined=.true.)
                     if (err /= CAPNP_OK) return
                  case default
                     call release_all_import(conn, node, err)
                     if (err /= CAPNP_OK) return
                     node%state = IMP_BROKEN
                     node%reason = "invalid 'receiverAnswer'"
                  end select
               end if
            end if
            call drain_answer_pipeline(conn, node%id, err)
            if (err /= CAPNP_OK) return
         end if
         node => node%next
      end do
   end subroutine settle_answer_promises

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

   !> Send Release. count is the referenceCount field.
   subroutine send_release(conn, id, count, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: id, count
      integer, intent(out) :: err
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(release_t) :: rel
      err = CAPNP_OK
      if (count <= 0_int64) return
      if (.not. associated(conn%transport) .or. conn%dead) return
      call capnp_message_init_builder(m, err)
      if (err /= CAPNP_OK) return
      msg = message_new_root(m, err)
      if (err /= CAPNP_OK) then
         call capnp_message_free(m)
         return
      end if
      rel = message_release_init(msg, err)
      if (err == CAPNP_OK) call release_id_set(rel, id, err)
      if (err == CAPNP_OK) call release_reference_count_set(rel, count, err)
      if (err == CAPNP_OK) call rpc_send_message(conn%transport, m, err)
      call capnp_message_free(m)
   end subroutine send_release

   !> Drop every remote reference this import still owes. Used when a
   !> promise import is superseded and its ImportClient is destroyed.
   subroutine release_all_import(conn, node, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(rpc_import_node_t), intent(inout) :: node
      integer, intent(out) :: err
      integer :: n
      err = CAPNP_OK
      if (node%released .or. node%refs <= 0) return
      n = node%refs
      node%refs = 0
      node%released = .true.
      call send_release(conn, node%wire_id, int(n, int64), err)
   end subroutine release_all_import

   !> Drop one remote reference, matching one rpc_release_send.
   subroutine release_one_import(conn, node, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(rpc_import_node_t), intent(inout) :: node
      integer, intent(out) :: err
      err = CAPNP_OK
      if (node%released .or. node%refs <= 0) return
      node%refs = node%refs - 1
      if (node%refs == 0) node%released = .true.
      call send_release(conn, node%wire_id, 1_int64, err)
   end subroutine release_one_import

   !> Drop refcount on an imported capability. A resolved promise
   !> releases the replacement, which is the cap the peer still exports.
   subroutine rpc_release_send(conn, cap, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(rpc_cap_t), intent(in) :: cap
      integer, intent(out) :: err
      type(rpc_import_node_t), pointer :: node, nxt
      logical :: found
      integer :: step
      err = CAPNP_OK
      if (cap%kind == RPC_CAP_LOCAL) then
         if (cap%id >= 0_int64 .and. cap%id < int(MAXE, int64)) &
            call drop_export(conn, int(cap%id), 1)
         return
      end if
      if (cap%kind /= RPC_CAP_IMPORT) return
      call import_find(conn, cap%id, node, found)
      if (.not. found) then
         call send_release(conn, cap%id, 1_int64, err)
         return
      end if
      do step = 1, 8
         if (node%state /= IMP_REMOTE) exit
         if (node%local_refs <= 0) return
         node%local_refs = node%local_refs - 1
         if (node%local_refs > 0) return
         call import_find(conn, node%resolved_id, nxt, found)
         if (.not. found) return
         node => nxt
      end do
      if (node%state == IMP_LOCAL .or. node%state == IMP_EMBARGO) then
         if (node%local_refs <= 0) return
         node%local_refs = node%local_refs - 1
         if (node%local_refs > 0) return
         if (node%pinned .and. node%resolved_id >= 0_int64 .and. &
             node%resolved_id < int(MAXE, int64)) then
            call drop_export_pin(conn, int(node%resolved_id))
            node%pinned = .false.
         end if
         return
      end if
      if (node%state == IMP_BROKEN) return
      call release_one_import(conn, node, err)
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
            conn%exports(i)%pins = 0
            call retain_server(srv)
            conn%exports(i)%srv => srv
            conn%exports(i)%owned = own
            conn%exports(i)%shared = .false.
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
         call handle_resolve(conn, msg, err)
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
      slot%finished = .false.
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
            if (err == CAPNP_OK) call flush_cap_table(conn, ctx, err)
         end if
         if (err /= CAPNP_OK) then
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
      logical :: dispatched, elsewhere
      c = message_call_get(msg, err)
      if (err /= CAPNP_OK) return
      qid = call_question_id_get(c)
      if (.not. claim_question(conn, qid, err)) return
      elsewhere = call_send_results_to_which(c) == CALL_SEND_RESULTS_TO_YOURSELF_TAG
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
      if (err == CAPNP_OK) call note_payload_imports(conn, params, .false., err)
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
         call finish_answer(conn, qid, rm, ctx, err, elsewhere=elsewhere)
         return
      end if
      ctx%interface_id = call_interface_id_get(c)
      ctx%method_id = int(call_method_id_get(c))
      ctx%params = payload_content_get(params, err)
      ctx%rmsg => rm
      ctx%results = return_results_init(r, err)
      ! The call keeps one reference so Release or close cannot destroy
      ! the server between entering dispatch and leaving it.
      call pin_export(conn, eid)
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
         call rollback_staged(conn, ctx)
         call fill_exception(r, 'method raised error', err)
      else
         call flush_cap_table(conn, ctx, err)
         if (err /= CAPNP_OK) then
            ! Exports added by the failed flush are dropped first;
            ! remaining owned servers and wrappers are freed once.
            call rollback_staged(conn, ctx)
            call fill_exception(r, 'export failed', err)
         end if
      end if
      conn%exports(eid)%dispatching = conn%exports(eid)%dispatching - 1
      call drop_export_pin(conn, eid)
      call finish_answer(conn, qid, rm, ctx, err, elsewhere=elsewhere)
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
         if (conn%exports(int(eid))%used .and. &
             conn%exports(int(eid))%refcount > 0) out_eid = int(eid)
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
         ! rpc.c++ 3109-3125 resolves a promisedAnswer through the answer's
         ! own pipeline, not the export table. A peer's Release of the
         ! export id does not end the answer, so a pin keeps it callable.
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
   subroutine note_introductions(conn, pl, err, handles, nhandles, qid)
      type(rpc_conn_t), intent(inout), target :: conn
      type(payload_t), intent(in) :: pl
      integer, intent(out) :: err
      integer(int64), intent(in), optional :: handles(0:)
      integer, intent(in), optional :: nhandles
      integer(int64), intent(in), optional :: qid
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

         call reserve_introduction(conn, j, err)
         if (err /= CAPNP_OK) return
         conn%introductions(j)%used = .true.
         conn%introductions(j)%nonce = third_party_cap_id_nonce_get(id)
         conn%introductions(j)%vine_id = third_party_cap_descriptor_vine_id_get(tp)
         conn%introductions(j)%vine_handle = -1_int64
         conn%introductions(j)%has_handle = .false.
         if (present(handles) .and. present(nhandles)) then
            if (i < nhandles) then
               if (handles(i) /= -1_int64) then
                  conn%introductions(j)%vine_handle = handles(i)
                  conn%introductions(j)%has_handle = .true.
               end if
            end if
         end if
         if (present(qid)) then
            conn%introductions(j)%qid = qid
            conn%introductions(j)%capidx = i
         end if
         conn%introductions(j)%port = int(vat_id_port_get(vat))
         conn%introductions(j)%host = host
         conn%introductions(j)%host_len = len(host)
      end do
      err = CAPNP_OK
   end subroutine note_introductions

   subroutine reserve_introduction(conn, slot, err)
      type(rpc_conn_t), intent(inout), target :: conn
      integer, intent(out) :: slot
      integer, intent(out) :: err
      type(rpc_introduction_t), allocatable :: grown(:)
      integer :: i, n, stat

      slot = -1
      err = CAPNP_OK
      if (.not. allocated(conn%introductions)) then
         allocate (conn%introductions(0:MAXINTRO - 1), stat=stat)
         if (stat /= 0) then
            err = CAPNP_ERR_ALLOC
            return
         end if
         conn%introductions = rpc_introduction_t()
      end if
      do i = lbound(conn%introductions, 1), ubound(conn%introductions, 1)
         if (.not. conn%introductions(i)%used) then
            slot = i
            return
         end if
      end do
      n = size(conn%introductions)
      allocate (grown(0:max(2 * n, MAXINTRO) - 1), stat=stat)
      if (stat /= 0) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      grown = rpc_introduction_t()
      grown(0:n - 1) = conn%introductions
      call move_alloc(grown, conn%introductions)
      slot = n
   end subroutine reserve_introduction

   !> Introductions handed to us and not yet picked up.
   function rpc_pending_introductions(conn, out) result(n)
      type(rpc_conn_t), intent(in) :: conn
      type(rpc_introduction_t), intent(out), optional :: out(:)
      integer :: n, i
      n = 0
      if (.not. allocated(conn%introductions)) return
      do i = lbound(conn%introductions, 1), ubound(conn%introductions, 1)
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
      if (.not. allocated(conn%introductions)) return
      do i = lbound(conn%introductions, 1), ubound(conn%introductions, 1)
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
      if (.not. allocated(conn%introductions)) return
      do i = lbound(conn%introductions, 1), ubound(conn%introductions, 1)
         if (.not. conn%introductions(i)%used) cycle
         if (conn%introductions(i)%nonce /= nonce) cycle
         call note_result_released(conn, conn%introductions(i))
         if (.not. conn%introductions(i)%holds_vine) then
            err = CAPNP_OK
         else if (conn%introductions(i)%has_handle) then
            vine%kind = RPC_CAP_IMPORT
            vine%id = conn%introductions(i)%vine_handle
            call rpc_release_send(conn, vine, err)
         else
            call send_release(conn, conn%introductions(i)%vine_id, 1_int64, err)
         end if
         conn%introductions(i) = rpc_introduction_t()
         return
      end do
   end subroutine rpc_introduction_done

   !> A pickup before Finish releases the result's own reference; a
   !> releasing Finish must then leave that cap out.
   subroutine note_result_released(conn, intro)
      type(rpc_conn_t), intent(inout), target :: conn
      type(rpc_introduction_t), intent(in) :: intro
      integer :: q
      if (intro%qid < 0_int64 .or. intro%qid >= int(MAXQ, int64)) return
      q = int(intro%qid)
      if (.not. conn%questions(q)%used .or. .not. conn%questions(q)%returned) return
      if (.not. allocated(conn%questions(q)%result_released)) return
      if (intro%capidx < 0 .or. intro%capidx >= size(conn%questions(q)%result_released)) return
      conn%questions(q)%result_released(intro%capidx) = .true.
   end subroutine note_result_released

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
      if (err /= CAPNP_OK) then
         call abort_conn(conn, 'provide: invalid imported capability', err)
         return
      end if
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
            ! A provision is an internal hold, not a reference the peer may
            ! release. Keeping it out of `refcount` preserves over-release
            ! detection on the wire.
            call pin_export(conn, eid)
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
      logical :: take_ownership

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
            ! shares ownership when the source export owned the server.
            take_ownership = .false.
            if (associated(conn%vat%provisions(i)%host)) then
               if (conn%vat%provisions(i)%source_eid >= 0 .and. &
                   conn%vat%provisions(i)%source_eid < MAXE) then
                  take_ownership = conn%vat%provisions(i)%host%exports( &
                      conn%vat%provisions(i)%source_eid)%owned
               end if
            end if
            new_eid = export_server(conn, conn%vat%provisions(i)%srv, take_ownership)
            if (new_eid < 0) then
               call send_answer_exception(conn, qid, 'accept: export table full', err)
               return
            end if
            if (take_ownership) then
               ! The introducer's export stays an owner next to the new
               ! one, so whichever is dropped last frees the server.
               if (.not. (associated(conn%vat%provisions(i)%host, conn) .and. &
                          new_eid == conn%vat%provisions(i)%source_eid)) then
                  call share_export_owner(conn%vat%provisions(i)%host%exports( &
                                          conn%vat%provisions(i)%source_eid))
                  call share_export_owner(conn%exports(new_eid))
               end if
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
      if (err /= CAPNP_OK) then
         call abort_conn(conn, 'join: invalid imported capability', err)
         return
      end if

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
      logical :: exported, is_membrane
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
         is_membrane = .false.
         select type (p)
         type is (rpc_membrane_server_t)
            is_membrane = .true.
         end select
         if (is_membrane) cycle
         exported = .false.
         do j = 0, MAXE - 1
            if (conn%exports(j)%used .and. associated(conn%exports(j)%srv, p)) then
               exported = .true.
               exit
            end if
         end do
         if (.not. exported) deallocate (ctx%staged(i)%s)
         ctx%staged(i)%s => null()
         ctx%staged(i)%owned = .false.
      end do
   end subroutine release_unflushed

   !> Allocate export ids for the staged servers and write the results
   !> capTable. The caller owns failure rollback through `rollback_staged`;
   !> keeping it in one place avoids erasing staged ownership information.
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
         eid = export_server(conn, ctx%staged(i)%s, ctx%staged(i)%owned)
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
      if (slot%finished) then
         do i = 0, ctx%nexp - 1
            if (ctx%staged_eids(i) >= 0) call drop_export(conn, ctx%staged_eids(i), 1)
         end do
         call capnp_message_free(rm)
         call answer_slot_clear(slot)
         if (qid >= int(MAXQ, int64)) call answer_drop_hi(conn, qid)
         err = CAPNP_OK
         return
      end if
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
      ! A handler may close the connection while it is running. There is no
      ! peer to receive either ordinary or redirected results in that case.
      if (conn%dead .or. .not. associated(conn%transport)) then
         err = CAPNP_ERR_IO
      ! yourself results stay here until takeFromOtherQuestion. C++ sends
      ! resultsSentElsewhere from sendRedirectReturn, not from the call.
      else if (.not. hold) then
         heldmsg = message_read_root(rm, herr)
         if (herr == CAPNP_OK) then
            if (message_which(heldmsg) == MESSAGE_RETURN_TAG) then
               heldr = message_return_get(heldmsg, herr)
               ! Cap-less results need no Finish (rpc.c++ sendReturn,
               ! noFinishNeeded when the response has no capabilities).
               ! A cap table entry is a capability even when it is
               ! receiverHosted and adds no export.
               if (herr == CAPNP_OK) then
                  if (return_which(heldr) == RETURN_RESULTS_TAG .and. ctx%nexp == 0) then
                     block
                        type(payload_t) :: cpl
                        type(capnp_ptr_t) :: ctab
                        integer :: cerr
                        logical :: has_caps
                        has_caps = .false.
                        cpl = return_results_get(heldr, cerr)
                        if (cerr == CAPNP_OK) then
                           ctab = payload_cap_table_get(cpl, cerr)
                           if (cerr == CAPNP_OK) then
                              if (capnp_list_len(ctab) > 0_int64) has_caps = .true.
                           end if
                        end if
                        if (.not. has_caps) then
                           call return_no_finish_needed_set(heldr, .true., err)
                           if (err == CAPNP_OK) capless = .true.
                        end if
                     end block
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
         ! The answer pipeline keeps each result capability until Finish
         ! (rpc.c++ PostReturnRpcPipeline / answer.pipeline). A Release of
         ! the export id does not remove that reference.
         call pin_answer_exports(conn, slot)
         if (err == CAPNP_OK) call settle_answer_promises(conn, qid, err)
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
      ! The redirected question takes over the answer's existing result
      ! references. C++ sends Finish(releaseResultCaps=false) for the
      ! yourself call, so adding another export reference leaks it.
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
      integer :: i, eid
      err = CAPNP_OK
      call answer_find(conn, other, slot, found)
      if (.not. found) return
      if (.not. associated(slot) .or. .not. slot%used) return
      if (.not. slot%redirected .or. .not. slot%has_results) return
      do i = 0, slot%nexp - 1
         eid = slot%exports(i)
         if (eid >= 0 .and. eid < MAXE) call drop_export(conn, eid, 1)
      end do
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
            type(capnp_ptr_t) :: rctab
            integer :: ierr, ncap
            rpl = return_results_get(r, ierr)
            ncap = 0
            if (ierr == CAPNP_OK .and. rpl%p%kind == CAPNP_PK_STRUCT) then
               rctab = payload_cap_table_get(rpl, ierr)
               if (ierr == CAPNP_OK) ncap = int(capnp_list_len(rctab))
               ierr = CAPNP_OK
            end if
            if (allocated(conn%questions(q)%result_handles)) &
               deallocate (conn%questions(q)%result_handles)
            if (allocated(conn%questions(q)%result_released)) &
               deallocate (conn%questions(q)%result_released)
            allocate (conn%questions(q)%result_handles(0:ncap - 1))
            allocate (conn%questions(q)%result_released(0:ncap - 1))
            conn%questions(q)%result_released = .false.
            if (ierr == CAPNP_OK) call note_payload_imports( &
               conn, rpl, .true., ierr, &
               conn%questions(q)%result_handles, conn%questions(q)%nresult_handles)
            if (ierr == CAPNP_OK) call note_introductions( &
               conn, rpl, ierr, &
               conn%questions(q)%result_handles, conn%questions(q)%nresult_handles, &
               int(q, int64))
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
      if (.not. jslot%has_results) then
         jslot%finished = .true.
         return
      end if
      if (finish_release_result_caps_get(f)) then
         do i = 0, jslot%nexp - 1
            eid = jslot%exports(i)
            if (eid >= 0 .and. eid < MAXE) call drop_export(conn, eid, 1)
         end do
      end if
      ! Finish always drops the pipeline reference, including when
      ! releaseResultCaps is false and the wire refs stay.
      call unpin_answer_exports(conn, jslot)
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
      if (.not. conn%exports(id)%used .or. conn%exports(id)%refcount <= 0) then
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

   subroutine pin_export(conn, eid)
      type(rpc_conn_t), intent(inout) :: conn
      integer, intent(in) :: eid
      if (eid < 0 .or. eid >= MAXE) return
      if (.not. conn%exports(eid)%used) return
      conn%exports(eid)%pins = conn%exports(eid)%pins + 1
   end subroutine pin_export

   !> Pin every result export recorded on an answer. One pin per entry,
   !> so a server listed twice keeps two pins.
   subroutine pin_answer_exports(conn, slot)
      type(rpc_conn_t), intent(inout) :: conn
      type(rpc_answer_slot_t), intent(inout) :: slot
      integer :: i, eid, n
      n = 0
      do i = 0, slot%nexp - 1
         eid = slot%exports(i)
         if (eid >= 0 .and. eid < MAXE) then
            call pin_export(conn, eid)
            n = n + 1
         end if
      end do
      if (n > 0) slot%exports_pinned = .true.
   end subroutine pin_answer_exports

   !> Drop the pins `pin_answer_exports` took. A slot whose wire
   !> refcount is already zero is freed with the last pin.
   subroutine unpin_answer_exports(conn, slot)
      type(rpc_conn_t), intent(inout) :: conn
      type(rpc_answer_slot_t), intent(inout) :: slot
      integer :: i, eid
      if (.not. slot%exports_pinned) return
      slot%exports_pinned = .false.
      do i = 0, slot%nexp - 1
         eid = slot%exports(i)
         if (eid >= 0 .and. eid < MAXE) call drop_export_pin(conn, eid)
      end do
   end subroutine unpin_answer_exports

   subroutine unpin_all_answers(conn)
      type(rpc_conn_t), intent(inout), target :: conn
      type(rpc_hi_node_t), pointer :: node
      integer :: i, b
      do i = 0, MAXQ - 1
         if (conn%answers(i)%exports_pinned) &
            call unpin_answer_exports(conn, conn%answers(i))
      end do
      do b = 0, HIBUCKETS - 1
         node => conn%hi_buckets(b)%head
         do while (associated(node))
            if (node%slot%exports_pinned) &
               call unpin_answer_exports(conn, node%slot)
            node => node%next
         end do
      end do
   end subroutine unpin_all_answers

   !> Count one more export that owns `srv` after a handoff.
   subroutine shared_owner_add(srv)
      class(*), pointer, intent(in) :: srv
      type(rpc_shared_owner_t), pointer :: node
      node => g_shared_owners
      do while (associated(node))
         if (associated(node%srv, srv)) then
            node%owners = node%owners + 1
            return
         end if
         node => node%next
      end do
      allocate (node)
      node%srv => srv
      node%owners = 1
      node%next => g_shared_owners
      g_shared_owners => node
   end subroutine shared_owner_add

   !> Drop one owning export of `srv`. True when it was the last one.
   function shared_owner_drop(srv) result(last)
      class(*), pointer, intent(in) :: srv
      logical :: last
      type(rpc_shared_owner_t), pointer :: node, prev
      last = .true.
      prev => null()
      node => g_shared_owners
      do while (associated(node))
         if (associated(node%srv, srv)) then
            node%owners = node%owners - 1
            last = node%owners <= 0
            if (last) then
               if (associated(prev)) then
                  prev%next => node%next
               else
                  g_shared_owners => node%next
               end if
               deallocate (node)
            end if
            return
         end if
         prev => node
         node => node%next
      end do
   end function shared_owner_drop

   function rpc_shared_owners_live() result(n)
      integer :: n
      type(rpc_shared_owner_t), pointer :: node
      n = 0
      node => g_shared_owners
      do while (associated(node))
         n = n + 1
         node => node%next
      end do
   end function rpc_shared_owners_live

   subroutine share_export_owner(slot)
      type(rpc_export_slot_t), intent(inout) :: slot
      if (slot%shared .or. .not. slot%owned .or. .not. associated(slot%srv)) return
      ! Membranes share cross-connection ownership via `refs`.
      select type (s => slot%srv)
      type is (rpc_membrane_server_t)
         return
      end select
      call shared_owner_add(slot%srv)
      slot%shared = .true.
   end subroutine share_export_owner

   !> Deallocate the server of an owned export being dropped, unless a
   !> handoff left another export that still owns it.
   subroutine free_owned_export(slot, held)
      type(rpc_export_slot_t), intent(in) :: slot
      class(*), pointer, intent(inout) :: held
      logical :: last
      last = .true.
      if (slot%shared) last = shared_owner_drop(held)
      if (last) deallocate (held)
      held => null()
   end subroutine free_owned_export

   subroutine drop_export_pin(conn, eid)
      type(rpc_conn_t), intent(inout) :: conn
      integer, intent(in) :: eid
      class(*), pointer :: held
      if (eid < 0 .or. eid >= MAXE) return
      if (.not. conn%exports(eid)%used) return
      if (conn%exports(eid)%pins <= 0) return
      conn%exports(eid)%pins = conn%exports(eid)%pins - 1
      if (conn%exports(eid)%refcount > 0 .or. conn%exports(eid)%pins > 0) return
      held => conn%exports(eid)%srv
      select type (held)
      type is (rpc_membrane_server_t)
      class default
         if (conn%exports(eid)%owned .and. associated(held)) &
            call free_owned_export(conn%exports(eid), held)
      end select
      conn%exports(eid)%used = .false.
      conn%exports(eid)%owned = .false.
      conn%exports(eid)%shared = .false.
      conn%exports(eid)%srv => null()
      conn%exports(eid)%refcount = 0
      conn%exports(eid)%pins = 0
      conn%exports(eid)%dispatching = 0
      call release_server(held)
   end subroutine drop_export_pin

   subroutine drop_export(conn, eid, count)
      type(rpc_conn_t), intent(inout) :: conn
      integer, intent(in) :: eid, count
      integer :: left
      class(*), pointer :: held
      if (eid < 0 .or. eid >= MAXE) return
      if (.not. conn%exports(eid)%used) return
      if (count <= 0 .or. count > conn%exports(eid)%refcount) return
      left = conn%exports(eid)%refcount - count
      conn%exports(eid)%refcount = left
      if (conn%exports(eid)%refcount <= 0 .and. conn%exports(eid)%pins <= 0) then
         held => conn%exports(eid)%srv
         ! Membranes use their own refcount. Any other owned export is
         ! freed here so close drops a server the peer never Releases.
         select type (held)
         type is (rpc_membrane_server_t)
         class default
            if (conn%exports(eid)%owned .and. associated(held)) &
               call free_owned_export(conn%exports(eid), held)
         end select
         conn%exports(eid)%used = .false.
         conn%exports(eid)%owned = .false.
         conn%exports(eid)%shared = .false.
         conn%exports(eid)%srv => null()
         conn%exports(eid)%refcount = 0
         conn%exports(eid)%pins = 0
         conn%exports(eid)%dispatching = 0
         call release_server(held)
      end if
   end subroutine drop_export

   !> receiverLoopback is accepted only for an embargo id this vat issued.
   !> senderLoopback is echoed when the target resolves to an import on
   !> this connection, and aborts when the target is a local server.
   !> An unknown export id is rejected on its own.
   subroutine handle_disembargo(conn, msg, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(message_t), intent(in) :: msg
      integer, intent(out) :: err
      type(disembargo_t) :: d
      type(message_target_t) :: tgt
      integer :: which
      integer(int64) :: embargo_id
      type(rpc_embargo_node_t), pointer :: emb
      type(rpc_import_node_t), pointer :: inode
      logical :: efound
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
         call import_find(conn, emb%promise_id, inode, efound)
         if (efound) then
            if (inode%state == IMP_EMBARGO .and. inode%embargo_id == embargo_id) then
               inode%state = IMP_LOCAL
               call drain_embargo_queue(conn, emb, int(inode%resolved_id), err)
               if (err /= CAPNP_OK) return
            end if
         end if
         err = CAPNP_OK
         return
      end if
      if (which /= DISEMBARGO_CONTEXT_SENDER_LOOPBACK_TAG) then
         call abort_conn(conn, 'Unimplemented Disembargo type.', err)
         return
      end if
      ! senderLoopback. getMessageTarget on an importedCap that is not a
      ! live export aborts with the export-id text (rpc.c++ 3098-3103).
      ! A resolved target whose brand is this connection is echoed as
      ! receiverLoopback (rpc.c++ 3477-3510). A local server does not
      ! point back at the sender.
      tgt = disembargo_target_get(d, err)
      if (err /= CAPNP_OK) return
      embargo_id = disembargo_context_sender_loopback_get(d)
      if (message_target_which(tgt) == MESSAGE_TARGET_IMPORTED_CAP_TAG) then
         block
            integer(int64) :: eid
            eid = message_target_imported_cap_get(tgt)
            if (eid < 0_int64 .or. eid >= int(MAXE, int64)) then
               call abort_conn(conn, 'Message target is not a current export ID.', err)
               return
            end if
            if (.not. conn%exports(int(eid))%used .or. &
                conn%exports(int(eid))%refcount <= 0) then
               call abort_conn(conn, 'Message target is not a current export ID.', err)
               return
            end if
         end block
         call abort_conn(conn, &
                         "'Disembargo' of type 'senderLoopback' sent to an " // &
                         "object that does not point back to the sender.", err)
         return
      end if
      if (message_target_which(tgt) == MESSAGE_TARGET_PROMISED_ANSWER_TAG) then
         call echo_promised_loopback(conn, tgt, embargo_id, err)
         return
      end if
      call abort_conn(conn, &
                      "'Disembargo' of type 'senderLoopback' sent to an " // &
                      "object that does not point back to the sender.", err)
   end subroutine handle_disembargo

   !> Resolve a promisedAnswer the way getMessageTarget does, then echo
   !> when the capability is an import or a pipeline on this connection.
   subroutine echo_promised_loopback(conn, tgt, embargo_id, err)
      type(rpc_conn_t), intent(inout), target :: conn
      type(message_target_t), intent(in) :: tgt
      integer(int64), intent(in) :: embargo_id
      integer, intent(out) :: err
      type(promised_answer_t) :: pa
      type(promised_answer_op_t) :: op
      type(capnp_ptr_t) :: ops, p, ctab
      type(message_t) :: amsg
      type(return_t) :: ar
      type(payload_t) :: apl
      type(cap_descriptor_t) :: cd
      type(rpc_answer_slot_t), pointer :: aslot
      logical :: afound
      integer(int64) :: aqid
      integer :: i
      character(len=*), parameter :: back = &
         "'Disembargo' of type 'senderLoopback' sent to an " // &
         "object that does not point back to the sender."
      err = CAPNP_OK
      pa = message_target_promised_answer_get(tgt, err)
      if (err /= CAPNP_OK) return
      aqid = promised_answer_question_id_get(pa)
      call answer_find(conn, aqid, aslot, afound)
      if (.not. afound) then
         call abort_conn(conn, back, err)
         return
      end if
      if (.not. aslot%has_results) then
         call abort_conn(conn, back, err)
         return
      end if
      amsg = message_read_root(aslot%retmsg, err)
      if (err /= CAPNP_OK) return
      ar = message_return_get(amsg, err)
      if (err /= CAPNP_OK) return
      if (return_which(ar) /= RETURN_RESULTS_TAG) then
         call abort_conn(conn, back, err)
         return
      end if
      apl = return_results_get(ar, err)
      p = payload_content_get(apl, err)
      if (err /= CAPNP_OK) return
      ops = promised_answer_transform_get(pa, err)
      if (err /= CAPNP_OK) then
         call abort_conn(conn, back, err)
         return
      end if
      do i = 0, int(capnp_list_len(ops)) - 1
         op%p = capnp_list_get_struct(ops, i, err)
         if (err /= CAPNP_OK) return
         select case (promised_answer_op_which(op))
         case (PROMISED_ANSWER_OP_NOOP_TAG)
         case (PROMISED_ANSWER_OP_GET_POINTER_FIELD_TAG)
            p = capnp_getp(p, int(promised_answer_op_get_pointer_field_get(op)), err)
            if (err /= CAPNP_OK) then
               call abort_conn(conn, back, err)
               return
            end if
         case default
            call abort_conn(conn, 'Unsupported pipeline op.', err)
            return
         end select
      end do
      if (p%kind /= CAPNP_PK_CAP) then
         call abort_conn(conn, back, err)
         return
      end if
      ctab = payload_cap_table_get(apl, err)
      if (err /= CAPNP_OK) then
         call abort_conn(conn, back, err)
         return
      end if
      if (p%capidx < 0_int64 .or. p%capidx >= capnp_list_len(ctab)) then
         call abort_conn(conn, back, err)
         return
      end if
      cd%p = capnp_list_get_struct(ctab, int(p%capidx), err)
      if (err /= CAPNP_OK) return
      select case (cap_descriptor_which(cd))
      case (CAP_DESCRIPTOR_RECEIVER_HOSTED_TAG)
         call send_receiver_loopback_import(conn, embargo_id, &
                                            cap_descriptor_receiver_hosted_get(cd), err)
      case (CAP_DESCRIPTOR_RECEIVER_ANSWER_TAG)
         block
            type(promised_answer_t) :: src
            src = cap_descriptor_receiver_answer_get(cd, err)
            if (err /= CAPNP_OK) return
            call send_receiver_loopback_answer(conn, embargo_id, src, err)
         end block
      case default
         ! senderHosted and senderPromise are capabilities this vat
         ! hosts. Their brand is not this connection.
         call abort_conn(conn, back, err)
      end select
   end subroutine echo_promised_loopback

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
         if (id < 0_int64 .or. id >= int(MAXE, int64)) then
            call abort_conn(conn, 'Tried to release invalid export ID.', err)
            return
         end if
         if (.not. conn%exports(int(id))%used .or. &
             conn%exports(int(id))%refcount <= 0) then
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
