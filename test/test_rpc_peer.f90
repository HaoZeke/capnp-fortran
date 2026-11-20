!> A raw peer scripts each session and reads every frame back as text.
!> Each case replays a probe the differential checker runs against
!> C++ 1.4.0 (interop/rpc-diff), and expects the frames C++ sends.
module test_rpc_peer
   use testdrive, only: unittest_type, error_type, check
   use capnp
   use rpc_capnp
   use capnp_posix
   use capnp_rpc
   use capnp_rpc_transport
   use rpc_servers, only: echo_srv_t, ECHO_IFACE
   implicit none
   private

   public :: collect_rpc_peer

   !> Method 2 returns a new local export, and method 7 answers later.
   !> Any other method returns the caller's export 4 as receiverHosted,
   !> as a reflect call does.
   type, extends(rpc_server_t) :: mix_srv_t
      class(rpc_server_t), pointer :: inner => null()
   contains
      procedure :: dispatch => mix_dispatch
   end type mix_srv_t

   type(echo_srv_t), target :: made
   character(len=2), parameter :: NOCAPS(0) = [character(len=2) ::]
   integer(int64), parameter :: NOIDS(0) = [integer(int64) ::]

contains

   subroutine collect_rpc_peer(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      allocate (testsuite(29))
      testsuite(1)%name = "param-promise-merge-hosted"
      testsuite(1)%test => t_merge_hosted
      testsuite(2)%name = "param-promise-merge-promise"
      testsuite(2)%test => t_merge_promise
      testsuite(3)%name = "fwd-ra-released-param"
      testsuite(3)%test => t_fwd_ra_released
      testsuite(4)%name = "released-export-reuse-in-dispatch"
      testsuite(4)%test => t_reuse_in_dispatch
      testsuite(5)%name = "yourself-drops-opp"
      testsuite(5)%test => t_yourself_opp
      testsuite(6)%name = "npp-fwd-keeps-promise-import"
      testsuite(6)%test => t_npp_keeps_promise
      testsuite(7)%name = "finish-plain-fwd-npp"
      testsuite(7)%test => t_fin_plain_npp
      testsuite(8)%name = "finish-plain-fwd-yourself"
      testsuite(8)%test => t_fin_plain_yourself
      testsuite(9)%name = "fwd-ra-plain-pending"
      testsuite(9)%test => t_fwd_ra_plain
      testsuite(10)%name = "proxy-early-finish"
      testsuite(10)%test => t_proxy_early_finish
      testsuite(11)%name = "disembargo-plain-fwd"
      testsuite(11)%test => t_dis_plain
      testsuite(12)%name = "disembargo-plain-fwd-cap"
      testsuite(12)%test => t_dis_plain_cap
      testsuite(13)%name = "disembargo-plain-fwd-finish"
      testsuite(13)%test => t_dis_plain_finish
      testsuite(14)%name = "disembargo-plain-fwd-promise-cap"
      testsuite(14)%test => t_dis_plain_promise
      testsuite(15)%name = "fwd-ra-running-answer"
      testsuite(15)%test => t_fwd_ra_running
      testsuite(16)%name = "disembargo-running-answer-proxy"
      testsuite(16)%test => t_dis_ra_running
      testsuite(17)%name = "param-promise-resolve-to-live-import"
      testsuite(17)%test => t_live_import
      testsuite(18)%name = "yourself-npp-plain-fwd"
      testsuite(18)%test => t_yourself_npp
      testsuite(19)%name = "disembargo-plain-fwd-npp"
      testsuite(19)%test => t_dis_plain_npp
      testsuite(20)%name = "fwd-ra-pending-npp"
      testsuite(20)%test => t_fwd_ra_pending_npp
      testsuite(21)%name = "fwd-ra-returned-npp"
      testsuite(21)%test => t_fwd_ra_returned_npp
      testsuite(22)%name = "disembargo-plain-fwd-opp"
      testsuite(22)%test => t_dis_plain_opp
      testsuite(23)%name = "finish-plain-fwd-opp"
      testsuite(23)%test => t_fin_plain_opp
      testsuite(24)%name = "plain-fwd-opp-after-return"
      testsuite(24)%test => t_opp_after_return
      testsuite(25)%name = "fwd-ra-failed-answer"
      testsuite(25)%test => t_fwd_ra_failed
      testsuite(26)%name = "fwd-ra-unknown-answer"
      testsuite(26)%test => t_fwd_ra_unknown
      testsuite(27)%name = "fwd-rh-broken-proxy"
      testsuite(27)%test => t_fwd_rh_broken
      testsuite(28)%name = "fwd-rh-broken-promise-again"
      testsuite(28)%test => t_fwd_rh_broken_again
      testsuite(29)%name = "fwd-rh-npp-promise-again"
      testsuite(29)%test => t_fwd_rh_npp_again
   end subroutine collect_rpc_peer

   subroutine mix_dispatch(self, ctx, err)
      class(mix_srv_t), intent(inout) :: self
      type(rpc_call_ctx_t), intent(inout) :: ctx
      integer, intent(out) :: err
      type(capnp_ptr_t) :: s, ctab
      type(cap_descriptor_t) :: cd
      integer :: idx
      if (ctx%method_id == 7) then
         err = CAPNP_OK
         call rpc_ctx_defer(ctx)
         return
      end if
      s = capnp_new_struct(ctx%rmsg, 0, 1, err)
      if (err /= CAPNP_OK) return
      if (ctx%method_id == 2) then
         idx = rpc_ctx_export_cap(ctx, self%inner, err)
         if (err /= CAPNP_OK) return
         call capnp_setp(s, 0, rpc_make_cap_ptr(ctx%rmsg, idx), err)
         if (err == CAPNP_OK) call payload_content_set(ctx%results, s, err)
         return
      end if
      call capnp_setp(s, 0, rpc_make_cap_ptr(ctx%rmsg, 0), err)
      if (err == CAPNP_OK) call payload_content_set(ctx%results, s, err)
      if (err == CAPNP_OK) ctab = payload_cap_table_init(ctx%results, 1_int64, err)
      if (err == CAPNP_OK) cd%p = capnp_list_get_struct(ctab, 0, err)
      if (err == CAPNP_OK) call cap_descriptor_receiver_hosted_set(cd, 4_int64, err)
   end subroutine mix_dispatch

   !> A connection under test on `conn`, its raw peer on `fd`, with the
   !> bootstrap export 0 taken.
   subroutine open_peer(conn, fd, srv)
      type(rpc_conn_t), intent(inout), target :: conn
      integer, intent(out) :: fd
      class(rpc_server_t), pointer, intent(in) :: srv
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(bootstrap_t) :: b
      character(len=:), allocatable :: t
      integer :: fa, e
      call px_socketpair(fa, fd, e)
      call rpc_conn_init(conn, fa, srv)
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      b = message_bootstrap_init(msg, e)
      call bootstrap_question_id_set(b, 0_int64, e)
      call rpc_send_message(fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(conn, e)
      t = wtext(fd)
   end subroutine open_peer

   subroutine close_peer(conn, fd)
      type(rpc_conn_t), intent(inout), target :: conn
      integer, intent(in) :: fd
      call rpc_conn_close(conn)
      call px_close(fd)
   end subroutine close_peer

   !> Call `method` on export 0, or on pa(aq, [0]) when aq >= 0. The
   !> params point at cap 0 of the entries `ck` ('sh', 'sp', 'rh', or 'ra'
   !> for receiverAnswer(id, [0])) with ids `cid`.
   subroutine wcall(fd, q, method, aq, ck, cid, yourself, npp, opp)
      integer, intent(in) :: fd, method
      integer(int64), intent(in) :: q, aq
      character(len=2), intent(in) :: ck(:)
      integer(int64), intent(in) :: cid(:)
      logical, intent(in), optional :: yourself, npp, opp
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(call_t) :: c
      type(message_target_t) :: tgt
      type(promised_answer_t) :: pa
      type(payload_t) :: params
      type(capnp_ptr_t) :: ctab
      type(cap_descriptor_t) :: cd
      integer :: i, e
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      c = message_call_init(msg, e)
      call call_question_id_set(c, q, e)
      call call_interface_id_set(c, ECHO_IFACE, e)
      call call_method_id_set(c, int(method, int32), e)
      if (present(yourself)) then
         if (yourself) call call_send_results_to_yourself_set(c, e)
      end if
      if (present(npp)) call call_no_promise_pipelining_set(c, npp, e)
      if (present(opp)) call call_only_promise_pipeline_set(c, opp, e)
      tgt = call_target_init(c, e)
      if (aq < 0_int64) then
         call message_target_imported_cap_set(tgt, 0_int64, e)
      else
         pa = message_target_promised_answer_init(tgt, e)
         call set_answer(pa, aq)
      end if
      params = call_params_init(c, e)
      if (size(ck) == 0) then
         call payload_content_set(params, capnp_new_struct(m, 1, 0, e), e)
      else
         call payload_content_set(params, rpc_make_cap_ptr(m, 0), e)
         ctab = payload_cap_table_init(params, int(size(ck), int64), e)
         do i = 1, size(ck)
            cd%p = capnp_list_get_struct(ctab, i - 1, e)
            select case (ck(i))
            case ('sh')
               call cap_descriptor_sender_hosted_set(cd, cid(i), e)
            case ('sp')
               call cap_descriptor_sender_promise_set(cd, cid(i), e)
            case ('rh')
               call cap_descriptor_receiver_hosted_set(cd, cid(i), e)
            case ('ra')
               pa = cap_descriptor_receiver_answer_init(cd, e)
               call set_answer(pa, cid(i))
            end select
         end do
      end if
      call rpc_send_message(fd, m, e)
      call capnp_message_free(m)
   end subroutine wcall

   !> promisedAnswer(aq, [0]).
   subroutine set_answer(pa, aq)
      type(promised_answer_t), intent(in) :: pa
      integer(int64), intent(in) :: aq
      type(promised_answer_op_t) :: op
      type(capnp_ptr_t) :: ops
      integer :: e
      call promised_answer_question_id_set(pa, aq, e)
      ops = promised_answer_transform_init(pa, 1_int64, e)
      op%p = capnp_list_get_struct(ops, 0, e)
      call promised_answer_op_get_pointer_field_set(op, 0_int32, e)
   end subroutine set_answer

   !> Return for question `q`: results, with cap 0 senderHosted `cap`
   !> (senderPromise when `promise`) when given, or resultsSentElsewhere.
   subroutine wret(fd, q, elsewhere, cap, promise)
      integer, intent(in) :: fd
      integer(int64), intent(in) :: q
      logical, intent(in) :: elsewhere
      integer(int64), intent(in), optional :: cap
      logical, intent(in), optional :: promise
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(return_t) :: r
      type(payload_t) :: pl
      type(capnp_ptr_t) :: s, ctab
      type(cap_descriptor_t) :: cd
      integer :: e
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      r = message_return_init(msg, e)
      call return_answer_id_set(r, q, e)
      call return_release_param_caps_set(r, .false., e)
      if (elsewhere) then
         call return_results_sent_elsewhere_set(r, e)
      else
         pl = return_results_init(r, e)
         if (present(cap)) then
            s = capnp_new_struct(m, 0, 1, e)
            call capnp_setp(s, 0, rpc_make_cap_ptr(m, 0), e)
            call payload_content_set(pl, s, e)
            ctab = payload_cap_table_init(pl, 1_int64, e)
            cd%p = capnp_list_get_struct(ctab, 0, e)
            if (present(promise)) then
               if (promise) then
                  call cap_descriptor_sender_promise_set(cd, cap, e)
               else
                  call cap_descriptor_sender_hosted_set(cd, cap, e)
               end if
            else
               call cap_descriptor_sender_hosted_set(cd, cap, e)
            end if
         else
            call payload_content_set(pl, capnp_new_struct(m, 1, 0, e), e)
         end if
      end if
      call rpc_send_message(fd, m, e)
      call capnp_message_free(m)
   end subroutine wret

   subroutine wfin(fd, q, rrc)
      integer, intent(in) :: fd
      integer(int64), intent(in) :: q
      logical, intent(in) :: rrc
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(finish_t) :: f
      integer :: e
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      f = message_finish_init(msg, e)
      call finish_question_id_set(f, q, e)
      call finish_release_result_caps_set(f, rrc, e)
      call rpc_send_message(fd, m, e)
      call capnp_message_free(m)
   end subroutine wfin

   subroutine wrel(fd, id, n)
      integer, intent(in) :: fd
      integer(int64), intent(in) :: id, n
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(release_t) :: rel
      integer :: e
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      rel = message_release_init(msg, e)
      call release_id_set(rel, id, e)
      call release_reference_count_set(rel, n, e)
      call rpc_send_message(fd, m, e)
      call capnp_message_free(m)
   end subroutine wrel

   !> Resolve promise `pid` to senderHosted, senderPromise or
   !> receiverHosted `id` ('sh', 'sp', 'rh'), or to receiverAnswer(id,
   !> [0]) ('ra').
   subroutine wres(fd, pid, kind, id)
      integer, intent(in) :: fd
      integer(int64), intent(in) :: pid, id
      character(len=2), intent(in) :: kind
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(resolve_t) :: rv
      type(cap_descriptor_t) :: cd
      type(promised_answer_t) :: pa
      integer :: e
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      rv = message_resolve_init(msg, e)
      call resolve_promise_id_set(rv, pid, e)
      cd = resolve_cap_init(rv, e)
      select case (kind)
      case ('sh')
         call cap_descriptor_sender_hosted_set(cd, id, e)
      case ('sp')
         call cap_descriptor_sender_promise_set(cd, id, e)
      case ('rh')
         call cap_descriptor_receiver_hosted_set(cd, id, e)
      case ('ra')
         pa = cap_descriptor_receiver_answer_init(cd, e)
         call set_answer(pa, id)
      end select
      call rpc_send_message(fd, m, e)
      call capnp_message_free(m)
   end subroutine wres

   !> Disembargo senderLoopback `id` on pa(aq, [0]).
   subroutine wdis(fd, aq, id)
      integer, intent(in) :: fd
      integer(int64), intent(in) :: aq, id
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(disembargo_t) :: d
      type(message_target_t) :: tgt
      type(promised_answer_t) :: pa
      integer :: e
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      d = message_disembargo_init(msg, e)
      tgt = disembargo_target_init(d, e)
      pa = message_target_promised_answer_init(tgt, e)
      call set_answer(pa, aq)
      call disembargo_context_sender_loopback_set(d, id, e)
      call rpc_send_message(fd, m, e)
      call capnp_message_free(m)
   end subroutine wdis

   !> The next frame as text, '' after 200 ms with nothing:
   !> "CALL <q> I<id>|P<q> c|y [caps]", "RET <id> res [caps]|exc|can|else|
   !> take<q>", "FIN <q> T|F", "REL <id> <n>", "DIS I<id>|P<q> sl|rl
   !> <id>", "RES <id> cap|exc", "ABORT", or "TAG <n>". A Call that
   !> hints onlyPromisePipeline ends in " opp". A cap is sh<id>, sp<id>,
   !> rh<id>, ra<q> or no. `q` gets the question id of a Call.
   function wtext(fd, q) result(txt)
      integer, intent(in) :: fd
      integer(int64), intent(out), optional :: q
      character(len=:), allocatable :: txt
      type(capnp_message_t), target :: reply
      type(message_t) :: msg
      type(call_t) :: c
      type(return_t) :: r
      type(finish_t) :: f
      type(release_t) :: rel
      type(disembargo_t) :: d
      type(resolve_t) :: rv
      type(message_target_t) :: tgt
      character(len=64) :: buf
      logical :: ready
      integer :: e
      txt = ''
      if (present(q)) q = -1_int64
      call px_poll_in(fd, 200, ready, e)
      if (e /= CAPNP_OK .or. .not. ready) return
      call rpc_recv_message(fd, reply, e)
      if (e /= CAPNP_OK) return
      msg = message_read_root(reply, e)
      if (e /= CAPNP_OK) then
         call capnp_message_free(reply)
         return
      end if
      select case (message_which(msg))
      case (MESSAGE_CALL_TAG)
         c = message_call_get(msg, e)
         if (present(q)) q = call_question_id_get(c)
         tgt = call_target_get(c, e)
         write (buf, '(a,i0,a)') 'CALL ', call_question_id_get(c), ' '
         txt = trim(buf) // ' ' // target_text(tgt)
         if (call_send_results_to_which(c) == CALL_SEND_RESULTS_TO_YOURSELF_TAG) then
            txt = txt // ' y '
         else
            txt = txt // ' c '
         end if
         txt = txt // caps_text(call_params_get(c, e))
         if (call_only_promise_pipeline_get(c)) txt = txt // ' opp'
      case (MESSAGE_RETURN_TAG)
         r = message_return_get(msg, e)
         write (buf, '(a,i0)') 'RET ', return_answer_id_get(r)
         txt = trim(buf)
         select case (return_which(r))
         case (RETURN_RESULTS_TAG)
            txt = txt // ' res ' // caps_text(return_results_get(r, e))
         case (RETURN_EXCEPTION_TAG)
            txt = txt // ' exc'
         case (RETURN_CANCELED_TAG)
            txt = txt // ' can'
         case (RETURN_RESULTS_SENT_ELSEWHERE_TAG)
            txt = txt // ' else'
         case (RETURN_TAKE_FROM_OTHER_QUESTION_TAG)
            write (buf, '(a,i0)') ' take', return_take_from_other_question_get(r)
            txt = txt // trim(buf)
         end select
      case (MESSAGE_FINISH_TAG)
         f = message_finish_get(msg, e)
         write (buf, '(a,i0,a,l1)') 'FIN ', finish_question_id_get(f), ' ', &
            finish_release_result_caps_get(f)
         txt = trim(buf)
      case (MESSAGE_RELEASE_TAG)
         rel = message_release_get(msg, e)
         write (buf, '(a,i0,a,i0)') 'REL ', release_id_get(rel), ' ', &
            release_reference_count_get(rel)
         txt = trim(buf)
      case (MESSAGE_DISEMBARGO_TAG)
         d = message_disembargo_get(msg, e)
         tgt = disembargo_target_get(d, e)
         txt = 'DIS ' // target_text(tgt)
         select case (disembargo_context_which(d))
         case (DISEMBARGO_CONTEXT_SENDER_LOOPBACK_TAG)
            write (buf, '(a,i0)') ' sl ', disembargo_context_sender_loopback_get(d)
         case (DISEMBARGO_CONTEXT_RECEIVER_LOOPBACK_TAG)
            write (buf, '(a,i0)') ' rl ', disembargo_context_receiver_loopback_get(d)
         case default
            buf = ' other'
         end select
         txt = txt // trim(buf)
      case (MESSAGE_RESOLVE_TAG)
         rv = message_resolve_get(msg, e)
         write (buf, '(a,i0)') 'RES ', resolve_promise_id_get(rv)
         if (resolve_which(rv) == RESOLVE_EXCEPTION_TAG) then
            txt = trim(buf) // ' exc'
         else
            txt = trim(buf) // ' cap'
         end if
      case (MESSAGE_ABORT_TAG)
         txt = 'ABORT'
      case default
         write (buf, '(a,i0)') 'TAG ', message_which(msg)
         txt = trim(buf)
      end select
      call capnp_message_free(reply)
   end function wtext

   function target_text(tgt) result(txt)
      type(message_target_t), intent(in) :: tgt
      character(len=:), allocatable :: txt
      type(promised_answer_t) :: pa
      character(len=32) :: buf
      integer :: e
      if (message_target_which(tgt) == MESSAGE_TARGET_IMPORTED_CAP_TAG) then
         write (buf, '(a,i0)') 'I', message_target_imported_cap_get(tgt)
      else
         pa = message_target_promised_answer_get(tgt, e)
         write (buf, '(a,i0)') 'P', promised_answer_question_id_get(pa)
      end if
      txt = trim(buf)
   end function target_text

   function caps_text(pl) result(txt)
      type(payload_t), intent(in) :: pl
      character(len=:), allocatable :: txt
      type(capnp_ptr_t) :: ctab
      type(cap_descriptor_t) :: cd
      type(promised_answer_t) :: pa
      character(len=32) :: buf
      integer :: i, e
      txt = '['
      ctab = payload_cap_table_get(pl, e)
      if (e == CAPNP_OK) then
         do i = 0, int(capnp_list_len(ctab)) - 1
            cd%p = capnp_list_get_struct(ctab, i, e)
            select case (cap_descriptor_which(cd))
            case (CAP_DESCRIPTOR_SENDER_HOSTED_TAG)
               write (buf, '(a,i0)') 'sh', cap_descriptor_sender_hosted_get(cd)
            case (CAP_DESCRIPTOR_SENDER_PROMISE_TAG)
               write (buf, '(a,i0)') 'sp', cap_descriptor_sender_promise_get(cd)
            case (CAP_DESCRIPTOR_RECEIVER_HOSTED_TAG)
               write (buf, '(a,i0)') 'rh', cap_descriptor_receiver_hosted_get(cd)
            case (CAP_DESCRIPTOR_RECEIVER_ANSWER_TAG)
               pa = cap_descriptor_receiver_answer_get(cd, e)
               write (buf, '(a,i0)') 'ra', promised_answer_question_id_get(pa)
            case default
               buf = 'no'
            end select
            if (i > 0) txt = txt // ' '
            txt = txt // trim(buf)
         end do
      end if
      txt = txt // ']'
   end function caps_text

   !> Every frame until the peer is quiet, joined by ' | '.
   function drain(fd) result(txt)
      integer, intent(in) :: fd
      character(len=:), allocatable :: txt, t
      integer :: n
      txt = ''
      do n = 1, 16
         t = wtext(fd)
         if (len(t) == 0) exit
         if (len(txt) > 0) txt = txt // ' | '
         txt = txt // t
      end do
   end function drain

   !> Pump `conn` for each frame the peer sent, then read its replies.
   function step(conn, fd, n) result(txt)
      type(rpc_conn_t), intent(inout), target :: conn
      integer, intent(in) :: fd, n
      character(len=:), allocatable :: txt
      integer :: i, e
      do i = 1, n
         if (.not. rpc_conn_alive(conn)) exit
         call rpc_pump_once(conn, e)
      end do
      txt = drain(fd)
   end function step

   !> A param promise resolves to the hosted cap another call holds. C++
   !> counts both under one import, Released with both refs at the end.
   subroutine t_merge_hosted(error)
      type(error_type), allocatable, intent(out) :: error
      type(rpc_conn_t), target :: conn
      type(mix_srv_t), target :: mix
      class(rpc_server_t), pointer :: srv
      character(len=:), allocatable :: a, b, c, d
      integer :: fd
      srv => mix
      call open_peer(conn, fd, srv)
      call wcall(fd, 1_int64, 1, -1_int64, ['sh'], [6_int64])
      call wcall(fd, 2_int64, 1, -1_int64, ['sp'], [7_int64])
      a = step(conn, fd, 2)
      call wres(fd, 7_int64, 'sh', 6_int64)
      b = step(conn, fd, 1)
      call wfin(fd, 1_int64, .true.)
      c = step(conn, fd, 1)
      call wfin(fd, 2_int64, .true.)
      d = step(conn, fd, 1)
      call check(error, b == 'REL 7 1' .and. c == '' .and. d == 'REL 6 2', &
                 'merge hosted: got '//b//' / '//c//' / '//d)
      call close_peer(conn, fd)
   end subroutine t_merge_hosted

   !> A param promise resolves to another param promise, which then
   !> resolves to a hosted cap. The second Release counts both refs.
   subroutine t_merge_promise(error)
      type(error_type), allocatable, intent(out) :: error
      type(rpc_conn_t), target :: conn
      type(mix_srv_t), target :: mix
      class(rpc_server_t), pointer :: srv
      character(len=:), allocatable :: a, b, c, d, f
      integer :: fd
      srv => mix
      call open_peer(conn, fd, srv)
      call wcall(fd, 1_int64, 1, -1_int64, ['sp'], [6_int64])
      call wcall(fd, 2_int64, 1, -1_int64, ['sp'], [7_int64])
      a = step(conn, fd, 2)
      call wres(fd, 7_int64, 'sp', 6_int64)
      b = step(conn, fd, 1)
      call wres(fd, 6_int64, 'sh', 8_int64)
      c = step(conn, fd, 1)
      call wfin(fd, 1_int64, .true.)
      d = step(conn, fd, 1)
      call wfin(fd, 2_int64, .true.)
      f = step(conn, fd, 1)
      call check(error, b == 'REL 7 1' .and. c == 'REL 6 2' .and. d == '' .and. &
                 f == 'REL 8 1', 'merge promise: got '//b//' / '//c//' / '//d//' / '//f)
      call close_peer(conn, fd)
   end subroutine t_merge_promise

   !> A forwarded receiverAnswer param names a result cap the peer has
   !> released. The answer still pins it, and C++ exports it again.
   subroutine t_fwd_ra_released(error)
      type(error_type), allocatable, intent(out) :: error
      type(rpc_conn_t), target :: conn
      type(mix_srv_t), target :: mix
      class(rpc_server_t), pointer :: srv
      character(len=:), allocatable :: a, b
      integer :: fd
      mix%inner => made
      srv => mix
      call open_peer(conn, fd, srv)
      call wcall(fd, 1_int64, 2, -1_int64, NOCAPS, NOIDS)
      call wcall(fd, 2_int64, 5, -1_int64, NOCAPS, NOIDS)
      a = step(conn, fd, 2)
      call wrel(fd, 1_int64, 1_int64)
      a = step(conn, fd, 1)
      call wcall(fd, 5_int64, 0, 2_int64, ['ra'], [1_int64])
      b = step(conn, fd, 1)
      call check(error, index(b, ' I4 y [sh') > 0, 'released param: got '//b)
      call close_peer(conn, fd)
   end subroutine t_fwd_ra_released

   !> A call on the pipeline of an answer whose result cap the peer has
   !> released makes a new export while it runs. C++ has freed the id,
   !> so the new export takes it, and the Finish that releases result
   !> caps drops a ref of that new export.
   subroutine t_reuse_in_dispatch(error)
      type(error_type), allocatable, intent(out) :: error
      type(rpc_conn_t), target :: conn
      type(mix_srv_t), target :: mix, mid
      class(rpc_server_t), pointer :: srv
      character(len=:), allocatable :: a, b, c
      integer :: fd
      mid%inner => made
      mix%inner => mid
      srv => mix
      call open_peer(conn, fd, srv)
      call wcall(fd, 1_int64, 2, -1_int64, NOCAPS, NOIDS)
      a = step(conn, fd, 1)
      call wrel(fd, 1_int64, 1_int64)
      a = step(conn, fd, 1)
      call wcall(fd, 2_int64, 2, 1_int64, NOCAPS, NOIDS)
      b = step(conn, fd, 1)
      call wfin(fd, 1_int64, .true.)
      c = step(conn, fd, 1)
      call check(error, b == 'RET 2 res [sh1]' .and. c == '' .and. rpc_conn_alive(conn), &
                 'reuse in dispatch: got '//b//' / '//c)
      call close_peer(conn, fd)
   end subroutine t_reuse_in_dispatch

   !> C++ ignores onlyPromisePipeline on a yourself call, so the call it
   !> sends on does not hint it.
   subroutine t_yourself_opp(error)
      type(error_type), allocatable, intent(out) :: error
      type(rpc_conn_t), target :: conn
      type(mix_srv_t), target :: mix
      class(rpc_server_t), pointer :: srv
      character(len=:), allocatable :: a, b
      integer(int64) :: k
      integer :: fd, e
      srv => mix
      call open_peer(conn, fd, srv)
      call wcall(fd, 1_int64, 5, -1_int64, NOCAPS, NOIDS)
      a = step(conn, fd, 1)
      call wcall(fd, 3_int64, 0, 1_int64, NOCAPS, NOIDS, yourself=.true., opp=.true.)
      call rpc_pump_once(conn, e)
      b = wtext(fd, k)
      call check(error, index(b, ' I4 c []') > 0 .and. index(b, 'opp') == 0, &
                 'yourself opp: got '//b)
      call close_peer(conn, fd)
   end subroutine t_yourself_opp

   !> A noPromisePipelining call goes on as a plain call, and its Return
   !> names a promise that then resolves to our own export. C++ keeps the
   !> promise import the Return named (resolutionsAtReturnTime) until our
   !> answer's Finish, so no Release goes at the Resolve.
   subroutine t_npp_keeps_promise(error)
      type(error_type), allocatable, intent(out) :: error
      type(rpc_conn_t), target :: conn
      type(mix_srv_t), target :: mix
      class(rpc_server_t), pointer :: srv
      character(len=:), allocatable :: a, b, c, d
      integer(int64) :: k
      integer :: fd, e
      mix%inner => made
      srv => mix
      call open_peer(conn, fd, srv)
      call wcall(fd, 1_int64, 5, -1_int64, NOCAPS, NOIDS)
      call wcall(fd, 2_int64, 2, -1_int64, NOCAPS, NOIDS)
      a = step(conn, fd, 2)
      call wcall(fd, 3_int64, 0, 1_int64, NOCAPS, NOIDS, npp=.true.)
      call rpc_pump_once(conn, e)
      a = wtext(fd, k)
      call wret(fd, k, .false., 9_int64, promise=.true.)
      b = step(conn, fd, 1)
      call wres(fd, 9_int64, 'rh', 1_int64)
      c = step(conn, fd, 1)
      call wfin(fd, 3_int64, .true.)
      d = step(conn, fd, 1)
      call check(error, k >= 0_int64 .and. b == 'FIN '//itoa(k)//' F | RET 3 res [rh9]' .and. &
                 index(c, 'REL') == 0 .and. d == 'REL 9 1', 'npp keeps promise: got '//b//' / '//c//' / '//d)
      call close_peer(conn, fd)
   end subroutine t_npp_keeps_promise

   !> Finish on a call that went on as a plain call cancels it at once:
   !> the question is finished, and the caller gets a canceled Return, or
   !> resultsSentElsewhere first for a yourself call.
   subroutine fin_plain_case(error, npp, label)
      type(error_type), allocatable, intent(out) :: error
      logical, intent(in) :: npp
      character(len=*), intent(in) :: label
      type(rpc_conn_t), target :: conn
      type(mix_srv_t), target :: mix
      class(rpc_server_t), pointer :: srv
      character(len=:), allocatable :: a, b, c, want
      integer(int64) :: k
      integer :: fd, e
      srv => mix
      call open_peer(conn, fd, srv)
      call wcall(fd, 1_int64, 5, -1_int64, NOCAPS, NOIDS)
      a = step(conn, fd, 1)
      call wcall(fd, 3_int64, 0, 1_int64, NOCAPS, NOIDS, yourself=.not. npp, npp=npp)
      call rpc_pump_once(conn, e)
      a = wtext(fd, k)
      call wfin(fd, 3_int64, .true.)
      b = step(conn, fd, 1)
      call wret(fd, k, .false.)
      c = step(conn, fd, 1)
      if (npp) then
         want = 'FIN '//itoa(k)//' T | RET 3 can'
      else
         want = 'RET 3 else | FIN '//itoa(k)//' T'
      end if
      call check(error, k >= 0_int64 .and. b == want .and. c == '', &
                 label//': got '//b//' / '//c)
      call close_peer(conn, fd)
   end subroutine fin_plain_case

   subroutine t_fin_plain_npp(error)
      type(error_type), allocatable, intent(out) :: error
      call fin_plain_case(error, .true., 'finish plain npp')
   end subroutine t_fin_plain_npp

   subroutine t_fin_plain_yourself(error)
      type(error_type), allocatable, intent(out) :: error
      call fin_plain_case(error, .false., 'finish plain yourself')
   end subroutine t_fin_plain_yourself

   !> A forwarded receiverAnswer param names an answer whose call went on
   !> as a plain call and is still pending. C++ exports a hosted cap that
   !> keeps the import the plain call returns until it is released.
   subroutine t_fwd_ra_plain(error)
      type(error_type), allocatable, intent(out) :: error
      type(rpc_conn_t), target :: conn
      type(mix_srv_t), target :: mix
      class(rpc_server_t), pointer :: srv
      character(len=:), allocatable :: a, b, c, d, f
      integer(int64) :: k1, k2
      integer :: fd, e
      srv => mix
      call open_peer(conn, fd, srv)
      call wcall(fd, 1_int64, 5, -1_int64, NOCAPS, NOIDS)
      a = step(conn, fd, 1)
      call wcall(fd, 3_int64, 2, 1_int64, NOCAPS, NOIDS, yourself=.true.)
      call rpc_pump_once(conn, e)
      a = wtext(fd, k1)
      call wcall(fd, 5_int64, 0, 1_int64, ['ra'], [3_int64])
      call rpc_pump_once(conn, e)
      b = wtext(fd, k2)
      a = drain(fd)
      call wret(fd, k2, .true.)
      call wret(fd, k1, .false., 9_int64)
      c = step(conn, fd, 2)
      call wfin(fd, 5_int64, .true.)
      call wfin(fd, 3_int64, .true.)
      d = step(conn, fd, 2)
      call wrel(fd, 1_int64, 1_int64)
      f = step(conn, fd, 1)
      call check(error, index(b, ' I4 y [sh1]') > 0 .and. c == '' .and. &
                 d == 'FIN '//itoa(k2)//' F | RET 3 else | FIN '//itoa(k1)//' F' .and. &
                 f == 'REL 9 1', 'plain pending: got '//b//' / '//c//' / '//d//' / '//f)
      call close_peer(conn, fd)
   end subroutine t_fwd_ra_plain

   !> A Finish on a plain forward whose answer a forwarded receiverAnswer
   !> param still uses. The proxy keeps the question and its result cap.
   subroutine t_proxy_early_finish(error)
      type(error_type), allocatable, intent(out) :: error
      type(rpc_conn_t), target :: conn
      type(mix_srv_t), target :: mix
      class(rpc_server_t), pointer :: srv
      character(len=:), allocatable :: a, b, c, d, f, g
      integer(int64) :: k1, k2
      integer :: fd, e
      srv => mix
      call open_peer(conn, fd, srv)
      call wcall(fd, 1_int64, 5, -1_int64, NOCAPS, NOIDS)
      a = step(conn, fd, 1)
      call wcall(fd, 3_int64, 2, 1_int64, NOCAPS, NOIDS, yourself=.true.)
      call rpc_pump_once(conn, e)
      a = wtext(fd, k1)
      call wcall(fd, 5_int64, 0, 1_int64, ['ra'], [3_int64])
      call rpc_pump_once(conn, e)
      b = wtext(fd, k2)
      a = drain(fd)
      call wret(fd, k2, .true.)
      a = step(conn, fd, 1)
      call wfin(fd, 3_int64, .true.)
      c = step(conn, fd, 1)
      call wret(fd, k1, .false., 9_int64)
      d = step(conn, fd, 1)
      call wfin(fd, 5_int64, .true.)
      f = step(conn, fd, 1)
      call wrel(fd, 1_int64, 1_int64)
      g = step(conn, fd, 1)
      call check(error, index(b, ' I4 y [sh1]') > 0 .and. c == 'RET 3 else' .and. &
                 d == 'FIN '//itoa(k1)//' F' .and. f == 'FIN '//itoa(k2)//' F' .and. &
                 g == 'REL 9 1', 'proxy early finish: got '//b//' / '//c//' / '//d// &
                 ' / '//f//' / '//g)
      call close_peer(conn, fd)
   end subroutine t_proxy_early_finish

   !> A plain forward still running, with a Disembargo on its answer, and
   !> the first `kind` of three endings: a null result (1), a cap (2), or
   !> a Finish before the cap (3). C++ waits for the answer, then echoes
   !> on the cap or aborts on the null one.
   subroutine dis_plain_case(error, kind, label)
      type(error_type), allocatable, intent(out) :: error
      integer, intent(in) :: kind
      character(len=*), intent(in) :: label
      type(rpc_conn_t), target :: conn
      type(mix_srv_t), target :: mix
      class(rpc_server_t), pointer :: srv
      character(len=:), allocatable :: a, b, c, d
      integer(int64) :: k
      integer :: fd, e
      srv => mix
      call open_peer(conn, fd, srv)
      call wcall(fd, 1_int64, 5, -1_int64, NOCAPS, NOIDS)
      a = step(conn, fd, 1)
      call wcall(fd, 3_int64, 0, 1_int64, NOCAPS, NOIDS, yourself=.true.)
      call rpc_pump_once(conn, e)
      a = wtext(fd, k)
      call wdis(fd, 3_int64, 556_int64)
      b = step(conn, fd, 1)
      d = ''
      select case (kind)
      case (1)
         call wret(fd, k, .false.)
         c = step(conn, fd, 1)
         call check(error, k >= 0_int64 .and. b == '' .and. c == 'ABORT', &
                    label//': got '//b//' / '//c)
      case (2)
         call wret(fd, k, .false., 9_int64)
         c = step(conn, fd, 1)
         call wfin(fd, 3_int64, .true.)
         d = step(conn, fd, 1)
         call check(error, k >= 0_int64 .and. b == '' .and. c == 'DIS I9 rl 556' .and. &
                    d == 'RET 3 else | FIN '//itoa(k)//' F | REL 9 1', &
                    label//': got '//b//' / '//c//' / '//d)
      case default
         call wfin(fd, 3_int64, .true.)
         c = step(conn, fd, 1)
         call wret(fd, k, .false., 9_int64)
         d = step(conn, fd, 1)
         call check(error, k >= 0_int64 .and. b == '' .and. c == 'RET 3 else' .and. &
                    d == 'FIN '//itoa(k)//' F | DIS I9 rl 556 | REL 9 1', &
                    label//': got '//b//' / '//c//' / '//d)
      end select
      call close_peer(conn, fd)
   end subroutine dis_plain_case

   subroutine t_dis_plain(error)
      type(error_type), allocatable, intent(out) :: error
      call dis_plain_case(error, 1, 'disembargo plain')
   end subroutine t_dis_plain

   subroutine t_dis_plain_cap(error)
      type(error_type), allocatable, intent(out) :: error
      call dis_plain_case(error, 2, 'disembargo plain cap')
   end subroutine t_dis_plain_cap

   subroutine t_dis_plain_finish(error)
      type(error_type), allocatable, intent(out) :: error
      call dis_plain_case(error, 3, 'disembargo plain finish')
   end subroutine t_dis_plain_finish

   !> A Disembargo waits on a plain forward that returns a promise cap.
   !> C++ echoes it once that promise resolves, after the Release.
   subroutine t_dis_plain_promise(error)
      type(error_type), allocatable, intent(out) :: error
      type(rpc_conn_t), target :: conn
      type(mix_srv_t), target :: mix
      class(rpc_server_t), pointer :: srv
      character(len=:), allocatable :: a, b, c, d
      integer(int64) :: k
      integer :: fd, e
      srv => mix
      call open_peer(conn, fd, srv)
      call wcall(fd, 1_int64, 5, -1_int64, NOCAPS, NOIDS)
      a = step(conn, fd, 1)
      call wcall(fd, 3_int64, 0, 1_int64, NOCAPS, NOIDS, yourself=.true.)
      call rpc_pump_once(conn, e)
      a = wtext(fd, k)
      call wdis(fd, 3_int64, 556_int64)
      a = step(conn, fd, 1)
      call wret(fd, k, .false., 9_int64, promise=.true.)
      b = step(conn, fd, 1)
      call wres(fd, 9_int64, 'sh', 10_int64)
      c = step(conn, fd, 1)
      call wfin(fd, 3_int64, .true.)
      d = step(conn, fd, 1)
      call check(error, k >= 0_int64 .and. a == '' .and. b == '' .and. &
                 c == 'REL 9 1 | DIS I10 rl 556' .and. &
                 d == 'RET 3 else | FIN '//itoa(k)//' F | REL 10 1', &
                 'disembargo plain promise: got '//a//' / '//b//' / '//c//' / '//d)
      call close_peer(conn, fd)
   end subroutine t_dis_plain_promise

   !> A forwarded receiverAnswer param names an answer still running in
   !> this vat. C++ takes the cap from its queued pipeline, writes it as
   !> senderPromise, and resolves it when the call returns.
   subroutine t_fwd_ra_running(error)
      type(error_type), allocatable, intent(out) :: error
      type(rpc_conn_t), target :: conn
      type(mix_srv_t), target :: mix
      class(rpc_server_t), pointer :: srv
      type(rpc_call_ctx_t), pointer :: ctx
      type(capnp_ptr_t) :: s
      character(len=:), allocatable :: a, b, c
      integer(int64) :: k
      integer :: fd, e, idx
      mix%inner => made
      srv => mix
      call open_peer(conn, fd, srv)
      call wcall(fd, 1_int64, 5, -1_int64, NOCAPS, NOIDS)
      call wcall(fd, 2_int64, 7, -1_int64, NOCAPS, NOIDS)
      a = step(conn, fd, 2)
      call wcall(fd, 5_int64, 0, 1_int64, ['ra'], [2_int64])
      call rpc_pump_once(conn, e)
      b = wtext(fd, k)
      a = drain(fd)
      ctx => rpc_deferred_ctx(conn, 2_int64)
      if (.not. associated(ctx)) then
         call check(error, .false., 'running answer: call 2 is not deferred')
         call close_peer(conn, fd)
         return
      end if
      idx = rpc_ctx_export_cap(ctx, made, e)
      s = capnp_new_struct(ctx%rmsg, 0, 1, e)
      call capnp_setp(s, 0, rpc_make_cap_ptr(ctx%rmsg, idx), e)
      call payload_content_set(ctx%results, s, e)
      call rpc_deferred_return(conn, 2_int64, e)
      c = drain(fd)
      call check(error, index(b, ' I4 y [sp') > 0 .and. index(c, 'RET 2 res [sh') == 1 .and. &
                 index(c, ' | RES ') > 0 .and. index(c, ' cap') > 0, &
                 'running answer: got '//b//' / '//c)
      call close_peer(conn, fd)
   end subroutine t_fwd_ra_running

   !> A senderLoopback Disembargo names the proxy written for an answer
   !> still running. C++ waits for the answer to return, and fails once no
   !> message is left to read, since the answer is a cap of this vat.
   subroutine t_dis_ra_running(error)
      type(error_type), allocatable, intent(out) :: error
      type(rpc_conn_t), target :: conn
      type(mix_srv_t), target :: mix
      class(rpc_server_t), pointer :: srv
      type(rpc_call_ctx_t), pointer :: ctx
      type(capnp_ptr_t) :: s
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(disembargo_t) :: d
      type(message_target_t) :: tgt
      character(len=:), allocatable :: a, b, c, w
      integer(int64) :: k, pid
      integer :: fd, e, idx, j
      logical :: handled
      mix%inner => made
      srv => mix
      call open_peer(conn, fd, srv)
      call wcall(fd, 1_int64, 5, -1_int64, NOCAPS, NOIDS)
      call wcall(fd, 2_int64, 7, -1_int64, NOCAPS, NOIDS)
      a = step(conn, fd, 2)
      call wcall(fd, 5_int64, 0, 1_int64, ['ra'], [2_int64])
      call rpc_pump_once(conn, e)
      b = wtext(fd, k)
      a = drain(fd)
      pid = -1_int64
      j = index(b, ' I4 y [sp')
      if (j > 0) then
         pid = 0_int64
         j = j + 9
         do while (j <= len(b))
            if (b(j:j) < '0' .or. b(j:j) > '9') exit
            pid = 10_int64*pid + int(ichar(b(j:j)) - ichar('0'), int64)
            j = j + 1
         end do
      end if
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      d = message_disembargo_init(msg, e)
      tgt = disembargo_target_init(d, e)
      call message_target_imported_cap_set(tgt, pid, e)
      call disembargo_context_sender_loopback_set(d, 9_int64, e)
      call rpc_send_message(fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(conn, e)
      w = drain(fd)
      ctx => rpc_deferred_ctx(conn, 2_int64)
      if (.not. associated(ctx)) then
         call check(error, .false., 'disembargo running answer: call 2 is not deferred, got '//w)
         call close_peer(conn, fd)
         return
      end if
      idx = rpc_ctx_export_cap(ctx, made, e)
      s = capnp_new_struct(ctx%rmsg, 0, 1, e)
      call capnp_setp(s, 0, rpc_make_cap_ptr(ctx%rmsg, idx), e)
      call payload_content_set(ctx%results, s, e)
      call rpc_deferred_return(conn, 2_int64, e)
      c = drain(fd)
      call rpc_pump_poll(conn, 0, handled, e)
      a = drain(fd)
      call check(error, pid >= 0_int64 .and. w == '' .and. index(c, 'RET 2 res [sh') == 1 .and. &
                 index(c, ' | RES ') > 0 .and. a == 'ABORT' .and. allocated(conn%abort_reason), &
                 'disembargo running answer: got '//b//' / '//w//' / '//c//' / '//a)
      if (allocated(error)) then
         call close_peer(conn, fd)
         return
      end if
      call check(error, index(conn%abort_reason, 'does not point back') > 0, &
                 'disembargo running answer: reason '//conn%abort_reason)
      call close_peer(conn, fd)
   end subroutine t_dis_ra_running

   !> A param promise resolves to a cap a plain forward's answer imports.
   !> The call keeps that import, and one Release counts both refs.
   subroutine t_live_import(error)
      type(error_type), allocatable, intent(out) :: error
      type(rpc_conn_t), target :: conn
      type(mix_srv_t), target :: mix
      class(rpc_server_t), pointer :: srv
      character(len=:), allocatable :: a, b, c, d
      integer(int64) :: k
      integer :: fd, e
      srv => mix
      call open_peer(conn, fd, srv)
      call wcall(fd, 1_int64, 5, -1_int64, NOCAPS, NOIDS)
      a = step(conn, fd, 1)
      call wcall(fd, 3_int64, 2, 1_int64, NOCAPS, NOIDS, npp=.true.)
      call rpc_pump_once(conn, e)
      a = wtext(fd, k)
      call wret(fd, k, .false., 9_int64)
      a = step(conn, fd, 1)
      call wcall(fd, 6_int64, 1, -1_int64, ['sp'], [7_int64])
      a = step(conn, fd, 1)
      call wres(fd, 7_int64, 'sh', 9_int64)
      b = step(conn, fd, 1)
      call wfin(fd, 3_int64, .true.)
      c = step(conn, fd, 1)
      call wfin(fd, 6_int64, .true.)
      d = step(conn, fd, 1)
      call check(error, b == 'REL 7 1' .and. c == '' .and. d == 'REL 9 2', &
                 'live import: got '//b//' / '//c//' / '//d)
      call close_peer(conn, fd)
   end subroutine t_live_import

   !> A yourself call with noPromisePipelining goes on as a plain call.
   !> C++ leaves its result chain unconsumed, so the question lives until
   !> Finish, and goes before resultsSentElsewhere.
   subroutine t_yourself_npp(error)
      type(error_type), allocatable, intent(out) :: error
      type(rpc_conn_t), target :: conn
      type(mix_srv_t), target :: mix
      class(rpc_server_t), pointer :: srv
      character(len=:), allocatable :: a, b, c
      integer(int64) :: k
      integer :: fd, e
      srv => mix
      call open_peer(conn, fd, srv)
      call wcall(fd, 1_int64, 5, -1_int64, NOCAPS, NOIDS)
      a = step(conn, fd, 1)
      call wcall(fd, 3_int64, 0, 1_int64, NOCAPS, NOIDS, yourself=.true., npp=.true.)
      call rpc_pump_once(conn, e)
      a = wtext(fd, k)
      call wret(fd, k, .false.)
      b = step(conn, fd, 1)
      call wfin(fd, 3_int64, .true.)
      c = step(conn, fd, 1)
      call check(error, k >= 0_int64 .and. b == '' .and. &
                 c == 'FIN '//itoa(k)//' F | RET 3 else', &
                 'yourself npp: got '//b//' / '//c)
      call close_peer(conn, fd)
   end subroutine t_yourself_npp

   !> A Disembargo on a plain forward that hinted noPromisePipelining.
   !> C++ aborts, as the disabled pipeline is broken.
   subroutine t_dis_plain_npp(error)
      type(error_type), allocatable, intent(out) :: error
      type(rpc_conn_t), target :: conn
      type(mix_srv_t), target :: mix
      class(rpc_server_t), pointer :: srv
      character(len=:), allocatable :: a, b
      integer(int64) :: k
      integer :: fd, e
      srv => mix
      call open_peer(conn, fd, srv)
      call wcall(fd, 1_int64, 5, -1_int64, NOCAPS, NOIDS)
      a = step(conn, fd, 1)
      call wcall(fd, 3_int64, 0, 1_int64, NOCAPS, NOIDS, npp=.true.)
      call rpc_pump_once(conn, e)
      a = wtext(fd, k)
      call wdis(fd, 3_int64, 556_int64)
      b = step(conn, fd, 1)
      call check(error, k >= 0_int64 .and. b == 'ABORT', 'disembargo plain npp: got '//b)
      call close_peer(conn, fd)
   end subroutine t_dis_plain_npp

   !> A forwarded receiverAnswer param on a noPromisePipelining answer.
   !> C++ writes the disabled pipeline, a broken senderPromise, and then
   !> resolves it to an exception.
   subroutine ra_npp_case(error, pending, label)
      type(error_type), allocatable, intent(out) :: error
      logical, intent(in) :: pending
      character(len=*), intent(in) :: label
      type(rpc_conn_t), target :: conn
      type(mix_srv_t), target :: mix
      class(rpc_server_t), pointer :: srv
      character(len=:), allocatable :: a, b, c
      integer(int64) :: k1, k2
      integer :: fd, e
      mix%inner => made
      srv => mix
      call open_peer(conn, fd, srv)
      call wcall(fd, 1_int64, 5, -1_int64, NOCAPS, NOIDS)
      a = step(conn, fd, 1)
      if (pending) then
         call wcall(fd, 3_int64, 2, 1_int64, NOCAPS, NOIDS, npp=.true.)
         call rpc_pump_once(conn, e)
         a = wtext(fd, k1)
      else
         call wcall(fd, 3_int64, 2, -1_int64, NOCAPS, NOIDS, npp=.true.)
         a = step(conn, fd, 1)
      end if
      call wcall(fd, 5_int64, 0, 1_int64, ['ra'], [3_int64])
      call rpc_pump_once(conn, e)
      b = wtext(fd, k2)
      c = drain(fd)
      call check(error, index(b, ' I4 y [sp') > 0 .and. &
                 index(c, 'RET 5 take'//itoa(k2)//' | RES ') == 1 .and. &
                 index(c, ' exc') == len(c) - 3, label//': got '//b//' / '//c)
      call close_peer(conn, fd)
   end subroutine ra_npp_case

   subroutine t_fwd_ra_pending_npp(error)
      type(error_type), allocatable, intent(out) :: error
      call ra_npp_case(error, .true., 'pending npp')
   end subroutine t_fwd_ra_pending_npp

   subroutine t_fwd_ra_returned_npp(error)
      type(error_type), allocatable, intent(out) :: error
      call ra_npp_case(error, .false., 'returned npp')
   end subroutine t_fwd_ra_returned_npp

   !> A plain forward that hinted both noPromisePipelining and
   !> onlyPromisePipeline goes out with sendForPipeline (rpc.c++ 2646). Its
   !> pipeline lives whatever the hint says, and C++ ignores the Return,
   !> so a Disembargo on the answer is echoed on the forwarded question.
   subroutine t_dis_plain_opp(error)
      type(error_type), allocatable, intent(out) :: error
      type(rpc_conn_t), target :: conn
      type(mix_srv_t), target :: mix
      class(rpc_server_t), pointer :: srv
      character(len=:), allocatable :: a, b, c
      integer(int64) :: k
      integer :: fd, e
      srv => mix
      call open_peer(conn, fd, srv)
      call wcall(fd, 1_int64, 5, -1_int64, NOCAPS, NOIDS)
      a = step(conn, fd, 1)
      call wcall(fd, 3_int64, 0, 1_int64, NOCAPS, NOIDS, npp=.true., opp=.true.)
      call rpc_pump_once(conn, e)
      a = wtext(fd, k)
      call wret(fd, k, .false., 9_int64)
      b = step(conn, fd, 1)
      call wdis(fd, 3_int64, 556_int64)
      c = step(conn, fd, 1)
      call check(error, k >= 0_int64 .and. b == '' .and. c == 'DIS P'//itoa(k)//' rl 556', &
                 'disembargo plain opp: got '//b//' / '//c)
      call close_peer(conn, fd)
   end subroutine t_dis_plain_opp

   !> A Finish on that plain forward before its Return. C++ sent the call
   !> with isAwaitingReturn false, so the Finish for the forwarded
   !> question has releaseResultCaps false.
   subroutine t_fin_plain_opp(error)
      type(error_type), allocatable, intent(out) :: error
      type(rpc_conn_t), target :: conn
      type(mix_srv_t), target :: mix
      class(rpc_server_t), pointer :: srv
      character(len=:), allocatable :: a, b
      integer(int64) :: k
      integer :: fd, e
      srv => mix
      call open_peer(conn, fd, srv)
      call wcall(fd, 1_int64, 5, -1_int64, NOCAPS, NOIDS)
      a = step(conn, fd, 1)
      call wcall(fd, 3_int64, 0, 1_int64, NOCAPS, NOIDS, npp=.true., opp=.true.)
      call rpc_pump_once(conn, e)
      a = wtext(fd, k)
      call wfin(fd, 3_int64, .true.)
      b = step(conn, fd, 1)
      call check(error, k >= 0_int64 .and. b == 'FIN '//itoa(k)//' F', &
                 'finish plain opp: got '//b)
      call close_peer(conn, fd)
   end subroutine t_fin_plain_opp

   !> Once the peer has returned a pipeline-only question, C++ sends later
   !> such calls as ordinary calls. Under noPromisePipelining the dropped
   !> response finishes the question at once.
   subroutine t_opp_after_return(error)
      type(error_type), allocatable, intent(out) :: error
      type(rpc_conn_t), target :: conn
      type(mix_srv_t), target :: mix
      class(rpc_server_t), pointer :: srv
      character(len=:), allocatable :: a, b, c
      integer(int64) :: k1, k2
      integer :: fd, e
      srv => mix
      call open_peer(conn, fd, srv)
      call wcall(fd, 1_int64, 5, -1_int64, NOCAPS, NOIDS)
      a = step(conn, fd, 1)
      call wcall(fd, 3_int64, 0, 1_int64, NOCAPS, NOIDS, npp=.true., opp=.true.)
      call rpc_pump_once(conn, e)
      a = wtext(fd, k1)
      call wret(fd, k1, .false.)
      a = step(conn, fd, 1)
      call wcall(fd, 5_int64, 0, 1_int64, NOCAPS, NOIDS, npp=.true., opp=.true.)
      call rpc_pump_once(conn, e)
      b = wtext(fd, k2)
      c = drain(fd)
      call check(error, k1 > 1000_int64 .and. k2 >= 0_int64 .and. k2 < 1000_int64 .and. &
                 b == 'CALL '//itoa(k2)//' I4 c [] opp' .and. c == 'FIN '//itoa(k2)//' T', &
                 'opp after return: got '//itoa(k1)//' / '//b//' / '//c)
      call close_peer(conn, fd)
   end subroutine t_opp_after_return

   !> A forwarded receiverAnswer param names an answer that failed. C++
   !> takes a broken cap from its pipeline and writes it as a new
   !> senderPromise export, which it then resolves to the exception.
   subroutine t_fwd_ra_failed(error)
      type(error_type), allocatable, intent(out) :: error
      type(rpc_conn_t), target :: conn
      type(mix_srv_t), target :: mix
      class(rpc_server_t), pointer :: srv
      character(len=:), allocatable :: a, b, c
      integer(int64) :: k
      integer :: fd, e
      srv => mix
      call open_peer(conn, fd, srv)
      call wcall(fd, 1_int64, 5, -1_int64, NOCAPS, NOIDS)
      call wcall(fd, 2_int64, 7, -1_int64, NOCAPS, NOIDS)
      a = step(conn, fd, 2)
      call rpc_deferred_return(conn, 2_int64, e, failed=.true., reason='boom')
      a = drain(fd)
      call wcall(fd, 5_int64, 0, 1_int64, ['ra'], [2_int64])
      call rpc_pump_once(conn, e)
      b = wtext(fd, k)
      c = drain(fd)
      call check(error, a == 'RET 2 exc' .and. index(b, ' I4 y [sp') > 0 .and. &
                 index(c, 'RES ') > 0 .and. index(c, ' exc') == len(c) - 3, &
                 'failed answer: got '//a//' / '//b//' / '//c)
      call close_peer(conn, fd)
   end subroutine t_fwd_ra_failed

   !> A forwarded receiverAnswer param names no answer. C++ writes the
   !> broken cap for "invalid 'receiverAnswer'" as a new promise export.
   subroutine t_fwd_ra_unknown(error)
      type(error_type), allocatable, intent(out) :: error
      type(rpc_conn_t), target :: conn
      type(mix_srv_t), target :: mix
      class(rpc_server_t), pointer :: srv
      character(len=:), allocatable :: a, b, c
      integer(int64) :: k
      integer :: fd, e
      srv => mix
      call open_peer(conn, fd, srv)
      call wcall(fd, 1_int64, 5, -1_int64, NOCAPS, NOIDS)
      a = step(conn, fd, 1)
      call wcall(fd, 5_int64, 0, 1_int64, ['ra'], [77_int64])
      call rpc_pump_once(conn, e)
      b = wtext(fd, k)
      c = drain(fd)
      call check(error, index(b, ' I4 y [sp') > 0 .and. index(c, 'RES ') > 0 .and. &
                 index(c, ' exc') == len(c) - 3, 'unknown answer: got '//b//' / '//c)
      call close_peer(conn, fd)
   end subroutine t_fwd_ra_unknown

   !> A forwarded receiverHosted param names our promise export that has
   !> resolved to an exception. C++ writes that broken cap as a new
   !> senderPromise export and resolves it to the exception.
   subroutine t_fwd_rh_broken(error)
      type(error_type), allocatable, intent(out) :: error
      type(rpc_conn_t), target :: conn
      type(mix_srv_t), target :: mix
      class(rpc_server_t), pointer :: srv
      character(len=:), allocatable :: a, b, c, d, f
      integer(int64) :: k, px
      integer :: fd, e, i
      srv => mix
      call open_peer(conn, fd, srv)
      call wcall(fd, 1_int64, 5, -1_int64, NOCAPS, NOIDS)
      call wcall(fd, 2_int64, 7, -1_int64, NOCAPS, NOIDS)
      a = step(conn, fd, 2)
      call wcall(fd, 5_int64, 0, 1_int64, ['ra'], [2_int64])
      call rpc_pump_once(conn, e)
      b = wtext(fd, k)
      a = drain(fd)
      i = index(b, '[sp')
      px = -1_int64
      if (i > 0) read (b(i + 3:index(b, ']') - 1), *) px
      call rpc_deferred_return(conn, 2_int64, e, failed=.true., reason='boom')
      c = drain(fd)
      call wcall(fd, 6_int64, 0, 1_int64, ['rh'], [px])
      call rpc_pump_once(conn, e)
      d = wtext(fd, k)
      f = drain(fd)
      call check(error, px >= 0_int64 .and. c == 'RET 2 exc | RES '//itoa(px)//' exc' .and. &
                 index(d, ' I4 y [sp') > 0 .and. index(d, '[sp'//itoa(px)//']') == 0 .and. &
                 index(f, 'RES ') > 0 .and. index(f, ' exc') == len(f) - 3, &
                 'broken proxy: got '//b//' / '//c//' / '//d//' / '//f)
      call close_peer(conn, fd)
   end subroutine t_fwd_rh_broken

   !> A forwarded receiverHosted param names the broken promise an earlier
   !> forward wrote. C++ writes that broken cap again as the same
   !> senderPromise export, with no second Resolve.
   subroutine t_fwd_rh_broken_again(error)
      type(error_type), allocatable, intent(out) :: error
      type(rpc_conn_t), target :: conn
      type(mix_srv_t), target :: mix
      class(rpc_server_t), pointer :: srv
      character(len=:), allocatable :: a, b, c, d, f
      integer(int64) :: k, px
      integer :: fd, e, i
      srv => mix
      call open_peer(conn, fd, srv)
      call wcall(fd, 1_int64, 5, -1_int64, NOCAPS, NOIDS)
      a = step(conn, fd, 1)
      call wcall(fd, 5_int64, 0, 1_int64, ['ra'], [77_int64])
      call rpc_pump_once(conn, e)
      b = wtext(fd, k)
      c = drain(fd)
      i = index(b, '[sp')
      px = -1_int64
      if (i > 0) read (b(i + 3:index(b, ']') - 1), *) px
      call wcall(fd, 6_int64, 0, 1_int64, ['rh'], [px])
      call rpc_pump_once(conn, e)
      d = wtext(fd, k)
      f = drain(fd)
      call check(error, px >= 0_int64 .and. index(c, 'RES '//itoa(px)//' ') > 0 .and. &
                 index(d, ' I4 y [sp'//itoa(px)//']') > 0 .and. index(f, 'RES ') == 0, &
                 'broken again: got '//b//' / '//c//' / '//d//' / '//f)
      call close_peer(conn, fd)
   end subroutine t_fwd_rh_broken_again

   !> The same with a receiverAnswer on a returned noPromisePipelining
   !> answer that has caps. C++ wraps its disabled pipeline after the
   !> Return, so the broken cap is a local promise, and writing it again
   !> makes a new senderPromise with its own Resolve.
   subroutine t_fwd_rh_npp_again(error)
      type(error_type), allocatable, intent(out) :: error
      type(rpc_conn_t), target :: conn
      type(mix_srv_t), target :: mix
      class(rpc_server_t), pointer :: srv
      character(len=:), allocatable :: a, b, c, d, f
      integer(int64) :: k, px, py
      integer :: fd, e, i
      mix%inner => made
      srv => mix
      call open_peer(conn, fd, srv)
      call wcall(fd, 1_int64, 5, -1_int64, NOCAPS, NOIDS)
      call wcall(fd, 2_int64, 2, -1_int64, NOCAPS, NOIDS, npp=.true.)
      a = step(conn, fd, 2)
      call wcall(fd, 5_int64, 0, 1_int64, ['ra'], [2_int64])
      call rpc_pump_once(conn, e)
      b = wtext(fd, k)
      c = drain(fd)
      i = index(b, '[sp')
      px = -1_int64
      if (i > 0) read (b(i + 3:index(b, ']') - 1), *) px
      call wcall(fd, 6_int64, 0, 1_int64, ['rh'], [px])
      call rpc_pump_once(conn, e)
      d = wtext(fd, k)
      f = drain(fd)
      i = index(d, '[sp')
      py = -1_int64
      if (i > 0) read (d(i + 3:index(d, ']') - 1), *) py
      call check(error, index(a, 'RET 2 res [sh') > 0 .and. px >= 0_int64 .and. &
                 index(c, 'RES '//itoa(px)//' ') > 0 .and. py >= 0_int64 .and. py /= px .and. &
                 index(f, 'RES '//itoa(py)//' ') > 0, &
                 'npp broken again: got '//a//' / '//b//' / '//c//' / '//d//' / '//f)
      call close_peer(conn, fd)
   end subroutine t_fwd_rh_npp_again

   function itoa(i) result(s)
      integer(int64), intent(in) :: i
      character(len=:), allocatable :: s
      character(len=24) :: buf
      write (buf, '(i0)') i
      s = trim(buf)
   end function itoa

end module test_rpc_peer
