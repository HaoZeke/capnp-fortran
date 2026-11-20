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

   !> Method 2 returns a new local export. Any other method returns the
   !> caller's export 4 as receiverHosted, as a reflect call does.
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
      allocate (testsuite(2))
      testsuite(1)%name = "param-promise-merge-hosted"
      testsuite(1)%test => t_merge_hosted
      testsuite(2)%name = "param-promise-merge-promise"
      testsuite(2)%test => t_merge_promise
   end subroutine collect_rpc_peer

   subroutine mix_dispatch(self, ctx, err)
      class(mix_srv_t), intent(inout) :: self
      type(rpc_call_ctx_t), intent(inout) :: ctx
      integer, intent(out) :: err
      type(capnp_ptr_t) :: s, ctab
      type(cap_descriptor_t) :: cd
      integer :: idx
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
   !> params point at cap 0 of the entries `ck` ('sh', 'sp', or 'ra' for
   !> receiverAnswer(id, [0])) with ids `cid`.
   subroutine wcall(fd, q, method, aq, ck, cid, yourself, npp)
      integer, intent(in) :: fd, method
      integer(int64), intent(in) :: q, aq
      character(len=2), intent(in) :: ck(:)
      integer(int64), intent(in) :: cid(:)
      logical, intent(in), optional :: yourself, npp
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
   !> when given, or resultsSentElsewhere.
   subroutine wret(fd, q, elsewhere, cap)
      integer, intent(in) :: fd
      integer(int64), intent(in) :: q
      logical, intent(in) :: elsewhere
      integer(int64), intent(in), optional :: cap
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
            call cap_descriptor_sender_hosted_set(cd, cap, e)
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

   !> Resolve promise `pid` to senderHosted or senderPromise `id`
   !> ('sh', 'sp'), or to receiverAnswer(id, [0]) ('ra').
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
   !> <id>", "ABORT", or "TAG <n>". A cap is sh<id>, sp<id>, rh<id>,
   !> ra<q> or no. `q` gets the question id of a Call.
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

end module test_rpc_peer
