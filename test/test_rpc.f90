!> Two-party RPC over a socketpair, both vats in one process, pumped in
!> lockstep: bootstrap, pipelined and settled calls, capability results,
!> exceptions, finish/release bookkeeping, and the unimplemented reply.
module test_rpc
   use testdrive, only: unittest_type, error_type, check, test_failed, skip_test
   use iso_c_binding, only: c_int, c_funptr, c_funloc
   use capnp
   use capnp_any, only: capability_client_t
   use rpc_capnp
   ! The vat speaks the three-party network layer, which names a third
   ! vat and carries the join keys; see capnp_rpc.
   use rpc_threeparty_capnp
   use capnp_posix
   use capnp_rpc
   use capnp_rpc_transport
   use rpc_servers
   implicit none

   private
   public :: collect_rpc

   type, extends(rpc_membrane_policy_t) :: counting_membrane_policy_t
      integer :: inbound_calls = 0
      integer :: outbound_calls = 0
      logical :: block = .false.
   contains
      procedure :: inbound_call => counting_inbound_call
      procedure :: outbound_call => counting_outbound_call
   end type counting_membrane_policy_t

   type, extends(rpc_byte_stream_t) :: fail_stream_t
      integer :: accept_sends = 0
   contains
      procedure :: send_all => fail_stream_send
      procedure :: recv_all => fail_stream_recv
      procedure :: poll_in => fail_stream_poll
      procedure :: close => fail_stream_close
   end type fail_stream_t

   type(rpc_conn_t), target :: cli, srv
   type(echo_srv_t), target :: echo
   class(rpc_server_t), pointer :: boot
   integer :: fda, fdb, err

   integer(c_int), parameter :: SIGALRM = 14_c_int

   interface
      function c_fork() bind(c, name='fork') result(pid)
         import :: c_int
         integer(c_int) :: pid
      end function c_fork

      function c_alarm(seconds) bind(c, name='alarm') result(previous)
         import :: c_int
         integer(c_int), value :: seconds
         integer(c_int) :: previous
      end function c_alarm

      function c_sleep(seconds) bind(c, name='sleep') result(remaining)
         import :: c_int
         integer(c_int), value :: seconds
         integer(c_int) :: remaining
      end function c_sleep

      function c_signal(signum, handler) bind(c, name='signal') result(previous)
         import :: c_int, c_funptr
         integer(c_int), value :: signum
         type(c_funptr), value :: handler
         type(c_funptr) :: previous
      end function c_signal

      function c_siginterrupt(signum, flag) bind(c, name='siginterrupt') result(rc)
         import :: c_int
         integer(c_int), value :: signum, flag
         integer(c_int) :: rc
      end function c_siginterrupt

      function c_waitpid(pid, status, options) bind(c, name='waitpid') result(waited)
         import :: c_int
         integer(c_int), value :: pid, options
         integer(c_int), intent(out) :: status
         integer(c_int) :: waited
      end function c_waitpid

      subroutine c_exit(status) bind(c, name='_exit')
         import :: c_int
         integer(c_int), value :: status
      end subroutine c_exit
   end interface


contains

   subroutine collect_rpc(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      ! One ordered suite: cases share a socketpair connection (setup once).
      allocate (testsuite(7))
      testsuite(1)%name = "rpc"
      testsuite(1)%test => run_rpc
      testsuite(2)%name = "posix-recv-eintr-retried"
      testsuite(2)%test => t_recv_all_retries_eintr
      testsuite(3)%name = "rpc-framing-511-accepted"
      testsuite(3)%test => t_framing_511_accepted
      testsuite(4)%name = "rpc-framing-512-rejected"
      testsuite(4)%test => t_framing_512_rejected
      testsuite(5)%name = "rpc-framing-oversized-rejected"
      testsuite(5)%test => t_framing_oversized_rejected
      testsuite(6)%name = "rpc-framing-negative-traversal-rejected"
      testsuite(6)%test => t_framing_negative_traversal_rejected
      testsuite(7)%name = "rpc-framing-short-and-padded"
      testsuite(7)%test => t_framing_short_and_padded
   end subroutine collect_rpc

   subroutine check_(error, cond, name)
      type(error_type), allocatable, intent(inout) :: error
      logical, intent(in) :: cond
      character(len=*), intent(in) :: name
      if (allocated(error)) return
      call check(error, cond, name)
   end subroutine check_

   subroutine alarm_handler(signum) bind(c)
      integer(c_int), value :: signum
   end subroutine alarm_handler

   !> A signal that interrupts recv() must not turn a live socket into I/O
   !> failure. The child sends after SIGALRM has interrupted the parent.
   subroutine t_recv_all_retries_eintr(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int8) :: sent(0:3), received(0:3)
      integer :: fa, fb, e
      integer(c_int) :: pid, status, ignored, waited
      type(c_funptr) :: previous

      sent = [11_int8, 22_int8, 33_int8, 44_int8]
      received = -1_int8
      call px_socketpair(fa, fb, e)
      call check_(error, e == CAPNP_OK, 'posix EINTR: socketpair')
      if (allocated(error)) return

      pid = c_fork()
      call check_(error, pid >= 0_c_int, 'posix EINTR: fork')
      if (pid == 0_c_int) then
         ignored = c_sleep(2_c_int)
         call px_send_all(fa, sent, e)
         call px_close(fa)
         call px_close(fb)
         if (e == CAPNP_OK) then
            call c_exit(0_c_int)
         else
            call c_exit(1_c_int)
         end if
      end if
      if (.not. allocated(error)) then
         previous = c_signal(SIGALRM, c_funloc(alarm_handler))
         ignored = c_siginterrupt(SIGALRM, 1_c_int)
         ignored = c_alarm(1_c_int)
         call px_recv_all(fb, received, e)
         ignored = c_alarm(0_c_int)
         previous = c_signal(SIGALRM, previous)
         ignored = c_siginterrupt(SIGALRM, 0_c_int)
         call check_(error, e == CAPNP_OK, 'posix EINTR: recv retried')
         call check_(error, all(received == sent), 'posix EINTR: payload intact')
      end if
      waited = c_waitpid(pid, status, 0_c_int)
      call check_(error, waited == pid .and. status == 0_c_int, 'posix EINTR: child exits cleanly')
      call px_close(fa)
      call px_close(fb)
   end subroutine t_recv_all_retries_eintr

   subroutine counting_inbound_call(self, interface_id, method_id, err)
      class(counting_membrane_policy_t), intent(inout) :: self
      integer(int64), intent(in) :: interface_id
      integer, intent(in) :: method_id
      integer, intent(out) :: err
      self%inbound_calls = self%inbound_calls + 1
      if (method_id < 0) self%inbound_calls = self%inbound_calls + int(interface_id)
      if (self%block) then
         err = RPC_ERR_EXCEPTION
      else
         err = CAPNP_OK
      end if
   end subroutine counting_inbound_call

   subroutine counting_outbound_call(self, interface_id, method_id, err)
      class(counting_membrane_policy_t), intent(inout) :: self
      integer(int64), intent(in) :: interface_id
      integer, intent(in) :: method_id
      integer, intent(out) :: err
      self%outbound_calls = self%outbound_calls + 1
      if (method_id < 0) self%outbound_calls = self%outbound_calls + int(interface_id)
      if (self%block) then
         err = RPC_ERR_EXCEPTION
      else
         err = CAPNP_OK
      end if
   end subroutine counting_outbound_call

   subroutine run_rpc(error)
      type(error_type), allocatable, intent(out) :: error
      call px_socketpair(fda, fdb, err)
      call check_(error, err == CAPNP_OK, 'rpc: socketpair')
      if (allocated(error)) return
      boot => echo
      call rpc_conn_init(srv, fdb, boot)
      boot => null()
      call rpc_conn_init(cli, fda, boot)

      call t_bootstrap_and_echo(error)
      if (.not. allocated(error)) call t_cap_result_and_pipeline(error)
      if (.not. allocated(error)) call t_exception(error)
      if (.not. allocated(error)) call t_unimplemented(error)
      if (.not. allocated(error)) call t_persistent_save(error)
      if (.not. allocated(error)) call t_resolve_and_tail_calls(error)
      if (.not. allocated(error)) call t_sender_promise_import(error)
      if (.not. allocated(error)) call t_disembargo_echo(error)
      if (.not. allocated(error)) call t_disembargo_promised_answer(error)
      if (.not. allocated(error)) call t_disembargo_receiver_loopback_absorbed(error)
      if (.not. allocated(error)) call t_pump_poll(error)
      if (.not. allocated(error)) call t_provide_and_accept(error)
      if (.not. allocated(error)) call t_accept_unknown_nonce(error)
      if (.not. allocated(error)) call t_provide_unhosted(error)
      if (.not. allocated(error)) call t_two_provisions_independent(error)
      if (.not. allocated(error)) call t_accept_embargo(error)
      if (.not. allocated(error)) call t_third_party_introduction(error)
      if (.not. allocated(error)) call t_introduction_in_an_answer(error)
      if (.not. allocated(error)) call t_overlong_host_refused(error)
      if (.not. allocated(error)) call t_reference_introduction(error)
      if (.not. allocated(error)) call t_reference_frames(error)
      if (.not. allocated(error)) call t_join_same_capability(error)
      if (.not. allocated(error)) call t_join_unresolvable_part(error)
      if (.not. allocated(error)) call t_join_all_parts_unresolvable(error)
      if (.not. allocated(error)) call t_join_client_api(error)
      if (.not. allocated(error)) call t_join_incomplete_set_is_silent(error)
      if (.not. allocated(error)) call t_join_send_unwinds(error)
      if (.not. allocated(error)) call t_join_finish_releases_cap(error)
      if (.not. allocated(error)) call t_abandoned_join_cleared(error)
      if (.not. allocated(error)) call t_finish_provide_qid(error)
      if (.not. allocated(error)) call t_request_cancellation(error)
      if (.not. allocated(error)) call t_cancel_reserves_qid(error)
      if (.not. allocated(error)) call t_send_failure_releases_qid(error)
      if (.not. allocated(error)) call t_membrane(error)
      if (.not. allocated(error)) call t_export_table_full(error)
      if (.not. allocated(error)) call t_pluggable_transport(error)
      if (.not. allocated(error)) call t_dial_guard(error)

      call rpc_conn_close(cli)
      call rpc_conn_close(srv)
   end subroutine run_rpc


   !> Send one Join part: question `qid`, targeting export `eid`, carrying
   !> a two-party JoinKeyPart naming the set.
   subroutine send_join_part(qid, eid, jid, pcount, pnum, e)
      integer(int64), intent(in) :: qid, jid
      integer, intent(in) :: eid, pcount, pnum
      integer, intent(out) :: e
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(join_t) :: jn
      type(message_target_t) :: tgt
      type(join_key_part_t) :: kp
      call capnp_message_init_builder(m, e)
      if (e /= CAPNP_OK) return
      msg = message_new_root(m, e)
      if (e /= CAPNP_OK) return
      jn = message_join_init(msg, e)
      if (e /= CAPNP_OK) return
      call join_question_id_set(jn, qid, e)
      if (e /= CAPNP_OK) return
      tgt = join_target_init(jn, e)
      if (e /= CAPNP_OK) return
      call message_target_imported_cap_set(tgt, int(eid, int64), e)
      if (e /= CAPNP_OK) return
      kp = join_key_part_new(m, e)
      if (e /= CAPNP_OK) return
      call join_key_part_join_id_set(kp, jid, e)
      if (e /= CAPNP_OK) return
      call join_key_part_part_count_set(kp, pcount, e)
      if (e /= CAPNP_OK) return
      call join_key_part_part_num_set(kp, pnum, e)
      if (e /= CAPNP_OK) return
      call join_key_part_set(jn, kp%p, e)
      if (e /= CAPNP_OK) return
      call rpc_send_message(cli%fd, m, e)
      call capnp_message_free(m)
   end subroutine send_join_part

   subroutine send_finish(qid, e)
      integer(int64), intent(in) :: qid
      integer, intent(out) :: e
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(finish_t) :: f
      call capnp_message_init_builder(m, e)
      if (e /= CAPNP_OK) return
      msg = message_new_root(m, e)
      if (e /= CAPNP_OK) return
      f = message_finish_init(msg, e)
      if (e == CAPNP_OK) call finish_question_id_set(f, qid, e)
      if (e == CAPNP_OK) call finish_release_result_caps_set(f, .true., e)
      if (e == CAPNP_OK) call finish_require_early_cancellation_workaround_set(f, .false., e)
      if (e == CAPNP_OK) call rpc_send_message(cli%fd, m, e)
      call capnp_message_free(m)
   end subroutine send_finish

   !> Read one Return off the client socket and unpack its JoinResult.
   subroutine recv_join_result(ansid, jid, succeeded, has_cap, e)
      integer(int64), intent(out) :: ansid, jid
      logical, intent(out) :: succeeded, has_cap
      integer, intent(out) :: e
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(return_t) :: r
      type(payload_t) :: pl
      type(capnp_ptr_t) :: content
      type(capability_client_t) :: capp
      type(join_result_t) :: jr
      ansid = -1_int64
      jid = -1_int64
      succeeded = .false.
      has_cap = .false.
      call rpc_recv_message(cli%fd, m, e)
      if (e /= CAPNP_OK) return
      msg = message_read_root(m, e)
      if (e /= CAPNP_OK) return
      if (message_which(msg) /= MESSAGE_RETURN_TAG) then
         e = CAPNP_ERR_KIND
         call capnp_message_free(m)
         return
      end if
      r = message_return_get(msg, e)
      if (e /= CAPNP_OK) return
      ansid = return_answer_id_get(r)
      pl = return_results_get(r, e)
      if (e /= CAPNP_OK) return
      content = payload_content_get(pl, e)
      if (e /= CAPNP_OK) return
      jr%p = content
      jid = join_result_join_id_get(jr)
      succeeded = join_result_succeeded_get(jr)
      capp = join_result_cap_get(jr, e)
      if (e == CAPNP_OK) has_cap = capp%kind == RPC_CAP_IMPORT
      e = CAPNP_OK
      call capnp_message_free(m)
   end subroutine recv_join_result

   !> Find up to two distinct live exports on the server.
   subroutine two_live_exports(a, b, n)
      integer, intent(out) :: a, b, n
      integer :: i
      a = -1
      b = -1
      n = 0
      do i = 0, size(srv%exports) - 1
         if (srv%exports(i)%used) then
            if (a < 0) then
               a = i
            else if (b < 0) then
               b = i
               n = 2
               return
            end if
         end if
      end do
      if (a >= 0) n = 1
   end subroutine two_live_exports

   !> Carol -> us: claim the capability under `nonce`, but hold the
   !> answer until the introducer lifts the embargo.
   subroutine send_accept_embargoed(qid, nonce, e)
      integer(int64), intent(in) :: qid, nonce
      integer, intent(out) :: e
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(accept_t) :: ac
      type(provision_id_t) :: pid
      call capnp_message_init_builder(m, e)
      if (e /= CAPNP_OK) return
      msg = message_new_root(m, e)
      if (e /= CAPNP_OK) return
      ac = message_accept_init(msg, e)
      if (e /= CAPNP_OK) return
      call accept_question_id_set(ac, qid, e)
      if (e /= CAPNP_OK) return
      call accept_embargo_set(ac, .true., e)
      if (e /= CAPNP_OK) return
      pid = provision_id_new(m, e)
      if (e /= CAPNP_OK) return
      call provision_id_nonce_set(pid, nonce, e)
      if (e /= CAPNP_OK) return
      call accept_provision_set(ac, pid%p, e)
      if (e /= CAPNP_OK) return
      call rpc_send_message(cli%fd, m, e)
      call capnp_message_free(m)
   end subroutine send_accept_embargoed

   !> Alice -> us: lift the embargo, naming her own Provide question.
   subroutine send_disembargo_provide(provide_qid, e)
      integer(int64), intent(in) :: provide_qid
      integer, intent(out) :: e
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(disembargo_t) :: d
      call capnp_message_init_builder(m, e)
      if (e /= CAPNP_OK) return
      msg = message_new_root(m, e)
      if (e /= CAPNP_OK) return
      d = message_disembargo_init(msg, e)
      if (e /= CAPNP_OK) return
      call disembargo_context_provide_set(d, provide_qid, e)
      if (e /= CAPNP_OK) return
      call rpc_send_message(cli%fd, m, e)
      call capnp_message_free(m)
   end subroutine send_disembargo_provide

   !> An embargoed Accept claims the capability but is not answered until
   !> the introducer says the recipient's earlier calls have all arrived.
   !> Answering sooner would let a call sent straight to us overtake one
   !> still in flight through the introducer.
   subroutine t_accept_embargo(error)
      type(error_type), allocatable, intent(inout) :: error
      integer :: a, b, n, kind, before
      integer(int64) :: ansid
      integer(int64), parameter :: nonce = 20816_int64
      logical :: is_exc, readable
      call two_live_exports(a, b, n)
      if (allocated(error)) return
      before = rpc_pending_provisions(srv)

      call send_provide(70_int64, a, nonce, err)
      call rpc_pump_once(srv, err)
      call recv_answer(ansid, is_exc, kind, err)
      call check_(error, err == CAPNP_OK .and. .not. is_exc, 'embargo: provide answered')

      call send_accept_embargoed(71_int64, nonce, err)
      call rpc_pump_once(srv, err)
      call px_poll_in(cli%fd, 50, readable, err)
      call check_(error, err == CAPNP_OK .and. .not. readable, &
                  'embargo: an embargoed Accept is not answered yet')
      call check_(error, rpc_embargoed_accepts(srv) == 1, 'embargo: one held')

      ! The arrangement is claimed, so a second Accept finds nothing.
      call send_accept(72_int64, nonce, err)
      call rpc_pump_once(srv, err)
      call recv_answer(ansid, is_exc, kind, err)
      call check_(error, err == CAPNP_OK .and. is_exc, &
                  'embargo: a claimed provision cannot be claimed again')

      ! A Disembargo naming a Provide we never received changes nothing.
      call send_disembargo_provide(999_int64, err)
      call rpc_pump_once(srv, err)
      call px_poll_in(cli%fd, 50, readable, err)
      call check_(error, err == CAPNP_OK .and. .not. readable, &
                  'embargo: unknown provide question ignored')
      call check_(error, rpc_embargoed_accepts(srv) == 1, 'embargo: still held')

      call send_disembargo_provide(70_int64, err)
      call rpc_pump_once(srv, err)
      call recv_answer(ansid, is_exc, kind, err)
      call check_(error, err == CAPNP_OK .and. ansid == 71_int64 .and. .not. is_exc, &
                  'embargo: lifted, the Accept is answered')
      call check_(error, kind == CAPNP_PK_CAP, 'embargo: capability handed over')
      call check_(error, rpc_embargoed_accepts(srv) == 0, 'embargo: cleared')
      call check_(error, rpc_pending_provisions(srv) == before, &
                  'embargo: provision consumed')
   end subroutine t_accept_embargo

   ! ------------------------------------------------------------------
   ! Level 3, receiving half: introductions and vines
   ! ------------------------------------------------------------------

   !> Alice -> us: a Call whose params name a capability hosted by a
   !> third vat, with a vine we can use in the meantime.
   subroutine send_call_with_third_party_cap(qid, eid, host, port, nonce, vine, e)
      integer(int64), intent(in) :: qid, nonce, vine
      integer, intent(in) :: eid, port
      character(len=*), intent(in) :: host
      integer, intent(out) :: e
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(call_t) :: c
      type(message_target_t) :: tgt
      type(payload_t) :: params
      type(cap_descriptor_t) :: cd
      type(capnp_ptr_t) :: ctab
      call capnp_message_init_builder(m, e)
      if (e /= CAPNP_OK) return
      msg = message_new_root(m, e)
      if (e /= CAPNP_OK) return
      c = message_call_init(msg, e)
      if (e /= CAPNP_OK) return
      call call_question_id_set(c, qid, e)
      if (e /= CAPNP_OK) return
      tgt = call_target_init(c, e)
      if (e /= CAPNP_OK) return
      call message_target_imported_cap_set(tgt, int(eid, int64), e)
      if (e /= CAPNP_OK) return
      call call_interface_id_set(c, 4660_int64, e)
      if (e /= CAPNP_OK) return
      call call_method_id_set(c, 0, e)
      if (e /= CAPNP_OK) return
      params = call_params_init(c, e)
      if (e /= CAPNP_OK) return
      call payload_content_set(params, capnp_new_struct(m, 1, 0, e), e)
      if (e /= CAPNP_OK) return
      ctab = payload_cap_table_init(params, 1_int64, e)
      if (e /= CAPNP_OK) return
      cd%p = capnp_list_get_struct(ctab, 0, e)
      if (e /= CAPNP_OK) return
      call rpc_write_third_party_cap(cd, host, port, nonce, vine, e)
      if (e /= CAPNP_OK) return
      call rpc_send_message(cli%fd, m, e)
      call capnp_message_free(m)
   end subroutine send_call_with_third_party_cap

   !> Whether the next frame on the client socket is a Release naming
   !> `id`. Consumes one frame if there is one.
   !>
   !> Polls first: a missing frame is the interesting failure here, and
   !> reading a socket that will never speak would hang the suite instead
   !> of failing it.
   function next_is_release_of(id, e) result(match)
      integer(int64), intent(in) :: id
      integer, intent(out) :: e
      logical :: match, readable
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(release_t) :: rel
      match = .false.
      call px_poll_in(cli%fd, 50, readable, e)
      if (e /= CAPNP_OK .or. .not. readable) return
      call rpc_recv_message(cli%fd, m, e)
      if (e /= CAPNP_OK) return
      msg = message_read_root(m, e)
      if (e /= CAPNP_OK) return
      if (message_which(msg) == MESSAGE_RELEASE_TAG) then
         rel = message_release_get(msg, e)
         if (e == CAPNP_OK) match = release_id_get(rel) == id .and. &
                                    release_reference_count_get(rel) == 1_int64
      end if
      e = CAPNP_OK
      call capnp_message_free(m)
   end function next_is_release_of

   !> A payload naming a third party's capability is recorded as an
   !> introduction, and the vine survives until the pickup is finished.
   subroutine t_third_party_introduction(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_introduction_t) :: got(4)
      integer :: kind, n
      integer(int64) :: ansid
      logical :: is_exc
      call check_(error, rpc_pending_introductions(srv) == 0, &
                  'intro: none held yet')
      if (allocated(error)) return

      call send_call_with_third_party_cap(60_int64, 0, '10.0.0.7', 5000, &
                                          int(z'ABCDEF', int64), 77_int64, err)
      call check_(error, err == CAPNP_OK, 'intro: call sent')
      call rpc_pump_once(srv, err)
      ! The call itself is answered; the introduction rides in its params.
      call recv_answer(ansid, is_exc, kind, err)
      n = rpc_pending_introductions(srv, got)
      call check_(error, n == 1, 'intro: one held')
      if (allocated(error)) return
      call check_(error, got(1)%nonce == int(z'ABCDEF', int64), 'intro: nonce')
      call check_(error, got(1)%vine_id == 77_int64, 'intro: vine')
      call check_(error, got(1)%port == 5000, 'intro: port')
      call check_(error, got(1)%host(1:got(1)%host_len) == '10.0.0.7', 'intro: host')

      ! Nothing is released while the pickup is outstanding: the vine is
      ! the only way to reach the capability until then.
      call rpc_introduction_done(srv, int(z'ABCDEF', int64), err)
      call check_(error, err == CAPNP_OK, 'intro: pickup finished')
      call check_(error, next_is_release_of(77_int64, err) .and. err == CAPNP_OK, &
                  'intro: vine released on pickup')
      call check_(error, rpc_pending_introductions(srv) == 0, 'intro: cleared')

      ! Finishing an introduction nobody handed us is refused, even while
      ! another is outstanding: the nonce picks the arrangement, not the
      ! fact that there is one.
      call send_call_with_third_party_cap(61_int64, 0, '10.0.0.8', 5001, &
                                          153_int64, 78_int64, err)
      call rpc_pump_once(srv, err)
      call recv_answer(ansid, is_exc, kind, err)
      call check_(error, rpc_pending_introductions(srv) == 1, 'intro: second held')
      call rpc_introduction_done(srv, int(z'ABCDEF', int64), err)
      call check_(error, err /= CAPNP_OK, 'intro: unknown nonce refused')
      call check_(error, rpc_pending_introductions(srv) == 1, 'intro: other stands')
      call rpc_introduction_done(srv, 153_int64, err)
      call check_(error, err == CAPNP_OK, 'intro: arranged nonce finishes')
      call check_(error, next_is_release_of(78_int64, err), 'intro: second vine released')
   end subroutine t_third_party_introduction

   !> The introducer can also hand us the descriptor in an answer, not
   !> only in a call's params. Sent to the client vat, which asked the
   !> question, so it lands in handle_return.
   subroutine send_return_with_third_party_cap(qid, host, port, nonce, vine, e)
      integer(int64), intent(in) :: qid, nonce, vine
      integer, intent(in) :: port
      character(len=*), intent(in) :: host
      integer, intent(out) :: e
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(return_t) :: r
      type(payload_t) :: results
      type(cap_descriptor_t) :: cd
      type(capnp_ptr_t) :: ctab
      call capnp_message_init_builder(m, e)
      if (e /= CAPNP_OK) return
      msg = message_new_root(m, e)
      if (e /= CAPNP_OK) return
      r = message_return_init(msg, e)
      if (e /= CAPNP_OK) return
      call return_answer_id_set(r, qid, e)
      if (e /= CAPNP_OK) return
      results = return_results_init(r, e)
      if (e /= CAPNP_OK) return
      call payload_content_set(results, capnp_new_struct(m, 1, 0, e), e)
      if (e /= CAPNP_OK) return
      ctab = payload_cap_table_init(results, 1_int64, e)
      if (e /= CAPNP_OK) return
      cd%p = capnp_list_get_struct(ctab, 0, e)
      if (e /= CAPNP_OK) return
      call rpc_write_third_party_cap(cd, host, port, nonce, vine, e)
      if (e /= CAPNP_OK) return
      call rpc_send_message(srv%fd, m, e)
      call capnp_message_free(m)
   end subroutine send_return_with_third_party_cap

   !> The same descriptor in an answer is recorded too.
   subroutine t_introduction_in_an_answer(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_introduction_t) :: got(4)
      type(rpc_cap_t) :: bootcap
      integer :: n, before
      before = rpc_pending_introductions(cli)
      call rpc_bootstrap_send(cli, bootcap, err)
      ! Let the server answer, and take that answer off the wire, so the
      ! hand-built Return below is what the client reads next and no
      ! later case inherits a half-finished exchange.
      call rpc_pump_once(srv, err)
      call rpc_pump_once(cli, err)
      call send_return_with_third_party_cap(bootcap%id, '10.0.0.9', 5002, &
                                           119_int64, 80_int64, err)
      call check_(error, err == CAPNP_OK, 'intro: answer sent')
      call rpc_pump_once(cli, err)
      n = rpc_pending_introductions(cli, got)
      call check_(error, n == before + 1, 'intro: answer introduction held')
      if (allocated(error)) return
      call check_(error, got(n)%nonce == 119_int64, 'intro: answer nonce')
      call check_(error, got(n)%vine_id == 80_int64, 'intro: answer vine')
      call check_(error, got(n)%host(1:got(n)%host_len) == '10.0.0.9', &
                  'intro: answer host')
   end subroutine t_introduction_in_an_answer

   !> A host too long to store is refused rather than truncated: a
   !> truncated address names a different vat.
   subroutine t_overlong_host_refused(error)
      type(error_type), allocatable, intent(inout) :: error
      character(len=MAXHOST + 8) :: host
      integer :: kind, before
      integer(int64) :: ansid
      logical :: is_exc
      host = repeat('h', len(host))
      before = rpc_pending_introductions(srv)
      call send_call_with_third_party_cap(62_int64, 0, host, 5000, &
                                          65261_int64, 79_int64, err)
      call rpc_pump_once(srv, err)
      call recv_answer(ansid, is_exc, kind, err)
      call check_(error, rpc_pending_introductions(srv) == before, &
                  'intro: overlong host refused')
   end subroutine t_overlong_host_refused

   !> The frame the reference encoder writes, rather than one this
   !> library built: a layout the writer and reader share but the wire
   !> format does not would pass every case above.
   subroutine t_reference_introduction(error)
      type(error_type), allocatable, intent(inout) :: error
      integer(int8), allocatable :: frame(:)
      type(rpc_introduction_t) :: got(4)
      integer :: kind, before, n
      integer(int64) :: ansid
      logical :: is_exc
      before = rpc_pending_introductions(srv)
      call capnp_read_file('test/fixtures/rpc-introduce.bin', frame, err)
      call check_(error, err == CAPNP_OK, 'frames: introduce golden readable')
      if (allocated(error)) return
      call px_send_all(cli%fd, frame, err)
      call rpc_pump_once(srv, err)
      ! The call names export 0, which is live, so it is answered too;
      ! the introduction rides in its params either way.
      call recv_answer(ansid, is_exc, kind, err)
      n = rpc_pending_introductions(srv, got)
      call check_(error, n == before + 1, 'frames: reference introduction recorded')
      if (allocated(error)) return
      call check_(error, got(n)%nonce == int(z'ABCDEF', int64), 'frames: reference nonce')
      call check_(error, got(n)%vine_id == 77_int64, 'frames: reference vine')
      call check_(error, got(n)%port == 5000, 'frames: reference port')
      call check_(error, got(n)%host(1:got(n)%host_len) == '10.0.0.7', &
                  'frames: reference host')
   end subroutine t_reference_introduction

   !> Level 3 driven by frames the reference `capnp` CLI encoded.
   !>
   !> Every other level 3 case here builds its own Provide and Accept, so
   !> it shows the vat agrees with this library's builder and nothing
   !> more: a layout both sides share but the wire format does not would
   !> pass all of them. These bytes come from the reference
   !> implementation (scripts/gen-rpc-frames.sh), so decoding them is the
   !> claim that a peer can hand this vat a capability.
   !>
   !> The frames say: hold export 0 for whoever presents 0xfeedface
   !> (question 42), then claim it (question 43).
   subroutine t_reference_frames(error)
      type(error_type), allocatable, intent(inout) :: error
      integer(int8), allocatable :: frame(:)
      integer :: a, b, n, kind, before
      integer(int64) :: ansid
      logical :: is_exc
      call two_live_exports(a, b, n)
      call check_(error, a == 0, 'frames: export 0 is the bootstrap capability')
      if (allocated(error)) return
      before = rpc_pending_provisions(srv)

      call capnp_read_file('test/fixtures/rpc-provide.bin', frame, err)
      ! A checked-in golden that will not open is a broken tree, not a
      ! reason to pass.
      call check_(error, err == CAPNP_OK, 'frames: provide golden readable')
      if (allocated(error)) return
      call px_send_all(cli%fd, frame, err)
      call rpc_pump_once(srv, err)
      call recv_answer(ansid, is_exc, kind, err)
      call check_(error, err == CAPNP_OK .and. ansid == 42_int64 .and. .not. is_exc, &
                  'frames: reference Provide answered')
      call check_(error, rpc_pending_provisions(srv) == before + 1, &
                  'frames: reference Provide recorded')

      call capnp_read_file('test/fixtures/rpc-accept.bin', frame, err)
      call check_(error, err == CAPNP_OK, 'frames: accept golden readable')
      if (allocated(error)) return
      call px_send_all(cli%fd, frame, err)
      call rpc_pump_once(srv, err)
      call recv_answer(ansid, is_exc, kind, err)
      call check_(error, err == CAPNP_OK .and. ansid == 43_int64 .and. .not. is_exc, &
                  'frames: reference Accept answered')
      ! The nonce the vat matched on is the one the CLI wrote, which is
      ! the whole point: both sides read the same field.
      call check_(error, kind == CAPNP_PK_CAP, 'frames: capability handed over')
      call check_(error, rpc_pending_provisions(srv) == before, &
                  'frames: reference Accept consumed the arrangement')
   end subroutine t_reference_frames

   !> Level 4: two parts naming one capability join successfully, and
   !> exactly one result carries the joined cap.
   subroutine t_join_same_capability(error)
      type(error_type), allocatable, intent(inout) :: error
      integer :: a, b, n
      integer(int64) :: ansid1, ansid2, jid1, jid2
      logical :: ok1, ok2, cap1, cap2
      call two_live_exports(a, b, n)
      call check_(error, n >= 1, 'join: server has a live export')
      if (allocated(error)) return

      call send_join_part(700_int64, a, 9_int64, 2, 0, err)
      call check_(error, err == CAPNP_OK, 'join: part 0 sent')
      call rpc_pump_once(srv, err)
      call check_(error, err == CAPNP_OK, 'join: server took part 0')
      ! An incomplete set is not answerable yet, so nothing comes back.
      call send_join_part(701_int64, a, 9_int64, 2, 1, err)
      call check_(error, err == CAPNP_OK, 'join: part 1 sent')
      call rpc_pump_once(srv, err)
      call check_(error, err == CAPNP_OK, 'join: server took part 1')
      if (allocated(error)) return

      call recv_join_result(ansid1, jid1, ok1, cap1, err)
      call check_(error, err == CAPNP_OK, 'join: first result read')
      call recv_join_result(ansid2, jid2, ok2, cap2, err)
      call check_(error, err == CAPNP_OK, 'join: second result read')
      if (allocated(error)) return

      call check_(error, jid1 == 9_int64 .and. jid2 == 9_int64, 'join: joinId echoed')
      call check_(error, ok1 .and. ok2, 'join: same capability succeeds')
      call check_(error, (ansid1 == 700_int64 .and. ansid2 == 701_int64) .or. &
                  (ansid1 == 701_int64 .and. ansid2 == 700_int64), &
                  'join: both questions answered')
      ! JoinResult: one of the results carries the capability.
      call check_(error, count([cap1, cap2]) == 1, 'join: exactly one result carries the cap')
   end subroutine t_join_same_capability

   !> A part naming a capability this vat does not host cannot be proven
   !> equal to anything, so the whole set fails and no result carries a
   !> capability. Uses an export id that is deliberately not live, which
   !> is deterministic regardless of what earlier cases left exported.
   subroutine t_join_unresolvable_part(error)
      type(error_type), allocatable, intent(inout) :: error
      integer :: a, b, n, dead
      integer(int64) :: ansid1, ansid2, jid1, jid2
      logical :: ok1, ok2, cap1, cap2
      call two_live_exports(a, b, n)
      call check_(error, n >= 1, 'join: server has a live export')
      if (allocated(error)) return
      dead = free_export_slot()
      call check_(error, dead >= 0, 'join: found an unused export id')
      if (allocated(error)) return

      call send_join_part(710_int64, a, 11_int64, 2, 0, err)
      call check_(error, err == CAPNP_OK, 'join: resolvable part sent')
      call rpc_pump_once(srv, err)
      call send_join_part(711_int64, dead, 11_int64, 2, 1, err)
      call check_(error, err == CAPNP_OK, 'join: unresolvable part sent')
      call rpc_pump_once(srv, err)
      if (allocated(error)) return

      call recv_join_result(ansid1, jid1, ok1, cap1, err)
      call check_(error, err == CAPNP_OK, 'join: mixed first result')
      call recv_join_result(ansid2, jid2, ok2, cap2, err)
      call check_(error, err == CAPNP_OK, 'join: mixed second result')
      if (allocated(error)) return

      call check_(error, jid1 == 11_int64 .and. jid2 == 11_int64, 'join: mixed joinId echoed')
      ! All JoinResults in a set carry the same verdict.
      call check_(error, .not. ok1 .and. .not. ok2, 'join: unresolvable part fails the set')
      call check_(error, .not. cap1 .and. .not. cap2, 'join: failed join carries no cap')
   end subroutine t_join_unresolvable_part

   !> Every part agrees, but they agree on naming nothing: equality has to
   !> be proven against a capability we host, not against absence.
   subroutine t_join_all_parts_unresolvable(error)
      type(error_type), allocatable, intent(inout) :: error
      integer :: dead
      integer(int64) :: ansid1, ansid2, jid1, jid2
      logical :: ok1, ok2, cap1, cap2
      dead = free_export_slot()
      call check_(error, dead >= 0, 'join: found an unused export id')
      if (allocated(error)) return

      call send_join_part(740_int64, dead, 17_int64, 2, 0, err)
      call check_(error, err == CAPNP_OK, 'join: dead part 0 sent')
      call rpc_pump_once(srv, err)
      call send_join_part(741_int64, dead, 17_int64, 2, 1, err)
      call check_(error, err == CAPNP_OK, 'join: dead part 1 sent')
      call rpc_pump_once(srv, err)
      if (allocated(error)) return

      call recv_join_result(ansid1, jid1, ok1, cap1, err)
      call check_(error, err == CAPNP_OK, 'join: all-dead first result')
      call recv_join_result(ansid2, jid2, ok2, cap2, err)
      call check_(error, err == CAPNP_OK, 'join: all-dead second result')
      if (allocated(error)) return

      call check_(error, .not. ok1 .and. .not. ok2, &
                  'join: agreeing on nothing is not a join')
      call check_(error, .not. cap1 .and. .not. cap2, 'join: no cap for a failed set')
   end subroutine t_join_all_parts_unresolvable

   !> The client API sends a complete remote Join and settles the one
   !> capability carried by its successful result set.
   subroutine t_join_client_api(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_cap_t) :: caps(2), joined
      integer(int64) :: qids(2)
      integer :: a, b, n
      logical :: succeeded
      call two_live_exports(a, b, n)
      call check_(error, n >= 1, 'join client: server has a live export')
      if (allocated(error)) return
      caps%kind = RPC_CAP_IMPORT
      caps%id = int(a, int64)
      call rpc_join_send(cli, caps, 19_int64, qids, err)
      call check_(error, err == CAPNP_OK .and. all(qids >= 0_int64), &
                  'join client: complete set sent')
      call rpc_pump_once(srv, err)
      call rpc_pump_once(srv, err)
      call rpc_join_wait(cli, qids, joined, succeeded, err)
      call check_(error, err == CAPNP_OK .and. succeeded, &
                  'join client: equal remote capabilities join')
      call check_(error, joined%kind == RPC_CAP_IMPORT .and. joined%id == int(a, int64), &
                  'join client: joined capability settles')
      call rpc_pump_once(srv, err)
      call rpc_pump_once(srv, err)
   end subroutine t_join_client_api

   !> An export id the server is not using.
   function free_export_slot() result(idx)
      integer :: idx, i
      idx = -1
      do i = size(srv%exports) - 1, 0, -1
         if (.not. srv%exports(i)%used) then
            idx = i
            return
         end if
      end do
   end function free_export_slot

   !> A set that never completes is never answered: the receiver waits for
   !> every part before it can compare them.
   subroutine t_join_incomplete_set_is_silent(error)
      type(error_type), allocatable, intent(inout) :: error
      integer :: a, b, n
      integer(int64) :: ansid, jid
      logical :: succeeded, has_cap
      logical :: readable
      call two_live_exports(a, b, n)
      if (allocated(error)) return

      call send_join_part(720_int64, a, 13_int64, 3, 0, err)
      call check_(error, err == CAPNP_OK, 'join: lone part sent')
      call rpc_pump_once(srv, err)
      call check_(error, err == CAPNP_OK, 'join: server took the lone part')
      if (allocated(error)) return

      call px_poll_in(cli%fd, 50, readable, err)
      call check_(error, err == CAPNP_OK, 'join: poll for a premature reply')
      call check_(error, .not. readable, 'join: incomplete set draws no reply')

      call send_finish(720_int64, err)
      call check_(error, err == CAPNP_OK, 'join: cancellation finish sent')
      call rpc_pump_once(srv, err)
      call check_(error, err == CAPNP_OK, 'join: cancellation finish processed')

      ! Cancellation must free the incomplete Join state, allowing the
      ! same join id to begin a differently sized complete set.
      call send_join_part(721_int64, a, 13_int64, 1, 0, err)
      call rpc_pump_once(srv, err)
      call recv_join_result(ansid, jid, succeeded, has_cap, err)
      call check_(error, err == CAPNP_OK, 'join: canceled replacement result read')
      call check_(error, ansid == 721_int64, 'join: canceled replacement answer id')
      call check_(error, jid == 13_int64, 'join: canceled replacement join id')
      call check_(error, succeeded, 'join: canceled replacement succeeds')
      call check_(error, has_cap, 'join: canceled replacement carries cap')
   end subroutine t_join_incomplete_set_is_silent

   !> A later Join part that cannot be sent must Finish the parts already
   !> on the wire and return the question that never left.
   subroutine t_join_send_unwinds(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_cap_t) :: caps(2)
      integer(int64) :: qids(2)
      integer :: a, b, n, i
      logical :: held
      call two_live_exports(a, b, n)
      call check_(error, n >= 1, 'join unwind: server has a live export')
      if (allocated(error)) return
      caps(1)%kind = RPC_CAP_IMPORT
      caps(1)%id = int(a, int64)
      caps(2) = rpc_cap_t()
      call rpc_join_send(cli, caps, 77_int64, qids, err)
      call check_(error, err == CAPNP_ERR_ARG, 'join unwind: bad part is an argument error')
      call check_(error, qids(1) >= 0_int64 .and. .not. cli%questions(int(qids(1)))%used, &
                  'join unwind: sent part is finished locally')
      call check_(error, qids(2) < 0_int64, 'join unwind: unsent part is not reserved')
      call rpc_pump_once(srv, err)
      call rpc_pump_once(srv, err)
      held = .false.
      do i = 0, 7
         if (srv%joins(i)%used .and. srv%joins(i)%join_id == 77_int64) held = .true.
      end do
      call check_(error, .not. held, 'join unwind: Finish frees the server slot')
   end subroutine t_join_send_unwinds

   !> Finish(releaseResultCaps) drops the export reference a Join result added.
   subroutine t_join_finish_releases_cap(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: local_boot
      type(rpc_cap_t) :: cap, caps(1)
      integer(int64) :: qboot, qids(1)
      integer :: fa, fb, e, before
      call px_socketpair(fa, fb, e)
      call check_(error, e == CAPNP_OK, 'join release: socketpair')
      if (allocated(error)) return
      local_boot => local_echo
      call rpc_conn_init(sconn, fb, local_boot)
      local_boot => null()
      call rpc_conn_init(cconn, fa, local_boot)
      call rpc_bootstrap_send(cconn, cap, e)
      qboot = cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call rpc_result_cap(cconn, qboot, [integer ::], cap, e)
      call check_(error, e == CAPNP_OK .and. sconn%exports(0)%used, 'join release: bootstrap')
      if (allocated(error)) return
      before = sconn%exports(0)%refcount
      caps(1) = cap
      call rpc_join_send(cconn, caps, 4_int64, qids, e)
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qids(1), e)
      call rpc_finish_send(cconn, qids(1), .false., e)
      call rpc_pump_once(sconn, e)
      call check_(error, sconn%exports(0)%used, 'join release: original export remains')
      call check_(error, sconn%exports(0)%refcount == before, &
                  'join release: Finish drops the result reference')
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_join_finish_releases_cap

   !> A peer that dies mid-join does not leave the join slot occupied.
   subroutine t_abandoned_join_cleared(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: local_boot
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(join_t) :: jn
      type(message_target_t) :: tgt
      type(join_key_part_t) :: kp
      integer :: fa, fb, e, i
      logical :: held
      call px_socketpair(fa, fb, e)
      call check_(error, e == CAPNP_OK, 'join death: socketpair')
      if (allocated(error)) return
      local_boot => local_echo
      call rpc_conn_init(sconn, fb, local_boot)
      local_boot => null()
      call rpc_conn_init(cconn, fa, local_boot)
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      jn = message_join_init(msg, e)
      call join_question_id_set(jn, 5_int64, e)
      tgt = join_target_init(jn, e)
      call message_target_imported_cap_set(tgt, 0_int64, e)
      kp = join_key_part_new(m, e)
      call join_key_part_join_id_set(kp, 9_int64, e)
      call join_key_part_part_count_set(kp, 2, e)
      call join_key_part_part_num_set(kp, 0, e)
      call join_key_part_set(jn, kp%p, e)
      call rpc_send_message(cconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(sconn, e)
      held = .false.
      do i = 0, 7
         if (sconn%joins(i)%used) held = .true.
      end do
      call check_(error, held, 'join death: partial set is recorded')
      if (allocated(error)) return
      call rpc_conn_close(cconn)
      call rpc_pump_once(sconn, e)
      call check_(error, e == RPC_ERR_DEAD, 'join death: peer close is reported')
      held = .false.
      do i = 0, 7
         if (sconn%joins(i)%used) held = .true.
      end do
      call check_(error, .not. held, 'join death: slot freed without Finish')
      call rpc_conn_close(sconn)
   end subroutine t_abandoned_join_cleared

   !> Finish names a Provide question that does not fit in the answer table.
   subroutine t_finish_provide_qid(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: local_boot
      type(rpc_cap_t) :: cap
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(provide_t) :: pv
      type(message_target_t) :: tgt
      type(recipient_id_t) :: rid
      type(vat_id_t) :: vat
      type(finish_t) :: f
      integer(int64) :: qboot
      integer :: fa, fb, e, before, refs
      call px_socketpair(fa, fb, e)
      call check_(error, e == CAPNP_OK, 'provide finish: socketpair')
      if (allocated(error)) return
      local_boot => local_echo
      call rpc_conn_init(sconn, fb, local_boot)
      local_boot => null()
      call rpc_conn_init(cconn, fa, local_boot)
      call rpc_bootstrap_send(cconn, cap, e)
      qboot = cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call rpc_result_cap(cconn, qboot, [integer ::], cap, e)
      call check_(error, e == CAPNP_OK, 'provide finish: bootstrap')
      if (allocated(error)) return
      before = rpc_pending_provisions(sconn)
      refs = sconn%exports(0)%refcount
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      pv = message_provide_init(msg, e)
      call provide_question_id_set(pv, 100_int64, e)
      tgt = provide_target_init(pv, e)
      call message_target_imported_cap_set(tgt, 0_int64, e)
      rid = recipient_id_new(m, e)
      call recipient_id_nonce_set(rid, 4242_int64, e)
      vat = recipient_id_vat_init(rid, e)
      call vat_id_host_set(vat, '127.0.0.1', e)
      call vat_id_port_set(vat, 9, e)
      call provide_recipient_set(pv, rid%p, e)
      call rpc_send_message(cconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(sconn, e)
      call rpc_recv_message(cconn%fd, m, e)
      call capnp_message_free(m)
      call check_(error, rpc_pending_provisions(sconn) == before + 1, &
                  'provide finish: arrangement is pending')
      if (allocated(error)) return
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      f = message_finish_init(msg, e)
      call finish_question_id_set(f, 100_int64, e)
      call finish_release_result_caps_set(f, .true., e)
      call finish_require_early_cancellation_workaround_set(f, .false., e)
      call rpc_send_message(cconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(sconn, e)
      call check_(error, rpc_pending_provisions(sconn) == before, &
                  'provide finish: Finish releases the arrangement')
      call check_(error, sconn%exports(0)%used .and. sconn%exports(0)%refcount == refs, &
                  'provide finish: only the pin is dropped')
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_finish_provide_qid

   !> Cancellation is a Finish sent before Return, with the legacy early
   !> cancellation workaround disabled as required by modern peers.
   subroutine t_request_cancellation(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_cap_t) :: target
      type(capnp_message_t), target :: m, wire
      type(message_t) :: msg
      type(payload_t) :: params
      type(finish_t) :: finish
      integer(int64) :: qid
      integer :: a, b, n
      call two_live_exports(a, b, n)
      call check_(error, n >= 1, 'cancel: server has a live export')
      if (allocated(error)) return
      target%kind = RPC_CAP_IMPORT
      target%id = int(a, int64)
      call rpc_call_begin(cli, target, ECHO_IFACE, 0, m, params, qid, err)
      call payload_content_set(params, capnp_new_struct(m, 1, 1, err), err)
      call rpc_call_send(cli, m, err)
      call rpc_request_cancellation(cli, qid, err)
      call check_(error, err == CAPNP_OK, 'cancel: Finish sent')

      ! Drain the Call and inspect the following Finish without dispatching
      ! the call, which models cancellation before application delivery.
      call rpc_recv_message(srv%fd, wire, err)
      call capnp_message_free(wire)
      call rpc_recv_message(srv%fd, wire, err)
      msg = message_read_root(wire, err)
      call check_(error, message_which(msg) == MESSAGE_FINISH_TAG, &
                  'cancel: request is Finish')
      finish = message_finish_get(msg, err)
      call check_(error, finish_question_id_get(finish) == qid, &
                  'cancel: question id')
      call check_(error, finish_release_result_caps_get(finish), &
                  'cancel: result caps released')
      call check_(error, .not. finish_require_early_cancellation_workaround_get(finish), &
                  'cancel: modern cancellation semantics')
      call check_(error, cli%questions(int(qid))%used .and. &
                  cli%questions(int(qid))%canceled, &
                  'cancel: question reserved until its return')
      call capnp_message_free(wire)

      call rpc_finish_send(cli, -1_int64, .false., err)
      call check_(error, err == CAPNP_ERR_BOUNDS, 'cancel: invalid finish id rejected')
      call rpc_request_cancellation(cli, -1_int64, err)
      call check_(error, err == CAPNP_ERR_BOUNDS, 'cancel: invalid cancellation id rejected')
   end subroutine t_request_cancellation

   !> A canceled question keeps its id until the late Return arrives, so
   !> that Return cannot become the result of the next call.
   subroutine t_cancel_reserves_qid(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: local_boot
      type(rpc_cap_t) :: cap
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(capnp_ptr_t) :: s, content
      integer(int64) :: qboot, q1, q2
      integer :: fa, fb, e
      call px_socketpair(fa, fb, e)
      call check_(error, e == CAPNP_OK, 'cancel id: socketpair')
      if (allocated(error)) return
      local_boot => local_echo
      call rpc_conn_init(sconn, fb, local_boot)
      local_boot => null()
      call rpc_conn_init(cconn, fa, local_boot)
      call rpc_bootstrap_send(cconn, cap, e)
      qboot = cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call rpc_result_cap(cconn, qboot, [integer ::], cap, e)
      call check_(error, e == CAPNP_OK, 'cancel id: bootstrap')
      if (allocated(error)) return

      call capnp_message_init_builder(m, e)
      call rpc_call_begin(cconn, cap, ECHO_IFACE, 0, m, params, q1, e)
      s = capnp_new_struct(m, 1, 1, e)
      call capnp_set_i64(s, 0_int64, 1_int64, e)
      call capnp_set_text(s, 0, 'a', e)
      call payload_content_set(params, s, e)
      call rpc_call_send(cconn, m, e)
      call rpc_request_cancellation(cconn, q1, e)
      call check_(error, e == CAPNP_OK .and. cconn%questions(int(q1))%canceled, &
                  'cancel id: first call reserved')
      call rpc_wait(cconn, q1, e)
      call check_(error, e == RPC_ERR_EXCEPTION, 'cancel id: wait does not block')
      call rpc_wait(cconn, -1_int64, e)
      call check_(error, e == CAPNP_ERR_BOUNDS, 'cancel id: wait rejects a negative id')
      call rpc_wait(cconn, 64_int64, e)
      call check_(error, e == CAPNP_ERR_BOUNDS, 'cancel id: wait rejects an out of range id')

      call capnp_message_init_builder(m, e)
      call rpc_call_begin(cconn, cap, ECHO_IFACE, 0, m, params, q2, e)
      s = capnp_new_struct(m, 1, 1, e)
      call capnp_set_i64(s, 0_int64, 7_int64, e)
      call capnp_set_text(s, 0, 'b', e)
      call payload_content_set(params, s, e)
      call rpc_call_send(cconn, m, e)
      call check_(error, q2 /= q1, 'cancel id: new call does not reuse the id')
      if (allocated(error)) return

      call rpc_pump_once(sconn, e)
      call rpc_pump_once(sconn, e)
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, q2, e)
      call rpc_result_content(cconn, q2, content, e)
      call check_(error, e == CAPNP_OK .and. capnp_get_i64(content, 0_int64) == 14_int64, &
                  'cancel id: late return is not the new result')
      call check_(error, .not. cconn%questions(int(q1))%used, &
                  'cancel id: return releases the reserved id')
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_cancel_reserves_qid

   subroutine fail_stream_send(self, bytes, err)
      class(fail_stream_t), intent(inout) :: self
      integer(int8), intent(inout), target :: bytes(:)
      integer, intent(out) :: err
      if (self%accept_sends > 0) then
         self%accept_sends = self%accept_sends - 1
         err = CAPNP_OK
         if (size(bytes) < 0) err = CAPNP_ERR_IO
      else
         err = CAPNP_ERR_IO
      end if
   end subroutine fail_stream_send

   subroutine fail_stream_recv(self, bytes, err)
      class(fail_stream_t), intent(inout) :: self
      integer(int8), intent(inout), target :: bytes(:)
      integer, intent(out) :: err
      err = CAPNP_ERR_IO
      if (size(bytes) < 0) self%accept_sends = 0
   end subroutine fail_stream_recv

   subroutine fail_stream_poll(self, timeout_ms, readable, err)
      class(fail_stream_t), intent(inout) :: self
      integer, intent(in) :: timeout_ms
      logical, intent(out) :: readable
      integer, intent(out) :: err
      readable = .false.
      err = CAPNP_OK
      if (timeout_ms < 0) self%accept_sends = 0
   end subroutine fail_stream_poll

   subroutine fail_stream_close(self)
      class(fail_stream_t), intent(inout) :: self
      self%accept_sends = 0
   end subroutine fail_stream_close

   !> A question allocated for a call or bootstrap that never leaves the
   !> vat must not stay reserved, or the table fills after repeated errors.
   subroutine t_send_failure_releases_qid(error)
      type(error_type), allocatable, intent(inout) :: error
      type(fail_stream_t), target :: broken
      type(rpc_conn_t), target :: cconn
      class(rpc_server_t), pointer :: local_boot
      type(rpc_cap_t) :: cap
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      integer(int64) :: qid
      integer :: i, e
      local_boot => null()
      broken%accept_sends = 0
      call rpc_conn_init_stream(cconn, broken, local_boot)
      qid = 0_int64
      do i = 1, 65
         call rpc_bootstrap_send(cconn, cap, e)
      end do
      call check_(error, e == CAPNP_ERR_IO, 'send fail: bootstrap reports the stream')
      call check_(error, .not. cconn%questions(0)%used, &
                  'send fail: bootstrap does not keep the question')
      cap = rpc_cap_t()
      call rpc_call_begin(cconn, cap, ECHO_IFACE, 0, m, params, qid, e)
      call check_(error, e == CAPNP_ERR_ARG .and. qid < 0_int64, &
                  'send fail: bad target releases its question')
      call check_(error, .not. cconn%questions(0)%used, &
                  'send fail: question table still empty')
      call rpc_conn_close(cconn)
   end subroutine t_send_failure_releases_qid

   !> A membrane checks calls, recursively wraps result capabilities,
   !> blocks client-side calls through an explicit wrapper, and revokes
   !> every server capability sharing its policy.
   subroutine t_membrane(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: mcli, msrv
      type(echo_srv_t), target :: inner_server
      type(rpc_membrane_server_t), target :: wrapped_server
      type(counting_membrane_policy_t), target :: policy_store
      class(rpc_server_t), pointer :: inner, mboot
      class(rpc_membrane_policy_t), pointer :: policy
      type(rpc_cap_t) :: root, adder, blocked
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(capnp_ptr_t) :: s, content
      integer(int64) :: qb, qm, qa
      integer :: ma, mb, e
      logical :: handled

      call px_socketpair(ma, mb, e)
      call check_(error, e == CAPNP_OK, 'membrane: socketpair')
      if (allocated(error)) return
      inner => inner_server
      policy => policy_store
      call rpc_membrane_server_init(wrapped_server, inner, policy)
      mboot => wrapped_server
      call rpc_conn_init(msrv, mb, mboot)
      mboot => null()
      call rpc_conn_init(mcli, ma, mboot)

      call rpc_bootstrap_send(mcli, root, e)
      qb = root%id
      call rpc_pump_once(msrv, e)
      call rpc_wait(mcli, qb, e)
      call rpc_result_cap(mcli, qb, [integer ::], root, e)
      call rpc_finish_send(mcli, qb, .true., e)
      call rpc_pump_once(msrv, e)

      call rpc_call_begin(mcli, root, ECHO_IFACE, 1, m, params, qm, e)
      s = capnp_new_struct(m, 1, 0, e)
      call capnp_set_i64(s, 0_int64, 100_int64, e)
      call payload_content_set(params, s, e)
      call rpc_call_send(mcli, m, e)
      call rpc_pump_once(msrv, e)
      call rpc_wait(mcli, qm, e)
      call rpc_result_cap(mcli, qm, [0], adder, e)
      call check_(error, e == CAPNP_OK .and. policy_store%inbound_calls == 1, &
                  'membrane: root call checked')

      call rpc_call_begin(mcli, adder, ECHO_IFACE, 0, m, params, qa, e)
      s = capnp_new_struct(m, 1, 0, e)
      call capnp_set_i64(s, 0_int64, 5_int64, e)
      call payload_content_set(params, s, e)
      call rpc_call_send(mcli, m, e)
      call rpc_pump_once(msrv, e)
      call rpc_wait(mcli, qa, e)
      call rpc_result_content(mcli, qa, content, e)
      call check_(error, e == CAPNP_OK .and. capnp_get_i64(content, 0_int64) == 105_int64, &
                  'membrane: recursive result capability works')
      call rpc_finish_send(mcli, qm, .true., e)
      call rpc_pump_once(msrv, e)
      call rpc_finish_send(mcli, qa, .false., e)
      call rpc_pump_once(msrv, e)
      call check_(error, policy_store%inbound_calls == 2, &
                  'membrane: recursive result capability checked')

      policy_store%block = .true.
      blocked = rpc_membrane_cap(adder, policy)
      call rpc_call_begin(mcli, blocked, ECHO_IFACE, 0, m, params, qa, e)
      call check_(error, e == RPC_ERR_EXCEPTION .and. qa < 0_int64, &
                  'membrane: client wrapper blocks before send')
      blocked = rpc_membrane_cap(adder, policy, reverse=.true.)
      call rpc_call_begin(mcli, blocked, ECHO_IFACE, 0, m, params, qa, e)
      call check_(error, e == RPC_ERR_EXCEPTION .and. &
                  policy_store%outbound_calls == 1, &
                  'membrane: reverse wrapper checks outbound calls')
      policy_store%block = .false.

      ! A pipelined capability must carry the question's membrane, so a
      ! revoke stops the call before it is written to the socket.
      blocked = rpc_membrane_cap(adder, policy)
      call capnp_message_init_builder(m, e)
      call rpc_call_begin(mcli, blocked, ECHO_IFACE, 0, m, params, qa, e)
      s = capnp_new_struct(m, 1, 0, e)
      call capnp_set_i64(s, 0_int64, 1_int64, e)
      call payload_content_set(params, s, e)
      call capnp_message_free(m)
      blocked = rpc_pipeline_cap(mcli, qa, [0])

      call rpc_membrane_revoke(policy_store, 'test membrane revoked')
      call rpc_call_begin(mcli, blocked, ECHO_IFACE, 0, m, params, qa, e)
      call check_(error, e == RPC_ERR_EXCEPTION .and. qa < 0_int64, &
                  'membrane: revoked pipeline does not send')
      call rpc_pump_poll(msrv, 20, handled, e)
      call check_(error, e == CAPNP_OK .and. .not. handled, &
                  'membrane: server saw no pipelined call')
      call rpc_call_begin(mcli, adder, ECHO_IFACE, 0, m, params, qa, e)
      s = capnp_new_struct(m, 1, 0, e)
      call payload_content_set(params, s, e)
      call rpc_call_send(mcli, m, e)
      call rpc_pump_once(msrv, e)
      call rpc_wait(mcli, qa, e)
      call rpc_result_content(mcli, qa, content, e)
      call check_(error, e == RPC_ERR_EXCEPTION, &
                  'membrane: revocation reaches recursive wrappers')
      call rpc_finish_send(mcli, qa, .false., e)
      call rpc_pump_once(msrv, e)

      call rpc_conn_close(mcli)
      call rpc_conn_close(msrv)
   end subroutine t_membrane

   !> A cap-returning call against a full export table is an exception,
   !> and the membrane wrapper built for that call is not left allocated.
   subroutine t_export_table_full(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: mcli, msrv
      type(echo_srv_t), target :: inner_server
      type(rpc_membrane_server_t), target :: wrapped_server
      type(counting_membrane_policy_t), target :: policy_store
      class(rpc_server_t), pointer :: inner, mboot
      class(rpc_membrane_policy_t), pointer :: policy
      type(rpc_cap_t) :: root
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(capnp_ptr_t) :: s, content
      integer(int64) :: q
      integer :: ma, mb, e, i, nused
      call px_socketpair(ma, mb, e)
      call check_(error, e == CAPNP_OK, 'export full: socketpair')
      if (allocated(error)) return
      inner => inner_server
      policy => policy_store
      call rpc_membrane_server_init(wrapped_server, inner, policy)
      mboot => wrapped_server
      call rpc_conn_init(msrv, mb, mboot)
      mboot => null()
      call rpc_conn_init(mcli, ma, mboot)
      call rpc_bootstrap_send(mcli, root, e)
      q = root%id
      call rpc_pump_once(msrv, e)
      call rpc_wait(mcli, q, e)
      call rpc_result_cap(mcli, q, [integer ::], root, e)
      call rpc_finish_send(mcli, q, .true., e)
      call rpc_pump_once(msrv, e)
      do i = 1, 63
         call rpc_call_begin(mcli, root, ECHO_IFACE, 1, m, params, q, e)
         s = capnp_new_struct(m, 1, 0, e)
         call capnp_set_i64(s, 0_int64, int(i, int64), e)
         call payload_content_set(params, s, e)
         call rpc_call_send(mcli, m, e)
         call rpc_pump_once(msrv, e)
         call rpc_wait(mcli, q, e)
         call rpc_result_content(mcli, q, content, e)
         if (e /= CAPNP_OK) exit
         call rpc_finish_send(mcli, q, .true., e)
         call rpc_pump_once(msrv, e)
      end do
      call check_(error, e == CAPNP_OK, 'export full: table filled')
      if (allocated(error)) return
      nused = 0
      do i = 0, size(msrv%exports) - 1
         if (msrv%exports(i)%used) nused = nused + 1
      end do
      call check_(error, nused == size(msrv%exports), 'export full: every slot is live')
      call rpc_call_begin(mcli, root, ECHO_IFACE, 1, m, params, q, e)
      s = capnp_new_struct(m, 1, 0, e)
      call capnp_set_i64(s, 0_int64, 9_int64, e)
      call payload_content_set(params, s, e)
      call rpc_call_send(mcli, m, e)
      call rpc_pump_once(msrv, e)
      call rpc_wait(mcli, q, e)
      call rpc_result_content(mcli, q, content, e)
      call check_(error, e == RPC_ERR_EXCEPTION, 'export full: mint is an exception')
      nused = 0
      do i = 0, size(msrv%exports) - 1
         if (msrv%exports(i)%used) nused = nused + 1
      end do
      call check_(error, nused == size(msrv%exports), 'export full: no partial export kept')
      call rpc_finish_send(mcli, q, .false., e)
      call rpc_pump_once(msrv, e)
      call rpc_call_begin(mcli, root, ECHO_IFACE, 0, m, params, q, e)
      s = capnp_new_struct(m, 1, 1, e)
      call capnp_set_i64(s, 0_int64, 3_int64, e)
      call capnp_set_text(s, 0, 'z', e)
      call payload_content_set(params, s, e)
      call rpc_call_send(mcli, m, e)
      call rpc_pump_once(msrv, e)
      call rpc_wait(mcli, q, e)
      call rpc_result_content(mcli, q, content, e)
      call check_(error, e == CAPNP_OK .and. capnp_get_i64(content, 0_int64) == 6_int64, &
                  'export full: later call still answers')
      call rpc_conn_close(mcli)
      call rpc_conn_close(msrv)
   end subroutine t_export_table_full

   !> Connections can own an abstract byte stream rather than depending
   !> on the integer descriptor adapter in the RPC core.
   subroutine t_pluggable_transport(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_fd_stream_t), target :: astream, bstream
      type(rpc_conn_t), target :: aconn, bconn
      type(echo_srv_t), target :: stream_server
      class(rpc_server_t), pointer :: stream_boot
      type(rpc_cap_t) :: cap
      integer :: e
      call rpc_socketpair_streams(astream, bstream, e)
      call check_(error, e == CAPNP_OK, 'transport: stream socketpair')
      if (allocated(error)) return
      stream_boot => null()
      call rpc_conn_init_stream(aconn, astream, stream_boot)
      stream_boot => stream_server
      call rpc_conn_init_stream(bconn, bstream, stream_boot)
      call rpc_bootstrap_send(aconn, cap, e)
      call rpc_pump_once(bconn, e)
      call rpc_wait(aconn, cap%id, e)
      call check_(error, e == CAPNP_OK, 'transport: bootstrap over byte stream')
      call rpc_conn_close(aconn)
      call rpc_conn_close(bconn)
      call check_(error, astream%fd == PX_BAD_FD .and. bstream%fd == PX_BAD_FD, &
                  'transport: adapter close owns descriptors')
   end subroutine t_pluggable_transport

   !> Bootstrap, then a call on the still-promised bootstrap cap
   !> (pipelining), then settle the cap and call again.
   subroutine t_bootstrap_and_echo(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_cap_t) :: bootcap, settled
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(capnp_ptr_t) :: s, content
      integer(int64) :: q0, q1, q2
      character(len=:), allocatable :: txt

      call rpc_bootstrap_send(cli, bootcap, err)
      call check_(error, err == CAPNP_OK .and. bootcap%kind == RPC_CAP_PIPELINE, 'rpc: bootstrap sent')
      q0 = bootcap%id
      call rpc_pump_once(srv, err)
      call check_(error, err == CAPNP_OK, 'rpc: server answered bootstrap')

      ! Pipelined call: target the bootstrap promise before waiting.
      call rpc_call_begin(cli, bootcap, ECHO_IFACE, 0, m, params, q1, err)
      call check_(error, err == CAPNP_OK, 'rpc: call begin (pipelined)')
      s = capnp_new_struct(m, 1, 1, err)
      call capnp_set_i64(s, 0_int64, 21_int64, err)
      call capnp_set_text(s, 0, 'hi', err)
      call payload_content_set(params, s, err)
      call rpc_call_send(cli, m, err)
      call check_(error, err == CAPNP_OK, 'rpc: call sent')
      call rpc_pump_once(srv, err)
      call check_(error, err == CAPNP_OK, 'rpc: server dispatched pipelined call')

      call rpc_wait(cli, q0, err)
      call check_(error, err == CAPNP_OK, 'rpc: bootstrap returned')
      call rpc_result_cap(cli, q0, [integer ::], settled, err)
      call check_(error, err == CAPNP_OK .and. settled%kind == RPC_CAP_IMPORT, 'rpc: bootstrap cap settles')

      call rpc_wait(cli, q1, err)
      call check_(error, err == CAPNP_OK, 'rpc: echo returned')
      call rpc_result_content(cli, q1, content, err)
      call check_(error, err == CAPNP_OK, 'rpc: echo content')
      call check_(error, capnp_get_i64(content, 0_int64) == 42_int64, 'rpc: echo doubles')
      call capnp_get_text(content, 0, txt, err)
      call check_(error, txt == 'echo: hi', 'rpc: echo text')

      ! Settled-import call.
      call rpc_call_begin(cli, settled, ECHO_IFACE, 0, m, params, q2, err)
      s = capnp_new_struct(m, 1, 1, err)
      call capnp_set_i64(s, 0_int64, 5_int64, err)
      call capnp_set_text(s, 0, 'yo', err)
      call payload_content_set(params, s, err)
      call rpc_call_send(cli, m, err)
      call rpc_pump_once(srv, err)
      call rpc_wait(cli, q2, err)
      call rpc_result_content(cli, q2, content, err)
      call check_(error, err == CAPNP_OK .and. capnp_get_i64(content, 0_int64) == 10_int64, &
                  'rpc: settled call')

      call rpc_finish_send(cli, q0, .true., err)
      call rpc_pump_once(srv, err)
      call rpc_finish_send(cli, q1, .false., err)
      call rpc_pump_once(srv, err)
      call rpc_finish_send(cli, q2, .false., err)
      call rpc_pump_once(srv, err)
      call check_(error, err == CAPNP_OK, 'rpc: finishes processed')
   end subroutine t_bootstrap_and_echo

   !> Method 1 mints an adder capability in its results; pipeline into it
   !> before the return settles, then settle and call the import.
   subroutine t_cap_result_and_pipeline(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_cap_t) :: bootcap, adder_p, adder
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(capnp_ptr_t) :: s, content
      integer(int64) :: qb, qm, qp, qs

      call rpc_bootstrap_send(cli, bootcap, err)
      qb = bootcap%id
      call rpc_pump_once(srv, err)

      ! make-adder(base=100)
      call rpc_call_begin(cli, bootcap, ECHO_IFACE, 1, m, params, qm, err)
      s = capnp_new_struct(m, 1, 0, err)
      call capnp_set_i64(s, 0_int64, 100_int64, err)
      call payload_content_set(params, s, err)
      call rpc_call_send(cli, m, err)
      call rpc_pump_once(srv, err)
      call check_(error, err == CAPNP_OK, 'rpc: make-adder dispatched')

      ! Pipeline: add(5) on the promised adder at results ptr field 0.
      adder_p = rpc_pipeline_cap(cli, qm, [0])
      call rpc_call_begin(cli, adder_p, ECHO_IFACE, 0, m, params, qp, err)
      s = capnp_new_struct(m, 1, 0, err)
      call capnp_set_i64(s, 0_int64, 5_int64, err)
      call payload_content_set(params, s, err)
      call rpc_call_send(cli, m, err)
      call rpc_pump_once(srv, err)
      call check_(error, err == CAPNP_OK, 'rpc: pipelined add dispatched')

      call rpc_wait(cli, qb, err)
      call rpc_wait(cli, qm, err)
      call rpc_wait(cli, qp, err)
      call rpc_result_content(cli, qp, content, err)
      call check_(error, err == CAPNP_OK .and. capnp_get_i64(content, 0_int64) == 105_int64, &
                  'rpc: pipelined add result')
      call rpc_finish_send(cli, qb, .true., err)
      call rpc_pump_once(srv, err)
      call rpc_finish_send(cli, qp, .false., err)
      call rpc_pump_once(srv, err)

      ! Settle the adder and call it directly.
      call rpc_result_cap(cli, qm, [0], adder, err)
      call check_(error, err == CAPNP_OK .and. adder%kind == RPC_CAP_IMPORT, 'rpc: adder settles')
      call rpc_call_begin(cli, adder, ECHO_IFACE, 0, m, params, qs, err)
      s = capnp_new_struct(m, 1, 0, err)
      call capnp_set_i64(s, 0_int64, 7_int64, err)
      call payload_content_set(params, s, err)
      call rpc_call_send(cli, m, err)
      call rpc_pump_once(srv, err)
      call rpc_wait(cli, qs, err)
      call rpc_result_content(cli, qs, content, err)
      call check_(error, err == CAPNP_OK .and. capnp_get_i64(content, 0_int64) == 107_int64, &
                  'rpc: settled adder call')
      call rpc_finish_send(cli, qs, .false., err)
      call rpc_pump_once(srv, err)

      ! Retain the adder import across the finish of its origin question.
      call rpc_finish_send(cli, qm, .true., err)
      call rpc_pump_once(srv, err)
      call rpc_call_begin(cli, adder, ECHO_IFACE, 0, m, params, qs, err)
      s = capnp_new_struct(m, 1, 0, err)
      call capnp_set_i64(s, 0_int64, 1_int64, err)
      call payload_content_set(params, s, err)
      call rpc_call_send(cli, m, err)
      call rpc_pump_once(srv, err)
      call rpc_wait(cli, qs, err)
      call rpc_result_content(cli, qs, content, err)
      call check_(error, err == CAPNP_OK .and. capnp_get_i64(content, 0_int64) == 101_int64, &
                  'rpc: retained cap survives finish')
      call rpc_finish_send(cli, qs, .false., err)
      call rpc_pump_once(srv, err)

      ! Release the adder; a call afterwards must raise an exception.
      call rpc_release_send(cli, adder, err)
      call rpc_pump_once(srv, err)
      call rpc_call_begin(cli, adder, ECHO_IFACE, 0, m, params, qs, err)
      s = capnp_new_struct(m, 1, 0, err)
      call capnp_set_i64(s, 0_int64, 1_int64, err)
      call payload_content_set(params, s, err)
      call rpc_call_send(cli, m, err)
      call rpc_pump_once(srv, err)
      call rpc_wait(cli, qs, err)
      call rpc_result_content(cli, qs, content, err)
      call check_(error, err == RPC_ERR_EXCEPTION, 'rpc: released cap raises')
      call rpc_finish_send(cli, qs, .false., err)
      call rpc_pump_once(srv, err)
   end subroutine t_cap_result_and_pipeline

   !> Unknown method ids surface as exception returns.
   subroutine t_exception(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_cap_t) :: bootcap
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(capnp_ptr_t) :: s, content
      integer(int64) :: qb, qx
      call rpc_bootstrap_send(cli, bootcap, err)
      qb = bootcap%id
      call rpc_pump_once(srv, err)
      call rpc_call_begin(cli, bootcap, ECHO_IFACE, 42, m, params, qx, err)
      s = capnp_new_struct(m, 1, 0, err)
      call payload_content_set(params, s, err)
      call rpc_call_send(cli, m, err)
      call rpc_pump_once(srv, err)
      call rpc_wait(cli, qb, err)
      call rpc_wait(cli, qx, err)
      call rpc_result_content(cli, qx, content, err)
      call check_(error, err == RPC_ERR_EXCEPTION, 'rpc: bad method raises')
      call rpc_finish_send(cli, qb, .true., err)
      call rpc_pump_once(srv, err)
      call rpc_finish_send(cli, qx, .false., err)
      call rpc_pump_once(srv, err)
   end subroutine t_exception

   !> The obsolete save/delete messages are gone from the protocol, so
   !> they must come back as Message.unimplemented echoing the original,
   !> and the sender must absorb that reply without disturbing the
   !> connection.
   subroutine t_unimplemented(error)
      type(error_type), allocatable, intent(inout) :: error
      type(capnp_message_t), target :: m, rm
      type(message_t) :: msg, rmsg
      call capnp_message_init_builder(m, err)
      msg = message_new_root(m, err)
      call message_obsolete_delete_set(msg, capnp_new_struct(m, 1, 0, err), err)
      call rpc_send_message(cli%fd, m, err)
      call capnp_message_free(m)
      call check_(error, err == CAPNP_OK, 'rpc: obsoleteDelete sent')
      call rpc_pump_once(srv, err)
      call check_(error, err == CAPNP_OK, 'rpc: server handled obsoleteDelete')

      call rpc_recv_message(cli%fd, rm, err)
      call check_(error, err == CAPNP_OK, 'rpc: reply read')
      if (allocated(error)) return
      rmsg = message_read_root(rm, err)
      call check_(error, err == CAPNP_OK .and. &
                  message_which(rmsg) == MESSAGE_UNIMPLEMENTED_TAG, &
                  'rpc: reply is unimplemented')
      ! The reply echoes the message it could not handle, which is how the
      ! sender knows which one was refused.
      msg = message_unimplemented_get(rmsg, err)
      call check_(error, err == CAPNP_OK .and. &
                  message_which(msg) == MESSAGE_OBSOLETE_DELETE_TAG, &
                  'rpc: unimplemented echoes the original')
      call capnp_message_free(rm)

      ! And an unimplemented arriving at a vat is absorbed, not fatal.
      call capnp_message_init_builder(m, err)
      msg = message_new_root(m, err)
      rmsg = message_unimplemented_init(msg, err)
      call message_obsolete_save_set(rmsg, capnp_new_struct(m, 1, 0, err), err)
      call rpc_send_message(srv%fd, m, err)
      call capnp_message_free(m)
      call rpc_pump_once(cli, err)
      call check_(error, err == CAPNP_OK .and. .not. cli%dead, &
                  'rpc: client absorbs unimplemented')
   end subroutine t_unimplemented

   ! ------------------------------------------------------------------
   ! Level 3: handing a capability to a third vat
   ! ------------------------------------------------------------------

   !> Alice -> Bob: hold export `eid` for whoever presents `nonce`.
   subroutine send_provide(qid, eid, nonce, e)
      integer(int64), intent(in) :: qid, nonce
      integer, intent(in) :: eid
      integer, intent(out) :: e
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(provide_t) :: pv
      type(message_target_t) :: tgt
      type(recipient_id_t) :: rid
      type(vat_id_t) :: vat
      call capnp_message_init_builder(m, e)
      if (e /= CAPNP_OK) return
      msg = message_new_root(m, e)
      if (e /= CAPNP_OK) return
      pv = message_provide_init(msg, e)
      if (e /= CAPNP_OK) return
      call provide_question_id_set(pv, qid, e)
      if (e /= CAPNP_OK) return
      tgt = provide_target_init(pv, e)
      if (e /= CAPNP_OK) return
      call message_target_imported_cap_set(tgt, int(eid, int64), e)
      if (e /= CAPNP_OK) return
      rid = recipient_id_new(m, e)
      if (e /= CAPNP_OK) return
      call recipient_id_nonce_set(rid, nonce, e)
      if (e /= CAPNP_OK) return
      vat = recipient_id_vat_init(rid, e)
      if (e /= CAPNP_OK) return
      call vat_id_host_set(vat, '127.0.0.1', e)
      if (e /= CAPNP_OK) return
      call vat_id_port_set(vat, 4000, e)
      if (e /= CAPNP_OK) return
      call provide_recipient_set(pv, rid%p, e)
      if (e /= CAPNP_OK) return
      call rpc_send_message(cli%fd, m, e)
      call capnp_message_free(m)
   end subroutine send_provide

   !> Carol -> Bob: claim the capability held under `nonce`.
   subroutine send_accept(qid, nonce, e)
      integer(int64), intent(in) :: qid, nonce
      integer, intent(out) :: e
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(accept_t) :: ac
      type(provision_id_t) :: pid
      call capnp_message_init_builder(m, e)
      if (e /= CAPNP_OK) return
      msg = message_new_root(m, e)
      if (e /= CAPNP_OK) return
      ac = message_accept_init(msg, e)
      if (e /= CAPNP_OK) return
      call accept_question_id_set(ac, qid, e)
      if (e /= CAPNP_OK) return
      pid = provision_id_new(m, e)
      if (e /= CAPNP_OK) return
      call provision_id_nonce_set(pid, nonce, e)
      if (e /= CAPNP_OK) return
      call accept_provision_set(ac, pid%p, e)
      if (e /= CAPNP_OK) return
      call rpc_send_message(cli%fd, m, e)
      call capnp_message_free(m)
   end subroutine send_accept

   !> Read one Return: its answerId, whether it is an exception, and the
   !> pointer kind of its content.
   !> Polls first: a Return that never comes is the interesting failure
   !> in these cases, and reading a socket that will never speak would
   !> hang the suite instead of failing it.
   subroutine recv_answer(ansid, is_exc, kind, e)
      integer(int64), intent(out) :: ansid
      logical, intent(out) :: is_exc
      integer, intent(out) :: kind, e
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(return_t) :: r
      type(payload_t) :: pl
      type(capnp_ptr_t) :: content
      logical :: readable
      ansid = -1_int64
      is_exc = .false.
      kind = CAPNP_PK_NULL
      call px_poll_in(cli%fd, 200, readable, e)
      if (e /= CAPNP_OK) return
      if (.not. readable) then
         e = CAPNP_ERR_IO
         return
      end if
      call rpc_recv_message(cli%fd, m, e)
      if (e /= CAPNP_OK) return
      msg = message_read_root(m, e)
      if (e /= CAPNP_OK) return
      if (message_which(msg) /= MESSAGE_RETURN_TAG) then
         e = CAPNP_ERR_KIND
         call capnp_message_free(m)
         return
      end if
      r = message_return_get(msg, e)
      if (e /= CAPNP_OK) return
      ansid = return_answer_id_get(r)
      is_exc = return_which(r) /= RETURN_RESULTS_TAG
      if (.not. is_exc) then
         pl = return_results_get(r, e)
         if (e /= CAPNP_OK) return
         content = payload_content_get(pl, e)
         if (e /= CAPNP_OK) return
         kind = content%kind
      end if
      e = CAPNP_OK
      call capnp_message_free(m)
   end subroutine recv_answer

   !> A capability provided for a third vat is claimable exactly once.
   subroutine t_provide_and_accept(error)
      type(error_type), allocatable, intent(inout) :: error
      integer :: a, b, n, kind
      integer(int64) :: ansid
      integer(int64), parameter :: nonce = int(z'FEEDFACE', int64)
      logical :: is_exc
      call two_live_exports(a, b, n)
      call check_(error, a >= 0, 'provide: server has a live export')
      if (allocated(error)) return

      call send_provide(80_int64, a, nonce, err)
      call check_(error, err == CAPNP_OK, 'provide: sent')
      call rpc_pump_once(srv, err)
      call check_(error, err == CAPNP_OK, 'provide: server handled')
      call recv_answer(ansid, is_exc, kind, err)
      call check_(error, err == CAPNP_OK .and. ansid == 80_int64 .and. .not. is_exc, &
                  'provide: answered without exception')
      call check_(error, rpc_pending_provisions(srv) == 1, &
                  'provide: arrangement recorded')

      call send_accept(81_int64, nonce, err)
      call rpc_pump_once(srv, err)
      call recv_answer(ansid, is_exc, kind, err)
      call check_(error, err == CAPNP_OK .and. ansid == 81_int64 .and. .not. is_exc, &
                  'accept: answered without exception')
      ! What comes back is a capability, not a struct.
      call check_(error, kind == CAPNP_PK_CAP, 'accept: content is a capability')
      call check_(error, rpc_pending_provisions(srv) == 0, &
                  'accept: arrangement consumed')

      ! A nonce is single-use: leaving it claimable would let anyone who
      ! learned it take the capability again.
      call send_accept(82_int64, nonce, err)
      call rpc_pump_once(srv, err)
      call recv_answer(ansid, is_exc, kind, err)
      call check_(error, err == CAPNP_OK .and. ansid == 82_int64 .and. is_exc, &
                  'accept: replay refused')
   end subroutine t_provide_and_accept

   !> An Accept naming a nonce nobody arranged is refused, even while a
   !> different arrangement is standing: matching is on the nonce, not on
   !> the mere existence of something to hand over.
   subroutine t_accept_unknown_nonce(error)
      type(error_type), allocatable, intent(inout) :: error
      integer :: a, b, n, kind
      integer(int64) :: ansid
      integer(int64), parameter :: arranged = int(z'C0FFEE', int64)
      logical :: is_exc
      call two_live_exports(a, b, n)
      call check_(error, a >= 0, 'accept: server has a live export')
      if (allocated(error)) return

      call send_provide(84_int64, a, arranged, err)
      call rpc_pump_once(srv, err)
      call recv_answer(ansid, is_exc, kind, err)
      call check_(error, err == CAPNP_OK .and. .not. is_exc, 'accept: setup provide')

      call send_accept(85_int64, int(z'DEADBEEF', int64), err)
      call rpc_pump_once(srv, err)
      call recv_answer(ansid, is_exc, kind, err)
      call check_(error, err == CAPNP_OK .and. ansid == 85_int64 .and. is_exc, &
                  'accept: unknown nonce refused')
      call check_(error, rpc_pending_provisions(srv) == 1, &
                  'accept: the standing arrangement is untouched')

      ! Clear it, so later cases start from an empty table.
      call send_accept(87_int64, arranged, err)
      call rpc_pump_once(srv, err)
      call recv_answer(ansid, is_exc, kind, err)
      call check_(error, err == CAPNP_OK .and. .not. is_exc, 'accept: arranged nonce claimed')
   end subroutine t_accept_unknown_nonce

   !> Providing a capability the vat does not host is refused, and leaves
   !> no arrangement behind.
   subroutine t_provide_unhosted(error)
      type(error_type), allocatable, intent(inout) :: error
      integer :: kind, before
      integer(int64) :: ansid
      logical :: is_exc
      before = rpc_pending_provisions(srv)
      call send_provide(86_int64, size(srv%exports) - 1, 1234_int64, err)
      call rpc_pump_once(srv, err)
      call recv_answer(ansid, is_exc, kind, err)
      call check_(error, err == CAPNP_OK .and. ansid == 86_int64 .and. is_exc, &
                  'provide: unhosted target refused')
      call check_(error, rpc_pending_provisions(srv) == before, &
                  'provide: nothing recorded for a refused target')
   end subroutine t_provide_unhosted

   !> Two arrangements over the same capability are independent: claiming
   !> one leaves the other standing.
   subroutine t_two_provisions_independent(error)
      type(error_type), allocatable, intent(inout) :: error
      integer :: a, b, n, kind, before
      integer(int64) :: ansid
      logical :: is_exc
      call two_live_exports(a, b, n)
      if (allocated(error)) return
      before = rpc_pending_provisions(srv)

      call send_provide(90_int64, a, int(z'AAA', int64), err)
      call rpc_pump_once(srv, err)
      call recv_answer(ansid, is_exc, kind, err)
      call send_provide(91_int64, a, int(z'BBB', int64), err)
      call rpc_pump_once(srv, err)
      call recv_answer(ansid, is_exc, kind, err)
      call check_(error, rpc_pending_provisions(srv) == before + 2, &
                  'provide: two arrangements recorded')

      call send_accept(92_int64, int(z'AAA', int64), err)
      call rpc_pump_once(srv, err)
      call recv_answer(ansid, is_exc, kind, err)
      call check_(error, err == CAPNP_OK .and. .not. is_exc, 'accept: first claimed')
      call check_(error, rpc_pending_provisions(srv) == before + 1, &
                  'accept: the other arrangement stands')

      call send_accept(93_int64, int(z'BBB', int64), err)
      call rpc_pump_once(srv, err)
      call recv_answer(ansid, is_exc, kind, err)
      call check_(error, err == CAPNP_OK .and. .not. is_exc, 'accept: second claimed')
      call check_(error, rpc_pending_provisions(srv) == before, &
                  'accept: both arrangements consumed')
   end subroutine t_two_provisions_independent

   !> Level 2 persistence hook: Persistent.save on the bootstrap cap
   !> answers an application-defined SturdyRef.
   subroutine t_persistent_save(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_cap_t) :: bootcap
      type(capnp_message_t), target :: m, owner_msg
      type(capnp_ptr_t) :: owner, sturdy_ref, save_results
      integer(int64) :: qb, qs
      character(len=:), allocatable :: ref
      call rpc_bootstrap_send(cli, bootcap, err)
      qb = bootcap%id
      call rpc_pump_once(srv, err)

      ! Persistent<SturdyRef, Owner>.save carries an application-defined
      ! owner in SaveParams.sealFor.
      call capnp_message_init_builder(owner_msg, err)
      owner = capnp_new_struct(owner_msg, 0, 1, err)
      call capnp_set_root(owner_msg, owner, err)
      call capnp_set_text(owner, 0, 'alice', err)
      call rpc_persistent_save_begin(cli, bootcap, m, owner, qs, err)
      call rpc_call_send(cli, m, err)
      call rpc_pump_once(srv, err)
      call rpc_wait(cli, qb, err)
      call rpc_persistent_save_wait(cli, qs, sturdy_ref, err)
      call check_(error, err == CAPNP_OK, 'rpc: save returned')
      call check_(error, sturdy_ref%kind == CAPNP_PK_LIST, 'rpc: sturdy ref pointer returned')
      call rpc_result_content(cli, qs, save_results, err)
      call capnp_get_text(save_results, 0, ref, err)
      call check_(error, err == CAPNP_OK .and. ref == 'sturdy:echo-main:alice', &
                  'rpc: sealed sturdy ref')
      call rpc_finish_send(cli, qb, .true., err)
      call rpc_pump_once(srv, err)
      call rpc_finish_send(cli, qs, .false., err)
      call rpc_pump_once(srv, err)
      call capnp_message_free(owner_msg)
   end subroutine t_persistent_save

   !> Resolve messages come back as unimplemented (the vat keeps using
   !> promise paths); sendResultsTo.yourself calls fail cleanly; a
   !> takeFromOtherQuestion Return surfaces as an exception, not a
   !> kind error.
   subroutine t_resolve_and_tail_calls(error)
      type(error_type), allocatable, intent(inout) :: error
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(resolve_t) :: rv
      type(call_t) :: c
      type(message_target_t) :: tgt
      type(return_t) :: r
      type(rpc_cap_t) :: bootcap
      type(payload_t) :: params
      type(capnp_ptr_t) :: s, content
      integer(int64) :: qb, qy

      ! Resolve -> unimplemented reply, absorbed by the sender.
      call capnp_message_init_builder(m, err)
      msg = message_new_root(m, err)
      rv = message_resolve_init(msg, err)
      call resolve_promise_id_set(rv, 7_int64, err)
      call rpc_send_message(cli%fd, m, err)
      call capnp_message_free(m)
      call rpc_pump_once(srv, err)
      call check_(error, err == CAPNP_OK, 'rpc: resolve answered')
      call rpc_pump_once(cli, err)
      call check_(error, err == CAPNP_OK, 'rpc: resolve unimplemented absorbed')

      ! sendResultsTo.yourself -> clean exception return.
      call rpc_bootstrap_send(cli, bootcap, err)
      qb = bootcap%id
      call rpc_pump_once(srv, err)
      call rpc_call_begin(cli, bootcap, ECHO_IFACE, 0, m, params, qy, err)
      msg = message_read_root(m, err)
      c = message_call_get(msg, err)
      call call_send_results_to_yourself_set(c, err)
      s = capnp_new_struct(m, 1, 1, err)
      call capnp_set_i64(s, 0_int64, 1_int64, err)
      call capnp_set_text(s, 0, 'x', err)
      call payload_content_set(params, s, err)
      call rpc_call_send(cli, m, err)
      call rpc_pump_once(srv, err)
      call rpc_wait(cli, qb, err)
      call rpc_wait(cli, qy, err)
      call rpc_result_content(cli, qy, content, err)
      call check_(error, err == RPC_ERR_EXCEPTION, 'rpc: sendResultsTo.yourself raises')
      call rpc_finish_send(cli, qb, .true., err)
      call rpc_pump_once(srv, err)
      call rpc_finish_send(cli, qy, .false., err)
      call rpc_pump_once(srv, err)

      ! takeFromOtherQuestion Return -> exception with reason, not
      ! ERR_KIND. The peer (impersonated on the raw fd) redirects a live
      ! question.
      call rpc_call_begin(cli, bootcap, ECHO_IFACE, 0, m, params, qy, err)
      s = capnp_new_struct(m, 1, 1, err)
      call capnp_set_i64(s, 0_int64, 1_int64, err)
      call capnp_set_text(s, 0, 'x', err)
      call payload_content_set(params, s, err)
      call capnp_message_free(m) ! never sent; hand-craft the Return instead
      call capnp_message_init_builder(m, err)
      msg = message_new_root(m, err)
      r = message_return_init(msg, err)
      call return_answer_id_set(r, qy, err)
      call return_take_from_other_question_set(r, 0_int64, err)
      call rpc_send_message(srv%fd, m, err)
      call capnp_message_free(m)
      call rpc_wait(cli, qy, err)
      call check_(error, err == CAPNP_OK, 'rpc: tail-call return received')
      call rpc_result_content(cli, qy, content, err)
      call check_(error, err == RPC_ERR_EXCEPTION, 'rpc: tail-call return raises cleanly')
      call check_(error, index(rpc_conn_reason(cli), 'tail-call') > 0, 'rpc: tail-call reason')
      call rpc_finish_send(cli, qy, .false., err)
      call rpc_pump_once(srv, err)
   end subroutine t_resolve_and_tail_calls

   !> A senderPromise capTable entry settles into a usable import, per
   !> the continue-using-the-promise allowance.
   subroutine t_sender_promise_import(error)
      type(error_type), allocatable, intent(inout) :: error
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(return_t) :: r
      type(payload_t) :: pl
      type(cap_descriptor_t) :: cd
      type(capnp_ptr_t) :: ctab
      type(rpc_cap_t) :: bootcap, cap
      integer(int64) :: qb

      ! Impersonate the peer: swallow the bootstrap off the raw fd and
      ! answer it with a senderPromise capability.
      call rpc_bootstrap_send(cli, bootcap, err)
      qb = bootcap%id
      block
         type(capnp_message_t), target :: drain
         call rpc_recv_message(srv%fd, drain, err)
         call capnp_message_free(drain)
      end block
      call capnp_message_init_builder(m, err)
      msg = message_new_root(m, err)
      r = message_return_init(msg, err)
      call return_answer_id_set(r, qb, err)
      pl = return_results_init(r, err)
      call payload_content_set(pl, rpc_make_cap_ptr(m, 0), err)
      ctab = payload_cap_table_init(pl, 1_int64, err)
      cd%p = capnp_list_get_struct(ctab, 0, err)
      call cap_descriptor_sender_promise_set(cd, 5_int64, err)
      call rpc_send_message(srv%fd, m, err)
      call capnp_message_free(m)
      call rpc_wait(cli, qb, err)
      call rpc_result_cap(cli, qb, [integer ::], cap, err)
      call check_(error, err == CAPNP_OK .and. cap%kind == RPC_CAP_IMPORT .and. &
                  cap%id == 5_int64, 'rpc: senderPromise settles as import')
      call rpc_finish_send(cli, qb, .true., err)
      call rpc_pump_once(srv, err)
   end subroutine t_sender_promise_import

   !> Level 1 embargo: a senderLoopback Disembargo is answered by the
   !> peer as receiverLoopback with the same id and importedCap target
   !> (handle_disembargo on the pumped vat).
   subroutine t_disembargo_echo(error)
      type(error_type), allocatable, intent(inout) :: error
      type(capnp_message_t), target :: m, reply
      type(message_t) :: msg, rmsg
      type(disembargo_t) :: d, rd
      type(message_target_t) :: tgt, rtgt
      integer(int64), parameter :: emb_id = 99_int64
      integer(int64), parameter :: import_id = 3_int64
      call capnp_message_init_builder(m, err)
      msg = message_new_root(m, err)
      d = message_disembargo_init(msg, err)
      call disembargo_context_sender_loopback_set(d, emb_id, err)
      tgt = disembargo_target_init(d, err)
      call message_target_imported_cap_set(tgt, import_id, err)
      call rpc_send_message(cli%fd, m, err)
      call capnp_message_free(m)
      call check_(error, err == CAPNP_OK, 'rpc: disembargo sent')
      call rpc_pump_once(srv, err)
      call check_(error, err == CAPNP_OK, 'rpc: server handled disembargo')
      call rpc_recv_message(cli%fd, reply, err)
      call check_(error, err == CAPNP_OK, 'rpc: disembargo echo received')
      rmsg = message_read_root(reply, err)
      call check_(error, message_which(rmsg) == MESSAGE_DISEMBARGO_TAG, &
                  'rpc: echo is Disembargo')
      rd = message_disembargo_get(rmsg, err)
      call check_(error, disembargo_context_which(rd) == &
                  DISEMBARGO_CONTEXT_RECEIVER_LOOPBACK_TAG, &
                  'rpc: receiverLoopback context')
      call check_(error, disembargo_context_receiver_loopback_get(rd) == emb_id, &
                  'rpc: embargo id echoed')
      rtgt = disembargo_target_get(rd, err)
      call check_(error, message_target_which(rtgt) == MESSAGE_TARGET_IMPORTED_CAP_TAG, &
                  'rpc: echo target is importedCap')
      call check_(error, message_target_imported_cap_get(rtgt) == import_id, &
                  'rpc: import id echoed')
      call capnp_message_free(reply)
   end subroutine t_disembargo_echo

   !> senderLoopback with a promisedAnswer target is echoed the same way
   !> (question id preserved on the reply target).
   subroutine t_disembargo_promised_answer(error)
      type(error_type), allocatable, intent(inout) :: error
      type(capnp_message_t), target :: m, reply
      type(message_t) :: msg, rmsg
      type(disembargo_t) :: d, rd
      type(message_target_t) :: tgt, rtgt
      type(promised_answer_t) :: pa, rpa
      integer(int64), parameter :: emb_id = 77_int64
      integer(int64), parameter :: qid = 42_int64
      call capnp_message_init_builder(m, err)
      msg = message_new_root(m, err)
      d = message_disembargo_init(msg, err)
      call disembargo_context_sender_loopback_set(d, emb_id, err)
      tgt = disembargo_target_init(d, err)
      pa = message_target_promised_answer_init(tgt, err)
      call promised_answer_question_id_set(pa, qid, err)
      call rpc_send_message(cli%fd, m, err)
      call capnp_message_free(m)
      call check_(error, err == CAPNP_OK, 'rpc: pa disembargo sent')
      call rpc_pump_once(srv, err)
      call check_(error, err == CAPNP_OK, 'rpc: server handled pa disembargo')
      call rpc_recv_message(cli%fd, reply, err)
      call check_(error, err == CAPNP_OK, 'rpc: pa disembargo echo received')
      rmsg = message_read_root(reply, err)
      call check_(error, message_which(rmsg) == MESSAGE_DISEMBARGO_TAG, &
                  'rpc: pa echo is Disembargo')
      rd = message_disembargo_get(rmsg, err)
      call check_(error, disembargo_context_which(rd) == &
                  DISEMBARGO_CONTEXT_RECEIVER_LOOPBACK_TAG, &
                  'rpc: pa receiverLoopback')
      call check_(error, disembargo_context_receiver_loopback_get(rd) == emb_id, &
                  'rpc: pa embargo id echoed')
      rtgt = disembargo_target_get(rd, err)
      call check_(error, message_target_which(rtgt) == MESSAGE_TARGET_PROMISED_ANSWER_TAG, &
                  'rpc: pa echo target kind')
      rpa = message_target_promised_answer_get(rtgt, err)
      call check_(error, promised_answer_question_id_get(rpa) == qid, &
                  'rpc: pa question id echoed')
      call capnp_message_free(reply)
   end subroutine t_disembargo_promised_answer

   !> A receiverLoopback Disembargo is accepted (no error) and does not
   !> produce a further echo; the vat stays usable for a later bootstrap.
   subroutine t_disembargo_receiver_loopback_absorbed(error)
      type(error_type), allocatable, intent(inout) :: error
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(disembargo_t) :: d
      type(message_target_t) :: tgt
      type(rpc_cap_t) :: bootcap
      integer(int64) :: qb
      logical :: handled
      call capnp_message_init_builder(m, err)
      msg = message_new_root(m, err)
      d = message_disembargo_init(msg, err)
      call disembargo_context_receiver_loopback_set(d, 1_int64, err)
      tgt = disembargo_target_init(d, err)
      call message_target_imported_cap_set(tgt, 0_int64, err)
      call rpc_send_message(cli%fd, m, err)
      call capnp_message_free(m)
      call check_(error, err == CAPNP_OK, 'rpc: receiverLoopback sent')
      call rpc_pump_once(srv, err)
      call check_(error, err == CAPNP_OK, 'rpc: receiverLoopback absorbed')
      ! No echo should be pending: a short poll on the client stays quiet.
      call rpc_pump_poll(cli, 20, handled, err)
      call check_(error, err == CAPNP_OK .and. .not. handled, &
                  'rpc: no echo after receiverLoopback')
      ! Connection still works.
      call rpc_bootstrap_send(cli, bootcap, err)
      qb = bootcap%id
      call rpc_pump_once(srv, err)
      call rpc_wait(cli, qb, err)
      call check_(error, err == CAPNP_OK, 'rpc: bootstrap after receiverLoopback')
      call rpc_finish_send(cli, qb, .true., err)
      call rpc_pump_once(srv, err)
   end subroutine t_disembargo_receiver_loopback_absorbed

   !> Poll-driven pumping: a quiet connection times out with
   !> handled=.false.; a pending message is handled within the window.
   subroutine t_pump_poll(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_cap_t) :: bootcap
      logical :: handled
      call rpc_pump_poll(srv, 10, handled, err)
      call check_(error, err == CAPNP_OK .and. .not. handled, 'poll: quiet times out')
      call rpc_bootstrap_send(cli, bootcap, err)
      call rpc_pump_poll(srv, 1000, handled, err)
      call check_(error, err == CAPNP_OK .and. handled, 'poll: pending message handled')
      call rpc_wait(cli, bootcap%id, err)
      call check_(error, err == CAPNP_OK, 'poll: answer arrived')
      call rpc_finish_send(cli, bootcap%id, .true., err)
      call rpc_pump_once(srv, err)
   end subroutine t_pump_poll

   !> Send bytes and close the write side, so a reader that wrongly asks
   !> for more sees EOF (CAPNP_ERR_IO) and fails its check instead of
   !> blocking the suite.
   subroutine send_closed(fd, bytes, e)
      integer, intent(in) :: fd
      integer(int8), intent(in) :: bytes(:)
      integer, intent(out) :: e
      call px_send_all(fd, bytes, e)
      call px_shutdown_wr(fd)
   end subroutine send_closed

   !> Cap'n Proto C++ AsyncMessageReader accepts firstWord[0] == 510
   !> (511 segments).
   subroutine t_framing_511_accepted(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: src, dst
      type(capnp_ptr_t) :: s
      integer(int8), allocatable :: single(:), frame(:)
      integer(int64) :: header_bytes, seg0_bytes, seg0_words, extra_bytes
      integer :: fa, fb, e, i
      call capnp_message_init_builder(src, e)
      call check_(error, e == CAPNP_OK, 'framing 511: init builder')
      s = capnp_new_struct(src, 1, 0, e)
      call capnp_set_i64(s, 0_int64, 424242_int64, e)
      call capnp_set_root(src, s, e)
      call capnp_serialize_bytes(src, single, e)
      call check_(error, e == CAPNP_OK, 'framing 511: serialize base')
      if (allocated(error)) then
         call capnp_message_free(src)
         return
      end if

      header_bytes = 2048_int64
      seg0_bytes = size(single, kind=int64) - 8_int64
      seg0_words = seg0_bytes/CAPNP_WORD_BYTES
      extra_bytes = 510_int64*CAPNP_WORD_BYTES
      allocate (frame(0:header_bytes + seg0_bytes + extra_bytes - 1_int64))
      frame = 0_int8
      call cp_put_i32(frame, 0_int64, 510_int32)
      call cp_put_i32(frame, 4_int64, int(seg0_words, int32))
      do i = 2, 511
         call cp_put_i32(frame, int(i, int64)*4_int64, 1_int32)
      end do
      frame(header_bytes:header_bytes + seg0_bytes - 1_int64) = &
         single(8:8 + seg0_bytes - 1_int64)

      call px_socketpair(fa, fb, e)
      call check_(error, e == CAPNP_OK, 'framing 511: socketpair')
      if (.not. allocated(error)) then
         call send_closed(fa, frame, e)
         call check_(error, e == CAPNP_OK, 'framing 511: send frame')
         call rpc_recv_message(fb, dst, e)
         call check_(error, e == CAPNP_OK, 'framing 511: recv accepted')
         call check_(error, dst%nsegs == 511, 'framing 511: segment count')
         if (.not. allocated(error)) then
            s = capnp_root(dst, e)
            call check_(error, e == CAPNP_OK, 'framing 511: read root')
            call check_(error, capnp_get_i64(s, 0_int64) == 424242_int64, &
                        'framing 511: payload intact')
         end if
      end if
      call capnp_message_free(src)
      call capnp_message_free(dst)
      call px_close(fa)
      call px_close(fb)
   end subroutine t_framing_511_accepted

   !> Cap'n Proto C++ AsyncMessageReader rejects firstWord[0] >= 511
   !> (512 segments and u32 overflow) before reading moreSizes or allocating.
   subroutine t_framing_512_rejected(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t) :: dst
      integer(int8) :: head(0:7)
      integer :: fa, fb, e

      call px_socketpair(fa, fb, e)
      call check_(error, e == CAPNP_OK, 'framing 512: socketpair')
      if (.not. allocated(error)) then
         head = 0_int8
         call cp_put_i32(head, 0_int64, 511_int32)
         call cp_put_i32(head, 4_int64, 1_int32)
         call send_closed(fa, head, e)
         call check_(error, e == CAPNP_OK, 'framing 512: send header word')
         call rpc_recv_message(fb, dst, e)
         call check_(error, e == CAPNP_ERR_FRAMING, 'framing 512: rejected')
         call check_(error, dst%nsegs == 0, 'framing 512: message empty')
      end if
      call capnp_message_free(dst)
      call px_close(fa)
      call px_close(fb)

      if (allocated(error)) return
      call px_socketpair(fa, fb, e)
      call check_(error, e == CAPNP_OK, 'framing u32max: socketpair')
      if (.not. allocated(error)) then
         head = 0_int8
         call cp_put_i32(head, 0_int64, -1_int32)
         call cp_put_i32(head, 4_int64, 0_int32)
         call send_closed(fa, head, e)
         call check_(error, e == CAPNP_OK, 'framing u32max: send header word')
         call rpc_recv_message(fb, dst, e)
         call check_(error, e == CAPNP_ERR_FRAMING, 'framing u32max: rejected')
         call check_(error, dst%nsegs == 0, 'framing u32max: message empty')
      end if
      call capnp_message_free(dst)
      call px_close(fa)
      call px_close(fb)
   end subroutine t_framing_512_rejected

   !> Cap'n Proto C++ AsyncMessageReader rejects totalWords above
   !> traversalLimitInWords before allocating or reading the body.
   subroutine t_framing_oversized_rejected(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: src, dst
      type(capnp_ptr_t) :: s
      integer(int8) :: head(0:7), hdr2(0:15)
      integer(int8), allocatable :: wire(:)
      integer :: fa, fb, e

      ! Single segment one word past the 64 MiB default traversal limit.
      call px_socketpair(fa, fb, e)
      call check_(error, e == CAPNP_OK, 'oversized default+1: socketpair')
      if (.not. allocated(error)) then
         head = 0_int8
         call cp_put_i32(head, 0_int64, 0_int32)
         call cp_put_i32(head, 4_int64, int(CAPNP_DEFAULT_TRAVERSAL_WORDS + 1_int64, int32))
         call send_closed(fa, head, e)
         call rpc_recv_message(fb, dst, e)
         call check_(error, e == CAPNP_ERR_TRAVERSAL, 'oversized default+1: rejected')
         call check_(error, dst%nsegs == 0, 'oversized default+1: message empty')
      end if
      call capnp_message_free(dst)
      call px_close(fa)
      call px_close(fb)
      if (allocated(error)) return

      ! Single segment claiming 2^32 - 1 words (32 GiB): must fail before allocation.
      call px_socketpair(fa, fb, e)
      call check_(error, e == CAPNP_OK, 'oversized u32max: socketpair')
      if (.not. allocated(error)) then
         head = 0_int8
         call cp_put_i32(head, 0_int64, 0_int32)
         call cp_put_i32(head, 4_int64, -1_int32)
         call send_closed(fa, head, e)
         call rpc_recv_message(fb, dst, e)
         call check_(error, e == CAPNP_ERR_TRAVERSAL, 'oversized u32max: rejected')
         call check_(error, dst%nsegs == 0, 'oversized u32max: message empty')
      end if
      call capnp_message_free(dst)
      call px_close(fa)
      call px_close(fb)
      if (allocated(error)) return

      ! Two segments each under the limit whose sum exceeds it.
      call px_socketpair(fa, fb, e)
      call check_(error, e == CAPNP_OK, 'oversized sum: socketpair')
      if (.not. allocated(error)) then
         hdr2 = 0_int8
         call cp_put_i32(hdr2, 0_int64, 1_int32)
         call cp_put_i32(hdr2, 4_int64, 4194305_int32)
         call cp_put_i32(hdr2, 8_int64, 4194305_int32)
         call send_closed(fa, hdr2, e)
         call rpc_recv_message(fb, dst, e)
         call check_(error, e == CAPNP_ERR_TRAVERSAL, 'oversized sum: rejected')
         call check_(error, dst%nsegs == 0, 'oversized sum: message empty')
      end if
      call capnp_message_free(dst)
      call px_close(fa)
      call px_close(fb)
      if (allocated(error)) return

      ! Exact boundary: the frame check is totalWords <= limit, so a 3-word
      ! body is rejected at limit 2 and framed at limit 3. Reading the root
      ! then charges the root word twice plus the 2-word struct, so limit 3
      ! fails there and limit 4 reads.
      call capnp_message_init_builder(src, e)
      s = capnp_new_struct(src, 2, 0, e)
      call capnp_set_i64(s, 0_int64, 11_int64, e)
      call capnp_set_i64(s, 8_int64, 22_int64, e)
      call capnp_set_root(src, s, e)
      call capnp_serialize_bytes(src, wire, e)
      call check_(error, e == CAPNP_OK .and. size(wire, kind=int64) == 32_int64, &
                  'oversized boundary: 3-word message serialized')
      if (.not. allocated(error)) then
         call px_socketpair(fa, fb, e)
         call send_closed(fa, wire(0:7), e)
         call rpc_recv_message(fb, dst, e, traversal_words=2_int64)
         call check_(error, e == CAPNP_ERR_TRAVERSAL, 'oversized boundary: limit 2 rejects 3 words')
         call px_close(fa)
         call px_close(fb)

         call px_socketpair(fa, fb, e)
         call send_closed(fa, wire, e)
         call rpc_recv_message(fb, dst, e, traversal_words=3_int64)
         call check_(error, e == CAPNP_OK, 'oversized boundary: limit 3 frames 3 words')
         if (.not. allocated(error)) then
            s = capnp_root(dst, e)
            call check_(error, e == CAPNP_ERR_TRAVERSAL, &
                        'oversized boundary: limit 3 fails at root read')
         end if
         call px_close(fa)
         call px_close(fb)

         call px_socketpair(fa, fb, e)
         call send_closed(fa, wire, e)
         call rpc_recv_message(fb, dst, e, traversal_words=4_int64)
         call check_(error, e == CAPNP_OK, 'oversized boundary: limit 4 frames 3 words')
         if (.not. allocated(error)) then
            s = capnp_root(dst, e)
            call check_(error, e == CAPNP_OK .and. capnp_get_i64(s, 8_int64) == 22_int64, &
                        'oversized boundary: limit 4 payload intact')
         end if
         call px_close(fa)
         call px_close(fb)
      end if
      call capnp_message_free(src)
      call capnp_message_free(dst)
   end subroutine t_framing_oversized_rejected

   !> C++ exposes the traversal budget as uint64_t; reject an invalid
   !> signed Fortran value at this API boundary rather than treating it as
   !> a limit that rejects every valid frame.
   subroutine t_framing_negative_traversal_rejected(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t) :: dst
      integer(int8) :: head(0:7), unread(0:7)
      integer :: fa, fb, e

      call px_socketpair(fa, fb, e)
      call check_(error, e == CAPNP_OK, 'negative traversal: socketpair')
      if (.not. allocated(error)) then
         head = 0_int8
         call send_closed(fa, head, e)
         call rpc_recv_message(fb, dst, e, traversal_words=-1_int64)
         call check_(error, e == CAPNP_ERR_ARG, 'negative traversal: argument rejected')
         call check_(error, dst%nsegs == 0, 'negative traversal: message empty')
         unread = 1_int8
         call px_recv_all(fb, unread, e)
         call check_(error, e == CAPNP_OK .and. all(unread == 0_int8), &
                     'negative traversal: header left unread')
      end if
      call capnp_message_free(dst)
      call px_close(fa)
      call px_close(fb)
   end subroutine t_framing_negative_traversal_rejected

   !> Feed raw bytes to rpc_recv_message over a socketpair, closing the
   !> sender afterwards so a short frame reads as EOF.
   subroutine recv_raw(bytes, dst, e)
      integer(int8), intent(in) :: bytes(:)
      type(capnp_message_t), intent(inout) :: dst
      integer, intent(out) :: e
      integer :: fa, fb, se
      call px_socketpair(fa, fb, se)
      if (se /= CAPNP_OK) then
         e = se
         return
      end if
      if (size(bytes) > 0) call px_send_all(fa, bytes, se)
      call px_close(fa)
      call rpc_recv_message(fb, dst, e)
      call px_close(fb)
   end subroutine recv_raw

   !> AsyncMessageReader reads the odd-count padding word without looking
   !> at it, and any EOF inside the header or body is a disconnect.
   subroutine t_framing_short_and_padded(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t) :: dst
      integer(int8) :: buf(0:39)
      integer :: e

      buf = 0_int8
      call cp_put_i32(buf, 0_int64, 1_int32)
      call cp_put_i32(buf, 4_int64, 1_int32)
      call cp_put_i32(buf, 8_int64, 1_int32)
      call cp_put_i32(buf, 12_int64, -559038737_int32)
      call recv_raw(buf, dst, e)
      call check_(error, e == CAPNP_OK, 'short/padded: nonzero pad word accepted')
      call check_(error, dst%nsegs == 2, 'short/padded: two segments read')

      call recv_raw(buf(0:0), dst, e)
      call check_(error, e == CAPNP_ERR_IO, 'short/padded: EOF in first word')
      call check_(error, dst%nsegs == 0, 'short/padded: EOF in first word leaves msg empty')

      call recv_raw(buf(0:-1), dst, e)
      call check_(error, e == CAPNP_ERR_IO, 'short/padded: EOF before any byte')

      call recv_raw(buf(0:11), dst, e)
      call check_(error, e == CAPNP_ERR_IO, 'short/padded: EOF inside the size table')
      call check_(error, dst%nsegs == 0, 'short/padded: EOF inside the size table leaves msg empty')

      call recv_raw(buf(0:23), dst, e)
      call check_(error, e == CAPNP_ERR_IO, 'short/padded: EOF inside the body')
      call check_(error, dst%nsegs == 0, 'short/padded: EOF inside the body leaves msg empty')

      buf = 0_int8
      call recv_raw(buf(0:7), dst, e)
      call check_(error, e == CAPNP_OK, 'short/padded: empty one-segment frame is framed')
      call check_(error, dst%nsegs == 1, 'short/padded: empty frame segment count')
      call capnp_message_free(dst)
   end subroutine t_framing_short_and_padded

   subroutine deny_dial(host, port, allow)
      character(len=*), intent(in) :: host
      integer, intent(in) :: port
      logical, intent(out) :: allow
      allow = .false.
      if (port < 0 .and. len(host) < 0) allow = .true.
   end subroutine deny_dial

   subroutine allow_local_dial(host, port, allow)
      character(len=*), intent(in) :: host
      integer, intent(in) :: port
      logical, intent(out) :: allow
      allow = host == '127.0.0.1' .and. port > 0
   end subroutine allow_local_dial

   !> A dial hook can refuse an introduction, and a timeout returns
   !> instead of blocking. The default hook still reaches a local listener.
   subroutine t_dial_guard(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: holder, introduced
      class(rpc_server_t), pointer :: boot
      integer :: fa, fb, lfd, port, acc, e
      logical :: readable
      call px_socketpair(fa, fb, e)
      call px_tcp_listen(0, lfd, e)
      call px_tcp_bound_port(lfd, port, e)
      call check_(error, e == CAPNP_OK .and. port > 0, 'dial: listener')
      if (allocated(error)) return
      boot => null()
      call rpc_conn_init(holder, fa, boot)
      holder%introductions(0)%used = .true.
      holder%introductions(0)%nonce = 55_int64
      holder%introductions(0)%host = '127.0.0.1'
      holder%introductions(0)%host_len = 9
      holder%introductions(0)%port = port
      holder%dial_allow => deny_dial
      call rpc_introduction_dial(holder, 55_int64, introduced, boot, e)
      call check_(error, e == CAPNP_ERR_ARG, 'dial: hook refusal is an argument error')
      call px_poll_in(lfd, 100, readable, e)
      call check_(error, e == CAPNP_OK .and. .not. readable, 'dial: refused hook does not connect')

      holder%dial_allow => allow_local_dial
      call rpc_introduction_dial(holder, 55_int64, introduced, boot, e)
      call check_(error, e == CAPNP_OK, 'dial: allowed local connect')
      call px_poll_in(lfd, 500, readable, e)
      call check_(error, readable, 'dial: listener sees the allowed connect')
      call px_tcp_accept(lfd, acc, e)
      call px_close(acc)
      call rpc_conn_close(introduced)

      holder%introductions(0)%host = '192.0.2.1'
      holder%introductions(0)%port = 81
      holder%dial_allow => null()
      holder%dial_timeout_ms = 300
      call rpc_introduction_dial(holder, 55_int64, introduced, boot, e)
      call check_(error, e /= CAPNP_OK, 'dial: timeout or refusal returns')
      call rpc_conn_close(holder)
      call px_close(fb)
      call px_close(lfd)
   end subroutine t_dial_guard

end module test_rpc
