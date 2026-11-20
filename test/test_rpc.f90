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

   type, extends(rpc_server_t) :: stage_srv_t
      class(*), pointer :: staged => null()
   contains
      procedure :: dispatch => stage_srv_dispatch
   end type stage_srv_t

   !> Records method ids in the order they are dispatched.
   type, extends(rpc_server_t) :: order_srv_t
      integer :: n = 0
      integer :: seen(4) = 0
   contains
      procedure :: dispatch => order_dispatch
   end type order_srv_t

   !> Returns the caller's own import as `receiverHosted`, the descriptor
   !> C++ writes when a result points back at the peer.
   type, extends(rpc_server_t) :: reflect_srv_t
   contains
      procedure :: dispatch => reflect_dispatch
   end type reflect_srv_t

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
   logical :: rpc_up = .false.

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
      ! Ordered cases. They share one socketpair, so a failure
      ! names the check and the later checks still run.
      allocate (testsuite(103))
      testsuite(1)%name = "rpc-bootstrap-and-echo"
      testsuite(1)%test => w_bootstrap_and_echo
      testsuite(2)%name = "rpc-cap-result-and-pipeline"
      testsuite(2)%test => w_cap_result_and_pipeline
      testsuite(3)%name = "rpc-exception"
      testsuite(3)%test => w_exception
      testsuite(4)%name = "rpc-unimplemented"
      testsuite(4)%test => w_unimplemented
      testsuite(5)%name = "rpc-persistent-save"
      testsuite(5)%test => w_persistent_save
      testsuite(6)%name = "rpc-resolve-and-tail-calls"
      testsuite(6)%test => w_resolve_and_tail_calls
      testsuite(7)%name = "rpc-sender-promise-import"
      testsuite(7)%test => w_sender_promise_import
      testsuite(8)%name = "rpc-disembargo-echo"
      testsuite(8)%test => w_disembargo_echo
      testsuite(9)%name = "rpc-disembargo-promised-answer"
      testsuite(9)%test => w_disembargo_promised_answer
      testsuite(10)%name = "rpc-disembargo-receiver-loopback-absorbed"
      testsuite(10)%test => w_disembargo_receiver_loopback_absorbed
      testsuite(11)%name = "rpc-pump-poll"
      testsuite(11)%test => w_pump_poll
      testsuite(12)%name = "rpc-provide-and-accept"
      testsuite(12)%test => w_provide_and_accept
      testsuite(13)%name = "rpc-accept-unknown-nonce"
      testsuite(13)%test => w_accept_unknown_nonce
      testsuite(14)%name = "rpc-provide-unhosted"
      testsuite(14)%test => w_provide_unhosted
      testsuite(15)%name = "rpc-two-provisions-independent"
      testsuite(15)%test => w_two_provisions_independent
      testsuite(16)%name = "rpc-accept-embargo"
      testsuite(16)%test => w_accept_embargo
      testsuite(17)%name = "rpc-third-party-introduction"
      testsuite(17)%test => w_third_party_introduction
      testsuite(18)%name = "rpc-introduction-in-an-answer"
      testsuite(18)%test => w_introduction_in_an_answer
      testsuite(19)%name = "rpc-overlong-host-refused"
      testsuite(19)%test => w_overlong_host_refused
      testsuite(20)%name = "rpc-reference-introduction"
      testsuite(20)%test => w_reference_introduction
      testsuite(21)%name = "rpc-reference-frames"
      testsuite(21)%test => w_reference_frames
      testsuite(22)%name = "rpc-join-same-capability"
      testsuite(22)%test => w_join_same_capability
      testsuite(23)%name = "rpc-join-unresolvable-part"
      testsuite(23)%test => w_join_unresolvable_part
      testsuite(24)%name = "rpc-join-all-parts-unresolvable"
      testsuite(24)%test => w_join_all_parts_unresolvable
      testsuite(25)%name = "rpc-join-client-api"
      testsuite(25)%test => w_join_client_api
      testsuite(26)%name = "rpc-join-incomplete-set-is-silent"
      testsuite(26)%test => w_join_incomplete_set_is_silent
      testsuite(27)%name = "rpc-join-finish-one-part"
      testsuite(27)%test => w_join_finish_one_part
      testsuite(28)%name = "rpc-join-send-unwinds"
      testsuite(28)%test => w_join_send_unwinds
      testsuite(29)%name = "rpc-join-finish-releases-cap"
      testsuite(29)%test => w_join_finish_releases_cap
      testsuite(30)%name = "rpc-abandoned-join-cleared"
      testsuite(30)%test => w_abandoned_join_cleared
      testsuite(31)%name = "rpc-finish-provide-qid"
      testsuite(31)%test => w_finish_provide_qid
      testsuite(32)%name = "rpc-request-cancellation"
      testsuite(32)%test => w_request_cancellation
      testsuite(33)%name = "rpc-cancel-reserves-qid"
      testsuite(33)%test => w_cancel_reserves_qid
      testsuite(34)%name = "rpc-send-failure-releases-qid"
      testsuite(34)%test => w_send_failure_releases_qid
      testsuite(35)%name = "rpc-call-send-failure-releases"
      testsuite(35)%test => w_call_send_failure_releases
      testsuite(36)%name = "rpc-malformed-root-is-freed"
      testsuite(36)%test => w_malformed_root_is_freed
      testsuite(37)%name = "rpc-membrane-unwrap-frees"
      testsuite(37)%test => w_membrane_unwrap_frees
      testsuite(38)%name = "rpc-provide-overrelease-keeps-server"
      testsuite(38)%test => w_provide_overrelease_keeps_server
      testsuite(39)%name = "rpc-embargo-finish-answers-recipient"
      testsuite(39)%test => w_embargo_finish_answers_recipient
      testsuite(40)%name = "rpc-accept-finish-releases-cap"
      testsuite(40)%test => w_accept_finish_releases_cap
      testsuite(41)%name = "rpc-accept-finish-after-embargo"
      testsuite(41)%test => w_accept_finish_after_embargo
      testsuite(42)%name = "rpc-embargo-recipient-close"
      testsuite(42)%test => w_embargo_recipient_close
      testsuite(43)%name = "rpc-membrane"
      testsuite(43)%test => w_membrane
      testsuite(44)%name = "rpc-export-table-full"
      testsuite(44)%test => w_export_table_full
      testsuite(45)%name = "rpc-pluggable-transport"
      testsuite(45)%test => w_pluggable_transport
      testsuite(46)%name = "rpc-dial-guard"
      testsuite(46)%test => w_dial_guard
      testsuite(47)%name = "rpc-question-id-duplicate"
      testsuite(47)%test => w_question_id_duplicate
      testsuite(48)%name = "rpc-question-id-out-of-range"
      testsuite(48)%test => w_question_id_out_of_range
      testsuite(49)%name = "rpc-duplicate-return"
      testsuite(49)%test => w_duplicate_return
      testsuite(50)%name = "rpc-take-from-normal-answer"
      testsuite(50)%test => w_take_from_normal_answer
      testsuite(51)%name = "rpc-resolve-to-hosted"
      testsuite(51)%test => w_resolve_to_hosted
      testsuite(52)%name = "rpc-resolve-none"
      testsuite(52)%test => w_resolve_none
      testsuite(53)%name = "rpc-resolve-non-promise"
      testsuite(53)%test => w_resolve_non_promise
      testsuite(54)%name = "rpc-resolve-exception"
      testsuite(54)%test => w_resolve_exception
      testsuite(55)%name = "rpc-resolve-embargo"
      testsuite(55)%test => w_resolve_embargo
      testsuite(56)%name = "rpc-join-duplicate-qid"
      testsuite(56)%test => w_join_duplicate_qid
      testsuite(57)%name = "rpc-disembargo-provide-wrong-conn"
      testsuite(57)%test => w_disembargo_provide_wrong_conn
      testsuite(58)%name = "rpc-third-party-tail-aborts"
      testsuite(58)%test => w_third_party_tail_aborts
      testsuite(59)%name = "rpc-yourself-splice"
      testsuite(59)%test => w_yourself_splice
      testsuite(60)%name = "rpc-release-live-over"
      testsuite(60)%test => w_release_live_over
      testsuite(61)%name = "rpc-call-order"
      testsuite(61)%test => w_call_order
      testsuite(62)%name = "rpc-call-missing-export"
      testsuite(62)%test => w_call_missing_export
      testsuite(63)%name = "rpc-return-unknown-question"
      testsuite(63)%test => w_return_unknown_question
      testsuite(64)%name = "rpc-return-after-finish"
      testsuite(64)%test => w_return_after_finish
      testsuite(65)%name = "rpc-return-answer-id-64"
      testsuite(65)%test => w_return_answer_id_64
      testsuite(66)%name = "rpc-return-high-bit-ignored"
      testsuite(66)%test => w_return_high_bit_ignored
      testsuite(67)%name = "rpc-return-falsely-canceled"
      testsuite(67)%test => w_return_falsely_canceled
      testsuite(68)%name = "rpc-return-canceled-after-cancel"
      testsuite(68)%test => w_return_canceled_after_cancel
      testsuite(69)%name = "rpc-return-elsewhere-not-tail"
      testsuite(69)%test => w_return_elsewhere_not_tail
      testsuite(70)%name = "rpc-tail-return-with-results"
      testsuite(70)%test => w_tail_return_with_results
      testsuite(71)%name = "rpc-tail-return-with-exception"
      testsuite(71)%test => w_tail_return_with_exception
      testsuite(72)%name = "rpc-resolve-second-ignored"
      testsuite(72)%test => w_resolve_second_ignored
      testsuite(73)%name = "rpc-unimplemented-bootstrap"
      testsuite(73)%test => w_unimplemented_bootstrap
      testsuite(74)%name = "rpc-embargo-queues-locally"
      testsuite(74)%test => w_embargo_queues_locally
      testsuite(75)%name = "rpc-no-finish-needed"
      testsuite(75)%test => w_no_finish_needed
      testsuite(76)%name = "rpc-resolve-accepted"
      testsuite(76)%test => w_resolve_accepted
      testsuite(77)%name = "rpc-unknown-tags"
      testsuite(77)%test => w_unknown_tags
      testsuite(78)%name = "rpc-resolve-chained-local"
      testsuite(78)%test => w_resolve_chained_local
      testsuite(79)%name = "rpc-resolve-reused-id-target"
      testsuite(79)%test => w_resolve_reused_id_target
      testsuite(80)%name = "rpc-resolve-released-no-embargo"
      testsuite(80)%test => w_resolve_released_no_embargo
      testsuite(81)%name = "rpc-reused-handle-provide-join"
      testsuite(81)%test => w_reused_handle_provide_join
      testsuite(82)%name = "rpc-resolve-remote-reused-id"
      testsuite(82)%test => w_resolve_remote_reused_id
      testsuite(83)%name = "rpc-sender-loopback-echo"
      testsuite(83)%test => w_sender_loopback_echo
      testsuite(84)%name = "rpc-result-third-party-vine"
      testsuite(84)%test => w_result_third_party_vine
      testsuite(85)%name = "rpc-result-third-party-vine-finish"
      testsuite(85)%test => w_result_third_party_vine_finish
      testsuite(86)%name = "rpc-result-third-party-vine-reused-id"
      testsuite(86)%test => w_result_third_party_vine_reused_id
      testsuite(87)%name = "rpc-delayed-result-cap-reused-id"
      testsuite(87)%test => w_delayed_result_cap_reused_id
      testsuite(88)%name = "rpc-resolve-multiref-release"
      testsuite(88)%test => w_resolve_multiref_release
      testsuite(89)%name = "rpc-disembargo-promised-bad-walk"
      testsuite(89)%test => w_disembargo_promised_bad_walk
      testsuite(90)%name = "rpc-vine-done-after-finish-reused-id"
      testsuite(90)%test => w_vine_done_after_finish_reused_id
      testsuite(91)%name = "rpc-shutdown"
      testsuite(91)%test => w_shutdown
      testsuite(92)%name = "rpc-finish-after-resolve"
      testsuite(92)%test => w_finish_after_resolve
      testsuite(93)%name = "rpc-vine-finish-shared-id"
      testsuite(93)%test => w_vine_finish_shared_id
      testsuite(94)%name = "rpc-vine-done-shared-id-finish"
      testsuite(94)%test => w_vine_done_shared_id_finish
      testsuite(95)%name = "rpc-vine-past-sixteen-caps"
      testsuite(95)%test => w_vine_past_sixteen_caps
      testsuite(96)%name = "rpc-resolve-invalid-receiver-hosted"
      testsuite(96)%test => w_resolve_invalid_receiver_hosted
      testsuite(97)%name = "rpc-resolve-receiver-answer-walk"
      testsuite(97)%test => w_resolve_receiver_answer_walk
      testsuite(98)%name = "posix-recv-eintr-retried"
      testsuite(98)%test => t_recv_all_retries_eintr
      testsuite(99)%name = "rpc-framing-511-accepted"
      testsuite(99)%test => t_framing_511_accepted
      testsuite(100)%name = "rpc-framing-512-rejected"
      testsuite(100)%test => t_framing_512_rejected
      testsuite(101)%name = "rpc-framing-oversized-rejected"
      testsuite(101)%test => t_framing_oversized_rejected
      testsuite(102)%name = "rpc-framing-negative-traversal-rejected"
      testsuite(102)%test => t_framing_negative_traversal_rejected
      testsuite(103)%name = "rpc-framing-short-and-padded"
      testsuite(103)%test => t_framing_short_and_padded
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

   subroutine stage_srv_dispatch(self, ctx, err)
      class(stage_srv_t), intent(inout) :: self
      type(rpc_call_ctx_t), intent(inout) :: ctx
      integer, intent(out) :: err
      err = CAPNP_OK
      if (.not. associated(self%staged)) then
         err = CAPNP_ERR_ARG
         return
      end if
      ctx%staged(0)%s => self%staged
      ctx%nexp = 1
   end subroutine stage_srv_dispatch

   subroutine order_dispatch(self, ctx, err)
      class(order_srv_t), intent(inout) :: self
      type(rpc_call_ctx_t), intent(inout) :: ctx
      integer, intent(out) :: err
      type(capnp_ptr_t) :: s
      err = CAPNP_OK
      self%n = self%n + 1
      if (self%n <= size(self%seen)) self%seen(self%n) = ctx%method_id
      s = capnp_new_struct(ctx%rmsg, 0, 0, err)
      if (err == CAPNP_OK) call payload_content_set(ctx%results, s, err)
   end subroutine order_dispatch

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

   subroutine ensure_rpc(error)
      type(error_type), allocatable, intent(out) :: error
      if (rpc_up) return
      call px_socketpair(fda, fdb, err)
      if (err /= CAPNP_OK) then
         call test_failed(error, 'rpc: socketpair')
         return
      end if
      boot => echo
      call rpc_conn_init(srv, fdb, boot)
      boot => null()
      call rpc_conn_init(cli, fda, boot)
      rpc_up = .true.
   end subroutine ensure_rpc

   subroutine w_no_finish_needed(error)
      type(error_type), allocatable, intent(out) :: error
      call t_no_finish_needed(error)
   end subroutine w_no_finish_needed

   subroutine w_resolve_accepted(error)
      type(error_type), allocatable, intent(out) :: error
      call t_resolve_accepted(error)
   end subroutine w_resolve_accepted

   subroutine w_unknown_tags(error)
      type(error_type), allocatable, intent(out) :: error
      call t_unknown_tags(error)
   end subroutine w_unknown_tags

   subroutine w_resolve_chained_local(error)
      type(error_type), allocatable, intent(out) :: error
      call t_resolve_chained_local(error)
   end subroutine w_resolve_chained_local

   subroutine w_resolve_reused_id_target(error)
      type(error_type), allocatable, intent(out) :: error
      call t_resolve_reused_id_target(error)
   end subroutine w_resolve_reused_id_target

   subroutine w_resolve_released_no_embargo(error)
      type(error_type), allocatable, intent(out) :: error
      call t_resolve_released_no_embargo(error)
   end subroutine w_resolve_released_no_embargo

   subroutine w_reused_handle_provide_join(error)
      type(error_type), allocatable, intent(out) :: error
      call t_reused_handle_provide_join(error)
   end subroutine w_reused_handle_provide_join

   subroutine w_resolve_remote_reused_id(error)
      type(error_type), allocatable, intent(out) :: error
      call t_resolve_remote_reused_id(error)
   end subroutine w_resolve_remote_reused_id

   subroutine reflect_dispatch(self, ctx, err)
      class(reflect_srv_t), intent(inout) :: self
      type(rpc_call_ctx_t), intent(inout) :: ctx
      integer, intent(out) :: err
      type(capnp_ptr_t) :: ctab
      type(cap_descriptor_t) :: cd
      err = CAPNP_OK
      if (ctx%method_id < 0) then
         err = CAPNP_ERR_ARG
         return
      end if
      ctab = payload_cap_table_init(ctx%results, 1_int64, err)
      if (err /= CAPNP_OK) return
      cd%p = capnp_list_get_struct(ctab, 0, err)
      if (err /= CAPNP_OK) return
      call cap_descriptor_receiver_hosted_set(cd, 4_int64, err)
      if (err /= CAPNP_OK) return
      call payload_content_set(ctx%results, rpc_make_cap_ptr(ctx%rmsg, 0), err)
   end subroutine reflect_dispatch

   subroutine w_sender_loopback_echo(error)
      type(error_type), allocatable, intent(out) :: error
      call t_sender_loopback_echo(error)
      if (.not. allocated(error)) call t_sender_loopback_echo_yourself(error)
   end subroutine w_sender_loopback_echo

   subroutine w_result_third_party_vine(error)
      type(error_type), allocatable, intent(out) :: error
      call t_result_third_party_vine(error)
   end subroutine w_result_third_party_vine

   subroutine w_result_third_party_vine_finish(error)
      type(error_type), allocatable, intent(out) :: error
      call t_result_third_party_vine_finish(error)
   end subroutine w_result_third_party_vine_finish

   subroutine w_result_third_party_vine_reused_id(error)
      type(error_type), allocatable, intent(out) :: error
      call t_result_third_party_vine_reused_id(error)
   end subroutine w_result_third_party_vine_reused_id

   subroutine w_delayed_result_cap_reused_id(error)
      type(error_type), allocatable, intent(out) :: error
      call t_delayed_result_cap_reused_id(error)
   end subroutine w_delayed_result_cap_reused_id

   subroutine w_resolve_multiref_release(error)
      type(error_type), allocatable, intent(out) :: error
      call t_resolve_multiref_release(error)
   end subroutine w_resolve_multiref_release

   subroutine w_vine_done_after_finish_reused_id(error)
      type(error_type), allocatable, intent(out) :: error
      call t_vine_done_after_finish_reused_id(error)
   end subroutine w_vine_done_after_finish_reused_id

   subroutine w_disembargo_promised_bad_walk(error)
      type(error_type), allocatable, intent(out) :: error
      call t_disembargo_promised_bad_walk(error)
   end subroutine w_disembargo_promised_bad_walk

   subroutine w_finish_after_resolve(error)
      type(error_type), allocatable, intent(out) :: error
      call t_finish_after_resolve(error)
   end subroutine w_finish_after_resolve

   subroutine w_vine_finish_shared_id(error)
      type(error_type), allocatable, intent(out) :: error
      call t_vine_finish_shared_id(error)
   end subroutine w_vine_finish_shared_id

   subroutine w_vine_done_shared_id_finish(error)
      type(error_type), allocatable, intent(out) :: error
      call t_vine_done_shared_id_finish(error)
   end subroutine w_vine_done_shared_id_finish

   subroutine w_vine_past_sixteen_caps(error)
      type(error_type), allocatable, intent(out) :: error
      call t_vine_past_sixteen_caps(error)
   end subroutine w_vine_past_sixteen_caps

   subroutine w_resolve_invalid_receiver_hosted(error)
      type(error_type), allocatable, intent(out) :: error
      call t_resolve_invalid_receiver_hosted(error)
   end subroutine w_resolve_invalid_receiver_hosted

   subroutine w_resolve_receiver_answer_walk(error)
      type(error_type), allocatable, intent(out) :: error
      call t_resolve_receiver_answer_walk(error)
   end subroutine w_resolve_receiver_answer_walk

   subroutine w_shutdown(error)
      type(error_type), allocatable, intent(out) :: error
      if (.not. rpc_up) return
      call rpc_conn_close(cli)
      call rpc_conn_close(srv)
      rpc_up = .false.
   end subroutine w_shutdown

   subroutine w_bootstrap_and_echo(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_bootstrap_and_echo(error)
   end subroutine w_bootstrap_and_echo

   subroutine w_cap_result_and_pipeline(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_cap_result_and_pipeline(error)
   end subroutine w_cap_result_and_pipeline

   subroutine w_exception(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_exception(error)
   end subroutine w_exception

   subroutine w_unimplemented(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_unimplemented(error)
   end subroutine w_unimplemented

   subroutine w_persistent_save(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_persistent_save(error)
   end subroutine w_persistent_save

   subroutine w_resolve_and_tail_calls(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_resolve_and_tail_calls(error)
   end subroutine w_resolve_and_tail_calls

   subroutine w_sender_promise_import(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_sender_promise_import(error)
   end subroutine w_sender_promise_import

   subroutine w_disembargo_echo(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_disembargo_echo(error)
   end subroutine w_disembargo_echo

   subroutine w_disembargo_promised_answer(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_disembargo_promised_answer(error)
   end subroutine w_disembargo_promised_answer

   subroutine w_disembargo_receiver_loopback_absorbed(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_disembargo_receiver_loopback_absorbed(error)
   end subroutine w_disembargo_receiver_loopback_absorbed

   subroutine w_pump_poll(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_pump_poll(error)
   end subroutine w_pump_poll

   subroutine w_provide_and_accept(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_provide_and_accept(error)
   end subroutine w_provide_and_accept

   subroutine w_accept_unknown_nonce(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_accept_unknown_nonce(error)
   end subroutine w_accept_unknown_nonce

   subroutine w_provide_unhosted(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_provide_unhosted(error)
   end subroutine w_provide_unhosted

   subroutine w_two_provisions_independent(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_two_provisions_independent(error)
   end subroutine w_two_provisions_independent

   subroutine w_accept_embargo(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_accept_embargo(error)
   end subroutine w_accept_embargo

   subroutine w_third_party_introduction(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_third_party_introduction(error)
   end subroutine w_third_party_introduction

   subroutine w_introduction_in_an_answer(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_introduction_in_an_answer(error)
   end subroutine w_introduction_in_an_answer

   subroutine w_overlong_host_refused(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_overlong_host_refused(error)
   end subroutine w_overlong_host_refused

   subroutine w_reference_introduction(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_reference_introduction(error)
   end subroutine w_reference_introduction

   subroutine w_reference_frames(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_reference_frames(error)
   end subroutine w_reference_frames

   subroutine w_join_same_capability(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_join_same_capability(error)
   end subroutine w_join_same_capability

   subroutine w_join_unresolvable_part(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_join_unresolvable_part(error)
   end subroutine w_join_unresolvable_part

   subroutine w_join_all_parts_unresolvable(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_join_all_parts_unresolvable(error)
   end subroutine w_join_all_parts_unresolvable

   subroutine w_join_client_api(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_join_client_api(error)
   end subroutine w_join_client_api

   subroutine w_join_incomplete_set_is_silent(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_join_incomplete_set_is_silent(error)
   end subroutine w_join_incomplete_set_is_silent

   subroutine w_join_finish_one_part(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_join_finish_one_part(error)
   end subroutine w_join_finish_one_part

   subroutine w_join_send_unwinds(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_join_send_unwinds(error)
   end subroutine w_join_send_unwinds

   subroutine w_join_finish_releases_cap(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_join_finish_releases_cap(error)
   end subroutine w_join_finish_releases_cap

   subroutine w_abandoned_join_cleared(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_abandoned_join_cleared(error)
   end subroutine w_abandoned_join_cleared

   subroutine w_finish_provide_qid(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_finish_provide_qid(error)
   end subroutine w_finish_provide_qid

   subroutine w_request_cancellation(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_request_cancellation(error)
   end subroutine w_request_cancellation

   subroutine w_cancel_reserves_qid(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_cancel_reserves_qid(error)
   end subroutine w_cancel_reserves_qid

   subroutine w_send_failure_releases_qid(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_send_failure_releases_qid(error)
   end subroutine w_send_failure_releases_qid

   subroutine w_call_send_failure_releases(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_call_send_failure_releases(error)
   end subroutine w_call_send_failure_releases

   subroutine w_malformed_root_is_freed(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_malformed_root_is_freed(error)
   end subroutine w_malformed_root_is_freed

   subroutine w_membrane_unwrap_frees(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_membrane_unwrap_frees(error)
   end subroutine w_membrane_unwrap_frees

   subroutine w_provide_overrelease_keeps_server(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_provide_overrelease_keeps_server(error)
   end subroutine w_provide_overrelease_keeps_server

   subroutine w_embargo_finish_answers_recipient(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_embargo_finish_answers_recipient(error)
   end subroutine w_embargo_finish_answers_recipient

   subroutine w_accept_finish_releases_cap(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_accept_finish_releases_cap(error)
   end subroutine w_accept_finish_releases_cap

   subroutine w_accept_finish_after_embargo(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_accept_finish_after_embargo(error)
   end subroutine w_accept_finish_after_embargo

   subroutine w_embargo_recipient_close(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_embargo_recipient_close(error)
   end subroutine w_embargo_recipient_close

   subroutine w_membrane(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_membrane(error)
   end subroutine w_membrane

   subroutine w_export_table_full(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_export_table_full(error)
   end subroutine w_export_table_full

   subroutine w_pluggable_transport(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_pluggable_transport(error)
   end subroutine w_pluggable_transport

   subroutine w_dial_guard(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_dial_guard(error)
   end subroutine w_dial_guard

   subroutine w_question_id_duplicate(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_question_id_duplicate(error)
   end subroutine w_question_id_duplicate

   subroutine w_question_id_out_of_range(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_question_id_out_of_range(error)
   end subroutine w_question_id_out_of_range

   subroutine w_duplicate_return(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_duplicate_return(error)
   end subroutine w_duplicate_return

   subroutine w_take_from_normal_answer(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_take_from_normal_answer(error)
   end subroutine w_take_from_normal_answer

   subroutine w_resolve_to_hosted(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_resolve_to_hosted(error)
   end subroutine w_resolve_to_hosted

   subroutine w_resolve_none(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_resolve_none(error)
   end subroutine w_resolve_none

   subroutine w_resolve_non_promise(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_resolve_non_promise(error)
   end subroutine w_resolve_non_promise

   subroutine w_resolve_exception(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_resolve_exception(error)
   end subroutine w_resolve_exception

   subroutine w_resolve_embargo(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_resolve_embargo(error)
   end subroutine w_resolve_embargo

   subroutine w_join_duplicate_qid(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_join_duplicate_qid(error)
   end subroutine w_join_duplicate_qid

   subroutine w_disembargo_provide_wrong_conn(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_disembargo_provide_wrong_conn(error)
   end subroutine w_disembargo_provide_wrong_conn

   subroutine w_third_party_tail_aborts(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_third_party_tail_aborts(error)
   end subroutine w_third_party_tail_aborts

   subroutine w_yourself_splice(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_yourself_splice(error)
   end subroutine w_yourself_splice

   subroutine w_release_live_over(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_release_live_over(error)
   end subroutine w_release_live_over

   subroutine w_call_order(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_call_order(error)
   end subroutine w_call_order

   subroutine w_call_missing_export(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_call_missing_export(error)
   end subroutine w_call_missing_export

   subroutine w_return_unknown_question(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_return_unknown_question(error)
   end subroutine w_return_unknown_question

   subroutine w_return_after_finish(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_return_after_finish(error)
   end subroutine w_return_after_finish

   subroutine w_return_answer_id_64(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_return_answer_id_64(error)
   end subroutine w_return_answer_id_64

   subroutine w_return_high_bit_ignored(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_return_high_bit_ignored(error)
   end subroutine w_return_high_bit_ignored

   subroutine w_return_falsely_canceled(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_return_falsely_canceled(error)
   end subroutine w_return_falsely_canceled

   subroutine w_return_canceled_after_cancel(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_return_canceled_after_cancel(error)
   end subroutine w_return_canceled_after_cancel

   subroutine w_return_elsewhere_not_tail(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_return_elsewhere_not_tail(error)
   end subroutine w_return_elsewhere_not_tail

   subroutine w_tail_return_with_results(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_tail_return_with_results(error)
   end subroutine w_tail_return_with_results

   subroutine w_tail_return_with_exception(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_tail_return_with_exception(error)
   end subroutine w_tail_return_with_exception

   subroutine w_resolve_second_ignored(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_resolve_second_ignored(error)
   end subroutine w_resolve_second_ignored

   subroutine w_unimplemented_bootstrap(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_unimplemented_bootstrap(error)
   end subroutine w_unimplemented_bootstrap

   subroutine w_embargo_queues_locally(error)
      type(error_type), allocatable, intent(out) :: error
      call ensure_rpc(error)
      if (allocated(error)) return
      call t_embargo_queues_locally(error)
   end subroutine w_embargo_queues_locally

   !> Send one Join part: question `qid`, targeting export `eid`, carrying
   !> a two-party JoinKeyPart naming the set.
   subroutine send_join_part(fd, qid, eid, jid, pcount, pnum, e)
      integer, intent(in) :: fd
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
      call rpc_send_message(fd, m, e)
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
      type(capnp_message_t), target :: unused_call
      type(payload_t) :: unused_params
      integer :: n, before
      integer(int64) :: qinj
      before = rpc_pending_introductions(cli)
      call rpc_bootstrap_send(cli, bootcap, err)
      ! Let the server answer, and take that answer off the wire, so the
      ! hand-built Return below is what the client reads next and no
      ! later case inherits a half-finished exchange. The injected
      ! Return uses a fresh question id: a second Return for the
      ! bootstrap would abort the connection.
      call rpc_pump_once(srv, err)
      call rpc_pump_once(cli, err)
      call rpc_call_begin(cli, bootcap, ECHO_IFACE, 0, unused_call, &
                          unused_params, qinj, err)
      call capnp_message_free(unused_call)
      call send_return_with_third_party_cap(qinj, '10.0.0.9', 5002, &
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
      call rpc_finish_send(cli, qinj, .false., err)
      call rpc_pump_once(srv, err)
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

      call send_join_part(cli%fd, 700_int64, a, 9_int64, 2, 0, err)
      call check_(error, err == CAPNP_OK, 'join: part 0 sent')
      call rpc_pump_once(srv, err)
      call check_(error, err == CAPNP_OK, 'join: server took part 0')
      ! An incomplete set is not answerable yet, so nothing comes back.
      call send_join_part(cli%fd, 701_int64, a, 9_int64, 2, 1, err)
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

      call send_join_part(cli%fd, 710_int64, a, 11_int64, 2, 0, err)
      call check_(error, err == CAPNP_OK, 'join: resolvable part sent')
      call rpc_pump_once(srv, err)
      call send_join_part(cli%fd, 711_int64, dead, 11_int64, 2, 1, err)
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

      call send_join_part(cli%fd, 740_int64, dead, 17_int64, 2, 0, err)
      call check_(error, err == CAPNP_OK, 'join: dead part 0 sent')
      call rpc_pump_once(srv, err)
      call send_join_part(cli%fd, 741_int64, dead, 17_int64, 2, 1, err)
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

      call send_join_part(cli%fd, 720_int64, a, 13_int64, 3, 0, err)
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
      call send_join_part(cli%fd, 721_int64, a, 13_int64, 1, 0, err)
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

      ! Release the adder. A call to that export id is no longer a current
      ! export, so the vat aborts, as C++ 1.4.0 does.
      call rpc_release_send(cli, adder, err)
      call rpc_pump_once(srv, err)
      call check_(error, err == CAPNP_OK .and. .not. srv%exports(int(adder%id))%used, &
                  'rpc: release drops the export')
      call rpc_call_begin(cli, adder, ECHO_IFACE, 0, m, params, qs, err)
      s = capnp_new_struct(m, 1, 0, err)
      call capnp_set_i64(s, 0_int64, 1_int64, err)
      call payload_content_set(params, s, err)
      call rpc_call_send(cli, m, err)
      call expect_abort(srv, cli%fd, 'not a current export ID', error, &
                        'rpc: call to a released export aborts')
      call rpc_conn_close(cli)
      call rpc_conn_close(srv)
      rpc_up = .false.
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
   !> they must come back as Message.unimplemented echoing the original.
   !> An unimplemented reply for any message other than Resolve aborts,
   !> which is what C++ 1.4.0 does.
   subroutine t_unimplemented(error)
      type(error_type), allocatable, intent(inout) :: error
      type(capnp_message_t), target :: m, rm
      type(message_t) :: msg, rmsg
      type(exception_t) :: ex
      character(len=:), allocatable :: reason
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

      ! An unimplemented reply for a message this vat sends aborts.
      call capnp_message_init_builder(m, err)
      msg = message_new_root(m, err)
      rmsg = message_unimplemented_init(msg, err)
      call message_obsolete_save_set(rmsg, capnp_new_struct(m, 1, 0, err), err)
      call rpc_send_message(srv%fd, m, err)
      call capnp_message_free(m)
      call rpc_pump_once(cli, err)
      call check_(error, .not. rpc_conn_alive(cli), &
                  'rpc: unimplemented of a required message aborts')
      call recv_within(srv%fd, rm, error, 'rpc: unimplemented abort arrives')
      if (allocated(error)) return
      rmsg = message_read_root(rm, err)
      ex = message_abort_get(rmsg, err)
      call exception_reason_get(ex, reason, err)
      call check_(error, index(reason, 'required RPC message type') > 0, &
                  'rpc: unimplemented abort reason')
      call capnp_message_free(rm)
      call rpc_conn_close(cli)
      call rpc_conn_close(srv)
      rpc_up = .false.
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

   !> An unknown Resolve releases its incoming capability. sendResultsTo.yourself holds the
   !> results and returns resultsSentElsewhere.
   subroutine t_resolve_and_tail_calls(error)
      type(error_type), allocatable, intent(inout) :: error
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(resolve_t) :: rv
      type(cap_descriptor_t) :: cd
      type(call_t) :: c
      type(message_target_t) :: tgt
      type(rpc_cap_t) :: bootcap
      type(payload_t) :: params
      type(capnp_ptr_t) :: s
      integer(int64) :: qb, qy
      logical :: readable

      ! A Resolve for an import this vat has never seen drops the incoming
      ! senderHosted capability, so its sender receives Release(id, 1).
      call capnp_message_init_builder(m, err)
      msg = message_new_root(m, err)
      rv = message_resolve_init(msg, err)
      call resolve_promise_id_set(rv, 7_int64, err)
      cd = resolve_cap_init(rv, err)
      call cap_descriptor_sender_hosted_set(cd, 0_int64, err)
      call rpc_send_message(cli%fd, m, err)
      call capnp_message_free(m)
      call rpc_pump_once(srv, err)
      call check_(error, err == CAPNP_OK .and. rpc_conn_alive(srv), &
                  'rpc: unknown resolve is ignored')
      call check_(error, next_is_release_of(0_int64, err), &
                  'rpc: unknown resolve releases incoming capability')

      ! sendResultsTo.yourself runs the call and holds the results.
      ! Nothing is written until a takeFrom names that answer.
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
      ! resultsSentElsewhere is sent when a takeFrom arrives, not when
      ! the yourself call is delivered.
      call px_poll_in(cli%fd, 50, readable, err)
      call check_(error, err == CAPNP_OK .and. .not. readable, &
                  'rpc: yourself does not return before takeFrom')
      call check_(error, rpc_conn_alive(srv), 'rpc: yourself leaves the connection up')
      call rpc_finish_send(cli, qb, .true., err)
      call rpc_pump_once(srv, err)
      call rpc_finish_send(cli, qy, .false., err)
      call rpc_pump_once(srv, err)
      ! A takeFromOtherQuestion that does not name a yourself redirect
      ! aborts the connection. That case has its own pair
      ! (rpc-take-from-normal-answer) so this shared pair stays up.
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

   !> senderLoopback aimed at a capability this vat hosts aborts: that
   !> object does not point back at the sender. An unknown export id
   !> aborts with the export-id reason.
   subroutine t_disembargo_echo(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: cap
      type(capnp_message_t), target :: reply
      type(message_t) :: rmsg
      type(disembargo_t) :: rd
      type(message_target_t) :: rtgt
      type(exception_t) :: ex
      character(len=:), allocatable :: reason
      integer :: fa, fb, e
      integer(int64) :: qboot
      call px_socketpair(fa, fb, e)
      boot => local_echo
      call rpc_conn_init(sconn, fb, boot)
      boot => null()
      call rpc_conn_init(cconn, fa, boot)
      call rpc_bootstrap_send(cconn, cap, e)
      qboot = cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call check_(error, e == CAPNP_OK .and. sconn%exports(0)%used, &
                  'rpc: disembargo export is live')
      if (allocated(error)) return
      call send_loopback(cconn%fd, 99_int64, 0_int64, e)
      call rpc_pump_once(sconn, e)
      call check_(error, .not. rpc_conn_alive(sconn), &
                  'rpc: hosted senderLoopback aborts')
      call recv_within(cconn%fd, reply, error, 'rpc: hosted senderLoopback abort arrives')
      if (allocated(error)) return
      rmsg = message_read_root(reply, e)
      call check_(error, message_which(rmsg) == MESSAGE_ABORT_TAG, &
                  'rpc: hosted senderLoopback is Abort')
      ex = message_abort_get(rmsg, e)
      call exception_reason_get(ex, reason, e)
      call check_(error, index(reason, 'does not point back') > 0, &
                  'rpc: hosted senderLoopback reason')
      call capnp_message_free(reply)
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)

      call px_socketpair(fa, fb, e)
      boot => local_echo
      call rpc_conn_init(sconn, fb, boot)
      boot => null()
      call rpc_conn_init(cconn, fa, boot)
      call send_loopback(cconn%fd, 100_int64, 3_int64, e)
      call rpc_pump_once(sconn, e)
      call check_(error, .not. rpc_conn_alive(sconn), &
                  'rpc: unknown senderLoopback aborts')
      call recv_within(cconn%fd, reply, error, 'rpc: unknown senderLoopback abort arrives')
      if (allocated(error)) return
      rmsg = message_read_root(reply, e)
      ex = message_abort_get(rmsg, e)
      call exception_reason_get(ex, reason, e)
      call check_(error, index(reason, 'not a current export ID') > 0, &
                  'rpc: unknown senderLoopback reason')
      call capnp_message_free(reply)
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_disembargo_echo

   !> A promisedAnswer that resolves to a local export does not point
   !> back at the sender, so senderLoopback aborts. An unknown question
   !> id aborts the same way.
   subroutine t_disembargo_promised_answer(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: cap
      type(capnp_message_t), target :: m, reply
      type(message_t) :: msg, rmsg
      type(disembargo_t) :: d, rd
      type(message_target_t) :: tgt, rtgt
      type(promised_answer_t) :: pa
      type(exception_t) :: ex
      character(len=:), allocatable :: reason
      integer :: fa, fb, e
      integer(int64) :: qboot
      call px_socketpair(fa, fb, e)
      boot => local_echo
      call rpc_conn_init(sconn, fb, boot)
      boot => null()
      call rpc_conn_init(cconn, fa, boot)
      call rpc_bootstrap_send(cconn, cap, e)
      qboot = cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      d = message_disembargo_init(msg, e)
      call disembargo_context_sender_loopback_set(d, 77_int64, e)
      tgt = disembargo_target_init(d, e)
      pa = message_target_promised_answer_init(tgt, e)
      call promised_answer_question_id_set(pa, qboot, e)
      call rpc_send_message(cconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(sconn, e)
      call check_(error, .not. rpc_conn_alive(sconn), &
                  'rpc: resolved promisedAnswer aborts')
      call recv_within(cconn%fd, reply, error, 'rpc: resolved promisedAnswer abort arrives')
      if (allocated(error)) return
      rmsg = message_read_root(reply, e)
      call check_(error, message_which(rmsg) == MESSAGE_ABORT_TAG, &
                  'rpc: resolved promisedAnswer is Abort')
      ex = message_abort_get(rmsg, e)
      call exception_reason_get(ex, reason, e)
      call check_(error, index(reason, 'does not point back') > 0, &
                  'rpc: resolved promisedAnswer reason')
      call capnp_message_free(reply)
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
      call px_socketpair(fa, fb, e)
      boot => local_echo
      call rpc_conn_init(sconn, fb, boot)
      boot => null()
      call rpc_conn_init(cconn, fa, boot)
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      d = message_disembargo_init(msg, e)
      call disembargo_context_sender_loopback_set(d, 78_int64, e)
      tgt = disembargo_target_init(d, e)
      pa = message_target_promised_answer_init(tgt, e)
      call promised_answer_question_id_set(pa, 42_int64, e)
      call rpc_send_message(cconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(sconn, e)
      call recv_within(cconn%fd, reply, error, 'rpc: unknown promisedAnswer abort arrives')
      if (allocated(error)) return
      rmsg = message_read_root(reply, e)
      call check_(error, message_which(rmsg) == MESSAGE_ABORT_TAG, &
                  'rpc: unknown promisedAnswer aborts')
      ex = message_abort_get(rmsg, e)
      call exception_reason_get(ex, reason, e)
      call check_(error, index(reason, 'senderLoopback') > 0, &
                  'rpc: unknown promisedAnswer reason')
      call capnp_message_free(reply)
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_disembargo_promised_answer

   !> A receiverLoopback whose embargo id this connection did not issue
   !> aborts the connection.
   subroutine t_disembargo_receiver_loopback_absorbed(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: boot
      type(capnp_message_t), target :: m, reply
      type(message_t) :: msg, rmsg
      type(disembargo_t) :: d
      type(message_target_t) :: tgt
      type(exception_t) :: ex
      character(len=:), allocatable :: reason
      integer :: fa, fb, e
      call px_socketpair(fa, fb, e)
      boot => local_echo
      call rpc_conn_init(sconn, fb, boot)
      boot => null()
      call rpc_conn_init(cconn, fa, boot)
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      d = message_disembargo_init(msg, e)
      call disembargo_context_receiver_loopback_set(d, 1_int64, e)
      tgt = disembargo_target_init(d, e)
      call message_target_imported_cap_set(tgt, 0_int64, e)
      call rpc_send_message(cconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(sconn, e)
      call rpc_recv_message(cconn%fd, reply, e)
      rmsg = message_read_root(reply, e)
      call check_(error, message_which(rmsg) == MESSAGE_ABORT_TAG, &
                  'rpc: receiverLoopback aborts')
      ex = message_abort_get(rmsg, e)
      call exception_reason_get(ex, reason, e)
      call check_(error, index(reason, 'receiverLoopback') > 0, &
                  'rpc: receiverLoopback reason')
      call check_(error, .not. rpc_conn_alive(sconn), &
                  'rpc: receiverLoopback kills the connection')
      call capnp_message_free(reply)
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_disembargo_receiver_loopback_absorbed

   !> senderLoopback aimed at one importedCap.
   subroutine send_loopback(fd, emb_id, import_id, e)
      integer, intent(in) :: fd
      integer(int64), intent(in) :: emb_id, import_id
      integer, intent(out) :: e
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(disembargo_t) :: d
      type(message_target_t) :: tgt
      call capnp_message_init_builder(m, e)
      if (e /= CAPNP_OK) return
      msg = message_new_root(m, e)
      if (e /= CAPNP_OK) return
      d = message_disembargo_init(msg, e)
      if (e /= CAPNP_OK) return
      call disembargo_context_sender_loopback_set(d, emb_id, e)
      if (e /= CAPNP_OK) return
      tgt = disembargo_target_init(d, e)
      if (e /= CAPNP_OK) return
      call message_target_imported_cap_set(tgt, import_id, e)
      if (e /= CAPNP_OK) return
      call rpc_send_message(fd, m, e)
      call capnp_message_free(m)
   end subroutine send_loopback

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

   subroutine allow_any_dial(host, port, allow)
      character(len=*), intent(in) :: host
      integer, intent(in) :: port
      logical, intent(out) :: allow
      allow = .true.
      if (port < 0 .and. len(host) < 0) allow = .false.
   end subroutine allow_any_dial

   !> A dial hook can refuse an introduction, and a timeout returns
   !> instead of blocking. With no hook, a dial is refused.
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

      holder%introductions(0)%host = '127.0.0.1'
      holder%introductions(0)%port = port
      holder%dial_allow => null()
      call rpc_introduction_dial(holder, 55_int64, introduced, boot, e)
      call check_(error, e == CAPNP_ERR_ARG, 'dial: no hook is a refusal')
      call px_poll_in(lfd, 100, readable, e)
      call check_(error, e == CAPNP_OK .and. .not. readable, &
                  'dial: no hook does not connect')

      holder%introductions(0)%host = '192.0.2.1'
      holder%introductions(0)%port = 81
      holder%dial_allow => allow_any_dial
      holder%dial_timeout_ms = 300
      call rpc_introduction_dial(holder, 55_int64, introduced, boot, e)
      call check_(error, e /= CAPNP_OK, 'dial: timeout or refusal returns')
      call rpc_conn_close(holder)
      call px_close(fb)
      call px_close(lfd)
   end subroutine t_dial_guard

   !> Finish of one Join part exceptions every other part already in the
   !> set, including one that arrived after the finished part.
   subroutine t_join_finish_one_part(error)
      type(error_type), allocatable, intent(inout) :: error
      integer :: a, b, n, i, kind
      integer(int64) :: ansid
      logical :: is_exc, held
      call two_live_exports(a, b, n)
      call check_(error, n >= 1, 'join finish: server has a live export')
      if (allocated(error)) return
      call send_join_part(cli%fd, 910_int64, a, 44_int64, 3, 0, err)
      call rpc_pump_once(srv, err)
      call send_join_part(cli%fd, 911_int64, a, 44_int64, 3, 1, err)
      call rpc_pump_once(srv, err)
      call send_finish(910_int64, err)
      call rpc_pump_once(srv, err)
      call recv_answer(ansid, is_exc, kind, err)
      call check_(error, err == CAPNP_OK .and. ansid == 911_int64 .and. is_exc, &
                  'join finish: later part is exceptioned')
      held = .false.
      do i = 0, 7
         if (srv%joins(i)%used .and. srv%joins(i)%join_id == 44_int64) held = .true.
      end do
      call check_(error, .not. held, 'join finish: the set is torn down')
   end subroutine t_join_finish_one_part

   !> A failed rpc_call_send returns the question and every export ref
   !> the unsent cap table was holding.
   subroutine t_call_send_failure_releases(error)
      type(error_type), allocatable, intent(inout) :: error
      type(fail_stream_t), target :: broken
      type(rpc_conn_t), target :: cconn
      class(rpc_server_t), pointer :: noboot
      type(rpc_cap_t) :: cap
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(capnp_ptr_t) :: ctab
      type(cap_descriptor_t) :: cd
      integer(int64) :: qid
      integer :: e
      noboot => null()
      broken%accept_sends = 0
      call rpc_conn_init_stream(cconn, broken, noboot)
      cconn%exports(0)%used = .true.
      cconn%exports(0)%refcount = 1
      cconn%exports(1)%used = .true.
      cconn%exports(1)%refcount = 1
      cconn%exports(2)%used = .true.
      cconn%exports(2)%refcount = 1
      cap%kind = RPC_CAP_IMPORT
      cap%id = 0_int64
      call rpc_call_begin(cconn, cap, 1_int64, 0, m, params, qid, e)
      call check_(error, e == CAPNP_OK .and. qid >= 0_int64, &
                  'call send: question reserved')
      if (allocated(error)) return
      ctab = payload_cap_table_init(params, 3_int64, e)
      cd%p = capnp_list_get_struct(ctab, 0, e)
      call cap_descriptor_sender_hosted_set(cd, 0_int64, e)
      cd%p = capnp_list_get_struct(ctab, 1, e)
      call cap_descriptor_sender_promise_set(cd, 1_int64, e)
      cd%p = capnp_list_get_struct(ctab, 2, e)
      call rpc_write_third_party_cap(cd, '127.0.0.1', 9, 1_int64, 2_int64, e)
      call rpc_call_send(cconn, m, e)
      call check_(error, e /= CAPNP_OK, 'call send: transport failure is reported')
      call check_(error, .not. cconn%questions(int(qid))%used, &
                  'call send: question is released')
      call check_(error, .not. cconn%exports(0)%used .and. &
                  .not. cconn%exports(1)%used .and. .not. cconn%exports(2)%used, &
                  'call send: hosted, promise and vine refs are dropped')
      call rpc_conn_close(cconn)
   end subroutine t_call_send_failure_releases

   !> A frame whose root pointer does not resolve is freed and does not
   !> kill the connection.
   subroutine t_malformed_root_is_freed(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: cap
      integer(int8) :: frame(0:15)
      integer(int64) :: qboot
      integer :: fa, fb, e
      frame = 0_int8
      frame(4) = 1_int8
      frame(8) = int(z'90', int8)
      frame(9) = 1_int8
      frame(12) = 1_int8
      call px_socketpair(fa, fb, e)
      boot => local_echo
      call rpc_conn_init(sconn, fb, boot)
      boot => null()
      call rpc_conn_init(cconn, fa, boot)
      call px_send_all(fa, frame, e)
      call rpc_pump_once(sconn, e)
      call check_(error, e /= CAPNP_OK .and. .not. sconn%dead, &
                  'malformed: bad root is an error and the connection lives')
      if (allocated(error)) return
      call rpc_bootstrap_send(cconn, cap, e)
      qboot = cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call check_(error, e == CAPNP_OK, 'malformed: a later bootstrap is answered')
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_malformed_root_is_freed

   !> Unwrapping an owned membrane frees that wrapper.
   subroutine t_membrane_unwrap_frees(error)
      type(error_type), allocatable, intent(inout) :: error
      type(echo_srv_t), target :: plain
      type(stage_srv_t), target :: inner_fwd, inner_rev
      type(rpc_membrane_server_t) :: fwd, rev
      type(counting_membrane_policy_t), target :: policy
      class(rpc_membrane_policy_t), pointer :: pol
      class(rpc_server_t), pointer :: pplain, pfwd, prev
      type(rpc_call_ctx_t) :: ctx
      integer :: live, e
      logical :: inner
      pol => policy
      pplain => plain
      inner_fwd%staged => pplain
      pfwd => inner_fwd
      call rpc_membrane_server_init(fwd, pfwd, pol, reverse=.false.)
      live = rpc_membrane_owned_live()
      ctx%interface_id = 1_int64
      ctx%method_id = 0
      call fwd%dispatch(ctx, e)
      call check_(error, e == CAPNP_OK .and. rpc_membrane_owned_live() == live + 1, &
                  'membrane unwrap: crossing allocates one wrapper')
      if (allocated(error)) return
      inner_rev%staged => ctx%staged(0)%s
      prev => inner_rev
      call rpc_membrane_server_init(rev, prev, pol, reverse=.true.)
      call rev%dispatch(ctx, e)
      inner = .false.
      select type (s => ctx%staged(0)%s)
      class is (echo_srv_t)
         inner = associated(s, plain)
      end select
      call check_(error, e == CAPNP_OK .and. inner, &
                  'membrane unwrap: reverse crossing returns the inner server')
      call check_(error, rpc_membrane_owned_live() == live, &
                  'membrane unwrap: the owned wrapper is freed')
   end subroutine t_membrane_unwrap_frees

   !> Releasing more refs than an export holds is a protocol error. The
   !> provision's own reference keeps the server alive for Accept.
   subroutine t_provide_overrelease_keeps_server(error)
      type(error_type), allocatable, intent(inout) :: error
      type(echo_srv_t), target :: plain
      type(stage_srv_t), target :: inner_fwd
      type(rpc_membrane_server_t) :: fwd
      type(counting_membrane_policy_t), target :: policy
      class(rpc_membrane_policy_t), pointer :: pol
      class(rpc_server_t), pointer :: pfwd, owned_boot
      type(rpc_call_ctx_t) :: ctx
      type(rpc_conn_t), target :: cconn, sconn
      type(rpc_cap_t) :: cap
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(release_t) :: rel
      integer(int64) :: qboot, qacc
      integer :: fa, fb, e, live, refs
      pol => policy
      inner_fwd%staged => plain
      pfwd => inner_fwd
      call rpc_membrane_server_init(fwd, pfwd, pol, reverse=.false.)
      ctx%interface_id = 1_int64
      ctx%method_id = 0
      call fwd%dispatch(ctx, e)
      call check_(error, e == CAPNP_OK, 'overrelease: wrapper built')
      if (allocated(error)) return
      select type (s => ctx%staged(0)%s)
      class is (rpc_server_t)
         owned_boot => s
      class default
         call check_(error, .false., 'overrelease: wrapper is a server')
         return
      end select
      call px_socketpair(fa, fb, e)
      call rpc_conn_init(sconn, fb, owned_boot)
      owned_boot => null()
      call rpc_conn_init(cconn, fa, owned_boot)
      call rpc_bootstrap_send(cconn, cap, e)
      qboot = cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call rpc_result_cap(cconn, qboot, [integer ::], cap, e)
      call check_(error, e == CAPNP_OK .and. sconn%exports(0)%used, &
                  'overrelease: bootstrap exported')
      if (allocated(error)) return
      call rpc_provide_send(cconn, cap%id, '127.0.0.1', 9, 77_int64, qboot, e)
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      live = rpc_membrane_owned_live()
      refs = sconn%exports(0)%refcount
      ! A Release of every remaining ref, including the pin, must not
      ! free the wrapper the provision still holds. Do that before the
      ! protocol error: an over-release aborts the connection.
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      rel = message_release_init(msg, e)
      call release_id_set(rel, 0_int64, e)
      call release_reference_count_set(rel, int(refs, int64), e)
      call rpc_send_message(cconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(sconn, e)
      call check_(error, e == CAPNP_OK, 'overrelease: releasing the pin is legal')
      call check_(error, rpc_membrane_owned_live() == live, &
                  'overrelease: the server is not freed')
      call rpc_accept_send(cconn, 77_int64, .false., qacc, e)
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qacc, e)
      call check_(error, e == CAPNP_OK, 'overrelease: Accept still resolves')
      if (allocated(error)) return
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      rel = message_release_init(msg, e)
      call release_id_set(rel, 0_int64, e)
      call release_reference_count_set(rel, int(refs, int64) + 1_int64, e)
      call rpc_send_message(cconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(sconn, e)
      call check_(error, e == CAPNP_ERR_BOUNDS, 'overrelease: too many refs is an error')
      call check_(error, .not. rpc_conn_alive(sconn), 'overrelease: the connection aborts')
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_provide_overrelease_keeps_server

   !> Finish of an embargoed Provide answers the recipient connection and
   !> leaves the introducer's export alone. Closing the introducer's
   !> connection does the same.
   subroutine t_embargo_finish_answers_recipient(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_vat_t), target :: vat_store
      type(rpc_vat_t), pointer :: vat
      type(rpc_conn_t), target :: intro, prov_a, recip, prov_b
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: cap
      integer(int64) :: qboot, pq, aq
      integer :: ia, pa, ra, pb, e, refs
      integer(int64) :: ansid
      logical :: is_exc, readable
      integer :: kind
      vat => vat_store
      call px_socketpair(ia, pa, e)
      call px_socketpair(ra, pb, e)
      call check_(error, e == CAPNP_OK, 'embargo finish: socketpairs')
      if (allocated(error)) return
      boot => local_echo
      call rpc_conn_init(prov_a, pa, boot)
      call rpc_conn_init(prov_b, pb, boot)
      call rpc_conn_set_vat(prov_a, vat)
      call rpc_conn_set_vat(prov_b, vat)
      boot => null()
      call rpc_conn_init(intro, ia, boot)
      call rpc_conn_init(recip, ra, boot)
      call rpc_bootstrap_send(intro, cap, e)
      qboot = cap%id
      call rpc_pump_once(prov_a, e)
      call rpc_wait(intro, qboot, e)
      call rpc_result_cap(intro, qboot, [integer ::], cap, e)
      call check_(error, e == CAPNP_OK .and. prov_a%exports(0)%used, &
                  'embargo finish: introducer bootstrapped')
      if (allocated(error)) return
      refs = prov_a%exports(0)%refcount
      call rpc_provide_send(intro, cap%id, '127.0.0.1', 9, 88_int64, pq, e)
      call rpc_pump_once(prov_a, e)
      call rpc_wait(intro, pq, e)
      call rpc_accept_send(recip, 88_int64, .true., aq, e)
      call rpc_pump_once(prov_b, e)
      call px_poll_in(recip%fd, 50, readable, e)
      call check_(error, e == CAPNP_OK .and. .not. readable, &
                  'embargo finish: Accept waits')
      call rpc_finish_send(intro, pq, .false., e)
      call rpc_pump_once(prov_a, e)
      call px_poll_in(recip%fd, 200, readable, e)
      call check_(error, readable, 'embargo finish: recipient is answered')
      if (allocated(error)) return
      call recv_on(recip%fd, ansid, is_exc, kind, e)
      call check_(error, e == CAPNP_OK .and. ansid == aq .and. is_exc, &
                  'embargo finish: recipient gets the exception')
      call check_(error, prov_a%exports(0)%used .and. prov_a%exports(0)%refcount == refs, &
                  'embargo finish: introducer export is untouched')
      call rpc_conn_close(intro)
      call rpc_conn_close(recip)
      call rpc_conn_close(prov_a)
      call rpc_conn_close(prov_b)

      ! Closing the introducer's connection answers the recipient too.
      call px_socketpair(ia, pa, e)
      call px_socketpair(ra, pb, e)
      boot => local_echo
      call rpc_conn_init(prov_a, pa, boot)
      call rpc_conn_init(prov_b, pb, boot)
      call rpc_conn_set_vat(prov_a, vat)
      call rpc_conn_set_vat(prov_b, vat)
      boot => null()
      call rpc_conn_init(intro, ia, boot)
      call rpc_conn_init(recip, ra, boot)
      call rpc_bootstrap_send(intro, cap, e)
      qboot = cap%id
      call rpc_pump_once(prov_a, e)
      call rpc_wait(intro, qboot, e)
      call rpc_result_cap(intro, qboot, [integer ::], cap, e)
      refs = prov_a%exports(0)%refcount
      call rpc_provide_send(intro, cap%id, '127.0.0.1', 9, 89_int64, pq, e)
      call rpc_pump_once(prov_a, e)
      call rpc_wait(intro, pq, e)
      call rpc_accept_send(recip, 89_int64, .true., aq, e)
      call rpc_pump_once(prov_b, e)
      call rpc_conn_close(prov_a)
      call px_poll_in(recip%fd, 200, readable, e)
      call check_(error, readable, 'embargo close: recipient is answered')
      if (allocated(error)) return
      call recv_on(recip%fd, ansid, is_exc, kind, e)
      call check_(error, e == CAPNP_OK .and. ansid == aq .and. is_exc, &
                  'embargo close: recipient gets the exception')
      call rpc_conn_close(intro)
      call rpc_conn_close(recip)
      call rpc_conn_close(prov_b)
   end subroutine t_embargo_finish_answers_recipient

   !> Finish(releaseResultCaps) drops the export an Accept result published.
   subroutine t_accept_finish_releases_cap(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_vat_t), target :: vat_store
      type(rpc_vat_t), pointer :: vat
      type(rpc_conn_t), target :: intro, prov_a, recip, prov_b
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: cap
      integer(int64) :: qboot, pq, aq
      integer :: ia, pa, ra, pb, e
      vat => vat_store
      call px_socketpair(ia, pa, e)
      call px_socketpair(ra, pb, e)
      boot => local_echo
      call rpc_conn_init(prov_a, pa, boot)
      call rpc_conn_init(prov_b, pb, boot)
      call rpc_conn_set_vat(prov_a, vat)
      call rpc_conn_set_vat(prov_b, vat)
      boot => null()
      call rpc_conn_init(intro, ia, boot)
      call rpc_conn_init(recip, ra, boot)
      call rpc_bootstrap_send(intro, cap, e)
      qboot = cap%id
      call rpc_pump_once(prov_a, e)
      call rpc_wait(intro, qboot, e)
      call rpc_result_cap(intro, qboot, [integer ::], cap, e)
      call check_(error, e == CAPNP_OK, 'accept finish: bootstrap')
      if (allocated(error)) return
      call rpc_provide_send(intro, cap%id, '127.0.0.1', 9, 91_int64, pq, e)
      call rpc_pump_once(prov_a, e)
      call rpc_wait(intro, pq, e)
      call rpc_accept_send(recip, 91_int64, .false., aq, e)
      call rpc_pump_once(prov_b, e)
      call rpc_wait(recip, aq, e)
      call check_(error, e == CAPNP_OK .and. prov_b%exports(0)%used, &
                  'accept finish: the claimed cap is exported')
      if (allocated(error)) return
      call rpc_finish_send(recip, aq, .false., e)
      call rpc_pump_once(prov_b, e)
      call check_(error, e == CAPNP_OK .and. .not. prov_b%exports(0)%used, &
                  'accept finish: Finish releases the result export')
      call rpc_conn_close(intro)
      call rpc_conn_close(recip)
      call rpc_conn_close(prov_a)
      call rpc_conn_close(prov_b)
   end subroutine t_accept_finish_releases_cap

   !> The same release happens when the Accept was held for an embargo
   !> and the result is sent only after that embargo lifts.
   subroutine t_accept_finish_after_embargo(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_vat_t), target :: vat_store
      type(rpc_vat_t), pointer :: vat
      type(rpc_conn_t), target :: intro, prov_a, recip, prov_b
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: cap
      integer(int64) :: qboot, pq, aq
      integer :: ia, pa, ra, pb, e
      logical :: readable
      vat => vat_store
      call px_socketpair(ia, pa, e)
      call px_socketpair(ra, pb, e)
      boot => local_echo
      call rpc_conn_init(prov_a, pa, boot)
      call rpc_conn_init(prov_b, pb, boot)
      call rpc_conn_set_vat(prov_a, vat)
      call rpc_conn_set_vat(prov_b, vat)
      boot => null()
      call rpc_conn_init(intro, ia, boot)
      call rpc_conn_init(recip, ra, boot)
      call rpc_bootstrap_send(intro, cap, e)
      qboot = cap%id
      call rpc_pump_once(prov_a, e)
      call rpc_wait(intro, qboot, e)
      call rpc_result_cap(intro, qboot, [integer ::], cap, e)
      call check_(error, e == CAPNP_OK, 'embargo accept finish: bootstrap')
      if (allocated(error)) return
      call rpc_provide_send(intro, cap%id, '127.0.0.1', 9, 92_int64, pq, e)
      call rpc_pump_once(prov_a, e)
      call rpc_wait(intro, pq, e)
      call rpc_accept_send(recip, 92_int64, .true., aq, e)
      call rpc_pump_once(prov_b, e)
      call px_poll_in(recip%fd, 50, readable, e)
      call check_(error, e == CAPNP_OK .and. .not. readable, &
                  'embargo accept finish: Accept waits')
      call check_(error, prov_b%exports(0)%used, &
                  'embargo accept finish: export is held during the embargo')
      if (allocated(error)) return
      call rpc_disembargo_provide_send(intro, pq, e)
      call rpc_pump_once(prov_a, e)
      call rpc_wait(recip, aq, e)
      call check_(error, e == CAPNP_OK, 'embargo accept finish: result arrives')
      if (allocated(error)) return
      call rpc_finish_send(recip, aq, .false., e)
      call rpc_pump_once(prov_b, e)
      call check_(error, e == CAPNP_OK .and. .not. prov_b%exports(0)%used, &
                  'embargo accept finish: Finish releases the result export')
      call rpc_conn_close(intro)
      call rpc_conn_close(recip)
      call rpc_conn_close(prov_a)
      call rpc_conn_close(prov_b)
   end subroutine t_accept_finish_after_embargo

   !> Closing the recipient of an embargoed Accept drops the provision.
   !> A later Finish of the Provide must not send on that dead connection.
   subroutine t_embargo_recipient_close(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_vat_t), target :: vat_store
      type(rpc_vat_t), pointer :: vat
      type(rpc_conn_t), target :: intro, prov_a, recip, prov_b
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: cap
      integer(int64) :: qboot, pq, aq
      integer :: ia, pa, ra, pb, e, refs
      logical :: readable
      vat => vat_store
      call px_socketpair(ia, pa, e)
      call px_socketpair(ra, pb, e)
      boot => local_echo
      call rpc_conn_init(prov_a, pa, boot)
      call rpc_conn_init(prov_b, pb, boot)
      call rpc_conn_set_vat(prov_a, vat)
      call rpc_conn_set_vat(prov_b, vat)
      boot => null()
      call rpc_conn_init(intro, ia, boot)
      call rpc_conn_init(recip, ra, boot)
      call rpc_bootstrap_send(intro, cap, e)
      qboot = cap%id
      call rpc_pump_once(prov_a, e)
      call rpc_wait(intro, qboot, e)
      call rpc_result_cap(intro, qboot, [integer ::], cap, e)
      call check_(error, e == CAPNP_OK .and. prov_a%exports(0)%used, &
                  'recipient close: bootstrap')
      if (allocated(error)) return
      refs = prov_a%exports(0)%refcount
      call rpc_provide_send(intro, cap%id, '127.0.0.1', 9, 93_int64, pq, e)
      call rpc_pump_once(prov_a, e)
      call rpc_wait(intro, pq, e)
      call rpc_accept_send(recip, 93_int64, .true., aq, e)
      call rpc_pump_once(prov_b, e)
      call px_poll_in(recip%fd, 50, readable, e)
      call check_(error, e == CAPNP_OK .and. .not. readable, &
                  'recipient close: Accept waits')
      if (allocated(error)) return
      call rpc_conn_close(prov_b)
      call check_(error, rpc_pending_provisions(prov_a) == 0, &
                  'recipient close: the provision is dropped')
      call check_(error, prov_a%exports(0)%used .and. &
                  prov_a%exports(0)%refcount == refs, &
                  'recipient close: the pin is released')
      if (allocated(error)) return
      call rpc_finish_send(intro, pq, .false., e)
      call rpc_pump_once(prov_a, e)
      call check_(error, e == CAPNP_OK, &
                  'recipient close: Finish does not send to the dead peer')
      call check_(error, prov_a%exports(0)%refcount == refs, &
                  'recipient close: Finish leaves the introducer export')
      call rpc_conn_close(intro)
      call rpc_conn_close(recip)
      call rpc_conn_close(prov_a)
   end subroutine t_embargo_recipient_close

   !> Read one Return from an arbitrary connection.
   subroutine recv_on(fd, ansid, is_exc, kind, e)
      integer, intent(in) :: fd
      integer(int64), intent(out) :: ansid
      logical, intent(out) :: is_exc
      integer, intent(out) :: kind, e
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(return_t) :: r
      ansid = -1_int64
      is_exc = .false.
      kind = CAPNP_PK_NULL
      call rpc_recv_message(fd, m, e)
      if (e /= CAPNP_OK) return
      msg = message_read_root(m, e)
      if (e /= CAPNP_OK) then
         call capnp_message_free(m)
         return
      end if
      if (message_which(msg) /= MESSAGE_RETURN_TAG) then
         e = CAPNP_ERR_KIND
         call capnp_message_free(m)
         return
      end if
      r = message_return_get(msg, e)
      if (e /= CAPNP_OK) then
         call capnp_message_free(m)
         return
      end if
      ansid = return_answer_id_get(r)
      is_exc = return_which(r) /= RETURN_RESULTS_TAG
      call capnp_message_free(m)
   end subroutine recv_on

   subroutine send_finish_on(fd, qid, e)
      integer, intent(in) :: fd
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
      if (e == CAPNP_OK) call rpc_send_message(fd, m, e)
      call capnp_message_free(m)
   end subroutine send_finish_on

   subroutine send_raw_bootstrap(fd, qid, e)
      integer, intent(in) :: fd
      integer(int64), intent(in) :: qid
      integer, intent(out) :: e
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(bootstrap_t) :: b
      call capnp_message_init_builder(m, e)
      if (e /= CAPNP_OK) return
      msg = message_new_root(m, e)
      if (e /= CAPNP_OK) return
      b = message_bootstrap_init(msg, e)
      if (e /= CAPNP_OK) return
      call bootstrap_question_id_set(b, qid, e)
      if (e /= CAPNP_OK) return
      call rpc_send_message(fd, m, e)
      call capnp_message_free(m)
   end subroutine send_raw_bootstrap

   !> A second Bootstrap with a live question id aborts and does not take
   !> another export. Finish of an id we do not know leaves the connection up.
   subroutine t_question_id_duplicate(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: boot
      type(capnp_message_t), target :: reply
      type(message_t) :: rmsg
      type(exception_t) :: ex
      character(len=:), allocatable :: reason
      integer :: fa, fb, e, refs
      call px_socketpair(fa, fb, e)
      boot => local_echo
      call rpc_conn_init(sconn, fb, boot)
      boot => null()
      call rpc_conn_init(cconn, fa, boot)
      call send_finish_on(cconn%fd, 7_int64, e)
      call rpc_pump_once(sconn, e)
      call check_(error, rpc_conn_alive(sconn), 'qid: unknown Finish stays up')
      call send_raw_bootstrap(cconn%fd, 0_int64, e)
      call rpc_pump_once(sconn, e)
      call rpc_recv_message(cconn%fd, reply, e)
      call capnp_message_free(reply)
      call check_(error, sconn%exports(0)%used, 'qid: first bootstrap exported')
      if (allocated(error)) return
      refs = sconn%exports(0)%refcount
      call send_raw_bootstrap(cconn%fd, 0_int64, e)
      call rpc_pump_once(sconn, e)
      call check_(error, sconn%exports(0)%refcount == refs, &
                  'qid: duplicate does not take another export')
      call check_(error, .not. rpc_conn_alive(sconn), 'qid: duplicate aborts')
      call rpc_recv_message(cconn%fd, reply, e)
      rmsg = message_read_root(reply, e)
      call check_(error, message_which(rmsg) == MESSAGE_ABORT_TAG, &
                  'qid: duplicate is Abort')
      ex = message_abort_get(rmsg, e)
      call exception_reason_get(ex, reason, e)
      call check_(error, index(reason, 'already in use') > 0, 'qid: duplicate reason')
      call capnp_message_free(reply)
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_question_id_duplicate

   !> A question id past the fixed table stays until Finish, and the
   !> capability in its Return can be called.
   subroutine t_question_id_out_of_range(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: cap
      type(capnp_message_t), target :: reply, m
      type(message_t) :: msg
      type(return_t) :: r
      type(payload_t) :: pl, params
      type(capnp_ptr_t) :: content, ctab, s
      type(cap_descriptor_t) :: cd
      integer :: fa, fb, e
      integer(int64) :: qcall, hosted
      call px_socketpair(fa, fb, e)
      boot => local_echo
      call rpc_conn_init(sconn, fb, boot)
      boot => null()
      call rpc_conn_init(cconn, fa, boot)
      call send_raw_bootstrap(cconn%fd, 100_int64, e)
      call rpc_pump_once(sconn, e)
      call check_(error, e == CAPNP_OK, 'qid: id 100 is answered')
      call check_(error, sconn%exports(0)%used, 'qid: id 100 keeps the export')
      call check_(error, rpc_conn_alive(sconn), 'qid: id 100 leaves the connection up')
      if (allocated(error)) return
      call rpc_recv_message(cconn%fd, reply, e)
      msg = message_read_root(reply, e)
      r = message_return_get(msg, e)
      pl = return_results_get(r, e)
      content = payload_content_get(pl, e)
      ctab = payload_cap_table_get(pl, e)
      cd%p = capnp_list_get_struct(ctab, int(content%capidx), e)
      hosted = cap_descriptor_sender_hosted_get(cd)
      call capnp_message_free(reply)
      call check_(error, e == CAPNP_OK .and. hosted == 0_int64, &
                  'qid: id 100 returns export 0')
      if (allocated(error)) return
      cap%kind = RPC_CAP_IMPORT
      cap%id = hosted
      call rpc_call_begin(cconn, cap, ECHO_IFACE, 0, m, params, qcall, e)
      s = capnp_new_struct(m, 1, 1, e)
      call capnp_set_i64(s, 0_int64, 4_int64, e)
      call capnp_set_text(s, 0, 'x', e)
      call payload_content_set(params, s, e)
      call rpc_call_send(cconn, m, e)
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qcall, e)
      call rpc_result_content(cconn, qcall, content, e)
      call check_(error, e == CAPNP_OK .and. capnp_get_i64(content, 0_int64) == 8_int64, &
                  'qid: id 100 capability answers a call')
      call send_finish_on(cconn%fd, 100_int64, e)
      call rpc_pump_once(sconn, e)
      call check_(error, .not. sconn%exports(0)%used, &
                  'qid: Finish releases the high-id export')
      call check_(error, rpc_conn_alive(sconn), 'qid: Finish leaves the connection up')
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_question_id_out_of_range

   !> A second Return for a live question aborts and leaves the first
   !> result in place.
   subroutine t_duplicate_return(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: cap
      type(capnp_message_t), target :: m, reply
      type(message_t) :: msg, rmsg
      type(return_t) :: r
      type(exception_t) :: ex
      type(capnp_ptr_t) :: content
      character(len=:), allocatable :: reason
      integer :: fa, fb, e
      integer(int64) :: qboot
      call px_socketpair(fa, fb, e)
      boot => local_echo
      call rpc_conn_init(sconn, fb, boot)
      boot => null()
      call rpc_conn_init(cconn, fa, boot)
      call rpc_bootstrap_send(cconn, cap, e)
      qboot = cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call rpc_result_content(cconn, qboot, content, e)
      call check_(error, e == CAPNP_OK .and. content%kind == CAPNP_PK_CAP, &
                  'dup return: first result is the capability')
      if (allocated(error)) return
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      r = message_return_init(msg, e)
      call return_answer_id_set(r, qboot, e)
      call return_results_sent_elsewhere_set(r, e)
      call rpc_send_message(sconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(cconn, e)
      call check_(error, .not. rpc_conn_alive(cconn), 'dup return: aborts')
      call check_(error, cconn%questions(int(qboot))%returned, &
                  'dup return: the first result stays')
      call recv_within(sconn%fd, reply, error, 'dup return: abort arrives')
      if (allocated(error)) return
      rmsg = message_read_root(reply, e)
      ex = message_abort_get(rmsg, e)
      call exception_reason_get(ex, reason, e)
      call check_(error, index(reason, 'Duplicate Return.') > 0, 'dup return: reason')
      call capnp_message_free(reply)
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_duplicate_return

   !> takeFromOtherQuestion of a normal answer aborts. Only a
   !> sendResultsTo.yourself redirect may be taken.
   subroutine t_take_from_normal_answer(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: cap, unused
      type(capnp_message_t), target :: m, reply
      type(message_t) :: msg, rmsg
      type(payload_t) :: params
      type(return_t) :: r
      type(exception_t) :: ex
      type(capnp_ptr_t) :: s
      character(len=:), allocatable :: reason
      integer :: fa, fb, e
      integer(int64) :: qboot, qy, qsrv
      call px_socketpair(fa, fb, e)
      boot => local_echo
      call rpc_conn_init(sconn, fb, boot)
      boot => null()
      call rpc_conn_init(cconn, fa, boot)
      call rpc_bootstrap_send(cconn, cap, e)
      qboot = cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call rpc_result_cap(cconn, qboot, [integer ::], cap, e)
      call rpc_call_begin(cconn, cap, ECHO_IFACE, 0, m, params, qy, e)
      s = capnp_new_struct(m, 1, 1, e)
      call capnp_set_i64(s, 0_int64, 4_int64, e)
      call capnp_set_text(s, 0, 'x', e)
      call payload_content_set(params, s, e)
      call rpc_call_send(cconn, m, e)
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qy, e)
      call check_(error, e == CAPNP_OK, 'take normal: the call returned')
      if (allocated(error)) return
      call rpc_bootstrap_send(sconn, unused, e)
      qsrv = unused%id
      ! The server's bootstrap is sitting on the client socket. Drain it
      ! so the next read is the Abort this return must provoke.
      call rpc_recv_message(cconn%fd, reply, e)
      call capnp_message_free(reply)
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      r = message_return_init(msg, e)
      call return_answer_id_set(r, qsrv, e)
      ! The echo answer was cap-less, so it is already gone. The bootstrap
      ! answer is still stored and was not a yourself call.
      call return_take_from_other_question_set(r, qboot, e)
      call rpc_send_message(cconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(sconn, e)
      call check_(error, .not. rpc_conn_alive(sconn), 'take normal: aborts')
      call recv_within(cconn%fd, reply, error, 'take normal: abort arrives')
      if (allocated(error)) return
      rmsg = message_read_root(reply, e)
      ex = message_abort_get(rmsg, e)
      call exception_reason_get(ex, reason, e)
      call check_(error, index(reason, 'sendResultsTo.yourself') > 0, &
                  'take normal: reason names yourself')
      call capnp_message_free(reply)
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_take_from_normal_answer

   !> Answer `q` with a senderPromise so the client records a promise import.
   subroutine inject_promise(peer_fd, client, base, promise_id, q, e)
      integer, intent(in) :: peer_fd
      type(rpc_conn_t), intent(inout) :: client
      type(rpc_cap_t), intent(in) :: base
      integer(int64), intent(in) :: promise_id
      integer(int64), intent(out) :: q
      integer, intent(out) :: e
      type(capnp_message_t), target :: m
      type(payload_t) :: params, pl
      type(message_t) :: msg
      type(return_t) :: r
      type(cap_descriptor_t) :: cd
      type(capnp_ptr_t) :: ctab
      call rpc_call_begin(client, base, ECHO_IFACE, 0, m, params, q, e)
      if (e /= CAPNP_OK) return
      call capnp_message_free(m)
      call capnp_message_init_builder(m, e)
      if (e /= CAPNP_OK) return
      msg = message_new_root(m, e)
      r = message_return_init(msg, e)
      call return_answer_id_set(r, q, e)
      pl = return_results_init(r, e)
      call payload_content_set(pl, rpc_make_cap_ptr(m, 0), e)
      ctab = payload_cap_table_init(pl, 1_int64, e)
      cd%p = capnp_list_get_struct(ctab, 0, e)
      call cap_descriptor_sender_promise_set(cd, promise_id, e)
      if (e /= CAPNP_OK) return
      call rpc_send_message(peer_fd, m, e)
      call capnp_message_free(m)
   end subroutine inject_promise

   subroutine send_resolve_hosted(peer_fd, promise_id, hosted, e)
      integer, intent(in) :: peer_fd
      integer(int64), intent(in) :: promise_id, hosted
      integer, intent(out) :: e
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(resolve_t) :: rv
      type(cap_descriptor_t) :: cd
      call capnp_message_init_builder(m, e)
      if (e /= CAPNP_OK) return
      msg = message_new_root(m, e)
      rv = message_resolve_init(msg, e)
      call resolve_promise_id_set(rv, promise_id, e)
      cd = resolve_cap_init(rv, e)
      call cap_descriptor_sender_hosted_set(cd, hosted, e)
      if (e /= CAPNP_OK) return
      call rpc_send_message(peer_fd, m, e)
      call capnp_message_free(m)
   end subroutine send_resolve_hosted

   !> A promise import resolved to a senderHosted cap is called as that cap.
   subroutine t_resolve_to_hosted(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: echo_cap, pcap
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(capnp_ptr_t) :: s, content
      integer :: fa, fb, e
      integer(int64) :: qboot, qprom, qcall
      logical :: readable
      call px_socketpair(fa, fb, e)
      boot => local_echo
      call rpc_conn_init(sconn, fb, boot)
      boot => null()
      call rpc_conn_init(cconn, fa, boot)
      call rpc_bootstrap_send(cconn, echo_cap, e)
      qboot = echo_cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call rpc_result_cap(cconn, qboot, [integer ::], echo_cap, e)
      call inject_promise(sconn%fd, cconn, echo_cap, 4_int64, qprom, e)
      call rpc_wait(cconn, qprom, e)
      call rpc_result_cap(cconn, qprom, [integer ::], pcap, e)
      call check_(error, e == CAPNP_OK .and. pcap%id == 4_int64, &
                  'resolve: promise import recorded')
      if (allocated(error)) return
      sconn%exports(4)%used = .true.
      sconn%exports(4)%refcount = 1
      sconn%exports(4)%srv => local_echo
      call send_resolve_hosted(sconn%fd, 4_int64, 0_int64, e)
      call rpc_pump_once(cconn, e)
      call check_(error, e == CAPNP_OK .and. rpc_conn_alive(cconn), &
                  'resolve: hosted resolve accepted')
      call px_poll_in(sconn%fd, 1000, readable, e)
      call check_(error, e == CAPNP_OK .and. readable, &
                  'resolve: promise release arrives')
      if (allocated(error)) return
      call rpc_pump_once(sconn, e)
      call check_(error, .not. sconn%exports(4)%used, &
                  'resolve: promise export is released')
      call check_(error, rpc_conn_alive(sconn), 'resolve: promise release leaves the server up')
      call rpc_call_begin(cconn, pcap, ECHO_IFACE, 0, m, params, qcall, e)
      s = capnp_new_struct(m, 1, 1, e)
      call capnp_set_i64(s, 0_int64, 4_int64, e)
      call capnp_set_text(s, 0, 'x', e)
      call payload_content_set(params, s, e)
      call rpc_call_send(cconn, m, e)
      call rpc_pump_once(sconn, e)
      call wait_for(cconn, qcall, error, 'resolve: call after resolve returns')
      if (allocated(error)) return
      call rpc_result_content(cconn, qcall, content, e)
      call check_(error, e == CAPNP_OK .and. capnp_get_i64(content, 0_int64) == 8_int64, &
                  'resolve: call uses the hosted cap')
      call rpc_release_send(cconn, pcap, e)
      call px_poll_in(sconn%fd, 1000, readable, e)
      call check_(error, e == CAPNP_OK .and. readable, &
                  'resolve: replacement release arrives')
      if (allocated(error)) return
      call rpc_pump_once(sconn, e)
      ! Release drops the wire refcount. The bootstrap answer keeps a
      ! pipeline pin until Finish, so the slot stays until then.
      call check_(error, sconn%exports(0)%refcount == 0, &
                  'resolve: dropping the client releases the replacement')
      call check_(error, rpc_conn_alive(sconn), &
                  'resolve: replacement release leaves the server up')
      call rpc_finish_send(cconn, qboot, .true., e)
      call rpc_pump_once(sconn, e)
      call check_(error, e == CAPNP_OK .and. .not. sconn%exports(0)%used, &
                  'resolve: Finish frees the replacement')
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_resolve_to_hosted

   !> CapDescriptor.none in a Resolve aborts.
   subroutine t_resolve_none(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: echo_cap, pcap
      type(capnp_message_t), target :: m, reply
      type(message_t) :: msg, rmsg
      type(resolve_t) :: rv
      type(cap_descriptor_t) :: cd
      type(exception_t) :: ex
      character(len=:), allocatable :: reason
      integer :: fa, fb, e
      integer(int64) :: qboot, qprom
      call px_socketpair(fa, fb, e)
      boot => local_echo
      call rpc_conn_init(sconn, fb, boot)
      boot => null()
      call rpc_conn_init(cconn, fa, boot)
      call rpc_bootstrap_send(cconn, echo_cap, e)
      qboot = echo_cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call rpc_result_cap(cconn, qboot, [integer ::], echo_cap, e)
      call inject_promise(sconn%fd, cconn, echo_cap, 4_int64, qprom, e)
      call rpc_wait(cconn, qprom, e)
      call rpc_result_cap(cconn, qprom, [integer ::], pcap, e)
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      rv = message_resolve_init(msg, e)
      call resolve_promise_id_set(rv, 4_int64, e)
      cd = resolve_cap_init(rv, e)
      call cap_descriptor_none_set(cd, e)
      call rpc_send_message(sconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(cconn, e)
      call check_(error, .not. rpc_conn_alive(cconn), 'resolve: none aborts')
      call rpc_recv_message(sconn%fd, reply, e)
      rmsg = message_read_root(reply, e)
      ex = message_abort_get(rmsg, e)
      call exception_reason_get(ex, reason, e)
      call check_(error, index(reason, 'CapDescriptor.none') > 0, 'resolve: none reason')
      call capnp_message_free(reply)
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_resolve_none

   !> Resolve for a settled senderHosted import aborts.
   subroutine t_resolve_non_promise(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: echo_cap
      type(capnp_message_t), target :: reply
      type(message_t) :: rmsg
      type(exception_t) :: ex
      character(len=:), allocatable :: reason
      integer :: fa, fb, e
      integer(int64) :: qboot
      call px_socketpair(fa, fb, e)
      boot => local_echo
      call rpc_conn_init(sconn, fb, boot)
      boot => null()
      call rpc_conn_init(cconn, fa, boot)
      call rpc_bootstrap_send(cconn, echo_cap, e)
      qboot = echo_cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call send_resolve_hosted(sconn%fd, 0_int64, 1_int64, e)
      call rpc_pump_once(cconn, e)
      call check_(error, .not. rpc_conn_alive(cconn), 'resolve: non-promise aborts')
      call rpc_recv_message(sconn%fd, reply, e)
      rmsg = message_read_root(reply, e)
      ex = message_abort_get(rmsg, e)
      call exception_reason_get(ex, reason, e)
      call check_(error, index(reason, 'non-promise import') > 0, &
                  'resolve: non-promise reason')
      call capnp_message_free(reply)
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_resolve_non_promise

   !> A Resolve exception makes a later call on the promise fail.
   subroutine t_resolve_exception(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: echo_cap, pcap
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(resolve_t) :: rv
      type(exception_t) :: ex
      type(payload_t) :: params
      integer :: fa, fb, e
      integer(int64) :: qboot, qprom, qcall
      call px_socketpair(fa, fb, e)
      boot => local_echo
      call rpc_conn_init(sconn, fb, boot)
      boot => null()
      call rpc_conn_init(cconn, fa, boot)
      call rpc_bootstrap_send(cconn, echo_cap, e)
      qboot = echo_cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call rpc_result_cap(cconn, qboot, [integer ::], echo_cap, e)
      call inject_promise(sconn%fd, cconn, echo_cap, 4_int64, qprom, e)
      call rpc_wait(cconn, qprom, e)
      call rpc_result_cap(cconn, qprom, [integer ::], pcap, e)
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      rv = message_resolve_init(msg, e)
      call resolve_promise_id_set(rv, 4_int64, e)
      ex = resolve_exception_init(rv, e)
      call exception_type_set(ex, EXCEPTION_TYPE_FAILED_E, e)
      call exception_reason_set(ex, 'broken promise', e)
      call rpc_send_message(sconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(cconn, e)
      call check_(error, rpc_conn_alive(cconn), 'resolve: exception keeps the connection')
      call rpc_call_begin(cconn, pcap, ECHO_IFACE, 0, m, params, qcall, e)
      call check_(error, e == RPC_ERR_EXCEPTION, 'resolve: later call fails')
      call check_(error, index(rpc_conn_reason(cconn), 'broken promise') > 0, &
                  'resolve: exception reason kept')
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_resolve_exception

   !> A call on a promise that then resolves to a local cap issues an
   !> embargo. The matching receiverLoopback is accepted. Any other id
   !> aborts. A reflected resolve with no prior call releases the
   !> promise and sends no disembargo.
   subroutine t_resolve_embargo(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: echo_cap, quiet, called, reused
      type(capnp_message_t), target :: m, reply, drained
      type(message_t) :: msg, rmsg
      type(resolve_t) :: rv
      type(cap_descriptor_t) :: cd
      type(payload_t) :: params
      type(disembargo_t) :: d
      type(release_t) :: rel
      type(message_target_t) :: tgt
      integer :: fa, fb, e, nmsg
      integer(int64) :: qboot, qquiet, qprom, qcall, qreuse, emb
      logical :: readable
      call px_socketpair(fa, fb, e)
      boot => local_echo
      call rpc_conn_init(sconn, fb, boot)
      boot => null()
      call rpc_conn_init(cconn, fa, boot)
      ! receiverHosted 0 names this export; a missing id would be broken.
      cconn%exports(0)%used = .true.
      cconn%exports(0)%refcount = 1
      cconn%exports(0)%srv => local_echo
      call rpc_bootstrap_send(cconn, echo_cap, e)
      qboot = echo_cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call rpc_result_cap(cconn, qboot, [integer ::], echo_cap, e)
      call inject_promise(sconn%fd, cconn, echo_cap, 4_int64, qquiet, e)
      call rpc_wait(cconn, qquiet, e)
      call rpc_result_cap(cconn, qquiet, [integer ::], quiet, e)
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      rv = message_resolve_init(msg, e)
      call resolve_promise_id_set(rv, 4_int64, e)
      cd = resolve_cap_init(rv, e)
      call cap_descriptor_receiver_hosted_set(cd, 0_int64, e)
      call rpc_send_message(sconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(cconn, e)
      call px_poll_in(sconn%fd, 200, readable, e)
      call check_(error, e == CAPNP_OK .and. readable .and. rpc_conn_alive(cconn), &
                  'resolve: quiet resolve releases the promise')
      call recv_within(sconn%fd, reply, error, 'resolve: promise release arrives')
      if (allocated(error)) return
      rmsg = message_read_root(reply, e)
      rel = message_release_get(rmsg, e)
      call check_(error, message_which(rmsg) == MESSAGE_RELEASE_TAG .and. &
                  release_id_get(rel) == 4_int64, 'resolve: quiet release id')
      call capnp_message_free(reply)
      call px_poll_in(sconn%fd, 0, readable, e)
      call check_(error, e == CAPNP_OK .and. .not. readable, &
                  'resolve: no call means no embargo')
      if (allocated(error)) return
      ! The peer may now reuse wire id 4. The earlier application handle
      ! must keep naming its reflected local capability, while the new
      ! delivery receives a separate local import handle.
      sconn%exports(4)%used = .true.
      sconn%exports(4)%refcount = 1
      sconn%exports(4)%srv => local_echo
      call inject_promise(sconn%fd, cconn, echo_cap, 4_int64, qreuse, e)
      call rpc_wait(cconn, qreuse, e)
      call rpc_result_cap(cconn, qreuse, [integer ::], reused, e)
      call check_(error, e == CAPNP_OK .and. reused%kind == RPC_CAP_IMPORT .and. &
                  reused%id /= 4_int64, 'resolve: reused wire id gets private handle')
      if (allocated(error)) return
      call rpc_call_begin(cconn, quiet, ECHO_IFACE, 0, m, params, qcall, e)
      if (e == CAPNP_OK) call payload_content_set(params, capnp_new_struct(m, 0, 0, e), e)
      if (e == CAPNP_OK) call rpc_call_send(cconn, m, e)
      if (e == CAPNP_OK) call rpc_wait(cconn, qcall, e)
      call px_poll_in(sconn%fd, 0, readable, e)
      call check_(error, e == CAPNP_OK .and. .not. readable, &
                  'resolve: reflected capability remains local after reuse')
      if (allocated(error)) return
      ! Finish with releaseResultCaps already released the reused id, so
      ! the later release of the handle sends nothing more.
      call rpc_finish_send(cconn, qreuse, .false., e)
      if (e == CAPNP_OK) call rpc_release_send(cconn, reused, e)
      nmsg = 0
      do while (e == CAPNP_OK)
         call px_poll_in(sconn%fd, 0, readable, e)
         if (e /= CAPNP_OK .or. .not. readable) exit
         call rpc_recv_message(sconn%fd, drained, e)
         call capnp_message_free(drained)
         nmsg = nmsg + 1
      end do
      call check_(error, e == CAPNP_OK .and. nmsg == 1, &
                  'resolve: reused id is released once')
      if (allocated(error)) return
      call inject_promise(sconn%fd, cconn, echo_cap, 6_int64, qprom, e)
      call rpc_wait(cconn, qprom, e)
      call rpc_result_cap(cconn, qprom, [integer ::], called, e)
      call rpc_call_begin(cconn, called, ECHO_IFACE, 0, m, params, qcall, e)
      call rpc_call_send(cconn, m, e)
      sconn%exports(6)%used = .true.
      sconn%exports(6)%refcount = 1
      sconn%exports(6)%srv => local_echo
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qcall, e)
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      rv = message_resolve_init(msg, e)
      call resolve_promise_id_set(rv, 6_int64, e)
      cd = resolve_cap_init(rv, e)
      call cap_descriptor_receiver_hosted_set(cd, 0_int64, e)
      call rpc_send_message(sconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(cconn, e)
      call rpc_recv_message(sconn%fd, reply, e)
      rmsg = message_read_root(reply, e)
      call check_(error, message_which(rmsg) == MESSAGE_DISEMBARGO_TAG, &
                  'resolve: reflected resolve sends Disembargo')
      d = message_disembargo_get(rmsg, e)
      emb = disembargo_context_sender_loopback_get(d)
      tgt = disembargo_target_get(d, e)
      call check_(error, message_target_imported_cap_get(tgt) == 6_int64, &
                  'resolve: embargo targets the promise')
      call capnp_message_free(reply)
      call recv_within(sconn%fd, reply, error, &
                       'resolve: promise release follows the embargo')
      if (allocated(error)) return
      rmsg = message_read_root(reply, e)
      rel = message_release_get(rmsg, e)
      call check_(error, e == CAPNP_OK .and. &
                  message_which(rmsg) == MESSAGE_RELEASE_TAG .and. &
                  release_id_get(rel) == 6_int64, &
                  'resolve: embargoed promise is released')
      call capnp_message_free(reply)
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      d = message_disembargo_init(msg, e)
      call disembargo_context_receiver_loopback_set(d, emb, e)
      tgt = disembargo_target_init(d, e)
      call message_target_imported_cap_set(tgt, 6_int64, e)
      call rpc_send_message(sconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(cconn, e)
      call check_(error, e == CAPNP_OK .and. rpc_conn_alive(cconn), &
                  'resolve: issued receiverLoopback accepted')
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      d = message_disembargo_init(msg, e)
      call disembargo_context_receiver_loopback_set(d, emb + 1_int64, e)
      tgt = disembargo_target_init(d, e)
      call message_target_imported_cap_set(tgt, 6_int64, e)
      call rpc_send_message(sconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(cconn, e)
      call check_(error, .not. rpc_conn_alive(cconn), 'resolve: unknown embargo aborts')
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_resolve_embargo

   !> A Join part that reuses a live question id aborts. The export count
   !> stays where the first part left it.
   subroutine t_join_duplicate_qid(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: cap
      integer :: fa, fb, e, refs
      integer(int64) :: qboot
      call px_socketpair(fa, fb, e)
      boot => local_echo
      call rpc_conn_init(sconn, fb, boot)
      boot => null()
      call rpc_conn_init(cconn, fa, boot)
      call rpc_bootstrap_send(cconn, cap, e)
      qboot = cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call check_(error, sconn%exports(0)%used, 'join qid: export is live')
      if (allocated(error)) return
      call send_join_part(cconn%fd, 5_int64, 0, 80_int64, 2, 0, e)
      call rpc_pump_once(sconn, e)
      refs = sconn%exports(0)%refcount
      call send_join_part(cconn%fd, 5_int64, 0, 80_int64, 2, 1, e)
      call rpc_pump_once(sconn, e)
      call check_(error, sconn%exports(0)%refcount == refs, &
                  'join qid: duplicate does not change the export')
      call check_(error, .not. rpc_conn_alive(sconn), 'join qid: duplicate aborts')
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_join_duplicate_qid

   !> Disembargo.provide from the recipient must not lift the embargo.
   !> The introducer's connection still can.
   subroutine t_disembargo_provide_wrong_conn(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_vat_t), target :: vat_store
      type(rpc_vat_t), pointer :: vat
      type(rpc_conn_t), target :: intro, prov_a, recip, prov_b
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: cap
      integer(int64) :: qboot, pq, aq
      integer :: ia, pa, ra, pb, e
      logical :: readable
      vat => vat_store
      call px_socketpair(ia, pa, e)
      call px_socketpair(ra, pb, e)
      boot => local_echo
      call rpc_conn_init(prov_a, pa, boot)
      call rpc_conn_init(prov_b, pb, boot)
      call rpc_conn_set_vat(prov_a, vat)
      call rpc_conn_set_vat(prov_b, vat)
      boot => null()
      call rpc_conn_init(intro, ia, boot)
      call rpc_conn_init(recip, ra, boot)
      call rpc_bootstrap_send(intro, cap, e)
      qboot = cap%id
      call rpc_pump_once(prov_a, e)
      call rpc_wait(intro, qboot, e)
      call rpc_result_cap(intro, qboot, [integer ::], cap, e)
      call check_(error, e == CAPNP_OK, 'provide embargo: bootstrap')
      if (allocated(error)) return
      call rpc_provide_send(intro, cap%id, '127.0.0.1', 9, 94_int64, pq, e)
      call rpc_pump_once(prov_a, e)
      call rpc_wait(intro, pq, e)
      call rpc_accept_send(recip, 94_int64, .true., aq, e)
      call rpc_pump_once(prov_b, e)
      call check_(error, rpc_embargoed_accepts(prov_b) == 1, &
                  'provide embargo: held')
      call rpc_disembargo_provide_send(recip, pq, e)
      call rpc_pump_once(prov_b, e)
      call px_poll_in(recip%fd, 50, readable, e)
      call check_(error, e == CAPNP_OK .and. .not. readable, &
                  'provide embargo: recipient cannot lift it')
      call check_(error, rpc_embargoed_accepts(prov_a) == 1, &
                  'provide embargo: still held')
      call rpc_disembargo_provide_send(intro, pq, e)
      call rpc_pump_once(prov_a, e)
      call px_poll_in(recip%fd, 200, readable, e)
      call check_(error, readable, 'provide embargo: introducer lifts it')
      call check_(error, rpc_embargoed_accepts(prov_a) == 0, &
                  'provide embargo: cleared')
      call rpc_conn_close(intro)
      call rpc_conn_close(recip)
      call rpc_conn_close(prov_a)
      call rpc_conn_close(prov_b)
   end subroutine t_disembargo_provide_wrong_conn

   !> sendResultsTo.thirdParty aborts, which is what C++ 1.4.0 does.
   subroutine t_third_party_tail_aborts(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: cap
      type(capnp_message_t), target :: m, reply
      type(message_t) :: msg, rmsg
      type(call_t) :: c
      type(payload_t) :: params
      type(recipient_id_t) :: rid
      type(exception_t) :: ex
      type(capnp_ptr_t) :: s
      character(len=:), allocatable :: reason
      integer :: fa, fb, e
      integer(int64) :: qboot, qy
      call px_socketpair(fa, fb, e)
      boot => local_echo
      call rpc_conn_init(sconn, fb, boot)
      boot => null()
      call rpc_conn_init(cconn, fa, boot)
      call rpc_bootstrap_send(cconn, cap, e)
      qboot = cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call rpc_result_cap(cconn, qboot, [integer ::], cap, e)
      call rpc_call_begin(cconn, cap, ECHO_IFACE, 0, m, params, qy, e)
      msg = message_read_root(m, e)
      c = message_call_get(msg, e)
      rid = recipient_id_new(m, e)
      call call_send_results_to_third_party_set(c, rid%p, e)
      s = capnp_new_struct(m, 1, 1, e)
      call capnp_set_i64(s, 0_int64, 1_int64, e)
      call capnp_set_text(s, 0, 'x', e)
      call payload_content_set(params, s, e)
      call rpc_call_send(cconn, m, e)
      call rpc_pump_once(sconn, e)
      call rpc_recv_message(cconn%fd, reply, e)
      rmsg = message_read_root(reply, e)
      call check_(error, message_which(rmsg) == MESSAGE_ABORT_TAG, &
                  'tail: thirdParty aborts')
      ex = message_abort_get(rmsg, e)
      call exception_reason_get(ex, reason, e)
      call check_(error, index(reason, 'sendResultsTo') > 0, 'tail: thirdParty reason')
      call check_(error, .not. rpc_conn_alive(sconn), 'tail: thirdParty kills the connection')
      call capnp_message_free(reply)
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_third_party_tail_aborts

   !> Results held for sendResultsTo.yourself are what a later
   !> takeFromOtherQuestion Return delivers.
   subroutine t_yourself_splice(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: cap, unused
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(call_t) :: c
      type(payload_t) :: params
      type(return_t) :: r
      type(capnp_ptr_t) :: s, content
      integer :: fa, fb, e
      integer(int64) :: qboot, qy, qsrv
      logical :: readable
      call px_socketpair(fa, fb, e)
      boot => local_echo
      call rpc_conn_init(sconn, fb, boot)
      boot => null()
      call rpc_conn_init(cconn, fa, boot)
      call rpc_bootstrap_send(cconn, cap, e)
      qboot = cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call rpc_result_cap(cconn, qboot, [integer ::], cap, e)
      call rpc_call_begin(cconn, cap, ECHO_IFACE, 0, m, params, qy, e)
      msg = message_read_root(m, e)
      c = message_call_get(msg, e)
      call call_send_results_to_yourself_set(c, e)
      s = capnp_new_struct(m, 1, 1, e)
      call capnp_set_i64(s, 0_int64, 4_int64, e)
      call capnp_set_text(s, 0, 'x', e)
      call payload_content_set(params, s, e)
      call rpc_call_send(cconn, m, e)
      call rpc_pump_once(sconn, e)
      call px_poll_in(cconn%fd, 50, readable, e)
      call check_(error, e == CAPNP_OK .and. .not. readable, &
                  'splice: yourself waits for takeFrom')
      if (allocated(error)) return
      call rpc_bootstrap_send(sconn, unused, e)
      qsrv = unused%id
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      r = message_return_init(msg, e)
      call return_answer_id_set(r, qsrv, e)
      call return_take_from_other_question_set(r, qy, e)
      call rpc_send_message(cconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_wait(sconn, qsrv, e)
      call rpc_result_content(sconn, qsrv, content, e)
      call check_(error, e == CAPNP_OK .and. capnp_get_i64(content, 0_int64) == 8_int64, &
                  'splice: takeFromOtherQuestion yields the held result')
      if (allocated(error)) return
      call rpc_wait(cconn, qy, e)
      msg = message_read_root(cconn%questions(int(qy))%retmsg, e)
      r = message_return_get(msg, e)
      call check_(error, return_which(r) == RETURN_RESULTS_SENT_ELSEWHERE_TAG, &
                  'splice: takeFrom sends resultsSentElsewhere')
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_yourself_splice

   !> Releasing more references than an export holds aborts and leaves
   !> the count unchanged.
   subroutine t_release_live_over(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: cap
      type(capnp_message_t), target :: m, reply
      type(message_t) :: msg, rmsg
      type(release_t) :: rel
      type(exception_t) :: ex
      character(len=:), allocatable :: reason
      integer :: fa, fb, e, refs
      integer(int64) :: qboot
      call px_socketpair(fa, fb, e)
      boot => local_echo
      call rpc_conn_init(sconn, fb, boot)
      boot => null()
      call rpc_conn_init(cconn, fa, boot)
      call rpc_bootstrap_send(cconn, cap, e)
      qboot = cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      refs = sconn%exports(0)%refcount
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      rel = message_release_init(msg, e)
      call release_id_set(rel, 0_int64, e)
      call release_reference_count_set(rel, int(refs, int64) + 1_int64, e)
      call rpc_send_message(cconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(sconn, e)
      call check_(error, sconn%exports(0)%used .and. sconn%exports(0)%refcount == refs, &
                  'release: over-release leaves the count')
      call check_(error, .not. rpc_conn_alive(sconn), 'release: over-release aborts')
      call rpc_recv_message(cconn%fd, reply, e)
      rmsg = message_read_root(reply, e)
      ex = message_abort_get(rmsg, e)
      call exception_reason_get(ex, reason, e)
      call check_(error, index(reason, 'below zero') > 0, 'release: over-release reason')
      call capnp_message_free(reply)
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_release_live_over

   !> Two calls on one promise stay in receipt order across a Resolve
   !> for an unknown import.
   subroutine t_call_order(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(order_srv_t), target :: order
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: cap
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(resolve_t) :: rv
      type(cap_descriptor_t) :: cd
      type(payload_t) :: params
      type(capnp_ptr_t) :: s
      integer :: fa, fb, e
      integer(int64) :: qa, qb
      call px_socketpair(fa, fb, e)
      boot => order
      call rpc_conn_init(sconn, fb, boot)
      boot => null()
      call rpc_conn_init(cconn, fa, boot)
      call rpc_bootstrap_send(cconn, cap, e)
      call rpc_call_begin(cconn, cap, ECHO_IFACE, 1, m, params, qa, e)
      s = capnp_new_struct(m, 0, 0, e)
      call payload_content_set(params, s, e)
      call rpc_call_send(cconn, m, e)
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      rv = message_resolve_init(msg, e)
      call resolve_promise_id_set(rv, 1_int64, e)
      cd = resolve_cap_init(rv, e)
      call cap_descriptor_sender_hosted_set(cd, 0_int64, e)
      call rpc_send_message(cconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_call_begin(cconn, cap, ECHO_IFACE, 2, m, params, qb, e)
      s = capnp_new_struct(m, 0, 0, e)
      call payload_content_set(params, s, e)
      call rpc_call_send(cconn, m, e)
      call rpc_pump_once(sconn, e)
      call rpc_pump_once(sconn, e)
      call rpc_pump_once(sconn, e)
      call rpc_pump_once(sconn, e)
      call check_(error, order%n == 2 .and. order%seen(1) == 1 .and. order%seen(2) == 2, &
                  'order: calls stay in receipt order')
      call check_(error, rpc_conn_alive(sconn), 'order: unknown resolve leaves it up')
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_call_order

   subroutine recv_within(fd, m, error, name)
      integer, intent(in) :: fd
      type(capnp_message_t), intent(inout), target :: m
      type(error_type), allocatable, intent(inout) :: error
      character(len=*), intent(in) :: name
      logical :: readable
      integer :: e
      if (allocated(error)) return
      call px_poll_in(fd, 1000, readable, e)
      call check_(error, e == CAPNP_OK .and. readable, trim(name)//' timed out')
      if (allocated(error)) return
      call rpc_recv_message(fd, m, e)
      call check_(error, e == CAPNP_OK, name)
   end subroutine recv_within

   subroutine pump_ready(conn, error, name)
      type(rpc_conn_t), intent(inout), target :: conn
      type(error_type), allocatable, intent(inout) :: error
      character(len=*), intent(in) :: name
      logical :: readable
      integer :: e
      if (allocated(error)) return
      call px_poll_in(conn%fd, 1000, readable, e)
      call check_(error, e == CAPNP_OK .and. readable, name)
      if (allocated(error)) return
      call rpc_pump_once(conn, e)
   end subroutine pump_ready

   subroutine wait_for(conn, qid, error, name)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: qid
      type(error_type), allocatable, intent(inout) :: error
      character(len=*), intent(in) :: name
      integer :: i, e, q
      logical :: handled
      if (allocated(error)) return
      if (qid < 0_int64 .or. qid >= int(size(conn%questions), int64)) then
         call check_(error, .false., name)
         return
      end if
      q = int(qid)
      do i = 1, 40
         if (conn%dead .or. conn%questions(q)%returned) exit
         call rpc_pump_poll(conn, 25, handled, e)
         if (e /= CAPNP_OK .and. e /= RPC_ERR_DEAD) exit
      end do
      call check_(error, conn%questions(q)%returned, name)
   end subroutine wait_for

   subroutine open_echo_pair(cconn, sconn, echo, e)
      type(rpc_conn_t), intent(inout), target :: cconn, sconn
      type(echo_srv_t), intent(inout), target :: echo
      integer, intent(out) :: e
      class(rpc_server_t), pointer :: boot
      integer :: fa, fb
      call px_socketpair(fa, fb, e)
      if (e /= CAPNP_OK) return
      boot => echo
      call rpc_conn_init(sconn, fb, boot)
      boot => null()
      call rpc_conn_init(cconn, fa, boot)
   end subroutine open_echo_pair

   subroutine send_raw_call(fd, qid, export_id, e)
      integer, intent(in) :: fd
      integer(int64), intent(in) :: qid, export_id
      integer, intent(out) :: e
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(call_t) :: c
      type(message_target_t) :: tgt
      type(payload_t) :: params
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
      call message_target_imported_cap_set(tgt, export_id, e)
      if (e /= CAPNP_OK) return
      call call_interface_id_set(c, ECHO_IFACE, e)
      if (e /= CAPNP_OK) return
      call call_method_id_set(c, 0_int32, e)
      if (e /= CAPNP_OK) return
      call call_send_results_to_caller_set(c, e)
      if (e /= CAPNP_OK) return
      params = call_params_init(c, e)
      if (e /= CAPNP_OK) return
      call payload_content_set(params, capnp_new_struct(m, 0, 0, e), e)
      if (e /= CAPNP_OK) return
      call rpc_send_message(fd, m, e)
      call capnp_message_free(m)
   end subroutine send_raw_call

   subroutine send_raw_return(fd, qid, kind, e)
      integer, intent(in) :: fd
      integer(int64), intent(in) :: qid
      character(len=*), intent(in) :: kind
      integer, intent(out) :: e
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(return_t) :: r
      type(payload_t) :: pl
      type(exception_t) :: ex
      call capnp_message_init_builder(m, e)
      if (e /= CAPNP_OK) return
      msg = message_new_root(m, e)
      if (e /= CAPNP_OK) return
      r = message_return_init(msg, e)
      if (e /= CAPNP_OK) return
      call return_answer_id_set(r, qid, e)
      if (e /= CAPNP_OK) return
      select case (kind)
      case ('canceled')
         call return_canceled_set(r, e)
      case ('elsewhere')
         call return_results_sent_elsewhere_set(r, e)
      case ('results')
         pl = return_results_init(r, e)
         if (e == CAPNP_OK) call payload_content_set(pl, capnp_new_struct(m, 0, 0, e), e)
      case ('exception')
         ex = return_exception_init(r, e)
         if (e == CAPNP_OK) call exception_reason_set(ex, 'no', e)
      end select
      if (e /= CAPNP_OK) return
      call rpc_send_message(fd, m, e)
      call capnp_message_free(m)
   end subroutine send_raw_return

   subroutine expect_abort(conn, peer_fd, fragment, error, name)
      type(rpc_conn_t), intent(inout) :: conn
      integer, intent(in) :: peer_fd
      character(len=*), intent(in) :: fragment, name
      type(error_type), allocatable, intent(inout) :: error
      type(capnp_message_t), target :: reply
      type(message_t) :: rmsg
      type(exception_t) :: ex
      character(len=:), allocatable :: reason
      integer :: e
      if (allocated(error)) return
      call rpc_pump_once(conn, e)
      call check_(error, .not. rpc_conn_alive(conn), name)
      call recv_within(peer_fd, reply, error, name)
      if (allocated(error)) return
      rmsg = message_read_root(reply, e)
      ex = message_abort_get(rmsg, e)
      call exception_reason_get(ex, reason, e)
      call check_(error, index(reason, fragment) > 0, name)
      call capnp_message_free(reply)
   end subroutine expect_abort

   subroutine drain_one(fd, error, name)
      integer, intent(in) :: fd
      type(error_type), allocatable, intent(inout) :: error
      character(len=*), intent(in) :: name
      type(capnp_message_t), target :: m
      if (allocated(error)) return
      call recv_within(fd, m, error, name)
      if (allocated(error)) return
      call capnp_message_free(m)
   end subroutine drain_one

   !> A Call whose importedCap is not a live export aborts.
   subroutine t_call_missing_export(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: echo
      integer :: e
      call open_echo_pair(cconn, sconn, echo, e)
      call send_raw_call(cconn%fd, 1_int64, 9_int64, e)
      call expect_abort(sconn, cconn%fd, 'not a current export ID', error, &
                        'call: missing export aborts')
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_call_missing_export

   !> A Return for a question this vat never asked aborts.
   subroutine t_return_unknown_question(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: echo
      integer :: e
      call open_echo_pair(cconn, sconn, echo, e)
      call send_raw_return(sconn%fd, 3_int64, 'results', e)
      call expect_abort(cconn, sconn%fd, 'Invalid question ID in Return message.', error, &
                        'return: unknown question aborts')
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_return_unknown_question

   !> Finish frees the question. A Return that arrives afterwards aborts.
   subroutine t_return_after_finish(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: echo
      type(rpc_cap_t) :: cap
      integer :: e
      integer(int64) :: q
      call open_echo_pair(cconn, sconn, echo, e)
      call rpc_bootstrap_send(cconn, cap, e)
      q = cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, q, e)
      call rpc_finish_send(cconn, q, .true., e)
      call rpc_pump_once(sconn, e)
      call send_raw_return(sconn%fd, q, 'results', e)
      call expect_abort(cconn, sconn%fd, 'Invalid question ID in Return message.', error, &
                        'return: after Finish aborts')
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_return_after_finish

   !> Answer id 64 is outside the question table and aborts.
   subroutine t_return_answer_id_64(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: echo
      integer :: e
      call open_echo_pair(cconn, sconn, echo, e)
      call send_raw_return(sconn%fd, 64_int64, 'results', e)
      call expect_abort(cconn, sconn%fd, 'Invalid question ID in Return message.', error, &
                        'return: answer id 64 aborts')
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_return_answer_id_64

   !> The high bit marks a pipeline-only question. That Return is ignored.
   subroutine t_return_high_bit_ignored(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: echo
      logical :: readable
      integer :: e
      call open_echo_pair(cconn, sconn, echo, e)
      call send_raw_return(sconn%fd, 2147483648_int64, 'results', e)
      call rpc_pump_once(cconn, e)
      call check_(error, e == CAPNP_OK .and. rpc_conn_alive(cconn), &
                  'return: high bit is ignored')
      call px_poll_in(sconn%fd, 0, readable, e)
      call check_(error, e == CAPNP_OK .and. .not. readable, &
                  'return: high bit sends nothing back')
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_return_high_bit_ignored

   !> Return.canceled for a question the caller did not cancel aborts.
   subroutine t_return_falsely_canceled(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: echo
      type(rpc_cap_t) :: cap
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(capnp_ptr_t) :: s
      integer :: e
      integer(int64) :: qboot, q
      call open_echo_pair(cconn, sconn, echo, e)
      call rpc_bootstrap_send(cconn, cap, e)
      qboot = cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call rpc_result_cap(cconn, qboot, [integer ::], cap, e)
      call rpc_call_begin(cconn, cap, ECHO_IFACE, 0, m, params, q, e)
      s = capnp_new_struct(m, 1, 1, e)
      call capnp_set_i64(s, 0_int64, 1_int64, e)
      call capnp_set_text(s, 0, 'x', e)
      call payload_content_set(params, s, e)
      call rpc_call_send(cconn, m, e)
      call drain_one(sconn%fd, error, 'return: canceled drains the call')
      call send_raw_return(sconn%fd, q, 'canceled', e)
      call expect_abort(cconn, sconn%fd, 'falsely claims call was canceled', error, &
                        'return: false cancel aborts')
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_return_falsely_canceled

   !> A Return.canceled for a question this vat already canceled is legal.
   subroutine t_return_canceled_after_cancel(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: echo
      type(rpc_cap_t) :: cap
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(capnp_ptr_t) :: s
      integer :: e
      integer(int64) :: qboot, q
      call open_echo_pair(cconn, sconn, echo, e)
      call rpc_bootstrap_send(cconn, cap, e)
      qboot = cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call rpc_result_cap(cconn, qboot, [integer ::], cap, e)
      call rpc_call_begin(cconn, cap, ECHO_IFACE, 0, m, params, q, e)
      s = capnp_new_struct(m, 1, 1, e)
      call capnp_set_i64(s, 0_int64, 1_int64, e)
      call capnp_set_text(s, 0, 'x', e)
      call payload_content_set(params, s, e)
      call rpc_call_send(cconn, m, e)
      call drain_one(sconn%fd, error, 'return: cancel drains the call')
      call rpc_request_cancellation(cconn, q, e)
      call drain_one(sconn%fd, error, 'return: cancel drains the finish')
      call send_raw_return(sconn%fd, q, 'canceled', e)
      call rpc_pump_once(cconn, e)
      call check_(error, e == CAPNP_OK .and. rpc_conn_alive(cconn), &
                  'return: canceled question accepts Return.canceled')
      call check_(error, .not. cconn%questions(int(q))%used, &
                  'return: canceled question frees the slot')
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_return_canceled_after_cancel

   !> resultsSentElsewhere on a call that was not sendResultsTo.yourself aborts.
   subroutine t_return_elsewhere_not_tail(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: echo
      type(rpc_cap_t) :: cap
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(capnp_ptr_t) :: s
      integer :: e
      integer(int64) :: qboot, q
      call open_echo_pair(cconn, sconn, echo, e)
      call rpc_bootstrap_send(cconn, cap, e)
      qboot = cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call rpc_result_cap(cconn, qboot, [integer ::], cap, e)
      call rpc_call_begin(cconn, cap, ECHO_IFACE, 0, m, params, q, e)
      s = capnp_new_struct(m, 1, 1, e)
      call capnp_set_i64(s, 0_int64, 1_int64, e)
      call capnp_set_text(s, 0, 'x', e)
      call payload_content_set(params, s, e)
      call rpc_call_send(cconn, m, e)
      call drain_one(sconn%fd, error, 'return: elsewhere drains the call')
      call send_raw_return(sconn%fd, q, 'elsewhere', e)
      call expect_abort(cconn, sconn%fd, 'not a tail call', error, &
                        'return: elsewhere on a normal call aborts')
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_return_elsewhere_not_tail

   !> A yourself call must not be answered with results.
   subroutine t_tail_return_with_results(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: echo
      type(rpc_cap_t) :: cap
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(call_t) :: c
      type(payload_t) :: params
      type(capnp_ptr_t) :: s
      integer :: e
      integer(int64) :: qboot, q
      call open_echo_pair(cconn, sconn, echo, e)
      call rpc_bootstrap_send(cconn, cap, e)
      qboot = cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call rpc_result_cap(cconn, qboot, [integer ::], cap, e)
      call rpc_call_begin(cconn, cap, ECHO_IFACE, 0, m, params, q, e)
      msg = message_read_root(m, e)
      c = message_call_get(msg, e)
      call call_send_results_to_yourself_set(c, e)
      s = capnp_new_struct(m, 1, 1, e)
      call capnp_set_i64(s, 0_int64, 1_int64, e)
      call capnp_set_text(s, 0, 'x', e)
      call payload_content_set(params, s, e)
      call rpc_call_send(cconn, m, e)
      call drain_one(sconn%fd, error, 'return: tail drains the call')
      call send_raw_return(sconn%fd, q, 'results', e)
      call expect_abort(cconn, sconn%fd, 'not `results`', error, &
                        'return: results on a tail call abort')
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_tail_return_with_results

   !> A yourself call must not be answered with an exception.
   subroutine t_tail_return_with_exception(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: echo
      type(rpc_cap_t) :: cap
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(call_t) :: c
      type(payload_t) :: params
      type(capnp_ptr_t) :: s
      integer :: e
      integer(int64) :: qboot, q
      call open_echo_pair(cconn, sconn, echo, e)
      call rpc_bootstrap_send(cconn, cap, e)
      qboot = cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call rpc_result_cap(cconn, qboot, [integer ::], cap, e)
      call rpc_call_begin(cconn, cap, ECHO_IFACE, 0, m, params, q, e)
      msg = message_read_root(m, e)
      c = message_call_get(msg, e)
      call call_send_results_to_yourself_set(c, e)
      s = capnp_new_struct(m, 1, 1, e)
      call capnp_set_i64(s, 0_int64, 1_int64, e)
      call capnp_set_text(s, 0, 'x', e)
      call payload_content_set(params, s, e)
      call rpc_call_send(cconn, m, e)
      call drain_one(sconn%fd, error, 'return: tail exception drains the call')
      call send_raw_return(sconn%fd, q, 'exception', e)
      call expect_abort(cconn, sconn%fd, 'not `exception`', error, &
                        'return: exception on a tail call aborts')
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_tail_return_with_exception

   !> A second Resolve for a promise that already resolved releases the
   !> capability it carries without retargeting the original promise.
   subroutine t_resolve_second_ignored(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: echo
      type(rpc_cap_t) :: cap, pcap
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(capnp_ptr_t) :: s, content
      logical :: readable
      integer :: e
      integer(int64) :: qboot, qprom, qcall
      call open_echo_pair(cconn, sconn, echo, e)
      call rpc_bootstrap_send(cconn, cap, e)
      qboot = cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call rpc_result_cap(cconn, qboot, [integer ::], cap, e)
      call inject_promise(sconn%fd, cconn, cap, 4_int64, qprom, e)
      call rpc_wait(cconn, qprom, e)
      call rpc_result_cap(cconn, qprom, [integer ::], pcap, e)
      sconn%exports(4)%used = .true.
      sconn%exports(4)%refcount = 1
      sconn%exports(4)%srv => echo
      call send_resolve_hosted(sconn%fd, 4_int64, 0_int64, e)
      call rpc_pump_once(cconn, e)
      call drain_one(sconn%fd, error, 'resolve: second drains the promise release')
      call send_resolve_hosted(sconn%fd, 4_int64, 2_int64, e)
      call rpc_pump_once(cconn, e)
      call check_(error, rpc_conn_alive(cconn), 'resolve: second resolve is ignored')
      call drain_one(sconn%fd, error, &
                     'resolve: second resolve releases incoming capability')
      call rpc_call_begin(cconn, pcap, ECHO_IFACE, 0, m, params, qcall, e)
      s = capnp_new_struct(m, 1, 1, e)
      call capnp_set_i64(s, 0_int64, 4_int64, e)
      call capnp_set_text(s, 0, 'x', e)
      call payload_content_set(params, s, e)
      call rpc_call_send(cconn, m, e)
      call rpc_pump_once(sconn, e)
      call wait_for(cconn, qcall, error, 'resolve: second still calls the first cap')
      if (allocated(error)) return
      call rpc_result_content(cconn, qcall, content, e)
      call check_(error, e == CAPNP_OK .and. capnp_get_i64(content, 0_int64) == 8_int64, &
                  'resolve: second resolve does not retarget the call')
      call check_(error, rpc_conn_alive(sconn), 'resolve: second resolve leaves the server up')
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_resolve_second_ignored

   !> Unimplemented(Bootstrap) aborts. The question would otherwise never return.
   subroutine t_unimplemented_bootstrap(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: echo
      type(capnp_message_t), target :: m
      type(message_t) :: msg, inner
      type(bootstrap_t) :: b
      integer :: e
      call open_echo_pair(cconn, sconn, echo, e)
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      inner = message_unimplemented_init(msg, e)
      b = message_bootstrap_init(inner, e)
      call bootstrap_question_id_set(b, 1_int64, e)
      call rpc_send_message(cconn%fd, m, e)
      call capnp_message_free(m)
      call expect_abort(sconn, cconn%fd, 'required RPC message type', error, &
                        'unimplemented: bootstrap aborts')
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_unimplemented_bootstrap

   !> Calls made while a reflected promise is embargoed stay off the wire
   !> and run on the local export after the receiverLoopback.
   subroutine t_embargo_queues_locally(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: client_echo, server_echo
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: remote, promise, localboot
      type(capnp_message_t), target :: m, reply
      type(message_t) :: msg, rmsg
      type(resolve_t) :: rv
      type(cap_descriptor_t) :: cd
      type(payload_t) :: params
      type(disembargo_t) :: d
      type(message_target_t) :: tgt
      type(capnp_ptr_t) :: s, content
      logical :: readable
      integer :: fa, fb, e
      integer(int64) :: qboot, qsrv, qprom, qprobe, qheld, qlater, emb
      call px_socketpair(fa, fb, e)
      boot => server_echo
      call rpc_conn_init(sconn, fb, boot)
      boot => client_echo
      call rpc_conn_init(cconn, fa, boot)
      call rpc_bootstrap_send(cconn, remote, e)
      qboot = remote%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call rpc_result_cap(cconn, qboot, [integer ::], remote, e)
      call rpc_bootstrap_send(sconn, localboot, e)
      qsrv = localboot%id
      call rpc_pump_once(cconn, e)
      call rpc_wait(sconn, qsrv, e)
      call check_(error, cconn%exports(0)%used, 'embargo: client exported its bootstrap')
      call inject_promise(sconn%fd, cconn, remote, 4_int64, qprom, e)
      call rpc_wait(cconn, qprom, e)
      call rpc_result_cap(cconn, qprom, [integer ::], promise, e)
      call rpc_call_begin(cconn, promise, ECHO_IFACE, 0, m, params, qprobe, e)
      s = capnp_new_struct(m, 1, 1, e)
      call capnp_set_i64(s, 0_int64, 1_int64, e)
      call capnp_set_text(s, 0, 'x', e)
      call payload_content_set(params, s, e)
      call rpc_call_send(cconn, m, e)
      call drain_one(sconn%fd, error, 'embargo: probe call leaves the wire')
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      rv = message_resolve_init(msg, e)
      call resolve_promise_id_set(rv, 4_int64, e)
      cd = resolve_cap_init(rv, e)
      call cap_descriptor_receiver_hosted_set(cd, 0_int64, e)
      call rpc_send_message(sconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(cconn, e)
      call recv_within(sconn%fd, reply, error, 'embargo: disembargo arrives')
      if (allocated(error)) return
      rmsg = message_read_root(reply, e)
      call check_(error, message_which(rmsg) == MESSAGE_DISEMBARGO_TAG, &
                  'embargo: reflected resolve sends Disembargo')
      d = message_disembargo_get(rmsg, e)
      emb = disembargo_context_sender_loopback_get(d)
      call capnp_message_free(reply)
      call recv_within(sconn%fd, reply, error, 'embargo: promise release arrives')
      if (allocated(error)) return
      rmsg = message_read_root(reply, e)
      call check_(error, message_which(rmsg) == MESSAGE_RELEASE_TAG, &
                  'embargo: promise id is released after the disembargo')
      call capnp_message_free(reply)
      call rpc_call_begin(cconn, promise, ECHO_IFACE, 0, m, params, qheld, e)
      s = capnp_new_struct(m, 1, 1, e)
      call capnp_set_i64(s, 0_int64, 4_int64, e)
      call capnp_set_text(s, 0, 'x', e)
      call payload_content_set(params, s, e)
      call rpc_call_send(cconn, m, e)
      call px_poll_in(sconn%fd, 50, readable, e)
      call check_(error, e == CAPNP_OK .and. .not. readable, &
                  'embargo: a call during the embargo is not sent')
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      d = message_disembargo_init(msg, e)
      call disembargo_context_receiver_loopback_set(d, emb, e)
      tgt = disembargo_target_init(d, e)
      call message_target_imported_cap_set(tgt, 4_int64, e)
      call rpc_send_message(sconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(cconn, e)
      call wait_for(cconn, qheld, error, 'embargo: queued call returns locally')
      if (allocated(error)) return
      call rpc_result_content(cconn, qheld, content, e)
      call check_(error, e == CAPNP_OK .and. capnp_get_i64(content, 0_int64) == 8_int64, &
                  'embargo: queued call ran on the local cap')
      call rpc_call_begin(cconn, promise, ECHO_IFACE, 0, m, params, qlater, e)
      s = capnp_new_struct(m, 1, 1, e)
      call capnp_set_i64(s, 0_int64, 5_int64, e)
      call capnp_set_text(s, 0, 'y', e)
      call payload_content_set(params, s, e)
      call rpc_call_send(cconn, m, e)
      call px_poll_in(sconn%fd, 50, readable, e)
      call check_(error, e == CAPNP_OK .and. .not. readable, &
                  'embargo: a later call stays local')
      call wait_for(cconn, qlater, error, 'embargo: later call returns locally')
      if (allocated(error)) return
      call rpc_result_content(cconn, qlater, content, e)
      call check_(error, e == CAPNP_OK .and. capnp_get_i64(content, 0_int64) == 10_int64, &
                  'embargo: later call ran on the local cap')
      call check_(error, rpc_conn_alive(cconn) .and. rpc_conn_alive(sconn), &
                  'embargo: local delivery leaves both vats up')
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_embargo_queues_locally

   !> A cap-less result carries noFinishNeeded. A result that exports a
   !> capability does not.
   subroutine t_no_finish_needed(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: cap
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(payload_t) :: params
      type(return_t) :: r
      type(capnp_ptr_t) :: s
      integer :: fa, fb, e
      integer(int64) :: qboot, q
      call px_socketpair(fa, fb, e)
      boot => local_echo
      call rpc_conn_init(sconn, fb, boot)
      boot => null()
      call rpc_conn_init(cconn, fa, boot)
      call rpc_bootstrap_send(cconn, cap, e)
      qboot = cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      msg = message_read_root(cconn%questions(int(qboot))%retmsg, e)
      r = message_return_get(msg, e)
      call check_(error, .not. return_no_finish_needed_get(r), &
                  'noFinish: a capability result still needs Finish')
      if (allocated(error)) return
      call rpc_result_cap(cconn, qboot, [integer ::], cap, e)
      call rpc_call_begin(cconn, cap, ECHO_IFACE, 0, m, params, q, e)
      s = capnp_new_struct(m, 1, 1, e)
      call capnp_set_i64(s, 0_int64, 4_int64, e)
      call capnp_set_text(s, 0, 'x', e)
      call payload_content_set(params, s, e)
      call rpc_call_send(cconn, m, e)
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, q, e)
      msg = message_read_root(cconn%questions(int(q))%retmsg, e)
      r = message_return_get(msg, e)
      call check_(error, return_which(r) == RETURN_RESULTS_TAG, &
                  'noFinish: echo returned results')
      call check_(error, return_no_finish_needed_get(r), &
                  'noFinish: a cap-less result sets the flag')
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_no_finish_needed

   !> Resolve to a third-party vine or to one of our own answers is
   !> accepted. Neither form aborts the connection.
   subroutine t_resolve_accepted(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: cap, promise
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(resolve_t) :: rv
      type(cap_descriptor_t) :: cd
      type(third_party_cap_descriptor_t) :: tp
      type(promised_answer_t) :: pa
      type(payload_t) :: params
      type(capnp_ptr_t) :: s, content, ops
      integer :: fa, fb, e
      integer(int64) :: qboot, qprom, qcall
      call px_socketpair(fa, fb, e)
      boot => local_echo
      call rpc_conn_init(cconn, fa, boot)
      boot => null()
      call rpc_conn_init(sconn, fb, boot)
      ! The client answers this bootstrap, so receiverAnswer can name it.
      call rpc_bootstrap_send(sconn, cap, e)
      qboot = cap%id
      call pump_ready(cconn, error, 'bootstrap call arrives')
      call wait_for(sconn, qboot, error, 'question returns')
      cap = rpc_cap_t()
      cap%kind = RPC_CAP_IMPORT
      cap%id = 0_int64
      call inject_promise(sconn%fd, cconn, cap, 6_int64, qprom, e)
      call wait_for(cconn, qprom, error, 'question returns')
      call rpc_result_cap(cconn, qprom, [integer ::], promise, e)
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      rv = message_resolve_init(msg, e)
      call resolve_promise_id_set(rv, 6_int64, e)
      cd = resolve_cap_init(rv, e)
      tp = cap_descriptor_third_party_hosted_init(cd, e)
      call third_party_cap_descriptor_vine_id_set(tp, 9_int64, e)
      call rpc_send_message(sconn%fd, m, e)
      call capnp_message_free(m)
      call pump_ready(cconn, error, 'message arrives')
      call check_(error, e == CAPNP_OK .and. rpc_conn_alive(cconn), &
                  'resolve: thirdPartyHosted is accepted')
      if (allocated(error)) return
      call inject_promise(sconn%fd, cconn, cap, 7_int64, qprom, e)
      call wait_for(cconn, qprom, error, 'question returns')
      call rpc_result_cap(cconn, qprom, [integer ::], promise, e)
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      rv = message_resolve_init(msg, e)
      call resolve_promise_id_set(rv, 7_int64, e)
      cd = resolve_cap_init(rv, e)
      pa = cap_descriptor_receiver_answer_init(cd, e)
      call promised_answer_question_id_set(pa, qboot, e)
      ops = promised_answer_transform_init(pa, 0_int64, e)
      call rpc_send_message(sconn%fd, m, e)
      call capnp_message_free(m)
      call pump_ready(cconn, error, 'message arrives')
      call check_(error, e == CAPNP_OK .and. rpc_conn_alive(cconn), &
                  'resolve: receiverAnswer is accepted')
      if (allocated(error)) return
      call rpc_call_begin(cconn, promise, ECHO_IFACE, 0, m, params, qcall, e)
      s = capnp_new_struct(m, 1, 1, e)
      call capnp_set_i64(s, 0_int64, 4_int64, e)
      call capnp_set_text(s, 0, 'x', e)
      call payload_content_set(params, s, e)
      call rpc_call_send(cconn, m, e)
      call wait_for(cconn, qcall, error, 'question returns')
      call rpc_result_content(cconn, qcall, content, e)
      call check_(error, e == CAPNP_OK .and. capnp_get_i64(content, 0_int64) == 8_int64, &
                  'resolve: receiverAnswer calls the local export')
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_resolve_accepted

   !> Unknown union tags abort with the same text Cap'n Proto 1.4.0 uses.
   subroutine t_unknown_tags(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: cap
      type(capnp_message_t), target :: m, reply
      type(message_t) :: msg, rmsg, inner
      type(call_t) :: c
      type(return_t) :: r
      type(resolve_t) :: rv
      type(payload_t) :: params, pl
      type(message_target_t) :: tgt
      type(promised_answer_t) :: pa
      type(promised_answer_op_t) :: op
      type(cap_descriptor_t) :: cd
      type(exception_t) :: ex
      type(capnp_ptr_t) :: ops, ctab, s
      character(len=:), allocatable :: reason
      integer :: fa, fb, e
      integer(int64) :: q
      call px_socketpair(fa, fb, e)
      boot => local_echo
      call rpc_conn_init(sconn, fb, boot)
      boot => null()
      call rpc_conn_init(cconn, fa, boot)
      call rpc_bootstrap_send(cconn, cap, e)
      call pump_ready(sconn, error, 'message arrives')
      call wait_for(cconn, cap%id, error, 'question returns')
      call rpc_result_cap(cconn, cap%id, [integer ::], cap, e)
      call rpc_call_begin(cconn, cap, ECHO_IFACE, 0, m, params, q, e)
      call capnp_message_free(m)
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      r = message_return_init(msg, e)
      call return_answer_id_set(r, q, e)
      call capnp_set_u16(r%p, 6_int64, 99_int32, e)
      call rpc_send_message(sconn%fd, m, e)
      call capnp_message_free(m)
      call pump_ready(cconn, error, 'message arrives')
      call check_(error, .not. rpc_conn_alive(cconn), 'unknown return aborts')
      call recv_within(sconn%fd, reply, error, 'unknown return abort arrives')
      if (allocated(error)) return
      rmsg = message_read_root(reply, e)
      ex = message_abort_get(rmsg, e)
      call exception_reason_get(ex, reason, e)
      call check_(error, index(reason, "Unknown 'Return' type.") > 0, &
                  'unknown return names the type')
      call capnp_message_free(reply)
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
      if (allocated(error)) return

      call px_socketpair(fa, fb, e)
      boot => local_echo
      call rpc_conn_init(sconn, fb, boot)
      boot => null()
      call rpc_conn_init(cconn, fa, boot)
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      c = message_call_init(msg, e)
      call call_question_id_set(c, 1_int64, e)
      call call_interface_id_set(c, ECHO_IFACE, e)
      call call_method_id_set(c, 0_int32, e)
      tgt = call_target_init(c, e)
      call capnp_set_u16(tgt%p, 4_int64, 9_int32, e)
      params = call_params_init(c, e)
      call rpc_send_message(cconn%fd, m, e)
      call capnp_message_free(m)
      call pump_ready(sconn, error, 'message arrives')
      call check_(error, .not. rpc_conn_alive(sconn), 'unknown target aborts')
      call recv_within(cconn%fd, reply, error, 'unknown target abort arrives')
      if (allocated(error)) return
      rmsg = message_read_root(reply, e)
      ex = message_abort_get(rmsg, e)
      call exception_reason_get(ex, reason, e)
      call check_(error, index(reason, 'Unknown message target type.') > 0, &
                  'unknown target names the type')
      call capnp_message_free(reply)
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)

      call px_socketpair(fa, fb, e)
      boot => local_echo
      call rpc_conn_init(sconn, fb, boot)
      boot => null()
      call rpc_conn_init(cconn, fa, boot)
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      inner = message_unimplemented_init(msg, e)
      call capnp_set_u16(inner%p, 0_int64, int(MESSAGE_RESOLVE_TAG, int32), e)
      rv = message_resolve_init(inner, e)
      call resolve_promise_id_set(rv, 1_int64, e)
      cd = resolve_cap_init(rv, e)
      call cap_descriptor_sender_hosted_set(cd, 40_int64, e)
      call rpc_send_message(cconn%fd, m, e)
      call capnp_message_free(m)
      call pump_ready(sconn, error, 'message arrives')
      call check_(error, .not. rpc_conn_alive(sconn), 'bad unimplemented aborts')
      call recv_within(cconn%fd, reply, error, 'bad unimplemented abort arrives')
      if (allocated(error)) return
      rmsg = message_read_root(reply, e)
      ex = message_abort_get(rmsg, e)
      call exception_reason_get(ex, reason, e)
      call check_(error, index(reason, 'Tried to release invalid export ID.') > 0, &
                  'bad unimplemented names the export')
      call capnp_message_free(reply)
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)

      call px_socketpair(fa, fb, e)
      boot => local_echo
      call rpc_conn_init(sconn, fb, boot)
      boot => null()
      call rpc_conn_init(cconn, fa, boot)
      call rpc_bootstrap_send(cconn, cap, e)
      q = cap%id
      call pump_ready(sconn, error, 'message arrives')
      call wait_for(cconn, q, error, 'question returns')
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      c = message_call_init(msg, e)
      call call_question_id_set(c, 3_int64, e)
      call call_interface_id_set(c, ECHO_IFACE, e)
      call call_method_id_set(c, 0_int32, e)
      tgt = call_target_init(c, e)
      pa = message_target_promised_answer_init(tgt, e)
      call promised_answer_question_id_set(pa, q, e)
      ops = promised_answer_transform_init(pa, 1_int64, e)
      op%p = capnp_list_get_struct(ops, 0, e)
      call capnp_set_u16(op%p, 0_int64, 9_int32, e)
      params = call_params_init(c, e)
      call rpc_send_message(cconn%fd, m, e)
      call capnp_message_free(m)
      call pump_ready(sconn, error, 'message arrives')
      call check_(error, .not. rpc_conn_alive(sconn), 'unknown op aborts')
      call recv_within(cconn%fd, reply, error, 'unknown op abort arrives')
      if (allocated(error)) return
      rmsg = message_read_root(reply, e)
      ex = message_abort_get(rmsg, e)
      call exception_reason_get(ex, reason, e)
      call check_(error, index(reason, 'Unsupported pipeline op.') > 0, &
                  'unknown op names the op')
      call capnp_message_free(reply)
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)

      call px_socketpair(fa, fb, e)
      boot => local_echo
      call rpc_conn_init(sconn, fb, boot)
      boot => null()
      call rpc_conn_init(cconn, fa, boot)
      call rpc_bootstrap_send(cconn, cap, e)
      call pump_ready(sconn, error, 'message arrives')
      call wait_for(cconn, cap%id, error, 'question returns')
      call rpc_result_cap(cconn, cap%id, [integer ::], cap, e)
      call rpc_call_begin(cconn, cap, ECHO_IFACE, 0, m, params, q, e)
      call capnp_message_free(m)
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      r = message_return_init(msg, e)
      call return_answer_id_set(r, q, e)
      pl = return_results_init(r, e)
      ctab = payload_cap_table_init(pl, 1_int64, e)
      cd%p = capnp_list_get_struct(ctab, 0, e)
      call capnp_set_u16(cd%p, 0_int64, 9_int32, e)
      call rpc_send_message(sconn%fd, m, e)
      call capnp_message_free(m)
      call pump_ready(cconn, error, 'message arrives')
      call check_(error, .not. rpc_conn_alive(cconn), 'unknown descriptor aborts')
      call recv_within(sconn%fd, reply, error, 'unknown descriptor abort arrives')
      if (allocated(error)) return
      rmsg = message_read_root(reply, e)
      ex = message_abort_get(rmsg, e)
      call exception_reason_get(ex, reason, e)
      call check_(error, index(reason, 'unknown CapDescriptor type') > 0, &
                  'unknown descriptor names the type')
      call capnp_message_free(reply)
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_unknown_tags

   !> Calling an original promise handle after chained Resolve
   !> (promise -> senderPromise -> receiverHosted) must dispatch locally
   !> and record `received_call` on the followed promise handle.
   subroutine t_resolve_chained_local(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: echo_cap, p10, p11
      type(capnp_message_t), target :: m, reply
      type(message_t) :: msg, rmsg
      type(resolve_t) :: rv
      type(cap_descriptor_t) :: cd
      type(payload_t) :: params
      type(disembargo_t) :: d
      type(message_target_t) :: tgt
      integer :: fa, fb, e
      integer(int64) :: qboot, q10, q11, qcall
      logical :: readable

      call px_socketpair(fa, fb, e)
      boot => local_echo
      call rpc_conn_init(sconn, fb, boot)
      boot => local_echo
      call rpc_conn_init(cconn, fa, boot)
      ! Give cconn export 0 so receiverHosted(0) is a live export on cconn.
      call rpc_bootstrap_send(sconn, echo_cap, e)
      call rpc_pump_once(cconn, e)
      call rpc_wait(sconn, echo_cap%id, e)
      call rpc_bootstrap_send(cconn, echo_cap, e)
      qboot = echo_cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call rpc_result_cap(cconn, qboot, [integer ::], echo_cap, e)

      ! Part A: 10 -> senderPromise(20) -> receiverHosted(0).
      call inject_promise(sconn%fd, cconn, echo_cap, 10_int64, q10, e)
      call rpc_wait(cconn, q10, e)
      call rpc_result_cap(cconn, q10, [integer ::], p10, e)
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      rv = message_resolve_init(msg, e)
      call resolve_promise_id_set(rv, 10_int64, e)
      cd = resolve_cap_init(rv, e)
      call cap_descriptor_sender_promise_set(cd, 20_int64, e)
      call rpc_send_message(sconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(cconn, e)
      call recv_within(sconn%fd, reply, error, 'chained: release 10')
      if (.not. allocated(error)) call capnp_message_free(reply)

      if (.not. allocated(error)) then
         call capnp_message_init_builder(m, e)
         msg = message_new_root(m, e)
         rv = message_resolve_init(msg, e)
         call resolve_promise_id_set(rv, 20_int64, e)
         cd = resolve_cap_init(rv, e)
         call cap_descriptor_receiver_hosted_set(cd, 0_int64, e)
         call rpc_send_message(sconn%fd, m, e)
         call capnp_message_free(m)
         call rpc_pump_once(cconn, e)
         call recv_within(sconn%fd, reply, error, 'chained: release 20')
         if (.not. allocated(error)) call capnp_message_free(reply)
      end if

      if (.not. allocated(error)) then
         call rpc_call_begin(cconn, p10, ECHO_IFACE, 0, m, params, qcall, e)
         if (e == CAPNP_OK) call payload_content_set(params, capnp_new_struct(m, 0, 0, e), e)
         if (e == CAPNP_OK) call rpc_call_send(cconn, m, e)
         call px_poll_in(sconn%fd, 50, readable, e)
         call check_(error, e == CAPNP_OK .and. .not. readable, &
                     'chained: call on original handle dispatches locally')
         if (.not. allocated(error)) then
            call rpc_wait(cconn, qcall, e)
            call check_(error, e == CAPNP_OK, 'chained: local call returns ok')
         end if
      end if

      ! Part B: 11 -> senderPromise(21), call on p11, then 21 -> receiverHosted(0).
      if (.not. allocated(error)) then
         call inject_promise(sconn%fd, cconn, echo_cap, 11_int64, q11, e)
         call rpc_wait(cconn, q11, e)
         call rpc_result_cap(cconn, q11, [integer ::], p11, e)
         call capnp_message_init_builder(m, e)
         msg = message_new_root(m, e)
         rv = message_resolve_init(msg, e)
         call resolve_promise_id_set(rv, 11_int64, e)
         cd = resolve_cap_init(rv, e)
         call cap_descriptor_sender_promise_set(cd, 21_int64, e)
         call rpc_send_message(sconn%fd, m, e)
         call capnp_message_free(m)
         call rpc_pump_once(cconn, e)
         call recv_within(sconn%fd, reply, error, 'chained-call: release 11')
         if (.not. allocated(error)) call capnp_message_free(reply)
      end if
      if (.not. allocated(error)) then
         call rpc_call_begin(cconn, p11, ECHO_IFACE, 0, m, params, qcall, e)
         if (e == CAPNP_OK) call payload_content_set(params, capnp_new_struct(m, 0, 0, e), e)
         if (e == CAPNP_OK) call rpc_call_send(cconn, m, e)
         call recv_within(sconn%fd, reply, error, 'chained-call: wire call to 21')
         if (.not. allocated(error)) call capnp_message_free(reply)
      end if
      if (.not. allocated(error)) then
         call capnp_message_init_builder(m, e)
         msg = message_new_root(m, e)
         rv = message_resolve_init(msg, e)
         call resolve_promise_id_set(rv, 21_int64, e)
         cd = resolve_cap_init(rv, e)
         call cap_descriptor_receiver_hosted_set(cd, 0_int64, e)
         call rpc_send_message(sconn%fd, m, e)
         call capnp_message_free(m)
         call rpc_pump_once(cconn, e)
         call recv_within(sconn%fd, reply, error, 'chained-call: disembargo arrives')
         if (.not. allocated(error)) then
            rmsg = message_read_root(reply, e)
            call check_(error, message_which(rmsg) == MESSAGE_DISEMBARGO_TAG, &
                        'chained-call: sends Disembargo on followed promise')
            if (.not. allocated(error)) then
               d = message_disembargo_get(rmsg, e)
               tgt = disembargo_target_get(d, e)
               call check_(error, message_target_imported_cap_get(tgt) == 21_int64, &
                           'chained-call: Disembargo targets wire id 21')
            end if
            call capnp_message_free(reply)
         end if
      end if
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_resolve_chained_local

   !> When Resolve names a reused wire id that was previously reflected
   !> to IMP_LOCAL, `resolve_remote` must act on the new private handle:
   !> release it when the promise was already dropped, and propagate
   !> `received_call` onto it when the promise had a call.
   subroutine t_resolve_reused_id_target(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: echo_cap, p4, p5, p6
      type(capnp_message_t), target :: m, reply
      type(message_t) :: msg, rmsg
      type(resolve_t) :: rv
      type(cap_descriptor_t) :: cd
      type(payload_t) :: params
      type(release_t) :: rel
      type(disembargo_t) :: d
      type(message_target_t) :: tgt
      integer :: fa, fb, e
      integer(int64) :: qboot, q4, q5, q6, qcall

      call px_socketpair(fa, fb, e)
      boot => local_echo
      call rpc_conn_init(sconn, fb, boot)
      boot => local_echo
      call rpc_conn_init(cconn, fa, boot)
      call rpc_bootstrap_send(sconn, echo_cap, e)
      call rpc_pump_once(cconn, e)
      call rpc_wait(sconn, echo_cap%id, e)
      call rpc_bootstrap_send(cconn, echo_cap, e)
      qboot = echo_cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call rpc_result_cap(cconn, qboot, [integer ::], echo_cap, e)

      ! Turn wire id 4 into a reflected local capability.
      call inject_promise(sconn%fd, cconn, echo_cap, 4_int64, q4, e)
      call rpc_wait(cconn, q4, e)
      call rpc_result_cap(cconn, q4, [integer ::], p4, e)
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      rv = message_resolve_init(msg, e)
      call resolve_promise_id_set(rv, 4_int64, e)
      cd = resolve_cap_init(rv, e)
      call cap_descriptor_receiver_hosted_set(cd, 0_int64, e)
      call rpc_send_message(sconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(cconn, e)
      call recv_within(sconn%fd, reply, error, 'reused-target: release 4')
      if (.not. allocated(error)) call capnp_message_free(reply)

      ! Part A: drop promise 5, then Resolve(5 -> senderHosted(4)).
      if (.not. allocated(error)) then
         call inject_promise(sconn%fd, cconn, echo_cap, 5_int64, q5, e)
         call rpc_wait(cconn, q5, e)
         call rpc_result_cap(cconn, q5, [integer ::], p5, e)
         call rpc_release_send(cconn, p5, e)
         call recv_within(sconn%fd, reply, error, 'reused-target: release 5')
         if (.not. allocated(error)) call capnp_message_free(reply)
      end if
      if (.not. allocated(error)) then
         call capnp_message_init_builder(m, e)
         msg = message_new_root(m, e)
         rv = message_resolve_init(msg, e)
         call resolve_promise_id_set(rv, 5_int64, e)
         cd = resolve_cap_init(rv, e)
         call cap_descriptor_sender_hosted_set(cd, 4_int64, e)
         call rpc_send_message(sconn%fd, m, e)
         call capnp_message_free(m)
         call rpc_pump_once(cconn, e)
         call recv_within(sconn%fd, reply, error, 'reused-target: release reused 4')
         if (.not. allocated(error)) then
            rmsg = message_read_root(reply, e)
            rel = message_release_get(rmsg, e)
            call check_(error, message_which(rmsg) == MESSAGE_RELEASE_TAG .and. &
                        release_id_get(rel) == 4_int64, &
                        'reused-target: released promise drops reused replacement')
            call capnp_message_free(reply)
         end if
      end if

      ! Part B: call promise 6, Resolve(6 -> senderPromise(4)), then
      ! Resolve(4 -> receiverHosted(0)) must emit Disembargo for 4.
      if (.not. allocated(error)) then
         call inject_promise(sconn%fd, cconn, echo_cap, 6_int64, q6, e)
         call rpc_wait(cconn, q6, e)
         call rpc_result_cap(cconn, q6, [integer ::], p6, e)
         call rpc_call_begin(cconn, p6, ECHO_IFACE, 0, m, params, qcall, e)
         if (e == CAPNP_OK) call payload_content_set(params, capnp_new_struct(m, 0, 0, e), e)
         if (e == CAPNP_OK) call rpc_call_send(cconn, m, e)
         call recv_within(sconn%fd, reply, error, 'reused-target: wire call to 6')
         if (.not. allocated(error)) call capnp_message_free(reply)
      end if
      if (.not. allocated(error)) then
         call capnp_message_init_builder(m, e)
         msg = message_new_root(m, e)
         rv = message_resolve_init(msg, e)
         call resolve_promise_id_set(rv, 6_int64, e)
         cd = resolve_cap_init(rv, e)
         call cap_descriptor_sender_promise_set(cd, 4_int64, e)
         call rpc_send_message(sconn%fd, m, e)
         call capnp_message_free(m)
         call rpc_pump_once(cconn, e)
         call recv_within(sconn%fd, reply, error, 'reused-target: release 6')
         if (.not. allocated(error)) call capnp_message_free(reply)
      end if
      if (.not. allocated(error)) then
         call capnp_message_init_builder(m, e)
         msg = message_new_root(m, e)
         rv = message_resolve_init(msg, e)
         call resolve_promise_id_set(rv, 4_int64, e)
         cd = resolve_cap_init(rv, e)
         call cap_descriptor_receiver_hosted_set(cd, 0_int64, e)
         call rpc_send_message(sconn%fd, m, e)
         call capnp_message_free(m)
         call rpc_pump_once(cconn, e)
         call recv_within(sconn%fd, reply, error, 'reused-target: disembargo for 4')
         if (.not. allocated(error)) then
            rmsg = message_read_root(reply, e)
            call check_(error, message_which(rmsg) == MESSAGE_DISEMBARGO_TAG, &
                        'reused-target: propagated call sends Disembargo')
            if (.not. allocated(error)) then
               d = message_disembargo_get(rmsg, e)
               tgt = disembargo_target_get(d, e)
               call check_(error, message_target_imported_cap_get(tgt) == 4_int64, &
                           'reused-target: Disembargo targets wire id 4')
            end if
            call capnp_message_free(reply)
         end if
      end if
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_resolve_reused_id_target

   !> A reflected Resolve arriving after the client already released a
   !> called promise must not emit Disembargo. A Resolve naming a
   !> thirdPartyHosted cap for an unknown or already-released import must
   !> release the vine without aborting.
   subroutine t_resolve_released_no_embargo(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: echo_cap, p7
      type(capnp_message_t), target :: m, reply
      type(message_t) :: msg, rmsg
      type(resolve_t) :: rv
      type(cap_descriptor_t) :: cd
      type(payload_t) :: params
      type(release_t) :: rel
      integer :: fa, fb, e
      integer(int64) :: qboot, q7, qcall
      logical :: readable

      call px_socketpair(fa, fb, e)
      boot => local_echo
      call rpc_conn_init(sconn, fb, boot)
      boot => local_echo
      call rpc_conn_init(cconn, fa, boot)
      call rpc_bootstrap_send(sconn, echo_cap, e)
      call rpc_pump_once(cconn, e)
      call rpc_wait(sconn, echo_cap%id, e)
      call rpc_bootstrap_send(cconn, echo_cap, e)
      qboot = echo_cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call rpc_result_cap(cconn, qboot, [integer ::], echo_cap, e)

      ! Part A: call promise 7, release 7, then Resolve(7 -> receiverHosted(0)).
      call inject_promise(sconn%fd, cconn, echo_cap, 7_int64, q7, e)
      call rpc_wait(cconn, q7, e)
      call rpc_result_cap(cconn, q7, [integer ::], p7, e)
      call rpc_call_begin(cconn, p7, ECHO_IFACE, 0, m, params, qcall, e)
      if (e == CAPNP_OK) call payload_content_set(params, capnp_new_struct(m, 0, 0, e), e)
      if (e == CAPNP_OK) call rpc_call_send(cconn, m, e)
      call recv_within(sconn%fd, reply, error, 'released-resolve: call on 7')
      if (.not. allocated(error)) call capnp_message_free(reply)
      if (.not. allocated(error)) then
         call rpc_release_send(cconn, p7, e)
         call recv_within(sconn%fd, reply, error, 'released-resolve: release 7')
         if (.not. allocated(error)) call capnp_message_free(reply)
      end if
      if (.not. allocated(error)) then
         call capnp_message_init_builder(m, e)
         msg = message_new_root(m, e)
         rv = message_resolve_init(msg, e)
         call resolve_promise_id_set(rv, 7_int64, e)
         cd = resolve_cap_init(rv, e)
         call cap_descriptor_receiver_hosted_set(cd, 0_int64, e)
         call rpc_send_message(sconn%fd, m, e)
         call capnp_message_free(m)
         call rpc_pump_once(cconn, e)
         call px_poll_in(sconn%fd, 50, readable, e)
         call check_(error, e == CAPNP_OK .and. .not. readable, &
                     'released-resolve: no Disembargo after Release')
      end if

      ! Part B: Resolve(99 -> thirdPartyHosted(vine=8)) releases vine 8.
      if (.not. allocated(error)) then
         call capnp_message_init_builder(m, e)
         msg = message_new_root(m, e)
         rv = message_resolve_init(msg, e)
         call resolve_promise_id_set(rv, 99_int64, e)
         cd = resolve_cap_init(rv, e)
         call rpc_write_third_party_cap(cd, '127.0.0.1', 9, 55_int64, 8_int64, e)
         call rpc_send_message(sconn%fd, m, e)
         call capnp_message_free(m)
         call rpc_pump_once(cconn, e)
         call recv_within(sconn%fd, reply, error, 'released-resolve: vine release arrives')
         if (.not. allocated(error)) then
            rmsg = message_read_root(reply, e)
            rel = message_release_get(rmsg, e)
            call check_(error, message_which(rmsg) == MESSAGE_RELEASE_TAG .and. &
                        release_id_get(rel) == 8_int64, &
                        'released-resolve: unknown promise releases thirdParty vine')
            call capnp_message_free(reply)
         end if
      end if

      ! Part C: late Resolve for an already-released IMP_HOSTED import
      ! does not abort and releases the replacement.
      if (.not. allocated(error)) then
         call rpc_release_send(cconn, echo_cap, e)
         call recv_within(sconn%fd, reply, error, 'released-resolve: release echo_cap')
         if (.not. allocated(error)) call capnp_message_free(reply)
      end if
      if (.not. allocated(error)) then
         call capnp_message_init_builder(m, e)
         msg = message_new_root(m, e)
         rv = message_resolve_init(msg, e)
         call resolve_promise_id_set(rv, echo_cap%id, e)
         cd = resolve_cap_init(rv, e)
         call cap_descriptor_sender_hosted_set(cd, 12_int64, e)
         call rpc_send_message(sconn%fd, m, e)
         call capnp_message_free(m)
         call rpc_pump_once(cconn, e)
         call check_(error, rpc_conn_alive(cconn), &
                     'released-resolve: late Resolve on released hosted import ignored')
         call recv_within(sconn%fd, reply, error, 'released-resolve: replacement released')
         if (.not. allocated(error)) then
            rmsg = message_read_root(reply, e)
            rel = message_release_get(rmsg, e)
            call check_(error, message_which(rmsg) == MESSAGE_RELEASE_TAG .and. &
                        release_id_get(rel) == 12_int64, &
                        'released-resolve: late Resolve drops replacement 12')
            call capnp_message_free(reply)
         end if
      end if
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_resolve_released_no_embargo

   !> Provide and Join must translate private import handles allocated
   !> for reused wire ids back to the peer's wire export id.
   subroutine t_reused_handle_provide_join(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_vat_t), target :: vat
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: echo_cap, p4, reused, parts(2)
      type(capnp_message_t), target :: m, reply
      type(message_t) :: msg
      type(resolve_t) :: rv
      type(cap_descriptor_t) :: cd
      integer :: fa, fb, e
      integer(int64) :: qboot, q4, qreuse, pq, jq(2)

      call px_socketpair(fa, fb, e)
      boot => local_echo
      call rpc_conn_init(sconn, fb, boot)
      call rpc_conn_set_vat(sconn, vat)
      boot => local_echo
      call rpc_conn_init(cconn, fa, boot)
      call rpc_bootstrap_send(sconn, echo_cap, e)
      call rpc_pump_once(cconn, e)
      call rpc_wait(sconn, echo_cap%id, e)
      call rpc_bootstrap_send(cconn, echo_cap, e)
      qboot = echo_cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call rpc_result_cap(cconn, qboot, [integer ::], echo_cap, e)

      ! Reflect wire id 4 to IMP_LOCAL so a later delivery of wire id 4
      ! receives a negative private handle.
      call inject_promise(sconn%fd, cconn, echo_cap, 4_int64, q4, e)
      call rpc_wait(cconn, q4, e)
      call rpc_result_cap(cconn, q4, [integer ::], p4, e)
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      rv = message_resolve_init(msg, e)
      call resolve_promise_id_set(rv, 4_int64, e)
      cd = resolve_cap_init(rv, e)
      call cap_descriptor_receiver_hosted_set(cd, 0_int64, e)
      call rpc_send_message(sconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(cconn, e)
      call recv_within(sconn%fd, reply, error, 'reused-handle: release 4')
      if (.not. allocated(error)) call capnp_message_free(reply)

      if (.not. allocated(error)) then
         sconn%exports(4)%used = .true.
         sconn%exports(4)%refcount = 1
         sconn%exports(4)%srv => local_echo
         call inject_promise(sconn%fd, cconn, echo_cap, 4_int64, qreuse, e)
         call rpc_wait(cconn, qreuse, e)
         call rpc_result_cap(cconn, qreuse, [integer ::], reused, e)
         call check_(error, e == CAPNP_OK .and. reused%id < 0_int64, &
                     'reused-handle: got private handle')
      end if

      if (.not. allocated(error)) then
         call rpc_provide_send(cconn, reused%id, '127.0.0.1', 9, 88_int64, pq, e)
         if (e == CAPNP_OK) call rpc_pump_once(sconn, e)
         if (e == CAPNP_OK) call rpc_wait(cconn, pq, e)
         call check_(error, e == CAPNP_OK .and. rpc_conn_alive(sconn), &
                     'reused-handle: Provide translates private handle to wire id')
      end if

      if (.not. allocated(error)) then
         parts(1) = reused
         parts(2) = reused
         call rpc_join_send(cconn, parts, 91_int64, jq, e)
         if (e == CAPNP_OK) call rpc_pump_once(sconn, e)
         if (e == CAPNP_OK) call rpc_pump_once(sconn, e)
         if (e == CAPNP_OK) call rpc_wait(cconn, jq(1), e)
         if (e == CAPNP_OK) call rpc_wait(cconn, jq(2), e)
         call check_(error, e == CAPNP_OK .and. rpc_conn_alive(sconn), &
                     'reused-handle: Join translates private handle to wire id')
      end if
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_reused_handle_provide_join

   !> A promise that resolved to a remote capability keeps naming that
   !> capability when the peer hands the promise's export id out again.
   subroutine t_resolve_remote_reused_id(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: echo_cap, old, fresh
      type(capnp_message_t), target :: reply, drained
      type(message_t) :: rmsg
      type(release_t) :: rel
      integer :: fa, fb, e, nmsg
      integer(int64) :: qboot, qold, qfresh
      logical :: readable
      call px_socketpair(fa, fb, e)
      boot => local_echo
      call rpc_conn_init(sconn, fb, boot)
      boot => null()
      call rpc_conn_init(cconn, fa, boot)
      call rpc_bootstrap_send(cconn, echo_cap, e)
      qboot = echo_cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call rpc_result_cap(cconn, qboot, [integer ::], echo_cap, e)
      call inject_promise(sconn%fd, cconn, echo_cap, 4_int64, qold, e)
      call rpc_wait(cconn, qold, e)
      call rpc_result_cap(cconn, qold, [integer ::], old, e)
      call send_resolve_hosted(sconn%fd, 4_int64, 5_int64, e)
      call rpc_pump_once(cconn, e)
      call recv_within(sconn%fd, reply, error, 'remote-reuse: promise release')
      if (allocated(error)) return
      rmsg = message_read_root(reply, e)
      rel = message_release_get(rmsg, e)
      call check_(error, message_which(rmsg) == MESSAGE_RELEASE_TAG .and. &
                  release_id_get(rel) == 4_int64, 'remote-reuse: promise 4 released')
      call capnp_message_free(reply)
      if (allocated(error)) return
      call inject_promise(sconn%fd, cconn, echo_cap, 4_int64, qfresh, e)
      call rpc_wait(cconn, qfresh, e)
      call rpc_result_cap(cconn, qfresh, [integer ::], fresh, e)
      call check_(error, e == CAPNP_OK .and. fresh%id /= old%id, &
                  'remote-reuse: reused id gets its own handle')
      if (allocated(error)) return
      call rpc_release_send(cconn, old, e)
      call recv_within(sconn%fd, reply, error, 'remote-reuse: replacement release')
      if (allocated(error)) return
      rmsg = message_read_root(reply, e)
      rel = message_release_get(rmsg, e)
      call check_(error, message_which(rmsg) == MESSAGE_RELEASE_TAG .and. &
                  release_id_get(rel) == 5_int64 .and. &
                  release_reference_count_get(rel) == 1_int64, &
                  'remote-reuse: old handle releases its replacement')
      call capnp_message_free(reply)
      if (allocated(error)) return
      call rpc_release_send(cconn, fresh, e)
      call recv_within(sconn%fd, reply, error, 'remote-reuse: fresh release')
      if (allocated(error)) return
      rmsg = message_read_root(reply, e)
      rel = message_release_get(rmsg, e)
      call check_(error, message_which(rmsg) == MESSAGE_RELEASE_TAG .and. &
                  release_id_get(rel) == 4_int64, &
                  'remote-reuse: fresh handle releases the reused id')
      call capnp_message_free(reply)
      nmsg = 0
      do while (e == CAPNP_OK)
         call px_poll_in(sconn%fd, 0, readable, e)
         if (e /= CAPNP_OK .or. .not. readable) exit
         call rpc_recv_message(sconn%fd, drained, e)
         call capnp_message_free(drained)
         nmsg = nmsg + 1
      end do
      call check_(error, e == CAPNP_OK .and. nmsg == 0, &
                  'remote-reuse: nothing else is released')
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_resolve_remote_reused_id

   !> A result that points back at the peer is `receiverHosted`. C++
   !> echoes `senderLoopback` as `receiverLoopback` aimed at that import
   !> (`rpc.c++` `handleDisembargo`, lines 3477-3510).
   subroutine t_sender_loopback_echo(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(reflect_srv_t), target :: reflector
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: cap
      type(capnp_message_t), target :: m, reply
      type(message_t) :: msg, rmsg
      type(disembargo_t) :: d, rd
      type(message_target_t) :: tgt, rtgt
      type(promised_answer_t) :: pa
      type(payload_t) :: params
      type(capnp_ptr_t) :: s
      integer :: fa, fb, e
      integer(int64) :: qboot, qcall
      call px_socketpair(fa, fb, e)
      boot => reflector
      call rpc_conn_init(sconn, fb, boot)
      boot => null()
      call rpc_conn_init(cconn, fa, boot)
      call rpc_bootstrap_send(cconn, cap, e)
      qboot = cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call rpc_result_cap(cconn, qboot, [integer ::], cap, e)
      call check_(error, e == CAPNP_OK, 'loopback echo: bootstrap')
      if (allocated(error)) return
      call rpc_call_begin(cconn, cap, 1_int64, 0, m, params, qcall, e)
      s = capnp_new_struct(m, 0, 0, e)
      call payload_content_set(params, s, e)
      call rpc_call_send(cconn, m, e)
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qcall, e)
      call check_(error, e == CAPNP_OK, 'loopback echo: reflected result')
      if (allocated(error)) return
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      d = message_disembargo_init(msg, e)
      call disembargo_context_sender_loopback_set(d, 41_int64, e)
      tgt = disembargo_target_init(d, e)
      pa = message_target_promised_answer_init(tgt, e)
      call promised_answer_question_id_set(pa, qcall, e)
      call rpc_send_message(cconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(sconn, e)
      call recv_within(cconn%fd, reply, error, 'loopback echo: reply arrives')
      if (allocated(error)) return
      rmsg = message_read_root(reply, e)
      call check_(error, e == CAPNP_OK .and. &
                  message_which(rmsg) == MESSAGE_DISEMBARGO_TAG, &
                  'loopback echo: reply is Disembargo')
      rd = message_disembargo_get(rmsg, e)
      call check_(error, disembargo_context_which(rd) == &
                  DISEMBARGO_CONTEXT_RECEIVER_LOOPBACK_TAG .and. &
                  disembargo_context_receiver_loopback_get(rd) == 41_int64, &
                  'loopback echo: receiverLoopback carries the embargo id')
      rtgt = disembargo_target_get(rd, e)
      call check_(error, message_target_which(rtgt) == &
                  MESSAGE_TARGET_IMPORTED_CAP_TAG .and. &
                  message_target_imported_cap_get(rtgt) == 4_int64, &
                  'loopback echo: target is the reflected import')
      call check_(error, rpc_conn_alive(sconn), 'loopback echo: vat stays up')
      call capnp_message_free(reply)
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_sender_loopback_echo

   !> The same echo for a call with sendResultsTo.yourself. Its results
   !> stay here, and a result that points back at the peer still echoes.
   subroutine t_sender_loopback_echo_yourself(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(reflect_srv_t), target :: reflector
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: cap
      type(capnp_message_t), target :: m, reply
      type(message_t) :: msg, rmsg
      type(call_t) :: c
      type(disembargo_t) :: d, rd
      type(message_target_t) :: tgt, rtgt
      type(promised_answer_t) :: pa
      type(payload_t) :: params
      type(capnp_ptr_t) :: s
      integer :: fa, fb, e
      integer(int64) :: qboot, qcall
      call px_socketpair(fa, fb, e)
      boot => reflector
      call rpc_conn_init(sconn, fb, boot)
      boot => null()
      call rpc_conn_init(cconn, fa, boot)
      call rpc_bootstrap_send(cconn, cap, e)
      qboot = cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call rpc_result_cap(cconn, qboot, [integer ::], cap, e)
      call rpc_call_begin(cconn, cap, 1_int64, 0, m, params, qcall, e)
      msg = message_read_root(m, e)
      c = message_call_get(msg, e)
      call call_send_results_to_yourself_set(c, e)
      s = capnp_new_struct(m, 0, 0, e)
      call payload_content_set(params, s, e)
      call rpc_call_send(cconn, m, e)
      call rpc_pump_once(sconn, e)
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      d = message_disembargo_init(msg, e)
      call disembargo_context_sender_loopback_set(d, 42_int64, e)
      tgt = disembargo_target_init(d, e)
      pa = message_target_promised_answer_init(tgt, e)
      call promised_answer_question_id_set(pa, qcall, e)
      call rpc_send_message(cconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(sconn, e)
      call check_(error, rpc_conn_alive(sconn), 'loopback echo yourself: vat stays up')
      if (.not. allocated(error)) then
         call recv_within(cconn%fd, reply, error, 'loopback echo yourself: reply arrives')
      end if
      if (.not. allocated(error)) then
         rmsg = message_read_root(reply, e)
         call check_(error, e == CAPNP_OK .and. &
                     message_which(rmsg) == MESSAGE_DISEMBARGO_TAG, &
                     'loopback echo yourself: reply is Disembargo')
         rd = message_disembargo_get(rmsg, e)
         call check_(error, disembargo_context_which(rd) == &
                     DISEMBARGO_CONTEXT_RECEIVER_LOOPBACK_TAG .and. &
                     disembargo_context_receiver_loopback_get(rd) == 42_int64, &
                     'loopback echo yourself: receiverLoopback carries the id')
         rtgt = disembargo_target_get(rd, e)
         call check_(error, message_target_which(rtgt) == &
                     MESSAGE_TARGET_IMPORTED_CAP_TAG .and. &
                     message_target_imported_cap_get(rtgt) == 4_int64, &
                     'loopback echo yourself: target is the reflected import')
         call capnp_message_free(reply)
      end if
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_sender_loopback_echo_yourself

   !> C++ `receiveCap` imports the vine of a `thirdPartyHosted` result
   !> (`rpc.c++` lines 1628-1630). `rpc_result_cap` must return that
   !> import, and the level-3 pickup must share its one reference.
   subroutine t_result_third_party_vine(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: cap, vine
      type(rpc_introduction_t) :: got(4)
      type(capnp_message_t), target :: m, reply
      type(message_t) :: msg, rmsg
      type(return_t) :: r
      type(payload_t) :: params, results
      type(cap_descriptor_t) :: cd
      type(capnp_ptr_t) :: ctab
      type(release_t) :: rel
      integer :: fa, fb, e, n
      integer(int64) :: qboot, qcall
      logical :: readable
      call px_socketpair(fa, fb, e)
      boot => local_echo
      call rpc_conn_init(sconn, fb, boot)
      boot => null()
      call rpc_conn_init(cconn, fa, boot)
      call rpc_bootstrap_send(cconn, cap, e)
      qboot = cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call rpc_result_cap(cconn, qboot, [integer ::], cap, e)
      call check_(error, e == CAPNP_OK, 'vine result: bootstrap')
      if (allocated(error)) return
      call rpc_call_begin(cconn, cap, 1_int64, 0, m, params, qcall, e)
      call capnp_message_free(m)
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      r = message_return_init(msg, e)
      call return_answer_id_set(r, qcall, e)
      results = return_results_init(r, e)
      call payload_content_set(results, rpc_make_cap_ptr(m, 0), e)
      ctab = payload_cap_table_init(results, 1_int64, e)
      cd%p = capnp_list_get_struct(ctab, 0, e)
      call rpc_write_third_party_cap(cd, '10.1.0.3', 5003, 909_int64, 80_int64, e)
      call rpc_send_message(sconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(cconn, e)
      call rpc_result_cap(cconn, qcall, [integer ::], vine, e)
      call check_(error, e == CAPNP_OK .and. vine%kind == RPC_CAP_IMPORT .and. &
                  vine%id == 80_int64, 'vine result: the vine is the result cap')
      if (allocated(error)) return
      n = rpc_pending_introductions(cconn, got)
      call check_(error, n == 1 .and. got(1)%vine_id == 80_int64 .and. &
                  got(1)%nonce == 909_int64, 'vine result: introduction recorded')
      if (allocated(error)) return
      call rpc_release_send(cconn, vine, e)
      call px_poll_in(sconn%fd, 1000, readable, e)
      call check_(error, e == CAPNP_OK .and. readable, 'vine result: release arrives')
      if (allocated(error)) return
      call rpc_recv_message(sconn%fd, reply, e)
      rmsg = message_read_root(reply, e)
      rel = message_release_get(rmsg, e)
      call check_(error, e == CAPNP_OK .and. &
                  message_which(rmsg) == MESSAGE_RELEASE_TAG .and. &
                  release_id_get(rel) == 80_int64 .and. &
                  release_reference_count_get(rel) == 1_int64, &
                  'vine result: one reference of the vine')
      call capnp_message_free(reply)
      call rpc_introduction_done(cconn, 909_int64, e)
      call px_poll_in(sconn%fd, 50, readable, e)
      call check_(error, e == CAPNP_OK .and. .not. readable, &
                  'vine result: pickup shares that reference')
      call check_(error, rpc_pending_introductions(cconn) == 0, &
                  'vine result: introduction cleared')
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_result_third_party_vine

   !> Finish(releaseResultCaps=true) drops a thirdPartyHosted vine import so a
   !> later rpc_introduction_done sends no duplicate Release.
   subroutine t_result_third_party_vine_finish(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: cap
      type(capnp_message_t), target :: m, reply
      type(message_t) :: msg, rmsg
      type(return_t) :: r
      type(payload_t) :: params, results
      type(cap_descriptor_t) :: cd
      type(capnp_ptr_t) :: ctab
      integer :: fa, fb, e
      integer(int64) :: qboot, qcall
      logical :: readable
      call px_socketpair(fa, fb, e)
      boot => local_echo
      call rpc_conn_init(sconn, fb, boot)
      boot => null()
      call rpc_conn_init(cconn, fa, boot)
      call rpc_bootstrap_send(cconn, cap, e)
      qboot = cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call rpc_result_cap(cconn, qboot, [integer ::], cap, e)
      call rpc_call_begin(cconn, cap, 1_int64, 0, m, params, qcall, e)
      call capnp_message_free(m)
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      r = message_return_init(msg, e)
      call return_answer_id_set(r, qcall, e)
      results = return_results_init(r, e)
      call payload_content_set(results, rpc_make_cap_ptr(m, 0), e)
      ctab = payload_cap_table_init(results, 1_int64, e)
      cd%p = capnp_list_get_struct(ctab, 0, e)
      call rpc_write_third_party_cap(cd, '10.1.0.4', 5004, 910_int64, 81_int64, e)
      call rpc_send_message(sconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(cconn, e)
      call rpc_finish_send(cconn, qcall, .false., e)
      call px_poll_in(sconn%fd, 1000, readable, e)
      call check_(error, e == CAPNP_OK .and. readable, 'vine finish: Finish arrives')
      if (allocated(error)) then
         call rpc_conn_close(cconn); call rpc_conn_close(sconn); return
      end if
      call rpc_recv_message(sconn%fd, reply, e)
      rmsg = message_read_root(reply, e)
      call check_(error, e == CAPNP_OK .and. message_which(rmsg) == MESSAGE_FINISH_TAG, &
                  'vine finish: Finish tag')
      call capnp_message_free(reply)
      call rpc_introduction_done(cconn, 910_int64, e)
      call px_poll_in(sconn%fd, 50, readable, e)
      call check_(error, e == CAPNP_OK .and. .not. readable, &
                  'vine finish: no duplicate Release after Finish')
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_result_third_party_vine_finish

   !> A Finish that released a thirdPartyHosted vine frees its export id.
   !> When the peer hands that id out again, a late `rpc_introduction_done`
   !> for the first Return must not release the new reference.
   subroutine t_vine_done_after_finish_reused_id(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: cap, cap2
      type(capnp_message_t), target :: m, reply
      type(message_t) :: msg, rmsg
      type(return_t) :: r
      type(release_t) :: rel
      type(payload_t) :: params, results
      type(cap_descriptor_t) :: cd
      type(capnp_ptr_t) :: ctab
      integer :: fa, fb, e
      integer(int64) :: qboot, qcall, qcall2
      logical :: readable
      call px_socketpair(fa, fb, e)
      boot => local_echo
      call rpc_conn_init(sconn, fb, boot)
      boot => null()
      call rpc_conn_init(cconn, fa, boot)
      call rpc_bootstrap_send(cconn, cap, e)
      qboot = cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call rpc_result_cap(cconn, qboot, [integer ::], cap, e)
      call rpc_call_begin(cconn, cap, 1_int64, 0, m, params, qcall, e)
      call capnp_message_free(m)
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      r = message_return_init(msg, e)
      call return_answer_id_set(r, qcall, e)
      results = return_results_init(r, e)
      call payload_content_set(results, rpc_make_cap_ptr(m, 0), e)
      ctab = payload_cap_table_init(results, 1_int64, e)
      cd%p = capnp_list_get_struct(ctab, 0, e)
      call rpc_write_third_party_cap(cd, '10.1.0.4', 5004, 911_int64, 81_int64, e)
      call rpc_send_message(sconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(cconn, e)
      call rpc_finish_send(cconn, qcall, .false., e)
      call rpc_recv_message(sconn%fd, reply, e)
      rmsg = message_read_root(reply, e)
      call check_(error, e == CAPNP_OK .and. message_which(rmsg) == MESSAGE_FINISH_TAG, &
                  'vine reuse: Finish tag')
      call capnp_message_free(reply)
      call rpc_call_begin(cconn, cap, 1_int64, 0, m, params, qcall2, e)
      call capnp_message_free(m)
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      r = message_return_init(msg, e)
      call return_answer_id_set(r, qcall2, e)
      results = return_results_init(r, e)
      call payload_content_set(results, rpc_make_cap_ptr(m, 0), e)
      ctab = payload_cap_table_init(results, 1_int64, e)
      cd%p = capnp_list_get_struct(ctab, 0, e)
      call cap_descriptor_sender_hosted_set(cd, 81_int64, e)
      call rpc_send_message(sconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(cconn, e)
      call rpc_result_cap(cconn, qcall2, [integer ::], cap2, e)
      call check_(error, e == CAPNP_OK .and. cap2%kind == RPC_CAP_IMPORT, &
                  'vine reuse: second result cap')
      call rpc_introduction_done(cconn, 911_int64, e)
      call px_poll_in(sconn%fd, 50, readable, e)
      call check_(error, e == CAPNP_OK .and. .not. readable, &
                  'vine reuse: stale done leaves the new reference alone')
      call rpc_release_send(cconn, cap2, e)
      call px_poll_in(sconn%fd, 1000, readable, e)
      call check_(error, e == CAPNP_OK .and. readable, &
                  'vine reuse: releasing the new capability sends Release')
      if (.not. allocated(error)) then
         call rpc_recv_message(sconn%fd, reply, e)
         rmsg = message_read_root(reply, e)
         call check_(error, e == CAPNP_OK .and. message_which(rmsg) == MESSAGE_RELEASE_TAG, &
                     'vine reuse: Release tag')
         if (.not. allocated(error)) then
            rel = message_release_get(rmsg, e)
            call check_(error, release_id_get(rel) == 81_int64 .and. &
                        release_reference_count_get(rel) == 1_int64, &
                        'vine reuse: Release names the reused id once')
         end if
         call capnp_message_free(reply)
      end if
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_vine_done_after_finish_reused_id

   !> A thirdPartyHosted vine whose wire id collides with a resolved promise
   !> uses its private import handle in rpc_introduction_done.
   subroutine t_result_third_party_vine_reused_id(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: cap, pcap, vine
      type(capnp_message_t), target :: m, reply
      type(message_t) :: msg, rmsg
      type(return_t) :: r
      type(resolve_t) :: rv
      type(payload_t) :: params, results
      type(cap_descriptor_t) :: cd
      type(capnp_ptr_t) :: ctab
      type(release_t) :: rel
      integer :: fa, fb, e
      integer(int64) :: qboot, q1, q2
      logical :: readable
      call px_socketpair(fa, fb, e)
      boot => local_echo
      call rpc_conn_init(sconn, fb, boot)
      boot => null()
      call rpc_conn_init(cconn, fa, boot)
      call rpc_bootstrap_send(cconn, cap, e)
      qboot = cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call rpc_result_cap(cconn, qboot, [integer ::], cap, e)

      ! q1 returns senderPromise(1), then resolves 1 -> senderHosted(2).
      call rpc_call_begin(cconn, cap, 1_int64, 0, m, params, q1, e)
      call capnp_message_free(m)
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      r = message_return_init(msg, e)
      call return_answer_id_set(r, q1, e)
      results = return_results_init(r, e)
      call payload_content_set(results, rpc_make_cap_ptr(m, 0), e)
      ctab = payload_cap_table_init(results, 1_int64, e)
      cd%p = capnp_list_get_struct(ctab, 0, e)
      call cap_descriptor_sender_promise_set(cd, 1_int64, e)
      call rpc_send_message(sconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(cconn, e)
      call rpc_result_cap(cconn, q1, [integer ::], pcap, e)

      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      rv = message_resolve_init(msg, e)
      call resolve_promise_id_set(rv, 1_int64, e)
      cd = resolve_cap_init(rv, e)
      call cap_descriptor_sender_hosted_set(cd, 2_int64, e)
      call rpc_send_message(sconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(cconn, e)
      ! Drain Release(1, 1) from Resolve.
      call rpc_recv_message(sconn%fd, reply, e)
      call capnp_message_free(reply)

      ! q2 reuses wire id 1 as a thirdPartyHosted vine.
      call rpc_call_begin(cconn, cap, 1_int64, 0, m, params, q2, e)
      call capnp_message_free(m)
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      r = message_return_init(msg, e)
      call return_answer_id_set(r, q2, e)
      results = return_results_init(r, e)
      call payload_content_set(results, rpc_make_cap_ptr(m, 0), e)
      ctab = payload_cap_table_init(results, 1_int64, e)
      cd%p = capnp_list_get_struct(ctab, 0, e)
      call rpc_write_third_party_cap(cd, '10.1.0.5', 5005, 911_int64, 1_int64, e)
      call rpc_send_message(sconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(cconn, e)
      call rpc_result_cap(cconn, q2, [integer ::], vine, e)
      call check_(error, e == CAPNP_OK .and. vine%id < -1_int64, &
                  'vine reuse: private handle assigned')
      if (allocated(error)) then
         call rpc_conn_close(cconn); call rpc_conn_close(sconn); return
      end if

      ! rpc_introduction_done must release wire id 1 (the vine), not 2 (pcap).
      call rpc_introduction_done(cconn, 911_int64, e)
      call px_poll_in(sconn%fd, 1000, readable, e)
      call check_(error, e == CAPNP_OK .and. readable, 'vine reuse: release arrives')
      if (allocated(error)) then
         call rpc_conn_close(cconn); call rpc_conn_close(sconn); return
      end if
      call rpc_recv_message(sconn%fd, reply, e)
      rmsg = message_read_root(reply, e)
      rel = message_release_get(rmsg, e)
      call check_(error, e == CAPNP_OK .and. &
                  message_which(rmsg) == MESSAGE_RELEASE_TAG .and. &
                  release_id_get(rel) == 1_int64 .and. &
                  release_reference_count_get(rel) == 1_int64, &
                  'vine reuse: releases vine wire id 1')
      call capnp_message_free(reply)

      ! Releasing pcap then releases wire id 2.
      call rpc_release_send(cconn, pcap, e)
      call rpc_recv_message(sconn%fd, reply, e)
      rmsg = message_read_root(reply, e)
      rel = message_release_get(rmsg, e)
      call check_(error, e == CAPNP_OK .and. &
                  message_which(rmsg) == MESSAGE_RELEASE_TAG .and. &
                  release_id_get(rel) == 2_int64 .and. &
                  release_reference_count_get(rel) == 1_int64, &
                  'vine reuse: pcap still releases wire id 2')
      call capnp_message_free(reply)
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_result_third_party_vine_reused_id

   !> Delayed rpc_result_cap returns each question's own import handle even
   !> after its wire id was resolved and reused by a later Return.
   subroutine t_delayed_result_cap_reused_id(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: cap, c1, c2
      type(capnp_message_t), target :: m, reply
      type(message_t) :: msg, rmsg
      type(return_t) :: r
      type(resolve_t) :: rv
      type(payload_t) :: params, results
      type(cap_descriptor_t) :: cd
      type(capnp_ptr_t) :: ctab
      type(release_t) :: rel
      integer :: fa, fb, e
      integer(int64) :: qboot, q1, q2
      call px_socketpair(fa, fb, e)
      boot => local_echo
      call rpc_conn_init(sconn, fb, boot)
      boot => null()
      call rpc_conn_init(cconn, fa, boot)
      call rpc_bootstrap_send(cconn, cap, e)
      qboot = cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call rpc_result_cap(cconn, qboot, [integer ::], cap, e)

      ! q1 returns senderPromise(1); do not read rpc_result_cap yet.
      call rpc_call_begin(cconn, cap, 1_int64, 0, m, params, q1, e)
      call capnp_message_free(m)
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      r = message_return_init(msg, e)
      call return_answer_id_set(r, q1, e)
      results = return_results_init(r, e)
      call payload_content_set(results, rpc_make_cap_ptr(m, 0), e)
      ctab = payload_cap_table_init(results, 1_int64, e)
      cd%p = capnp_list_get_struct(ctab, 0, e)
      call cap_descriptor_sender_promise_set(cd, 1_int64, e)
      call rpc_send_message(sconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(cconn, e)

      ! Resolve 1 -> senderHosted(2), draining Release(1, 1).
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      rv = message_resolve_init(msg, e)
      call resolve_promise_id_set(rv, 1_int64, e)
      cd = resolve_cap_init(rv, e)
      call cap_descriptor_sender_hosted_set(cd, 2_int64, e)
      call rpc_send_message(sconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(cconn, e)
      call rpc_recv_message(sconn%fd, reply, e)
      call capnp_message_free(reply)

      ! q2 reuses wire id 1 as senderHosted(1).
      call rpc_call_begin(cconn, cap, 1_int64, 0, m, params, q2, e)
      call capnp_message_free(m)
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      r = message_return_init(msg, e)
      call return_answer_id_set(r, q2, e)
      results = return_results_init(r, e)
      call payload_content_set(results, rpc_make_cap_ptr(m, 0), e)
      ctab = payload_cap_table_init(results, 1_int64, e)
      cd%p = capnp_list_get_struct(ctab, 0, e)
      call cap_descriptor_sender_hosted_set(cd, 1_int64, e)
      call rpc_send_message(sconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(cconn, e)

      ! Now extract both handles.
      call rpc_result_cap(cconn, q1, [integer ::], c1, e)
      call rpc_result_cap(cconn, q2, [integer ::], c2, e)
      call check_(error, c1%id == 1_int64 .and. c2%id < -1_int64, &
                  'delayed result cap: distinct handles preserved')
      if (allocated(error)) then
         call rpc_conn_close(cconn); call rpc_conn_close(sconn); return
      end if

      call rpc_release_send(cconn, c1, e)
      call rpc_recv_message(sconn%fd, reply, e)
      rmsg = message_read_root(reply, e)
      rel = message_release_get(rmsg, e)
      call check_(error, message_which(rmsg) == MESSAGE_RELEASE_TAG .and. &
                  release_id_get(rel) == 2_int64, &
                  'delayed result cap: c1 releases resolved target 2')
      call capnp_message_free(reply)

      call rpc_release_send(cconn, c2, e)
      call rpc_recv_message(sconn%fd, reply, e)
      rmsg = message_read_root(reply, e)
      rel = message_release_get(rmsg, e)
      call check_(error, message_which(rmsg) == MESSAGE_RELEASE_TAG .and. &
                  release_id_get(rel) == 1_int64, &
                  'delayed result cap: c2 releases reused wire id 1')
      call capnp_message_free(reply)
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_delayed_result_cap_reused_id

   !> A promise delivered twice owns one Resolve reference to its target.
   !> Releasing both promise handles must send one Release for the target,
   !> leaving a direct handle to that same target alive.
   subroutine t_resolve_multiref_release(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: cap, p1, p2, direct
      type(capnp_message_t), target :: m, reply
      type(message_t) :: msg, rmsg
      type(return_t) :: r
      type(resolve_t) :: rv
      type(payload_t) :: params, results
      type(cap_descriptor_t) :: cd
      type(capnp_ptr_t) :: ctab
      type(release_t) :: rel
      integer :: fa, fb, e
      integer(int64) :: qboot, q1, q2, q3
      logical :: readable
      call px_socketpair(fa, fb, e)
      boot => local_echo
      call rpc_conn_init(sconn, fb, boot)
      boot => null()
      call rpc_conn_init(cconn, fa, boot)
      call rpc_bootstrap_send(cconn, cap, e)
      qboot = cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call rpc_result_cap(cconn, qboot, [integer ::], cap, e)

      ! Deliver senderPromise(1) twice (refs=2) and senderHosted(2) directly.
      call rpc_call_begin(cconn, cap, 1_int64, 0, m, params, q1, e)
      call capnp_message_free(m)
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      r = message_return_init(msg, e)
      call return_answer_id_set(r, q1, e)
      results = return_results_init(r, e)
      call payload_content_set(results, rpc_make_cap_ptr(m, 0), e)
      ctab = payload_cap_table_init(results, 1_int64, e)
      cd%p = capnp_list_get_struct(ctab, 0, e)
      call cap_descriptor_sender_promise_set(cd, 1_int64, e)
      call rpc_send_message(sconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(cconn, e)
      call rpc_result_cap(cconn, q1, [integer ::], p1, e)

      call rpc_call_begin(cconn, cap, 1_int64, 0, m, params, q2, e)
      call capnp_message_free(m)
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      r = message_return_init(msg, e)
      call return_answer_id_set(r, q2, e)
      results = return_results_init(r, e)
      call payload_content_set(results, rpc_make_cap_ptr(m, 0), e)
      ctab = payload_cap_table_init(results, 1_int64, e)
      cd%p = capnp_list_get_struct(ctab, 0, e)
      call cap_descriptor_sender_promise_set(cd, 1_int64, e)
      call rpc_send_message(sconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(cconn, e)
      call rpc_result_cap(cconn, q2, [integer ::], p2, e)

      call rpc_call_begin(cconn, cap, 1_int64, 0, m, params, q3, e)
      call capnp_message_free(m)
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      r = message_return_init(msg, e)
      call return_answer_id_set(r, q3, e)
      results = return_results_init(r, e)
      call payload_content_set(results, rpc_make_cap_ptr(m, 0), e)
      ctab = payload_cap_table_init(results, 1_int64, e)
      cd%p = capnp_list_get_struct(ctab, 0, e)
      call cap_descriptor_sender_hosted_set(cd, 2_int64, e)
      call rpc_send_message(sconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(cconn, e)
      call rpc_result_cap(cconn, q3, [integer ::], direct, e)

      ! Resolve 1 -> senderHosted(2); drains Release(1, 2).
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      rv = message_resolve_init(msg, e)
      call resolve_promise_id_set(rv, 1_int64, e)
      cd = resolve_cap_init(rv, e)
      call cap_descriptor_sender_hosted_set(cd, 2_int64, e)
      call rpc_send_message(sconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(cconn, e)
      call rpc_recv_message(sconn%fd, reply, e)
      rmsg = message_read_root(reply, e)
      rel = message_release_get(rmsg, e)
      call check_(error, message_which(rmsg) == MESSAGE_RELEASE_TAG .and. &
                  release_id_get(rel) == 1_int64 .and. &
                  release_reference_count_get(rel) == 2_int64, &
                  'multiref resolve: drops both promise refs')
      call capnp_message_free(reply)
      if (allocated(error)) then
         call rpc_conn_close(cconn); call rpc_conn_close(sconn); return
      end if

      ! First promise handle release must not release target 2 yet.
      call rpc_release_send(cconn, p1, e)
      call px_poll_in(sconn%fd, 50, readable, e)
      call check_(error, e == CAPNP_OK .and. .not. readable, &
                  'multiref resolve: first handle keeps target alive')
      if (allocated(error)) then
         call rpc_conn_close(cconn); call rpc_conn_close(sconn); return
      end if

      ! Second promise handle release drops the one Resolve ref on 2.
      call rpc_release_send(cconn, p2, e)
      call rpc_recv_message(sconn%fd, reply, e)
      rmsg = message_read_root(reply, e)
      rel = message_release_get(rmsg, e)
      call check_(error, message_which(rmsg) == MESSAGE_RELEASE_TAG .and. &
                  release_id_get(rel) == 2_int64 .and. &
                  release_reference_count_get(rel) == 1_int64, &
                  'multiref resolve: last promise handle releases target')
      call capnp_message_free(reply)

      ! A redundant release of p2 must not touch direct's ref on 2.
      call rpc_release_send(cconn, p2, e)
      call px_poll_in(sconn%fd, 50, readable, e)
      call check_(error, e == CAPNP_OK .and. .not. readable, &
                  'multiref resolve: redundant release is ignored')
      if (allocated(error)) then
         call rpc_conn_close(cconn); call rpc_conn_close(sconn); return
      end if

      ! Releasing direct drops the final reference on 2.
      call rpc_release_send(cconn, direct, e)
      call rpc_recv_message(sconn%fd, reply, e)
      rmsg = message_read_root(reply, e)
      rel = message_release_get(rmsg, e)
      call check_(error, message_which(rmsg) == MESSAGE_RELEASE_TAG .and. &
                  release_id_get(rel) == 2_int64 .and. &
                  release_reference_count_get(rel) == 1_int64, &
                  'multiref resolve: direct handle still owns its ref')
      call capnp_message_free(reply)
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_resolve_multiref_release

   !> A senderLoopback Disembargo walking a non-struct field in a
   !> promisedAnswer must abort the connection rather than return an
   !> unhandled read error.
   subroutine t_disembargo_promised_bad_walk(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(reflect_srv_t), target :: rsrv
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: cap
      type(capnp_message_t), target :: m, reply
      type(message_t) :: msg, rmsg
      type(payload_t) :: params
      type(disembargo_t) :: dis
      type(message_target_t) :: tgt
      type(promised_answer_t) :: pa
      type(promised_answer_op_t) :: op
      type(capnp_ptr_t) :: ops
      integer :: fa, fb, e
      integer(int64) :: qboot, qcall
      logical :: readable
      call px_socketpair(fa, fb, e)
      boot => rsrv
      call rpc_conn_init(sconn, fb, boot)
      boot => null()
      call rpc_conn_init(cconn, fa, boot)
      call rpc_bootstrap_send(cconn, cap, e)
      qboot = cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call rpc_result_cap(cconn, qboot, [integer ::], cap, e)
      call rpc_call_begin(cconn, cap, 1_int64, 0, m, params, qcall, e)
      call rpc_call_send(cconn, m, e)
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qcall, e)

      ! reflect_dispatch returns a bare cap pointer (non-struct content).
      ! Walking getPointerField(0) fails capnp_getp and must send Abort.
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      dis = message_disembargo_init(msg, e)
      call disembargo_context_sender_loopback_set(dis, 77_int64, e)
      tgt = disembargo_target_init(dis, e)
      pa = message_target_promised_answer_init(tgt, e)
      call promised_answer_question_id_set(pa, qcall, e)
      ops = promised_answer_transform_init(pa, 1_int64, e)
      op%p = capnp_list_get_struct(ops, 0, e)
      call promised_answer_op_get_pointer_field_set(op, 0, e)
      call rpc_send_message(cconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(sconn, e)
      call check_(error, .not. rpc_conn_alive(sconn), &
                  'bad loopback walk: server aborted connection')
      if (allocated(error)) then
         call rpc_conn_close(cconn); call rpc_conn_close(sconn); return
      end if
      call px_poll_in(cconn%fd, 1000, readable, e)
      call check_(error, e == CAPNP_OK .and. readable, &
                  'bad loopback walk: Abort arrives at peer')
      if (allocated(error)) then
         call rpc_conn_close(cconn); call rpc_conn_close(sconn); return
      end if
      call rpc_recv_message(cconn%fd, reply, e)
      rmsg = message_read_root(reply, e)
      call check_(error, e == CAPNP_OK .and. &
                  message_which(rmsg) == MESSAGE_ABORT_TAG, &
                  'bad loopback walk: peer receives Abort')
      call capnp_message_free(reply)
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_disembargo_promised_bad_walk

   !> Resolve releases the promise export. Finish after that must set
   !> releaseResultCaps false and Release the resolved capability once.
   !> releaseResultCaps true makes a C++ 1.4.0 peer abort with
   !> "Tried to release invalid export ID."
   subroutine t_finish_after_resolve(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: cap, promised
      type(capnp_message_t), target :: m, reply
      type(message_t) :: msg, rmsg
      type(return_t) :: r
      type(resolve_t) :: rv
      type(payload_t) :: params, results
      type(cap_descriptor_t) :: cd
      type(capnp_ptr_t) :: ctab
      type(finish_t) :: fin
      type(release_t) :: rel
      integer :: fa, fb, e, nmsg
      integer(int64) :: qboot, q1
      logical :: readable, saw_finish, saw_target, saw_promise_again
      logical :: flag
      call px_socketpair(fa, fb, e)
      boot => local_echo
      call rpc_conn_init(sconn, fb, boot)
      boot => null()
      call rpc_conn_init(cconn, fa, boot)
      call rpc_bootstrap_send(cconn, cap, e)
      qboot = cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call rpc_result_cap(cconn, qboot, [integer ::], cap, e)

      call rpc_call_begin(cconn, cap, 1_int64, 0, m, params, q1, e)
      call capnp_message_free(m)
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      r = message_return_init(msg, e)
      call return_answer_id_set(r, q1, e)
      results = return_results_init(r, e)
      call payload_content_set(results, rpc_make_cap_ptr(m, 0), e)
      ctab = payload_cap_table_init(results, 1_int64, e)
      cd%p = capnp_list_get_struct(ctab, 0, e)
      call cap_descriptor_sender_promise_set(cd, 4_int64, e)
      call rpc_send_message(sconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(cconn, e)
      call rpc_result_cap(cconn, q1, [integer ::], promised, e)
      call check_(error, e == CAPNP_OK .and. promised%kind == RPC_CAP_IMPORT, &
                  'finish-after-resolve: promise cap')
      if (allocated(error)) then
         call rpc_conn_close(cconn); call rpc_conn_close(sconn); return
      end if

      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      rv = message_resolve_init(msg, e)
      call resolve_promise_id_set(rv, 4_int64, e)
      cd = resolve_cap_init(rv, e)
      call cap_descriptor_sender_hosted_set(cd, 5_int64, e)
      call rpc_send_message(sconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(cconn, e)
      call rpc_recv_message(sconn%fd, reply, e)
      rmsg = message_read_root(reply, e)
      rel = message_release_get(rmsg, e)
      call check_(error, e == CAPNP_OK .and. &
                  message_which(rmsg) == MESSAGE_RELEASE_TAG .and. &
                  release_id_get(rel) == 4_int64 .and. &
                  release_reference_count_get(rel) == 1_int64, &
                  'finish-after-resolve: Resolve releases the promise')
      call capnp_message_free(reply)
      if (allocated(error)) then
         call rpc_conn_close(cconn); call rpc_conn_close(sconn); return
      end if

      call rpc_finish_send(cconn, q1, .false., e)
      call check_(error, e == CAPNP_OK .and. rpc_conn_alive(cconn), &
                  'finish-after-resolve: client stays up')
      saw_finish = .false.
      saw_target = .false.
      saw_promise_again = .false.
      flag = .true.
      do nmsg = 1, 4
         call px_poll_in(sconn%fd, 200, readable, e)
         if (e /= CAPNP_OK .or. .not. readable) exit
         call rpc_recv_message(sconn%fd, reply, e)
         if (e /= CAPNP_OK) exit
         rmsg = message_read_root(reply, e)
         if (message_which(rmsg) == MESSAGE_FINISH_TAG) then
            fin = message_finish_get(rmsg, e)
            saw_finish = e == CAPNP_OK .and. finish_question_id_get(fin) == q1
            if (saw_finish) flag = finish_release_result_caps_get(fin)
         else if (message_which(rmsg) == MESSAGE_RELEASE_TAG) then
            rel = message_release_get(rmsg, e)
            if (e == CAPNP_OK .and. release_id_get(rel) == 5_int64 .and. &
                release_reference_count_get(rel) == 1_int64) saw_target = .true.
            if (e == CAPNP_OK .and. release_id_get(rel) == 4_int64) &
               saw_promise_again = .true.
         end if
         call capnp_message_free(reply)
      end do
      call check_(error, saw_finish .and. .not. flag, &
                  'finish-after-resolve: releaseResultCaps is false')
      call check_(error, saw_target, &
                  'finish-after-resolve: Releases the resolved capability')
      call check_(error, .not. saw_promise_again, &
                  'finish-after-resolve: promise export is not released again')
      call check_(error, rpc_conn_alive(cconn), &
                  'finish-after-resolve: connection stays up')
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_finish_after_resolve

   !> A client on `cconn` holding the scripted peer's bootstrap as `cap`.
   subroutine scripted_pair(cconn, sconn, srv, cap)
      type(rpc_conn_t), intent(inout), target :: cconn, sconn
      class(rpc_server_t), pointer, intent(in) :: srv
      type(rpc_cap_t), intent(out) :: cap
      class(rpc_server_t), pointer :: none
      integer :: fa, fb, e
      integer(int64) :: qboot
      call px_socketpair(fa, fb, e)
      call rpc_conn_init(sconn, fb, srv)
      none => null()
      call rpc_conn_init(cconn, fa, none)
      call rpc_bootstrap_send(cconn, cap, e)
      qboot = cap%id
      call rpc_pump_once(sconn, e)
      call rpc_wait(cconn, qboot, e)
      call rpc_result_cap(cconn, qboot, [integer ::], cap, e)
   end subroutine scripted_pair

   !> Return `ncaps` results for `qid`, pointer field i holding cap i.
   !> Cap i is senderHosted `id0 + i`, except index `vine_at`, which is
   !> thirdPartyHosted with that id as its vine and `nonce`, and index
   !> `promise_at`, which is senderPromise.
   subroutine scripted_caps_return(cconn, sconn, qid, ncaps, vine_at, id0, nonce, promise_at)
      type(rpc_conn_t), intent(inout), target :: cconn, sconn
      integer(int64), intent(in) :: qid, id0, nonce
      integer, intent(in) :: ncaps, vine_at
      integer, intent(in), optional :: promise_at
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(return_t) :: r
      type(payload_t) :: results
      type(capnp_ptr_t) :: s, ctab
      type(cap_descriptor_t) :: cd
      integer :: i, e
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      r = message_return_init(msg, e)
      call return_answer_id_set(r, qid, e)
      results = return_results_init(r, e)
      s = capnp_new_struct(m, 0, ncaps, e)
      do i = 0, ncaps - 1
         call capnp_setp(s, i, rpc_make_cap_ptr(m, i), e)
      end do
      call payload_content_set(results, s, e)
      ctab = payload_cap_table_init(results, int(ncaps, int64), e)
      do i = 0, ncaps - 1
         cd%p = capnp_list_get_struct(ctab, i, e)
         if (i == vine_at) then
            call rpc_write_third_party_cap(cd, '10.1.0.4', 5004, nonce, id0 + i, e)
         else if (present(promise_at)) then
            if (i == promise_at) then
               call cap_descriptor_sender_promise_set(cd, id0 + i, e)
            else
               call cap_descriptor_sender_hosted_set(cd, id0 + i, e)
            end if
         else
            call cap_descriptor_sender_hosted_set(cd, id0 + i, e)
         end if
      end do
      call rpc_send_message(sconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(cconn, e)
   end subroutine scripted_caps_return

   !> Next frame the client sent, if any arrives within `ms`. For Finish
   !> `a` is the question and `b` is 1 when releaseResultCaps is set; for
   !> Release they are the id and the count.
   subroutine scripted_frame(sconn, ms, got, tag, a, b)
      type(rpc_conn_t), intent(inout), target :: sconn
      integer, intent(in) :: ms
      logical, intent(out) :: got
      integer, intent(out) :: tag
      integer(int64), intent(out) :: a, b
      type(capnp_message_t), target :: reply
      type(message_t) :: rmsg
      type(finish_t) :: fin
      type(release_t) :: rel
      integer :: e
      got = .false.
      tag = -1
      a = -1_int64
      b = -1_int64
      call px_poll_in(sconn%fd, ms, got, e)
      if (e /= CAPNP_OK .or. .not. got) then
         got = .false.
         return
      end if
      call rpc_recv_message(sconn%fd, reply, e)
      if (e /= CAPNP_OK) then
         got = .false.
         return
      end if
      rmsg = message_read_root(reply, e)
      tag = message_which(rmsg)
      if (tag == MESSAGE_FINISH_TAG) then
         fin = message_finish_get(rmsg, e)
         a = finish_question_id_get(fin)
         b = merge(1_int64, 0_int64, finish_release_result_caps_get(fin))
      else if (tag == MESSAGE_RELEASE_TAG) then
         rel = message_release_get(rmsg, e)
         a = release_id_get(rel)
         b = release_reference_count_get(rel)
      end if
      call capnp_message_free(reply)
   end subroutine scripted_frame

   !> Two concurrent Returns name the same export: q1 as a thirdPartyHosted
   !> vine, q2 as senderHosted. Finish(q1) releasing its caps already
   !> returns q1's reference, so the pickup must not release q2's.
   subroutine t_vine_finish_shared_id(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: srv
      type(rpc_cap_t) :: cap, cap2
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      integer(int64) :: q1, q2, a, b
      integer :: e, tag
      logical :: got
      srv => local_echo
      call scripted_pair(cconn, sconn, srv, cap)
      call rpc_call_begin(cconn, cap, 1_int64, 0, m, params, q1, e)
      call capnp_message_free(m)
      call rpc_call_begin(cconn, cap, 1_int64, 0, m, params, q2, e)
      call capnp_message_free(m)
      call scripted_caps_return(cconn, sconn, q1, 1, 0, 81_int64, 911_int64)
      call scripted_caps_return(cconn, sconn, q2, 1, -1, 81_int64, 0_int64)
      call rpc_result_cap(cconn, q2, [0], cap2, e)
      call check_(error, e == CAPNP_OK .and. cap2%kind == RPC_CAP_IMPORT, &
                  'shared vine: second result cap')
      call rpc_finish_send(cconn, q1, .false., e)
      call scripted_frame(sconn, 1000, got, tag, a, b)
      call check_(error, got .and. tag == MESSAGE_FINISH_TAG .and. a == q1 .and. &
                  b == 1_int64, 'shared vine: Finish releases q1 results')
      call rpc_introduction_done(cconn, 911_int64, e)
      call check_(error, e == CAPNP_OK, 'shared vine: pickup after Finish succeeds')
      call scripted_frame(sconn, 50, got, tag, a, b)
      call check_(error, .not. got, 'shared vine: pickup after Finish sends no Release')
      call rpc_finish_send(cconn, q2, .true., e)
      call scripted_frame(sconn, 1000, got, tag, a, b)
      call rpc_release_send(cconn, cap2, e)
      call scripted_frame(sconn, 1000, got, tag, a, b)
      call check_(error, got .and. tag == MESSAGE_RELEASE_TAG .and. a == 81_int64 .and. &
                  b == 1_int64, 'shared vine: q2 releases its reference once')
      call check_(error, rpc_conn_alive(cconn), 'shared vine: connection stays up')
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_vine_finish_shared_id

   !> The pickup comes first and releases q1's reference. A releasing
   !> Finish or cancellation of q1 must then leave that cap out.
   subroutine t_vine_done_shared_id_finish(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: srv
      type(rpc_cap_t) :: cap
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      integer(int64) :: q1, q2, q3, q4, a, b
      integer :: e, tag
      logical :: got
      srv => local_echo
      call scripted_pair(cconn, sconn, srv, cap)
      call rpc_call_begin(cconn, cap, 1_int64, 0, m, params, q1, e)
      call capnp_message_free(m)
      call rpc_call_begin(cconn, cap, 1_int64, 0, m, params, q2, e)
      call capnp_message_free(m)
      call scripted_caps_return(cconn, sconn, q1, 1, 0, 81_int64, 911_int64)
      call scripted_caps_return(cconn, sconn, q2, 1, -1, 81_int64, 0_int64)
      call rpc_introduction_done(cconn, 911_int64, e)
      call scripted_frame(sconn, 1000, got, tag, a, b)
      call check_(error, e == CAPNP_OK .and. got .and. tag == MESSAGE_RELEASE_TAG .and. &
                  a == 81_int64 .and. b == 1_int64, 'done first: pickup releases the vine')
      call rpc_finish_send(cconn, q1, .false., e)
      call scripted_frame(sconn, 1000, got, tag, a, b)
      call check_(error, got .and. tag == MESSAGE_FINISH_TAG .and. a == q1 .and. &
                  b == 0_int64, 'done first: Finish keeps q2 reference')
      call scripted_frame(sconn, 50, got, tag, a, b)
      call check_(error, .not. got, 'done first: Finish sends no second Release')
      call rpc_finish_send(cconn, q2, .false., e)
      call scripted_frame(sconn, 1000, got, tag, a, b)
      call check_(error, got .and. tag == MESSAGE_FINISH_TAG .and. a == q2 .and. &
                  b == 1_int64, 'done first: q2 Finish releases its result')

      call rpc_call_begin(cconn, cap, 1_int64, 0, m, params, q3, e)
      call capnp_message_free(m)
      call rpc_call_begin(cconn, cap, 1_int64, 0, m, params, q4, e)
      call capnp_message_free(m)
      call scripted_caps_return(cconn, sconn, q3, 1, 0, 82_int64, 912_int64)
      call scripted_caps_return(cconn, sconn, q4, 1, -1, 82_int64, 0_int64)
      call rpc_introduction_done(cconn, 912_int64, e)
      call scripted_frame(sconn, 1000, got, tag, a, b)
      call rpc_request_cancellation(cconn, q3, e)
      call scripted_frame(sconn, 1000, got, tag, a, b)
      call check_(error, e == CAPNP_OK .and. got .and. tag == MESSAGE_FINISH_TAG .and. &
                  a == q3 .and. b == 0_int64, 'done first: cancel keeps q4 reference')
      call check_(error, rpc_conn_alive(cconn), 'done first: connection stays up')
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_vine_done_shared_id_finish

   !> A vine past the sixteenth result cap is tracked like any other: the
   !> pickup releases it through its handle, and the releasing Finish
   !> that follows releases only the other sixteen.
   subroutine t_vine_past_sixteen_caps(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: srv
      type(rpc_cap_t) :: cap, vine
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      integer(int64) :: q, a, b
      integer :: e, tag, n, nrel
      logical :: got, saw_finish, saw_vine
      srv => local_echo
      call scripted_pair(cconn, sconn, srv, cap)
      call rpc_call_begin(cconn, cap, 1_int64, 0, m, params, q, e)
      call capnp_message_free(m)
      call scripted_caps_return(cconn, sconn, q, 17, 16, 1_int64, 913_int64)
      call rpc_result_cap(cconn, q, [16], vine, e)
      call check_(error, e == CAPNP_OK .and. vine%kind == RPC_CAP_IMPORT .and. &
                  vine%id == 17_int64, 'cap 16: vine import')
      call rpc_introduction_done(cconn, 913_int64, e)
      call scripted_frame(sconn, 1000, got, tag, a, b)
      call check_(error, e == CAPNP_OK .and. got .and. tag == MESSAGE_RELEASE_TAG .and. &
                  a == 17_int64 .and. b == 1_int64, 'cap 16: pickup releases the vine')
      call rpc_finish_send(cconn, q, .false., e)
      saw_finish = .false.
      saw_vine = .false.
      nrel = 0
      do n = 1, 20
         call scripted_frame(sconn, 200, got, tag, a, b)
         if (.not. got) exit
         if (tag == MESSAGE_FINISH_TAG) saw_finish = a == q .and. b == 0_int64
         if (tag == MESSAGE_RELEASE_TAG) then
            nrel = nrel + 1
            if (a == 17_int64) saw_vine = .true.
         end if
      end do
      call check_(error, saw_finish, 'cap 16: Finish leaves the released vine out')
      call check_(error, nrel == 16 .and. .not. saw_vine, &
                  'cap 16: Finish releases the other sixteen once')
      call check_(error, rpc_conn_alive(cconn), 'cap 16: connection stays up')
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_vine_past_sixteen_caps

   !> Promise `pid` from the scripted peer, with one call already sent
   !> through it, so a reflected resolution would need an embargo.
   subroutine scripted_called_promise(cconn, sconn, cap, pid, promised)
      type(rpc_conn_t), intent(inout), target :: cconn, sconn
      type(rpc_cap_t), intent(in) :: cap
      integer(int64), intent(in) :: pid
      type(rpc_cap_t), intent(out) :: promised
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      integer(int64) :: q, qc, a, b
      integer :: e, tag
      logical :: got
      call rpc_call_begin(cconn, cap, 1_int64, 0, m, params, q, e)
      call capnp_message_free(m)
      call scripted_caps_return(cconn, sconn, q, 1, -1, pid, 0_int64, promise_at=0)
      call rpc_result_cap(cconn, q, [0], promised, e)
      call rpc_finish_send(cconn, q, .true., e)
      call scripted_frame(sconn, 1000, got, tag, a, b)
      call rpc_call_begin(cconn, promised, 1_int64, 0, m, params, qc, e)
      call payload_content_set(params, capnp_new_struct(m, 0, 0, e), e)
      call rpc_call_send(cconn, m, e)
      call scripted_frame(sconn, 1000, got, tag, a, b)
   end subroutine scripted_called_promise

   subroutine scripted_resolve_receiver_hosted(cconn, sconn, pid, eid)
      type(rpc_conn_t), intent(inout), target :: cconn, sconn
      integer(int64), intent(in) :: pid, eid
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(resolve_t) :: rv
      type(cap_descriptor_t) :: cd
      integer :: e
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      rv = message_resolve_init(msg, e)
      call resolve_promise_id_set(rv, pid, e)
      cd = resolve_cap_init(rv, e)
      call cap_descriptor_receiver_hosted_set(cd, eid, e)
      call rpc_send_message(sconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(cconn, e)
   end subroutine scripted_resolve_receiver_hosted

   !> A called promise that resolves to an export this vat does not have
   !> on the wire, missing or pin-only, is broken: C++ releases the promise
   !> and sends no Disembargo (rpc.c++ 1171-1181).
   subroutine t_resolve_invalid_receiver_hosted(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo, pinned_echo
      class(rpc_server_t), pointer :: srv
      type(rpc_cap_t) :: cap, promised
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      integer(int64) :: qc, a, b
      integer :: e, tag
      logical :: got
      character(len=*), parameter :: why = "invalid 'receiverHosted' export ID"
      srv => local_echo
      call scripted_pair(cconn, sconn, srv, cap)

      call scripted_called_promise(cconn, sconn, cap, 4_int64, promised)
      call scripted_resolve_receiver_hosted(cconn, sconn, 4_int64, 999_int64)
      call scripted_frame(sconn, 1000, got, tag, a, b)
      call check_(error, got .and. tag == MESSAGE_RELEASE_TAG .and. a == 4_int64 .and. &
                  b == 1_int64, 'invalid receiverHosted: promise released')
      call scripted_frame(sconn, 50, got, tag, a, b)
      call check_(error, .not. got, 'invalid receiverHosted: no Disembargo')
      call rpc_call_begin(cconn, promised, 1_int64, 0, m, params, qc, e)
      call check_(error, e == RPC_ERR_EXCEPTION .and. rpc_conn_reason(cconn) == why, &
                  'invalid receiverHosted: calls fail as broken')

      srv => pinned_echo
      cconn%exports(5)%used = .true.
      cconn%exports(5)%refcount = 0
      cconn%exports(5)%pins = 1
      cconn%exports(5)%srv => srv
      call scripted_called_promise(cconn, sconn, cap, 6_int64, promised)
      call scripted_resolve_receiver_hosted(cconn, sconn, 6_int64, 5_int64)
      call scripted_frame(sconn, 1000, got, tag, a, b)
      call check_(error, got .and. tag == MESSAGE_RELEASE_TAG .and. a == 6_int64 .and. &
                  b == 1_int64, 'pin-only receiverHosted: promise released')
      call scripted_frame(sconn, 50, got, tag, a, b)
      call check_(error, .not. got, 'pin-only receiverHosted: no Disembargo')
      call check_(error, cconn%exports(5)%pins == 1, &
                  'pin-only receiverHosted: export gains no pin')
      call check_(error, rpc_conn_alive(cconn), 'receiverHosted: connection stays up')
      cconn%exports(5)%used = .false.
      cconn%exports(5)%pins = 0
      cconn%exports(5)%srv => null()
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_resolve_invalid_receiver_hosted


   !> `cconn` hosts the echo bootstrap and holds two active answers for
   !> the peer `sconn`: `qboot`, whose content is the bootstrap cap with
   !> export id `eid`, and `qadder`, a struct holding an adder in ptr 0.
   subroutine ra_pair(cconn, sconn, srv, qboot, qadder, eid)
      type(rpc_conn_t), intent(inout), target :: cconn, sconn
      class(rpc_server_t), pointer, intent(in) :: srv
      integer(int64), intent(out) :: qboot, qadder, eid
      class(rpc_server_t), pointer :: none
      type(rpc_cap_t) :: boot
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      integer :: fa, fb, e
      call px_socketpair(fa, fb, e)
      call rpc_conn_init(cconn, fa, srv)
      none => null()
      call rpc_conn_init(sconn, fb, none)
      call rpc_bootstrap_send(sconn, boot, e)
      qboot = boot%id
      call rpc_pump_once(cconn, e)
      call rpc_wait(sconn, qboot, e)
      call rpc_result_cap(sconn, qboot, [integer ::], boot, e)
      eid = boot%id
      call rpc_call_begin(sconn, boot, ECHO_IFACE, 1, m, params, qadder, e)
      call payload_content_set(params, capnp_new_struct(m, 1, 0, e), e)
      call rpc_call_send(sconn, m, e)
      call rpc_pump_once(cconn, e)
      call rpc_wait(sconn, qadder, e)
   end subroutine ra_pair

   !> Promise `pid` on `cconn` with one call already sent through it.
   subroutine ra_called_promise(cconn, sconn, pid, promised)
      type(rpc_conn_t), intent(inout), target :: cconn, sconn
      integer(int64), intent(in) :: pid
      type(rpc_cap_t), intent(out) :: promised
      type(rpc_cap_t) :: base
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      integer(int64) :: q, qc
      integer :: e
      base = rpc_cap_t()
      base%kind = RPC_CAP_IMPORT
      base%id = 0_int64
      call inject_promise(sconn%fd, cconn, base, pid, q, e)
      call rpc_wait(cconn, q, e)
      call rpc_result_cap(cconn, q, [integer ::], promised, e)
      call rpc_call_begin(cconn, promised, ECHO_IFACE, 0, m, params, qc, e)
      call payload_content_set(params, capnp_new_struct(m, 1, 1, e), e)
      call rpc_call_send(cconn, m, e)
   end subroutine ra_called_promise

   !> Resolve `pid` to receiverAnswer `qid` walking pointer `fields`.
   subroutine ra_resolve(cconn, sconn, pid, qid, fields)
      type(rpc_conn_t), intent(inout), target :: cconn, sconn
      integer(int64), intent(in) :: pid, qid
      integer, intent(in) :: fields(:)
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(resolve_t) :: rv
      type(cap_descriptor_t) :: cd
      type(promised_answer_t) :: pa
      type(promised_answer_op_t) :: op
      type(capnp_ptr_t) :: ops
      integer :: i, e
      call capnp_message_init_builder(m, e)
      msg = message_new_root(m, e)
      rv = message_resolve_init(msg, e)
      call resolve_promise_id_set(rv, pid, e)
      cd = resolve_cap_init(rv, e)
      pa = cap_descriptor_receiver_answer_init(cd, e)
      call promised_answer_question_id_set(pa, qid, e)
      ops = promised_answer_transform_init(pa, int(size(fields), int64), e)
      do i = 1, size(fields)
         op%p = capnp_list_get_struct(ops, i - 1, e)
         call promised_answer_op_get_pointer_field_set(op, int(fields(i), int32), e)
      end do
      call rpc_send_message(sconn%fd, m, e)
      call capnp_message_free(m)
      call rpc_pump_once(cconn, e)
   end subroutine ra_resolve

   !> Drain what `cconn` sent to the raw peer socket: whether it released
   !> promise `pid` and whether it sent a Disembargo.
   subroutine ra_drain(sconn, pid, released, disembargo)
      type(rpc_conn_t), intent(inout), target :: sconn
      integer(int64), intent(in) :: pid
      logical, intent(out) :: released, disembargo
      integer(int64) :: a, b
      integer :: n, tag
      logical :: got
      released = .false.
      disembargo = .false.
      do n = 1, 20
         call scripted_frame(sconn, 100, got, tag, a, b)
         if (.not. got) exit
         if (tag == MESSAGE_RELEASE_TAG .and. a == pid) released = .true.
         if (tag == MESSAGE_DISEMBARGO_TAG) disembargo = .true.
      end do
   end subroutine ra_drain

   !> receiverAnswer as C++ 1.4.0 reads it (rpc.c++ 1604-1624): a walk
   !> through a cap aborts with a schema mismatch, a walk to a null
   !> pointer gives a broken promise, and an export the active answer
   !> still pins resolves as reflected even after its last Release.
   subroutine t_resolve_receiver_answer_walk(error)
      type(error_type), allocatable, intent(inout) :: error
      type(rpc_conn_t), target :: cconn, sconn
      type(echo_srv_t), target :: local_echo
      class(rpc_server_t), pointer :: srv
      type(rpc_cap_t) :: promised, peer_boot
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      integer(int64) :: qboot, qadder, eid, qc
      integer :: e
      logical :: released, disembargo
      character(len=*), parameter :: not_struct = "Schema mismatch: Message contains " // &
                                     "non-struct pointer where struct pointer was expected."
      srv => local_echo

      call ra_pair(cconn, sconn, srv, qboot, qadder, eid)
      call ra_called_promise(cconn, sconn, 4_int64, promised)
      call ra_resolve(cconn, sconn, 4_int64, qboot, [0])
      call check_(error, .not. rpc_conn_alive(cconn) .and. &
                  rpc_conn_reason(cconn) == not_struct, &
                  'receiverAnswer: walk through a cap aborts')
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
      if (allocated(error)) return

      call ra_pair(cconn, sconn, srv, qboot, qadder, eid)
      call ra_called_promise(cconn, sconn, 5_int64, promised)
      call ra_resolve(cconn, sconn, 5_int64, qadder, [5, 0])
      call ra_drain(sconn, 5_int64, released, disembargo)
      call check_(error, rpc_conn_alive(cconn) .and. released .and. .not. disembargo, &
                  'receiverAnswer: null walk releases the promise')
      call rpc_call_begin(cconn, promised, ECHO_IFACE, 0, m, params, qc, e)
      call check_(error, e == RPC_ERR_EXCEPTION .and. &
                  rpc_conn_reason(cconn) == 'Called null capability.', &
                  'receiverAnswer: null walk breaks the promise')
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
      if (allocated(error)) return

      call ra_pair(cconn, sconn, srv, qboot, qadder, eid)
      peer_boot = rpc_cap_t()
      peer_boot%kind = RPC_CAP_IMPORT
      peer_boot%id = eid
      call rpc_release_send(sconn, peer_boot, e)
      call rpc_pump_once(cconn, e)
      call check_(error, cconn%exports(int(eid))%used .and. &
                  cconn%exports(int(eid))%refcount == 0, &
                  'receiverAnswer: answer pins the released export')
      call ra_called_promise(cconn, sconn, 6_int64, promised)
      call ra_resolve(cconn, sconn, 6_int64, qboot, [integer ::])
      call ra_drain(sconn, 6_int64, released, disembargo)
      call check_(error, rpc_conn_alive(cconn) .and. disembargo, &
                  'receiverAnswer: pinned export resolves as reflected')
      call rpc_call_begin(cconn, promised, ECHO_IFACE, 0, m, params, qc, e)
      call check_(error, e == CAPNP_OK, 'receiverAnswer: pinned export takes calls')
      if (e == CAPNP_OK) call capnp_message_free(m)
      call rpc_conn_close(cconn)
      call rpc_conn_close(sconn)
   end subroutine t_resolve_receiver_answer_walk

end module test_rpc
