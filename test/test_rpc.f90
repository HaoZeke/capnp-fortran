!> Two-party RPC over a socketpair, both vats in one process, pumped in
!> lockstep: bootstrap, pipelined and settled calls, capability results,
!> exceptions, finish/release bookkeeping, and the unimplemented reply.
program test_rpc
   use capnp
   use rpc_capnp
   use capnp_posix
   use capnp_rpc
   use capnp_rpc_transport
   use rpc_servers
   implicit none

   integer :: nfail = 0
   type(rpc_conn_t), target :: cli, srv
   type(echo_srv_t), target :: echo
   class(rpc_server_t), pointer :: boot
   integer :: fda, fdb, err

   call px_socketpair(fda, fdb, err)
   call check_(err == CAPNP_OK, 'rpc: socketpair')
   boot => echo
   call rpc_conn_init(srv, fdb, boot)
   boot => null()
   call rpc_conn_init(cli, fda, boot)

   call t_bootstrap_and_echo()
   call t_cap_result_and_pipeline()
   call t_exception()
   call t_unimplemented()
   call t_persistent_save()
   call t_resolve_and_tail_calls()
   call t_sender_promise_import()
   call t_disembargo_echo()
   call t_pump_poll()

   call rpc_conn_close(cli)
   call rpc_conn_close(srv)

   if (nfail > 0) then
      print '(a,i0,a)', 'FAILED: ', nfail, ' assertion(s)'
      error stop 1
   end if
   print '(a)', 'All rpc tests passed.'

contains

   subroutine check_(cond, name)
      logical, intent(in) :: cond
      character(len=*), intent(in) :: name
      if (.not. cond) then
         nfail = nfail + 1
         print '(a,a)', 'FAIL: ', name
      end if
   end subroutine check_

   !> Bootstrap, then a call on the still-promised bootstrap cap
   !> (pipelining), then settle the cap and call again.
   subroutine t_bootstrap_and_echo()
      type(rpc_cap_t) :: bootcap, settled
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(capnp_ptr_t) :: s, content
      integer(int64) :: q0, q1, q2
      character(len=:), allocatable :: txt

      call rpc_bootstrap_send(cli, bootcap, err)
      call check_(err == CAPNP_OK .and. bootcap%kind == RPC_CAP_PIPELINE, 'rpc: bootstrap sent')
      q0 = bootcap%id
      call rpc_pump_once(srv, err)
      call check_(err == CAPNP_OK, 'rpc: server answered bootstrap')

      ! Pipelined call: target the bootstrap promise before waiting.
      call rpc_call_begin(cli, bootcap, ECHO_IFACE, 0, m, params, q1, err)
      call check_(err == CAPNP_OK, 'rpc: call begin (pipelined)')
      s = capnp_new_struct(m, 1, 1, err)
      call capnp_set_i64(s, 0_int64, 21_int64, err)
      call capnp_set_text(s, 0, 'hi', err)
      call payload_content_set(params, s, err)
      call rpc_call_send(cli, m, err)
      call check_(err == CAPNP_OK, 'rpc: call sent')
      call rpc_pump_once(srv, err)
      call check_(err == CAPNP_OK, 'rpc: server dispatched pipelined call')

      call rpc_wait(cli, q0, err)
      call check_(err == CAPNP_OK, 'rpc: bootstrap returned')
      call rpc_result_cap(cli, q0, [integer ::], settled, err)
      call check_(err == CAPNP_OK .and. settled%kind == RPC_CAP_IMPORT, 'rpc: bootstrap cap settles')

      call rpc_wait(cli, q1, err)
      call check_(err == CAPNP_OK, 'rpc: echo returned')
      call rpc_result_content(cli, q1, content, err)
      call check_(err == CAPNP_OK, 'rpc: echo content')
      call check_(capnp_get_i64(content, 0_int64) == 42_int64, 'rpc: echo doubles')
      call capnp_get_text(content, 0, txt, err)
      call check_(txt == 'echo: hi', 'rpc: echo text')

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
      call check_(err == CAPNP_OK .and. capnp_get_i64(content, 0_int64) == 10_int64, &
                  'rpc: settled call')

      call rpc_finish_send(cli, q1, .false., err)
      call rpc_pump_once(srv, err)
      call rpc_finish_send(cli, q2, .false., err)
      call rpc_pump_once(srv, err)
      call check_(err == CAPNP_OK, 'rpc: finishes processed')
   end subroutine t_bootstrap_and_echo

   !> Method 1 mints an adder capability in its results; pipeline into it
   !> before the return settles, then settle and call the import.
   subroutine t_cap_result_and_pipeline()
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
      call check_(err == CAPNP_OK, 'rpc: make-adder dispatched')

      ! Pipeline: add(5) on the promised adder at results ptr field 0.
      adder_p = rpc_pipeline_cap(qm, [0])
      call rpc_call_begin(cli, adder_p, ECHO_IFACE, 0, m, params, qp, err)
      s = capnp_new_struct(m, 1, 0, err)
      call capnp_set_i64(s, 0_int64, 5_int64, err)
      call payload_content_set(params, s, err)
      call rpc_call_send(cli, m, err)
      call rpc_pump_once(srv, err)
      call check_(err == CAPNP_OK, 'rpc: pipelined add dispatched')

      call rpc_wait(cli, qb, err)
      call rpc_wait(cli, qm, err)
      call rpc_wait(cli, qp, err)
      call rpc_result_content(cli, qp, content, err)
      call check_(err == CAPNP_OK .and. capnp_get_i64(content, 0_int64) == 105_int64, &
                  'rpc: pipelined add result')

      ! Settle the adder and call it directly.
      call rpc_result_cap(cli, qm, [0], adder, err)
      call check_(err == CAPNP_OK .and. adder%kind == RPC_CAP_IMPORT, 'rpc: adder settles')
      call rpc_call_begin(cli, adder, ECHO_IFACE, 0, m, params, qs, err)
      s = capnp_new_struct(m, 1, 0, err)
      call capnp_set_i64(s, 0_int64, 7_int64, err)
      call payload_content_set(params, s, err)
      call rpc_call_send(cli, m, err)
      call rpc_pump_once(srv, err)
      call rpc_wait(cli, qs, err)
      call rpc_result_content(cli, qs, content, err)
      call check_(err == CAPNP_OK .and. capnp_get_i64(content, 0_int64) == 107_int64, &
                  'rpc: settled adder call')

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
      call check_(err == CAPNP_OK .and. capnp_get_i64(content, 0_int64) == 101_int64, &
                  'rpc: retained cap survives finish')

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
      call check_(err == RPC_ERR_EXCEPTION, 'rpc: released cap raises')
   end subroutine t_cap_result_and_pipeline

   !> Unknown method ids surface as exception returns.
   subroutine t_exception()
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
      call check_(err == RPC_ERR_EXCEPTION, 'rpc: bad method raises')
   end subroutine t_exception

   !> A level 3 message (provide) must come back as Message.unimplemented
   !> and be quietly absorbed.
   subroutine t_unimplemented()
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(provide_t) :: pv
      call capnp_message_init_builder(m, err)
      msg = message_new_root(m, err)
      pv = message_provide_init(msg, err)
      call provide_question_id_set(pv, 60_int64, err)
      call rpc_send_message(cli%fd, m, err)
      call capnp_message_free(m)
      call check_(err == CAPNP_OK, 'rpc: provide sent')
      call rpc_pump_once(srv, err)
      call check_(err == CAPNP_OK, 'rpc: server replied unimplemented')
      call rpc_pump_once(cli, err)
      call check_(err == CAPNP_OK, 'rpc: client absorbed unimplemented')
   end subroutine t_unimplemented

   !> Level 2 persistence hook: Persistent.save on the bootstrap cap
   !> answers an application-defined SturdyRef.
   subroutine t_persistent_save()
      type(rpc_cap_t) :: bootcap
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(capnp_ptr_t) :: s, content
      integer(int64) :: qb, qs
      character(len=:), allocatable :: ref
      call rpc_bootstrap_send(cli, bootcap, err)
      qb = bootcap%id
      call rpc_pump_once(srv, err)
      call rpc_call_begin(cli, bootcap, RPC_PERSISTENT_IFACE, RPC_PERSISTENT_SAVE, &
                          m, params, qs, err)
      s = capnp_new_struct(m, 0, 0, err)
      call payload_content_set(params, s, err)
      call rpc_call_send(cli, m, err)
      call rpc_pump_once(srv, err)
      call rpc_wait(cli, qb, err)
      call rpc_wait(cli, qs, err)
      call rpc_result_content(cli, qs, content, err)
      call check_(err == CAPNP_OK, 'rpc: save returned')
      call capnp_get_text(content, 0, ref, err)
      call check_(err == CAPNP_OK .and. ref == 'sturdy:echo-main', 'rpc: sturdy ref')
   end subroutine t_persistent_save

   !> Resolve messages come back as unimplemented (the vat keeps using
   !> promise paths); sendResultsTo.yourself calls fail cleanly; a
   !> takeFromOtherQuestion Return surfaces as an exception, not a
   !> kind error.
   subroutine t_resolve_and_tail_calls()
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
      call check_(err == CAPNP_OK, 'rpc: resolve answered')
      call rpc_pump_once(cli, err)
      call check_(err == CAPNP_OK, 'rpc: resolve unimplemented absorbed')

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
      call check_(err == RPC_ERR_EXCEPTION, 'rpc: sendResultsTo.yourself raises')

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
      call check_(err == CAPNP_OK, 'rpc: tail-call return received')
      call rpc_result_content(cli, qy, content, err)
      call check_(err == RPC_ERR_EXCEPTION, 'rpc: tail-call return raises cleanly')
      call check_(index(rpc_conn_reason(cli), 'tail-call') > 0, 'rpc: tail-call reason')
   end subroutine t_resolve_and_tail_calls

   !> A senderPromise capTable entry settles into a usable import, per
   !> the continue-using-the-promise allowance.
   subroutine t_sender_promise_import()
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
      call check_(err == CAPNP_OK .and. cap%kind == RPC_CAP_IMPORT .and. &
                  cap%id == 5_int64, 'rpc: senderPromise settles as import')
   end subroutine t_sender_promise_import

   !> Level 1 embargo: a senderLoopback Disembargo is answered by the
   !> peer as receiverLoopback with the same id and importedCap target
   !> (handle_disembargo on the pumped vat).
   subroutine t_disembargo_echo()
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
      call check_(err == CAPNP_OK, 'rpc: disembargo sent')
      call rpc_pump_once(srv, err)
      call check_(err == CAPNP_OK, 'rpc: server handled disembargo')
      call rpc_recv_message(cli%fd, reply, err)
      call check_(err == CAPNP_OK, 'rpc: disembargo echo received')
      rmsg = message_read_root(reply, err)
      call check_(message_which(rmsg) == MESSAGE_DISEMBARGO_TAG, &
                  'rpc: echo is Disembargo')
      rd = message_disembargo_get(rmsg, err)
      call check_(disembargo_context_which(rd) == &
                  DISEMBARGO_CONTEXT_RECEIVER_LOOPBACK_TAG, &
                  'rpc: receiverLoopback context')
      call check_(disembargo_context_receiver_loopback_get(rd) == emb_id, &
                  'rpc: embago id echoed')
      rtgt = disembargo_target_get(rd, err)
      call check_(message_target_which(rtgt) == MESSAGE_TARGET_IMPORTED_CAP_TAG, &
                  'rpc: echo target is importedCap')
      call check_(message_target_imported_cap_get(rtgt) == import_id, &
                  'rpc: import id echoed')
      call capnp_message_free(reply)
   end subroutine t_disembargo_echo

   !> Poll-driven pumping: a quiet connection times out with
   !> handled=.false.; a pending message is handled within the window.
   subroutine t_pump_poll()
      type(rpc_cap_t) :: bootcap
      logical :: handled
      call rpc_pump_poll(srv, 10, handled, err)
      call check_(err == CAPNP_OK .and. .not. handled, 'poll: quiet times out')
      call rpc_bootstrap_send(cli, bootcap, err)
      call rpc_pump_poll(srv, 1000, handled, err)
      call check_(err == CAPNP_OK .and. handled, 'poll: pending message handled')
      call rpc_wait(cli, bootcap%id, err)
      call check_(err == CAPNP_OK, 'poll: answer arrived')
   end subroutine t_pump_poll

end program test_rpc
