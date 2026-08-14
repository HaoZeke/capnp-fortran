!> Two-party RPC over a socketpair, both vats in one process, pumped in
!> lockstep: bootstrap, pipelined and settled calls, capability results,
!> exceptions, finish/release bookkeeping, and the unimplemented reply.
module test_rpc
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed, skip_test
   use capnp
   use rpc_capnp
   use rpc_twoparty_capnp
   use capnp_posix
   use capnp_rpc
   use capnp_rpc_transport
   use rpc_servers
   implicit none

   private
   public :: collect_rpc

   type(rpc_conn_t), target :: cli, srv
   type(echo_srv_t), target :: echo
   class(rpc_server_t), pointer :: boot
   integer :: fda, fdb, err


contains

   subroutine collect_rpc(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      ! One ordered suite: cases share a socketpair connection (setup once).
      testsuite = [new_unittest("rpc", run_rpc)]
   end subroutine collect_rpc

   subroutine check_(error, cond, name)
      type(error_type), allocatable, intent(inout) :: error
      logical, intent(in) :: cond
      character(len=*), intent(in) :: name
      if (allocated(error)) return
      call check(error, cond, name)
   end subroutine check_

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
      if (.not. allocated(error)) call t_join_same_capability(error)
      if (.not. allocated(error)) call t_join_unresolvable_part(error)
      if (.not. allocated(error)) call t_join_all_parts_unresolvable(error)
      if (.not. allocated(error)) call t_join_incomplete_set_is_silent(error)

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

   !> Read one Return off the client socket and unpack its JoinResult.
   subroutine recv_join_result(ansid, jid, succeeded, has_cap, e)
      integer(int64), intent(out) :: ansid, jid
      logical, intent(out) :: succeeded, has_cap
      integer, intent(out) :: e
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(return_t) :: r
      type(payload_t) :: pl
      type(capnp_ptr_t) :: content, capp
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
      if (e == CAPNP_OK) has_cap = capp%kind == CAPNP_PK_CAP
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
   end subroutine t_join_incomplete_set_is_silent

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
      adder_p = rpc_pipeline_cap(qm, [0])
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
   end subroutine t_exception

   !> A level 3 message (provide) must come back as Message.unimplemented
   !> and be quietly absorbed.
   subroutine t_unimplemented(error)
      type(error_type), allocatable, intent(inout) :: error
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(provide_t) :: pv
      call capnp_message_init_builder(m, err)
      msg = message_new_root(m, err)
      pv = message_provide_init(msg, err)
      call provide_question_id_set(pv, 60_int64, err)
      call rpc_send_message(cli%fd, m, err)
      call capnp_message_free(m)
      call check_(error, err == CAPNP_OK, 'rpc: provide sent')
      call rpc_pump_once(srv, err)
      call check_(error, err == CAPNP_OK, 'rpc: server replied unimplemented')
      call rpc_pump_once(cli, err)
      call check_(error, err == CAPNP_OK, 'rpc: client absorbed unimplemented')
   end subroutine t_unimplemented

   !> Level 2 persistence hook: Persistent.save on the bootstrap cap
   !> answers an application-defined SturdyRef.
   subroutine t_persistent_save(error)
      type(error_type), allocatable, intent(inout) :: error
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
      call check_(error, err == CAPNP_OK, 'rpc: save returned')
      call capnp_get_text(content, 0, ref, err)
      call check_(error, err == CAPNP_OK .and. ref == 'sturdy:echo-main', 'rpc: sturdy ref')
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
   end subroutine t_pump_poll

end module test_rpc
