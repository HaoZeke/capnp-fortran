!> A whole level 3 handoff, with three vats.
!>
!> Alice holds a capability that Bob hosts and wants Carol to have it.
!> The cases in test_rpc drive one side at a time with hand-built frames;
!> this one runs all three vats and lets them speak to each other, which
!> is the only way to see that the halves agree:
!>
!>   Alice -> Bob    Provide{target, recipient = RecipientId{carol, nonce}}
!>   Alice -> Carol  a payload carrying ThirdPartyCapId{bob, nonce}
!>   Carol -> Bob    Accept{ProvisionId{nonce}}
!>   Bob   -> Carol  Return carrying the capability
!>
!> The nonce is the only thing the three messages share, which is what
!> lets Bob hand the capability over without taking Carol's word for who
!> sent her.
module test_rpc_handoff
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use capnp
   use rpc_capnp
   use rpc_threeparty_capnp
   use capnp_posix
   use capnp_rpc
   use capnp_rpc_transport
   use rpc_servers, only: echo_srv_t, adder_srv_t, ECHO_IFACE
   implicit none

   private
   public :: collect_rpc_handoff

   integer(int64), parameter :: NONCE = 24301_int64

contains

   subroutine collect_rpc_handoff(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [new_unittest("handoff", run_handoff)]
   end subroutine collect_rpc_handoff

   subroutine check_(error, cond, name)
      type(error_type), allocatable, intent(inout) :: error
      logical, intent(in) :: cond
      character(len=*), intent(in) :: name
      if (allocated(error)) return
      call check(error, cond, name)
   end subroutine check_

   !> Alice -> Carol: a call whose params name the capability Bob hosts.
   !> Alice is the introducer, so she writes the descriptor.
   subroutine tell_where_to_go(fd, qid, host, port, nonce, vine, e)
      integer, intent(in) :: fd, port
      integer(int64), intent(in) :: qid, nonce, vine
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
      call message_target_imported_cap_set(tgt, 0_int64, e)
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
      call rpc_send_message(fd, m, e)
      call capnp_message_free(m)
   end subroutine tell_where_to_go

   subroutine run_handoff(error)
      type(error_type), allocatable, intent(out) :: error
      type(rpc_vat_t), target :: bob_vat_store
      type(rpc_vat_t), pointer :: bob_vat
      type(rpc_conn_t), target :: alice_to_bob, bob_to_alice
      type(rpc_conn_t), target :: carol_to_bob, bob_to_carol
      type(rpc_conn_t), target :: alice_to_carol, carol_to_alice
      type(echo_srv_t), target :: hosted, carols
      !> Bob answers Carol's connection with a different object, so the
      !> two connections do not agree on export ids by accident. The
      !> capability Alice hands over must arrive under an id of Carol's
      !> connection, not the one Alice used.
      type(adder_srv_t), target :: sidecar
      class(rpc_server_t), pointer :: boot
      type(rpc_introduction_t) :: learned(4)
      type(rpc_cap_t) :: bootcap, claimed, sidecap
      type(capnp_ptr_t) :: content
      integer :: ab, ba, cb, bc, ac, ca, err, n
      integer(int64) :: provide_qid, claim, replay, qcall
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(capnp_ptr_t) :: s

      call px_socketpair(ab, ba, err)
      call check_(error, err == CAPNP_OK, 'handoff: alice-bob pair')
      call px_socketpair(cb, bc, err)
      call check_(error, err == CAPNP_OK, 'handoff: carol-bob pair')
      call px_socketpair(ac, ca, err)
      call check_(error, err == CAPNP_OK, 'handoff: alice-carol pair')
      if (allocated(error)) return

      boot => null()
      call rpc_conn_init(alice_to_bob, ab, boot)
      call rpc_conn_init(carol_to_bob, cb, boot)
      call rpc_conn_init(alice_to_carol, ac, boot)
      boot => hosted
      call rpc_conn_init(bob_to_alice, ba, boot)
      sidecar%base = 1000_int64
      boot => sidecar
      call rpc_conn_init(bob_to_carol, bc, boot)
      boot => carols
      call rpc_conn_init(carol_to_alice, ca, boot)

      ! Bob is one vat with two connections, so the arrangement Alice
      ! makes on hers is claimable on Carol's.
      bob_vat => bob_vat_store
      call rpc_conn_set_vat(bob_to_alice, bob_vat)
      call rpc_conn_set_vat(bob_to_carol, bob_vat)

      ! Alice bootstraps, so Bob exports the capability to her.
      call rpc_bootstrap_send(alice_to_bob, bootcap, err)
      call rpc_pump_once(bob_to_alice, err)
      call rpc_wait(alice_to_bob, bootcap%id, err)
      call check_(error, err == CAPNP_OK, 'handoff: bootstrap answered')

      ! 1. Alice tells Bob to expect Carol.
      call rpc_provide_send(alice_to_bob, 0_int64, '10.0.0.2', 5001, NONCE, &
                            provide_qid, err)
      call check_(error, err == CAPNP_OK, 'handoff: provide sent')
      call rpc_pump_once(bob_to_alice, err)
      call rpc_wait(alice_to_bob, provide_qid, err)
      call check_(error, err == CAPNP_OK, 'handoff: provide answered')
      call check_(error, rpc_pending_provisions(bob_to_alice) == 1, &
                  'handoff: arrangement recorded')
      ! The same vat, seen through its other connection.
      call check_(error, rpc_pending_provisions(bob_to_carol) == 1, &
                  'handoff: the vat''s other connection sees it')

      ! 2. Alice tells Carol where to go. Carol records the introduction
      !    rather than dialling: reaching Bob is the network's job, and
      !    here the connection already exists.
      call tell_where_to_go(ac, 90_int64, '10.0.0.1', 5000, NONCE, 7_int64, err)
      call check_(error, err == CAPNP_OK, 'handoff: introduction sent')
      call rpc_pump_once(carol_to_alice, err)
      n = rpc_pending_introductions(carol_to_alice, learned)
      call check_(error, n == 1, 'handoff: introduction recorded')
      if (allocated(error)) return
      call check_(error, learned(1)%nonce == NONCE, 'handoff: introduction nonce')
      call check_(error, learned(1)%host(1:learned(1)%host_len) == '10.0.0.1', &
                  'handoff: introduction host')
      call check_(error, learned(1)%port == 5000, 'handoff: introduction port')

      ! Carol bootstraps Bob first, so her connection's export 0 is the
      ! sidecar and the handed-over capability cannot land on 0 too.
      call rpc_bootstrap_send(carol_to_bob, sidecap, err)
      call rpc_pump_once(bob_to_carol, err)
      call rpc_wait(carol_to_bob, sidecap%id, err)
      call check_(error, err == CAPNP_OK, 'handoff: carol bootstrapped the sidecar')

      ! 3. Carol presents the nonce to Bob, over her own connection. She
      !    was never told which export id Alice used, and it would mean
      !    nothing here: the arrangement is keyed by the nonce alone.
      call rpc_accept_send(carol_to_bob, NONCE, .false., claim, err)
      call check_(error, err == CAPNP_OK, 'handoff: accept sent')
      call rpc_pump_once(bob_to_carol, err)
      call rpc_wait(carol_to_bob, claim, err)
      call check_(error, err == CAPNP_OK, 'handoff: accept answered')
      ! The capability is the payload's content itself, so no field walk.
      call rpc_result_cap(carol_to_bob, claim, [integer ::], claimed, err)
      call check_(error, err == CAPNP_OK .and. claimed%kind == RPC_CAP_IMPORT, &
                  'handoff: Carol holds the capability')
      call check_(error, rpc_pending_provisions(bob_to_alice) == 0, &
                  'handoff: arrangement consumed')

      ! Claimable exactly once, on any connection.
      call rpc_accept_send(carol_to_bob, NONCE, .false., replay, err)
      call rpc_pump_once(bob_to_carol, err)
      call rpc_wait(carol_to_bob, replay, err)
      call rpc_result_content(carol_to_bob, replay, content, err)
      call check_(error, err == RPC_ERR_EXCEPTION, &
                  'handoff: a nonce cannot be claimed twice')

      ! Calling the claimed capability reaches the object Alice was
      ! talking to, not whatever else sits at that id on Carol's
      ! connection: echo doubles, the sidecar would have added 1000.
      call capnp_message_init_builder(m, err)
      call rpc_call_begin(carol_to_bob, claimed, ECHO_IFACE, 0, m, params, qcall, err)
      s = capnp_new_struct(m, 1, 1, err)
      call capnp_set_i64(s, 0_int64, 21_int64, err)
      call capnp_set_text(s, 0, 'hi', err)
      call payload_content_set(params, s, err)
      call rpc_call_send(carol_to_bob, m, err)
      call rpc_pump_once(bob_to_carol, err)
      call rpc_wait(carol_to_bob, qcall, err)
      call rpc_result_content(carol_to_bob, qcall, content, err)
      call check_(error, err == CAPNP_OK, 'handoff: call on the claimed cap answered')
      call check_(error, capnp_get_i64(content, 0_int64) == 42_int64, &
                  'handoff: the claimed cap is the one Alice provided')

      ! 4. Carol drops the vine now that the pickup is done.
      call rpc_introduction_done(carol_to_alice, NONCE, err)
      call check_(error, err == CAPNP_OK, 'handoff: pickup finished')
      call check_(error, rpc_pending_introductions(carol_to_alice) == 0, &
                  'handoff: vine dropped')

      call rpc_conn_close(alice_to_bob)
      call rpc_conn_close(bob_to_alice)
      call rpc_conn_close(carol_to_bob)
      call rpc_conn_close(bob_to_carol)
      call rpc_conn_close(alice_to_carol)
      call rpc_conn_close(carol_to_alice)
   end subroutine run_handoff

end module test_rpc_handoff
