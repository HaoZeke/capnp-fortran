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
   use testdrive, only: unittest_type, error_type, check
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

   !> Permits every call. The handoff case needs a membrane so the
   !> capability Bob exports is an owned wrapper, which is the object
   !> drop_export frees.
   type, extends(rpc_membrane_policy_t) :: allow_policy_t
      integer :: seen = 0
   contains
      procedure :: inbound_call => allow_inbound
      procedure :: outbound_call => allow_outbound
   end type allow_policy_t

   type, extends(rpc_server_t) :: owned_inner_srv_t
   contains
      procedure :: dispatch => owned_inner_dispatch
      final :: owned_inner_final
   end type owned_inner_srv_t

   type, extends(rpc_server_t) :: owned_factory_srv_t
   contains
      procedure :: dispatch => owned_factory_dispatch
   end type owned_factory_srv_t

   integer :: owned_inner_deaths = 0

contains

   subroutine collect_rpc_handoff(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      allocate (testsuite(8))
      testsuite(1)%name = "handoff"
      testsuite(1)%test => run_handoff
      testsuite(2)%name = "handoff membrane ref"
      testsuite(2)%test => run_membrane_handoff
      testsuite(3)%name = "membrane releases owned inner server"
      testsuite(3)%test => run_membrane_owned_inner
      testsuite(4)%name = "membrane flush failure frees owned inner server"
      testsuite(4)%test => run_membrane_owned_flush_fail
      testsuite(5)%name = "membrane owned server staged twice is freed once"
      testsuite(5)%test => run_membrane_owned_twice
      testsuite(6)%name = "membrane handoff leaves no shared owner entry"
      testsuite(6)%test => run_membrane_owned_handoff
      testsuite(7)%name = "handoff records every introduction"
      testsuite(7)%test => run_introduction_capacity
      testsuite(8)%name = "introductions past the bound fall back to the vine"
      testsuite(8)%test => run_introduction_bound
   end subroutine collect_rpc_handoff

   subroutine allow_inbound(self, interface_id, method_id, err)
      class(allow_policy_t), intent(inout) :: self
      integer(int64), intent(in) :: interface_id
      integer, intent(in) :: method_id
      integer, intent(out) :: err
      self%seen = self%seen + 1
      err = CAPNP_OK
      if (method_id < 0) err = int(interface_id)
   end subroutine allow_inbound

   subroutine allow_outbound(self, interface_id, method_id, err)
      class(allow_policy_t), intent(inout) :: self
      integer(int64), intent(in) :: interface_id
      integer, intent(in) :: method_id
      integer, intent(out) :: err
      self%seen = self%seen + 1
      err = CAPNP_OK
      if (method_id < 0) err = int(interface_id)
   end subroutine allow_outbound

   subroutine owned_inner_final(self)
      type(owned_inner_srv_t), intent(inout) :: self
      owned_inner_deaths = owned_inner_deaths + 1
   end subroutine owned_inner_final

   subroutine owned_inner_dispatch(self, ctx, err)
      class(owned_inner_srv_t), intent(inout) :: self
      type(rpc_call_ctx_t), intent(inout) :: ctx
      integer, intent(out) :: err
      associate (server => self, call => ctx)
      end associate
      err = CAPNP_OK
   end subroutine owned_inner_dispatch

   subroutine owned_factory_dispatch(self, ctx, err)
      class(owned_factory_srv_t), intent(inout) :: self
      type(rpc_call_ctx_t), intent(inout) :: ctx
      integer, intent(out) :: err
      type(owned_inner_srv_t), pointer :: inner
      class(rpc_server_t), pointer :: server
      type(capnp_ptr_t) :: result
      integer :: idx, idx2
      associate (factory => self)
      end associate
      allocate (inner)
      server => inner
      idx = rpc_ctx_export_cap(ctx, server, err, owned=.true.)
      if (err /= CAPNP_OK) then
         deallocate (inner)
         return
      end if
      if (ctx%method_id == 1) then
         idx2 = rpc_ctx_export_cap(ctx, server, err, owned=.true.)
         if (err /= CAPNP_OK) return
         result = capnp_new_struct(ctx%rmsg, 0, 2, err)
         if (err == CAPNP_OK) call capnp_setp(result, 0, rpc_make_cap_ptr(ctx%rmsg, idx), err)
         if (err == CAPNP_OK) call capnp_setp(result, 1, rpc_make_cap_ptr(ctx%rmsg, idx2), err)
      else
         result = capnp_new_struct(ctx%rmsg, 0, 1, err)
         if (err == CAPNP_OK) call capnp_setp(result, 0, rpc_make_cap_ptr(ctx%rmsg, idx), err)
      end if
      if (err == CAPNP_OK) call payload_content_set(ctx%results, result, err)
   end subroutine owned_factory_dispatch

   subroutine check_(error, cond, name)
      type(error_type), allocatable, intent(inout) :: error
      logical, intent(in) :: cond
      character(len=*), intent(in) :: name
      if (allocated(error)) return
      call check(error, cond, name)
   end subroutine check_

   !> Alice -> Carol: a call whose params name the capability Bob hosts.
   !> Alice is the introducer, so she writes the descriptor.
   subroutine tell_where_to_go(fd, qid, host, port, nonce, vine, e, count)
      integer, intent(in) :: fd, port
      integer(int64), intent(in) :: qid, nonce, vine
      character(len=*), intent(in) :: host
      integer, intent(out) :: e
      integer, intent(in), optional :: count
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(call_t) :: c
      type(message_target_t) :: tgt
      type(payload_t) :: params
      type(cap_descriptor_t) :: cd
      type(capnp_ptr_t) :: ctab
      integer :: i, n
      n = 1
      if (present(count)) n = count
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
      ctab = payload_cap_table_init(params, int(n, int64), e)
      if (e /= CAPNP_OK) return
      do i = 0, n - 1
         cd%p = capnp_list_get_struct(ctab, i, e)
         if (e /= CAPNP_OK) return
         call rpc_write_third_party_cap(cd, host, port, nonce + int(i, int64), &
                                        vine + int(i, int64), e)
         if (e /= CAPNP_OK) return
      end do
      call rpc_send_message(fd, m, e)
      call capnp_message_free(m)
   end subroutine tell_where_to_go

   subroutine run_introduction_capacity(error)
      type(error_type), allocatable, intent(out) :: error
      type(rpc_conn_t), target :: receiver
      type(rpc_introduction_t) :: learned(9)
      class(rpc_server_t), pointer :: boot
      integer :: sender_fd, receiver_fd, err, n

      call px_socketpair(sender_fd, receiver_fd, err)
      call check_(error, err == CAPNP_OK, 'introduction capacity: socket pair')
      if (allocated(error)) return
      boot => null()
      call rpc_conn_init(receiver, receiver_fd, boot)
      call tell_where_to_go(sender_fd, 91_int64, '127.0.0.1', 1, 300_int64, &
                            700_int64, err, count=9)
      call check_(error, err == CAPNP_OK, 'introduction capacity: message sent')
      if (.not. allocated(error)) then
         call rpc_pump_once(receiver, err)
         n = rpc_pending_introductions(receiver, learned)
         call check_(error, n == 9, 'introduction capacity: all entries recorded')
      end if
      call rpc_conn_close(receiver)
      call px_close(sender_fd)
   end subroutine run_introduction_capacity

   !> A peer can name any number of third-party caps. Past MAXINTRO_HELD
   !> the receiver keeps only the vine, as C++ 1.4.0 does for every one:
   !> a call's vine is released once the call is answered. A finished
   !> pickup frees its entry for the next introduction.
   subroutine run_introduction_bound(error)
      type(error_type), allocatable, intent(out) :: error
      type(rpc_conn_t), target :: receiver, sender
      type(echo_srv_t), target :: hosted
      type(rpc_introduction_t) :: learned(MAXINTRO_HELD)
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: bootcap
      integer(int64), allocatable :: released(:)
      integer :: sender_fd, receiver_fd, err, n, i, returns
      logical :: found

      call px_socketpair(sender_fd, receiver_fd, err)
      call check_(error, err == CAPNP_OK, 'introduction bound: socket pair')
      if (allocated(error)) return
      boot => hosted
      call rpc_conn_init(receiver, receiver_fd, boot)
      boot => null()
      call rpc_conn_init(sender, sender_fd, boot)
      call rpc_bootstrap_send(sender, bootcap, err)
      call rpc_pump_once(receiver, err)
      call rpc_wait(sender, bootcap%id, err)
      call check_(error, err == CAPNP_OK, 'introduction bound: bootstrapped')

      if (.not. allocated(error)) then
         call tell_where_to_go(sender_fd, 91_int64, '127.0.0.1', 1, 300_int64, &
                               700_int64, err, count=MAXINTRO_HELD + 3)
         call rpc_pump_once(receiver, err)
         n = rpc_pending_introductions(receiver)
         call check_(error, rpc_conn_alive(receiver) .and. n == MAXINTRO_HELD, &
                     'introduction bound: entries kept up to the bound')
      end if
      if (.not. allocated(error)) then
         call drain_releases(sender_fd, released, returns)
         call check_(error, returns == 1 .and. size(released) == 3, &
                     'introduction bound: the call is answered and the rest released')
      end if
      if (.not. allocated(error)) then
         do i = 1, 3
            call check_(error, released(i) == 700_int64 + int(MAXINTRO_HELD + i - 1, int64), &
                        'introduction bound: vines past the bound released')
         end do
      end if
      if (.not. allocated(error)) then
         call rpc_introduction_done(receiver, 300_int64, err)
         call drain_releases(sender_fd, released, returns)
         call check_(error, err == CAPNP_OK .and. size(released) == 1, &
                     'introduction bound: pickup releases its vine')
      end if
      if (.not. allocated(error)) then
         call tell_where_to_go(sender_fd, 92_int64, '127.0.0.1', 1, 5000_int64, &
                               9000_int64, err)
         call rpc_pump_once(receiver, err)
         n = rpc_pending_introductions(receiver, learned)
         found = .false.
         do i = 1, min(n, size(learned))
            if (learned(i)%nonce == 5000_int64) found = .true.
         end do
         call drain_releases(sender_fd, released, returns)
         call check_(error, n == MAXINTRO_HELD .and. found .and. size(released) == 0, &
                     'introduction bound: a freed entry takes the next introduction')
      end if
      call rpc_conn_close(receiver)
      call rpc_conn_close(sender)
   end subroutine run_introduction_bound

   !> Read what the receiver sent: Release ids in order and the number
   !> of Returns.
   subroutine drain_releases(fd, released, returns)
      integer, intent(in) :: fd
      integer(int64), allocatable, intent(out) :: released(:)
      integer, intent(out) :: returns
      type(capnp_message_t), target :: reply
      type(message_t) :: rmsg
      type(release_t) :: rel
      logical :: got
      integer :: e
      allocate (released(0))
      returns = 0
      do
         call px_poll_in(fd, 100, got, e)
         if (e /= CAPNP_OK .or. .not. got) exit
         call rpc_recv_message(fd, reply, e)
         if (e /= CAPNP_OK) exit
         rmsg = message_read_root(reply, e)
         if (message_which(rmsg) == MESSAGE_RETURN_TAG) returns = returns + 1
         if (message_which(rmsg) == MESSAGE_RELEASE_TAG) then
            rel = message_release_get(rmsg, e)
            if (release_reference_count_get(rel) == 1_int64) &
               released = [released, release_id_get(rel)]
         end if
         call capnp_message_free(reply)
      end do
   end subroutine drain_releases

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
      type(rpc_cap_t) :: bootcap, claimed, sidecap, alice_cap
      type(capnp_ptr_t) :: content
      integer :: ab, ba, bc, ac, ca, listen_fd, bob_port, err, n
      integer(int64) :: provide_qid, claim, replay, qcall, after_release
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(capnp_ptr_t) :: s

      call px_socketpair(ab, ba, err)
      call check_(error, err == CAPNP_OK, 'handoff: alice-bob pair')
      call px_socketpair(ac, ca, err)
      call check_(error, err == CAPNP_OK, 'handoff: alice-carol pair')
      call px_tcp_listen(0, listen_fd, err)
      call check_(error, err == CAPNP_OK, 'handoff: bob listener')
      call px_tcp_bound_port(listen_fd, bob_port, err)
      call check_(error, err == CAPNP_OK .and. bob_port > 0, 'handoff: listener port')
      if (allocated(error)) return

      boot => null()
      call rpc_conn_init(alice_to_bob, ab, boot)
      call rpc_conn_init(alice_to_carol, ac, boot)
      boot => hosted
      call rpc_conn_init(bob_to_alice, ba, boot)
      boot => carols
      call rpc_conn_init(carol_to_alice, ca, boot)

      ! Bob is one vat with two connections, so the arrangement Alice
      ! makes on hers is claimable on Carol's.
      bob_vat => bob_vat_store
      call rpc_conn_set_vat(bob_to_alice, bob_vat)

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

      ! 2. Alice tells Carol where to go. Carol records the introduction
      !    before the network layer dials it.
      call tell_where_to_go(ac, 90_int64, '127.0.0.1', bob_port, NONCE, 7_int64, err)
      call check_(error, err == CAPNP_OK, 'handoff: introduction sent')
      call rpc_pump_once(carol_to_alice, err)
      n = rpc_pending_introductions(carol_to_alice, learned)
      call check_(error, n == 1, 'handoff: introduction recorded')
      if (allocated(error)) return
      call check_(error, learned(1)%nonce == NONCE, 'handoff: introduction nonce')
      call check_(error, learned(1)%host(1:learned(1)%host_len) == '127.0.0.1', &
                  'handoff: introduction host')
      call check_(error, learned(1)%port == bob_port, 'handoff: introduction port')

      ! The network-layer VatId is actionable: Carol dials Bob using the
      ! address in ThirdPartyCapId, then Bob accepts it as another
      ! connection belonging to the same vat.
      boot => null()
      carol_to_alice%dial_allow => allow_local_dial
      call rpc_introduction_dial(carol_to_alice, NONCE, carol_to_bob, boot, err)
      call check_(error, err == CAPNP_OK, 'handoff: introduction dialed')
      call px_tcp_accept(listen_fd, bc, err)
      call check_(error, err == CAPNP_OK, 'handoff: bob accepted Carol')
      sidecar%base = 1000_int64
      boot => sidecar
      call rpc_conn_init(bob_to_carol, bc, boot)
      call rpc_conn_set_vat(bob_to_carol, bob_vat)
      call check_(error, rpc_pending_provisions(bob_to_carol) == 1, &
                  'handoff: the vat''s new connection sees it')
      call px_close(listen_fd)

      ! Carol bootstraps Bob first, so her connection's export 0 is the
      ! sidecar and the handed-over capability cannot land on 0 too.
      call rpc_bootstrap_send(carol_to_bob, sidecap, err)
      call rpc_pump_once(bob_to_carol, err)
      call rpc_wait(carol_to_bob, sidecap%id, err)
      call check_(error, err == CAPNP_OK, 'handoff: carol bootstrapped the sidecar')

      ! The provision holds a reference to Alice's export. Releasing her
      ! import while that arrangement is open must not drop it.
      alice_cap%kind = RPC_CAP_IMPORT
      alice_cap%id = 0_int64
      call rpc_release_send(alice_to_bob, alice_cap, err)
      call rpc_pump_once(bob_to_alice, err)
      call check_(error, bob_to_alice%exports(0)%used, &
                  'handoff: open provision pins the source export')
      if (allocated(error)) return

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

      ! Accept consumes the provision and drops its pin. Alice already
      ! released her own reference, so the source export is gone and a
      ! second Provide of that id is rejected.
      call rpc_provide_send(alice_to_bob, 0_int64, '127.0.0.1', bob_port, &
                            NONCE + 1_int64, after_release, err)
      call rpc_pump_once(bob_to_alice, err)
      call rpc_wait(alice_to_bob, after_release, err)
      call rpc_result_content(alice_to_bob, after_release, content, err)
      call check_(error, err == RPC_ERR_EXCEPTION, &
                  'handoff: accepted provision does not pin source export')

      call rpc_conn_close(alice_to_bob)
      call rpc_conn_close(bob_to_alice)
      call rpc_conn_close(carol_to_bob)
      call rpc_conn_close(bob_to_carol)
      call rpc_conn_close(alice_to_carol)
      call rpc_conn_close(carol_to_alice)
   end subroutine run_handoff

   !> Hand off an owned membrane wrapper and call it after the introducer
   !> has released her reference. Both connections then close. Without a
   !> reference on the provision and on the accepted export, the second
   !> drop frees the wrapper twice and the call dispatches into it.
   subroutine run_membrane_handoff(error)
      type(error_type), allocatable, intent(out) :: error
      type(rpc_vat_t), target :: bob_vat_store
      type(rpc_vat_t), pointer :: bob_vat
      type(rpc_conn_t), target :: alice, bob_alice, carol, bob_carol
      type(echo_srv_t), target :: inner
      type(rpc_membrane_server_t), target :: wrapped
      type(allow_policy_t), target :: policy_store
      class(rpc_server_t), pointer :: boot, inner_p
      class(rpc_membrane_policy_t), pointer :: policy
      type(rpc_cap_t) :: bootcap, minted, claimed
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(capnp_ptr_t) :: s, content
      integer :: ab, ba, cb, bc, err
      integer(int64) :: qb, qm, provide_qid, claim, qcall

      call px_socketpair(ab, ba, err)
      call check_(error, err == CAPNP_OK, 'membrane handoff: alice pair')
      call px_socketpair(cb, bc, err)
      call check_(error, err == CAPNP_OK, 'membrane handoff: carol pair')
      if (allocated(error)) return

      inner_p => inner
      policy => policy_store
      call rpc_membrane_server_init(wrapped, inner_p, policy)
      boot => null()
      call rpc_conn_init(alice, ab, boot)
      call rpc_conn_init(carol, cb, boot)
      boot => wrapped
      call rpc_conn_init(bob_alice, ba, boot)
      call rpc_conn_init(bob_carol, bc, boot)
      bob_vat => bob_vat_store
      call rpc_conn_set_vat(bob_alice, bob_vat)
      call rpc_conn_set_vat(bob_carol, bob_vat)

      call rpc_bootstrap_send(alice, bootcap, err)
      qb = bootcap%id
      call rpc_pump_once(bob_alice, err)
      call rpc_wait(alice, qb, err)
      call rpc_result_cap(alice, qb, [integer ::], bootcap, err)
      call check_(error, err == CAPNP_OK, 'membrane handoff: bootstrap')
      if (allocated(error)) return

      ! Method 1 mints an adder. The membrane wraps it in an owned server.
      call capnp_message_init_builder(m, err)
      call rpc_call_begin(alice, bootcap, ECHO_IFACE, 1, m, params, qm, err)
      s = capnp_new_struct(m, 1, 0, err)
      call capnp_set_i64(s, 0_int64, 100_int64, err)
      call payload_content_set(params, s, err)
      call rpc_call_send(alice, m, err)
      call rpc_pump_once(bob_alice, err)
      call rpc_wait(alice, qm, err)
      call rpc_result_cap(alice, qm, [0], minted, err)
      call check_(error, err == CAPNP_OK .and. minted%kind == RPC_CAP_IMPORT, &
                  'membrane handoff: minted cap')
      if (allocated(error)) return

      call rpc_provide_send(alice, minted%id, '127.0.0.1', 9, NONCE + 7_int64, &
                            provide_qid, err)
      call rpc_pump_once(bob_alice, err)
      call rpc_wait(alice, provide_qid, err)
      call check_(error, err == CAPNP_OK .and. rpc_pending_provisions(bob_alice) == 1, &
                  'membrane handoff: provision recorded')

      ! Alice drops her reference before Carol accepts. The provision must
      ! keep the wrapper alive.
      call rpc_finish_send(alice, qm, .false., err)
      call rpc_pump_once(bob_alice, err)
      call rpc_finish_send(alice, qb, .false., err)
      call rpc_pump_once(bob_alice, err)

      call rpc_accept_send(carol, NONCE + 7_int64, .false., claim, err)
      call rpc_pump_once(bob_carol, err)
      call rpc_wait(carol, claim, err)
      call rpc_result_cap(carol, claim, [integer ::], claimed, err)
      call check_(error, err == CAPNP_OK .and. claimed%kind == RPC_CAP_IMPORT, &
                  'membrane handoff: accept survived the release')
      if (allocated(error)) return

      call capnp_message_init_builder(m, err)
      call rpc_call_begin(carol, claimed, ECHO_IFACE, 0, m, params, qcall, err)
      s = capnp_new_struct(m, 1, 0, err)
      call capnp_set_i64(s, 0_int64, 5_int64, err)
      call payload_content_set(params, s, err)
      call rpc_call_send(carol, m, err)
      call rpc_pump_once(bob_carol, err)
      call rpc_wait(carol, qcall, err)
      call rpc_result_content(carol, qcall, content, err)
      call check_(error, err == CAPNP_OK .and. capnp_get_i64(content, 0_int64) == 105_int64, &
                  'membrane handoff: claimed wrapper still answers')

      call rpc_conn_close(alice)
      call rpc_conn_close(bob_alice)
      call rpc_conn_close(carol)
      call rpc_conn_close(bob_carol)
   end subroutine run_membrane_handoff

   subroutine run_membrane_owned_inner(error)
      type(error_type), allocatable, intent(out) :: error
      type(rpc_conn_t), target :: cli, srv
      type(owned_factory_srv_t), target :: factory
      type(rpc_membrane_server_t), target :: wrapped
      type(allow_policy_t), target :: policy_store
      class(rpc_server_t), pointer :: inner, boot
      class(rpc_membrane_policy_t), pointer :: policy
      type(rpc_cap_t) :: pipe
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(capnp_ptr_t) :: empty
      integer :: fa, fb, err
      integer(int64) :: qb, q
      call px_socketpair(fa, fb, err)
      call check_(error, err == CAPNP_OK, 'membrane inner: socketpair')
      if (allocated(error)) return
      inner => factory
      policy => policy_store
      call rpc_membrane_server_init(wrapped, inner, policy)
      boot => wrapped
      call rpc_conn_init(srv, fb, boot)
      boot => null()
      call rpc_conn_init(cli, fa, boot)
      owned_inner_deaths = 0
      call rpc_bootstrap_send(cli, pipe, err)
      qb = pipe%id
      call rpc_pump_once(srv, err)
      call rpc_wait(cli, qb, err)
      call rpc_result_cap(cli, qb, [integer ::], pipe, err)
      call check_(error, err == CAPNP_OK, 'membrane inner: bootstrap')
      if (.not. allocated(error)) then
         call rpc_call_begin(cli, pipe, 991_int64, 0, m, params, q, err)
         if (err == CAPNP_OK) empty = capnp_new_struct(m, 0, 0, err)
         if (err == CAPNP_OK) call payload_content_set(params, empty, err)
         if (err == CAPNP_OK) call rpc_call_send(cli, m, err)
         if (err == CAPNP_OK) call rpc_pump_once(srv, err)
         if (err == CAPNP_OK) call rpc_wait(cli, q, err)
         if (err == CAPNP_OK) call rpc_finish_send(cli, q, .false., err)
         if (err == CAPNP_OK) call rpc_pump_once(srv, err)
         call check_(error, err == CAPNP_OK .and. owned_inner_deaths == 1, &
                     'membrane inner: owned inner server is released')
      end if
      call rpc_conn_close(cli)
      call rpc_conn_close(srv)
   end subroutine run_membrane_owned_inner

   subroutine run_membrane_owned_flush_fail(error)
      type(error_type), allocatable, intent(out) :: error
      type(rpc_conn_t), target :: cli, srv
      type(owned_factory_srv_t), target :: factory
      type(rpc_membrane_server_t), target :: wrapped
      type(allow_policy_t), target :: policy_store
      class(rpc_server_t), pointer :: inner, boot
      class(rpc_membrane_policy_t), pointer :: policy
      type(rpc_cap_t) :: pipe
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(capnp_ptr_t) :: empty
      integer :: fa, fb, err, i, before_live
      integer(int64) :: qb, q
      call px_socketpair(fa, fb, err)
      call check_(error, err == CAPNP_OK, 'membrane flush fail: socketpair')
      if (allocated(error)) return
      inner => factory
      policy => policy_store
      call rpc_membrane_server_init(wrapped, inner, policy)
      boot => wrapped
      call rpc_conn_init(srv, fb, boot)
      boot => null()
      call rpc_conn_init(cli, fa, boot)
      owned_inner_deaths = 0
      before_live = rpc_membrane_owned_live()
      call rpc_bootstrap_send(cli, pipe, err)
      qb = pipe%id
      call rpc_pump_once(srv, err)
      call rpc_wait(cli, qb, err)
      call rpc_result_cap(cli, qb, [integer ::], pipe, err)
      call check_(error, err == CAPNP_OK, 'membrane flush fail: bootstrap')
      if (.not. allocated(error)) then
         do i = 1, size(srv%exports) - 1
            srv%exports(i)%used = .true.
            srv%exports(i)%refcount = 1
            srv%exports(i)%srv => factory
         end do
         call rpc_call_begin(cli, pipe, 991_int64, 0, m, params, q, err)
         if (err == CAPNP_OK) empty = capnp_new_struct(m, 0, 0, err)
         if (err == CAPNP_OK) call payload_content_set(params, empty, err)
         if (err == CAPNP_OK) call rpc_call_send(cli, m, err)
         if (err == CAPNP_OK) call rpc_pump_once(srv, err)
         if (err == CAPNP_OK) call rpc_wait(cli, q, err)
         call check_(error, err == CAPNP_OK .and. owned_inner_deaths == 1 .and. &
                     rpc_membrane_owned_live() == before_live, &
                     'membrane flush fail: owned inner server and wrapper freed')
      end if
      call rpc_conn_close(cli)
      call rpc_conn_close(srv)
   end subroutine run_membrane_owned_flush_fail

   subroutine run_membrane_owned_twice(error)
      type(error_type), allocatable, intent(out) :: error
      type(rpc_conn_t), target :: cli, srv
      type(owned_factory_srv_t), target :: factory
      type(rpc_membrane_server_t), target :: wrapped
      type(allow_policy_t), target :: policy_store
      class(rpc_server_t), pointer :: inner, boot
      class(rpc_membrane_policy_t), pointer :: policy
      type(rpc_cap_t) :: pipe
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(capnp_ptr_t) :: empty
      integer :: fa, fb, err, before_live
      integer(int64) :: qb, q
      call px_socketpair(fa, fb, err)
      call check_(error, err == CAPNP_OK, 'membrane twice: socketpair')
      if (allocated(error)) return
      inner => factory
      policy => policy_store
      call rpc_membrane_server_init(wrapped, inner, policy)
      boot => wrapped
      call rpc_conn_init(srv, fb, boot)
      boot => null()
      call rpc_conn_init(cli, fa, boot)
      owned_inner_deaths = 0
      before_live = rpc_membrane_owned_live()
      call rpc_bootstrap_send(cli, pipe, err)
      qb = pipe%id
      call rpc_pump_once(srv, err)
      call rpc_wait(cli, qb, err)
      call rpc_result_cap(cli, qb, [integer ::], pipe, err)
      call check_(error, err == CAPNP_OK, 'membrane twice: bootstrap')
      if (.not. allocated(error)) then
         call rpc_call_begin(cli, pipe, 991_int64, 1, m, params, q, err)
         if (err == CAPNP_OK) empty = capnp_new_struct(m, 0, 0, err)
         if (err == CAPNP_OK) call payload_content_set(params, empty, err)
         if (err == CAPNP_OK) call rpc_call_send(cli, m, err)
         if (err == CAPNP_OK) call rpc_pump_once(srv, err)
         if (err == CAPNP_OK) call rpc_wait(cli, q, err)
         if (err == CAPNP_OK) call rpc_finish_send(cli, q, .false., err)
         if (err == CAPNP_OK) call rpc_pump_once(srv, err)
         call check_(error, err == CAPNP_OK .and. owned_inner_deaths == 1 .and. &
                     rpc_membrane_owned_live() == before_live, &
                     'membrane twice: owned inner server freed once')
      end if
      call rpc_conn_close(cli)
      call rpc_conn_close(srv)
   end subroutine run_membrane_owned_twice

   subroutine run_membrane_owned_handoff(error)
      type(error_type), allocatable, intent(out) :: error
      type(rpc_vat_t), target :: vat
      type(rpc_conn_t), target :: intro, prov_a, recip, prov_b
      type(owned_factory_srv_t), target :: factory
      type(rpc_membrane_server_t), target :: wrapped
      type(allow_policy_t), target :: policy_store
      class(rpc_server_t), pointer :: inner, boot
      class(rpc_membrane_policy_t), pointer :: policy
      type(rpc_cap_t) :: bcap, ocap, got
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(capnp_ptr_t) :: empty
      integer :: ia, pa, ra, pb, err, before_live, before_shared
      integer(int64) :: qb, qo, pq, aq
      call px_socketpair(ia, pa, err)
      if (err == CAPNP_OK) call px_socketpair(ra, pb, err)
      call check_(error, err == CAPNP_OK, 'membrane owned handoff: socketpairs')
      if (allocated(error)) return
      inner => factory
      policy => policy_store
      call rpc_membrane_server_init(wrapped, inner, policy)
      boot => wrapped
      call rpc_conn_init(prov_a, pa, boot)
      call rpc_conn_init(prov_b, pb, boot)
      call rpc_conn_set_vat(prov_a, vat)
      call rpc_conn_set_vat(prov_b, vat)
      boot => null()
      call rpc_conn_init(intro, ia, boot)
      call rpc_conn_init(recip, ra, boot)
      owned_inner_deaths = 0
      before_live = rpc_membrane_owned_live()
      before_shared = rpc_shared_owners_live()
      call rpc_bootstrap_send(intro, bcap, err)
      qb = bcap%id
      call rpc_pump_once(prov_a, err)
      call rpc_wait(intro, qb, err)
      call rpc_result_cap(intro, qb, [integer ::], bcap, err)
      call check_(error, err == CAPNP_OK, 'membrane owned handoff: bootstrap')
      if (.not. allocated(error)) then
         call rpc_call_begin(intro, bcap, 991_int64, 0, m, params, qo, err)
         if (err == CAPNP_OK) empty = capnp_new_struct(m, 0, 0, err)
         if (err == CAPNP_OK) call payload_content_set(params, empty, err)
         if (err == CAPNP_OK) call rpc_call_send(intro, m, err)
         if (err == CAPNP_OK) call rpc_pump_once(prov_a, err)
         if (err == CAPNP_OK) call rpc_wait(intro, qo, err)
         if (err == CAPNP_OK) call rpc_result_cap(intro, qo, [0], ocap, err)
         if (err == CAPNP_OK) call rpc_provide_send(intro, ocap%id, '127.0.0.1', 9, 511_int64, pq, err)
         if (err == CAPNP_OK) call rpc_pump_once(prov_a, err)
         if (err == CAPNP_OK) call rpc_wait(intro, pq, err)
         if (err == CAPNP_OK) call rpc_accept_send(recip, 511_int64, .false., aq, err)
         if (err == CAPNP_OK) call rpc_pump_once(prov_b, err)
         if (err == CAPNP_OK) call rpc_wait(recip, aq, err)
         if (err == CAPNP_OK) call rpc_result_cap(recip, aq, [integer ::], got, err)
         ! The call answer and the Accept answer keep pipeline pins until
         ! Finish. Drop those pins first so the Releases are the last
         ! references, which is when the owned inner server is freed.
         if (err == CAPNP_OK) call rpc_finish_send(intro, qo, .true., err)
         if (err == CAPNP_OK) call rpc_pump_once(prov_a, err)
         if (err == CAPNP_OK) call rpc_finish_send(recip, aq, .true., err)
         if (err == CAPNP_OK) call rpc_pump_once(prov_b, err)
         if (err == CAPNP_OK) call rpc_release_send(intro, ocap, err)
         if (err == CAPNP_OK) call rpc_pump_once(prov_a, err)
         if (err == CAPNP_OK) call rpc_release_send(recip, got, err)
         if (err == CAPNP_OK) call rpc_pump_once(prov_b, err)
         call check_(error, err == CAPNP_OK .and. owned_inner_deaths == 1 .and. &
                     rpc_membrane_owned_live() == before_live .and. &
                     rpc_shared_owners_live() == before_shared, &
                     'membrane owned handoff: no shared owner entry leaked')
      end if
      call rpc_conn_close(intro)
      call rpc_conn_close(recip)
      call rpc_conn_close(prov_a)
      call rpc_conn_close(prov_b)
   end subroutine run_membrane_owned_handoff

   subroutine allow_local_dial(host, port, allow)
      character(len=*), intent(in) :: host
      integer, intent(in) :: port
      logical, intent(out) :: allow
      allow = host == '127.0.0.1' .and. port > 0
   end subroutine allow_local_dial

end module test_rpc_handoff
