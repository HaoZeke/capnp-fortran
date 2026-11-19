!> Drop a capability that a C++ 1.4.0 peer resolved a promise to.
!> The peer's live count must fall to zero.
program live_client
   use capnp
   use rpc_capnp
   use capnp_posix
   use capnp_rpc
   implicit none

   integer(int64), parameter :: BOX_IFACE = -5689160577028382157_int64
   integer, parameter :: WAIT_TRIES = 80

   type(rpc_conn_t), target :: conn
   class(rpc_server_t), pointer :: nosrv
   type(rpc_cap_t) :: boot, promised
   type(capnp_message_t), target :: m
   type(payload_t) :: params
   type(capnp_ptr_t) :: s, content
   character(len=16) :: arg
   integer(int64) :: qboot, qprom, qres, qlive
   integer :: port, fd, err, i, nfail
   logical :: back

   nfail = 0
   nosrv => null()
   port = 43118
   call get_command_argument(1, arg)
   if (len_trim(arg) > 0) read (arg, *) port

   fd = PX_BAD_FD
   do i = 1, 50
      call px_tcp_connect(port, fd, err)
      if (err == CAPNP_OK) exit
      call execute_command_line('sleep 0.2')
   end do
   if (fd == PX_BAD_FD) then
      print '(a)', 'FAIL: could not connect to C++ peer'
      error stop 1
   end if
   call rpc_conn_init(conn, fd, nosrv)

   call rpc_bootstrap_send(conn, boot, err)
   call check_(err == CAPNP_OK, 'bootstrap sent')
   qboot = boot%id
   call wait_back(conn, qboot, back)
   call check_(back, 'bootstrap returned')
   call rpc_result_cap(conn, qboot, [integer ::], boot, err)
   call check_(err == CAPNP_OK .and. boot%kind == RPC_CAP_IMPORT, 'bootstrap settles')

   call rpc_call_begin(conn, boot, BOX_IFACE, 0, m, params, qprom, err)
   s = capnp_new_struct(m, 0, 0, err)
   call payload_content_set(params, s, err)
   call rpc_call_send(conn, m, err)
   call wait_back(conn, qprom, back)
   call check_(back, 'getPromise returned')
   call rpc_result_cap(conn, qprom, [0], promised, err)
   call check_(err == CAPNP_OK .and. promised%kind == RPC_CAP_IMPORT, 'promise cap')
   ! Keep the promise import. Finish must not release it before Resolve.
   call rpc_finish_send(conn, qprom, .true., err)
   call check_(err == CAPNP_OK, 'getPromise finished')

   call rpc_call_begin(conn, boot, BOX_IFACE, 1, m, params, qres, err)
   s = capnp_new_struct(m, 0, 0, err)
   call payload_content_set(params, s, err)
   call rpc_call_send(conn, m, err)
   call wait_back(conn, qres, back)
   call check_(back, 'resolveNow returned')
   call rpc_finish_send(conn, qres, .false., err)

   ! The Resolve is on the wire ahead of the resolveNow Return, so the
   ! wait above already applied it. Dropping the client must release the
   ! replacement, and the following call observes that.
   call rpc_release_send(conn, promised, err)
   call check_(err == CAPNP_OK, 'resolved cap released')

   call rpc_call_begin(conn, boot, BOX_IFACE, 2, m, params, qlive, err)
   s = capnp_new_struct(m, 0, 0, err)
   call payload_content_set(params, s, err)
   call rpc_call_send(conn, m, err)
   call wait_back(conn, qlive, back)
   call check_(back, 'live returned')
   call rpc_result_content(conn, qlive, content, err)
   call check_(err == CAPNP_OK .and. capnp_get_i64(content, 0_int64) == 0_int64, &
               'C++ peer live count is 0')
   call rpc_finish_send(conn, qlive, .false., err)
   call rpc_finish_send(conn, qboot, .true., err)
   call rpc_conn_close(conn)

   if (nfail > 0) then
      print '(a,i0,a)', 'FAILED: ', nfail, ' assertion(s)'
      error stop 1
   end if
   print '(a)', 'Promise release dropped the C++ capability.'

contains

   subroutine check_(cond, name)
      logical, intent(in) :: cond
      character(len=*), intent(in) :: name
      if (.not. cond) then
         nfail = nfail + 1
         print '(a,a)', 'FAIL: ', name
      end if
   end subroutine check_

   subroutine wait_back(conn, qid, ok)
      type(rpc_conn_t), intent(inout), target :: conn
      integer(int64), intent(in) :: qid
      logical, intent(out) :: ok
      integer :: n, e, q
      logical :: handled
      ok = .false.
      if (qid < 0_int64 .or. qid >= int(size(conn%questions), int64)) return
      q = int(qid)
      do n = 1, WAIT_TRIES
         if (conn%dead) return
         if (conn%questions(q)%returned) then
            ok = .true.
            return
         end if
         call rpc_pump_poll(conn, 50, handled, e)
         if (e /= CAPNP_OK .and. e /= RPC_ERR_DEAD) return
      end do
   end subroutine wait_back

end program live_client
