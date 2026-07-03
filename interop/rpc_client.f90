!> RPC interop client: connect to the capnp-C++ Adder server, bootstrap,
!> call add() both pipelined (before the bootstrap return settles) and on
!> the settled import. Exits nonzero on any mismatch.
program rpc_client
   use capnp
   use rpc_capnp
   use capnp_posix
   use capnp_rpc
   implicit none

   ! Signed container for interface id 0xea01e10cbc414411 (adder.capnp).
   integer(int64), parameter :: ADDER_IFACE = -1584738149043452911_int64

   type(rpc_conn_t), target :: conn
   class(rpc_server_t), pointer :: nosrv
   type(rpc_cap_t) :: bootcap, adder
   type(capnp_message_t), target :: m
   type(payload_t) :: params
   type(capnp_ptr_t) :: s, content
   character(len=16) :: arg
   integer(int64) :: q1, q2
   integer :: port, fd, err, i, nfail

   nfail = 0
   nosrv => null()
   port = 43117
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

   call rpc_bootstrap_send(conn, bootcap, err)
   call check_(err == CAPNP_OK, 'bootstrap sent')

   ! Pipelined call on the not-yet-settled bootstrap capability.
   call rpc_call_begin(conn, bootcap, ADDER_IFACE, 0, m, params, q1, err)
   call check_(err == CAPNP_OK, 'call begin')
   s = capnp_new_struct(m, 2, 0, err)
   call capnp_set_i64(s, 0_int64, 2_int64, err)
   call capnp_set_i64(s, 8_int64, 40_int64, err)
   call payload_content_set(params, s, err)
   call rpc_call_send(conn, m, err)
   call check_(err == CAPNP_OK, 'pipelined add sent')

   call rpc_wait(conn, q1, err)
   call check_(err == CAPNP_OK, 'pipelined add returned')
   call rpc_result_content(conn, q1, content, err)
   call check_(err == CAPNP_OK, 'pipelined add content')
   call check_(capnp_get_i64(content, 0_int64) == 42_int64, 'pipelined add sum == 42')

   ! Settle the bootstrap capability and call the import directly.
   call rpc_wait(conn, bootcap%id, err)
   call check_(err == CAPNP_OK, 'bootstrap returned')
   call rpc_result_cap(conn, bootcap%id, [integer ::], adder, err)
   call check_(err == CAPNP_OK .and. adder%kind == RPC_CAP_IMPORT, 'bootstrap cap settles')

   call rpc_call_begin(conn, adder, ADDER_IFACE, 0, m, params, q2, err)
   s = capnp_new_struct(m, 2, 0, err)
   call capnp_set_i64(s, 0_int64, 7_int64, err)
   call capnp_set_i64(s, 8_int64, 8_int64, err)
   call payload_content_set(params, s, err)
   call rpc_call_send(conn, m, err)
   call rpc_wait(conn, q2, err)
   call rpc_result_content(conn, q2, content, err)
   call check_(err == CAPNP_OK .and. capnp_get_i64(content, 0_int64) == 15_int64, &
               'settled add sum == 15')

   call rpc_finish_send(conn, q1, .false., err)
   call rpc_finish_send(conn, q2, .false., err)
   call rpc_finish_send(conn, bootcap%id, .true., err)
   call rpc_conn_close(conn)

   if (nfail > 0) then
      print '(a,i0,a)', 'FAILED: ', nfail, ' assertion(s)'
      error stop 1
   end if
   print '(a)', 'All rpc interop assertions passed.'

contains

   subroutine check_(cond, name)
      logical, intent(in) :: cond
      character(len=*), intent(in) :: name
      if (.not. cond) then
         nfail = nfail + 1
         print '(a,a)', 'FAIL: ', name
      end if
   end subroutine check_

end program rpc_client
