!> RPC interop client: connect to the capnp-C++ Adder server, bootstrap,
!> call add() both pipelined (before the bootstrap return settles) and on
!> the settled import. Exits nonzero on any mismatch.
program rpc_client
   use capnp
   use rpc_capnp
   use capnp_posix
   use capnp_rpc
   use capnp_rpc_transport, only: rpc_recv_message
   implicit none

   ! Signed container for interface id 0xea01e10cbc414411 (adder.capnp).
   integer(int64), parameter :: ADDER_IFACE = -1584738149043452911_int64

   type(rpc_conn_t), target :: conn
   class(rpc_server_t), pointer :: nosrv
   type(rpc_cap_t) :: bootcap, adder
   type(capnp_message_t), target :: m, reply
   type(message_t) :: rmsg
   type(bootstrap_t) :: bs
   type(return_t) :: ret
   type(payload_t) :: params
   type(capnp_ptr_t) :: s, content
   integer(int8), allocatable :: wire(:), frame511(:)
   integer(int8) :: oversize_hdr(0:7)
   integer(int64) :: header_bytes, seg0_bytes, seg0_words
   character(len=16) :: arg
   integer(int64) :: q1, q2, qcancel, qcount
   integer :: port, fd, fd2, fd3, err, i, nfail

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

   ! A C++ method marked allowCancellation never completes by itself.
   ! Finish must cancel it, and the next ordered call observes that the
   ! server-side promise was dropped.
   call rpc_call_begin(conn, adder, ADDER_IFACE, 1, m, params, qcancel, err)
   s = capnp_new_struct(m, 0, 0, err)
   call payload_content_set(params, s, err)
   call rpc_call_send(conn, m, err)
   call rpc_request_cancellation(conn, qcancel, err)
   call check_(err == CAPNP_OK, 'cancellation requested')

   call rpc_call_begin(conn, adder, ADDER_IFACE, 2, m, params, qcount, err)
   s = capnp_new_struct(m, 0, 0, err)
   call payload_content_set(params, s, err)
   call rpc_call_send(conn, m, err)
   call rpc_wait(conn, qcount, err)
   call rpc_result_content(conn, qcount, content, err)
   call check_(err == CAPNP_OK .and. capnp_get_u32(content, 0_int64) == 1, &
               'C++ peer canceled the opted-in request')

   call rpc_finish_send(conn, q1, .false., err)
   call rpc_finish_send(conn, q2, .false., err)
   call rpc_finish_send(conn, qcount, .false., err)
   call rpc_finish_send(conn, bootcap%id, .true., err)
   call rpc_conn_close(conn)

   ! 511-segment Bootstrap frame accepted by the Cap'n Proto C++ 1.4.0 peer.
   call px_tcp_connect(port, fd2, err)
   call check_(err == CAPNP_OK, '511-seg interop connected')
   if (err == CAPNP_OK) then
      call capnp_message_init_builder(m, err)
      rmsg = message_new_root(m, err)
      bs = message_bootstrap_init(rmsg, err)
      call bootstrap_question_id_set(bs, 99_int64, err)
      call capnp_serialize_bytes(m, wire, err)
      call capnp_message_free(m)
      header_bytes = 2048_int64
      seg0_bytes = size(wire, kind=int64) - 8_int64
      seg0_words = seg0_bytes/CAPNP_WORD_BYTES
      allocate (frame511(0:header_bytes + seg0_bytes - 1_int64))
      frame511 = 0_int8
      call cp_put_i32(frame511, 0_int64, 510_int32)
      call cp_put_i32(frame511, 4_int64, int(seg0_words, int32))
      frame511(header_bytes:header_bytes + seg0_bytes - 1_int64) = &
         wire(8:8 + seg0_bytes - 1_int64)
      call px_send_all(fd2, frame511, err)
      call check_(err == CAPNP_OK, '511-seg bootstrap sent')
      call rpc_recv_message(fd2, reply, err)
      call check_(err == CAPNP_OK, '511-seg bootstrap reply received')
      if (err == CAPNP_OK) then
         rmsg = message_read_root(reply, err)
         call check_(err == CAPNP_OK .and. message_which(rmsg) == MESSAGE_RETURN_TAG, &
                     '511-seg bootstrap reply is Return')
         if (err == CAPNP_OK .and. message_which(rmsg) == MESSAGE_RETURN_TAG) then
            ret = message_return_get(rmsg, err)
            call check_(err == CAPNP_OK .and. return_answer_id_get(ret) == 99_int64, &
                        '511-seg bootstrap answerId == 99')
         end if
      end if
      call capnp_message_free(reply)
      if (allocated(wire)) deallocate (wire)
      if (allocated(frame511)) deallocate (frame511)
      call px_close(fd2)
   end if

   ! Oversized header (> traversalLimitInWords) rejected by the C++ 1.4.0 peer
   ! before reading a body (peer responds with Abort or closes the stream).
   call px_tcp_connect(port, fd3, err)
   call check_(err == CAPNP_OK, 'oversized interop connected')
   if (err == CAPNP_OK) then
      oversize_hdr = 0_int8
      call cp_put_i32(oversize_hdr, 0_int64, 0_int32)
      call cp_put_i32(oversize_hdr, 4_int64, &
                      int(CAPNP_DEFAULT_TRAVERSAL_WORDS + 1_int64, int32))
      call px_send_all(fd3, oversize_hdr, err)
      call check_(err == CAPNP_OK, 'oversized header sent')
      call rpc_recv_message(fd3, reply, err)
      if (err == CAPNP_OK) then
         rmsg = message_read_root(reply, err)
         call check_(err == CAPNP_OK .and. message_which(rmsg) == MESSAGE_ABORT_TAG, &
                     'oversized header rejected with Abort')
      else
         call check_(err == CAPNP_ERR_IO, 'oversized header rejected with disconnect')
      end if
      call capnp_message_free(reply)
      call px_close(fd3)
   end if

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
