!> Owned servers staged into a call that fails before the cap table
!> flushes. A finalizer counts each release. One object is released
!> once: on the failed return, or later with the export that already
!> owned it.
module test_rpc_lifetime
   use testdrive, only: unittest_type, error_type, check
   use capnp
   use rpc_capnp
   use capnp_posix
   use capnp_rpc
   use capnp_rpc_transport
   implicit none
   private

   public :: collect_rpc_lifetime

   integer(int64), parameter :: LIFE_IFACE = 424242_int64

   type, extends(rpc_server_t) :: counted_srv_t
      integer :: token = 0
   contains
      procedure :: dispatch => counted_dispatch
      final :: counted_final
   end type counted_srv_t

   type, extends(rpc_server_t) :: life_srv_t
   contains
      procedure :: dispatch => life_dispatch
   end type life_srv_t

   integer :: final_count = 0
   integer :: next_token = 0
   integer :: inside_deaths = -1
   integer :: touched = 0
   integer :: pipeline_log(2) = 0
   integer :: pipeline_count = 0
   type(counted_srv_t), pointer :: held => null()
   type(rpc_conn_t), pointer :: gsrv => null()

contains

   subroutine collect_rpc_lifetime(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      allocate (testsuite(21))
      testsuite(1)%name = "staged owned server is released after failure"
      testsuite(1)%test => run_fail_one
      testsuite(2)%name = "server staged twice is released once"
      testsuite(2)%test => run_fail_twice
      testsuite(3)%name = "exported server staged again is released once"
      testsuite(3)%test => run_fail_exported
      testsuite(4)%name = "owned server outlives close in its dispatch"
      testsuite(4)%test => run_close_in_dispatch
      testsuite(5)%name = "owned server outlives a release during dispatch"
      testsuite(5)%test => run_release_during_dispatch
      testsuite(6)%name = "full export table returns an exception"
      testsuite(6)%test => run_export_table_full
      testsuite(7)%name = "partial flush releases both staged servers"
      testsuite(7)%test => run_partial_flush
      testsuite(8)%name = "failed flush keeps a reused export"
      testsuite(8)%test => run_partial_reused
      testsuite(9)%name = "closed tail dispatch drops staged owned server"
      testsuite(9)%test => run_closed_tail_dispatch
      testsuite(10)%name = "local dispatch failure releases staged server"
      testsuite(10)%test => run_local_dispatch_failure
      testsuite(11)%name = "local flush failure releases staged server"
      testsuite(11)%test => run_local_flush_failure
      testsuite(12)%name = "local chained pipelines preserve call order"
      testsuite(12)%test => run_local_pipeline_order
      testsuite(13)%name = "closed pipeline dispatch frees queued calls"
      testsuite(13)%test => run_pipeline_close
      testsuite(14)%name = "canceled local pipeline call does not run"
      testsuite(14)%test => run_cancel_pipeline
      testsuite(15)%name = "canceled local call releases its result"
      testsuite(15)%test => run_cancel_returned
      testsuite(16)%name = "owned handoff outlives release before accept"
      testsuite(16)%test => run_handoff_release_before
      testsuite(17)%name = "owned handoff outlives release after accept"
      testsuite(17)%test => run_handoff_release_after
      testsuite(18)%name = "provide with an invalid import aborts"
      testsuite(18)%test => run_provide_invalid_import
      testsuite(19)%name = "yourself call to a missing answer is held"
      testsuite(19)%test => run_yourself_missing_answer
      testsuite(20)%name = "owned handoff outlives release by the recipient"
      testsuite(20)%test => run_handoff_recipient_first
      testsuite(21)%name = "over-release cannot consume a provision pin"
      testsuite(21)%test => run_provision_over_release
   end subroutine collect_rpc_lifetime

   subroutine counted_final(self)
      type(counted_srv_t), intent(inout) :: self
      final_count = final_count + 1
      if (associated(held)) then
         if (held%token == self%token) held => null()
      end if
   end subroutine counted_final

   subroutine counted_dispatch(self, ctx, err)
      class(counted_srv_t), intent(inout) :: self
      type(rpc_call_ctx_t), intent(inout) :: ctx
      integer, intent(out) :: err
      integer :: e2
      err = CAPNP_OK
      if (ctx%method_id == 11) then
         if (associated(gsrv)) call rpc_conn_close(gsrv)
         inside_deaths = final_count
         touched = self%token
      else if (ctx%method_id == 10) then
         if (associated(gsrv)) call rpc_pump_once(gsrv, e2)
         inside_deaths = final_count
         touched = self%token
      else if (self%token < 0 .or. ctx%method_id < 0) then
         err = CAPNP_ERR_ARG
      else if (ctx%method_id >= 20 .and. ctx%method_id <= 30) then
         if (pipeline_count < size(pipeline_log)) then
            pipeline_count = pipeline_count + 1
            pipeline_log(pipeline_count) = ctx%method_id
         end if
      end if
   end subroutine counted_dispatch

   subroutine life_dispatch(self, ctx, err)
      class(life_srv_t), intent(inout) :: self
      type(rpc_call_ctx_t), intent(inout) :: ctx
      integer, intent(out) :: err
      type(counted_srv_t), pointer :: obj, obj2
      class(rpc_server_t), pointer :: sp
      type(capnp_ptr_t) :: s
      integer :: idx, idx1
      err = CAPNP_OK
      associate (server => self)
      end associate
      if (ctx%interface_id /= LIFE_IFACE) then
         err = CAPNP_ERR_ARG
         return
      end if
      select case (ctx%method_id)
      case (1, 2)
         allocate (obj)
         next_token = next_token + 1
         obj%token = next_token
         sp => obj
         idx = rpc_ctx_export_cap(ctx, sp, err, owned=.true.)
         if (err /= CAPNP_OK .or. idx < 0) then
            deallocate (obj)
            err = CAPNP_ERR_ALLOC
            return
         end if
         if (ctx%method_id == 2) then
            idx = rpc_ctx_export_cap(ctx, sp, err, owned=.true.)
            if (err /= CAPNP_OK .or. idx < 0) return
         end if
         err = CAPNP_ERR_ARG
      case (3)
         allocate (held)
         next_token = next_token + 1
         held%token = next_token
         sp => held
         idx = rpc_ctx_export_cap(ctx, sp, err, owned=.true.)
         if (err /= CAPNP_OK) then
            deallocate (held)
            return
         end if
         s = capnp_new_struct(ctx%rmsg, 0, 1, err)
         if (err /= CAPNP_OK) return
         call capnp_setp(s, 0, rpc_make_cap_ptr(ctx%rmsg, idx), err)
         if (err /= CAPNP_OK) return
         call payload_content_set(ctx%results, s, err)
      case (4)
         if (.not. associated(held)) then
            err = CAPNP_ERR_ARG
            return
         end if
         sp => held
         idx = rpc_ctx_export_cap(ctx, sp, err, owned=.true.)
         if (err /= CAPNP_OK .or. idx < 0) return
         err = CAPNP_ERR_ARG
      case (7)
         allocate (obj)
         next_token = next_token + 1
         obj%token = next_token
         sp => obj
         idx = rpc_ctx_export_cap(ctx, sp, err, owned=.true.)
         if (err /= CAPNP_OK .or. idx < 0) then
            deallocate (obj)
            err = CAPNP_ERR_ALLOC
            return
         end if
         s = capnp_new_struct(ctx%rmsg, 0, 1, err)
         if (err /= CAPNP_OK) return
         call capnp_setp(s, 0, rpc_make_cap_ptr(ctx%rmsg, idx), err)
         if (err /= CAPNP_OK) return
         call payload_content_set(ctx%results, s, err)
      case (8)
         ! Two distinct owned servers, returned on the success path so
         ! the cap table is flushed.
         allocate (obj)
         next_token = next_token + 1
         obj%token = next_token
         sp => obj
         idx1 = rpc_ctx_export_cap(ctx, sp, err, owned=.true.)
         if (err /= CAPNP_OK .or. idx1 < 0) then
            deallocate (obj)
            err = CAPNP_ERR_ALLOC
            return
         end if
         allocate (obj2)
         next_token = next_token + 1
         obj2%token = next_token
         sp => obj2
         idx = rpc_ctx_export_cap(ctx, sp, err, owned=.true.)
         if (err /= CAPNP_OK .or. idx < 0) then
            deallocate (obj2)
            err = CAPNP_ERR_ALLOC
            return
         end if
         s = capnp_new_struct(ctx%rmsg, 0, 2, err)
         if (err /= CAPNP_OK) return
         call capnp_setp(s, 0, rpc_make_cap_ptr(ctx%rmsg, idx1), err)
         if (err /= CAPNP_OK) return
         call capnp_setp(s, 1, rpc_make_cap_ptr(ctx%rmsg, idx), err)
         if (err /= CAPNP_OK) return
         call payload_content_set(ctx%results, s, err)
      case (9)
         ! A new owned server, then the server already exported in
         ! `held`, then another new owned server.
         if (.not. associated(held)) then
            err = CAPNP_ERR_ARG
            return
         end if
         allocate (obj)
         next_token = next_token + 1
         obj%token = next_token
         sp => obj
         idx1 = rpc_ctx_export_cap(ctx, sp, err, owned=.true.)
         if (err /= CAPNP_OK .or. idx1 < 0) then
            deallocate (obj)
            err = CAPNP_ERR_ALLOC
            return
         end if
         sp => held
         idx = rpc_ctx_export_cap(ctx, sp, err, owned=.true.)
         if (err /= CAPNP_OK .or. idx < 0) return
         allocate (obj2)
         next_token = next_token + 1
         obj2%token = next_token
         sp => obj2
         idx = rpc_ctx_export_cap(ctx, sp, err, owned=.true.)
         if (err /= CAPNP_OK .or. idx < 0) then
            deallocate (obj2)
            err = CAPNP_ERR_ALLOC
            return
         end if
         s = capnp_new_struct(ctx%rmsg, 0, 1, err)
         if (err /= CAPNP_OK) return
         call capnp_setp(s, 0, rpc_make_cap_ptr(ctx%rmsg, idx1), err)
         if (err /= CAPNP_OK) return
         call payload_content_set(ctx%results, s, err)
      case (12)
         allocate (obj)
         next_token = next_token + 1
         obj%token = next_token
         sp => obj
         idx = rpc_ctx_export_cap(ctx, sp, err, owned=.true.)
         if (err /= CAPNP_OK .or. idx < 0) then
            deallocate (obj)
            err = CAPNP_ERR_ALLOC
            return
         end if
         s = capnp_new_struct(ctx%rmsg, 0, 1, err)
         if (err /= CAPNP_OK) return
         call capnp_setp(s, 0, rpc_make_cap_ptr(ctx%rmsg, idx), err)
         if (err /= CAPNP_OK) return
         call payload_content_set(ctx%results, s, err)
         if (associated(gsrv)) call rpc_conn_close(gsrv)
      case default
         err = CAPNP_ERR_ARG
      end select
   end subroutine life_dispatch

   subroutine check_(error, cond, name)
      type(error_type), allocatable, intent(inout) :: error
      logical, intent(in) :: cond
      character(len=*), intent(in) :: name
      if (allocated(error)) return
      call check(error, cond, name)
   end subroutine check_

   subroutine open_pair(cli, srv, factory, settled, opened, error)
      type(rpc_conn_t), intent(inout), target :: cli, srv
      type(life_srv_t), intent(in), target :: factory
      type(rpc_cap_t), intent(out) :: settled
      logical, intent(out) :: opened
      type(error_type), allocatable, intent(inout) :: error
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: pipe
      integer :: fda, fdb, err
      opened = .false.
      final_count = 0
      held => null()
      settled = rpc_cap_t()
      call px_socketpair(fda, fdb, err)
      call check_(error, err == CAPNP_OK, 'life: socketpair')
      if (allocated(error)) return
      boot => factory
      call rpc_conn_init(srv, fdb, boot)
      boot => null()
      call rpc_conn_init(cli, fda, boot)
      opened = .true.
      call rpc_bootstrap_send(cli, pipe, err)
      call check_(error, err == CAPNP_OK, 'life: bootstrap sent')
      if (allocated(error)) return
      call rpc_pump_once(srv, err)
      call rpc_wait(cli, pipe%id, err)
      call rpc_result_cap(cli, pipe%id, [integer ::], settled, err)
      call check_(error, err == CAPNP_OK, 'life: bootstrap settled')
   end subroutine open_pair

   subroutine call_method(cli, srv, settled, method, err, qid_out)
      type(rpc_conn_t), intent(inout), target :: cli, srv
      type(rpc_cap_t), intent(in) :: settled
      integer, intent(in) :: method
      integer, intent(out) :: err
      integer(int64), intent(out), optional :: qid_out
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(capnp_ptr_t) :: s, content
      integer(int64) :: qid
      qid = -1_int64
      call rpc_call_begin(cli, settled, LIFE_IFACE, method, m, params, qid, err)
      if (present(qid_out)) qid_out = qid
      if (err /= CAPNP_OK) return
      s = capnp_new_struct(m, 0, 0, err)
      if (err /= CAPNP_OK) return
      call payload_content_set(params, s, err)
      if (err /= CAPNP_OK) return
      call rpc_call_send(cli, m, err)
      if (err /= CAPNP_OK) return
      call rpc_pump_once(srv, err)
      if (err /= CAPNP_OK) return
      call rpc_wait(cli, qid, err)
      if (err /= CAPNP_OK) return
      call rpc_result_content(cli, qid, content, err)
   end subroutine call_method

   subroutine close_pair(cli, srv, opened)
      type(rpc_conn_t), intent(inout) :: cli, srv
      logical, intent(in) :: opened
      if (.not. opened) return
      call rpc_conn_close(srv)
      call rpc_conn_close(cli)
   end subroutine close_pair

   subroutine run_fail_one(error)
      type(error_type), allocatable, intent(out) :: error
      type(life_srv_t), target :: factory
      type(rpc_conn_t), target :: cli, srv
      type(rpc_cap_t) :: settled
      logical :: opened
      integer :: err
      call open_pair(cli, srv, factory, settled, opened, error)
      if (.not. allocated(error)) then
         final_count = 0
         call call_method(cli, srv, settled, 1, err)
         call check_(error, err == RPC_ERR_EXCEPTION, &
                     'staged owned server is released after failure: exception')
         call check_(error, final_count == 1, &
                     'staged owned server is released after failure')
      end if
      call close_pair(cli, srv, opened)
      if (opened) call check_(error, final_count == 1, &
                              'staged owned server is released after failure: close')
   end subroutine run_fail_one

   subroutine run_fail_twice(error)
      type(error_type), allocatable, intent(out) :: error
      type(life_srv_t), target :: factory
      type(rpc_conn_t), target :: cli, srv
      type(rpc_cap_t) :: settled
      logical :: opened
      integer :: err
      call open_pair(cli, srv, factory, settled, opened, error)
      if (.not. allocated(error)) then
         final_count = 0
         call call_method(cli, srv, settled, 2, err)
         call check_(error, err == RPC_ERR_EXCEPTION, &
                     'server staged twice is released once: exception')
         call check_(error, final_count == 1, &
                     'server staged twice is released once')
      end if
      call close_pair(cli, srv, opened)
      if (opened) call check_(error, final_count == 1, &
                              'server staged twice is released once: close')
   end subroutine run_fail_twice

   subroutine run_fail_exported(error)
      type(error_type), allocatable, intent(out) :: error
      type(life_srv_t), target :: factory
      type(rpc_conn_t), target :: cli, srv
      type(rpc_cap_t) :: settled
      logical :: opened
      integer :: err
      call open_pair(cli, srv, factory, settled, opened, error)
      if (.not. allocated(error)) then
         final_count = 0
         call call_method(cli, srv, settled, 3, err)
         call check_(error, err == CAPNP_OK, &
                     'exported server staged again is released once: export')
         call check_(error, final_count == 0, &
                     'exported server staged again is released once: still live')
      end if
      if (.not. allocated(error)) then
         call call_method(cli, srv, settled, 4, err)
         call check_(error, err == RPC_ERR_EXCEPTION, &
                     'exported server staged again is released once: exception')
         call check_(error, final_count == 0, &
                     'exported server staged again is released once: not yet')
      end if
      call close_pair(cli, srv, opened)
      if (opened .and. .not. allocated(error)) &
         call check_(error, final_count == 1, &
                     'exported server staged again is released once')
   end subroutine run_fail_exported

   !> Close from inside dispatch. Deaths are 0 until the call returns, then 1.
   subroutine run_close_in_dispatch(error)
      type(error_type), allocatable, intent(out) :: error
      type(life_srv_t), target :: factory
      type(rpc_conn_t), target :: cli, srv
      type(rpc_cap_t) :: settled, cap
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(capnp_ptr_t) :: s
      logical :: opened
      integer(int64) :: qid, export_qid
      integer :: err, tok, e2
      gsrv => null()
      call open_pair(cli, srv, factory, settled, opened, error)
      if (.not. allocated(error)) then
         gsrv => srv
         final_count = 0
         inside_deaths = -1
         touched = 0
         call call_method(cli, srv, settled, 3, err, export_qid)
         call check_(error, err == CAPNP_OK, 'close-in-dispatch: export')
         if (err == CAPNP_OK) call rpc_result_cap(cli, export_qid, [0], cap, err)
         call check_(error, err == CAPNP_OK .and. associated(held), &
                     'close-in-dispatch: cap settled')
      end if
      if (.not. allocated(error)) then
         tok = held%token
         call rpc_finish_send(cli, export_qid, .true., e2)
         call rpc_pump_once(srv, e2)
         call rpc_call_begin(cli, cap, LIFE_IFACE, 11, m, params, qid, err)
         if (err == CAPNP_OK) s = capnp_new_struct(m, 0, 0, err)
         if (err == CAPNP_OK) call payload_content_set(params, s, err)
         if (err == CAPNP_OK) call rpc_call_send(cli, m, err)
         if (err == CAPNP_OK) call rpc_pump_once(srv, err)
         call check_(error, inside_deaths == 0, &
                     'close-in-dispatch: no death inside the call')
         call check_(error, touched == tok, &
                     'close-in-dispatch: server readable inside the call')
         call check_(error, final_count == 1, &
                     'close-in-dispatch: one death after the call')
      end if
      gsrv => null()
      call close_pair(cli, srv, opened)
      if (opened) call check_(error, final_count == 1, &
                              'close-in-dispatch: second close frees nothing')
   end subroutine run_close_in_dispatch

   !> A Release pumped from the handler must not free the server until
   !> the handler returns.
   subroutine run_release_during_dispatch(error)
      type(error_type), allocatable, intent(out) :: error
      type(life_srv_t), target :: factory
      type(rpc_conn_t), target :: cli, srv
      type(rpc_cap_t) :: settled, cap
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(capnp_ptr_t) :: s, content
      logical :: opened
      integer(int64) :: qid, export_qid
      integer :: err, tok, e2
      gsrv => null()
      call open_pair(cli, srv, factory, settled, opened, error)
      if (.not. allocated(error)) then
         gsrv => srv
         final_count = 0
         inside_deaths = -1
         touched = 0
         call call_method(cli, srv, settled, 3, err, export_qid)
         call check_(error, err == CAPNP_OK, 'release-in-dispatch: export')
         if (err == CAPNP_OK) call rpc_result_cap(cli, export_qid, [0], cap, err)
         call check_(error, err == CAPNP_OK .and. associated(held), &
                     'release-in-dispatch: cap settled')
      end if
      if (.not. allocated(error)) then
         tok = held%token
         call rpc_finish_send(cli, export_qid, .true., e2)
         call rpc_pump_once(srv, e2)
         call rpc_call_begin(cli, cap, LIFE_IFACE, 10, m, params, qid, err)
         if (err == CAPNP_OK) s = capnp_new_struct(m, 0, 0, err)
         if (err == CAPNP_OK) call payload_content_set(params, s, err)
         if (err == CAPNP_OK) call rpc_call_send(cli, m, err)
         if (err == CAPNP_OK) call rpc_release_send(cli, cap, err)
         if (err == CAPNP_OK) call rpc_pump_once(srv, err)
         call check_(error, inside_deaths == 0, &
                     'release-in-dispatch: no death inside the call')
         call check_(error, touched == tok, &
                     'release-in-dispatch: server readable inside the call')
         call check_(error, final_count == 1, &
                     'release-in-dispatch: one death after the call')
         if (err == CAPNP_OK) call rpc_wait(cli, qid, err)
         if (err == CAPNP_OK) call rpc_result_content(cli, qid, content, err)
         call check_(error, err == CAPNP_OK, 'release-in-dispatch: call returned')
      end if
      gsrv => null()
      call close_pair(cli, srv, opened)
      if (opened) call check_(error, final_count == 1, &
                              'release-in-dispatch: close frees nothing more')
   end subroutine run_release_during_dispatch

   subroutine run_closed_tail_dispatch(error)
      type(error_type), allocatable, intent(out) :: error
      type(life_srv_t), target :: factory
      type(rpc_conn_t), target :: cli, srv
      type(rpc_cap_t) :: settled
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(call_t) :: c
      type(payload_t) :: params
      type(capnp_ptr_t) :: s
      logical :: opened
      integer(int64) :: qid
      integer :: err
      gsrv => null()
      call open_pair(cli, srv, factory, settled, opened, error)
      if (.not. allocated(error)) then
         gsrv => srv
         final_count = 0
         call rpc_call_begin(cli, settled, LIFE_IFACE, 12, m, params, qid, err)
         if (err == CAPNP_OK) then
            msg = message_read_root(m, err)
            c = message_call_get(msg, err)
            if (err == CAPNP_OK) call call_send_results_to_yourself_set(c, err)
         end if
         if (err == CAPNP_OK) s = capnp_new_struct(m, 0, 0, err)
         if (err == CAPNP_OK) call payload_content_set(params, s, err)
         if (err == CAPNP_OK) call rpc_call_send(cli, m, err)
         if (err == CAPNP_OK) call rpc_pump_once(srv, err)
         call check_(error, srv%dead .and. final_count == 1, &
                     'closed tail dispatch drops staged owned server')
      end if
      gsrv => null()
      call close_pair(cli, srv, opened)
   end subroutine run_closed_tail_dispatch

   subroutine run_local_dispatch_failure(error)
      type(error_type), allocatable, intent(out) :: error
      type(life_srv_t), target :: factory
      type(rpc_conn_t), target :: cli, srv
      type(rpc_cap_t) :: settled, local
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(capnp_ptr_t) :: s
      logical :: opened
      integer(int64) :: qid
      integer :: err
      call open_pair(cli, srv, factory, settled, opened, error)
      if (.not. allocated(error)) then
         local%kind = RPC_CAP_LOCAL
         local%id = 0_int64
         final_count = 0
         call rpc_call_begin(srv, local, LIFE_IFACE, 1, m, params, qid, err)
         if (err == CAPNP_OK) s = capnp_new_struct(m, 0, 0, err)
         if (err == CAPNP_OK) call payload_content_set(params, s, err)
         if (err == CAPNP_OK) call rpc_call_send(srv, m, err)
         call check_(error, err == CAPNP_OK .and. final_count == 1, &
                     'local dispatch failure releases staged server')
      end if
      call close_pair(cli, srv, opened)
   end subroutine run_local_dispatch_failure

   subroutine run_local_flush_failure(error)
      type(error_type), allocatable, intent(out) :: error
      type(life_srv_t), target :: factory
      type(rpc_conn_t), target :: cli, srv
      type(rpc_cap_t) :: settled, local
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(capnp_ptr_t) :: s
      logical :: opened
      integer(int64) :: qid
      integer :: err, i
      call open_pair(cli, srv, factory, settled, opened, error)
      if (.not. allocated(error)) then
         do i = 1, size(srv%exports) - 1
            srv%exports(i)%used = .true.
            srv%exports(i)%refcount = 1
            srv%exports(i)%srv => factory
         end do
         local%kind = RPC_CAP_LOCAL
         local%id = 0_int64
         final_count = 0
         call rpc_call_begin(srv, local, LIFE_IFACE, 7, m, params, qid, err)
         if (err == CAPNP_OK) s = capnp_new_struct(m, 0, 0, err)
         if (err == CAPNP_OK) call payload_content_set(params, s, err)
         if (err == CAPNP_OK) call rpc_call_send(srv, m, err)
         call check_(error, err == CAPNP_OK .and. final_count == 1, &
                     'local flush failure releases staged server')
      end if
      call close_pair(cli, srv, opened)
   end subroutine run_local_flush_failure

   subroutine run_local_pipeline_order(error)
      type(error_type), allocatable, intent(out) :: error
      type(life_srv_t), target :: factory
      type(rpc_conn_t), target :: cli, srv
      type(rpc_cap_t) :: settled, local, pipeline
      type(capnp_message_t), target :: m1, m2, m3
      type(payload_t) :: p1, p2, p3
      type(capnp_ptr_t) :: s
      logical :: opened
      integer(int64) :: q1, q2, q3
      integer :: err
      call open_pair(cli, srv, factory, settled, opened, error)
      if (.not. allocated(error)) then
         local%kind = RPC_CAP_LOCAL
         local%id = 0_int64
         pipeline_count = 0
         pipeline_log = 0
         call rpc_call_begin(srv, local, LIFE_IFACE, 3, m1, p1, q1, err)
         if (err == CAPNP_OK) s = capnp_new_struct(m1, 0, 0, err)
         if (err == CAPNP_OK) call payload_content_set(p1, s, err)
         pipeline%kind = RPC_CAP_PIPELINE
         pipeline%id = q1
         pipeline%nops = 1
         pipeline%ops(0) = 0
         if (err == CAPNP_OK) call rpc_call_begin(srv, pipeline, LIFE_IFACE, 20, m2, p2, q2, err)
         if (err == CAPNP_OK) s = capnp_new_struct(m2, 0, 0, err)
         if (err == CAPNP_OK) call payload_content_set(p2, s, err)
         if (err == CAPNP_OK) call rpc_call_send(srv, m2, err)
         if (err == CAPNP_OK) call rpc_call_begin(srv, pipeline, LIFE_IFACE, 30, m3, p3, q3, err)
         if (err == CAPNP_OK) s = capnp_new_struct(m3, 0, 0, err)
         if (err == CAPNP_OK) call payload_content_set(p3, s, err)
         if (err == CAPNP_OK) call rpc_call_send(srv, m3, err)
         if (err == CAPNP_OK) call rpc_call_send(srv, m1, err)
         call check_(error, err == CAPNP_OK .and. pipeline_count == 2 .and. &
                     pipeline_log(1) == 20 .and. pipeline_log(2) == 30, &
                     'local chained pipelines preserve call order')
      end if
      call close_pair(cli, srv, opened)
   end subroutine run_local_pipeline_order

   subroutine build_pipe(srv, m, p, mth, q1, qq, err)
      type(rpc_conn_t), intent(inout), target :: srv
      type(capnp_message_t), intent(inout), target :: m
      type(payload_t), intent(out) :: p
      integer, intent(in) :: mth
      integer(int64), intent(in) :: q1
      integer(int64), intent(out) :: qq
      integer, intent(out) :: err
      type(rpc_cap_t) :: pipeline
      type(capnp_ptr_t) :: s
      pipeline%kind = RPC_CAP_PIPELINE
      pipeline%id = q1
      pipeline%nops = 1
      pipeline%ops(0) = 0
      call rpc_call_begin(srv, pipeline, LIFE_IFACE, mth, m, p, qq, err)
      if (err == CAPNP_OK) s = capnp_new_struct(m, 0, 0, err)
      if (err == CAPNP_OK) call payload_content_set(p, s, err)
   end subroutine build_pipe

   !> The first of two calls waiting on a local result closes the
   !> connection. The call behind it never runs and its queue entry is freed.
   subroutine run_pipeline_close(error)
      type(error_type), allocatable, intent(out) :: error
      type(life_srv_t), target :: factory
      type(rpc_conn_t), target :: cli, srv
      type(rpc_cap_t) :: settled, local
      type(capnp_message_t), target :: m1, m2, m3
      type(payload_t) :: p1, p2, p3
      type(capnp_ptr_t) :: s
      logical :: opened
      integer(int64) :: q1, q2, q3
      integer :: err
      call open_pair(cli, srv, factory, settled, opened, error)
      if (.not. allocated(error)) then
         gsrv => srv
         local%kind = RPC_CAP_LOCAL
         local%id = 0_int64
         pipeline_count = 0
         pipeline_log = 0
         call rpc_call_begin(srv, local, LIFE_IFACE, 3, m1, p1, q1, err)
         if (err == CAPNP_OK) s = capnp_new_struct(m1, 0, 0, err)
         if (err == CAPNP_OK) call payload_content_set(p1, s, err)
         if (err == CAPNP_OK) call build_pipe(srv, m2, p2, 11, q1, q2, err)
         if (err == CAPNP_OK) call rpc_call_send(srv, m2, err)
         if (err == CAPNP_OK) call build_pipe(srv, m3, p3, 20, q1, q3, err)
         if (err == CAPNP_OK) call rpc_call_send(srv, m3, err)
         if (err == CAPNP_OK) call rpc_call_send(srv, m1, err)
         call check_(error, err == RPC_ERR_DEAD .and. pipeline_count == 0 .and. &
                     .not. rpc_conn_alive(srv), &
                     'closed pipeline dispatch frees queued calls')
         gsrv => null()
      end if
      call close_pair(cli, srv, opened)
   end subroutine run_pipeline_close

   !> A pipelined call canceled while it waits for its target question is
   !> dropped. It must not run later or write a Return into a freed slot.
   subroutine run_cancel_pipeline(error)
      type(error_type), allocatable, intent(out) :: error
      type(life_srv_t), target :: factory
      type(rpc_conn_t), target :: cli, srv
      type(rpc_cap_t) :: settled, local
      type(capnp_message_t), target :: m1, m2
      type(payload_t) :: p1, p2
      type(capnp_ptr_t) :: s
      logical :: opened
      integer(int64) :: q1, q2
      integer :: err, cerr
      call open_pair(cli, srv, factory, settled, opened, error)
      if (.not. allocated(error)) then
         local%kind = RPC_CAP_LOCAL
         local%id = 0_int64
         pipeline_count = 0
         pipeline_log = 0
         call rpc_call_begin(srv, local, LIFE_IFACE, 3, m1, p1, q1, err)
         if (err == CAPNP_OK) s = capnp_new_struct(m1, 0, 0, err)
         if (err == CAPNP_OK) call payload_content_set(p1, s, err)
         if (err == CAPNP_OK) call build_pipe(srv, m2, p2, 20, q1, q2, err)
         if (err == CAPNP_OK) call rpc_call_send(srv, m2, err)
         cerr = CAPNP_ERR_ARG
         if (err == CAPNP_OK) call rpc_request_cancellation(srv, q2, cerr)
         if (err == CAPNP_OK) call rpc_call_send(srv, m1, err)
         call check_(error, err == CAPNP_OK .and. cerr == CAPNP_OK .and. &
                     pipeline_count == 0 .and. &
                     .not. srv%questions(int(q2))%used .and. &
                     .not. srv%questions(int(q2))%returned, &
                     'canceled local pipeline call does not run')
      end if
      call close_pair(cli, srv, opened)
   end subroutine run_cancel_pipeline

   !> Canceling a local call that already returned drops its result
   !> exports, as Finish with releaseResultCaps does.
   subroutine run_cancel_returned(error)
      type(error_type), allocatable, intent(out) :: error
      type(life_srv_t), target :: factory
      type(rpc_conn_t), target :: cli, srv
      type(rpc_cap_t) :: settled, local
      type(capnp_message_t), target :: m1
      type(payload_t) :: p1
      type(capnp_ptr_t) :: s
      logical :: opened
      integer(int64) :: q1
      integer :: err, cerr
      call open_pair(cli, srv, factory, settled, opened, error)
      if (.not. allocated(error)) then
         local%kind = RPC_CAP_LOCAL
         local%id = 0_int64
         call rpc_call_begin(srv, local, LIFE_IFACE, 7, m1, p1, q1, err)
         if (err == CAPNP_OK) s = capnp_new_struct(m1, 0, 0, err)
         if (err == CAPNP_OK) call payload_content_set(p1, s, err)
         if (err == CAPNP_OK) call rpc_call_send(srv, m1, err)
         call check_(error, err == CAPNP_OK .and. final_count == 0 .and. &
                     srv%questions(int(q1))%returned, &
                     'canceled local call releases its result: returned')
         cerr = CAPNP_ERR_ARG
         if (.not. allocated(error)) call rpc_request_cancellation(srv, q1, cerr)
         call check_(error, cerr == CAPNP_OK .and. final_count == 1, &
                     'canceled local call releases its result')
      end if
      call close_pair(cli, srv, opened)
      if (opened) call check_(error, final_count == 1, &
                              'canceled local call releases its result: close')
   end subroutine run_cancel_returned

   subroutine run_handoff_release_before(error)
      type(error_type), allocatable, intent(out) :: error
      call handoff_owned(.true., .false., error)
   end subroutine run_handoff_release_before

   subroutine run_handoff_release_after(error)
      type(error_type), allocatable, intent(out) :: error
      call handoff_owned(.false., .false., error)
   end subroutine run_handoff_release_after

   !> A peer holding one wire reference to a provided export cannot
   !> release two. The provision pin is not a wire reference, so the
   !> Release aborts the connection and the server stays alive until the
   !> connection closes.
   subroutine run_provision_over_release(error)
      type(error_type), allocatable, intent(out) :: error
      type(rpc_vat_t), target :: vat
      type(life_srv_t), target :: factory
      type(rpc_conn_t), target :: intro, prov_a
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: boot_cap, owned_cap
      type(capnp_message_t), target :: m
      type(message_t) :: msg
      type(release_t) :: rel
      integer(int64) :: qboot, qown, pq
      integer :: ia, pa, err
      final_count = 0
      call px_socketpair(ia, pa, err)
      boot => factory
      call rpc_conn_init(prov_a, pa, boot)
      call rpc_conn_set_vat(prov_a, vat)
      boot => null()
      call rpc_conn_init(intro, ia, boot)
      call rpc_bootstrap_send(intro, boot_cap, err)
      qboot = boot_cap%id
      call rpc_pump_once(prov_a, err)
      call rpc_wait(intro, qboot, err)
      call rpc_result_cap(intro, qboot, [integer ::], boot_cap, err)
      call check_(error, err == CAPNP_OK, 'pin over-release: bootstrap')
      if (.not. allocated(error)) then
         call call_method(intro, prov_a, boot_cap, 7, err, qown)
         call rpc_result_cap(intro, qown, [0], owned_cap, err)
         call check_(error, err == CAPNP_OK .and. final_count == 0, &
                     'pin over-release: owned result exported')
      end if
      if (.not. allocated(error)) then
         call rpc_provide_send(intro, owned_cap%id, '127.0.0.1', 9, 72_int64, pq, err)
         call rpc_pump_once(prov_a, err)
         call rpc_wait(intro, pq, err)
         call check_(error, err == CAPNP_OK, 'pin over-release: provide')
      end if
      if (.not. allocated(error)) then
         call capnp_message_init_builder(m, err)
         msg = message_new_root(m, err)
         rel = message_release_init(msg, err)
         call release_id_set(rel, owned_cap%id, err)
         call release_reference_count_set(rel, 2_int64, err)
         call rpc_send_message(intro%fd, m, err)
         call capnp_message_free(m)
         call rpc_pump_once(prov_a, err)
         call check_(error, .not. rpc_conn_alive(prov_a) .and. final_count == 0, &
                     'pin over-release: aborts and keeps the server')
      end if
      call rpc_conn_close(intro)
      call rpc_conn_close(prov_a)
      if (.not. allocated(error)) call check_(error, final_count == 1, &
                                              'pin over-release: freed once at close')
   end subroutine run_provision_over_release

   subroutine run_handoff_recipient_first(error)
      type(error_type), allocatable, intent(out) :: error
      call handoff_owned(.false., .true., error)
   end subroutine run_handoff_recipient_first

   !> An owned server exported on the introducer's connection is also
   !> exported on the recipient's at Accept. Both exports own it, so it
   !> is freed when the last one is released, in either order and whether
   !> the introducer's Release comes before or after the Accept.
   subroutine handoff_owned(release_first, recipient_first, error)
      logical, intent(in) :: release_first, recipient_first
      type(error_type), allocatable, intent(out) :: error
      type(rpc_vat_t), target :: vat
      type(life_srv_t), target :: factory
      type(rpc_conn_t), target :: intro, prov_a, recip, prov_b
      class(rpc_server_t), pointer :: boot
      type(rpc_cap_t) :: boot_cap, owned_cap, got
      type(capnp_message_t), target :: m
      type(payload_t) :: params
      type(capnp_ptr_t) :: s
      integer(int64) :: qboot, qown, pq, aq, qcall
      integer :: ia, pa, ra, pb, err
      final_count = 0
      call px_socketpair(ia, pa, err)
      call px_socketpair(ra, pb, err)
      boot => factory
      call rpc_conn_init(prov_a, pa, boot)
      call rpc_conn_init(prov_b, pb, boot)
      call rpc_conn_set_vat(prov_a, vat)
      call rpc_conn_set_vat(prov_b, vat)
      boot => null()
      call rpc_conn_init(intro, ia, boot)
      call rpc_conn_init(recip, ra, boot)
      call rpc_bootstrap_send(intro, boot_cap, err)
      qboot = boot_cap%id
      call rpc_pump_once(prov_a, err)
      call rpc_wait(intro, qboot, err)
      call rpc_result_cap(intro, qboot, [integer ::], boot_cap, err)
      call check_(error, err == CAPNP_OK, 'owned handoff: bootstrap')
      if (.not. allocated(error)) then
         call call_method(intro, prov_a, boot_cap, 7, err, qown)
         call rpc_result_cap(intro, qown, [0], owned_cap, err)
         call check_(error, err == CAPNP_OK .and. final_count == 0, &
                     'owned handoff: owned result exported')
      end if
      if (.not. allocated(error)) then
         call rpc_provide_send(intro, owned_cap%id, '127.0.0.1', 9, 71_int64, pq, err)
         call rpc_pump_once(prov_a, err)
         call rpc_wait(intro, pq, err)
         call check_(error, err == CAPNP_OK, 'owned handoff: provide')
      end if
      if (.not. allocated(error) .and. release_first) then
         call rpc_release_send(intro, owned_cap, err)
         call rpc_pump_once(prov_a, err)
         call check_(error, final_count == 0 .and. rpc_conn_alive(prov_a), &
                     'owned handoff: provision outlives the introducer release')
      end if
      if (.not. allocated(error)) then
         call rpc_accept_send(recip, 71_int64, .false., aq, err)
         call rpc_pump_once(prov_b, err)
         call rpc_wait(recip, aq, err)
         call rpc_result_cap(recip, aq, [integer ::], got, err)
         call check_(error, err == CAPNP_OK .and. final_count == 0, &
                     'owned handoff: accept')
      end if
      if (.not. allocated(error) .and. recipient_first) then
         call rpc_release_send(recip, got, err)
         call rpc_pump_once(prov_b, err)
         call check_(error, final_count == 0 .and. rpc_conn_alive(prov_b), &
                     'owned handoff: introducer export outlives the recipient')
         call rpc_call_begin(intro, owned_cap, LIFE_IFACE, 0, m, params, qcall, err)
         if (err == CAPNP_OK) s = capnp_new_struct(m, 0, 0, err)
         if (err == CAPNP_OK) call payload_content_set(params, s, err)
         if (err == CAPNP_OK) call rpc_call_send(intro, m, err)
         call rpc_pump_once(prov_a, err)
         if (err == CAPNP_OK) call rpc_wait(intro, qcall, err)
         call check_(error, err == CAPNP_OK .and. final_count == 0, &
                     'owned handoff: introducer can still call the server')
         if (.not. allocated(error)) then
            call rpc_release_send(intro, owned_cap, err)
            call rpc_pump_once(prov_a, err)
            call check_(error, final_count == 1, &
                        'owned handoff: introducer release frees the server')
         end if
      else if (.not. allocated(error)) then
         if (.not. release_first) then
            call rpc_release_send(intro, owned_cap, err)
            call rpc_pump_once(prov_a, err)
         end if
         call check_(error, final_count == 0 .and. rpc_conn_alive(prov_a), &
                     'owned handoff: recipient export outlives the introducer')
         if (.not. allocated(error)) then
            call rpc_call_begin(recip, got, LIFE_IFACE, 0, m, params, qcall, err)
            if (err == CAPNP_OK) s = capnp_new_struct(m, 0, 0, err)
            if (err == CAPNP_OK) call payload_content_set(params, s, err)
            if (err == CAPNP_OK) call rpc_call_send(recip, m, err)
            call rpc_pump_once(prov_b, err)
            if (err == CAPNP_OK) call rpc_wait(recip, qcall, err)
            call check_(error, err == CAPNP_OK .and. final_count == 0, &
                        'owned handoff: recipient can call the server')
         end if
         if (.not. allocated(error)) then
            call rpc_release_send(recip, got, err)
            call rpc_pump_once(prov_b, err)
            call check_(error, final_count == 1, &
                        'owned handoff: recipient release frees the server')
         end if
      end if
      call rpc_conn_close(intro)
      call rpc_conn_close(recip)
      call rpc_conn_close(prov_a)
      call rpc_conn_close(prov_b)
      if (.not. allocated(error)) call check_(error, final_count == 1, &
                                              'owned handoff: freed exactly once')
   end subroutine handoff_owned

   subroutine run_provide_invalid_import(error)
      type(error_type), allocatable, intent(out) :: error
      type(life_srv_t), target :: factory
      type(rpc_conn_t), target :: cli, srv
      type(rpc_cap_t) :: settled
      logical :: opened
      integer(int64) :: pq
      integer :: err
      call open_pair(cli, srv, factory, settled, opened, error)
      if (.not. allocated(error)) then
         call rpc_provide_send(cli, 9999_int64, '127.0.0.1', 9, 5_int64, pq, err)
         call check_(error, err == CAPNP_OK, 'provide invalid import: sent')
      end if
      if (.not. allocated(error)) then
         call rpc_pump_once(srv, err)
         call check_(error, .not. rpc_conn_alive(srv), &
                     'provide with an invalid import aborts')
      end if
      call close_pair(cli, srv, opened)
   end subroutine run_provide_invalid_import

   !> A sendResultsTo.yourself call whose target answer is missing keeps
   !> its exception for a later takeFromOtherQuestion. Sending it as an
   !> ordinary Return aborts the caller's connection.
   subroutine run_yourself_missing_answer(error)
      type(error_type), allocatable, intent(out) :: error
      type(life_srv_t), target :: factory
      type(rpc_conn_t), target :: cli, srv
      type(rpc_cap_t) :: settled
      type(capnp_message_t), target :: m1, m2
      type(payload_t) :: p1, p2
      type(capnp_ptr_t) :: s
      type(message_t) :: msg
      type(call_t) :: c
      logical :: opened, readable
      integer(int64) :: q1, q2
      integer :: err
      call open_pair(cli, srv, factory, settled, opened, error)
      if (.not. allocated(error)) then
         call rpc_call_begin(cli, settled, LIFE_IFACE, 3, m1, p1, q1, err)
         if (err == CAPNP_OK) call build_pipe(cli, m2, p2, 20, q1, q2, err)
         if (err == CAPNP_OK) msg = message_read_root(m2, err)
         if (err == CAPNP_OK) c = message_call_get(msg, err)
         if (err == CAPNP_OK) call call_send_results_to_yourself_set(c, err)
         if (err == CAPNP_OK) call rpc_call_send(cli, m2, err)
         if (err == CAPNP_OK) call rpc_pump_once(srv, err)
         if (err == CAPNP_OK) call px_poll_in(cli%fd, 50, readable, err)
         call check_(error, err == CAPNP_OK .and. rpc_conn_alive(srv) .and. &
                     .not. readable, &
                     'yourself call to a missing answer is held')
         call capnp_message_free(m1)
      end if
      call close_pair(cli, srv, opened)
   end subroutine run_yourself_missing_answer

   !> With the export table full, the overflow calls are exception Returns
   !> and only the servers that were not exported are freed before close.
   subroutine run_export_table_full(error)
      type(error_type), allocatable, intent(out) :: error
      type(life_srv_t), target :: factory
      type(rpc_conn_t), target :: cli, srv
      type(rpc_cap_t) :: settled
      logical :: opened
      integer :: err, i, nok, nerr, base, e2
      integer(int64) :: qid_done
      base = 0
      e2 = 0
      gsrv => null()
      call open_pair(cli, srv, factory, settled, opened, error)
      if (.not. allocated(error)) then
         final_count = 0
         base = next_token
         nok = 0
         nerr = 0
         do i = 1, 70
            call call_method(cli, srv, settled, 7, err, qid_done)
            if (err == CAPNP_OK) nok = nok + 1
            if (err == RPC_ERR_EXCEPTION) nerr = nerr + 1
            if (qid_done >= 0_int64) call rpc_finish_send(cli, qid_done, .true., e2)
            call rpc_pump_once(srv, e2)
         end do
         call check_(error, nok == 63, 'export table full: 63 calls returned')
         call check_(error, nerr == 7, 'export table full: 7 calls raised')
         call check_(error, final_count == next_token - base - 63, &
                     'export table full: only the unpublished servers died')
         call check_(error, final_count == 7, 'export table full: seven deaths')
      end if
      call close_pair(cli, srv, opened)
      if (opened .and. .not. allocated(error)) &
         call check_(error, final_count == next_token - base, &
                     'export table full: close frees each server once')
   end subroutine run_export_table_full

   !> Fill the export table, then Release one counted server so one slot
   !> is free. nlive is the number of counted servers still exported.
   !> with_held exports `held` first and leaves that export in place.
   subroutine fill_one_free_slot(cli, srv, settled, with_held, victim, &
                                 held_cap, nlive, error)
      type(rpc_conn_t), intent(inout), target :: cli, srv
      type(rpc_cap_t), intent(in) :: settled
      logical, intent(in) :: with_held
      type(rpc_cap_t), intent(out) :: victim, held_cap
      integer, intent(out) :: nlive
      type(error_type), allocatable, intent(inout) :: error
      integer :: err, i, n7, nok, e2, base
      integer(int64) :: qid
      nlive = 0
      victim = rpc_cap_t()
      held_cap = rpc_cap_t()
      final_count = 0
      base = next_token
      nok = 0
      e2 = 0
      n7 = 63
      if (with_held) n7 = 62
      if (with_held) then
         call call_method(cli, srv, settled, 3, err, qid)
         call check_(error, err == CAPNP_OK, 'fill: held export')
         if (allocated(error)) return
         call rpc_result_cap(cli, qid, [0], held_cap, err)
         call check_(error, err == CAPNP_OK .and. associated(held), &
                     'fill: held cap')
         if (allocated(error)) return
         call rpc_finish_send(cli, qid, .true., e2)
         call rpc_pump_once(srv, e2)
         nok = 1
      end if
      do i = 1, n7
         call call_method(cli, srv, settled, 7, err, qid)
         if (err /= CAPNP_OK) exit
         if (i == 1) then
            call rpc_result_cap(cli, qid, [0], victim, err)
            if (err /= CAPNP_OK) exit
         end if
         call rpc_finish_send(cli, qid, .true., e2)
         call rpc_pump_once(srv, e2)
         nok = nok + 1
      end do
      call check_(error, nok == 63, 'fill: 63 exports before the free slot')
      if (allocated(error)) return
      call check_(error, victim%kind == RPC_CAP_IMPORT, 'fill: victim import')
      if (allocated(error)) return
      call rpc_release_send(cli, victim, err)
      call check_(error, err == CAPNP_OK, 'fill: release sent')
      if (allocated(error)) return
      call rpc_pump_once(srv, err)
      call check_(error, err == CAPNP_OK, 'fill: release pumped')
      if (allocated(error)) return
      call check_(error, final_count == 1, 'fill: one server released')
      nlive = next_token - base - final_count
      call check_(error, nlive == 62, 'fill: 62 counted servers still live')
   end subroutine fill_one_free_slot

   !> One free export slot, two staged owned servers. Both are released
   !> while the connection stays open, and close releases nothing again.
   subroutine run_partial_flush(error)
      type(error_type), allocatable, intent(out) :: error
      type(life_srv_t), target :: factory
      type(rpc_conn_t), target :: cli, srv
      type(rpc_cap_t) :: settled, victim, held_cap
      logical :: opened
      integer :: err, nlive
      nlive = 0
      call open_pair(cli, srv, factory, settled, opened, error)
      if (.not. allocated(error)) &
         call fill_one_free_slot(cli, srv, settled, .false., victim, &
                                 held_cap, nlive, error)
      if (.not. allocated(error)) then
         final_count = 0
         call call_method(cli, srv, settled, 8, err)
         write (*, '(a,i0)') 'partial-flush deaths=', final_count
         call check_(error, err == RPC_ERR_EXCEPTION, &
                     'partial flush releases both staged servers: exception')
         call check_(error, final_count == 2, &
                     'partial flush releases both staged servers')
      end if
      call close_pair(cli, srv, opened)
      if (opened .and. .not. allocated(error)) then
         write (*, '(a,i0)') 'partial-flush after close deaths=', final_count
         call check_(error, final_count == nlive + 2, &
                     'partial flush releases both staged servers: close')
      end if
   end subroutine run_partial_flush

   !> One free slot. The flush reuses an export from an earlier call and
   !> then runs out of slots. The earlier server stays until its own
   !> Release, and the two new servers are released with the exception.
   subroutine run_partial_reused(error)
      type(error_type), allocatable, intent(out) :: error
      type(life_srv_t), target :: factory
      type(rpc_conn_t), target :: cli, srv
      type(rpc_cap_t) :: settled, victim, held_cap
      logical :: opened
      integer :: err, nlive, tok
      nlive = 0
      tok = 0
      call open_pair(cli, srv, factory, settled, opened, error)
      if (.not. allocated(error)) &
         call fill_one_free_slot(cli, srv, settled, .true., victim, &
                                 held_cap, nlive, error)
      if (.not. allocated(error)) then
         tok = held%token
         final_count = 0
         call call_method(cli, srv, settled, 9, err)
         write (*, '(a,i0)') 'reused-flush deaths=', final_count
         call check_(error, err == RPC_ERR_EXCEPTION, &
                     'failed flush keeps a reused export: exception')
         call check_(error, final_count == 2, &
                     'failed flush keeps a reused export: two new servers')
         call check_(error, associated(held), &
                     'failed flush keeps a reused export: still live')
         if (.not. allocated(error)) &
            call check_(error, held%token == tok, &
                        'failed flush keeps a reused export: readable')
      end if
      if (.not. allocated(error)) then
         call rpc_release_send(cli, held_cap, err)
         call check_(error, err == CAPNP_OK, &
                     'failed flush keeps a reused export: release sent')
         if (.not. allocated(error)) call rpc_pump_once(srv, err)
         write (*, '(a,i0)') 'reused-flush after release deaths=', final_count
         call check_(error, final_count == 3, &
                     'failed flush keeps a reused export: original release')
         call check_(error, .not. associated(held), &
                     'failed flush keeps a reused export: released once')
      end if
      call close_pair(cli, srv, opened)
      if (opened .and. .not. allocated(error)) then
         write (*, '(a,i0)') 'reused-flush after close deaths=', final_count
         call check_(error, final_count == nlive + 2, &
                     'failed flush keeps a reused export: close')
      end if
   end subroutine run_partial_reused

end module test_rpc_lifetime
