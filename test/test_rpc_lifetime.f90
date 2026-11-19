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
   type(counted_srv_t), pointer :: held => null()
   type(rpc_conn_t), pointer :: gsrv => null()

contains

   subroutine collect_rpc_lifetime(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      allocate (testsuite(6))
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
      end if
   end subroutine counted_dispatch

   subroutine life_dispatch(self, ctx, err)
      class(life_srv_t), intent(inout) :: self
      type(rpc_call_ctx_t), intent(inout) :: ctx
      integer, intent(out) :: err
      type(counted_srv_t), pointer :: obj
      class(rpc_server_t), pointer :: sp
      type(capnp_ptr_t) :: s
      integer :: idx
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

end module test_rpc_lifetime
