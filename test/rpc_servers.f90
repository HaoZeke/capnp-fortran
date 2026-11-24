!> Capability servers for the RPC tests: an echo/factory service and the
!> adder capabilities it mints.
module rpc_servers
   use capnp
   use rpc_capnp
   use capnp_rpc
   implicit none
   private

   public :: echo_srv_t, adder_srv_t, ECHO_IFACE, free_minted_adders

   type :: adder_ref_t
      type(adder_srv_t), pointer :: p => null()
   end type adder_ref_t

   type(adder_ref_t), allocatable :: minted(:)

   integer(int64), parameter :: ECHO_IFACE = 81985529216486895_int64 ! 0x0123456789abcdef
   integer :: next_adder_token = 0

   !> method 0: results = { i64 = params.i64 * 2, text = 'echo: '//params.text }
   !> method 1: results = { cap ptr0 = new adder with base = params.i64 }
   type, extends(rpc_server_t) :: echo_srv_t
   contains
      procedure :: dispatch => echo_dispatch
   end type echo_srv_t

   !> method 0: results.i64 = base + params.i64
   type, extends(rpc_server_t) :: adder_srv_t
      integer(int64) :: base = 0_int64
      integer :: token = 0
   contains
      procedure :: dispatch => adder_dispatch
      final :: adder_final
   end type adder_srv_t

contains

   subroutine echo_dispatch(self, ctx, err)
      class(echo_srv_t), intent(inout) :: self
      type(rpc_call_ctx_t), intent(inout) :: ctx
      integer, intent(out) :: err
      type(capnp_ptr_t) :: s
      type(capnp_ptr_t) :: owner
      type(adder_srv_t), pointer :: ad
      class(rpc_server_t), pointer :: sp
      character(len=:), allocatable :: txt, owner_name
      integer :: idx
      err = CAPNP_OK
      associate (server => self)
      end associate
      if (ctx%interface_id == RPC_PERSISTENT_IFACE) then
         ! Level 2 persistence hook: save() answers an app-defined
         ! SturdyRef (here a text token) in results ptr 0.
         if (ctx%method_id /= RPC_PERSISTENT_SAVE) then
            err = CAPNP_ERR_ARG
            return
         end if
         owner = capnp_getp(ctx%params, 0, err)
         if (err /= CAPNP_OK) return
         s = capnp_new_struct(ctx%rmsg, 0, 1, err)
         if (err /= CAPNP_OK) return
         if (owner%kind == CAPNP_PK_NULL) then
            call capnp_set_text(s, 0, 'sturdy:echo-main', err)
         else
            call capnp_get_text(owner, 0, owner_name, err)
            if (err /= CAPNP_OK) return
            call capnp_set_text(s, 0, 'sturdy:echo-main:'//owner_name, err)
         end if
         if (err == CAPNP_OK) call payload_content_set(ctx%results, s, err)
         return
      end if
      if (ctx%interface_id /= ECHO_IFACE) then
         err = CAPNP_ERR_ARG
         return
      end if
      select case (ctx%method_id)
      case (0)
         s = capnp_new_struct(ctx%rmsg, 1, 1, err)
         if (err /= CAPNP_OK) return
         call capnp_set_i64(s, 0_int64, capnp_get_i64(ctx%params, 0_int64)*2_int64, err)
         call capnp_get_text(ctx%params, 0, txt, err)
         call capnp_set_text(s, 0, 'echo: '//txt, err)
         if (err == CAPNP_OK) call payload_content_set(ctx%results, s, err)
      case (1)
         allocate (ad)
         next_adder_token = next_adder_token + 1
         ad%token = next_adder_token
         call remember_adder(ad)
         ad%base = capnp_get_i64(ctx%params, 0_int64)
         sp => ad
         ! The export table owns this object and frees it with the last
         ! reference. A failed return never reaches that table.
         idx = rpc_ctx_export_cap(ctx, sp, err, owned=.true.)
         if (err /= CAPNP_OK) then
            deallocate (ad)
            return
         end if
         s = capnp_new_struct(ctx%rmsg, 0, 1, err)
         if (err /= CAPNP_OK) then
            ctx%nexp = ctx%nexp - 1
            ctx%staged(idx)%s => null()
            ctx%staged(idx)%owned = .false.
            deallocate (ad)
            return
         end if
         call capnp_setp(s, 0, rpc_make_cap_ptr(ctx%rmsg, idx), err)
         if (err == CAPNP_OK) call payload_content_set(ctx%results, s, err)
         if (err /= CAPNP_OK) then
            ctx%nexp = ctx%nexp - 1
            ctx%staged(idx)%s => null()
            ctx%staged(idx)%owned = .false.
            deallocate (ad)
         end if
      case default
         err = CAPNP_ERR_ARG
      end select
   end subroutine echo_dispatch

   subroutine remember_adder(ad)
      type(adder_srv_t), pointer, intent(in) :: ad
      type(adder_ref_t), allocatable :: grown(:)
      integer :: n
      if (.not. allocated(minted)) allocate (minted(0))
      n = size(minted)
      allocate (grown(n + 1))
      if (n > 0) grown(1:n) = minted
      grown(n + 1)%p => ad
      call move_alloc(grown, minted)
   end subroutine remember_adder

   !> Free adders minted by the echo server. Call this after every
   !> connection that exported them has been closed.
   subroutine free_minted_adders()
      type(adder_srv_t), pointer :: p
      integer :: i
      if (.not. allocated(minted)) return
      do i = 1, size(minted)
         if (.not. associated(minted(i)%p)) cycle
         p => minted(i)%p
         minted(i)%p => null()
         deallocate (p)
      end do
      deallocate (minted)
   end subroutine free_minted_adders

   !> The export table frees an owned adder. Drop the runner's pointer
   !> so that later cleanup does not free the same object.
   subroutine adder_final(self)
      type(adder_srv_t), intent(inout) :: self
      integer :: i
      if (.not. allocated(minted)) return
      do i = 1, size(minted)
         if (.not. associated(minted(i)%p)) cycle
         if (minted(i)%p%token == self%token) minted(i)%p => null()
      end do
   end subroutine adder_final

   subroutine adder_dispatch(self, ctx, err)
      class(adder_srv_t), intent(inout) :: self
      type(rpc_call_ctx_t), intent(inout) :: ctx
      integer, intent(out) :: err
      type(capnp_ptr_t) :: s
      err = CAPNP_OK
      if (ctx%method_id /= 0) then
         err = CAPNP_ERR_ARG
         return
      end if
      s = capnp_new_struct(ctx%rmsg, 1, 0, err)
      if (err /= CAPNP_OK) return
      call capnp_set_i64(s, 0_int64, self%base + capnp_get_i64(ctx%params, 0_int64), err)
      if (err == CAPNP_OK) call payload_content_set(ctx%results, s, err)
   end subroutine adder_dispatch

end module rpc_servers
