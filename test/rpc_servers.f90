!> Capability servers for the RPC tests: an echo/factory service and the
!> adder capabilities it mints.
module rpc_servers
   use capnp
   use rpc_capnp
   use capnp_rpc
   implicit none
   private

   public :: echo_srv_t, adder_srv_t, ECHO_IFACE

   integer(int64), parameter :: ECHO_IFACE = 81985529216486895_int64 ! 0x0123456789abcdef

   !> method 0: results = { i64 = params.i64 * 2, text = 'echo: '//params.text }
   !> method 1: results = { cap ptr0 = new adder with base = params.i64 }
   type, extends(rpc_server_t) :: echo_srv_t
   contains
      procedure :: dispatch => echo_dispatch
   end type echo_srv_t

   !> method 0: results.i64 = base + params.i64
   type, extends(rpc_server_t) :: adder_srv_t
      integer(int64) :: base = 0_int64
   contains
      procedure :: dispatch => adder_dispatch
   end type adder_srv_t

contains

   subroutine echo_dispatch(self, ctx, err)
      class(echo_srv_t), intent(inout) :: self
      type(rpc_call_ctx_t), intent(inout) :: ctx
      integer, intent(out) :: err
      type(capnp_ptr_t) :: s
      type(adder_srv_t), pointer :: ad
      class(rpc_server_t), pointer :: sp
      character(len=:), allocatable :: txt
      integer :: idx
      err = CAPNP_OK
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
         ad%base = capnp_get_i64(ctx%params, 0_int64)
         sp => ad
         idx = rpc_ctx_export_cap(ctx, sp, err)
         if (err /= CAPNP_OK) return
         s = capnp_new_struct(ctx%rmsg, 0, 1, err)
         if (err /= CAPNP_OK) return
         call capnp_setp(s, 0, rpc_make_cap_ptr(ctx%rmsg, idx), err)
         if (err == CAPNP_OK) call payload_content_set(ctx%results, s, err)
      case default
         err = CAPNP_ERR_ARG
      end select
   end subroutine echo_dispatch

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
