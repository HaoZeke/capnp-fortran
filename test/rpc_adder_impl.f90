!> Typed Adder implementation on the generated abstract server base:
!> the emitter's dispatch routes method ordinals here.
module rpc_adder_impl
   use capnp
   use capnp_rpc, only: rpc_call_ctx_t
   use adder_capnp
   implicit none
   private

   public :: my_adder_t

   type, extends(adder_server_t) :: my_adder_t
      integer :: canceled = 0
   contains
      procedure :: add => my_add
      procedure :: wait_forever => my_wait_forever
      procedure :: cancellation_count => my_cancellation_count
   end type my_adder_t

contains

   subroutine my_add(self, ctx, params, results, err)
      class(my_adder_t), intent(inout) :: self
      type(rpc_call_ctx_t), intent(inout) :: ctx
      type(adder_add_params_t), intent(in) :: params
      type(adder_add_results_t), intent(in) :: results
      integer, intent(out) :: err
      err = CAPNP_OK
      associate (server => self)
      end associate
      call adder_add_results_sum_set(results, &
                                     adder_add_params_a_get(params) + &
                                     adder_add_params_b_get(params), err)
   end subroutine my_add

   !> The reference peer waits until the caller cancels. This local
   !> server only has to answer so the generated base can be extended.
   subroutine my_wait_forever(self, ctx, params, results, err)
      class(my_adder_t), intent(inout) :: self
      type(rpc_call_ctx_t), intent(inout) :: ctx
      type(adder_wait_forever_params_t), intent(in) :: params
      type(adder_wait_forever_results_t), intent(in) :: results
      integer, intent(out) :: err
      err = CAPNP_OK
      associate (ignored => params, ignored2 => results, ignored3 => ctx)
      end associate
      self%canceled = self%canceled + 1
   end subroutine my_wait_forever

   subroutine my_cancellation_count(self, ctx, params, results, err)
      class(my_adder_t), intent(inout) :: self
      type(rpc_call_ctx_t), intent(inout) :: ctx
      type(adder_cancellation_count_params_t), intent(in) :: params
      type(adder_cancellation_count_results_t), intent(in) :: results
      integer, intent(out) :: err
      associate (ignored => params, ignored2 => ctx)
      end associate
      call adder_cancellation_count_results_count_set( &
         results, int(self%canceled, int64), err)
   end subroutine my_cancellation_count

end module rpc_adder_impl
