!> Typed Adder implementation on the generated abstract server base:
!> the emitter's dispatch routes method ordinals here.
module rpc_adder_impl
   use capnp
   use adder_capnp
   implicit none
   private

   public :: my_adder_t

   type, extends(adder_server_t) :: my_adder_t
   contains
      procedure :: add => my_add
   end type my_adder_t

contains

   subroutine my_add(self, params, results, err)
      class(my_adder_t), intent(inout) :: self
      type(adder_add_params_t), intent(in) :: params
      type(adder_add_results_t), intent(in) :: results
      integer, intent(out) :: err
      err = CAPNP_OK
      call adder_add_results_sum_set(results, &
                                     adder_add_params_a_get(params) + &
                                     adder_add_params_b_get(params), err)
   end subroutine my_add

end module rpc_adder_impl
