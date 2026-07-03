!> Writer implementation for the streaming tests: accumulates chunk
!> bytes across `write` stream calls, reports the total from `done`.
module rpc_writer_impl
   use capnp
   use streamer_capnp
   implicit none
   private

   public :: my_writer_t

   type, extends(writer_server_t) :: my_writer_t
      integer(int64) :: total = 0_int64
   contains
      procedure :: write => my_write
      procedure :: done => my_done
   end type my_writer_t

contains

   subroutine my_write(self, params, results, err)
      class(my_writer_t), intent(inout) :: self
      type(writer_write_params_t), intent(in) :: params
      type(stream_result_t), intent(in) :: results
      integer, intent(out) :: err
      integer(int8), allocatable :: b(:)
      call writer_write_params_chunk_get(params, b, err)
      if (err /= CAPNP_OK) return
      self%total = self%total + size(b, kind=int64)
   end subroutine my_write

   subroutine my_done(self, params, results, err)
      class(my_writer_t), intent(inout) :: self
      type(writer_done_params_t), intent(in) :: params
      type(writer_done_results_t), intent(in) :: results
      integer, intent(out) :: err
      err = CAPNP_OK
      call writer_done_results_total_set(results, self%total, err)
   end subroutine my_done

end module rpc_writer_impl
