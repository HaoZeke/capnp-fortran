!> Typed generated-code RPC: the emitter's client helpers and abstract
!> server base carry an Adder call end to end over a socketpair.
module test_rpc_typed
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed, skip_test
   use capnp
   use adder_capnp
   use capnp_posix
   use capnp_rpc
   use rpc_adder_impl
   implicit none

   private
   public :: collect_rpc_typed

contains

   subroutine collect_rpc_typed(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [new_unittest("rpc-typed", run_rpc_typed)]
   end subroutine collect_rpc_typed

   subroutine check_(error, cond, name)
      type(error_type), allocatable, intent(inout) :: error
      logical, intent(in) :: cond
      character(len=*), intent(in) :: name
      if (allocated(error)) return
      call check(error, cond, name)
   end subroutine check_

   subroutine run_rpc_typed(error)
      type(error_type), allocatable, intent(out) :: error
      type(rpc_conn_t), target :: cli, srv
      type(my_adder_t), target :: impl
      class(rpc_server_t), pointer :: boot
      type(adder_client_t) :: client
      type(capnp_message_t), target :: m
      type(adder_add_params_t) :: params
      type(adder_add_results_t) :: results
      integer(int64) :: qid
      integer :: fda, fdb, err

      call px_socketpair(fda, fdb, err)
      call check_(error, err == CAPNP_OK, 'typed: socketpair')
      boot => impl
      call rpc_conn_init(srv, fdb, boot)
      boot => null()
      call rpc_conn_init(cli, fda, boot)

      ! Bootstrap resolves the typed client; the pipeline cap works as-is.
      call rpc_bootstrap_send(cli, client%cap, err)
      call check_(error, err == CAPNP_OK, 'typed: bootstrap sent')
      call rpc_pump_once(srv, err)

      call adder_add_begin(cli, client, m, params, qid, err)
      call check_(error, err == CAPNP_OK, 'typed: call begin')
      call adder_add_params_a_set(params, 19_int64, err)
      call adder_add_params_b_set(params, 23_int64, err)
      call rpc_call_send(cli, m, err)
      call check_(error, err == CAPNP_OK, 'typed: call sent')
      call rpc_pump_once(srv, err)
      call check_(error, err == CAPNP_OK, 'typed: dispatched')

      call adder_add_wait(cli, qid, results, err)
      call check_(error, err == CAPNP_OK, 'typed: returned')
      call check_(error, adder_add_results_sum_get(results) == 42_int64, 'typed: sum == 42')

      ! Settle the bootstrap and call again on the import.
      qid = client%cap%id
      call rpc_wait(cli, qid, err)
      call rpc_result_cap(cli, qid, [integer ::], client%cap, err)
      call check_(error, err == CAPNP_OK .and. client%cap%kind == RPC_CAP_IMPORT, 'typed: cap settles')
      call adder_add_begin(cli, client, m, params, qid, err)
      call adder_add_params_a_set(params, -1_int64, err)
      call adder_add_params_b_set(params, 1_int64, err)
      call rpc_call_send(cli, m, err)
      call rpc_pump_once(srv, err)
      call adder_add_wait(cli, qid, results, err)
      call check_(error, err == CAPNP_OK .and. adder_add_results_sum_get(results) == 0_int64, &
                  'typed: settled sum == 0')

      call rpc_conn_close(cli)
      call rpc_conn_close(srv)
   end subroutine run_rpc_typed

end module test_rpc_typed
