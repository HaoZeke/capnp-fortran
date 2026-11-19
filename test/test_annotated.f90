!> An annotated interface must come out as Fortran that compiles: the
!> method note is a module parameter, and the server type can be extended.
module test_annotated
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use capnp_rpc, only: rpc_call_ctx_t
   use dynamic_capnp
   implicit none
   private
   public :: collect_annotated

   type, extends(annotated_service_server_t) :: ping_srv_t
   contains
      procedure :: ping => ping_srv
   end type ping_srv_t

contains

   subroutine collect_annotated(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [new_unittest("annotated-interface", run_annotated)]
   end subroutine collect_annotated

   subroutine ping_srv(self, ctx, params, results, err)
      class(ping_srv_t), intent(inout) :: self
      type(rpc_call_ctx_t), intent(inout) :: ctx
      type(annotated_service_ping_params_t), intent(in) :: params
      type(annotated_service_ping_results_t), intent(in) :: results
      integer, intent(out) :: err
      err = CAPNP_OK
      associate (server => self)
      end associate
      associate (unused_ctx => ctx)
      end associate
      associate (unused_params => params)
      end associate
      associate (unused_results => results)
      end associate
   end subroutine ping_srv

   subroutine run_annotated(error)
      type(error_type), allocatable, intent(out) :: error
      type(ping_srv_t) :: srv
      call check(error, &
                 ANNOTATED_SERVICE_PING_ANNOTATION_METHOD_NOTE == 'ping method', &
                 'annotated: method note is generated')
      if (allocated(error)) return
      call check(error, ANNOTATED_SERVICE_PING_ANNOTATION_METHOD_NOTE_KIND == 12, &
                 'annotated: method note kind')
      if (allocated(error)) return
      call check(error, same_type_as(srv, srv), 'annotated: server type extends')
   end subroutine run_annotated

end module test_annotated
