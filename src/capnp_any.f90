!> Typed views of AnyStruct and AnyList fields, and a resolved Capability
!> field. Generated modules use these by name, so schemas that import each
!> other do not declare them twice and a schema may still declare a struct
!> of its own with one of these names.
module capnp_any
   use capnp_kinds
   use capnp_message, only: capnp_ptr_t
   implicit none
   private

   public :: any_struct_t, any_list_t, capability_client_t

   type :: any_struct_t
      type(capnp_ptr_t) :: p
   end type any_struct_t

   type :: any_list_t
      type(capnp_ptr_t) :: p
   end type any_list_t

   type :: capability_client_t
      integer :: kind
      integer(int64) :: id
   end type capability_client_t

end module capnp_any
