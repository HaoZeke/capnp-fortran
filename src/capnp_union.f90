!> Union discriminant helpers. A union's tag is a u16 in the data section at
!> discriminantOffset, counted in 16-bit units from the section start.
module capnp_union
   use capnp_kinds
   use capnp_message, only: capnp_ptr_t, capnp_get_u16, capnp_set_u16
   implicit none
   private

   public :: capnp_which, capnp_set_which

contains

   !> Read the active union member ordinal.
   function capnp_which(p, disc_off16) result(w)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(in) :: disc_off16
      integer :: w
      w = int(capnp_get_u16(p, int(disc_off16, int64)*2_int64))
   end function capnp_which

   !> Set the active union member ordinal.
   subroutine capnp_set_which(p, disc_off16, w, err)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(in) :: disc_off16
      integer, intent(in) :: w
      integer, intent(out) :: err
      call capnp_set_u16(p, int(disc_off16, int64)*2_int64, int(w, int32), err)
   end subroutine capnp_set_which

end module capnp_union
