!> Errno for a failed write. The accessor and the interrupted-call value
!> come from the C library, so the write path is not tied to one platform.
module os_errno
   use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_f_pointer
   implicit none
   private
   public :: os_errno_value, os_eintr

   interface
      function capnp_errno_loc() bind(c, name="capnp_errno_loc") result(p)
         import c_ptr
         type(c_ptr) :: p
      end function capnp_errno_loc
      function capnp_eintr() bind(c, name="capnp_eintr") result(v)
         import c_int
         integer(c_int) :: v
      end function capnp_eintr
   end interface

contains

   function os_errno_value() result(e)
      integer :: e
      integer(c_int), pointer :: ep
      call c_f_pointer(capnp_errno_loc(), ep)
      e = int(ep)
   end function os_errno_value

   function os_eintr() result(v)
      integer :: v
      v = int(capnp_eintr())
   end function os_eintr

end module os_errno
