!> Kinds, wire-format constants, and error codes shared by every capnp module.
module capnp_kinds
   use iso_fortran_env, only: int8, int16, int32, int64, real32, real64
   implicit none
   private

   public :: int8, int16, int32, int64, real32, real64

   !> Error codes. 0 means success; everything else names the failure.
   integer, parameter, public :: CAPNP_OK = 0
   integer, parameter, public :: CAPNP_ERR_BOUNDS = 1    !< access outside a segment
   integer, parameter, public :: CAPNP_ERR_KIND = 2      !< pointer kind mismatch
   integer, parameter, public :: CAPNP_ERR_DEPTH = 3     !< nesting depth limit hit
   integer, parameter, public :: CAPNP_ERR_TRAVERSAL = 4 !< traversal word limit hit
   integer, parameter, public :: CAPNP_ERR_ALLOC = 5     !< builder allocation failed
   integer, parameter, public :: CAPNP_ERR_FRAMING = 6   !< malformed segment table
   integer, parameter, public :: CAPNP_ERR_PACKED = 7    !< malformed packed stream
   integer, parameter, public :: CAPNP_ERR_ARG = 8       !< invalid argument
   integer, parameter, public :: CAPNP_ERR_SEGMENT = 9   !< bad segment id in far pointer
   integer, parameter, public :: CAPNP_ERR_IO = 10       !< file I/O failure

   !> Wire pointer kinds (bits 0-1 of a pointer word).
   integer, parameter, public :: CAPNP_WK_STRUCT = 0
   integer, parameter, public :: CAPNP_WK_LIST = 1
   integer, parameter, public :: CAPNP_WK_FAR = 2
   integer, parameter, public :: CAPNP_WK_CAP = 3

   !> Resolved object kinds carried by capnp_ptr_t.
   integer, parameter, public :: CAPNP_PK_NULL = 0
   integer, parameter, public :: CAPNP_PK_STRUCT = 1
   integer, parameter, public :: CAPNP_PK_LIST = 2
   integer, parameter, public :: CAPNP_PK_CAP = 3

   !> List element size codes (bits 32-34 of a list pointer).
   integer, parameter, public :: CAPNP_SZ_VOID = 0
   integer, parameter, public :: CAPNP_SZ_BIT = 1
   integer, parameter, public :: CAPNP_SZ_BYTE = 2
   integer, parameter, public :: CAPNP_SZ_TWO = 3
   integer, parameter, public :: CAPNP_SZ_FOUR = 4
   integer, parameter, public :: CAPNP_SZ_EIGHT = 5
   integer, parameter, public :: CAPNP_SZ_PTR = 6
   integer, parameter, public :: CAPNP_SZ_COMPOSITE = 7

   integer(int64), parameter, public :: CAPNP_WORD_BYTES = 8_int64

   !> Reader guards, matching the C++ defaults (64 MiB traversal, depth 64).
   integer(int64), parameter, public :: CAPNP_DEFAULT_TRAVERSAL_WORDS = 8388608_int64
   integer, parameter, public :: CAPNP_DEFAULT_DEPTH_LIMIT = 64

   public :: capnp_list_step_bits

contains

   !> Stride of one element, in bits, for a non-composite size code.
   pure function capnp_list_step_bits(esize) result(bits)
      integer, intent(in) :: esize
      integer :: bits
      select case (esize)
      case (CAPNP_SZ_VOID); bits = 0
      case (CAPNP_SZ_BIT); bits = 1
      case (CAPNP_SZ_BYTE); bits = 8
      case (CAPNP_SZ_TWO); bits = 16
      case (CAPNP_SZ_FOUR); bits = 32
      case (CAPNP_SZ_EIGHT, CAPNP_SZ_PTR); bits = 64
      case default; bits = -1
      end select
   end function capnp_list_step_bits

end module capnp_kinds
