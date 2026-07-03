!> Hand-rolled reader for the CodeGeneratorRequest subset capnpc-fortran
!> needs. Offsets come from `capnp compile -ocapnp schema.capnp` (capnp
!> 1.4.0); the same bootstrap approach as capnpc-c, which reads its own
!> request with its own runtime instead of generated code.
module capnp_schema
   use capnp
   implicit none
   private

   ! Node union tags (tag u16 at data byte 12).
   integer, parameter, public :: NODE_FILE = 0
   integer, parameter, public :: NODE_STRUCT = 1
   integer, parameter, public :: NODE_ENUM = 2
   integer, parameter, public :: NODE_INTERFACE = 3
   integer, parameter, public :: NODE_CONST = 4
   integer, parameter, public :: NODE_ANNOTATION = 5

   ! Field union tags (tag u16 at data byte 8).
   integer, parameter, public :: FIELD_SLOT = 0
   integer, parameter, public :: FIELD_GROUP = 1

   ! Type union tags (tag u16 at data byte 0).
   integer, parameter, public :: TYPE_VOID = 0
   integer, parameter, public :: TYPE_BOOL = 1
   integer, parameter, public :: TYPE_INT8 = 2
   integer, parameter, public :: TYPE_INT16 = 3
   integer, parameter, public :: TYPE_INT32 = 4
   integer, parameter, public :: TYPE_INT64 = 5
   integer, parameter, public :: TYPE_UINT8 = 6
   integer, parameter, public :: TYPE_UINT16 = 7
   integer, parameter, public :: TYPE_UINT32 = 8
   integer, parameter, public :: TYPE_UINT64 = 9
   integer, parameter, public :: TYPE_FLOAT32 = 10
   integer, parameter, public :: TYPE_FLOAT64 = 11
   integer, parameter, public :: TYPE_TEXT = 12
   integer, parameter, public :: TYPE_DATA = 13
   integer, parameter, public :: TYPE_LIST = 14
   integer, parameter, public :: TYPE_ENUM = 15
   integer, parameter, public :: TYPE_STRUCT = 16
   integer, parameter, public :: TYPE_INTERFACE = 17
   integer, parameter, public :: TYPE_ANY_POINTER = 18

   integer, parameter, public :: NO_DISCRIMINANT = 65535

   public :: cgr_nodes, cgr_requested_files
   public :: reqfile_id, reqfile_filename
   public :: node_id, node_display_name, node_prefix_len, node_scope_id, node_which
   public :: node_nested, node_nested_name, node_nested_id
   public :: node_struct_data_words, node_struct_pointer_count, node_struct_is_group
   public :: node_struct_discriminant_count, node_struct_discriminant_offset
   public :: node_struct_fields, node_enumerants, node_const_type, node_const_value
   public :: node_interface_methods, method_name
   public :: method_param_struct_type, method_result_struct_type
   public :: field_name, field_code_order, field_discriminant, field_which
   public :: field_slot_offset, field_slot_type, field_slot_default, field_group_type_id
   public :: field_slot_had_default
   public :: enumerant_name
   public :: type_which, type_list_element, type_type_id
   public :: value_which, value_bool, value_i8, value_i16, value_i32, value_i64
   public :: value_u8, value_u16, value_u32, value_u64, value_f32, value_f64
   public :: value_text, value_enum, value_pointer, value_data
   public :: type_brand, type_anyptr_which, type_param_scope_id, type_param_index
   public :: brand_scopes, scope_scope_id, scope_which, scope_bindings
   public :: binding_which, binding_type

   integer, parameter, public :: ANYPTR_UNCONSTRAINED = 0
   integer, parameter, public :: ANYPTR_PARAMETER = 1
   integer, parameter, public :: SCOPE_BIND = 0
   integer, parameter, public :: SCOPE_INHERIT = 1
   integer, parameter, public :: BINDING_UNBOUND = 0
   integer, parameter, public :: BINDING_TYPE = 1

contains

   !> Brand of a struct/enum/interface Type variant: ptr 0 of Type.
   function type_brand(p, err) result(b)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(out) :: err
      type(capnp_ptr_t) :: b
      b = capnp_getp(p, 0, err)
   end function type_brand

   !> Type.anyPointer inner union tag (u16 at byte 8): unconstrained=0,
   !> parameter=1, implicitMethodParameter=2.
   function type_anyptr_which(p) result(w)
      type(capnp_ptr_t), intent(in) :: p
      integer :: w
      w = int(capnp_get_u16(p, 8_int64))
   end function type_anyptr_which

   !> Type.anyPointer.parameter: scopeId u64 at byte 16, parameterIndex
   !> u16 at byte 10.
   function type_param_scope_id(p) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64) :: v
      v = capnp_get_i64(p, 16_int64)
   end function type_param_scope_id

   function type_param_index(p) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer :: v
      v = int(capnp_get_u16(p, 10_int64))
   end function type_param_index

   !> Brand.scopes: List(Scope) at ptr 0.
   function brand_scopes(p, err) result(l)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(out) :: err
      type(capnp_ptr_t) :: l
      l = capnp_getp(p, 0, err)
   end function brand_scopes

   !> Scope: scopeId u64 at byte 0, union tag u16 at byte 8 (bind=0,
   !> inherit=1), bind list at ptr 0.
   function scope_scope_id(p) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64) :: v
      v = capnp_get_i64(p, 0_int64)
   end function scope_scope_id

   function scope_which(p) result(w)
      type(capnp_ptr_t), intent(in) :: p
      integer :: w
      w = int(capnp_get_u16(p, 8_int64))
   end function scope_which

   function scope_bindings(p, err) result(l)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(out) :: err
      type(capnp_ptr_t) :: l
      l = capnp_getp(p, 0, err)
   end function scope_bindings

   !> Binding: union tag u16 at byte 0 (unbound=0, type=1), type at ptr 0.
   function binding_which(p) result(w)
      type(capnp_ptr_t), intent(in) :: p
      integer :: w
      w = int(capnp_get_u16(p, 0_int64))
   end function binding_which

   function binding_type(p, err) result(t)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(out) :: err
      type(capnp_ptr_t) :: t
      t = capnp_getp(p, 0, err)
   end function binding_type

   ! --- CodeGeneratorRequest: 0 data words, 4 ptrs ---------------------

   function cgr_nodes(root, err) result(l)
      type(capnp_ptr_t), intent(in) :: root
      integer, intent(out) :: err
      type(capnp_ptr_t) :: l
      l = capnp_getp(root, 0, err)
   end function cgr_nodes

   function cgr_requested_files(root, err) result(l)
      type(capnp_ptr_t), intent(in) :: root
      integer, intent(out) :: err
      type(capnp_ptr_t) :: l
      l = capnp_getp(root, 1, err)
   end function cgr_requested_files

   ! --- RequestedFile ---------------------------------------------------

   function reqfile_id(p) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64) :: v
      v = capnp_get_i64(p, 0_int64)
   end function reqfile_id

   subroutine reqfile_filename(p, s, err)
      type(capnp_ptr_t), intent(in) :: p
      character(len=:), allocatable, intent(out) :: s
      integer, intent(out) :: err
      call capnp_get_text(p, 0, s, err)
   end subroutine reqfile_filename

   ! --- Node: 6 data words, 6 ptrs --------------------------------------

   function node_id(p) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64) :: v
      v = capnp_get_i64(p, 0_int64)
   end function node_id

   subroutine node_display_name(p, s, err)
      type(capnp_ptr_t), intent(in) :: p
      character(len=:), allocatable, intent(out) :: s
      integer, intent(out) :: err
      call capnp_get_text(p, 0, s, err)
   end subroutine node_display_name

   function node_prefix_len(p) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64) :: v
      v = capnp_get_u32(p, 8_int64)
   end function node_prefix_len

   function node_scope_id(p) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64) :: v
      v = capnp_get_i64(p, 16_int64)
   end function node_scope_id

   function node_which(p) result(w)
      type(capnp_ptr_t), intent(in) :: p
      integer :: w
      w = int(capnp_get_u16(p, 12_int64))
   end function node_which

   function node_nested(p, err) result(l)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(out) :: err
      type(capnp_ptr_t) :: l
      l = capnp_getp(p, 1, err)
   end function node_nested

   subroutine node_nested_name(nn, s, err)
      type(capnp_ptr_t), intent(in) :: nn
      character(len=:), allocatable, intent(out) :: s
      integer, intent(out) :: err
      call capnp_get_text(nn, 0, s, err)
   end subroutine node_nested_name

   function node_nested_id(nn) result(v)
      type(capnp_ptr_t), intent(in) :: nn
      integer(int64) :: v
      v = capnp_get_i64(nn, 0_int64)
   end function node_nested_id

   function node_struct_data_words(p) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer :: v
      v = int(capnp_get_u16(p, 14_int64))
   end function node_struct_data_words

   function node_struct_pointer_count(p) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer :: v
      v = int(capnp_get_u16(p, 24_int64))
   end function node_struct_pointer_count

   function node_struct_is_group(p) result(v)
      type(capnp_ptr_t), intent(in) :: p
      logical :: v
      v = capnp_get_bool(p, 224_int64)
   end function node_struct_is_group

   function node_struct_discriminant_count(p) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer :: v
      v = int(capnp_get_u16(p, 30_int64))
   end function node_struct_discriminant_count

   function node_struct_discriminant_offset(p) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64) :: v
      v = capnp_get_u32(p, 32_int64)
   end function node_struct_discriminant_offset

   function node_struct_fields(p, err) result(l)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(out) :: err
      type(capnp_ptr_t) :: l
      l = capnp_getp(p, 3, err)
   end function node_struct_fields

   function node_enumerants(p, err) result(l)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(out) :: err
      type(capnp_ptr_t) :: l
      l = capnp_getp(p, 3, err)
   end function node_enumerants

   !> Node.interface.methods: List(Method) at ptr 3.
   function node_interface_methods(p, err) result(l)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(out) :: err
      type(capnp_ptr_t) :: l
      l = capnp_getp(p, 3, err)
   end function node_interface_methods

   !> Method: name ptr 0, paramStructType u64 at byte 8, resultStructType
   !> u64 at byte 16 (per `capnp compile -ocapnp schema.capnp`).
   subroutine method_name(p, s, err)
      type(capnp_ptr_t), intent(in) :: p
      character(len=:), allocatable, intent(out) :: s
      integer, intent(out) :: err
      call capnp_get_text(p, 0, s, err)
   end subroutine method_name

   function method_param_struct_type(p) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64) :: v
      v = capnp_get_i64(p, 8_int64)
   end function method_param_struct_type

   function method_result_struct_type(p) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64) :: v
      v = capnp_get_i64(p, 16_int64)
   end function method_result_struct_type

   function node_const_type(p, err) result(t)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(out) :: err
      type(capnp_ptr_t) :: t
      t = capnp_getp(p, 3, err)
   end function node_const_type

   function node_const_value(p, err) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(out) :: err
      type(capnp_ptr_t) :: v
      v = capnp_getp(p, 4, err)
   end function node_const_value

   ! --- Field: 3 data words, 4 ptrs --------------------------------------

   subroutine field_name(p, s, err)
      type(capnp_ptr_t), intent(in) :: p
      character(len=:), allocatable, intent(out) :: s
      integer, intent(out) :: err
      call capnp_get_text(p, 0, s, err)
   end subroutine field_name

   function field_code_order(p) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer :: v
      v = int(capnp_get_u16(p, 0_int64))
   end function field_code_order

   !> 65535 (NO_DISCRIMINANT) when the field is not a union member.
   function field_discriminant(p) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer :: v
      v = int(capnp_get_u16(p, 2_int64, default=65535_int32))
   end function field_discriminant

   function field_which(p) result(w)
      type(capnp_ptr_t), intent(in) :: p
      integer :: w
      w = int(capnp_get_u16(p, 8_int64))
   end function field_which

   !> Offset in units of the field's own size (bits for Bool, ptr slots for
   !> pointer types).
   function field_slot_offset(p) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64) :: v
      v = capnp_get_u32(p, 4_int64)
   end function field_slot_offset

   function field_slot_type(p, err) result(t)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(out) :: err
      type(capnp_ptr_t) :: t
      t = capnp_getp(p, 2, err)
   end function field_slot_type

   function field_slot_default(p, err) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(out) :: err
      type(capnp_ptr_t) :: v
      v = capnp_getp(p, 3, err)
   end function field_slot_default

   function field_group_type_id(p) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64) :: v
      v = capnp_get_i64(p, 16_int64)
   end function field_group_type_id

   function field_slot_had_default(p) result(v)
      type(capnp_ptr_t), intent(in) :: p
      logical :: v
      v = capnp_get_bool(p, 128_int64)
   end function field_slot_had_default

   ! --- Enumerant: 1 data word, 2 ptrs -----------------------------------

   subroutine enumerant_name(p, s, err)
      type(capnp_ptr_t), intent(in) :: p
      character(len=:), allocatable, intent(out) :: s
      integer, intent(out) :: err
      call capnp_get_text(p, 0, s, err)
   end subroutine enumerant_name

   ! --- Type: 3 data words, 1 ptr ----------------------------------------

   function type_which(p) result(w)
      type(capnp_ptr_t), intent(in) :: p
      integer :: w
      w = int(capnp_get_u16(p, 0_int64))
   end function type_which

   function type_list_element(p, err) result(t)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(out) :: err
      type(capnp_ptr_t) :: t
      t = capnp_getp(p, 0, err)
   end function type_list_element

   !> typeId for enum/struct/interface variants (same slot in all three).
   function type_type_id(p) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64) :: v
      v = capnp_get_i64(p, 8_int64)
   end function type_type_id

   ! --- Value: 2 data words, 1 ptr ----------------------------------------

   function value_which(p) result(w)
      type(capnp_ptr_t), intent(in) :: p
      integer :: w
      w = int(capnp_get_u16(p, 0_int64))
   end function value_which

   function value_bool(p) result(v)
      type(capnp_ptr_t), intent(in) :: p
      logical :: v
      v = capnp_get_bool(p, 16_int64)
   end function value_bool

   function value_i8(p) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer(int8) :: v
      v = capnp_get_i8(p, 2_int64)
   end function value_i8

   function value_i16(p) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer(int16) :: v
      v = capnp_get_i16(p, 2_int64)
   end function value_i16

   function value_i32(p) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer(int32) :: v
      v = capnp_get_i32(p, 4_int64)
   end function value_i32

   function value_i64(p) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64) :: v
      v = capnp_get_i64(p, 8_int64)
   end function value_i64

   function value_u8(p) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer(int16) :: v
      v = capnp_get_u8(p, 2_int64)
   end function value_u8

   function value_u16(p) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer(int32) :: v
      v = capnp_get_u16(p, 2_int64)
   end function value_u16

   function value_u32(p) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64) :: v
      v = capnp_get_u32(p, 4_int64)
   end function value_u32

   function value_u64(p) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64) :: v
      v = capnp_get_i64(p, 8_int64)
   end function value_u64

   function value_f32(p) result(v)
      type(capnp_ptr_t), intent(in) :: p
      real(real32) :: v
      v = capnp_get_f32(p, 4_int64)
   end function value_f32

   function value_f64(p) result(v)
      type(capnp_ptr_t), intent(in) :: p
      real(real64) :: v
      v = capnp_get_f64(p, 8_int64)
   end function value_f64

   !> The default object behind a struct/list/anyPointer Value variant.
   function value_pointer(p, err) result(q)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(out) :: err
      type(capnp_ptr_t) :: q
      q = capnp_getp(p, 0, err)
   end function value_pointer

   subroutine value_data(p, b, err)
      type(capnp_ptr_t), intent(in) :: p
      integer(int8), allocatable, intent(out) :: b(:)
      integer, intent(out) :: err
      call capnp_get_data(p, 0, b, err)
   end subroutine value_data

   subroutine value_text(p, s, err)
      type(capnp_ptr_t), intent(in) :: p
      character(len=:), allocatable, intent(out) :: s
      integer, intent(out) :: err
      call capnp_get_text(p, 0, s, err)
   end subroutine value_text

   function value_enum(p) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer :: v
      v = int(capnp_get_u16(p, 2_int64))
   end function value_enum

end module capnp_schema
