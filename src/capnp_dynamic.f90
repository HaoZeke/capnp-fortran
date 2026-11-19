!> Dynamic reflection (SchemaLoader-style): load a compiled schema (a
!> CodeGeneratorRequest, `capnp compile -o-`) at runtime and read or
!> write messages by type and field NAME, no generated code involved.
!>
!> A loaded schema owns the CGR message, so schema variables carry the
!> `target` attribute like any message. Handles into user data are the
!> ordinary capnp_ptr_t values the runtime already produces.
module capnp_dynamic
   use capnp
   use capnp_schema
   implicit none
   private

   public :: capnp_dyn_schema_t
   public :: capnp_dyn_load, capnp_dyn_free, capnp_dyn_find, capnp_dyn_new_root
   public :: capnp_dyn_get_int, capnp_dyn_get_f64, capnp_dyn_get_bool
   public :: capnp_dyn_get_text, capnp_dyn_get_data, capnp_dyn_getp
   public :: capnp_dyn_set_int, capnp_dyn_set_bool, capnp_dyn_set_f64
   public :: capnp_dyn_set_text, capnp_dyn_set_data
   public :: capnp_dyn_setp, capnp_dyn_init_struct, capnp_dyn_init_list
   public :: capnp_dyn_get_capability, capnp_dyn_set_capability
   public :: capnp_dyn_get_any_struct, capnp_dyn_get_any_list
   public :: capnp_dyn_select, capnp_dyn_which, capnp_dyn_field_type
   public :: capnp_dyn_brand_t, capnp_dyn_brand_scope_t, capnp_dyn_brand_binding_t
   public :: capnp_dyn_field_schema, capnp_dyn_load_brand
   public :: capnp_dyn_annotation_t, capnp_dyn_node_annotations
   public :: capnp_dyn_field_annotations, capnp_dyn_enumerant_annotations
   public :: capnp_dyn_method_annotations

   !> A loaded compiled schema: the CGR message plus a node index.
   type :: capnp_dyn_schema_t
      type(capnp_message_t) :: cgr
      type(capnp_ptr_t), allocatable :: nodes(:)
      logical :: loaded = .false.
   end type capnp_dyn_schema_t

   !> An annotation use as exposed by the C++ schema API. `value` and
   !> `brand` are handles into the loaded CodeGeneratorRequest and remain
   !> valid until capnp_dyn_free() is called on the owning schema.
   type :: capnp_dyn_annotation_t
      integer(int64) :: id = 0_int64
      type(capnp_ptr_t) :: value
      type(capnp_ptr_t) :: brand
   end type capnp_dyn_annotation_t

   type :: capnp_dyn_brand_binding_t
      logical :: unbound = .false.
      type(capnp_ptr_t) :: type
   end type capnp_dyn_brand_binding_t

   type :: capnp_dyn_brand_scope_t
      integer(int64) :: id = 0_int64
      logical :: inherited = .false.
      type(capnp_dyn_brand_binding_t), allocatable :: bindings(:)
   end type capnp_dyn_brand_scope_t

   type :: capnp_dyn_brand_t
      type(capnp_dyn_brand_scope_t), allocatable :: scopes(:)
   end type capnp_dyn_brand_t

contains

   !> Load a framed CodeGeneratorRequest (the bytes `capnp compile -o-`
   !> writes).
   subroutine capnp_dyn_load(schema, bytes, err)
      type(capnp_dyn_schema_t), intent(inout), target :: schema
      integer(int8), intent(in) :: bytes(0:)
      integer, intent(out) :: err
      type(capnp_ptr_t) :: root, l
      integer(int64) :: n, i
      call capnp_dyn_free(schema)
      ! Schema graphs are deep; match the plugin's generous reader guards.
      call capnp_deserialize_bytes(bytes, schema%cgr, err, &
                                   traversal_words=1073741824_int64, depth_limit=256)
      if (err /= CAPNP_OK) return
      root = capnp_root(schema%cgr, err)
      if (err /= CAPNP_OK) return
      l = cgr_nodes(root, err)
      if (err /= CAPNP_OK) return
      n = capnp_list_len(l)
      allocate (schema%nodes(n))
      do i = 1_int64, n
         schema%nodes(i) = capnp_list_get_struct(l, int(i - 1), err)
         if (err /= CAPNP_OK) return
      end do
      schema%loaded = .true.
   end subroutine capnp_dyn_load

   subroutine capnp_dyn_free(schema)
      type(capnp_dyn_schema_t), intent(inout) :: schema
      call capnp_message_free(schema%cgr)
      if (allocated(schema%nodes)) deallocate (schema%nodes)
      schema%loaded = .false.
   end subroutine capnp_dyn_free

   !> Node index for a type name: an exact displayName match, or the
   !> part after the file prefix ('Person' finds
   !> 'schema/addressbook.capnp:Person'). Prefers NODE_STRUCT over other
   !> kinds when several names collide. 0 when absent.
   function capnp_dyn_find(schema, name, err) result(idx)
      type(capnp_dyn_schema_t), intent(in) :: schema
      character(len=*), intent(in) :: name
      integer, intent(out) :: err
      integer :: idx, fallback
      character(len=:), allocatable :: dn, leaf
      integer :: i, colon
      err = CAPNP_OK
      idx = 0
      fallback = 0
      if (.not. schema%loaded) then
         err = CAPNP_ERR_ARG
         return
      end if
      do i = 1, size(schema%nodes)
         call node_display_name(schema%nodes(i), dn, err)
         if (err /= CAPNP_OK) return
         leaf = dn
         colon = index(dn, ':')
         if (colon > 0) leaf = dn(colon + 1:)
         if (dn /= name .and. leaf /= name) cycle
         ! Prefer a struct node (the usual type lookup target).
         if (node_which(schema%nodes(i)) == NODE_STRUCT) then
            idx = i
            return
         end if
         if (fallback == 0) fallback = i
      end do
      idx = fallback
   end function capnp_dyn_find

   function dyn_node_by_id(schema, id) result(idx)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer(int64), intent(in) :: id
      integer :: idx, i
      idx = 0
      do i = 1, size(schema%nodes)
         if (node_id(schema%nodes(i)) == id) then
            idx = i
            return
         end if
      end do
   end function dyn_node_by_id

   function capnp_dyn_new_root(schema, idx, msg, err) result(p)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      type(capnp_message_t), intent(inout), target :: msg
      integer, intent(out) :: err
      type(capnp_ptr_t) :: p
      p = capnp_ptr_t()
      if (.not. schema%loaded .or. idx < 1 .or. idx > size(schema%nodes) .or. &
          node_which(schema%nodes(idx)) /= NODE_STRUCT) then
         err = CAPNP_ERR_ARG
         return
      end if
      p = capnp_new_struct(msg, node_struct_data_words(schema%nodes(idx)), &
                           node_struct_pointer_count(schema%nodes(idx)), err)
      if (err == CAPNP_OK) call capnp_set_root(msg, p, err)
   end function capnp_dyn_new_root

   !> The named field's schema entry within struct node idx; searches
   !> group members through their group nodes is NOT done here -- name
   !> lookup is per-node, matching the C++ DynamicStruct behaviour of
   !> addressing groups as fields.
   subroutine dyn_field(schema, idx, fname, f, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      character(len=*), intent(in) :: fname
      type(capnp_ptr_t), intent(out) :: f
      integer, intent(out) :: err
      type(capnp_ptr_t) :: fl
      character(len=:), allocatable :: nm
      integer(int64) :: i
      err = CAPNP_OK
      if (.not. schema%loaded .or. .not. allocated(schema%nodes)) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (idx < 1 .or. idx > size(schema%nodes)) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (node_which(schema%nodes(idx)) /= NODE_STRUCT) then
         err = CAPNP_ERR_ARG
         return
      end if
      fl = node_struct_fields(schema%nodes(idx), err)
      if (err /= CAPNP_OK) return
      do i = 0_int64, capnp_list_len(fl) - 1_int64
         f = capnp_list_get_struct(fl, int(i), err)
         if (err /= CAPNP_OK) return
         call field_name(f, nm, err)
         if (err /= CAPNP_OK) return
         if (nm == fname) return
      end do
      err = CAPNP_ERR_ARG
   end subroutine dyn_field

   function capnp_dyn_field_schema(schema, idx, fname, err) result(t)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      character(len=*), intent(in) :: fname
      integer, intent(out) :: err
      type(capnp_ptr_t) :: t, f
      t = capnp_ptr_t()
      call dyn_field(schema, idx, fname, f, err)
      if (err /= CAPNP_OK) return
      if (field_which(f) == FIELD_GROUP) then
         err = CAPNP_ERR_KIND
         return
      end if
      t = field_slot_type(f, err)
   end function capnp_dyn_field_schema

   subroutine capnp_dyn_load_brand(t, brand, err)
      type(capnp_ptr_t), intent(in) :: t
      type(capnp_dyn_brand_t), intent(out) :: brand
      integer, intent(out) :: err
      type(capnp_ptr_t) :: raw, scopes, scope, bindings, binding
      integer(int64) :: i, j
      err = CAPNP_OK
      if (allocated(brand%scopes)) deallocate (brand%scopes)
      raw = type_brand(t, err)
      if (err /= CAPNP_OK) return
      if (raw%kind == CAPNP_PK_NULL) then
         allocate (brand%scopes(0))
         return
      end if
      scopes = brand_scopes(raw, err)
      if (err /= CAPNP_OK) return
      allocate (brand%scopes(capnp_list_len(scopes)))
      do i = 0_int64, capnp_list_len(scopes) - 1_int64
         scope = capnp_list_get_struct(scopes, int(i), err)
         if (err /= CAPNP_OK) return
         brand%scopes(i + 1)%id = scope_scope_id(scope)
         select case (scope_which(scope))
         case (SCOPE_INHERIT)
            brand%scopes(i + 1)%inherited = .true.
            allocate (brand%scopes(i + 1)%bindings(0))
            cycle
         case (SCOPE_BIND)
            brand%scopes(i + 1)%inherited = .false.
         case default
            err = CAPNP_ERR_KIND
            return
         end select
         bindings = scope_bindings(scope, err)
         if (err /= CAPNP_OK) return
         allocate (brand%scopes(i + 1)%bindings(capnp_list_len(bindings)))
         do j = 0_int64, capnp_list_len(bindings) - 1_int64
            binding = capnp_list_get_struct(bindings, int(j), err)
            if (err /= CAPNP_OK) return
            select case (binding_which(binding))
            case (BINDING_UNBOUND)
               brand%scopes(i + 1)%bindings(j + 1)%unbound = .true.
            case (BINDING_BOUND_TYPE)
               brand%scopes(i + 1)%bindings(j + 1)%unbound = .false.
               brand%scopes(i + 1)%bindings(j + 1)%type = binding_type(binding, err)
               if (err /= CAPNP_OK) return
            case default
               err = CAPNP_ERR_KIND
               return
            end select
         end do
      end do
   end subroutine capnp_dyn_load_brand

   subroutine dyn_select_field(schema, idx, p, f, err)
      use capnp_union, only: capnp_set_which
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      type(capnp_ptr_t), intent(in) :: p, f
      integer, intent(out) :: err
      integer :: disc
      err = CAPNP_OK
      if (field_which(f) == FIELD_GROUP) return
      disc = field_discriminant(f)
      if (disc == NO_DISCRIMINANT) return
      if (node_struct_discriminant_count(schema%nodes(idx)) <= 0) then
         err = CAPNP_ERR_KIND
         return
      end if
      call capnp_set_which(p, int(node_struct_discriminant_offset(schema%nodes(idx))), disc, err)
   end subroutine dyn_select_field

   subroutine dyn_annotations(list, annotations, err)
      type(capnp_ptr_t), intent(in) :: list
      type(capnp_dyn_annotation_t), allocatable, intent(out) :: annotations(:)
      integer, intent(out) :: err
      type(capnp_ptr_t) :: a
      integer(int64) :: i, n
      err = CAPNP_OK
      n = capnp_list_len(list)
      allocate (annotations(n))
      do i = 0_int64, n - 1_int64
         a = capnp_list_get_struct(list, int(i), err)
         if (err /= CAPNP_OK) return
         annotations(i + 1)%id = annotation_id(a)
         annotations(i + 1)%value = annotation_value(a, err)
         if (err /= CAPNP_OK) return
         annotations(i + 1)%brand = annotation_brand(a, err)
         if (err /= CAPNP_OK) return
      end do
   end subroutine dyn_annotations

   !> Annotations applied directly to a schema node, including files, types,
   !> constants, interfaces, and annotation declarations.
   subroutine capnp_dyn_node_annotations(schema, idx, annotations, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      type(capnp_dyn_annotation_t), allocatable, intent(out) :: annotations(:)
      integer, intent(out) :: err
      type(capnp_ptr_t) :: list
      if (.not. schema%loaded .or. idx < 1 .or. idx > size(schema%nodes)) then
         err = CAPNP_ERR_ARG
         allocate (annotations(0))
         return
      end if
      list = node_annotations(schema%nodes(idx), err)
      if (err /= CAPNP_OK) then
         allocate (annotations(0))
         return
      end if
      call dyn_annotations(list, annotations, err)
   end subroutine capnp_dyn_node_annotations

   !> Annotations applied to a named struct field, including group fields.
   subroutine capnp_dyn_field_annotations(schema, idx, fname, annotations, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      character(len=*), intent(in) :: fname
      type(capnp_dyn_annotation_t), allocatable, intent(out) :: annotations(:)
      integer, intent(out) :: err
      type(capnp_ptr_t) :: f, list
      call dyn_field(schema, idx, fname, f, err)
      if (err /= CAPNP_OK) then
         allocate (annotations(0))
         return
      end if
      list = field_annotations(f, err)
      if (err /= CAPNP_OK) then
         allocate (annotations(0))
         return
      end if
      call dyn_annotations(list, annotations, err)
   end subroutine capnp_dyn_field_annotations

   !> Annotations applied to an enumerant selected by its schema name.
   subroutine capnp_dyn_enumerant_annotations(schema, idx, name, annotations, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      character(len=*), intent(in) :: name
      type(capnp_dyn_annotation_t), allocatable, intent(out) :: annotations(:)
      integer, intent(out) :: err
      type(capnp_ptr_t) :: enums, en, list
      character(len=:), allocatable :: ename
      integer(int64) :: i
      if (.not. schema%loaded .or. idx < 1 .or. idx > size(schema%nodes) .or. &
          node_which(schema%nodes(idx)) /= NODE_ENUM) then
         err = CAPNP_ERR_ARG
         allocate (annotations(0))
         return
      end if
      enums = node_enumerants(schema%nodes(idx), err)
      if (err /= CAPNP_OK) then
         allocate (annotations(0))
         return
      end if
      do i = 0_int64, capnp_list_len(enums) - 1_int64
         en = capnp_list_get_struct(enums, int(i), err)
         if (err /= CAPNP_OK) exit
         call enumerant_name(en, ename, err)
         if (err /= CAPNP_OK) exit
         if (ename /= name) cycle
         list = enumerant_annotations(en, err)
         if (err == CAPNP_OK) call dyn_annotations(list, annotations, err)
         return
      end do
      if (.not. allocated(annotations)) allocate (annotations(0))
      if (err == CAPNP_OK) err = CAPNP_ERR_ARG
   end subroutine capnp_dyn_enumerant_annotations

   !> Annotations applied to an interface method selected by its schema name.
   subroutine capnp_dyn_method_annotations(schema, idx, name, annotations, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      character(len=*), intent(in) :: name
      type(capnp_dyn_annotation_t), allocatable, intent(out) :: annotations(:)
      integer, intent(out) :: err
      type(capnp_ptr_t) :: methods, method, list
      character(len=:), allocatable :: mname
      integer(int64) :: i
      if (.not. schema%loaded .or. idx < 1 .or. idx > size(schema%nodes) .or. &
          node_which(schema%nodes(idx)) /= NODE_INTERFACE) then
         err = CAPNP_ERR_ARG
         allocate (annotations(0))
         return
      end if
      methods = node_interface_methods(schema%nodes(idx), err)
      if (err /= CAPNP_OK) then
         allocate (annotations(0))
         return
      end if
      do i = 0_int64, capnp_list_len(methods) - 1_int64
         method = capnp_list_get_struct(methods, int(i), err)
         if (err /= CAPNP_OK) exit
         call method_name(method, mname, err)
         if (err /= CAPNP_OK) exit
         if (mname /= name) cycle
         list = method_annotations(method, err)
         if (err == CAPNP_OK) call dyn_annotations(list, annotations, err)
         return
      end do
      if (.not. allocated(annotations)) allocate (annotations(0))
      if (err == CAPNP_OK) err = CAPNP_ERR_ARG
   end subroutine capnp_dyn_method_annotations

   !> Wire type tag (TYPE_*) of a named field; TYPE_STRUCT for groups.
   function capnp_dyn_field_type(schema, idx, fname, err) result(tw)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      character(len=*), intent(in) :: fname
      integer, intent(out) :: err
      integer :: tw
      type(capnp_ptr_t) :: f, t
      tw = -1
      call dyn_field(schema, idx, fname, f, err)
      if (err /= CAPNP_OK) return
      if (field_which(f) == FIELD_GROUP) then
         tw = TYPE_STRUCT
         return
      end if
      t = field_slot_type(f, err)
      if (err == CAPNP_OK) tw = type_which(t)
   end function capnp_dyn_field_type

   !> Integer-family read (ints, uints, enum), widened to int64, with
   !> the schema default applied.
   function capnp_dyn_get_int(schema, idx, p, fname, err) result(v)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      type(capnp_ptr_t), intent(in) :: p
      character(len=*), intent(in) :: fname
      integer, intent(out) :: err
      integer(int64) :: v
      type(capnp_ptr_t) :: f, t, dv
      integer(int64) :: off
      v = 0_int64
      call dyn_field(schema, idx, fname, f, err)
      if (err /= CAPNP_OK) return
      t = field_slot_type(f, err)
      if (err /= CAPNP_OK) return
      dv = field_slot_default(f, err)
      if (err /= CAPNP_OK) return
      off = field_slot_offset(f)
      select case (type_which(t))
      case (TYPE_INT8)
         v = int(capnp_get_i8(p, off, default=int(value_i8(dv), int8)), int64)
      case (TYPE_UINT8)
         v = int(capnp_get_u8(p, off, default=value_u8(dv)), int64)
      case (TYPE_INT16)
         v = int(capnp_get_i16(p, off*2_int64, default=value_i16(dv)), int64)
      case (TYPE_UINT16, TYPE_ENUM)
         v = int(capnp_get_u16(p, off*2_int64, default=value_u16(dv)), int64)
      case (TYPE_INT32)
         v = int(capnp_get_i32(p, off*4_int64, default=value_i32(dv)), int64)
      case (TYPE_UINT32)
         v = capnp_get_u32(p, off*4_int64, default=value_u32(dv))
      case (TYPE_INT64, TYPE_UINT64)
         v = capnp_get_i64(p, off*8_int64, default=value_i64(dv))
      case default
         err = CAPNP_ERR_KIND
      end select
   end function capnp_dyn_get_int

   function capnp_dyn_get_f64(schema, idx, p, fname, err) result(v)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      type(capnp_ptr_t), intent(in) :: p
      character(len=*), intent(in) :: fname
      integer, intent(out) :: err
      real(real64) :: v
      type(capnp_ptr_t) :: f, t, dv
      integer(int64) :: off
      v = 0.0_real64
      call dyn_field(schema, idx, fname, f, err)
      if (err /= CAPNP_OK) return
      t = field_slot_type(f, err)
      if (err /= CAPNP_OK) return
      dv = field_slot_default(f, err)
      if (err /= CAPNP_OK) return
      off = field_slot_offset(f)
      select case (type_which(t))
      case (TYPE_FLOAT32)
         v = real(capnp_get_f32(p, off*4_int64, default=value_f32(dv)), real64)
      case (TYPE_FLOAT64)
         v = capnp_get_f64(p, off*8_int64, default=value_f64(dv))
      case default
         err = CAPNP_ERR_KIND
      end select
   end function capnp_dyn_get_f64

   function capnp_dyn_get_bool(schema, idx, p, fname, err) result(v)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      type(capnp_ptr_t), intent(in) :: p
      character(len=*), intent(in) :: fname
      integer, intent(out) :: err
      logical :: v
      type(capnp_ptr_t) :: f, t, dv
      v = .false.
      call dyn_field(schema, idx, fname, f, err)
      if (err /= CAPNP_OK) return
      t = field_slot_type(f, err)
      if (err /= CAPNP_OK) return
      if (type_which(t) /= TYPE_BOOL) then
         err = CAPNP_ERR_KIND
         return
      end if
      dv = field_slot_default(f, err)
      if (err /= CAPNP_OK) return
      v = capnp_get_bool(p, field_slot_offset(f), default=value_bool(dv))
   end function capnp_dyn_get_bool

   subroutine capnp_dyn_get_text(schema, idx, p, fname, s, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      type(capnp_ptr_t), intent(in) :: p
      character(len=*), intent(in) :: fname
      character(len=:), allocatable, intent(out) :: s
      integer, intent(out) :: err
      type(capnp_ptr_t) :: f, t, dv, q
      call dyn_field(schema, idx, fname, f, err)
      if (err /= CAPNP_OK) return
      t = field_slot_type(f, err)
      if (err /= CAPNP_OK) return
      if (type_which(t) /= TYPE_TEXT) then
         err = CAPNP_ERR_KIND
         return
      end if
      q = capnp_getp(p, int(field_slot_offset(f)), err)
      if (err /= CAPNP_OK) return
      if (q%kind /= CAPNP_PK_NULL) then
         call capnp_get_text(p, int(field_slot_offset(f)), s, err)
         return
      end if
      dv = field_slot_default(f, err)
      if (err /= CAPNP_OK) return
      call value_text(dv, s, err)
   end subroutine capnp_dyn_get_text

   subroutine capnp_dyn_get_data(schema, idx, p, fname, bytes, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      type(capnp_ptr_t), intent(in) :: p
      character(len=*), intent(in) :: fname
      integer(int8), allocatable, intent(out) :: bytes(:)
      integer, intent(out) :: err
      type(capnp_ptr_t) :: f, t, dv, q
      call dyn_field(schema, idx, fname, f, err)
      if (err /= CAPNP_OK) return
      t = field_slot_type(f, err)
      if (err /= CAPNP_OK) return
      if (type_which(t) /= TYPE_DATA) then
         err = CAPNP_ERR_KIND
         allocate (bytes(0:-1))
         return
      end if
      q = capnp_getp(p, int(field_slot_offset(f)), err)
      if (err /= CAPNP_OK) return
      if (q%kind /= CAPNP_PK_NULL) then
         call capnp_get_data(p, int(field_slot_offset(f)), bytes, err)
         return
      end if
      dv = field_slot_default(f, err)
      if (err /= CAPNP_OK) return
      call value_data(dv, bytes, err)
   end subroutine capnp_dyn_get_data

   !> Resolved pointer field (Text, Data, List, struct, anyPointer). For
   !> groups the parent handle itself addresses the members, so groups
   !> return p unchanged.
   function capnp_dyn_getp(schema, idx, p, fname, err) result(q)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      type(capnp_ptr_t), intent(in) :: p
      character(len=*), intent(in) :: fname
      integer, intent(out) :: err
      type(capnp_ptr_t) :: q
      type(capnp_ptr_t) :: f, t, dv
      call dyn_field(schema, idx, fname, f, err)
      if (err /= CAPNP_OK) return
      if (field_which(f) == FIELD_GROUP) then
         q = p
         return
      end if
      t = field_slot_type(f, err)
      if (err /= CAPNP_OK) return
      select case (type_which(t))
      case (TYPE_TEXT, TYPE_DATA, TYPE_LIST, TYPE_STRUCT, TYPE_INTERFACE, TYPE_ANY_POINTER, &
            TYPE_ANY_STRUCT, TYPE_ANY_LIST, TYPE_CAPABILITY)
      case default
         err = CAPNP_ERR_KIND
         return
      end select
      q = capnp_getp(p, int(field_slot_offset(f)), err)
      if (err /= CAPNP_OK .or. q%kind /= CAPNP_PK_NULL) return
      dv = field_slot_default(f, err)
      if (err /= CAPNP_OK) return
      q = value_pointer(dv, err)
   end function capnp_dyn_getp

   !> Union discriminant of struct node idx read from p.
   !>
   !> Cap'n Proto may report discriminantCount on the struct itself, or only
   !> on an anonymous-union group node that shares the parent's data section
   !> (addressbook Person.employment). When the parent count is zero, scan
   !> group fields for a child with a non-zero count and use that offset.
   !>
   !> Optional group names a union field (e.g. 'employment', 'primary') so
   !> multi-union structs are unambiguous. When omitted and several union
   !> groups exist, CAPNP_ERR_ARG is returned unless the parent itself has
   !> a non-zero discriminantCount.
   function capnp_dyn_which(schema, idx, p, err, group) result(tag)
      use capnp_union, only: capnp_which
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(out) :: err
      character(len=*), intent(in), optional :: group
      integer :: tag
      type(capnp_ptr_t) :: fl, f
      character(len=:), allocatable :: fn
      integer(int64) :: i, disc_off, want_id
      integer :: gidx, disc_count, n_union
      logical :: named
      err = CAPNP_OK
      tag = -1
      disc_off = -1_int64
      n_union = 0
      named = present(group)
      if (idx < 1 .or. idx > size(schema%nodes)) then
         err = CAPNP_ERR_ARG
         return
      end if
      disc_count = node_struct_discriminant_count(schema%nodes(idx))
      if (disc_count > 0 .and. .not. named) then
         disc_off = node_struct_discriminant_offset(schema%nodes(idx))
      else
         fl = node_struct_fields(schema%nodes(idx), err)
         if (err /= CAPNP_OK) return
         do i = 0_int64, capnp_list_len(fl) - 1_int64
            f = capnp_list_get_struct(fl, int(i), err)
            if (err /= CAPNP_OK) return
            if (field_which(f) /= FIELD_GROUP) cycle
            call field_name(f, fn, err)
            if (err /= CAPNP_OK) return
            if (named) then
               if (fn /= group) cycle
            end if
            want_id = field_group_type_id(f)
            gidx = 0
            do gidx = 1, size(schema%nodes)
               if (node_id(schema%nodes(gidx)) == want_id) exit
            end do
            if (gidx < 1 .or. gidx > size(schema%nodes)) cycle
            if (node_id(schema%nodes(gidx)) /= want_id) cycle
            disc_count = node_struct_discriminant_count(schema%nodes(gidx))
            if (disc_count <= 0) cycle
            n_union = n_union + 1
            if (named .or. n_union == 1) then
               disc_off = node_struct_discriminant_offset(schema%nodes(gidx))
            end if
            if (named) exit
         end do
         if (.not. named .and. n_union > 1) then
            err = CAPNP_ERR_ARG
            tag = -1
            return
         end if
      end if
      if (disc_off < 0_int64) then
         err = CAPNP_ERR_KIND
         return
      end if
      tag = capnp_which(p, int(disc_off))
   end function capnp_dyn_which

   !> Select a union member by its field name before initializing or assigning
   !> its value. The supplied node must be the struct or named-union group
   !> that owns the discriminant.
   subroutine capnp_dyn_select(schema, idx, p, fname, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      type(capnp_ptr_t), intent(in) :: p
      character(len=*), intent(in) :: fname
      integer, intent(out) :: err
      type(capnp_ptr_t) :: f
      call dyn_field(schema, idx, fname, f, err)
      if (err /= CAPNP_OK) return
      call dyn_select_field(schema, idx, p, f, err)
   end subroutine capnp_dyn_select

   subroutine capnp_dyn_set_int(schema, idx, p, fname, v, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      type(capnp_ptr_t), intent(in) :: p
      character(len=*), intent(in) :: fname
      integer(int64), intent(in) :: v
      integer, intent(out) :: err
      type(capnp_ptr_t) :: f, t, dv
      integer(int64) :: off
      call dyn_field(schema, idx, fname, f, err)
      if (err /= CAPNP_OK) return
      t = field_slot_type(f, err)
      if (err /= CAPNP_OK) return
      dv = field_slot_default(f, err)
      if (err /= CAPNP_OK) return
      select case (type_which(t))
      case (TYPE_INT8, TYPE_UINT8, TYPE_INT16, TYPE_UINT16, TYPE_INT32, TYPE_UINT32, &
            TYPE_INT64, TYPE_UINT64, TYPE_ENUM)
      case default
         err = CAPNP_ERR_KIND
         return
      end select
      call dyn_select_field(schema, idx, p, f, err)
      if (err /= CAPNP_OK) return
      off = field_slot_offset(f)
      select case (type_which(t))
      case (TYPE_INT8)
         call capnp_set_i8(p, off, int(v, int8), err, default=int(value_i8(dv), int8))
      case (TYPE_UINT8)
         call capnp_set_u8(p, off, int(v, int16), err, default=value_u8(dv))
      case (TYPE_INT16)
         call capnp_set_i16(p, off*2_int64, int(v, int16), err, default=value_i16(dv))
      case (TYPE_UINT16, TYPE_ENUM)
         call capnp_set_u16(p, off*2_int64, int(v, int32), err, default=value_u16(dv))
      case (TYPE_INT32)
         call capnp_set_i32(p, off*4_int64, int(v, int32), err, default=value_i32(dv))
      case (TYPE_UINT32)
         call capnp_set_u32(p, off*4_int64, v, err, default=value_u32(dv))
      case (TYPE_INT64, TYPE_UINT64)
         call capnp_set_i64(p, off*8_int64, v, err, default=value_i64(dv))
      case default
         err = CAPNP_ERR_KIND
      end select
   end subroutine capnp_dyn_set_int

   subroutine capnp_dyn_set_bool(schema, idx, p, fname, v, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      type(capnp_ptr_t), intent(in) :: p
      character(len=*), intent(in) :: fname
      logical, intent(in) :: v
      integer, intent(out) :: err
      type(capnp_ptr_t) :: f, t, dv
      call dyn_field(schema, idx, fname, f, err)
      if (err /= CAPNP_OK) return
      t = field_slot_type(f, err)
      if (err /= CAPNP_OK) return
      if (type_which(t) /= TYPE_BOOL) then
         err = CAPNP_ERR_KIND
         return
      end if
      dv = field_slot_default(f, err)
      if (err /= CAPNP_OK) return
      call dyn_select_field(schema, idx, p, f, err)
      if (err /= CAPNP_OK) return
      call capnp_set_bool(p, field_slot_offset(f), v, err, default=value_bool(dv))
   end subroutine capnp_dyn_set_bool

   subroutine capnp_dyn_set_f64(schema, idx, p, fname, v, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      type(capnp_ptr_t), intent(in) :: p
      character(len=*), intent(in) :: fname
      real(real64), intent(in) :: v
      integer, intent(out) :: err
      type(capnp_ptr_t) :: f, t, dv
      integer(int64) :: off
      call dyn_field(schema, idx, fname, f, err)
      if (err /= CAPNP_OK) return
      t = field_slot_type(f, err)
      if (err /= CAPNP_OK) return
      dv = field_slot_default(f, err)
      if (err /= CAPNP_OK) return
      select case (type_which(t))
      case (TYPE_FLOAT32, TYPE_FLOAT64)
      case default
         err = CAPNP_ERR_KIND
         return
      end select
      call dyn_select_field(schema, idx, p, f, err)
      if (err /= CAPNP_OK) return
      off = field_slot_offset(f)
      select case (type_which(t))
      case (TYPE_FLOAT32)
         call capnp_set_f32(p, off*4_int64, real(v, real32), err, default=value_f32(dv))
      case (TYPE_FLOAT64)
         call capnp_set_f64(p, off*8_int64, v, err, default=value_f64(dv))
      case default
         err = CAPNP_ERR_KIND
      end select
   end subroutine capnp_dyn_set_f64

   subroutine capnp_dyn_set_text(schema, idx, p, fname, s, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      type(capnp_ptr_t), intent(in) :: p
      character(len=*), intent(in) :: fname
      character(len=*), intent(in) :: s
      integer, intent(out) :: err
      type(capnp_ptr_t) :: f, t
      call dyn_field(schema, idx, fname, f, err)
      if (err /= CAPNP_OK) return
      t = field_slot_type(f, err)
      if (err /= CAPNP_OK) return
      if (type_which(t) /= TYPE_TEXT) then
         err = CAPNP_ERR_KIND
         return
      end if
      call dyn_select_field(schema, idx, p, f, err)
      if (err /= CAPNP_OK) return
      call capnp_set_text(p, int(field_slot_offset(f)), s, err)
   end subroutine capnp_dyn_set_text

   subroutine capnp_dyn_set_data(schema, idx, p, fname, bytes, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      type(capnp_ptr_t), intent(in) :: p
      character(len=*), intent(in) :: fname
      integer(int8), intent(in) :: bytes(:)
      integer, intent(out) :: err
      type(capnp_ptr_t) :: f, t
      call dyn_field(schema, idx, fname, f, err)
      if (err /= CAPNP_OK) return
      t = field_slot_type(f, err)
      if (err /= CAPNP_OK) return
      if (type_which(t) /= TYPE_DATA) then
         err = CAPNP_ERR_KIND
         return
      end if
      call dyn_select_field(schema, idx, p, f, err)
      if (err /= CAPNP_OK) return
      call capnp_set_data(p, int(field_slot_offset(f)), bytes, err)
   end subroutine capnp_dyn_set_data

   subroutine capnp_dyn_setp(schema, idx, p, fname, value, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      type(capnp_ptr_t), intent(in) :: p, value
      character(len=*), intent(in) :: fname
      integer, intent(out) :: err
      type(capnp_ptr_t) :: f, t
      call dyn_field(schema, idx, fname, f, err)
      if (err /= CAPNP_OK) return
      t = field_slot_type(f, err)
      if (err /= CAPNP_OK) return
      select case (type_which(t))
      case (TYPE_TEXT, TYPE_DATA, TYPE_LIST, TYPE_STRUCT, TYPE_INTERFACE, TYPE_ANY_POINTER, &
            TYPE_ANY_STRUCT, TYPE_ANY_LIST, TYPE_CAPABILITY)
         if (value%kind /= CAPNP_PK_NULL) then
            select case (type_which(t))
            case (TYPE_STRUCT, TYPE_ANY_STRUCT)
               if (value%kind /= CAPNP_PK_STRUCT) then
                  err = CAPNP_ERR_KIND
                  return
               end if
            case (TYPE_LIST, TYPE_ANY_LIST)
               if (value%kind /= CAPNP_PK_LIST) then
                  err = CAPNP_ERR_KIND
                  return
               end if
            case (TYPE_TEXT, TYPE_DATA)
               if (value%kind /= CAPNP_PK_LIST .or. value%esize /= CAPNP_SZ_BYTE) then
                  err = CAPNP_ERR_KIND
                  return
               end if
            case (TYPE_INTERFACE, TYPE_CAPABILITY)
               if (value%kind /= CAPNP_PK_CAP) then
                  err = CAPNP_ERR_KIND
                  return
               end if
            end select
         end if
         call dyn_select_field(schema, idx, p, f, err)
         if (err /= CAPNP_OK) return
         call capnp_setp(p, int(field_slot_offset(f)), value, err)
      case default
         err = CAPNP_ERR_KIND
      end select
   end subroutine capnp_dyn_setp

   function capnp_dyn_get_capability(schema, idx, p, fname, err) result(capidx)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      type(capnp_ptr_t), intent(in) :: p
      character(len=*), intent(in) :: fname
      integer, intent(out) :: err
      integer(int64) :: capidx
      type(capnp_ptr_t) :: value, f, t
      capidx = 0_int64
      call dyn_field(schema, idx, fname, f, err)
      if (err /= CAPNP_OK) return
      t = field_slot_type(f, err)
      if (err /= CAPNP_OK) return
      if (type_which(t) /= TYPE_INTERFACE .and. type_which(t) /= TYPE_CAPABILITY .and. &
          type_which(t) /= TYPE_ANY_POINTER) then
         err = CAPNP_ERR_KIND
         return
      end if
      value = capnp_getp(p, int(field_slot_offset(f)), err)
      if (err /= CAPNP_OK) return
      if (value%kind == CAPNP_PK_NULL) return
      if (value%kind /= CAPNP_PK_CAP) then
         err = CAPNP_ERR_KIND
         return
      end if
      capidx = value%capidx
   end function capnp_dyn_get_capability

   subroutine capnp_dyn_set_capability(schema, idx, p, fname, capidx, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      type(capnp_ptr_t), intent(in) :: p
      character(len=*), intent(in) :: fname
      integer(int64), intent(in) :: capidx
      integer, intent(out) :: err
      type(capnp_ptr_t) :: value
      if (capidx < 0_int64 .or. capidx > 4294967295_int64) then
         err = CAPNP_ERR_ARG
         return
      end if
      value = capnp_ptr_t()
      if (associated(p%msg)) value%msg => p%msg
      value%kind = CAPNP_PK_CAP
      value%capidx = capidx
      call capnp_dyn_setp(schema, idx, p, fname, value, err)
   end subroutine capnp_dyn_set_capability

   function capnp_dyn_get_any_struct(schema, idx, p, fname, err) result(value)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      type(capnp_ptr_t), intent(in) :: p
      character(len=*), intent(in) :: fname
      integer, intent(out) :: err
      type(capnp_ptr_t) :: value
      value = capnp_dyn_getp(schema, idx, p, fname, err)
      if (err /= CAPNP_OK .or. value%kind == CAPNP_PK_NULL) return
      if (value%kind /= CAPNP_PK_STRUCT) err = CAPNP_ERR_KIND
   end function capnp_dyn_get_any_struct

   function capnp_dyn_get_any_list(schema, idx, p, fname, err) result(value)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      type(capnp_ptr_t), intent(in) :: p
      character(len=*), intent(in) :: fname
      integer, intent(out) :: err
      type(capnp_ptr_t) :: value
      value = capnp_dyn_getp(schema, idx, p, fname, err)
      if (err /= CAPNP_OK .or. value%kind == CAPNP_PK_NULL) return
      if (value%kind /= CAPNP_PK_LIST) err = CAPNP_ERR_KIND
   end function capnp_dyn_get_any_list

   function capnp_dyn_init_struct(schema, idx, p, fname, err) result(value)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      type(capnp_ptr_t), intent(in) :: p
      character(len=*), intent(in) :: fname
      integer, intent(out) :: err
      type(capnp_ptr_t) :: value, f, t
      integer :: nidx
      value = capnp_ptr_t()
      call dyn_field(schema, idx, fname, f, err)
      if (err /= CAPNP_OK) return
      t = field_slot_type(f, err)
      if (err /= CAPNP_OK) return
      if (type_which(t) /= TYPE_STRUCT) then
         err = CAPNP_ERR_KIND
         return
      end if
      nidx = dyn_node_by_id(schema, type_type_id(t))
      if (nidx == 0) then
         err = CAPNP_ERR_ARG
         return
      end if
      call dyn_select_field(schema, idx, p, f, err)
      if (err /= CAPNP_OK) return
      value = capnp_new_struct(p%msg, node_struct_data_words(schema%nodes(nidx)), &
                               node_struct_pointer_count(schema%nodes(nidx)), err)
      if (err == CAPNP_OK) call capnp_setp(p, int(field_slot_offset(f)), value, err)
   end function capnp_dyn_init_struct

   function capnp_dyn_init_list(schema, idx, p, fname, n, err) result(value)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      type(capnp_ptr_t), intent(in) :: p
      character(len=*), intent(in) :: fname
      integer(int64), intent(in) :: n
      integer, intent(out) :: err
      type(capnp_ptr_t) :: value, f, t, elem
      integer :: nidx, esize
      value = capnp_ptr_t()
      call dyn_field(schema, idx, fname, f, err)
      if (err /= CAPNP_OK) return
      t = field_slot_type(f, err)
      if (err /= CAPNP_OK) return
      if (type_which(t) /= TYPE_LIST) then
         err = CAPNP_ERR_KIND
         return
      end if
      elem = type_list_element(t, err)
      if (err /= CAPNP_OK) return
      if (n < 0_int64) then
         err = CAPNP_ERR_ARG
         return
      end if
      call dyn_select_field(schema, idx, p, f, err)
      if (err /= CAPNP_OK) return
      if (type_which(elem) == TYPE_STRUCT) then
         nidx = dyn_node_by_id(schema, type_type_id(elem))
         if (nidx == 0) then
            err = CAPNP_ERR_ARG
            return
         end if
         value = capnp_new_composite_list(p%msg, n, node_struct_data_words(schema%nodes(nidx)), &
                                          node_struct_pointer_count(schema%nodes(nidx)), err)
      else
         select case (type_which(elem))
         case (TYPE_VOID); esize = CAPNP_SZ_VOID
         case (TYPE_BOOL); esize = CAPNP_SZ_BIT
         case (TYPE_INT8, TYPE_UINT8); esize = CAPNP_SZ_BYTE
         case (TYPE_INT16, TYPE_UINT16, TYPE_ENUM); esize = CAPNP_SZ_TWO
         case (TYPE_INT32, TYPE_UINT32, TYPE_FLOAT32); esize = CAPNP_SZ_FOUR
         case (TYPE_INT64, TYPE_UINT64, TYPE_FLOAT64); esize = CAPNP_SZ_EIGHT
         case default; esize = CAPNP_SZ_PTR
         end select
         value = capnp_new_list(p%msg, esize, n, err)
      end if
      if (err == CAPNP_OK) call capnp_setp(p, int(field_slot_offset(f)), value, err)
   end function capnp_dyn_init_list

end module capnp_dynamic
