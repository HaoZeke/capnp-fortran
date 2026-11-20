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

   ! Groups share their parent's pointer section, so a chain of them does
   ! not count toward the reader nesting limit of 64, and capnp 1.4.0
   ! compiles chains of hundreds. Stop only before a cyclic graph in a
   ! corrupt schema exhausts the stack.
   integer, parameter :: SCHEMA_WALK_LIMIT = 4096

   !> A loaded compiled schema: the CGR message plus a node index.
   type :: capnp_dyn_schema_t
      type(capnp_message_t) :: cgr
      type(capnp_ptr_t), allocatable :: nodes(:)
      logical :: loaded = .false.
      !> One frozen copy of each defaulted pointer handed to readers, keyed
      !> by node, slot and field: union arms share a slot but not a default.
      !> The schema message itself stays untouched.
      !> Freed with the schema. Further reads reuse the copy, so a reader
      !> loop does not grow the arena.
      type(capnp_message_t) :: ro_arena
      logical :: ro_ready = .false.
      !> Parallel cache of reader copies. ro_node(i), ro_slot(i), the field
      !> location ro_fseg(i) and ro_foff(i), and ro_view(i) describe one
      !> field. n_ro is the live prefix.
      integer, allocatable :: ro_node(:), ro_fseg(:)
      integer(int64), allocatable :: ro_slot(:), ro_foff(:)
      type(capnp_ptr_t), allocatable :: ro_view(:)
      integer :: n_ro = 0
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
      !> Raw type pointer from the schema brand. Stays as stored.
      type(capnp_ptr_t) :: type
      !> type_which of the binding after parameter references in this
      !> brand are followed. -1 when unbound or unresolved.
      integer :: resolved_which = -1
      !> Struct, enum, or interface id when the resolved type has one,
      !> or the element id of a list of those. 0 otherwise.
      integer(int64) :: resolved_id = 0_int64
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
   !> writes). traversal_words sets the reader budget (default 2**30 words).
   subroutine capnp_dyn_load(schema, bytes, err, traversal_words)
      type(capnp_dyn_schema_t), intent(inout), target :: schema
      integer(int8), intent(in) :: bytes(0:)
      integer, intent(out) :: err
      integer(int64), intent(in), optional :: traversal_words
      type(capnp_ptr_t) :: root, l
      integer(int64) :: n, i, budget
      integer :: stat
      call capnp_dyn_free(schema)
      ! Schema graphs are deep; match the plugin's generous reader guards.
      budget = 1073741824_int64
      if (present(traversal_words)) budget = traversal_words
      call capnp_deserialize_bytes(bytes, schema%cgr, err, &
                                   traversal_words=budget, depth_limit=256)
      if (err /= CAPNP_OK) then
         call capnp_dyn_free(schema)
         return
      end if
      root = capnp_root(schema%cgr, err)
      if (err /= CAPNP_OK) then
         call capnp_dyn_free(schema)
         return
      end if
      l = cgr_nodes(root, err)
      if (err /= CAPNP_OK) then
         call capnp_dyn_free(schema)
         return
      end if
      n = capnp_list_len(l)
      if (.not. wire_count_ok(l, n)) then
         err = CAPNP_ERR_ALLOC
         call capnp_dyn_free(schema)
         return
      end if
      allocate (schema%nodes(n), stat=stat)
      if (stat /= 0) then
         err = CAPNP_ERR_ALLOC
         call capnp_dyn_free(schema)
         return
      end if
      do i = 1_int64, n
         schema%nodes(i) = capnp_list_get_struct(l, int(i - 1), err)
         if (err /= CAPNP_OK) then
            call capnp_dyn_free(schema)
            return
         end if
      end do
      schema%loaded = .true.
   end subroutine capnp_dyn_load

   subroutine capnp_dyn_free(schema)
      type(capnp_dyn_schema_t), intent(inout) :: schema
      call capnp_message_free(schema%cgr)
      if (schema%ro_ready) call capnp_message_free(schema%ro_arena)
      schema%ro_ready = .false.
      schema%n_ro = 0
      if (allocated(schema%ro_node)) deallocate (schema%ro_node)
      if (allocated(schema%ro_slot)) deallocate (schema%ro_slot)
      if (allocated(schema%ro_fseg)) deallocate (schema%ro_fseg)
      if (allocated(schema%ro_foff)) deallocate (schema%ro_foff)
      if (allocated(schema%ro_view)) deallocate (schema%ro_view)
      if (allocated(schema%nodes)) deallocate (schema%nodes)
      schema%loaded = .false.
   end subroutine capnp_dyn_free

   !> A host array sized from a wire count. A reader charged a zero-size
   !> list one traversal word per element when it read the pointer, as C++
   !> amplifiedRead does, so its counts fit the configured budget. A
   !> builder has no budget.
   function wire_count_ok(p, n) result(ok)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: n
      logical :: ok
      if (n < 0_int64) then
         ok = .false.
         return
      end if
      ok = associated(p%msg) .or. n == 0_int64
   end function wire_count_ok

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
      if (.not. schema%loaded) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (.not. allocated(schema%nodes)) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (idx < 1) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (idx > size(schema%nodes)) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (node_which(schema%nodes(idx)) /= NODE_STRUCT) then
         err = CAPNP_ERR_ARG
         return
      end if
      p = capnp_new_struct(msg, node_struct_data_words(schema%nodes(idx)), &
                           node_struct_pointer_count(schema%nodes(idx)), err)
      if (err == CAPNP_OK) then
         call capnp_set_root(msg, p, err)
         if (err == CAPNP_OK) p%schema_id = node_id(schema%nodes(idx))
      end if
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
      if (.not. schema%loaded) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (.not. allocated(schema%nodes)) then
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
      integer(int64) :: i, j, nsc, nb
      integer :: stat
      err = CAPNP_OK
      if (allocated(brand%scopes)) deallocate (brand%scopes)
      if (t%kind /= CAPNP_PK_STRUCT) then
         err = CAPNP_ERR_KIND
         allocate (brand%scopes(0), stat=stat)
         if (stat /= 0) err = CAPNP_ERR_ALLOC
         return
      end if
      ! Brand is a pointer on struct, enum and interface only. Text, lists
      ! and anyPointer use that pointer for something else.
      select case (type_which(t))
      case (TYPE_STRUCT, TYPE_ENUM, TYPE_INTERFACE)
      case default
         err = CAPNP_ERR_KIND
         allocate (brand%scopes(0), stat=stat)
         if (stat /= 0) err = CAPNP_ERR_ALLOC
         return
      end select
      raw = type_brand(t, err)
      if (err /= CAPNP_OK) return
      if (raw%kind == CAPNP_PK_NULL) then
         allocate (brand%scopes(0), stat=stat)
         if (stat /= 0) err = CAPNP_ERR_ALLOC
         return
      end if
      scopes = brand_scopes(raw, err)
      if (err /= CAPNP_OK) return
      nsc = capnp_list_len(scopes)
      if (.not. wire_count_ok(scopes, nsc)) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      allocate (brand%scopes(nsc), stat=stat)
      if (stat /= 0) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      do i = 0_int64, nsc - 1_int64
         scope = capnp_list_get_struct(scopes, int(i), err)
         if (err /= CAPNP_OK) return
         brand%scopes(i + 1)%id = scope_scope_id(scope)
         select case (scope_which(scope))
         case (SCOPE_INHERIT)
            brand%scopes(i + 1)%inherited = .true.
            allocate (brand%scopes(i + 1)%bindings(0), stat=stat)
            if (stat /= 0) then
               err = CAPNP_ERR_ALLOC
               return
            end if
            cycle
         case (SCOPE_BIND)
            brand%scopes(i + 1)%inherited = .false.
         case default
            err = CAPNP_ERR_KIND
            return
         end select
         bindings = scope_bindings(scope, err)
         if (err /= CAPNP_OK) return
         nb = capnp_list_len(bindings)
         if (.not. wire_count_ok(bindings, nb)) then
            err = CAPNP_ERR_ALLOC
            return
         end if
         allocate (brand%scopes(i + 1)%bindings(nb), stat=stat)
         if (stat /= 0) then
            err = CAPNP_ERR_ALLOC
            return
         end if
         do j = 0_int64, nb - 1_int64
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
      call dyn_resolve_brand(brand, err)
   end subroutine capnp_dyn_load_brand

   !> Fill resolved_which and resolved_id from each raw binding.
   subroutine dyn_resolve_brand(brand, err)
      type(capnp_dyn_brand_t), intent(inout) :: brand
      integer, intent(out) :: err
      ! A parameter inside a binding names the enclosing scope, so it is
      ! never resolved against this brand. TestGenerics(Bar, Foo) inside
      ! TestGenerics would otherwise loop.
      type(capnp_dyn_brand_t) :: outer
      integer :: i, j
      err = CAPNP_OK
      if (.not. allocated(brand%scopes)) return
      do i = 1, size(brand%scopes)
         if (.not. allocated(brand%scopes(i)%bindings)) cycle
         do j = 1, size(brand%scopes(i)%bindings)
            if (brand%scopes(i)%bindings(j)%unbound) cycle
            call dyn_resolve_type(outer, brand%scopes(i)%bindings(j)%type, 0, &
                                  brand%scopes(i)%bindings(j)%resolved_which, &
                                  brand%scopes(i)%bindings(j)%resolved_id, err)
            if (err /= CAPNP_OK) return
         end do
      end do
   end subroutine dyn_resolve_brand

   !> Resolve one type, following a parameter to another scope in brand.
   recursive subroutine dyn_resolve_type(brand, t, depth, which, id, err)
      type(capnp_dyn_brand_t), intent(in) :: brand
      type(capnp_ptr_t), intent(in) :: t
      integer, intent(in) :: depth
      integer, intent(out) :: which
      integer(int64), intent(out) :: id
      integer, intent(out) :: err
      type(capnp_ptr_t) :: et
      integer :: ew
      which = -1
      id = 0_int64
      err = CAPNP_OK
      if (depth > SCHEMA_WALK_LIMIT) then
         err = CAPNP_ERR_DEPTH
         return
      end if
      if (t%kind == CAPNP_PK_NULL) return
      which = type_which(t)
      ! A discriminant with no arm is unset. Leaving the raw tag (anyPointer
      ! is 18) would claim a member that is not active.
      if (which < TYPE_VOID .or. which > TYPE_ANY_POINTER) then
         which = -1
         return
      end if
      select case (which)
      case (TYPE_STRUCT, TYPE_ENUM, TYPE_INTERFACE)
         id = type_type_id(t)
      case (TYPE_LIST)
         et = type_list_element(t, err)
         if (err /= CAPNP_OK) return
         ! The element may itself be a parameter of this brand.
         call dyn_resolve_type(brand, et, depth + 1, ew, id, err)
         if (err /= CAPNP_OK) return
         which = TYPE_LIST
         if (ew /= TYPE_STRUCT .and. ew /= TYPE_ENUM .and. ew /= TYPE_INTERFACE) id = 0_int64
      case (TYPE_ANY_POINTER)
         call dyn_resolve_anyptr(brand, t, depth, which, id, err)
      end select
   end subroutine dyn_resolve_type

   !> An anyPointer parameter with no binding, an unknown scope, or an
   !> index past the binding list stays AnyPointer. That is what the C++
   !> schema loader returns, so a type parameter added later does not
   !> break an older brand. Only an anyPointer arm that is not one of the
   !> three legal arms, or an unconstrained tag outside its range, is unset.
   recursive subroutine dyn_resolve_anyptr(brand, t, depth, which, id, err)
      type(capnp_dyn_brand_t), intent(in) :: brand
      type(capnp_ptr_t), intent(in) :: t
      integer, intent(in) :: depth
      integer, intent(inout) :: which
      integer(int64), intent(inout) :: id
      integer, intent(out) :: err
      integer :: aw, uw, i, idx
      err = CAPNP_OK
      aw = type_anyptr_which(t)
      select case (aw)
      case (ANYPTR_UNCONSTRAINED)
         uw = type_uncon_which(t)
         if (uw < UNCON_ANY .or. uw > UNCON_CAPABILITY) which = -1
      case (ANYPTR_IMPLICIT)
      case (ANYPTR_PARAMETER)
         if (.not. allocated(brand%scopes)) then
            which = TYPE_ANY_POINTER
            id = 0_int64
            return
         end if
         idx = type_param_index(t) + 1
         do i = 1, size(brand%scopes)
            if (brand%scopes(i)%inherited) cycle
            if (brand%scopes(i)%id /= type_param_scope_id(t)) cycle
            if (.not. allocated(brand%scopes(i)%bindings)) then
               which = TYPE_ANY_POINTER
               id = 0_int64
               return
            end if
            if (idx < 1 .or. idx > size(brand%scopes(i)%bindings)) then
               which = TYPE_ANY_POINTER
               id = 0_int64
               return
            end if
            if (brand%scopes(i)%bindings(idx)%unbound) then
               which = TYPE_ANY_POINTER
               id = 0_int64
               return
            end if
            call dyn_resolve_type(brand, brand%scopes(i)%bindings(idx)%type, &
                                  depth + 1, which, id, err)
            return
         end do
         which = TYPE_ANY_POINTER
         id = 0_int64
      case default
         which = -1
      end select
   end subroutine dyn_resolve_anyptr

   !> Zero one slot. Wire zero reads back as the field's default.
   subroutine dyn_zero_slot(p, f, err)
      type(capnp_ptr_t), intent(in) :: p, f
      integer, intent(out) :: err
      type(capnp_ptr_t) :: t, nil
      integer(int64) :: off
      err = CAPNP_OK
      t = field_slot_type(f, err)
      if (err /= CAPNP_OK) return
      off = field_slot_offset(f)
      nil = capnp_ptr_t()
      select case (type_which(t))
      case (TYPE_VOID)
      case (TYPE_BOOL)
         call capnp_set_bool(p, off, .false., err)
      case (TYPE_INT8)
         call capnp_set_i8(p, off, 0_int8, err)
      case (TYPE_UINT8)
         call capnp_set_u8(p, off, 0_int16, err)
      case (TYPE_INT16)
         call capnp_set_i16(p, off*2_int64, 0_int16, err)
      case (TYPE_UINT16, TYPE_ENUM)
         call capnp_set_u16(p, off*2_int64, 0_int32, err)
      case (TYPE_INT32)
         call capnp_set_i32(p, off*4_int64, 0_int32, err)
      case (TYPE_UINT32)
         call capnp_set_u32(p, off*4_int64, 0_int64, err)
      case (TYPE_INT64, TYPE_UINT64)
         call capnp_set_i64(p, off*8_int64, 0_int64, err)
      case (TYPE_FLOAT32)
         call capnp_set_f32(p, off*4_int64, 0.0_real32, err)
      case (TYPE_FLOAT64)
         call capnp_set_f64(p, off*8_int64, 0.0_real64, err)
      case (TYPE_TEXT, TYPE_DATA, TYPE_LIST, TYPE_STRUCT, TYPE_INTERFACE, TYPE_ANY_POINTER)
         call capnp_setp(p, int(off), nil, err)
      end select
   end subroutine dyn_zero_slot

   !> Zero every slot and inner union discriminant of a struct or group,
   !> including nested groups.
   recursive subroutine dyn_clear_slots(schema, idx, p, depth, err)
      use capnp_union, only: capnp_set_which
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(in) :: depth
      integer, intent(out) :: err
      type(capnp_ptr_t) :: fl, f
      integer(int64) :: i
      err = CAPNP_OK
      if (depth > SCHEMA_WALK_LIMIT) then
         err = CAPNP_ERR_DEPTH
         return
      end if
      if (idx < 1 .or. idx > size(schema%nodes)) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (node_struct_discriminant_count(schema%nodes(idx)) > 0) then
         call capnp_set_which(p, int(node_struct_discriminant_offset(schema%nodes(idx))), 0, err)
         if (err /= CAPNP_OK) return
      end if
      fl = node_struct_fields(schema%nodes(idx), err)
      if (err /= CAPNP_OK) return
      do i = 0_int64, capnp_list_len(fl) - 1_int64
         f = capnp_list_get_struct(fl, int(i), err)
         if (err /= CAPNP_OK) return
         call dyn_clear_field(schema, p, f, depth, err)
         if (err /= CAPNP_OK) return
      end do
   end subroutine dyn_clear_slots

   recursive subroutine dyn_clear_field(schema, p, f, depth, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      type(capnp_ptr_t), intent(in) :: p, f
      integer, intent(in) :: depth
      integer, intent(out) :: err
      integer :: gidx
      err = CAPNP_OK
      if (field_which(f) == FIELD_GROUP) then
         gidx = dyn_node_by_id(schema, field_group_type_id(f))
         if (gidx == 0) then
            err = CAPNP_ERR_ARG
            return
         end if
         call dyn_clear_slots(schema, gidx, p, depth + 1, err)
         return
      end if
      call dyn_zero_slot(p, f, err)
   end subroutine dyn_clear_field

   !> Zero the union arm currently selected, before the discriminant moves.
   subroutine dyn_clear_arm(schema, idx, p, disc, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(in) :: disc
      integer, intent(out) :: err
      type(capnp_ptr_t) :: fl, f
      integer(int64) :: i
      err = CAPNP_OK
      fl = node_struct_fields(schema%nodes(idx), err)
      if (err /= CAPNP_OK) return
      do i = 0_int64, capnp_list_len(fl) - 1_int64
         f = capnp_list_get_struct(fl, int(i), err)
         if (err /= CAPNP_OK) return
         if (field_discriminant(f) /= disc) cycle
         call dyn_clear_field(schema, p, f, 0, err)
         return
      end do
   end subroutine dyn_clear_arm

   subroutine dyn_select_field(schema, idx, p, f, err)
      use capnp_union, only: capnp_set_which, capnp_which
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      type(capnp_ptr_t), intent(in) :: p, f
      integer, intent(out) :: err
      integer :: disc, old, off
      err = CAPNP_OK
      disc = field_discriminant(f)
      if (disc == NO_DISCRIMINANT) return
      if (node_struct_discriminant_count(schema%nodes(idx)) <= 0) then
         err = CAPNP_ERR_KIND
         return
      end if
      off = int(node_struct_discriminant_offset(schema%nodes(idx)))
      old = capnp_which(p, off)
      ! C++ DynamicStruct::init clears the previous member. The arm being
      ! selected is left alone so the write that follows can fill it.
      if (old /= disc) then
         call dyn_clear_arm(schema, idx, p, old, err)
         if (err /= CAPNP_OK) return
      end if
      call capnp_set_which(p, off, disc, err)
   end subroutine dyn_select_field

   !> A field that sits past the data section reads as the default.
   !> A field inside the claimed section needs a struct whose bytes are
   !> in a segment. layout.h StructReader::getDataField (1098) returns
   !> zero past dataSize, and layout.c++ (800) rejects a struct that does
   !> not lie in its segment.
   subroutine dyn_bits_readable(p, bit_off, bit_len, err)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: bit_off, bit_len
      integer, intent(out) :: err
      integer(int64) :: bits, byte_off, nbytes
      err = CAPNP_OK
      if (p%kind /= CAPNP_PK_NULL .and. p%kind /= CAPNP_PK_STRUCT) then
         err = CAPNP_ERR_KIND
         return
      end if
      if (bit_off < 0_int64 .or. bit_len < 0_int64) then
         err = CAPNP_ERR_BOUNDS
         return
      end if
      if (p%dbits >= 0_int64) then
         bits = p%dbits
      else
         bits = int(p%dwords, int64)*64_int64
      end if
      if (bit_len == 0_int64 .or. bit_off + bit_len > bits) return
      if (.not. associated(p%msg)) then
         err = CAPNP_ERR_KIND
         return
      end if
      if (.not. allocated(p%msg%segs) .or. p%seg < 1 .or. p%seg > p%msg%nsegs) then
         err = CAPNP_ERR_BOUNDS
         return
      end if
      byte_off = p%off + bit_off/8_int64
      nbytes = (bit_off + bit_len + 7_int64)/8_int64 - bit_off/8_int64
      if (p%off < 0_int64 .or. byte_off < 0_int64 .or. &
          byte_off + nbytes > p%msg%segs(p%seg)%len) err = CAPNP_ERR_BOUNDS
   end subroutine dyn_bits_readable

   !> A pointer field beyond a struct's pointer section is null. A field
   !> inside that section must name bytes in a message segment.
   subroutine dyn_pointer_readable(p, slot, err)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(in) :: slot
      integer, intent(out) :: err
      integer(int64) :: byte_off
      err = CAPNP_OK
      if (p%kind == CAPNP_PK_NULL) return
      if (p%kind /= CAPNP_PK_STRUCT) then
         err = CAPNP_ERR_KIND
         return
      end if
      if (slot < 0) then
         err = CAPNP_ERR_BOUNDS
         return
      end if
      if (slot >= p%pwords) return
      if (.not. associated(p%msg)) then
         err = CAPNP_ERR_KIND
         return
      end if
      if (.not. allocated(p%msg%segs) .or. p%seg < 1 .or. p%seg > p%msg%nsegs) then
         err = CAPNP_ERR_BOUNDS
         return
      end if
      byte_off = p%off + int(p%dwords + slot, int64)*8_int64
      if (p%off < 0_int64 .or. byte_off < 0_int64 .or. &
          byte_off + 8_int64 > p%msg%segs(p%seg)%len) err = CAPNP_ERR_BOUNDS
   end subroutine dyn_pointer_readable

   !> C++ DynamicStruct::get rejects a union arm that is not selected.
   subroutine dyn_require_active(schema, idx, p, f, err)
      use capnp_union, only: capnp_which
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      type(capnp_ptr_t), intent(in) :: p, f
      integer, intent(out) :: err
      integer :: disc, off
      err = CAPNP_OK
      disc = field_discriminant(f)
      if (disc == NO_DISCRIMINANT) return
      if (idx < 1 .or. idx > size(schema%nodes)) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (node_struct_discriminant_count(schema%nodes(idx)) <= 0) then
         err = CAPNP_ERR_KIND
         return
      end if
      off = int(node_struct_discriminant_offset(schema%nodes(idx)))
      call dyn_bits_readable(p, int(off, int64)*16_int64, 16_int64, err)
      if (err /= CAPNP_OK) return
      if (capnp_which(p, off) /= disc) err = CAPNP_ERR_KIND
   end subroutine dyn_require_active

   subroutine dyn_annotations(list, annotations, err)
      type(capnp_ptr_t), intent(in) :: list
      type(capnp_dyn_annotation_t), allocatable, intent(out) :: annotations(:)
      integer, intent(out) :: err
      type(capnp_ptr_t) :: a
      integer(int64) :: i, n
      integer :: stat
      err = CAPNP_OK
      n = capnp_list_len(list)
      if (.not. wire_count_ok(list, n)) then
         err = CAPNP_ERR_ALLOC
         allocate (annotations(0), stat=stat)
         return
      end if
      allocate (annotations(n), stat=stat)
      if (stat /= 0) then
         err = CAPNP_ERR_ALLOC
         return
      end if
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
      if (.not. schema%loaded) then
         err = CAPNP_ERR_ARG
         allocate (annotations(0))
         return
      end if
      if (.not. allocated(schema%nodes)) then
         err = CAPNP_ERR_ARG
         allocate (annotations(0))
         return
      end if
      if (idx < 1) then
         err = CAPNP_ERR_ARG
         allocate (annotations(0))
         return
      end if
      if (idx > size(schema%nodes)) then
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
      if (.not. schema%loaded) then
         err = CAPNP_ERR_ARG
         allocate (annotations(0))
         return
      end if
      if (.not. allocated(schema%nodes)) then
         err = CAPNP_ERR_ARG
         allocate (annotations(0))
         return
      end if
      if (idx < 1) then
         err = CAPNP_ERR_ARG
         allocate (annotations(0))
         return
      end if
      if (idx > size(schema%nodes)) then
         err = CAPNP_ERR_ARG
         allocate (annotations(0))
         return
      end if
      if (node_which(schema%nodes(idx)) /= NODE_ENUM) then
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
      if (.not. schema%loaded) then
         err = CAPNP_ERR_ARG
         allocate (annotations(0))
         return
      end if
      if (.not. allocated(schema%nodes)) then
         err = CAPNP_ERR_ARG
         allocate (annotations(0))
         return
      end if
      if (idx < 1) then
         err = CAPNP_ERR_ARG
         allocate (annotations(0))
         return
      end if
      if (idx > size(schema%nodes)) then
         err = CAPNP_ERR_ARG
         allocate (annotations(0))
         return
      end if
      if (node_which(schema%nodes(idx)) /= NODE_INTERFACE) then
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
   !> `brand`, when present, follows parameter bindings, including a
   !> parameter used as a list element. `resolved_id` is the struct,
   !> enum or interface id after that walk, or 0.
   function capnp_dyn_field_type(schema, idx, fname, err, brand, resolved_id) result(tw)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      character(len=*), intent(in) :: fname
      integer, intent(out) :: err
      type(capnp_dyn_brand_t), intent(in), optional :: brand
      integer(int64), intent(out), optional :: resolved_id
      integer :: tw
      integer(int64) :: rid
      type(capnp_ptr_t) :: f, t
      tw = -1
      rid = 0_int64
      call dyn_field(schema, idx, fname, f, err)
      if (err /= CAPNP_OK) return
      if (field_which(f) == FIELD_GROUP) then
         tw = TYPE_STRUCT
         if (present(resolved_id)) resolved_id = 0_int64
         return
      end if
      t = field_slot_type(f, err)
      if (err /= CAPNP_OK) return
      if (present(brand)) then
         call dyn_resolve_type(brand, t, 0, tw, rid, err)
      else
         tw = type_which(t)
         if (tw == TYPE_STRUCT .or. tw == TYPE_ENUM .or. tw == TYPE_INTERFACE) rid = type_type_id(t)
      end if
      if (present(resolved_id)) resolved_id = rid
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
      call dyn_require_active(schema, idx, p, f, err)
      if (err /= CAPNP_OK) return
      t = field_slot_type(f, err)
      if (err /= CAPNP_OK) return
      dv = field_slot_default(f, err)
      if (err /= CAPNP_OK) return
      off = field_slot_offset(f)
      select case (type_which(t))
      case (TYPE_INT8)
         call dyn_bits_readable(p, off*8_int64, 8_int64, err)
         if (err /= CAPNP_OK) return
         v = int(capnp_get_i8(p, off, default=int(value_i8(dv), int8)), int64)
      case (TYPE_UINT8)
         call dyn_bits_readable(p, off*8_int64, 8_int64, err)
         if (err /= CAPNP_OK) return
         v = int(capnp_get_u8(p, off, default=value_u8(dv)), int64)
      case (TYPE_INT16)
         call dyn_bits_readable(p, off*16_int64, 16_int64, err)
         if (err /= CAPNP_OK) return
         v = int(capnp_get_i16(p, off*2_int64, default=value_i16(dv)), int64)
      case (TYPE_UINT16, TYPE_ENUM)
         call dyn_bits_readable(p, off*16_int64, 16_int64, err)
         if (err /= CAPNP_OK) return
         v = int(capnp_get_u16(p, off*2_int64, default=value_u16(dv)), int64)
      case (TYPE_INT32)
         call dyn_bits_readable(p, off*32_int64, 32_int64, err)
         if (err /= CAPNP_OK) return
         v = int(capnp_get_i32(p, off*4_int64, default=value_i32(dv)), int64)
      case (TYPE_UINT32)
         call dyn_bits_readable(p, off*32_int64, 32_int64, err)
         if (err /= CAPNP_OK) return
         v = capnp_get_u32(p, off*4_int64, default=value_u32(dv))
      case (TYPE_INT64, TYPE_UINT64)
         call dyn_bits_readable(p, off*64_int64, 64_int64, err)
         if (err /= CAPNP_OK) return
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
      call dyn_require_active(schema, idx, p, f, err)
      if (err /= CAPNP_OK) return
      t = field_slot_type(f, err)
      if (err /= CAPNP_OK) return
      dv = field_slot_default(f, err)
      if (err /= CAPNP_OK) return
      off = field_slot_offset(f)
      select case (type_which(t))
      case (TYPE_FLOAT32)
         call dyn_bits_readable(p, off*32_int64, 32_int64, err)
         if (err /= CAPNP_OK) return
         v = real(capnp_get_f32(p, off*4_int64, default=value_f32(dv)), real64)
      case (TYPE_FLOAT64)
         call dyn_bits_readable(p, off*64_int64, 64_int64, err)
         if (err /= CAPNP_OK) return
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
      call dyn_require_active(schema, idx, p, f, err)
      if (err /= CAPNP_OK) return
      t = field_slot_type(f, err)
      if (err /= CAPNP_OK) return
      if (type_which(t) /= TYPE_BOOL) then
         err = CAPNP_ERR_KIND
         return
      end if
      dv = field_slot_default(f, err)
      if (err /= CAPNP_OK) return
      call dyn_bits_readable(p, field_slot_offset(f), 1_int64, err)
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
      call dyn_require_active(schema, idx, p, f, err)
      if (err /= CAPNP_OK) return
      t = field_slot_type(f, err)
      if (err /= CAPNP_OK) return
      if (type_which(t) /= TYPE_TEXT) then
         err = CAPNP_ERR_KIND
         return
      end if
      call dyn_pointer_readable(p, int(field_slot_offset(f)), err)
      if (err /= CAPNP_OK) return
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
      call dyn_require_active(schema, idx, p, f, err)
      if (err /= CAPNP_OK) return
      t = field_slot_type(f, err)
      if (err /= CAPNP_OK) return
      if (type_which(t) /= TYPE_DATA) then
         err = CAPNP_ERR_KIND
         allocate (bytes(0:-1))
         return
      end if
      call dyn_pointer_readable(p, int(field_slot_offset(f)), err)
      if (err /= CAPNP_OK) return
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
   !>
   !> Reader pointer lookups store one cached copy of each defaulted field on the schema.
   !> schema is therefore intent(inout). A routine that only holds the
   !> schema as intent(in) cannot call this, nor capnp_dyn_get_any_struct
   !> or capnp_dyn_get_any_list, which share the same cache.
   function capnp_dyn_getp(schema, idx, p, fname, err) result(q)
      type(capnp_dyn_schema_t), intent(inout) :: schema
      integer, intent(in) :: idx
      type(capnp_ptr_t), intent(in) :: p
      character(len=*), intent(in) :: fname
      integer, intent(out) :: err
      type(capnp_ptr_t) :: q
      type(capnp_ptr_t) :: f, t, dv
      q = capnp_ptr_t()
      call dyn_field(schema, idx, fname, f, err)
      if (err /= CAPNP_OK) return
      call dyn_require_active(schema, idx, p, f, err)
      if (err /= CAPNP_OK) return
      if (field_which(f) == FIELD_GROUP) then
         if (p%kind /= CAPNP_PK_NULL .and. p%kind /= CAPNP_PK_STRUCT) then
            err = CAPNP_ERR_KIND
            return
         end if
         q = p
         return
      end if
      t = field_slot_type(f, err)
      if (err /= CAPNP_OK) return
      select case (type_which(t))
      case (TYPE_TEXT, TYPE_DATA, TYPE_LIST, TYPE_STRUCT, TYPE_INTERFACE, TYPE_ANY_POINTER)
      case default
         err = CAPNP_ERR_KIND
         return
      end select
      call dyn_pointer_readable(p, int(field_slot_offset(f)), err)
      if (err /= CAPNP_OK) return
      q = capnp_getp(p, int(field_slot_offset(f)), err)
      if (err /= CAPNP_OK .or. q%kind /= CAPNP_PK_NULL) return
      dv = field_slot_default(f, err)
      if (err /= CAPNP_OK) return
      q = value_pointer(dv, err)
      if (err /= CAPNP_OK) return
      ! The default object lives in the schema message. A reader gets a
      ! frozen view of one cached copy of this field. A builder copies
      ! into its own message, as the C++ runtime does, so a write cannot
      ! change later defaults. A frozen view is read-only, so it reads as
      ! a reader even though its arena is a builder. A null parent reads
      ! defaults without allocating in its message. Association is tested
      ! before the builder flag: Fortran does not promise to skip the
      ! second operand of or.
      if (p%kind == CAPNP_PK_NULL .or. .not. associated(p%msg)) then
         call dyn_reader_view(schema, idx, f, q, err)
      else if (p%frozen .or. .not. p%msg%is_builder) then
         call dyn_reader_view(schema, idx, f, q, err)
      else
         call dyn_copy_default(p, int(field_slot_offset(f)), q, err)
      end if
      if (err == CAPNP_OK .and. type_which(t) == TYPE_STRUCT .and. &
          q%kind == CAPNP_PK_STRUCT) then
         q%schema_id = type_type_id(t)
      end if
      if (err == CAPNP_OK) then
         if (p%kind == CAPNP_PK_NULL .or. .not. associated(p%msg)) then
            q%frozen = .true.
         else if (p%frozen .or. .not. p%msg%is_builder) then
            q%frozen = .true.
         end if
      end if
   end function capnp_dyn_getp

   !> Copy a schema default into a reader-owned arena once per field and
   !> freeze the handle. Later reads of the same field reuse that copy.
   subroutine dyn_reader_view(schema, node, f, q, err)
      type(capnp_dyn_schema_t), intent(inout), target :: schema
      integer, intent(in) :: node
      type(capnp_ptr_t), intent(in) :: f
      type(capnp_ptr_t), intent(inout) :: q
      integer, intent(out) :: err
      type(capnp_ptr_t) :: copied
      integer, allocatable :: grown_node(:), grown_fseg(:)
      integer(int64), allocatable :: grown_slot(:), grown_foff(:)
      type(capnp_ptr_t), allocatable :: grown_view(:)
      integer(int64) :: sid, slot
      integer :: i, n, stat
      err = CAPNP_OK
      if (q%kind == CAPNP_PK_NULL) return
      slot = field_slot_offset(f)
      if (allocated(schema%ro_node)) then
         do i = 1, schema%n_ro
            if (schema%ro_node(i) == node .and. schema%ro_slot(i) == slot .and. &
                schema%ro_fseg(i) == f%seg .and. schema%ro_foff(i) == f%off) then
               q = schema%ro_view(i)
               return
            end if
         end do
      end if
      sid = q%schema_id
      if (.not. schema%ro_ready) then
         call capnp_message_init_builder(schema%ro_arena, err)
         if (err /= CAPNP_OK) return
         schema%ro_ready = .true.
      end if
      copied = capnp_copy(schema%ro_arena, q, err)
      if (err /= CAPNP_OK) return
      copied%schema_id = sid
      copied%frozen = .true.
      n = schema%n_ro
      if (.not. allocated(schema%ro_node)) then
         allocate (schema%ro_node(8), schema%ro_slot(8), schema%ro_view(8), &
                   schema%ro_fseg(8), schema%ro_foff(8), stat=stat)
         if (stat /= 0) then
            err = CAPNP_ERR_ALLOC
            return
         end if
      else if (n >= size(schema%ro_node)) then
         allocate (grown_node(n*2), grown_slot(n*2), grown_view(n*2), &
                   grown_fseg(n*2), grown_foff(n*2), stat=stat)
         if (stat /= 0) then
            err = CAPNP_ERR_ALLOC
            return
         end if
         grown_node(1:n) = schema%ro_node(1:n)
         grown_slot(1:n) = schema%ro_slot(1:n)
         grown_view(1:n) = schema%ro_view(1:n)
         grown_fseg(1:n) = schema%ro_fseg(1:n)
         grown_foff(1:n) = schema%ro_foff(1:n)
         call move_alloc(grown_node, schema%ro_node)
         call move_alloc(grown_slot, schema%ro_slot)
         call move_alloc(grown_view, schema%ro_view)
         call move_alloc(grown_fseg, schema%ro_fseg)
         call move_alloc(grown_foff, schema%ro_foff)
      end if
      schema%n_ro = n + 1
      schema%ro_node(schema%n_ro) = node
      schema%ro_slot(schema%n_ro) = slot
      schema%ro_fseg(schema%n_ro) = f%seg
      schema%ro_foff(schema%n_ro) = f%off
      schema%ro_view(schema%n_ro) = copied
      q = copied
   end subroutine dyn_reader_view

   !> Copy a schema default into the caller's builder and, when the slot
   !> exists, store that copy in the field.
   subroutine dyn_copy_default(host, slot, q, err)
      type(capnp_ptr_t), intent(in) :: host
      integer, intent(in) :: slot
      type(capnp_ptr_t), intent(inout) :: q
      integer, intent(out) :: err
      type(capnp_ptr_t) :: copied
      err = CAPNP_OK
      if (q%kind == CAPNP_PK_NULL) return
      if (.not. associated(host%msg) .or. .not. associated(q%msg)) return
      if (associated(q%msg, host%msg)) return
      if (.not. host%msg%is_builder) return
      copied = capnp_copy(host%msg, q, err)
      if (err /= CAPNP_OK) return
      q = copied
      if (host%kind == CAPNP_PK_STRUCT .and. slot >= 0 .and. slot < host%pwords) then
         call capnp_setp(host, slot, q, err)
      end if
   end subroutine dyn_copy_default

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
      if (.not. schema%loaded) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (.not. allocated(schema%nodes)) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (idx < 1) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (idx > size(schema%nodes)) then
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
      call dyn_bits_readable(p, disc_off*16_int64, 16_int64, err)
      if (err /= CAPNP_OK) then
         tag = -1
         return
      end if
      tag = capnp_which(p, int(disc_off))
      ! C++ which() is empty when the discriminant names no arm.
      if (tag < 0 .or. tag >= disc_count) tag = -1
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
      ! Setters of ordinary fields also call dyn_select_field, which must
      ! stay a no-op for them. select itself is only for union arms.
      if (field_discriminant(f) == NO_DISCRIMINANT) then
         err = CAPNP_ERR_KIND
         return
      end if
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
            TYPE_INT64, TYPE_UINT64, TYPE_ENUM, TYPE_FLOAT32, TYPE_FLOAT64)
      case default
         err = CAPNP_ERR_KIND
         return
      end select
      ! dynamic.c++ checkRoundTrip (1737) and signedToUnsigned (1699):
      ! as<int8_t>() and as<uint8_t>() reject a value that does not survive
      ! the narrowing cast. Unsigned targets reject a negative.
      if (.not. dyn_int_fits(type_which(t), v)) then
         err = CAPNP_ERR_ARG
         return
      end if
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
      case (TYPE_INT64)
         call capnp_set_i64(p, off*8_int64, v, err, default=value_i64(dv))
      case (TYPE_UINT64)
         call capnp_set_i64(p, off*8_int64, v, err, default=value_i64(dv))
      case (TYPE_FLOAT32)
         call capnp_set_f32(p, off*4_int64, real(v, real32), err, default=value_f32(dv))
      case (TYPE_FLOAT64)
         call capnp_set_f64(p, off*8_int64, real(v, real64), err, default=value_f64(dv))
      case default
         err = CAPNP_ERR_KIND
      end select
   end subroutine capnp_dyn_set_int

   !> True when v round-trips through the field width. Matches
   !> DynamicValue::Reader::as in dynamic.c++.
   pure function dyn_int_fits(tw, v) result(ok)
      integer, intent(in) :: tw
      integer(int64), intent(in) :: v
      logical :: ok
      ok = .false.
      select case (tw)
      case (TYPE_INT8)
         ok = v >= -128_int64 .and. v <= 127_int64
      case (TYPE_UINT8)
         ok = v >= 0_int64 .and. v <= 255_int64
      case (TYPE_INT16)
         ok = v >= -32768_int64 .and. v <= 32767_int64
      case (TYPE_UINT16, TYPE_ENUM)
         ok = v >= 0_int64 .and. v <= 65535_int64
      case (TYPE_INT32)
         ok = v >= -2147483648_int64 .and. v <= 2147483647_int64
      case (TYPE_UINT32)
         ok = v >= 0_int64 .and. v <= 4294967295_int64
      case (TYPE_INT64, TYPE_FLOAT32, TYPE_FLOAT64)
         ok = .true.
      case (TYPE_UINT64)
         ok = v >= 0_int64
      end select
   end function dyn_int_fits

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

   !> True when value is a struct of the field's schema. A non-zero schema
   !> id must match the field, and the layout must match the node.
   function dyn_struct_ok(schema, t, value) result(ok)
      type(capnp_dyn_schema_t), intent(in) :: schema
      type(capnp_ptr_t), intent(in) :: t, value
      logical :: ok
      integer(int64) :: sid
      integer :: nidx
      ok = .false.
      if (value%kind /= CAPNP_PK_STRUCT) return
      sid = type_type_id(t)
      if (value%schema_id /= 0_int64 .and. value%schema_id /= sid) return
      nidx = dyn_node_by_id(schema, sid)
      if (nidx /= 0) then
         if (value%dwords /= node_struct_data_words(schema%nodes(nidx))) return
         if (value%pwords /= node_struct_pointer_count(schema%nodes(nidx))) return
      end if
      ok = .true.
   end function dyn_struct_ok

   !> True when a list's element size matches the field's element type.
   function dyn_list_ok(schema, elem, value) result(ok)
      type(capnp_dyn_schema_t), intent(in) :: schema
      type(capnp_ptr_t), intent(in) :: elem, value
      logical :: ok
      integer :: nidx
      ok = .false.
      if (value%kind /= CAPNP_PK_LIST) return
      select case (type_which(elem))
      case (TYPE_VOID)
         ok = value%esize == CAPNP_SZ_VOID
      case (TYPE_BOOL)
         ok = value%esize == CAPNP_SZ_BIT
      case (TYPE_INT8, TYPE_UINT8)
         ok = value%esize == CAPNP_SZ_BYTE
      case (TYPE_INT16, TYPE_UINT16, TYPE_ENUM)
         ok = value%esize == CAPNP_SZ_TWO
      case (TYPE_INT32, TYPE_UINT32, TYPE_FLOAT32)
         ok = value%esize == CAPNP_SZ_FOUR
      case (TYPE_INT64, TYPE_UINT64, TYPE_FLOAT64)
         ok = value%esize == CAPNP_SZ_EIGHT
      case (TYPE_TEXT, TYPE_DATA, TYPE_LIST, TYPE_ANY_POINTER, TYPE_INTERFACE)
         ok = value%esize == CAPNP_SZ_PTR
      case (TYPE_STRUCT)
         if (value%esize /= CAPNP_SZ_COMPOSITE) return
         nidx = dyn_node_by_id(schema, type_type_id(elem))
         ok = .true.
         if (nidx /= 0) then
            if (value%dwords /= node_struct_data_words(schema%nodes(nidx))) ok = .false.
            if (value%pwords /= node_struct_pointer_count(schema%nodes(nidx))) ok = .false.
         end if
      case default
         ! dynamic.c++ elementSizeFor (70-97) maps an unknown tag to
         ! ElementSize::VOID, rather than treating it as a pointer.
         ok = value%esize == CAPNP_SZ_VOID
      end select
   end function dyn_list_ok

   !> True when an anyPointer Type accepts a value of this resolved kind.
   !> Unconstrained struct, list and capability are the inner union at
   !> byte 10, not Type tags. Parameters and implicit parameters accept
   !> any pointer. anyKind accepts struct, list and capability.
   function any_accepts(t, kind) result(ok)
      type(capnp_ptr_t), intent(in) :: t
      integer, intent(in) :: kind
      logical :: ok
      integer :: inner
      ok = .false.
      if (type_which(t) /= TYPE_ANY_POINTER) return
      select case (type_anyptr_which(t))
      case (ANYPTR_PARAMETER, ANYPTR_IMPLICIT)
         ok = kind == CAPNP_PK_STRUCT .or. kind == CAPNP_PK_LIST .or. kind == CAPNP_PK_CAP
      case (ANYPTR_UNCONSTRAINED)
         inner = type_uncon_which(t)
         select case (inner)
         case (UNCON_ANY)
            ok = kind == CAPNP_PK_STRUCT .or. kind == CAPNP_PK_LIST .or. kind == CAPNP_PK_CAP
         case (UNCON_STRUCT)
            ok = kind == CAPNP_PK_STRUCT
         case (UNCON_LIST)
            ok = kind == CAPNP_PK_LIST
         case (UNCON_CAPABILITY)
            ok = kind == CAPNP_PK_CAP
         end select
      end select
   end function any_accepts

   subroutine capnp_dyn_setp(schema, idx, p, fname, value, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      type(capnp_ptr_t), intent(in) :: p, value
      character(len=*), intent(in) :: fname
      integer, intent(out) :: err
      type(capnp_ptr_t) :: f, t, elem
      call dyn_field(schema, idx, fname, f, err)
      if (err /= CAPNP_OK) return
      t = field_slot_type(f, err)
      if (err /= CAPNP_OK) return
      select case (type_which(t))
      case (TYPE_TEXT, TYPE_DATA, TYPE_LIST, TYPE_STRUCT, TYPE_INTERFACE, TYPE_ANY_POINTER)
         if (value%kind /= CAPNP_PK_NULL) then
            select case (type_which(t))
            case (TYPE_STRUCT)
               if (.not. dyn_struct_ok(schema, t, value)) then
                  err = CAPNP_ERR_KIND
                  return
               end if
            case (TYPE_LIST)
               elem = type_list_element(t, err)
               if (err /= CAPNP_OK) return
               if (.not. dyn_list_ok(schema, elem, value)) then
                  err = CAPNP_ERR_KIND
                  return
               end if
            case (TYPE_TEXT, TYPE_DATA)
               if (value%kind /= CAPNP_PK_LIST .or. value%esize /= CAPNP_SZ_BYTE) then
                  err = CAPNP_ERR_KIND
                  return
               end if
               if (type_which(t) == TYPE_TEXT) then
                  ! capnp_get_text rejects Text without its trailing NUL.
                  if (value%nelem < 1_int64) then
                     err = CAPNP_ERR_KIND
                     return
                  end if
                  if (capnp_list_get_i8(value, value%nelem - 1_int64, err) /= 0_int8) then
                     err = CAPNP_ERR_KIND
                     return
                  end if
                  if (err /= CAPNP_OK) return
               end if
            case (TYPE_INTERFACE)
               if (value%kind /= CAPNP_PK_CAP) then
                  err = CAPNP_ERR_KIND
                  return
               end if
            case (TYPE_ANY_POINTER)
               if (.not. any_accepts(t, value%kind)) then
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
      call dyn_require_active(schema, idx, p, f, err)
      if (err /= CAPNP_OK) return
      t = field_slot_type(f, err)
      if (err /= CAPNP_OK) return
      if (type_which(t) /= TYPE_INTERFACE) then
         if (type_which(t) /= TYPE_ANY_POINTER) then
            err = CAPNP_ERR_KIND
            return
         end if
         if (.not. any_accepts(t, CAPNP_PK_CAP)) then
            err = CAPNP_ERR_KIND
            return
         end if
      end if
      call dyn_pointer_readable(p, int(field_slot_offset(f)), err)
      if (err /= CAPNP_OK) return
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

   !> Struct view of an anyPointer field. schema is intent(inout) because
   !> a missing value is filled from the reader default cache. See
   !> capnp_dyn_getp.
   function capnp_dyn_get_any_struct(schema, idx, p, fname, err) result(value)
      type(capnp_dyn_schema_t), intent(inout) :: schema
      integer, intent(in) :: idx
      type(capnp_ptr_t), intent(in) :: p
      character(len=*), intent(in) :: fname
      integer, intent(out) :: err
      type(capnp_ptr_t) :: value, f, t
      value = capnp_ptr_t()
      call dyn_field(schema, idx, fname, f, err)
      if (err /= CAPNP_OK) return
      if (field_which(f) == FIELD_GROUP) then
         err = CAPNP_ERR_KIND
         return
      end if
      t = field_slot_type(f, err)
      if (err /= CAPNP_OK) return
      if (.not. any_accepts(t, CAPNP_PK_STRUCT)) then
         err = CAPNP_ERR_KIND
         return
      end if
      value = capnp_dyn_getp(schema, idx, p, fname, err)
      if (err /= CAPNP_OK .or. value%kind == CAPNP_PK_NULL) return
      if (value%kind /= CAPNP_PK_STRUCT) err = CAPNP_ERR_KIND
   end function capnp_dyn_get_any_struct

   !> List view of an anyPointer field. schema is intent(inout) because
   !> a missing value is filled from the reader default cache. See
   !> capnp_dyn_getp.
   function capnp_dyn_get_any_list(schema, idx, p, fname, err) result(value)
      type(capnp_dyn_schema_t), intent(inout) :: schema
      integer, intent(in) :: idx
      type(capnp_ptr_t), intent(in) :: p
      character(len=*), intent(in) :: fname
      integer, intent(out) :: err
      type(capnp_ptr_t) :: value, f, t
      value = capnp_ptr_t()
      call dyn_field(schema, idx, fname, f, err)
      if (err /= CAPNP_OK) return
      if (field_which(f) == FIELD_GROUP) then
         err = CAPNP_ERR_KIND
         return
      end if
      t = field_slot_type(f, err)
      if (err /= CAPNP_OK) return
      if (.not. any_accepts(t, CAPNP_PK_LIST)) then
         err = CAPNP_ERR_KIND
         return
      end if
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
      if (.not. associated(p%msg)) then
         err = CAPNP_ERR_ARG
         return
      end if
      call dyn_select_field(schema, idx, p, f, err)
      if (err /= CAPNP_OK) return
      value = capnp_new_struct(p%msg, node_struct_data_words(schema%nodes(nidx)), &
                               node_struct_pointer_count(schema%nodes(nidx)), err)
      if (err /= CAPNP_OK) return
      value%schema_id = node_id(schema%nodes(nidx))
      call capnp_setp(p, int(field_slot_offset(f)), value, err)
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
      if (.not. associated(p%msg)) then
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
         case (TYPE_TEXT, TYPE_DATA, TYPE_LIST, TYPE_ANY_POINTER, TYPE_INTERFACE)
            esize = CAPNP_SZ_PTR
         case default
            ! Unknown dynamic element tags are forward-compatible void lists
            ! in Cap'n Proto C++ 1.4.0.
            esize = CAPNP_SZ_VOID
         end select
         value = capnp_new_list(p%msg, esize, n, err)
      end if
      if (err == CAPNP_OK) call capnp_setp(p, int(field_slot_offset(f)), value, err)
   end function capnp_dyn_init_list

end module capnp_dynamic
