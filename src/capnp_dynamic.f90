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
   public :: capnp_dyn_load, capnp_dyn_free, capnp_dyn_find
   public :: capnp_dyn_get_int, capnp_dyn_get_f64, capnp_dyn_get_bool
   public :: capnp_dyn_get_text, capnp_dyn_getp
   public :: capnp_dyn_set_int, capnp_dyn_set_text
   public :: capnp_dyn_which, capnp_dyn_field_type

   !> A loaded compiled schema: the CGR message plus a node index.
   type :: capnp_dyn_schema_t
      type(capnp_message_t) :: cgr
      type(capnp_ptr_t), allocatable :: nodes(:)
      logical :: loaded = .false.
   end type capnp_dyn_schema_t

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
      if (idx < 1 .or. idx > size(schema%nodes)) then
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
      integer(int64) :: off, d
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
      type(capnp_ptr_t) :: f, t
      call dyn_field(schema, idx, fname, f, err)
      if (err /= CAPNP_OK) return
      t = field_slot_type(f, err)
      if (err /= CAPNP_OK) return
      if (type_which(t) /= TYPE_TEXT) then
         err = CAPNP_ERR_KIND
         return
      end if
      call capnp_get_text(p, int(field_slot_offset(f)), s, err)
   end subroutine capnp_dyn_get_text

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
      type(capnp_ptr_t) :: f
      call dyn_field(schema, idx, fname, f, err)
      if (err /= CAPNP_OK) return
      if (field_which(f) == FIELD_GROUP) then
         q = p
         return
      end if
      q = capnp_getp(p, int(field_slot_offset(f)), err)
   end function capnp_dyn_getp

   !> Union discriminant of struct node idx read from p.
   !>
   !> Cap'n Proto may report discriminantCount on the struct itself, or only
   !> on an anonymous-union group node that shares the parent's data section
   !> (addressbook Person.employment). When the parent count is zero, scan
   !> group fields for a child with a non-zero count and use that offset.
   function capnp_dyn_which(schema, idx, p, err) result(tag)
      use capnp_union, only: capnp_which
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(out) :: err
      integer :: tag
      type(capnp_ptr_t) :: fl, f
      integer(int64) :: i, disc_off
      integer :: gidx, disc_count
      err = CAPNP_OK
      tag = -1
      disc_off = -1_int64
      if (idx < 1 .or. idx > size(schema%nodes)) then
         err = CAPNP_ERR_ARG
         return
      end if
      disc_count = node_struct_discriminant_count(schema%nodes(idx))
      if (disc_count > 0) then
         disc_off = node_struct_discriminant_offset(schema%nodes(idx))
      else
         fl = node_struct_fields(schema%nodes(idx), err)
         if (err /= CAPNP_OK) return
         do i = 0_int64, capnp_list_len(fl) - 1_int64
            f = capnp_list_get_struct(fl, int(i), err)
            if (err /= CAPNP_OK) return
            if (field_which(f) /= FIELD_GROUP) cycle
            gidx = 0
            do gidx = 1, size(schema%nodes)
               if (node_id(schema%nodes(gidx)) == field_group_type_id(f)) exit
            end do
            if (gidx < 1 .or. gidx > size(schema%nodes)) cycle
            if (node_id(schema%nodes(gidx)) /= field_group_type_id(f)) cycle
            disc_count = node_struct_discriminant_count(schema%nodes(gidx))
            if (disc_count > 0) then
               disc_off = node_struct_discriminant_offset(schema%nodes(gidx))
               exit
            end if
         end do
      end if
      if (disc_off < 0_int64) then
         err = CAPNP_ERR_KIND
         return
      end if
      tag = capnp_which(p, int(disc_off))
   end function capnp_dyn_which

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
      call capnp_set_text(p, int(field_slot_offset(f)), s, err)
   end subroutine capnp_dyn_set_text

end module capnp_dynamic
