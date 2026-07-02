!> Fortran code emitter: walks the node graph and writes one module per
!> requested schema file. Naming: camelCase becomes snake_case; nested
!> scopes join with underscores (Person.PhoneNumber -> person_phone_number).
module capnpc_emit
   use capnp
   use capnpc_schema
   implicit none
   private

   public :: emit_file

   type :: node_ref_t
      type(capnp_ptr_t) :: p
      integer(int64) :: id = 0_int64
   end type node_ref_t

   type :: blob_t
      character(len=:), allocatable :: name
      integer(int8), allocatable :: bytes(:)
   end type blob_t

   type(node_ref_t), allocatable :: g_nodes(:)
   integer :: g_out = -1
   !> Pass 1 walks without writing, collecting imports and default blobs;
   !> pass 2 writes.
   logical :: g_suppress = .false.
   character(len=:), allocatable :: g_prefix
   character(len=64) :: g_imports(64)
   integer :: g_nimports = 0
   type(blob_t) :: g_blobs(256)
   integer :: g_nblobs = 0

contains

   ! --- node table ------------------------------------------------------

   subroutine load_nodes(root, err)
      type(capnp_ptr_t), intent(in) :: root
      integer, intent(out) :: err
      type(capnp_ptr_t) :: l
      integer(int64) :: n, i
      l = cgr_nodes(root, err)
      if (err /= CAPNP_OK) return
      n = capnp_list_len(l)
      if (allocated(g_nodes)) deallocate (g_nodes)
      allocate (g_nodes(n))
      do i = 1_int64, n
         g_nodes(i)%p = capnp_list_get_struct(l, int(i - 1), err)
         if (err /= CAPNP_OK) return
         g_nodes(i)%id = node_id(g_nodes(i)%p)
      end do
   end subroutine load_nodes

   function find_node(id) result(idx)
      integer(int64), intent(in) :: id
      integer :: idx
      integer :: i
      idx = 0
      do i = 1, size(g_nodes)
         if (g_nodes(i)%id == id) then
            idx = i
            return
         end if
      end do
   end function find_node

   ! --- naming ------------------------------------------------------------

   pure function snake(s) result(o)
      character(len=*), intent(in) :: s
      character(len=:), allocatable :: o
      integer :: i
      character :: c
      o = ''
      do i = 1, len(s)
         c = s(i:i)
         if (c >= 'A' .and. c <= 'Z') then
            if (i > 1) then
               if (o(len(o):len(o)) /= '_') o = o//'_'
            end if
            o = o//achar(iachar(c) + 32)
         else
            o = o//c
         end if
      end do
   end function snake

   !> Fortran identifier for a node: displayName past the file prefix, dots
   !> to underscores, snake_cased. Avoids scopeId walks (groups have odd
   !> scope ids).
   subroutine node_fname(np, o, err)
      type(capnp_ptr_t), intent(in) :: np
      character(len=:), allocatable, intent(out) :: o
      integer, intent(out) :: err
      character(len=:), allocatable :: dn
      integer :: i
      call node_display_name(np, dn, err)
      if (err /= CAPNP_OK) return
      i = index(dn, ':')
      if (i > 0) dn = dn(i + 1:)
      o = ''
      do i = 1, len(dn)
         if (dn(i:i) == '.') then
            o = o//'_'
         else
            o = o//dn(i:i)
         end if
      end do
      o = snake(o)
   end subroutine node_fname

   pure function upcase(s) result(o)
      character(len=*), intent(in) :: s
      character(len=:), allocatable :: o
      integer :: i
      o = s
      do i = 1, len(o)
         if (o(i:i) >= 'a' .and. o(i:i) <= 'z') o(i:i) = achar(iachar(o(i:i)) - 32)
      end do
   end function upcase

   pure function itoa(v) result(o)
      integer(int64), intent(in) :: v
      character(len=:), allocatable :: o
      character(len=24) :: b
      write (b, '(i0)') v
      o = trim(b)
   end function itoa

   ! --- output helpers ------------------------------------------------------

   subroutine w(line)
      character(len=*), intent(in) :: line
      if (g_suppress) return
      write (g_out, '(a)') line
   end subroutine w

   !> Module name owning a node: its displayName's file part, snake_cased,
   !> with the extension dropped and '_capnp' appended.
   subroutine node_module(np, o, err)
      type(capnp_ptr_t), intent(in) :: np
      character(len=:), allocatable, intent(out) :: o
      integer, intent(out) :: err
      character(len=:), allocatable :: dn
      integer :: i
      call node_display_name(np, dn, err)
      if (err /= CAPNP_OK) return
      i = index(dn, ':')
      if (i > 0) dn = dn(1:i - 1)
      i = index(dn, '/', back=.true.)
      if (i > 0) dn = dn(i + 1:)
      i = index(dn, '.')
      if (i > 0) dn = dn(1:i - 1)
      o = snake(dn)//'_capnp'
   end subroutine node_module

   !> Record a cross-file type reference during pass 1.
   subroutine note_import(idx, err)
      type(capnp_ptr_t) :: np
      integer, intent(in) :: idx
      integer, intent(out) :: err
      character(len=:), allocatable :: dn, m
      integer :: i
      err = CAPNP_OK
      np = g_nodes(idx)%p
      call node_display_name(np, dn, err)
      if (err /= CAPNP_OK) return
      i = index(dn, ':')
      if (i > 0) dn = dn(1:i - 1)
      if (dn == g_prefix) return
      call node_module(np, m, err)
      if (err /= CAPNP_OK) return
      do i = 1, g_nimports
         if (trim(g_imports(i)) == m) return
      end do
      if (g_nimports < size(g_imports)) then
         g_nimports = g_nimports + 1
         g_imports(g_nimports) = m
      end if
   end subroutine note_import

   !> Register a default-object blob during pass 1; emitted as an int8
   !> parameter array in the declaration section.
   subroutine note_blob(name, bytes)
      character(len=*), intent(in) :: name
      integer(int8), intent(in) :: bytes(:)
      integer :: i
      if (.not. g_suppress) return ! only collect once, during the dry pass
      do i = 1, g_nblobs
         if (g_blobs(i)%name == name) return
      end do
      if (g_nblobs == size(g_blobs)) return
      g_nblobs = g_nblobs + 1
      g_blobs(g_nblobs)%name = name
      g_blobs(g_nblobs)%bytes = bytes
   end subroutine note_blob

   subroutine emit_blob_params()
      integer :: i
      integer(int64) :: j, n
      character(len=:), allocatable :: line
      do i = 1, g_nblobs
         n = size(g_blobs(i)%bytes, kind=int64)
         call w('   integer(int8), parameter :: '//g_blobs(i)%name//'(0:'// &
                itoa(n - 1)//') = [ &')
         line = '      '
         do j = 1_int64, n
            line = line//itoa(int(g_blobs(i)%bytes(j), int64))//'_int8'
            if (j < n) line = line//', '
            if (mod(j, 10_int64) == 0_int64 .and. j < n) then
               call w(line//' &')
               line = '      '
            end if
         end do
         call w(line//']')
      end do
      if (g_nblobs > 0) call w('')
   end subroutine emit_blob_params

   ! --- entry point -----------------------------------------------------------

   !> Emit the module for one requested file node.
   subroutine emit_file(root, file_id, filename, err)
      type(capnp_ptr_t), intent(in) :: root
      integer(int64), intent(in) :: file_id
      character(len=*), intent(in) :: filename
      integer, intent(out) :: err
      character(len=:), allocatable :: modname, base, prefix, dn
      integer :: i, ios, fidx
      logical :: mine

      call load_nodes(root, err)
      if (err /= CAPNP_OK) return

      base = filename
      i = index(base, '/', back=.true.)
      if (i > 0) base = base(i + 1:)
      i = index(base, '.')
      if (i > 0) base = base(1:i - 1)
      modname = snake(base)//'_capnp'

      fidx = find_node(file_id)
      if (fidx == 0) then
         err = CAPNP_ERR_ARG
         return
      end if
      call node_display_name(g_nodes(fidx)%p, prefix, err)
      if (err /= CAPNP_OK) return
      g_prefix = prefix
      g_nimports = 0
      g_nblobs = 0

      ! Pass 1 (dry): walk the procedures to discover cross-file imports and
      ! explicit pointer-default blobs before the header is written.
      g_suppress = .true.
      do i = 1, size(g_nodes)
         call node_display_name(g_nodes(i)%p, dn, err)
         if (err /= CAPNP_OK) return
         mine = len(dn) > len(prefix) .and. index(dn, prefix//':') == 1
         if (.not. mine) cycle
         if (node_which(g_nodes(i)%p) == NODE_STRUCT) then
            if (.not. node_struct_is_group(g_nodes(i)%p)) then
               call emit_struct_procs(g_nodes(i)%p, err)
               if (err /= CAPNP_OK) return
            end if
         end if
      end do
      g_suppress = .false.

      open (newunit=g_out, file=modname//'.f90', status='replace', action='write', iostat=ios)
      if (ios /= 0) then
         err = CAPNP_ERR_IO
         return
      end if

      call w('!> Generated by capnpc-fortran from '//trim(filename)//'. Do not edit.')
      call w('module '//modname)
      call w('   use capnp')
      do i = 1, g_nimports
         call w('   use '//trim(g_imports(i)))
      end do
      call w('   implicit none')
      call w('   public')
      call w('')

      ! Declarations pass: enums, consts, struct params + handle types.
      do i = 1, size(g_nodes)
         call node_display_name(g_nodes(i)%p, dn, err)
         if (err /= CAPNP_OK) exit
         mine = len(dn) > len(prefix) .and. index(dn, prefix//':') == 1
         if (.not. mine) cycle
         select case (node_which(g_nodes(i)%p))
         case (NODE_ENUM)
            call emit_enum_decl(g_nodes(i)%p, err)
         case (NODE_CONST)
            call emit_const_decl(g_nodes(i)%p, err)
         case (NODE_STRUCT)
            if (.not. node_struct_is_group(g_nodes(i)%p)) &
               call emit_struct_decl(g_nodes(i)%p, err)
         end select
         if (err /= CAPNP_OK) exit
      end do

      call emit_blob_params()

      call w('contains')
      call w('')

      ! Procedures pass: struct accessors (groups reached via parents).
      if (err == CAPNP_OK) then
         do i = 1, size(g_nodes)
            call node_display_name(g_nodes(i)%p, dn, err)
            if (err /= CAPNP_OK) exit
            mine = len(dn) > len(prefix) .and. index(dn, prefix//':') == 1
            if (.not. mine) cycle
            if (node_which(g_nodes(i)%p) == NODE_STRUCT) then
               if (.not. node_struct_is_group(g_nodes(i)%p)) then
                  call emit_struct_procs(g_nodes(i)%p, err)
                  if (err /= CAPNP_OK) exit
               end if
            end if
         end do
      end if

      call w('end module '//modname)
      close (g_out)
   end subroutine emit_file

   ! --- enums / consts ---------------------------------------------------------

   subroutine emit_enum_decl(np, err)
      type(capnp_ptr_t), intent(in) :: np
      integer, intent(out) :: err
      type(capnp_ptr_t) :: el, en
      character(len=:), allocatable :: tn, nm
      integer(int64) :: i
      call node_fname(np, tn, err)
      if (err /= CAPNP_OK) return
      el = node_enumerants(np, err)
      if (err /= CAPNP_OK) return
      do i = 0_int64, capnp_list_len(el) - 1_int64
         en = capnp_list_get_struct(el, int(i), err)
         if (err /= CAPNP_OK) return
         call enumerant_name(en, nm, err)
         if (err /= CAPNP_OK) return
         call w('   integer, parameter :: '//upcase(tn)//'_'//upcase(snake(nm))// &
                ' = '//itoa(i))
      end do
      call w('')
   end subroutine emit_enum_decl

   subroutine emit_const_decl(np, err)
      type(capnp_ptr_t), intent(in) :: np
      integer, intent(out) :: err
      type(capnp_ptr_t) :: v
      character(len=:), allocatable :: cn, s
      call node_fname(np, cn, err)
      if (err /= CAPNP_OK) return
      v = node_const_value(np, err)
      if (err /= CAPNP_OK) return
      select case (value_which(v))
      case (TYPE_BOOL)
         if (value_bool(v)) then
            call w('   logical, parameter :: '//upcase(cn)//' = .true.')
         else
            call w('   logical, parameter :: '//upcase(cn)//' = .false.')
         end if
      case (TYPE_INT8, TYPE_INT16, TYPE_INT32, TYPE_INT64, TYPE_ENUM, &
            TYPE_UINT8, TYPE_UINT16, TYPE_UINT32, TYPE_UINT64)
         call w('   integer(int64), parameter :: '//upcase(cn)//' = '// &
                itoa(const_int_value(v))//'_int64')
      case (TYPE_FLOAT32)
         call w('   real(real32), parameter :: '//upcase(cn)//' = transfer('// &
                itoa(int(cp_f32_bits(value_f32(v)), int64))//'_int32, 1.0_real32)')
      case (TYPE_FLOAT64)
         call w('   real(real64), parameter :: '//upcase(cn)//' = transfer('// &
                itoa(cp_f64_bits(value_f64(v)))//'_int64, 1.0_real64)')
      case (TYPE_TEXT)
         call value_text(v, s, err)
         if (err /= CAPNP_OK) return
         call w('   character(len=*), parameter :: '//upcase(cn)//" = '"//s//"'")
      case default
         call w('   ! const '//cn//': unsupported value kind '// &
                itoa(int(value_which(v), int64)))
      end select
   end subroutine emit_const_decl

   function const_int_value(v) result(x)
      type(capnp_ptr_t), intent(in) :: v
      integer(int64) :: x
      select case (value_which(v))
      case (TYPE_INT8); x = int(value_i8(v), int64)
      case (TYPE_INT16); x = int(value_i16(v), int64)
      case (TYPE_INT32); x = int(value_i32(v), int64)
      case (TYPE_INT64); x = value_i64(v)
      case (TYPE_UINT8); x = int(value_u8(v), int64)
      case (TYPE_UINT16); x = int(value_u16(v), int64)
      case (TYPE_UINT32); x = value_u32(v)
      case (TYPE_UINT64); x = value_u64(v)
      case (TYPE_ENUM); x = int(value_enum(v), int64)
      case default; x = 0_int64
      end select
   end function const_int_value

   ! --- structs ------------------------------------------------------------------

   subroutine emit_struct_decl(np, err)
      type(capnp_ptr_t), intent(in) :: np
      integer, intent(out) :: err
      character(len=:), allocatable :: tn
      call node_fname(np, tn, err)
      if (err /= CAPNP_OK) return
      call w('   integer, parameter :: '//upcase(tn)//'_DWORDS = '// &
             itoa(int(node_struct_data_words(np), int64)))
      call w('   integer, parameter :: '//upcase(tn)//'_PWORDS = '// &
             itoa(int(node_struct_pointer_count(np), int64)))
      call w('   type :: '//tn//'_t')
      call w('      type(capnp_ptr_t) :: p')
      call w('   end type '//tn//'_t')
      call w('')
   end subroutine emit_struct_decl

   subroutine emit_struct_procs(np, err)
      type(capnp_ptr_t), intent(in) :: np
      integer, intent(out) :: err
      character(len=:), allocatable :: tn
      call node_fname(np, tn, err)
      if (err /= CAPNP_OK) return

      call w('   function '//tn//'_new(msg, err) result(h)')
      call w('      type(capnp_message_t), intent(inout), target :: msg')
      call w('      integer, intent(out) :: err')
      call w('      type('//tn//'_t) :: h')
      call w('      h%p = capnp_new_struct(msg, '//upcase(tn)//'_DWORDS, '// &
             upcase(tn)//'_PWORDS, err)')
      call w('   end function '//tn//'_new')
      call w('')
      call w('   function '//tn//'_new_root(msg, err) result(h)')
      call w('      type(capnp_message_t), intent(inout), target :: msg')
      call w('      integer, intent(out) :: err')
      call w('      type('//tn//'_t) :: h')
      call w('      h = '//tn//'_new(msg, err)')
      call w('      if (err == CAPNP_OK) call capnp_set_root(msg, h%p, err)')
      call w('   end function '//tn//'_new_root')
      call w('')
      call w('   function '//tn//'_read_root(msg, err) result(h)')
      call w('      type(capnp_message_t), intent(inout), target :: msg')
      call w('      integer, intent(out) :: err')
      call w('      type('//tn//'_t) :: h')
      call w('      h%p = capnp_root(msg, err)')
      call w('   end function '//tn//'_read_root')
      call w('')

      call emit_fields_of(np, tn, tn, err)
   end subroutine emit_struct_procs

   !> Emit accessors for every field of node np. Accessors take handle type
   !> ht_t; generated names start with pfx (differs from ht for groups).
   recursive subroutine emit_fields_of(np, ht, pfx, err)
      type(capnp_ptr_t), intent(in) :: np
      character(len=*), intent(in) :: ht, pfx
      integer, intent(out) :: err
      type(capnp_ptr_t) :: fl, f
      integer(int64) :: i
      integer :: disc_count
      disc_count = node_struct_discriminant_count(np)
      if (disc_count > 0) then
         call w('   function '//pfx//'_which(h) result(tag)')
         call w('      type('//ht//'_t), intent(in) :: h')
         call w('      integer :: tag')
         call w('      tag = int(capnp_get_u16(h%p, '// &
                itoa(node_struct_discriminant_offset(np)*2_int64)//'_int64))')
         call w('   end function '//pfx//'_which')
         call w('')
      end if
      fl = node_struct_fields(np, err)
      if (err /= CAPNP_OK) return
      do i = 0_int64, capnp_list_len(fl) - 1_int64
         f = capnp_list_get_struct(fl, int(i), err)
         if (err /= CAPNP_OK) return
         call emit_field(np, f, ht, pfx, err)
         if (err /= CAPNP_OK) return
      end do
   end subroutine emit_fields_of

   recursive subroutine emit_field(np, f, ht, pfx, err)
      type(capnp_ptr_t), intent(in) :: np, f
      character(len=*), intent(in) :: ht, pfx
      integer, intent(out) :: err
      type(capnp_ptr_t) :: t, dv
      character(len=:), allocatable :: fn, an, gset
      integer :: disc, gidx
      integer(int64) :: off

      call field_name(f, fn, err)
      if (err /= CAPNP_OK) return
      an = pfx//'_'//snake(fn)
      disc = field_discriminant(f)
      gset = ''
      if (disc /= NO_DISCRIMINANT) then
         gset = '      call capnp_set_u16(h%p, '// &
                itoa(node_struct_discriminant_offset(np)*2_int64)// &
                '_int64, '//itoa(int(disc, int64))//'_int32, err)'
      end if

      if (field_which(f) == FIELD_GROUP) then
         gidx = find_node(field_group_type_id(f))
         if (gidx == 0) then
            err = CAPNP_ERR_ARG
            return
         end if
         if (disc /= NO_DISCRIMINANT) then
            ! Selecting a union group member gets an explicit setter.
            call w('   subroutine '//an//'_select(h, err)')
            call w('      type('//ht//'_t), intent(in) :: h')
            call w('      integer, intent(out) :: err')
            call w(gset)
            call w('   end subroutine '//an//'_select')
            call w('')
         end if
         call emit_fields_of(g_nodes(gidx)%p, ht, an, err)
         return
      end if

      t = field_slot_type(f, err)
      if (err /= CAPNP_OK) return
      dv = field_slot_default(f, err)
      if (err /= CAPNP_OK) return
      off = field_slot_offset(f)

      select case (type_which(t))
      case (TYPE_VOID)
         if (disc /= NO_DISCRIMINANT) then
            call w('   subroutine '//an//'_set(h, err)')
            call w('      type('//ht//'_t), intent(in) :: h')
            call w('      integer, intent(out) :: err')
            call w(gset)
            call w('   end subroutine '//an//'_set')
            call w('')
         end if
      case (TYPE_BOOL)
         call emit_bool_field(an, ht, off, dv, gset)
      case (TYPE_INT8, TYPE_INT16, TYPE_INT32, TYPE_INT64, &
            TYPE_UINT8, TYPE_UINT16, TYPE_UINT32, TYPE_UINT64)
         call emit_int_field(an, ht, type_which(t), off, dv, gset)
      case (TYPE_FLOAT32, TYPE_FLOAT64)
         call emit_float_field(an, ht, type_which(t), off, dv, gset)
      case (TYPE_ENUM)
         call emit_enum_field(an, ht, off, dv, gset)
      case (TYPE_TEXT)
         call emit_text_field(an, ht, int(off), dv, gset, err)
      case (TYPE_DATA)
         call emit_data_field(an, ht, int(off), dv, gset, err)
      case (TYPE_STRUCT)
         call emit_struct_field(an, ht, int(off), type_type_id(t), &
                                field_slot_had_default(f), dv, gset, err)
      case (TYPE_LIST)
         call emit_list_field(an, ht, int(off), t, &
                              field_slot_had_default(f), dv, gset, err)
      case (TYPE_ANY_POINTER)
         call emit_anyptr_field(an, ht, int(off), gset)
      case default
         call w('   ! field '//an//': unsupported type '// &
                itoa(int(type_which(t), int64)))
         call w('')
      end select
   end subroutine emit_field

   !> Serialize a Value's default object into a standalone message blob and
   !> register it (pass 1). Returns whether a non-null default exists.
   function register_default_blob(an, dv) result(has)
      character(len=*), intent(in) :: an
      type(capnp_ptr_t), intent(in) :: dv
      logical :: has
      type(capnp_message_t), target :: tmp
      type(capnp_ptr_t) :: dobj, c
      integer(int8), allocatable :: bytes(:)
      integer :: err
      has = .false.
      select case (value_which(dv))
      case (TYPE_STRUCT, TYPE_LIST, TYPE_ANY_POINTER)
      case default
         return
      end select
      dobj = value_pointer(dv, err)
      if (err /= CAPNP_OK .or. dobj%kind == CAPNP_PK_NULL) return
      has = .true.
      if (.not. g_suppress) return ! blob already collected in pass 1
      call capnp_message_init_builder(tmp, err)
      if (err /= CAPNP_OK) return
      c = capnp_copy(tmp, dobj, err)
      if (err /= CAPNP_OK) return
      call capnp_set_root(tmp, c, err)
      if (err /= CAPNP_OK) return
      call capnp_serialize_bytes(tmp, bytes, err)
      if (err /= CAPNP_OK) return
      call note_blob(upcase(an)//'_DEFAULT', bytes)
      call capnp_message_free(tmp)
   end function register_default_blob

   subroutine emit_anyptr_field(an, ht, pidx, gset)
      character(len=*), intent(in) :: an, ht, gset
      integer, intent(in) :: pidx
      call w('   function '//an//'_get(h, err) result(q)')
      call w('      type('//ht//'_t), intent(in) :: h')
      call w('      integer, intent(out) :: err')
      call w('      type(capnp_ptr_t) :: q')
      call w('      q = capnp_getp(h%p, '//itoa(int(pidx, int64))//', err)')
      call w('   end function '//an//'_get')
      call w('')
      call w('   subroutine '//an//'_set(h, q, err)')
      call w('      type('//ht//'_t), intent(in) :: h')
      call w('      type(capnp_ptr_t), intent(in) :: q')
      call w('      integer, intent(out) :: err')
      if (len(gset) > 0) call w(gset)
      call w('      call capnp_setp(h%p, '//itoa(int(pidx, int64))//', q, err)')
      call w('   end subroutine '//an//'_set')
      call w('')
   end subroutine emit_anyptr_field

   ! --- field emitters ------------------------------------------------------

   subroutine emit_bool_field(an, ht, bit_off, dv, gset)
      character(len=*), intent(in) :: an, ht, gset
      integer(int64), intent(in) :: bit_off
      type(capnp_ptr_t), intent(in) :: dv
      character(len=:), allocatable :: defarg
      defarg = ''
      if (value_which(dv) == TYPE_BOOL) then
         if (value_bool(dv)) defarg = ', default=.true.'
      end if
      call w('   function '//an//'_get(h) result(v)')
      call w('      type('//ht//'_t), intent(in) :: h')
      call w('      logical :: v')
      call w('      v = capnp_get_bool(h%p, '//itoa(bit_off)//'_int64'//defarg//')')
      call w('   end function '//an//'_get')
      call w('')
      call w('   subroutine '//an//'_set(h, v, err)')
      call w('      type('//ht//'_t), intent(in) :: h')
      call w('      logical, intent(in) :: v')
      call w('      integer, intent(out) :: err')
      if (len(gset) > 0) call w(gset)
      call w('      call capnp_set_bool(h%p, '//itoa(bit_off)//'_int64, v, err'//defarg//')')
      call w('   end subroutine '//an//'_set')
      call w('')
   end subroutine emit_bool_field

   subroutine emit_int_field(an, ht, tw, off, dv, gset)
      character(len=*), intent(in) :: an, ht, gset
      integer, intent(in) :: tw
      integer(int64), intent(in) :: off
      type(capnp_ptr_t), intent(in) :: dv
      character(len=:), allocatable :: acc, vt, defarg
      integer(int64) :: bytes, d
      select case (tw)
      case (TYPE_INT8); acc = 'i8'; vt = 'integer(int8)'; bytes = 1
      case (TYPE_INT16); acc = 'i16'; vt = 'integer(int16)'; bytes = 2
      case (TYPE_INT32); acc = 'i32'; vt = 'integer(int32)'; bytes = 4
      case (TYPE_INT64); acc = 'i64'; vt = 'integer(int64)'; bytes = 8
      case (TYPE_UINT8); acc = 'u8'; vt = 'integer(int16)'; bytes = 1
      case (TYPE_UINT16); acc = 'u16'; vt = 'integer(int32)'; bytes = 2
      case (TYPE_UINT32); acc = 'u32'; vt = 'integer(int64)'; bytes = 4
      case default; acc = 'i64'; vt = 'integer(int64)'; bytes = 8 ! uint64
      end select
      d = const_int_value(dv)
      defarg = ''
      if (d /= 0_int64) defarg = ', default='//itoa(d)//kind_suffix(vt)
      call w('   function '//an//'_get(h) result(v)')
      call w('      type('//ht//'_t), intent(in) :: h')
      call w('      '//vt//' :: v')
      call w('      v = capnp_get_'//acc//'(h%p, '//itoa(off*bytes)//'_int64'//defarg//')')
      call w('   end function '//an//'_get')
      call w('')
      call w('   subroutine '//an//'_set(h, v, err)')
      call w('      type('//ht//'_t), intent(in) :: h')
      call w('      '//vt//', intent(in) :: v')
      call w('      integer, intent(out) :: err')
      if (len(gset) > 0) call w(gset)
      call w('      call capnp_set_'//acc//'(h%p, '//itoa(off*bytes)//'_int64, v, err'//defarg//')')
      call w('   end subroutine '//an//'_set')
      call w('')
   end subroutine emit_int_field

   pure function kind_suffix(vt) result(s)
      character(len=*), intent(in) :: vt
      character(len=:), allocatable :: s
      s = '_'//vt(9:len(vt) - 1) ! integer(intNN) -> _intNN
   end function kind_suffix

   subroutine emit_float_field(an, ht, tw, off, dv, gset)
      character(len=*), intent(in) :: an, ht, gset
      integer, intent(in) :: tw
      integer(int64), intent(in) :: off
      type(capnp_ptr_t), intent(in) :: dv
      character(len=:), allocatable :: acc, vt, defarg
      integer(int64) :: bytes
      if (tw == TYPE_FLOAT32) then
         acc = 'f32'; vt = 'real(real32)'; bytes = 4
         defarg = ''
         if (value_which(dv) == TYPE_FLOAT32) then
            if (cp_f32_bits(value_f32(dv)) /= 0_int32) &
               defarg = ', default=cp_bits_f32('// &
                        itoa(int(cp_f32_bits(value_f32(dv)), int64))//'_int32)'
         end if
      else
         acc = 'f64'; vt = 'real(real64)'; bytes = 8
         defarg = ''
         if (value_which(dv) == TYPE_FLOAT64) then
            if (cp_f64_bits(value_f64(dv)) /= 0_int64) &
               defarg = ', default=cp_bits_f64('// &
                        itoa(cp_f64_bits(value_f64(dv)))//'_int64)'
         end if
      end if
      call w('   function '//an//'_get(h) result(v)')
      call w('      type('//ht//'_t), intent(in) :: h')
      call w('      '//vt//' :: v')
      call w('      v = capnp_get_'//acc//'(h%p, '//itoa(off*bytes)//'_int64'//defarg//')')
      call w('   end function '//an//'_get')
      call w('')
      call w('   subroutine '//an//'_set(h, v, err)')
      call w('      type('//ht//'_t), intent(in) :: h')
      call w('      '//vt//', intent(in) :: v')
      call w('      integer, intent(out) :: err')
      if (len(gset) > 0) call w(gset)
      call w('      call capnp_set_'//acc//'(h%p, '//itoa(off*bytes)//'_int64, v, err'//defarg//')')
      call w('   end subroutine '//an//'_set')
      call w('')
   end subroutine emit_float_field

   subroutine emit_enum_field(an, ht, off, dv, gset)
      character(len=*), intent(in) :: an, ht, gset
      integer(int64), intent(in) :: off
      type(capnp_ptr_t), intent(in) :: dv
      character(len=:), allocatable :: defarg
      integer :: d
      d = 0
      if (value_which(dv) == TYPE_ENUM) d = value_enum(dv)
      defarg = ''
      if (d /= 0) defarg = ', default='//itoa(int(d, int64))//'_int32'
      call w('   function '//an//'_get(h) result(v)')
      call w('      type('//ht//'_t), intent(in) :: h')
      call w('      integer :: v')
      call w('      v = int(capnp_get_u16(h%p, '//itoa(off*2_int64)//'_int64'//defarg//'))')
      call w('   end function '//an//'_get')
      call w('')
      call w('   subroutine '//an//'_set(h, v, err)')
      call w('      type('//ht//'_t), intent(in) :: h')
      call w('      integer, intent(in) :: v')
      call w('      integer, intent(out) :: err')
      if (len(gset) > 0) call w(gset)
      call w('      call capnp_set_u16(h%p, '//itoa(off*2_int64)// &
             '_int64, int(v, int32), err'//defarg//')')
      call w('   end subroutine '//an//'_set')
      call w('')
   end subroutine emit_enum_field

   subroutine emit_text_field(an, ht, pidx, dv, gset, err)
      character(len=*), intent(in) :: an, ht, gset
      integer, intent(in) :: pidx
      type(capnp_ptr_t), intent(in) :: dv
      integer, intent(out) :: err
      character(len=:), allocatable :: dtext
      err = CAPNP_OK
      dtext = ''
      if (value_which(dv) == TYPE_TEXT) call value_text(dv, dtext, err)
      if (err /= CAPNP_OK) return
      call w('   subroutine '//an//'_get(h, s, err)')
      call w('      type('//ht//'_t), intent(in) :: h')
      call w('      character(len=:), allocatable, intent(out) :: s')
      call w('      integer, intent(out) :: err')
      if (len(dtext) > 0) then
         call w('      type(capnp_ptr_t) :: q')
         call w('      q = capnp_getp(h%p, '//itoa(int(pidx, int64))//', err)')
         call w('      if (err == CAPNP_OK .and. q%kind == CAPNP_PK_NULL) then')
         call w("         s = '"//dtext//"'")
         call w('         return')
         call w('      end if')
      end if
      call w('      call capnp_get_text(h%p, '//itoa(int(pidx, int64))//', s, err)')
      call w('   end subroutine '//an//'_get')
      call w('')
      call w('   subroutine '//an//'_set(h, s, err)')
      call w('      type('//ht//'_t), intent(in) :: h')
      call w('      character(len=*), intent(in) :: s')
      call w('      integer, intent(out) :: err')
      if (len(gset) > 0) call w(gset)
      call w('      call capnp_set_text(h%p, '//itoa(int(pidx, int64))//', s, err)')
      call w('   end subroutine '//an//'_set')
      call w('')
   end subroutine emit_text_field

   subroutine emit_data_field(an, ht, pidx, dv, gset, err)
      character(len=*), intent(in) :: an, ht, gset
      integer, intent(in) :: pidx
      type(capnp_ptr_t), intent(in) :: dv
      integer, intent(out) :: err
      integer(int8), allocatable :: db(:)
      err = CAPNP_OK
      if (value_which(dv) == TYPE_DATA) then
         call value_data(dv, db, err)
         if (err /= CAPNP_OK) return
         if (size(db) > 0 .and. g_suppress) call note_blob(upcase(an)//'_DEFAULT', db)
      else
         allocate (db(0))
      end if
      call w('   subroutine '//an//'_get(h, b, err)')
      call w('      type('//ht//'_t), intent(in) :: h')
      call w('      integer(int8), allocatable, intent(out) :: b(:)')
      call w('      integer, intent(out) :: err')
      if (size(db) > 0) then
         call w('      type(capnp_ptr_t) :: q')
         call w('      q = capnp_getp(h%p, '//itoa(int(pidx, int64))//', err)')
         call w('      if (err == CAPNP_OK .and. q%kind == CAPNP_PK_NULL) then')
         call w('         b = '//upcase(an)//'_DEFAULT')
         call w('         return')
         call w('      end if')
      end if
      call w('      call capnp_get_data(h%p, '//itoa(int(pidx, int64))//', b, err)')
      call w('   end subroutine '//an//'_get')
      call w('')
      call w('   subroutine '//an//'_set(h, b, err)')
      call w('      type('//ht//'_t), intent(in) :: h')
      call w('      integer(int8), intent(in) :: b(:)')
      call w('      integer, intent(out) :: err')
      if (len(gset) > 0) call w(gset)
      call w('      call capnp_set_data(h%p, '//itoa(int(pidx, int64))//', b, err)')
      call w('   end subroutine '//an//'_set')
      call w('')
   end subroutine emit_data_field

   subroutine emit_struct_field(an, ht, pidx, tid, had_default, dv, gset, err)
      character(len=*), intent(in) :: an, ht, gset
      integer, intent(in) :: pidx
      integer(int64), intent(in) :: tid
      logical, intent(in) :: had_default
      type(capnp_ptr_t), intent(in) :: dv
      integer, intent(out) :: err
      character(len=:), allocatable :: st
      integer :: idx
      logical :: hasdef
      err = CAPNP_OK
      idx = find_node(tid)
      if (idx == 0) then
         err = CAPNP_ERR_ARG
         return
      end if
      call note_import(idx, err)
      if (err /= CAPNP_OK) return
      call node_fname(g_nodes(idx)%p, st, err)
      if (err /= CAPNP_OK) return
      hasdef = .false.
      if (had_default) hasdef = register_default_blob(an, dv)
      call w('   function '//an//'_get(h, err) result(o)')
      call w('      type('//ht//'_t), intent(in) :: h')
      call w('      integer, intent(out) :: err')
      call w('      type('//st//'_t) :: o')
      if (hasdef) then
         call w('      type(capnp_message_t), save, target :: defmsg')
         call w('      logical, save :: defloaded = .false.')
         call w('      type(capnp_ptr_t) :: d')
         call w('      o%p = capnp_getp(h%p, '//itoa(int(pidx, int64))//', err)')
         call w('      if (err /= CAPNP_OK .or. o%p%kind /= CAPNP_PK_NULL) return')
         call w('      if (.not. defloaded) then')
         call w('         call capnp_deserialize_bytes('//upcase(an)//'_DEFAULT, defmsg, err)')
         call w('         if (err /= CAPNP_OK) return')
         call w('         defloaded = .true.')
         call w('      end if')
         call w('      d = capnp_root(defmsg, err)')
         call w('      if (err /= CAPNP_OK) return')
         call w('      if (associated(h%p%msg)) then')
         call w('         if (h%p%msg%is_builder) then')
         call w('            ! Builder get() materialises the default in place, as C++ does.')
         call w('            o%p = capnp_copy(h%p%msg, d, err)')
         call w('            if (err == CAPNP_OK) call capnp_setp(h%p, '// &
                itoa(int(pidx, int64))//', o%p, err)')
         call w('            return')
         call w('         end if')
         call w('      end if')
         call w('      o%p = d')
      else
         call w('      o%p = capnp_getp(h%p, '//itoa(int(pidx, int64))//', err)')
      end if
      call w('   end function '//an//'_get')
      call w('')
      call w('   function '//an//'_init(h, err) result(o)')
      call w('      type('//ht//'_t), intent(in) :: h')
      call w('      integer, intent(out) :: err')
      call w('      type('//st//'_t) :: o')
      if (len(gset) > 0) call w(gset)
      call w('      o%p = capnp_new_struct(h%p%msg, '//upcase(st)//'_DWORDS, '// &
             upcase(st)//'_PWORDS, err)')
      call w('      if (err == CAPNP_OK) call capnp_setp(h%p, '// &
             itoa(int(pidx, int64))//', o%p, err)')
      call w('   end function '//an//'_init')
      call w('')
   end subroutine emit_struct_field

   subroutine emit_list_field(an, ht, pidx, t, had_default, dv, gset, err)
      character(len=*), intent(in) :: an, ht, gset
      integer, intent(in) :: pidx
      type(capnp_ptr_t), intent(in) :: t, dv
      logical, intent(in) :: had_default
      integer, intent(out) :: err
      type(capnp_ptr_t) :: et
      character(len=:), allocatable :: newexpr, st
      integer :: idx
      logical :: hasdef
      et = type_list_element(t, err)
      if (err /= CAPNP_OK) return
      select case (type_which(et))
      case (TYPE_STRUCT)
         idx = find_node(type_type_id(et))
         if (idx == 0) then
            err = CAPNP_ERR_ARG
            return
         end if
         call note_import(idx, err)
         if (err /= CAPNP_OK) return
         call node_fname(g_nodes(idx)%p, st, err)
         if (err /= CAPNP_OK) return
         newexpr = 'capnp_new_composite_list(h%p%msg, n, '//upcase(st)// &
                   '_DWORDS, '//upcase(st)//'_PWORDS, err)'
      case default
         newexpr = 'capnp_new_list(h%p%msg, '// &
                   itoa(int(list_esize_for(type_which(et)), int64))//', n, err)'
      end select
      hasdef = .false.
      if (had_default) hasdef = register_default_blob(an, dv)
      call w('   function '//an//'_get(h, err) result(l)')
      call w('      type('//ht//'_t), intent(in) :: h')
      call w('      integer, intent(out) :: err')
      call w('      type(capnp_ptr_t) :: l')
      if (hasdef) then
         call w('      type(capnp_message_t), save, target :: defmsg')
         call w('      logical, save :: defloaded = .false.')
         call w('      l = capnp_getp(h%p, '//itoa(int(pidx, int64))//', err)')
         call w('      if (err /= CAPNP_OK .or. l%kind /= CAPNP_PK_NULL) return')
         call w('      if (.not. defloaded) then')
         call w('         call capnp_deserialize_bytes('//upcase(an)//'_DEFAULT, defmsg, err)')
         call w('         if (err /= CAPNP_OK) return')
         call w('         defloaded = .true.')
         call w('      end if')
         call w('      l = capnp_root(defmsg, err)')
      else
         call w('      l = capnp_getp(h%p, '//itoa(int(pidx, int64))//', err)')
      end if
      call w('   end function '//an//'_get')
      call w('')
      select case (type_which(et))
      case (TYPE_TEXT)
         call w('   subroutine '//an//'_get_elem(h, i, s, err)')
         call w('      type('//ht//'_t), intent(in) :: h')
         call w('      integer, intent(in) :: i')
         call w('      character(len=:), allocatable, intent(out) :: s')
         call w('      integer, intent(out) :: err')
         call w('      type(capnp_ptr_t) :: l')
         call w('      l = capnp_getp(h%p, '//itoa(int(pidx, int64))//', err)')
         call w('      if (err == CAPNP_OK) call capnp_list_get_text(l, i, s, err)')
         call w('   end subroutine '//an//'_get_elem')
         call w('')
      case (TYPE_DATA)
         call w('   subroutine '//an//'_get_elem(h, i, b, err)')
         call w('      type('//ht//'_t), intent(in) :: h')
         call w('      integer, intent(in) :: i')
         call w('      integer(int8), allocatable, intent(out) :: b(:)')
         call w('      integer, intent(out) :: err')
         call w('      type(capnp_ptr_t) :: l')
         call w('      l = capnp_getp(h%p, '//itoa(int(pidx, int64))//', err)')
         call w('      if (err == CAPNP_OK) call capnp_get_data(l, i, b, err)')
         call w('   end subroutine '//an//'_get_elem')
         call w('')
      end select
      call w('   function '//an//'_init(h, n, err) result(l)')
      call w('      type('//ht//'_t), intent(in) :: h')
      call w('      integer(int64), intent(in) :: n')
      call w('      integer, intent(out) :: err')
      call w('      type(capnp_ptr_t) :: l')
      if (len(gset) > 0) call w(gset)
      call w('      l = '//newexpr)
      call w('      if (err == CAPNP_OK) call capnp_setp(h%p, '// &
             itoa(int(pidx, int64))//', l, err)')
      call w('   end function '//an//'_init')
      call w('')
   end subroutine emit_list_field

   pure function list_esize_for(tw) result(es)
      integer, intent(in) :: tw
      integer :: es
      select case (tw)
      case (TYPE_VOID); es = CAPNP_SZ_VOID
      case (TYPE_BOOL); es = CAPNP_SZ_BIT
      case (TYPE_INT8, TYPE_UINT8); es = CAPNP_SZ_BYTE
      case (TYPE_INT16, TYPE_UINT16, TYPE_ENUM); es = CAPNP_SZ_TWO
      case (TYPE_INT32, TYPE_UINT32, TYPE_FLOAT32); es = CAPNP_SZ_FOUR
      case (TYPE_INT64, TYPE_UINT64, TYPE_FLOAT64); es = CAPNP_SZ_EIGHT
      case default; es = CAPNP_SZ_PTR ! text, data, list, anyPointer
      end select
   end function list_esize_for

end module capnpc_emit
