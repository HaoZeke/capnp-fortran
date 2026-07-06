!> Fortran code emitter: walks the node graph and writes one module per
!> requested schema file. Naming: camelCase becomes snake_case; nested
!> scopes join with underscores (Person.PhoneNumber -> person_phone_number).
module capnpc_emit
   use capnp
   use capnp_schema
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
   !> Whether this file declares interfaces (adds `use capnp_rpc`).
   logical :: g_has_iface = .false.

   !> Brand instantiations: one per distinct (generic node, bindings)
   !> use, e.g. Box(Text) -> box_text. Collected in pass 1 from branded
   !> struct fields; each gets a handle type plus accessors with the
   !> generic parameters substituted.
   type :: inst_t
      integer :: gidx = 0
      character(len=:), allocatable :: name
      type(capnp_ptr_t) :: bindings(0:3)
      integer :: nbind = 0
   end type inst_t
   type(inst_t) :: g_insts(64)
   integer :: g_ninsts = 0
   !> Nonzero while emitting an instantiation's accessors.
   integer :: g_cur_inst = 0

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
   !> scope ids). Deeply nested nodes overflow Fortran's 63-char identifier
   !> limit once accessor suffixes are appended, so long names compress to
   !> a 15-char head plus the node's unique 16-hex id (32 chars total,
   !> leaving room for `_<field>_set_elem`-class suffixes).
   subroutine node_fname(np, o, err)
      type(capnp_ptr_t), intent(in) :: np
      character(len=:), allocatable, intent(out) :: o
      integer, intent(out) :: err
      character(len=:), allocatable :: dn
      character(len=16) :: hexid
      integer :: i
      call node_display_name(np, dn, err)
      if (err /= CAPNP_OK) return
      i = index(dn, ':')
      if (i > 0) dn = dn(i + 1:)
      o = ''
      do i = 1, len(dn)
         ! Scope dots and non-identifier characters (implicit method
         ! structs carry '$', e.g. Adder.add$Params) become underscores;
         ! snake() collapses any doubling.
         if ((dn(i:i) >= 'a' .and. dn(i:i) <= 'z') .or. &
             (dn(i:i) >= 'A' .and. dn(i:i) <= 'Z') .or. &
             (dn(i:i) >= '0' .and. dn(i:i) <= '9') .or. dn(i:i) == '_') then
            o = o//dn(i:i)
         else
            if (len(o) > 0) then
               if (o(len(o):len(o)) /= '_') o = o//'_'
            end if
         end if
      end do
      o = snake(o)
      if (len(o) > 32) then
         write (hexid, '(z16.16)') node_id(np)
         do i = 1, 16
            if (hexid(i:i) >= 'A' .and. hexid(i:i) <= 'F') &
               hexid(i:i) = achar(iachar(hexid(i:i)) + 32)
         end do
         o = o(1:15)
         if (o(15:15) == '_') o = o(1:14)
         o = o//'_'//hexid
      end if
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

   !> Fortran character-literal body: single quotes doubled.
   pure function fquote(s) result(o)
      character(len=*), intent(in) :: s
      character(len=:), allocatable :: o
      integer :: i
      o = ''
      do i = 1, len(s)
         o = o//s(i:i)
         if (s(i:i) == "'") o = o//"'"
      end do
   end function fquote

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
      do i = 1, len(dn)
         if (dn(i:i) == '-') dn(i:i) = '_'
      end do
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

   !> Typed constructor with bare default-kind values: -128 stays legal
   !> (a bare -128_int8 literal is unary minus on out-of-range 128_int8).
   subroutine emit_blob_params()
      integer :: i
      integer(int64) :: j, n
      character(len=:), allocatable :: line
      do i = 1, g_nblobs
         n = size(g_blobs(i)%bytes, kind=int64)
         call w('   integer(int8), parameter :: '//g_blobs(i)%name//'(0:'// &
                itoa(n - 1)//') = [integer(int8) :: &')
         line = '      '
         do j = 1_int64, n
            line = line//itoa(int(g_blobs(i)%bytes(j), int64))
            if (j < n) line = line//', '
            if (mod(j, 16_int64) == 0_int64 .and. j < n) then
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
      do i = 1, len(base)
         if (base(i:i) == '-') base(i:i) = '_'
      end do
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
      g_has_iface = .false.
      g_ninsts = 0
      g_cur_inst = 0

      ! Pass 1 (dry): walk the procedures to discover cross-file imports and
      ! explicit pointer-default blobs before the header is written.
      g_suppress = .true.
      do i = 1, size(g_nodes)
         call node_display_name(g_nodes(i)%p, dn, err)
         if (err /= CAPNP_OK) return
         mine = len(dn) > len(prefix) .and. index(dn, prefix//':') == 1
         if (.not. mine) cycle
         select case (node_which(g_nodes(i)%p))
         case (NODE_STRUCT)
            if (.not. node_struct_is_group(g_nodes(i)%p)) then
               call emit_struct_procs(g_nodes(i)%p, err)
               if (err /= CAPNP_OK) return
            end if
         case (NODE_CONST)
            call emit_const_decl(g_nodes(i)%p, err)
            if (err /= CAPNP_OK) return
            call emit_const_proc(g_nodes(i)%p, err)
            if (err /= CAPNP_OK) return
         case (NODE_INTERFACE)
            g_has_iface = .true.
            call emit_interface_decl(g_nodes(i)%p, err)
            if (err /= CAPNP_OK) return
            call emit_interface_procs(g_nodes(i)%p, err)
            if (err /= CAPNP_OK) return
         end select
      end do
      ! Converge the instantiation worklist: emitting an instantiation's
      ! accessors can register further instantiations.
      i = 1
      do while (i <= g_ninsts)
         call emit_inst_procs(i, err)
         if (err /= CAPNP_OK) return
         i = i + 1
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
      if (g_has_iface) then
         call w('   use capnp_rpc')
         call w('   use rpc_capnp, only: payload_t, payload_content_set')
      end if
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
      if (err == CAPNP_OK) call emit_inst_decls(err)

      ! Interface declarations follow every struct declaration: their
      ! abstract method interfaces IMPORT the param/result handle types,
      ! which must already be declared.
      if (err == CAPNP_OK) then
         do i = 1, size(g_nodes)
            call node_display_name(g_nodes(i)%p, dn, err)
            if (err /= CAPNP_OK) exit
            mine = len(dn) > len(prefix) .and. index(dn, prefix//':') == 1
            if (.not. mine) cycle
            if (node_which(g_nodes(i)%p) == NODE_INTERFACE) then
               call emit_interface_decl(g_nodes(i)%p, err)
               if (err /= CAPNP_OK) exit
            end if
         end do
      end if

      call w('contains')
      call w('')

      ! Procedures pass: struct accessors (groups reached via parents).
      if (err == CAPNP_OK) then
         do i = 1, size(g_nodes)
            call node_display_name(g_nodes(i)%p, dn, err)
            if (err /= CAPNP_OK) exit
            mine = len(dn) > len(prefix) .and. index(dn, prefix//':') == 1
            if (.not. mine) cycle
            select case (node_which(g_nodes(i)%p))
            case (NODE_STRUCT)
               if (.not. node_struct_is_group(g_nodes(i)%p)) then
                  call emit_struct_procs(g_nodes(i)%p, err)
                  if (err /= CAPNP_OK) exit
               end if
            case (NODE_CONST)
               call emit_const_proc(g_nodes(i)%p, err)
               if (err /= CAPNP_OK) exit
            case (NODE_INTERFACE)
               call emit_interface_procs(g_nodes(i)%p, err)
               if (err /= CAPNP_OK) exit
            end select
         end do
         if (err == CAPNP_OK) then
            do i = 1, g_ninsts
               call emit_inst_procs(i, err)
               if (err /= CAPNP_OK) exit
            end do
         end if
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
      integer(int8), allocatable :: db(:)
      logical :: has
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
         call w('   character(len=*), parameter :: '//upcase(cn)//" = '"//fquote(s)//"'")
      case (TYPE_DATA)
         call value_data(v, db, err)
         if (err /= CAPNP_OK) return
         if (size(db) == 0) then
            call w('   integer(int8), parameter :: '//upcase(cn)// &
                   '(0:-1) = [integer(int8) ::]')
         else
            ! Pass 1 collects the bytes; emit_blob_params writes the parameter.
            call note_blob(upcase(cn), db)
         end if
      case (TYPE_STRUCT, TYPE_LIST, TYPE_ANY_POINTER)
         ! Pass 1 serializes the value into a <CN>_DEFAULT blob; the accessor
         ! function lands in the procedures section (emit_const_proc).
         has = register_default_blob(cn, v)
      case default
         call w('   ! const '//cn//': unsupported value kind '// &
                itoa(int(value_which(v), int64)))
      end select
   end subroutine emit_const_decl

   !> Pointer-valued constant accessor: materialise the blob message once
   !> (saved across calls) and hand out its root, so repeated reads alias
   !> one object as capnp-c and capnp-C++ const globals do.
   subroutine emit_const_proc(np, err)
      type(capnp_ptr_t), intent(in) :: np
      integer, intent(out) :: err
      type(capnp_ptr_t) :: v, t, dobj
      character(len=:), allocatable :: cn, st, lhs
      integer :: idx
      err = CAPNP_OK
      v = node_const_value(np, err)
      if (err /= CAPNP_OK) return
      select case (value_which(v))
      case (TYPE_STRUCT, TYPE_LIST, TYPE_ANY_POINTER)
      case default
         return
      end select
      call node_fname(np, cn, err)
      if (err /= CAPNP_OK) return
      st = ''
      lhs = 'o'
      if (value_which(v) == TYPE_STRUCT) then
         t = node_const_type(np, err)
         if (err /= CAPNP_OK) return
         idx = find_node(type_type_id(t))
         if (idx == 0) then
            err = CAPNP_ERR_ARG
            return
         end if
         call note_import(idx, err)
         if (err /= CAPNP_OK) return
         call node_fname(g_nodes(idx)%p, st, err)
         if (err /= CAPNP_OK) return
         lhs = 'o%p'
      end if
      dobj = value_pointer(v, err)
      if (err /= CAPNP_OK) return
      call w('   function '//cn//'(err) result(o)')
      call w('      integer, intent(out) :: err')
      if (len(st) > 0) then
         call w('      type('//st//'_t) :: o')
      else
         call w('      type(capnp_ptr_t) :: o')
      end if
      if (dobj%kind == CAPNP_PK_NULL) then
         call w('      err = CAPNP_OK')
      else
         call w('      type(capnp_message_t), save, target :: cmsg')
         call w('      logical, save :: loaded = .false.')
         call w('      err = CAPNP_OK')
         call w('      if (.not. loaded) then')
         call w('         call capnp_deserialize_bytes('//upcase(cn)//'_DEFAULT, cmsg, err)')
         call w('         if (err /= CAPNP_OK) return')
         call w('         loaded = .true.')
         call w('      end if')
         call w('      '//lhs//' = capnp_root(cmsg, err)')
      end if
      call w('   end function '//cn)
      call w('')
   end subroutine emit_const_proc

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
      call emit_union_tag_decls(np, tn, err)
      call w('')
   end subroutine emit_struct_decl

   !> Named tag constants for union members: <PREFIX>_<FIELD>_TAG, the
   !> value <prefix>_which(h) returns when that member is active.
   recursive subroutine emit_union_tag_decls(np, pfx, err)
      type(capnp_ptr_t), intent(in) :: np
      character(len=*), intent(in) :: pfx
      integer, intent(out) :: err
      type(capnp_ptr_t) :: fl, f
      character(len=:), allocatable :: fn
      integer(int64) :: i
      integer :: disc, gidx
      fl = node_struct_fields(np, err)
      if (err /= CAPNP_OK) return
      do i = 0_int64, capnp_list_len(fl) - 1_int64
         f = capnp_list_get_struct(fl, int(i), err)
         if (err /= CAPNP_OK) return
         call field_name(f, fn, err)
         if (err /= CAPNP_OK) return
         disc = field_discriminant(f)
         if (disc /= NO_DISCRIMINANT) then
            call w('   integer, parameter :: '//upcase(pfx)//'_'// &
                   upcase(snake(fn))//'_TAG = '//itoa(int(disc, int64)))
         end if
         if (field_which(f) == FIELD_GROUP) then
            gidx = find_node(field_group_type_id(f))
            if (gidx == 0) then
               err = CAPNP_ERR_ARG
               return
            end if
            call emit_union_tag_decls(g_nodes(gidx)%p, pfx//'_'//snake(fn), err)
            if (err /= CAPNP_OK) return
         end if
      end do
   end subroutine emit_union_tag_decls

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
      ! Inside a brand instantiation, generic parameters of the current
      ! generic scope resolve to their bound types.
      if (g_cur_inst > 0) then
         if (type_which(t) == TYPE_ANY_POINTER) then
            if (type_anyptr_which(t) == ANYPTR_PARAMETER) then
               if (type_param_scope_id(t) == &
                   g_nodes(g_insts(g_cur_inst)%gidx)%id) then
                  if (type_param_index(t) < g_insts(g_cur_inst)%nbind) then
                     ! Unbound parameters keep their AnyPointer accessors.
                     if (g_insts(g_cur_inst)%bindings(type_param_index(t))%kind &
                         /= CAPNP_PK_NULL) &
                        t = g_insts(g_cur_inst)%bindings(type_param_index(t))
                  end if
               end if
            end if
         end if
      end if
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
         call emit_struct_field(an, ht, int(off), t, &
                                field_slot_had_default(f), dv, gset, err)
      case (TYPE_LIST)
         call emit_list_field(an, ht, int(off), t, &
                              field_slot_had_default(f), dv, gset, err)
      case (TYPE_INTERFACE)
         call emit_interface_field(an, ht, int(off), t, gset, err)
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

   !> Interface-typed struct field: get/set the wire capability as a
   !> <iface>_client_t (cap index in/out). Uses the same client type the
   !> method stubs generate for that interface.
   subroutine emit_interface_field(an, ht, pidx, t, gset, err)
      character(len=*), intent(in) :: an, ht, gset
      integer, intent(in) :: pidx
      type(capnp_ptr_t), intent(in) :: t
      integer, intent(out) :: err
      character(len=:), allocatable :: st
      integer :: idx
      err = CAPNP_OK
      idx = find_node(type_type_id(t))
      if (idx == 0) then
         err = CAPNP_ERR_ARG
         return
      end if
      call note_import(idx, err)
      if (err /= CAPNP_OK) return
      call node_fname(g_nodes(idx)%p, st, err)
      if (err /= CAPNP_OK) return
      g_has_iface = .true.
      call w('   function '//an//'_get(h, err) result(c)')
      call w('      type('//ht//'_t), intent(in) :: h')
      call w('      integer, intent(out) :: err')
      call w('      type('//st//'_client_t) :: c')
      call w('      type(capnp_ptr_t) :: q')
      call w('      c%cap%kind = RPC_CAP_NONE')
      call w('      c%cap%id = 0_int64')
      call w('      q = capnp_getp(h%p, '//itoa(int(pidx, int64))//', err)')
      call w('      if (err /= CAPNP_OK) return')
      call w('      if (q%kind == CAPNP_PK_CAP) then')
      call w('         c%cap%kind = RPC_CAP_IMPORT')
      call w('         c%cap%id = q%capidx')
      call w('      end if')
      call w('   end function '//an//'_get')
      call w('')
      call w('   subroutine '//an//'_set(h, c, err)')
      call w('      type('//ht//'_t), intent(in) :: h')
      call w('      type('//st//'_client_t), intent(in) :: c')
      call w('      integer, intent(out) :: err')
      call w('      type(capnp_ptr_t) :: q')
      if (len(gset) > 0) call w(gset)
      call w('      q = capnp_ptr_t()')
      call w('      if (associated(h%p%msg)) q%msg => h%p%msg')
      call w('      q%kind = CAPNP_PK_CAP')
      call w('      q%capidx = c%cap%id')
      call w('      call capnp_setp(h%p, '//itoa(int(pidx, int64))//', q, err)')
      call w('   end subroutine '//an//'_set')
      call w('')
   end subroutine emit_interface_field

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
         call w("         s = '"//fquote(dtext)//"'")
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

   subroutine emit_struct_field(an, ht, pidx, t, had_default, dv, gset, err)
      character(len=*), intent(in) :: an, ht, gset
      integer, intent(in) :: pidx
      type(capnp_ptr_t), intent(in) :: t
      logical, intent(in) :: had_default
      type(capnp_ptr_t), intent(in) :: dv
      integer, intent(out) :: err
      character(len=:), allocatable :: st, iname
      integer :: idx
      logical :: hasdef
      err = CAPNP_OK
      idx = find_node(type_type_id(t))
      if (idx == 0) then
         err = CAPNP_ERR_ARG
         return
      end if
      call note_import(idx, err)
      if (err /= CAPNP_OK) return
      call node_fname(g_nodes(idx)%p, st, err)
      if (err /= CAPNP_OK) return
      ! A branded generic use gets the instantiation's handle type.
      call inst_name_for(t, idx, iname, err)
      if (err /= CAPNP_OK) return
      if (len(iname) > 0) st = iname
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
      character(len=:), allocatable :: newexpr, st, iname
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
         ! A branded generic element gets the instantiation's handle type.
         call inst_name_for(et, idx, iname, err)
         if (err /= CAPNP_OK) return
         if (len(iname) > 0) st = iname
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
      case (TYPE_STRUCT)
         call w('   function '//an//'_get_elem(h, i, err) result(o)')
         call w('      type('//ht//'_t), intent(in) :: h')
         call w('      integer, intent(in) :: i')
         call w('      integer, intent(out) :: err')
         call w('      type('//st//'_t) :: o')
         call w('      type(capnp_ptr_t) :: l')
         call w('      l = capnp_getp(h%p, '//itoa(int(pidx, int64))//', err)')
         call w('      if (err == CAPNP_OK) o%p = capnp_list_get_struct(l, i, err)')
         call w('   end function '//an//'_get_elem')
         call w('')
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
         call w('   subroutine '//an//'_set_elem(h, i, s, err)')
         call w('      type('//ht//'_t), intent(in) :: h')
         call w('      integer, intent(in) :: i')
         call w('      character(len=*), intent(in) :: s')
         call w('      integer, intent(out) :: err')
         call w('      type(capnp_ptr_t) :: l')
         call w('      l = capnp_getp(h%p, '//itoa(int(pidx, int64))//', err)')
         call w('      if (err == CAPNP_OK) call capnp_list_set_text(l, i, s, err)')
         call w('   end subroutine '//an//'_set_elem')
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
         call w('   subroutine '//an//'_set_elem(h, i, b, err)')
         call w('      type('//ht//'_t), intent(in) :: h')
         call w('      integer, intent(in) :: i')
         call w('      integer(int8), intent(in) :: b(:)')
         call w('      integer, intent(out) :: err')
         call w('      type(capnp_ptr_t) :: l')
         call w('      l = capnp_getp(h%p, '//itoa(int(pidx, int64))//', err)')
         call w('      if (err == CAPNP_OK) call capnp_set_data(l, i, b, err)')
         call w('   end subroutine '//an//'_set_elem')
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

   ! --- brand instantiations ---------------------------------------------------

   !> Instantiation handle name for a branded struct Type, registering
   !> the instantiation on first sight. '' when the use is unbranded.
   subroutine inst_name_for(t, gidx, o, err)
      type(capnp_ptr_t), intent(in) :: t
      integer, intent(in) :: gidx
      character(len=:), allocatable, intent(out) :: o
      integer, intent(out) :: err
      type(capnp_ptr_t) :: b, scopes, sc, binds, bd
      type(capnp_ptr_t) :: btypes(0:3)
      character(len=:), allocatable :: gname, part
      integer(int64) :: i, j
      integer :: n, k, bidx
      err = CAPNP_OK
      o = ''
      b = type_brand(t, err)
      if (err /= CAPNP_OK) return
      if (b%kind == CAPNP_PK_NULL) return
      scopes = brand_scopes(b, err)
      if (err /= CAPNP_OK) return
      n = 0
      do i = 0_int64, capnp_list_len(scopes) - 1_int64
         sc = capnp_list_get_struct(scopes, int(i), err)
         if (err /= CAPNP_OK) return
         if (scope_scope_id(sc) /= g_nodes(gidx)%id) cycle
         if (scope_which(sc) /= SCOPE_BIND) cycle
         binds = scope_bindings(sc, err)
         if (err /= CAPNP_OK) return
         do j = 0_int64, min(capnp_list_len(binds), 4_int64) - 1_int64
            bd = capnp_list_get_struct(binds, int(j), err)
            if (err /= CAPNP_OK) return
            if (binding_which(bd) == BINDING_BOUND_TYPE) then
               btypes(n) = binding_type(bd, err)
               if (err /= CAPNP_OK) return
               ! A binding that is itself a parameter of the generic being
               ! instantiated resolves through the current instantiation,
               ! so nested branded uses (Box(T) inside Nest(T)) settle.
               if (g_cur_inst > 0) then
                  if (type_which(btypes(n)) == TYPE_ANY_POINTER) then
                     if (type_anyptr_which(btypes(n)) == ANYPTR_PARAMETER) then
                        if (type_param_scope_id(btypes(n)) == &
                            g_nodes(g_insts(g_cur_inst)%gidx)%id .and. &
                            type_param_index(btypes(n)) < g_insts(g_cur_inst)%nbind) &
                           btypes(n) = g_insts(g_cur_inst)%bindings(type_param_index(btypes(n)))
                     end if
                  end if
               end if
            else
               btypes(n) = capnp_ptr_t() ! unbound: stays AnyPointer
            end if
            n = n + 1
         end do
      end do
      if (n == 0) return
      call node_fname(g_nodes(gidx)%p, gname, err)
      if (err /= CAPNP_OK) return
      o = gname
      do k = 0, n - 1
         call inst_suffix(btypes(k), part, err)
         if (err /= CAPNP_OK) return
         o = o//'_'//part
      end do
      ! Long names truncate deterministically (content checksum), so a
      ! second sighting of the same instantiation dedups to one entry.
      if (len(o) > 32) then
         bidx = 0
         do k = 1, len(o)
            bidx = mod(bidx*31 + iachar(o(k:k)), 997)
         end do
         o = o(1:28)//'_'//itoa(int(bidx, int64))
      end if
      do k = 1, g_ninsts
         if (g_insts(k)%name == o) return
      end do
      if (g_ninsts >= size(g_insts)) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      g_ninsts = g_ninsts + 1
      g_insts(g_ninsts)%gidx = gidx
      g_insts(g_ninsts)%name = o
      g_insts(g_ninsts)%nbind = n
      do k = 0, n - 1
         g_insts(g_ninsts)%bindings(k) = btypes(k)
      end do
   end subroutine inst_name_for

   !> Name fragment for one binding type. Lists recurse into their
   !> element so distinct list bindings get distinct instantiations.
   recursive subroutine inst_suffix(bt, part, err)
      type(capnp_ptr_t), intent(in) :: bt
      character(len=:), allocatable, intent(out) :: part
      integer, intent(out) :: err
      type(capnp_ptr_t) :: et
      character(len=:), allocatable :: ep
      integer :: idx
      err = CAPNP_OK
      if (bt%kind == CAPNP_PK_NULL) then
         part = 'any'
         return
      end if
      select case (type_which(bt))
      case (TYPE_BOOL)
         part = 'bool'
      case (TYPE_INT8, TYPE_INT16, TYPE_INT32, TYPE_INT64)
         part = 'i'//itoa(int(8*2**(type_which(bt) - TYPE_INT8), int64))
      case (TYPE_UINT8, TYPE_UINT16, TYPE_UINT32, TYPE_UINT64)
         part = 'u'//itoa(int(8*2**(type_which(bt) - TYPE_UINT8), int64))
      case (TYPE_FLOAT32)
         part = 'f32'
      case (TYPE_FLOAT64)
         part = 'f64'
      case (TYPE_TEXT)
         part = 'text'
      case (TYPE_DATA)
         part = 'data'
      case (TYPE_LIST)
         et = type_list_element(bt, err)
         if (err /= CAPNP_OK) return
         call inst_suffix(et, ep, err)
         if (err /= CAPNP_OK) return
         part = 'list_'//ep
      case (TYPE_STRUCT, TYPE_ENUM, TYPE_INTERFACE)
         idx = find_node(type_type_id(bt))
         if (idx == 0) then
            err = CAPNP_ERR_ARG
            return
         end if
         call note_import(idx, err)
         if (err /= CAPNP_OK) return
         call node_fname(g_nodes(idx)%p, part, err)
      case default
         part = 'any'
      end select
   end subroutine inst_suffix

   !> Handle types and size parameters for every instantiation.
   subroutine emit_inst_decls(err)
      integer, intent(out) :: err
      integer :: k
      type(capnp_ptr_t) :: np
      err = CAPNP_OK
      do k = 1, g_ninsts
         np = g_nodes(g_insts(k)%gidx)%p
         call w('   integer, parameter :: '//upcase(g_insts(k)%name)//'_DWORDS = '// &
                itoa(int(node_struct_data_words(np), int64)))
         call w('   integer, parameter :: '//upcase(g_insts(k)%name)//'_PWORDS = '// &
                itoa(int(node_struct_pointer_count(np), int64)))
         call w('   type :: '//g_insts(k)%name//'_t')
         call w('      type(capnp_ptr_t) :: p')
         call w('   end type '//g_insts(k)%name//'_t')
         call w('')
      end do
   end subroutine emit_inst_decls

   !> Accessors for one instantiation: the generic node's fields with
   !> its parameters substituted by the bindings.
   subroutine emit_inst_procs(k, err)
      integer, intent(in) :: k
      integer, intent(out) :: err
      character(len=:), allocatable :: tn
      integer :: prev
      tn = g_insts(k)%name
      call w('   function '//tn//'_new(msg, err) result(h)')
      call w('      type(capnp_message_t), intent(inout), target :: msg')
      call w('      integer, intent(out) :: err')
      call w('      type('//tn//'_t) :: h')
      call w('      h%p = capnp_new_struct(msg, '//upcase(tn)//'_DWORDS, '// &
             upcase(tn)//'_PWORDS, err)')
      call w('   end function '//tn//'_new')
      call w('')
      call w('   function '//tn//'_read_root(msg, err) result(h)')
      call w('      type(capnp_message_t), intent(inout), target :: msg')
      call w('      integer, intent(out) :: err')
      call w('      type('//tn//'_t) :: h')
      call w('      h%p = capnp_root(msg, err)')
      call w('   end function '//tn//'_read_root')
      call w('')
      prev = g_cur_inst
      g_cur_inst = k
      call emit_fields_of(g_nodes(g_insts(k)%gidx)%p, tn, tn, err)
      g_cur_inst = prev
   end subroutine emit_inst_procs

   ! --- interfaces -----------------------------------------------------------

   !> Resolve a method's param and result struct handle-type names,
   !> registering cross-file imports.
   subroutine method_types(mp, pn, rn, err)
      type(capnp_ptr_t), intent(in) :: mp
      character(len=:), allocatable, intent(out) :: pn, rn
      integer, intent(out) :: err
      integer :: pidx, ridx
      pidx = find_node(method_param_struct_type(mp))
      ridx = find_node(method_result_struct_type(mp))
      if (pidx == 0 .or. ridx == 0) then
         err = CAPNP_ERR_ARG
         return
      end if
      call note_import(pidx, err)
      if (err /= CAPNP_OK) return
      call note_import(ridx, err)
      if (err /= CAPNP_OK) return
      call node_fname(g_nodes(pidx)%p, pn, err)
      if (err /= CAPNP_OK) return
      call node_fname(g_nodes(ridx)%p, rn, err)
   end subroutine method_types

   !> Interface declarations: the interface id, a client handle type, an
   !> abstract server base extending rpc_server_t whose generated dispatch
   !> routes method ordinals to deferred typed procedures.
   subroutine emit_interface_decl(np, err)
      type(capnp_ptr_t), intent(in) :: np
      integer, intent(out) :: err
      type(capnp_ptr_t) :: ml, mp
      character(len=:), allocatable :: tn, mn, pn, rn
      integer(int64) :: i
      call node_fname(np, tn, err)
      if (err /= CAPNP_OK) return
      ml = node_interface_methods(np, err)
      if (err /= CAPNP_OK) return
      call w('   integer(int64), parameter :: '//upcase(tn)//'_INTERFACE_ID = '// &
             itoa(node_id(np))//'_int64')
      call w('   type :: '//tn//'_client_t')
      call w('      type(rpc_cap_t) :: cap')
      call w('   end type '//tn//'_client_t')
      call w('   type, extends(rpc_server_t), abstract :: '//tn//'_server_t')
      call w('   contains')
      do i = 0_int64, capnp_list_len(ml) - 1_int64
         mp = capnp_list_get_struct(ml, int(i), err)
         if (err /= CAPNP_OK) return
         call method_name(mp, mn, err)
         if (err /= CAPNP_OK) return
         mn = snake(mn)
         call w('      procedure('//tn//'_'//mn//'_ifc), deferred :: '//mn)
      end do
      call w('      procedure :: dispatch => '//tn//'_dispatch')
      call w('   end type '//tn//'_server_t')
      if (capnp_list_len(ml) > 0_int64) then
         call w('   abstract interface')
         do i = 0_int64, capnp_list_len(ml) - 1_int64
            mp = capnp_list_get_struct(ml, int(i), err)
            if (err /= CAPNP_OK) return
            call method_name(mp, mn, err)
            if (err /= CAPNP_OK) return
            mn = snake(mn)
            call method_types(mp, pn, rn, err)
            if (err /= CAPNP_OK) return
            call w('      subroutine '//tn//'_'//mn//'_ifc(self, params, results, err)')
            if (pn == rn) then
               call w('         import :: '//tn//'_server_t, '//pn//'_t')
            else
               call w('         import :: '//tn//'_server_t, '//pn//'_t, '//rn//'_t')
            end if
            call w('         class('//tn//'_server_t), intent(inout) :: self')
            call w('         type('//pn//'_t), intent(in) :: params')
            call w('         type('//rn//'_t), intent(in) :: results')
            call w('         integer, intent(out) :: err')
            call w('      end subroutine '//tn//'_'//mn//'_ifc')
         end do
         call w('   end interface')
      end if
      call w('')
   end subroutine emit_interface_decl

   !> Client call helpers and the server dispatch router.
   subroutine emit_interface_procs(np, err)
      type(capnp_ptr_t), intent(in) :: np
      integer, intent(out) :: err
      type(capnp_ptr_t) :: ml, mp
      character(len=:), allocatable :: tn, mn, pn, rn
      integer(int64) :: i
      call node_fname(np, tn, err)
      if (err /= CAPNP_OK) return
      ml = node_interface_methods(np, err)
      if (err /= CAPNP_OK) return
      do i = 0_int64, capnp_list_len(ml) - 1_int64
         mp = capnp_list_get_struct(ml, int(i), err)
         if (err /= CAPNP_OK) return
         call method_name(mp, mn, err)
         if (err /= CAPNP_OK) return
         mn = snake(mn)
         call method_types(mp, pn, rn, err)
         if (err /= CAPNP_OK) return
         call w('   subroutine '//tn//'_'//mn//'_begin(conn, client, m, params, qid, err)')
         call w('      type(rpc_conn_t), intent(inout), target :: conn')
         call w('      type('//tn//'_client_t), intent(in) :: client')
         call w('      type(capnp_message_t), intent(inout), target :: m')
         call w('      type('//pn//'_t), intent(out) :: params')
         call w('      integer(int64), intent(out) :: qid')
         call w('      integer, intent(out) :: err')
         call w('      type(payload_t) :: pl')
         call w('      call rpc_call_begin(conn, client%cap, '//upcase(tn)// &
                '_INTERFACE_ID, '//itoa(i)//', m, pl, qid, err)')
         call w('      if (err /= CAPNP_OK) return')
         call w('      params%p = capnp_new_struct(m, '//upcase(pn)//'_DWORDS, '// &
                upcase(pn)//'_PWORDS, err)')
         call w('      if (err == CAPNP_OK) call payload_content_set(pl, params%p, err)')
         call w('   end subroutine '//tn//'_'//mn//'_begin')
         call w('')
         call w('   subroutine '//tn//'_'//mn//'_wait(conn, qid, results, err)')
         call w('      type(rpc_conn_t), intent(inout), target :: conn')
         call w('      integer(int64), intent(in) :: qid')
         call w('      type('//rn//'_t), intent(out) :: results')
         call w('      integer, intent(out) :: err')
         call w('      call rpc_wait(conn, qid, err)')
         call w('      if (err /= CAPNP_OK) return')
         call w('      call rpc_result_content(conn, qid, results%p, err)')
         call w('   end subroutine '//tn//'_'//mn//'_wait')
         call w('')
      end do
      call w('   subroutine '//tn//'_dispatch(self, ctx, err)')
      call w('      class('//tn//'_server_t), intent(inout) :: self')
      call w('      type(rpc_call_ctx_t), intent(inout) :: ctx')
      call w('      integer, intent(out) :: err')
      call w('      err = CAPNP_ERR_ARG')
      call w('      if (ctx%interface_id /= '//upcase(tn)//'_INTERFACE_ID) return')
      call w('      select case (ctx%method_id)')
      do i = 0_int64, capnp_list_len(ml) - 1_int64
         mp = capnp_list_get_struct(ml, int(i), err)
         if (err /= CAPNP_OK) return
         call method_name(mp, mn, err)
         if (err /= CAPNP_OK) return
         mn = snake(mn)
         call method_types(mp, pn, rn, err)
         if (err /= CAPNP_OK) return
         call w('      case ('//itoa(i)//')')
         call w('         block')
         call w('            type('//pn//'_t) :: params')
         call w('            type('//rn//'_t) :: results')
         call w('            params%p = ctx%params')
         call w('            results%p = capnp_new_struct(ctx%rmsg, '//upcase(rn)// &
                '_DWORDS, '//upcase(rn)//'_PWORDS, err)')
         call w('            if (err /= CAPNP_OK) return')
         call w('            call self%'//mn//'(params, results, err)')
         call w('            if (err == CAPNP_OK) call payload_content_set(ctx%results, results%p, err)')
         call w('         end block')
      end do
      call w('      case default')
      call w('         err = CAPNP_ERR_ARG')
      call w('      end select')
      call w('   end subroutine '//tn//'_dispatch')
      call w('')
   end subroutine emit_interface_procs

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
