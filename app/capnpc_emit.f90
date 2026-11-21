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
   !> Distinct handle types for constrained AnyPointer variants.
   logical :: g_need_any_struct = .false.
   logical :: g_need_any_list = .false.
   logical :: g_need_capability = .false.
   !> stream.capnp StreamResult. `-> stream` is this result type.
   !> schema.h InterfaceSchema::Method::isStreaming.
   integer(int64), parameter :: STREAM_RESULT_ID = -7395022517275938450_int64

   !> Brand instantiations: one per distinct (generic node, bindings)
   !> use, e.g. Box(Text) -> box_text. Collected in pass 1 from branded
   !> struct fields; each gets a handle type plus accessors with the
   !> generic parameters substituted.
   type :: inst_t
      integer :: gidx = 0
      character(len=:), allocatable :: name
      !> 0-based binding list. Sized to the brand, so arity is not capped.
      type(capnp_ptr_t), allocatable :: bindings(:)
      integer :: nbind = 0
      !> Nonzero when the bindings were inherited from this scope id
      !> (Method.paramBrand is often Scope.inherit of the interface).
      integer(int64) :: inherit_scope = 0_int64
   end type inst_t
   type(inst_t) :: g_insts(64)
   integer :: g_ninsts = 0
   !> Nonzero while emitting an instantiation's accessors.
   integer :: g_cur_inst = 0

   ! Fortran 2003 max identifier length (gfortran / F2003).
   integer, parameter :: F_IDENT_MAX = 63
   ! Longest accessor suffix we append: `_set_elem` (9 chars).
   integer, parameter :: F_IDENT_SUFFIX = 9

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

   !> Polynomial rolling hash -> lowercase 16-hex (deterministic, pure).
   !> The hash is h*131 + c modulo 2**64, read as two's complement. Signed
   !> overflow is undefined in Fortran, so the product is carried in two
   !> 32-bit halves and only reassembled with bit operations.
   pure function ident_hash_hex(s) result(hex)
      character(len=*), intent(in) :: s
      character(len=16) :: hex
      integer(int64), parameter :: mask32 = 4294967295_int64
      integer(int64) :: h, lo, hi, t
      integer :: i, d
      lo = 0_int64
      hi = 0_int64
      do i = 1, len(s)
         t = lo * 131_int64 + int(iachar(s(i:i)), int64)
         lo = iand(t, mask32)
         hi = iand(hi * 131_int64 + ishft(t, -32), mask32)
      end do
      h = ior(ishft(hi, 32), lo)
      ! Manual hex (avoid internal write: not pure in F2003).
      hex = '0000000000000000'
      do i = 16, 1, -1
         d = int(iand(h, 15_int64))
         if (d < 10) then
            hex(i:i) = achar(iachar('0') + d)
         else
            hex(i:i) = achar(iachar('a') + d - 10)
         end if
         h = h / 16_int64
      end do
   end function ident_hash_hex

   !> Clamp an identifier to max_len (default F_IDENT_MAX). Over-long names
   !> become head_hash_tail so every occurrence of the logical name maps to
   !> the same token; hash covers the *original* full string.
   pure function ident_fit(s, max_len) result(o)
      character(len=*), intent(in) :: s
      integer, intent(in), optional :: max_len
      character(len=:), allocatable :: o
      character(len=16) :: hex
      integer :: lim, budget, left, right
      lim = F_IDENT_MAX
      if (present(max_len)) lim = max_len
      if (lim < 20) lim = 20
      if (len(s) <= lim) then
         o = s
         return
      end if
      hex = ident_hash_hex(s)
      ! head + '_' + 16 hex + '_' + tail
      budget = lim - 18
      left = max(1, budget / 2)
      right = max(1, budget - left)
      o = s(1:left)
      if (o(len(o):len(o)) == '_') o = o(1:max(1, len(o) - 1))
      o = o//'_'//hex//'_'//s(len(s) - right + 1:len(s))
      if (len(o) > lim) o = o(1:lim)
   end function ident_fit

   !> Field-path base (pfx_field): leave room for `_set_elem`.
   pure function accessor_base(pfx, field) result(o)
      character(len=*), intent(in) :: pfx, field
      character(len=:), allocatable :: o
      o = ident_fit(pfx//'_'//snake(field), F_IDENT_MAX - F_IDENT_SUFFIX)
   end function accessor_base

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
      ! Final clamp in case the fixed form still grows via suffixes elsewhere.
      o = ident_fit(o, F_IDENT_MAX - F_IDENT_SUFFIX)
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

   !> Fortran character expression for an arbitrary string.
   !> Printable characters stay inside a quoted literal (quotes doubled).
   !> Controls and DEL become achar() calls, matching the decoded value of
   !> a Cap'n Proto string escape (kj/parse/char.h escapeSequence).
   pure function ftext(s) result(o)
      character(len=*), intent(in) :: s
      character(len=:), allocatable :: o
      integer :: i, a
      logical :: in_lit
      if (len(s) == 0) then
         o = "''"
         return
      end if
      o = ''
      in_lit = .false.
      do i = 1, len(s)
         a = iachar(s(i:i))
         if (a < 32 .or. a >= 127) then
            if (in_lit) then
               o = o//"'"
               in_lit = .false.
            end if
            if (len(o) > 0) o = o//'//'
            o = o//'achar('//itoa(int(a, int64))//')'
         else
            if (.not. in_lit) then
               if (len(o) > 0) o = o//'//'
               o = o//"'"
               in_lit = .true.
            end if
            o = o//s(i:i)
            if (s(i:i) == "'") o = o//"'"
         end if
      end do
      if (in_lit) o = o//"'"
   end function ftext

   !> Kind-suffixed integer token. The most negative value of a kind has
   !> no literal: Fortran parses the sign apart from the digits, so
   !> -128_int8 is rejected. capnpc-c++.c++ literalValue prints the number
   !> (the C++ literal is in range); spell the Fortran form with huge().
   pure function signed_lit(v, suffix) result(o)
      integer(int64), intent(in) :: v
      character(len=*), intent(in) :: suffix
      character(len=:), allocatable :: o
      logical :: minv
      minv = .false.
      select case (suffix)
      case ('_int8')
         minv = v == int(-huge(0_int8) - 1_int8, int64)
      case ('_int16')
         minv = v == int(-huge(0_int16) - 1_int16, int64)
      case ('_int32')
         minv = v == int(-huge(0_int32) - 1_int32, int64)
      case ('_int64')
         minv = v == -huge(0_int64) - 1_int64
      end select
      if (minv) then
         o = '-huge(0'//suffix//')-1'//suffix
      else
         o = itoa(v)//suffix
      end if
   end function signed_lit

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
      character(len=:), allocatable :: modname, base, prefix, dn, dir, outfile
      integer :: i, ios, fidx
      logical :: mine

      call load_nodes(root, err)
      if (err /= CAPNP_OK) return

      ! capnpc-c++.c++ writeFile keeps the requested file's directory, so
      ! two schemas that share a basename do not overwrite each other.
      ! The module name stays the basename, which is what callers import.
      base = filename
      dir = ''
      i = index(base, '/', back=.true.)
      if (i > 0) then
         dir = base(1:i)
         base = base(i + 1:)
      end if
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
      g_need_any_struct = .false.
      g_need_any_list = .false.
      g_need_capability = .false.
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

      outfile = dir//modname//'.f90'
      if (len(dir) > 0) then
         call execute_command_line('mkdir -p "'//dir(1:len(dir) - 1)//'"', &
                                   exitstat=ios)
         if (ios /= 0) then
            err = CAPNP_ERR_IO
            return
         end if
      end if
      open (newunit=g_out, file=outfile, status='replace', action='write', iostat=ios)
      if (ios /= 0) then
         err = CAPNP_ERR_IO
         return
      end if

      call w('!> Generated by capnpc-fortran from '//trim(filename)//'. Do not edit.')
      call w('module '//modname)
      call w('   use capnp')
      if (g_has_iface) call w('   use capnp_rpc')
      ! rpc_capnp is the schema module. capnp_rpc already uses the
      ! three-party schema, so a capability field must not pull capnp_rpc
      ! back in. Descriptor symbols are enough to read a cap table.
      if (g_has_iface .or. g_need_capability) then
         call w('   use rpc_capnp, only: payload_t, payload_content_set, &')
         call w('      cap_descriptor_t, cap_descriptor_which, &')
         call w('      cap_descriptor_sender_hosted_get, &')
         call w('      cap_descriptor_sender_promise_get, &')
         call w('      cap_descriptor_receiver_hosted_get, &')
         call w('      cap_descriptor_none_tag, cap_descriptor_sender_hosted_tag, &')
         call w('      cap_descriptor_sender_promise_tag, cap_descriptor_receiver_hosted_tag')
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
      if (err == CAPNP_OK) call emit_annotation_surface(modname, fidx, err)
      if (err == CAPNP_OK) call emit_any_decls()

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

      if (err == CAPNP_OK .and. g_has_iface) call emit_cap_resolve()
      if (err == CAPNP_OK .and. g_need_capability) call emit_cap_resolve_id()
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
         ! Trailing _E: Fortran is case-insensitive, so PREFIX_KIND_SET (enum
         ! enumerant `set`) would collide with prefix_kind_set (field accessor).
         call w('   integer, parameter :: '// &
                ident_fit(upcase(tn)//'_'//upcase(snake(nm))//'_E')//' = '//itoa(i))
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
                signed_lit(const_int_value(v), '_int64'))
      case (TYPE_FLOAT32)
         call w('   real(real32), parameter :: '//upcase(cn)//' = transfer('// &
                signed_lit(int(cp_f32_bits(value_f32(v)), int64), '_int32')// &
                ', 1.0_real32)')
      case (TYPE_FLOAT64)
         call w('   real(real64), parameter :: '//upcase(cn)//' = transfer('// &
                signed_lit(cp_f64_bits(value_f64(v)), '_int64')//', 1.0_real64)')
      case (TYPE_TEXT)
         call value_text(v, s, err)
         if (err /= CAPNP_OK) return
         call w('   character(len=*), parameter :: '//upcase(cn)//' = '//ftext(s))
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
            call w('   integer, parameter :: '// &
                   ident_fit(upcase(pfx)//'_'//upcase(snake(fn))//'_TAG')// &
                   ' = '//itoa(int(disc, int64)))
         end if
         if (field_which(f) == FIELD_GROUP) then
            gidx = find_node(field_group_type_id(f))
            if (gidx == 0) then
               err = CAPNP_ERR_ARG
               return
            end if
            call emit_union_tag_decls(g_nodes(gidx)%p, &
                 accessor_base(pfx, fn), err)
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
      an = accessor_base(pfx, fn)
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
               if (inst_scope_hit(g_cur_inst, type_param_scope_id(t))) then
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
         call emit_anyptr_field(an, ht, int(off), t, gset, err)
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

   !> Pointer-field presence. capnpc-c++.c++ emits hasX() as a null check
   !> on the pointer slot.
   subroutine emit_has(an, ht, pidx)
      character(len=*), intent(in) :: an, ht
      integer, intent(in) :: pidx
      call w('   function '//an//'_has(h) result(v)')
      call w('      type('//ht//'_t), intent(in) :: h')
      call w('      logical :: v')
      call w('      type(capnp_ptr_t) :: q')
      call w('      integer :: err')
      call w('      q = capnp_getp(h%p, '//itoa(int(pidx, int64))//', err)')
      call w('      v = err == CAPNP_OK .and. q%kind /= CAPNP_PK_NULL')
      call w('   end function '//an//'_has')
      call w('')
   end subroutine emit_has

   !> AnyPointer fields keep a typed handle when the schema names a kind.
   !> capnpc-c++.c++ maps unconstrained anyPointer to AnyPointer, AnyStruct,
   !> AnyList, or Capability. anyKind stays capnp_ptr_t.
   subroutine emit_anyptr_field(an, ht, pidx, t, gset, err)
      character(len=*), intent(in) :: an, ht, gset
      integer, intent(in) :: pidx
      type(capnp_ptr_t), intent(in) :: t
      integer, intent(out) :: err
      integer :: akind
      err = CAPNP_OK
      akind = ANYPTR_KIND_ANY
      if (type_anyptr_which(t) == ANYPTR_UNCONSTRAINED) akind = type_anyptr_kind(t)
      call emit_has(an, ht, pidx)
      select case (akind)
      case (ANYPTR_KIND_STRUCT)
         g_need_any_struct = .true.
         call emit_wrapped_ptr(an, ht, pidx, 'any_struct_t', gset)
      case (ANYPTR_KIND_LIST)
         g_need_any_list = .true.
         call emit_wrapped_ptr(an, ht, pidx, 'any_list_t', gset)
      case (ANYPTR_KIND_CAPABILITY)
         ! A local client, not rpc_cap_t: the three-party schema is used by
         ! capnp_rpc, so this module must not use capnp_rpc in return.
         g_need_capability = .true.
         call emit_capability_field(an, ht, pidx, gset)
      case default
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
      end select
   end subroutine emit_anyptr_field

   subroutine emit_wrapped_ptr(an, ht, pidx, tn, gset)
      character(len=*), intent(in) :: an, ht, tn, gset
      integer, intent(in) :: pidx
      call w('   function '//an//'_get(h, err) result(o)')
      call w('      type('//ht//'_t), intent(in) :: h')
      call w('      integer, intent(out) :: err')
      call w('      type('//tn//') :: o')
      call w('      o%p = capnp_getp(h%p, '//itoa(int(pidx, int64))//', err)')
      call w('   end function '//an//'_get')
      call w('')
      call w('   subroutine '//an//'_set(h, src, err)')
      call w('      type('//ht//'_t), intent(in) :: h')
      call w('      type('//tn//'), intent(in) :: src')
      call w('      integer, intent(out) :: err')
      if (len(gset) > 0) call w(gset)
      call w('      call capnp_setp(h%p, '//itoa(int(pidx, int64))//', src%p, err)')
      call w('   end subroutine '//an//'_set')
      call w('')
   end subroutine emit_wrapped_ptr

   !> Capability client get/set. The optional cap table resolves the slot
   !> the way layout.c++ injectCap does; without it the slot index is the id.
   subroutine emit_cap_client(an, ht, pidx, tn, gset)
      character(len=*), intent(in) :: an, ht, tn, gset
      integer, intent(in) :: pidx
      call w('   function '//an//'_get(h, err, ctab) result(c)')
      call w('      type('//ht//'_t), intent(in) :: h')
      call w('      integer, intent(out) :: err')
      call w('      type('//tn//'_t) :: c')
      call w('      type(capnp_ptr_t), intent(in), optional :: ctab')
      call w('      type(capnp_ptr_t) :: q')
      call w('      c%cap%kind = RPC_CAP_NONE')
      call w('      c%cap%id = 0_int64')
      call w('      q = capnp_getp(h%p, '//itoa(int(pidx, int64))//', err)')
      call w('      if (err /= CAPNP_OK) return')
      call w('      if (q%kind /= CAPNP_PK_CAP) return')
      call w('      if (present(ctab)) then')
      call w('         call cap_resolve(ctab, q%capidx, c%cap, err)')
      call w('      else')
      call w('         c%cap%kind = RPC_CAP_IMPORT')
      call w('         c%cap%id = q%capidx')
      call w('      end if')
      call w('   end function '//an//'_get')
      call w('')
      call w('   subroutine '//an//'_set(h, c, err)')
      call w('      type('//ht//'_t), intent(in) :: h')
      call w('      type('//tn//'_t), intent(in) :: c')
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
   end subroutine emit_cap_client

   !> AnyPointer(Capability). The getter returns a client (kind, id). The
   !> setter still takes the wire pointer: capnp_rpc passes
   !> rpc_make_cap_ptr into join_result_cap_set.
   subroutine emit_capability_field(an, ht, pidx, gset)
      character(len=*), intent(in) :: an, ht, gset
      integer, intent(in) :: pidx
      call w('   function '//an//'_get(h, err, ctab) result(c)')
      call w('      type('//ht//'_t), intent(in) :: h')
      call w('      integer, intent(out) :: err')
      call w('      type(capability_client_t) :: c')
      call w('      type(capnp_ptr_t), intent(in), optional :: ctab')
      call w('      type(capnp_ptr_t) :: q')
      call w('      c%kind = 0')
      call w('      c%id = 0_int64')
      call w('      q = capnp_getp(h%p, '//itoa(int(pidx, int64))//', err)')
      call w('      if (err /= CAPNP_OK) return')
      call w('      if (q%kind /= CAPNP_PK_CAP) return')
      call w('      if (present(ctab)) then')
      call w('         call cap_resolve_id(ctab, q%capidx, c%kind, c%id, err)')
      call w('      else')
      call w('         c%kind = 1')
      call w('         c%id = q%capidx')
      call w('      end if')
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
   end subroutine emit_capability_field

   !> Interface-typed struct field: get/set the wire capability as a
   !> <iface>_client_t (cap index in/out). Uses the same client type the
   !> method stubs generate for that interface.
   subroutine emit_interface_field(an, ht, pidx, t, gset, err)
      character(len=*), intent(in) :: an, ht, gset
      integer, intent(in) :: pidx
      type(capnp_ptr_t), intent(in) :: t
      integer, intent(out) :: err
      character(len=:), allocatable :: st, iname
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
      call inst_name_for(t, idx, iname, err)
      if (err /= CAPNP_OK) return
      if (len(iname) > 0) st = iname
      g_has_iface = .true.
      call emit_has(an, ht, pidx)
      call emit_cap_client(an, ht, pidx, st//'_client', gset)
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
      if (d /= 0_int64) defarg = ', default='//signed_lit(d, kind_suffix(vt))
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
                        signed_lit(int(cp_f32_bits(value_f32(dv)), int64), '_int32')//')'
         end if
      else
         acc = 'f64'; vt = 'real(real64)'; bytes = 8
         defarg = ''
         if (value_which(dv) == TYPE_FLOAT64) then
            if (cp_f64_bits(value_f64(dv)) /= 0_int64) &
               defarg = ', default=cp_bits_f64('// &
                        signed_lit(cp_f64_bits(value_f64(dv)), '_int64')//')'
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
      call emit_has(an, ht, pidx)
      call w('   subroutine '//an//'_get(h, s, err)')
      call w('      type('//ht//'_t), intent(in) :: h')
      call w('      character(len=:), allocatable, intent(out) :: s')
      call w('      integer, intent(out) :: err')
      if (len(dtext) > 0) then
         call w('      type(capnp_ptr_t) :: q')
         call w('      q = capnp_getp(h%p, '//itoa(int(pidx, int64))//', err)')
         call w('      if (err == CAPNP_OK .and. q%kind == CAPNP_PK_NULL) then')
         call w('         s = '//ftext(dtext))
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
      call emit_has(an, ht, pidx)
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
      call emit_has(an, ht, pidx)
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
      ! capnpc-c++.c++ PointerHelpers::set copies a reader into the slot.
      call w('   subroutine '//an//'_set(h, src, err)')
      call w('      type('//ht//'_t), intent(in) :: h')
      call w('      type('//st//'_t), intent(in) :: src')
      call w('      integer, intent(out) :: err')
      call w('      type(capnp_ptr_t) :: c')
      if (len(gset) > 0) call w(gset)
      call w('      c = capnp_copy(h%p%msg, src%p, err)')
      call w('      if (err == CAPNP_OK) call capnp_setp(h%p, '// &
             itoa(int(pidx, int64))//', c, err)')
      call w('   end subroutine '//an//'_set')
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
      call emit_has(an, ht, pidx)
      call w('   function '//an//'_get(h, err) result(l)')
      call w('      type('//ht//'_t), intent(in) :: h')
      call w('      integer, intent(out) :: err')
      call w('      type(capnp_ptr_t) :: l')
      if (hasdef) then
         ! Builder get copies the default into the message. A reader keeps
         ! the saved default. layout.c++ getWritableListPointer copies the
         ! default word when the slot is null.
         call w('      type(capnp_message_t), save, target :: defmsg')
         call w('      logical, save :: defloaded = .false.')
         call w('      type(capnp_ptr_t) :: d')
         call w('      l = capnp_getp(h%p, '//itoa(int(pidx, int64))//', err)')
         call w('      if (err /= CAPNP_OK .or. l%kind /= CAPNP_PK_NULL) return')
         call w('      if (.not. defloaded) then')
         call w('         call capnp_deserialize_bytes('//upcase(an)//'_DEFAULT, defmsg, err)')
         call w('         if (err /= CAPNP_OK) return')
         call w('         defloaded = .true.')
         call w('      end if')
         call w('      d = capnp_root(defmsg, err)')
         call w('      if (err /= CAPNP_OK) return')
         call w('      if (associated(h%p%msg)) then')
         call w('         if (h%p%msg%is_builder) then')
         call w('            l = capnp_copy(h%p%msg, d, err)')
         call w('            if (err == CAPNP_OK) call capnp_setp(h%p, '// &
                itoa(int(pidx, int64))//', l, err)')
         call w('            return')
         call w('         end if')
         call w('      end if')
         call w('      l = d')
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
         call w('      l = '//an//'_get(h, err)')
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
         call w('      l = '//an//'_get(h, err)')
         call w('      if (err == CAPNP_OK) call capnp_list_get_text(l, i, s, err)')
         call w('   end subroutine '//an//'_get_elem')
         call w('')
         call w('   subroutine '//an//'_set_elem(h, i, s, err)')
         call w('      type('//ht//'_t), intent(in) :: h')
         call w('      integer, intent(in) :: i')
         call w('      character(len=*), intent(in) :: s')
         call w('      integer, intent(out) :: err')
         call w('      type(capnp_ptr_t) :: l')
         call w('      l = '//an//'_get(h, err)')
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
         call w('      l = '//an//'_get(h, err)')
         call w('      if (err == CAPNP_OK) call capnp_get_data(l, i, b, err)')
         call w('   end subroutine '//an//'_get_elem')
         call w('')
         call w('   subroutine '//an//'_set_elem(h, i, b, err)')
         call w('      type('//ht//'_t), intent(in) :: h')
         call w('      integer, intent(in) :: i')
         call w('      integer(int8), intent(in) :: b(:)')
         call w('      integer, intent(out) :: err')
         call w('      type(capnp_ptr_t) :: l')
         call w('      l = '//an//'_get(h, err)')
         call w('      if (err == CAPNP_OK) call capnp_set_data(l, i, b, err)')
         call w('   end subroutine '//an//'_set_elem')
         call w('')
      case (TYPE_BOOL, TYPE_INT8, TYPE_INT16, TYPE_INT32, TYPE_INT64, &
            TYPE_UINT8, TYPE_UINT16, TYPE_UINT32, TYPE_UINT64, &
            TYPE_FLOAT32, TYPE_FLOAT64, TYPE_ENUM)
         call emit_scalar_list_elem(an, ht, type_which(et))
      case (TYPE_LIST, TYPE_ANY_POINTER)
         ! Nested lists and AnyPointer elements are pointer slots.
         ! list.h List<List<T>>::operator[] / capnp_list_getp.
         call w('   function '//an//'_get_elem(h, i, err) result(o)')
         call w('      type('//ht//'_t), intent(in) :: h')
         call w('      integer, intent(in) :: i')
         call w('      integer, intent(out) :: err')
         call w('      type(capnp_ptr_t) :: o, l')
         call w('      l = '//an//'_get(h, err)')
         call w('      if (err == CAPNP_OK) o = capnp_list_getp(l, i, err)')
         call w('   end function '//an//'_get_elem')
         call w('')
         call w('   subroutine '//an//'_set_elem(h, i, q, err)')
         call w('      type('//ht//'_t), intent(in) :: h')
         call w('      integer, intent(in) :: i')
         call w('      type(capnp_ptr_t), intent(in) :: q')
         call w('      integer, intent(out) :: err')
         call w('      type(capnp_ptr_t) :: l')
         call w('      l = '//an//'_get(h, err)')
         call w('      if (err == CAPNP_OK) call capnp_setp(l, i, q, err)')
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

   !> Element get/set for a primitive, bool, or enum list.
   !> list.h operator[] reads the element; unsigned values are widened
   !> into the same container the scalar field accessors use.
   subroutine emit_scalar_list_elem(an, ht, tw)
      character(len=*), intent(in) :: an, ht
      integer, intent(in) :: tw
      character(len=:), allocatable :: vt, raw, setcall, widen, narrow
      logical :: unsigned
      unsigned = .false.
      widen = ''
      narrow = ''
      select case (tw)
      case (TYPE_BOOL)
         vt = 'logical'
         raw = 'capnp_list_get_bool(l, int(i, int64), err)'
         setcall = 'call capnp_list_set_bool(l, int(i, int64), v, err)'
      case (TYPE_INT8)
         vt = 'integer(int8)'
         raw = 'capnp_list_get_i8(l, int(i, int64), err)'
         setcall = 'call capnp_list_set_i8(l, int(i, int64), v, err)'
      case (TYPE_INT16)
         vt = 'integer(int16)'
         raw = 'capnp_list_get_i16(l, int(i, int64), err)'
         setcall = 'call capnp_list_set_i16(l, int(i, int64), v, err)'
      case (TYPE_INT32)
         vt = 'integer(int32)'
         raw = 'capnp_list_get_i32(l, int(i, int64), err)'
         setcall = 'call capnp_list_set_i32(l, int(i, int64), v, err)'
      case (TYPE_INT64, TYPE_UINT64)
         vt = 'integer(int64)'
         raw = 'capnp_list_get_i64(l, int(i, int64), err)'
         setcall = 'call capnp_list_set_i64(l, int(i, int64), v, err)'
      case (TYPE_UINT8)
         vt = 'integer(int16)'
         raw = 'int(capnp_list_get_i8(l, int(i, int64), err), int16)'
         setcall = 'call capnp_list_set_i8(l, int(i, int64), int(narrow, int8), err)'
         unsigned = .true.
         widen = '256_int16'
         narrow = 'int(v, int32)'
      case (TYPE_UINT16, TYPE_ENUM)
         if (tw == TYPE_ENUM) then
            vt = 'integer'
         else
            vt = 'integer(int32)'
         end if
         raw = 'int(capnp_list_get_i16(l, int(i, int64), err))'
         setcall = 'call capnp_list_set_i16(l, int(i, int64), int(narrow, int16), err)'
         unsigned = .true.
         widen = '65536'
         narrow = 'int(v, int32)'
      case (TYPE_UINT32)
         vt = 'integer(int64)'
         raw = 'int(capnp_list_get_i32(l, int(i, int64), err), int64)'
         setcall = 'call capnp_list_set_i32(l, int(i, int64), int(narrow, int32), err)'
         unsigned = .true.
         widen = '4294967296_int64'
         narrow = 'v'
      case (TYPE_FLOAT32)
         vt = 'real(real32)'
         raw = 'capnp_list_get_f32(l, int(i, int64), err)'
         setcall = 'call capnp_list_set_f32(l, int(i, int64), v, err)'
      case (TYPE_FLOAT64)
         vt = 'real(real64)'
         raw = 'capnp_list_get_f64(l, int(i, int64), err)'
         setcall = 'call capnp_list_set_f64(l, int(i, int64), v, err)'
      case default
         return
      end select
      call w('   function '//an//'_get_elem(h, i, err) result(v)')
      call w('      type('//ht//'_t), intent(in) :: h')
      call w('      integer, intent(in) :: i')
      call w('      integer, intent(out) :: err')
      call w('      '//vt//' :: v')
      call w('      type(capnp_ptr_t) :: l')
      if (tw == TYPE_BOOL) then
         call w('      v = .false.')
      else if (tw == TYPE_FLOAT32 .or. tw == TYPE_FLOAT64) then
         call w('      v = 0.0')
      else
         call w('      v = 0')
      end if
      call w('      l = '//an//'_get(h, err)')
      call w('      if (err /= CAPNP_OK) return')
      call w('      v = '//raw)
      if (unsigned) call w('      if (v < 0) v = v + '//widen)
      call w('   end function '//an//'_get_elem')
      call w('')
      call w('   subroutine '//an//'_set_elem(h, i, v, err)')
      call w('      type('//ht//'_t), intent(in) :: h')
      call w('      integer, intent(in) :: i')
      call w('      '//vt//', intent(in) :: v')
      call w('      integer, intent(out) :: err')
      call w('      type(capnp_ptr_t) :: l')
      if (unsigned) then
         call w('      integer(int64) :: narrow')
      end if
      call w('      l = '//an//'_get(h, err)')
      call w('      if (err /= CAPNP_OK) return')
      if (unsigned) then
         call w('      narrow = int('//narrow//', int64)')
         call w('      if (narrow >= '//widen//'/2) narrow = narrow - '//widen)
         ! setcall references `narrow` which is int64; the int() cast is in setcall
         ! For uint32 widen is 4294967296_int64, half is not integer-divisible in the
         ! source the way I wrote it. Emit a numeric half instead.
      end if
      call w('      '//setcall)
      call w('   end subroutine '//an//'_set_elem')
      call w('')
   end subroutine emit_scalar_list_elem

   ! --- brand instantiations ---------------------------------------------------

   !> Instantiation handle name for a branded struct Type, registering
   !> the instantiation on first sight. '' when the use is unbranded.
   subroutine inst_name_for(t, gidx, o, err)
      type(capnp_ptr_t), intent(in) :: t
      integer, intent(in) :: gidx
      character(len=:), allocatable, intent(out) :: o
      integer, intent(out) :: err
      type(capnp_ptr_t) :: b
      err = CAPNP_OK
      o = ''
      b = type_brand(t, err)
      if (err /= CAPNP_OK) return
      call inst_from_brand(gidx, b, o, err)
   end subroutine inst_name_for

   !> Register one (generic node, brand) instantiation. Bindings are kept
   !> in full: capnpc-c++.c++ emits one template parameter per schema
   !> parameter, with no fixed arity.
   subroutine inst_from_brand(gidx, b, o, err)
      integer, intent(in) :: gidx
      type(capnp_ptr_t), intent(in) :: b
      character(len=:), allocatable, intent(out) :: o
      integer, intent(out) :: err
      type(capnp_ptr_t) :: scopes, sc, binds, bd
      type(capnp_ptr_t), allocatable :: btypes(:)
      character(len=:), allocatable :: gname, part
      integer(int64) :: i, j, nb
      integer :: n, k, bidx
      err = CAPNP_OK
      o = ''
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
         nb = capnp_list_len(binds)
         if (allocated(btypes)) deallocate (btypes)
         if (nb > 0_int64) then
            allocate (btypes(0:int(nb) - 1))
         else
            allocate (btypes(0:0))
         end if
         do j = 0_int64, nb - 1_int64
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
                        if (inst_scope_hit(g_cur_inst, type_param_scope_id(btypes(n))) .and. &
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
      g_insts(g_ninsts)%inherit_scope = 0_int64
      if (allocated(g_insts(g_ninsts)%bindings)) &
         deallocate (g_insts(g_ninsts)%bindings)
      allocate (g_insts(g_ninsts)%bindings(0:n - 1))
      do k = 0, n - 1
         g_insts(g_ninsts)%bindings(k) = btypes(k)
      end do
   end subroutine inst_from_brand

   !> True when scope_id is this instantiation's generic node or the
   !> scope whose brand it inherited.
   function inst_scope_hit(k, scope_id) result(yes)
      integer, intent(in) :: k
      integer(int64), intent(in) :: scope_id
      logical :: yes
      yes = scope_id == g_nodes(g_insts(k)%gidx)%id
      if (.not. yes) yes = g_insts(k)%inherit_scope /= 0_int64 .and. &
                          scope_id == g_insts(k)%inherit_scope
   end function inst_scope_hit

   !> Method param/result brands often say "inherit" the interface brand
   !> instead of repeating the bindings. Copy the current instantiation's
   !> bindings onto the struct so its fields resolve the same way.
   subroutine inst_inherit_brand(gidx, b, o, err)
      integer, intent(in) :: gidx
      type(capnp_ptr_t), intent(in) :: b
      character(len=:), allocatable, intent(out) :: o
      integer, intent(out) :: err
      type(capnp_ptr_t) :: scopes, sc
      type(capnp_ptr_t), allocatable :: btypes(:)
      character(len=:), allocatable :: gname, part
      integer(int64) :: i, sid
      integer :: n, k, src, bidx
      err = CAPNP_OK
      o = ''
      if (g_cur_inst <= 0) return
      if (b%kind == CAPNP_PK_NULL) return
      src = g_cur_inst
      n = g_insts(src)%nbind
      if (n <= 0) return
      scopes = brand_scopes(b, err)
      if (err /= CAPNP_OK) return
      sid = 0_int64
      do i = 0_int64, capnp_list_len(scopes) - 1_int64
         sc = capnp_list_get_struct(scopes, int(i), err)
         if (err /= CAPNP_OK) return
         if (scope_which(sc) /= SCOPE_INHERIT) cycle
         if (scope_scope_id(sc) == g_nodes(g_insts(src)%gidx)%id .or. &
             (g_insts(src)%inherit_scope /= 0_int64 .and. &
              scope_scope_id(sc) == g_insts(src)%inherit_scope)) then
            sid = scope_scope_id(sc)
            exit
         end if
      end do
      if (sid == 0_int64) return
      allocate (btypes(0:n - 1))
      do k = 0, n - 1
         btypes(k) = g_insts(src)%bindings(k)
      end do
      call node_fname(g_nodes(gidx)%p, gname, err)
      if (err /= CAPNP_OK) return
      o = gname
      do k = 0, n - 1
         call inst_suffix(btypes(k), part, err)
         if (err /= CAPNP_OK) return
         o = o//'_'//part
      end do
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
      g_insts(g_ninsts)%inherit_scope = sid
      if (allocated(g_insts(g_ninsts)%bindings)) &
         deallocate (g_insts(g_ninsts)%bindings)
      allocate (g_insts(g_ninsts)%bindings(0:n - 1))
      do k = 0, n - 1
         g_insts(g_ninsts)%bindings(k) = btypes(k)
      end do
   end subroutine inst_inherit_brand

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
   !> Struct instantiations also get the union tag constants the
   !> unbranded struct declaration emits. Interface instantiations get
   !> a client and a server, declared after the struct handles they import.
   subroutine emit_inst_decls(err)
      integer, intent(out) :: err
      integer :: k, prev
      type(capnp_ptr_t) :: np
      err = CAPNP_OK
      do k = 1, g_ninsts
         np = g_nodes(g_insts(k)%gidx)%p
         if (node_which(np) /= NODE_STRUCT) cycle
         call w('   integer, parameter :: '//upcase(g_insts(k)%name)//'_DWORDS = '// &
                itoa(int(node_struct_data_words(np), int64)))
         call w('   integer, parameter :: '//upcase(g_insts(k)%name)//'_PWORDS = '// &
                itoa(int(node_struct_pointer_count(np), int64)))
         call w('   type :: '//g_insts(k)%name//'_t')
         call w('      type(capnp_ptr_t) :: p')
         call w('   end type '//g_insts(k)%name//'_t')
         call emit_union_tag_decls(np, g_insts(k)%name, err)
         if (err /= CAPNP_OK) return
         call w('')
      end do
      prev = g_cur_inst
      do k = 1, g_ninsts
         np = g_nodes(g_insts(k)%gidx)%p
         if (node_which(np) /= NODE_INTERFACE) cycle
         g_cur_inst = k
         g_has_iface = .true.
         call emit_interface_decl(np, err)
         if (err /= CAPNP_OK) exit
      end do
      g_cur_inst = prev
   end subroutine emit_inst_decls

   !> Accessors for one instantiation: struct fields with parameters
   !> substituted, or the branded interface's client and server.
   subroutine emit_inst_procs(k, err)
      integer, intent(in) :: k
      integer, intent(out) :: err
      character(len=:), allocatable :: tn
      type(capnp_ptr_t) :: np
      integer :: prev
      tn = g_insts(k)%name
      np = g_nodes(g_insts(k)%gidx)%p
      prev = g_cur_inst
      g_cur_inst = k
      if (node_which(np) == NODE_INTERFACE) then
         call emit_interface_procs(np, err)
         g_cur_inst = prev
         return
      end if
      if (node_which(np) /= NODE_STRUCT) then
         g_cur_inst = prev
         err = CAPNP_OK
         return
      end if
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
      call emit_fields_of(np, tn, tn, err)
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
      if (err /= CAPNP_OK) return
      ! A branded interface instantiation substitutes paramBrand and
      ! resultBrand, so GenIface(Text) methods see Text rather than the
      ! unbound parameter. capnpc-c++.c++ templates do the same substitution.
      if (g_cur_inst > 0) then
         call brand_struct(mp, pidx, .true., pn, err)
         if (err /= CAPNP_OK) return
         call brand_struct(mp, ridx, .false., rn, err)
      end if
   end subroutine method_types

   subroutine brand_struct(mp, idx, is_param, name, err)
      type(capnp_ptr_t), intent(in) :: mp
      integer, intent(in) :: idx
      logical, intent(in) :: is_param
      character(len=:), allocatable, intent(inout) :: name
      integer, intent(out) :: err
      type(capnp_ptr_t) :: b
      character(len=:), allocatable :: iname
      if (is_param) then
         b = method_param_brand(mp, err)
      else
         b = method_result_brand(mp, err)
      end if
      if (err /= CAPNP_OK) return
      call inst_from_brand(idx, b, iname, err)
      if (err /= CAPNP_OK) return
      if (len(iname) == 0) then
         call inst_inherit_brand(idx, b, iname, err)
         if (err /= CAPNP_OK) return
      end if
      if (len(iname) > 0) name = iname
   end subroutine brand_struct

   !> Fortran name of this interface, or the active instantiation's name.
   subroutine iface_name(np, tn, err)
      type(capnp_ptr_t), intent(in) :: np
      character(len=:), allocatable, intent(out) :: tn
      integer, intent(out) :: err
      call node_fname(np, tn, err)
      if (err /= CAPNP_OK) return
      if (g_cur_inst <= 0) return
      if (g_nodes(g_insts(g_cur_inst)%gidx)%id == node_id(np)) &
         tn = g_insts(g_cur_inst)%name
   end subroutine iface_name

   !> First superclass type name, or '' when the interface stands alone.
   !> capnpc-c++.c++ Server extends each super::Server.
   subroutine iface_parent(np, pname, nsuper, err)
      type(capnp_ptr_t), intent(in) :: np
      character(len=:), allocatable, intent(out) :: pname
      integer, intent(out) :: nsuper
      integer, intent(out) :: err
      type(capnp_ptr_t) :: sl, sc
      integer :: sidx
      pname = ''
      nsuper = 0
      sl = node_interface_superclasses(np, err)
      if (err /= CAPNP_OK) return
      nsuper = int(capnp_list_len(sl))
      if (nsuper < 1) return
      sc = capnp_list_get_struct(sl, 0, err)
      if (err /= CAPNP_OK) return
      sidx = find_node(superclass_id(sc))
      if (sidx == 0) then
         nsuper = 0
         return
      end if
      call note_import(sidx, err)
      if (err /= CAPNP_OK) return
      call node_fname(g_nodes(sidx)%p, pname, err)
   end subroutine iface_parent

   !> Interface declarations: the interface id, a client handle type, an
   !> abstract server base extending rpc_server_t whose generated dispatch
   !> routes method ordinals to deferred typed procedures.
   subroutine emit_interface_decl(np, err)
      type(capnp_ptr_t), intent(in) :: np
      integer, intent(out) :: err
      type(capnp_ptr_t) :: ml, mp
      character(len=:), allocatable :: tn, mn, pn, rn, parent
      integer(int64) :: i
      integer :: nsuper
      call iface_name(np, tn, err)
      if (err /= CAPNP_OK) return
      call iface_parent(np, parent, nsuper, err)
      if (err /= CAPNP_OK) return
      ml = node_interface_methods(np, err)
      if (err /= CAPNP_OK) return
      call w('   integer(int64), parameter :: '//upcase(tn)//'_INTERFACE_ID = '// &
             itoa(node_id(np))//'_int64')
      call w('   type :: '//tn//'_client_t')
      call w('      type(rpc_cap_t) :: cap')
      call w('   end type '//tn//'_client_t')
      if (len(parent) > 0) then
         call w('   type, extends('//parent//'_server_t), abstract :: '//tn//'_server_t')
      else
         call w('   type, extends(rpc_server_t), abstract :: '//tn//'_server_t')
      end if
      call w('   contains')
      do i = 0_int64, capnp_list_len(ml) - 1_int64
         mp = capnp_list_get_struct(ml, int(i), err)
         if (err /= CAPNP_OK) return
         call method_name(mp, mn, err)
         if (err /= CAPNP_OK) return
         mn = snake(mn)
         call w('      procedure('//tn//'_'//mn//'_ifc), deferred :: '//mn)
      end do
      call emit_further_supers(np, tn, 'd', err)
      if (err /= CAPNP_OK) return
      call w('      procedure :: dispatch => '//tn//'_dispatch')
      call w('   end type '//tn//'_server_t')
      if (capnp_list_len(ml) > 0_int64 .or. nsuper > 1) then
         call w('   abstract interface')
         do i = 0_int64, capnp_list_len(ml) - 1_int64
            mp = capnp_list_get_struct(ml, int(i), err)
            if (err /= CAPNP_OK) return
            call method_name(mp, mn, err)
            if (err /= CAPNP_OK) return
            mn = snake(mn)
            call method_types(mp, pn, rn, err)
            if (err /= CAPNP_OK) return
            call emit_method_ifc(tn, mn, pn, rn)
         end do
         call emit_further_supers(np, tn, 'a', err)
         if (err /= CAPNP_OK) return
         call w('   end interface')
      end if
      call w('')
   end subroutine emit_interface_decl

   !> Client call helpers and the server dispatch router.
   subroutine emit_interface_procs(np, err)
      type(capnp_ptr_t), intent(in) :: np
      integer, intent(out) :: err
      type(capnp_ptr_t) :: ml, mp
      character(len=:), allocatable :: tn, mn, pn, rn, parent
      integer(int64) :: i
      integer :: nsuper
      call iface_name(np, tn, err)
      if (err /= CAPNP_OK) return
      call iface_parent(np, parent, nsuper, err)
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
         if (method_result_struct_type(mp) == STREAM_RESULT_ID) then
            ! schema.h isStreaming: the result type is StreamResult.
            ! capnpc-c++.c++ emits StreamingRequest / newStreamingCall.
            call w('   subroutine '//tn//'_'//mn//'_stream(conn, stream, m, qid, err)')
            call w('      type(rpc_conn_t), intent(inout), target :: conn')
            call w('      type(rpc_stream_t), intent(inout) :: stream')
            call w('      type(capnp_message_t), intent(inout) :: m')
            call w('      integer(int64), intent(in) :: qid')
            call w('      integer, intent(out) :: err')
            call w('      call rpc_stream_send(conn, stream, m, qid, err)')
            call w('   end subroutine '//tn//'_'//mn//'_stream')
            call w('')
         end if
      end do
      call w('   subroutine '//tn//'_dispatch(self, ctx, err)')
      call w('      class('//tn//'_server_t), intent(inout) :: self')
      call w('      type(rpc_call_ctx_t), intent(inout) :: ctx')
      call w('      integer, intent(out) :: err')
      call w('      err = CAPNP_ERR_ARG')
      if (nsuper > 1) then
         call emit_further_supers(np, tn, 'c', err)
         if (err /= CAPNP_OK) return
      end if
      if (len(parent) > 0) then
         ! capnpc-c++.c++ dispatchCallInternal switches on the superclass
         ! interface id and forwards. The parent dispatch accepts this
         ! extended server.
         call w('      if (ctx%interface_id /= '//upcase(tn)//'_INTERFACE_ID) then')
         call w('         call '//parent//'_dispatch(self, ctx, err)')
         call w('         return')
         call w('      end if')
      else
         call w('      if (ctx%interface_id /= '//upcase(tn)//'_INTERFACE_ID) return')
      end if
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
         call w('            call self%'//mn//'(ctx, params, results, err)')
         call w('            if (err == CAPNP_OK) call payload_content_set(ctx%results, results%p, err)')
         call w('         end block')
      end do
      call w('      case default')
      call w('         err = CAPNP_ERR_ARG')
      call w('      end select')
      call w('   end subroutine '//tn//'_dispatch')
      call w('')
   end subroutine emit_interface_procs

   subroutine emit_method_ifc(tn, mn, pn, rn)
      character(len=*), intent(in) :: tn, mn, pn, rn
      call w('      subroutine '//tn//'_'//mn//'_ifc(self, ctx, params, results, err)')
      if (pn == rn) then
         call w('         import :: '//tn//'_server_t, rpc_call_ctx_t, '//pn//'_t')
      else
         call w('         import :: '//tn//'_server_t, rpc_call_ctx_t, '//pn//'_t, '//rn//'_t')
      end if
      call w('         class('//tn//'_server_t), intent(inout) :: self')
      call w('         type(rpc_call_ctx_t), intent(inout) :: ctx')
      call w('         type('//pn//'_t), intent(in) :: params')
      call w('         type('//rn//'_t), intent(in) :: results')
      call w('         integer, intent(out) :: err')
      call w('      end subroutine '//tn//'_'//mn//'_ifc')
   end subroutine emit_method_ifc

   !> Methods of superclasses after the first. The first is inherited by
   !> extends; further ones are copied because Fortran has one parent.
   !> mode d = deferred binding, a = abstract interface, c = dispatch.
   subroutine emit_further_supers(np, tn, mode, err)
      type(capnp_ptr_t), intent(in) :: np
      character(len=*), intent(in) :: tn
      character(len=1), intent(in) :: mode
      integer, intent(out) :: err
      type(capnp_ptr_t) :: sl, sc, ml, mp
      character(len=:), allocatable :: sn, mn, pn, rn
      character(len=64) :: seen(32)
      integer :: sidx, nseen, k
      integer(int64) :: s, i
      logical :: skip
      err = CAPNP_OK
      nseen = 0
      sl = node_interface_superclasses(np, err)
      if (err /= CAPNP_OK) return
      if (capnp_list_len(sl) < 2_int64) return
      do s = 1_int64, capnp_list_len(sl) - 1_int64
         sc = capnp_list_get_struct(sl, int(s), err)
         if (err /= CAPNP_OK) return
         sidx = find_node(superclass_id(sc))
         if (sidx == 0) cycle
         call note_import(sidx, err)
         if (err /= CAPNP_OK) return
         call node_fname(g_nodes(sidx)%p, sn, err)
         if (err /= CAPNP_OK) return
         ml = node_interface_methods(g_nodes(sidx)%p, err)
         if (err /= CAPNP_OK) return
         if (mode == 'c') then
            call w('      if (ctx%interface_id == '//upcase(sn)//'_INTERFACE_ID) then')
            call w('         select case (ctx%method_id)')
         end if
         do i = 0_int64, capnp_list_len(ml) - 1_int64
            mp = capnp_list_get_struct(ml, int(i), err)
            if (err /= CAPNP_OK) return
            call method_name(mp, mn, err)
            if (err /= CAPNP_OK) return
            mn = snake(mn)
            if (iface_has_method(np, mn)) cycle
            sidx = find_first_super(sl)
            if (sidx > 0) then
               if (iface_has_method(g_nodes(sidx)%p, mn)) cycle
            end if
            skip = .false.
            do k = 1, nseen
               if (seen(k) == mn) skip = .true.
            end do
            if (skip) cycle
            if (nseen < 32) then
               nseen = nseen + 1
               seen(nseen) = mn
            end if
            call method_types(mp, pn, rn, err)
            if (err /= CAPNP_OK) return
            select case (mode)
            case ('d')
               call w('      procedure('//tn//'_'//mn//'_ifc), deferred :: '//mn)
            case ('a')
               call emit_method_ifc(tn, mn, pn, rn)
            case ('c')
               call w('         case ('//itoa(i)//')')
               call w('            block')
               call w('               type('//pn//'_t) :: params')
               call w('               type('//rn//'_t) :: results')
               call w('               params%p = ctx%params')
               call w('               results%p = capnp_new_struct(ctx%rmsg, '// &
                      upcase(rn)//'_DWORDS, '//upcase(rn)//'_PWORDS, err)')
               call w('               if (err /= CAPNP_OK) return')
               call w('               call self%'//mn//'(ctx, params, results, err)')
               call w('               if (err == CAPNP_OK) &')
               call w('                  call payload_content_set(ctx%results, results%p, err)')
               call w('            end block')
            end select
         end do
         if (mode == 'c') then
            call w('         case default')
            call w('            err = CAPNP_ERR_ARG')
            call w('         end select')
            call w('         return')
            call w('      end if')
         end if
      end do
   end subroutine emit_further_supers

   function find_first_super(sl) result(idx)
      type(capnp_ptr_t), intent(in) :: sl
      integer :: idx
      type(capnp_ptr_t) :: sc
      integer :: err
      idx = 0
      if (capnp_list_len(sl) < 1_int64) return
      sc = capnp_list_get_struct(sl, 0, err)
      if (err /= CAPNP_OK) return
      idx = find_node(superclass_id(sc))
   end function find_first_super

   function iface_has_method(np, want) result(yes)
      type(capnp_ptr_t), intent(in) :: np
      character(len=*), intent(in) :: want
      logical :: yes
      type(capnp_ptr_t) :: ml, mp
      character(len=:), allocatable :: mn
      integer :: err
      integer(int64) :: i
      yes = .false.
      if (np%kind == CAPNP_PK_NULL) return
      ml = node_interface_methods(np, err)
      if (err /= CAPNP_OK) return
      do i = 0_int64, capnp_list_len(ml) - 1_int64
         mp = capnp_list_get_struct(ml, int(i), err)
         if (err /= CAPNP_OK) return
         call method_name(mp, mn, err)
         if (err /= CAPNP_OK) return
         if (snake(mn) == want) then
            yes = .true.
            return
         end if
      end do
   end function iface_has_method

   subroutine emit_any_decls()
      if (g_need_any_struct) then
         call w('   type :: any_struct_t')
         call w('      type(capnp_ptr_t) :: p')
         call w('   end type any_struct_t')
         call w('')
      end if
      if (g_need_any_list) then
         call w('   type :: any_list_t')
         call w('      type(capnp_ptr_t) :: p')
         call w('   end type any_list_t')
         call w('')
      end if
      if (g_need_capability) then
         call w('   type :: capability_client_t')
         call w('      integer :: kind')
         call w('      integer(int64) :: id')
         call w('   end type capability_client_t')
         call w('')
      end if
   end subroutine emit_any_decls

   subroutine emit_annotation_surface(modname, fidx, err)
      character(len=*), intent(in) :: modname
      integer, intent(in) :: fidx
      integer, intent(out) :: err
      character(len=:), allocatable :: dn, owner
      integer :: i
      logical :: mine
      err = CAPNP_OK
      do i = 1, size(g_nodes)
         call node_display_name(g_nodes(i)%p, dn, err)
         if (err /= CAPNP_OK) return
         mine = len(dn) > len(g_prefix) .and. index(dn, g_prefix//':') == 1
         if (.not. mine) cycle
         call node_fname(g_nodes(i)%p, owner, err)
         if (err /= CAPNP_OK) return
         if (node_which(g_nodes(i)%p) == NODE_ANNOTATION) then
            call w('   integer(int64), parameter :: '//upcase(owner)//'_ID = '// &
                   signed_lit(node_id(g_nodes(i)%p), '_int64'))
         end if
         call emit_applied(g_nodes(i)%p, owner, err)
         if (err /= CAPNP_OK) return
      end do
      call emit_applied(g_nodes(fidx)%p, modname, err)
   end subroutine emit_annotation_surface

   subroutine emit_applied(np, owner, err)
      type(capnp_ptr_t), intent(in) :: np
      character(len=*), intent(in) :: owner
      integer, intent(out) :: err
      type(capnp_ptr_t) :: al, fl, f, ml, mp
      character(len=:), allocatable :: fn
      integer(int64) :: i
      al = node_annotations(np, err)
      if (err /= CAPNP_OK) return
      call emit_ann_list(owner, al, err)
      if (err /= CAPNP_OK) return
      select case (node_which(np))
      case (NODE_STRUCT)
         fl = node_struct_fields(np, err)
         if (err /= CAPNP_OK) return
         do i = 0_int64, capnp_list_len(fl) - 1_int64
            f = capnp_list_get_struct(fl, int(i), err)
            if (err /= CAPNP_OK) return
            call field_name(f, fn, err)
            if (err /= CAPNP_OK) return
            al = field_annotations(f, err)
            if (err /= CAPNP_OK) return
            call emit_ann_list(owner//'_'//snake(fn), al, err)
            if (err /= CAPNP_OK) return
         end do
      case (NODE_INTERFACE)
         ml = node_interface_methods(np, err)
         if (err /= CAPNP_OK) return
         do i = 0_int64, capnp_list_len(ml) - 1_int64
            mp = capnp_list_get_struct(ml, int(i), err)
            if (err /= CAPNP_OK) return
            call method_name(mp, fn, err)
            if (err /= CAPNP_OK) return
            al = method_annotations(mp, err)
            if (err /= CAPNP_OK) return
            call emit_ann_list(owner//'_'//snake(fn), al, err)
            if (err /= CAPNP_OK) return
         end do
      end select
   end subroutine emit_applied

   subroutine emit_ann_list(owner, al, err)
      character(len=*), intent(in) :: owner
      type(capnp_ptr_t), intent(in) :: al
      integer, intent(out) :: err
      type(capnp_ptr_t) :: a, v
      character(len=:), allocatable :: aname, cname, s
      integer(int8), allocatable :: db(:)
      integer :: aidx
      integer(int64) :: i
      err = CAPNP_OK
      do i = 0_int64, capnp_list_len(al) - 1_int64
         a = capnp_list_get_struct(al, int(i), err)
         if (err /= CAPNP_OK) return
         aidx = find_node(annotation_id(a))
         if (aidx == 0) cycle
         call node_fname(g_nodes(aidx)%p, aname, err)
         if (err /= CAPNP_OK) return
         v = annotation_value(a, err)
         if (err /= CAPNP_OK) return
         cname = ident_fit(upcase(owner)//'_'//upcase(aname))
         select case (value_which(v))
         case (TYPE_BOOL)
            if (value_bool(v)) then
               call w('   logical, parameter :: '//cname//' = .true.')
            else
               call w('   logical, parameter :: '//cname//' = .false.')
            end if
         case (TYPE_INT8, TYPE_INT16, TYPE_INT32, TYPE_INT64, TYPE_ENUM, &
               TYPE_UINT8, TYPE_UINT16, TYPE_UINT32, TYPE_UINT64)
            call w('   integer(int64), parameter :: '//cname//' = '// &
                   signed_lit(const_int_value(v), '_int64'))
         case (TYPE_FLOAT32)
            call w('   real(real32), parameter :: '//cname//' = transfer('// &
                   signed_lit(int(cp_f32_bits(value_f32(v)), int64), '_int32')// &
                   ', 1.0_real32)')
         case (TYPE_FLOAT64)
            call w('   real(real64), parameter :: '//cname//' = transfer('// &
                   signed_lit(cp_f64_bits(value_f64(v)), '_int64')//', 1.0_real64)')
         case (TYPE_TEXT)
            call value_text(v, s, err)
            if (err /= CAPNP_OK) return
            call w('   character(len=*), parameter :: '//cname//' = '//ftext(s))
         case (TYPE_DATA)
            call value_data(v, db, err)
            if (err /= CAPNP_OK) return
            if (size(db) == 0) then
               call w('   integer(int8), parameter :: '//cname// &
                      '(0:-1) = [integer(int8) ::]')
            else
               call note_blob(cname, db)
            end if
         end select
      end do
   end subroutine emit_ann_list

   !> Resolve a capability-table slot to an rpc_cap_t.
   !> layout.c++ setCapabilityPointer stores the table index; the
   !> descriptor id is what a caller addresses.
   subroutine emit_cap_resolve()
      call w('   subroutine cap_resolve(ctab, idx, cap, err)')
      call w('      type(capnp_ptr_t), intent(in) :: ctab')
      call w('      integer(int64), intent(in) :: idx')
      call w('      type(rpc_cap_t), intent(out) :: cap')
      call w('      integer, intent(out) :: err')
      call w('      type(cap_descriptor_t) :: cd')
      call w('      cap%kind = RPC_CAP_NONE')
      call w('      cap%id = 0_int64')
      call w('      err = CAPNP_OK')
      call w('      if (idx < 0_int64 .or. idx >= capnp_list_len(ctab)) then')
      call w('         err = CAPNP_ERR_BOUNDS')
      call w('         return')
      call w('      end if')
      call w('      cd%p = capnp_list_get_struct(ctab, int(idx), err)')
      call w('      if (err /= CAPNP_OK) return')
      call w('      select case (cap_descriptor_which(cd))')
      call w('      case (cap_descriptor_none_tag)')
      call w('         cap%kind = RPC_CAP_NONE')
      call w('      case (cap_descriptor_sender_hosted_tag)')
      call w('         cap%kind = RPC_CAP_IMPORT')
      call w('         cap%id = cap_descriptor_sender_hosted_get(cd)')
      call w('      case (cap_descriptor_sender_promise_tag)')
      call w('         cap%kind = RPC_CAP_IMPORT')
      call w('         cap%id = cap_descriptor_sender_promise_get(cd)')
      call w('      case (cap_descriptor_receiver_hosted_tag)')
      call w('         cap%kind = RPC_CAP_IMPORT')
      call w('         cap%id = cap_descriptor_receiver_hosted_get(cd)')
      call w('      case default')
      call w('         err = CAPNP_ERR_KIND')
      call w('      end select')
      call w('   end subroutine cap_resolve')
      call w('')
   end subroutine emit_cap_resolve

   !> Same table walk as cap_resolve, writing kind and id without rpc_cap_t.
   !> kind 0 is none and kind 1 is an import (RPC_CAP_NONE / RPC_CAP_IMPORT).
   subroutine emit_cap_resolve_id()
      call w('   subroutine cap_resolve_id(ctab, idx, kind, id, err)')
      call w('      type(capnp_ptr_t), intent(in) :: ctab')
      call w('      integer(int64), intent(in) :: idx')
      call w('      integer, intent(out) :: kind')
      call w('      integer(int64), intent(out) :: id')
      call w('      integer, intent(out) :: err')
      call w('      type(cap_descriptor_t) :: cd')
      call w('      kind = 0')
      call w('      id = 0_int64')
      call w('      err = CAPNP_OK')
      call w('      if (idx < 0_int64 .or. idx >= capnp_list_len(ctab)) then')
      call w('         err = CAPNP_ERR_BOUNDS')
      call w('         return')
      call w('      end if')
      call w('      cd%p = capnp_list_get_struct(ctab, int(idx), err)')
      call w('      if (err /= CAPNP_OK) return')
      call w('      select case (cap_descriptor_which(cd))')
      call w('      case (cap_descriptor_none_tag)')
      call w('         kind = 0')
      call w('      case (cap_descriptor_sender_hosted_tag)')
      call w('         kind = 1')
      call w('         id = cap_descriptor_sender_hosted_get(cd)')
      call w('      case (cap_descriptor_sender_promise_tag)')
      call w('         kind = 1')
      call w('         id = cap_descriptor_sender_promise_get(cd)')
      call w('      case (cap_descriptor_receiver_hosted_tag)')
      call w('         kind = 1')
      call w('         id = cap_descriptor_receiver_hosted_get(cd)')
      call w('      case default')
      call w('         err = CAPNP_ERR_KIND')
      call w('      end select')
      call w('   end subroutine cap_resolve_id')
      call w('')
   end subroutine emit_cap_resolve_id

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
