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
   !> Module name of each file node (blank for the other nodes). A file in a
   !> directory carries the directory in the name.
   character(len=64), allocatable :: g_fmod(:)
   !> The plain-basename module that re-exports a file's module, or blank.
   character(len=64), allocatable :: g_shim(:)
   integer :: g_out = -1
   !> Pass 1 walks without writing, collecting imports and default blobs;
   !> pass 2 writes.
   logical :: g_suppress = .false.
   character(len=:), allocatable :: g_prefix
   character(len=64) :: g_imports(64)
   logical :: g_imp_soft(64) = .false.
   integer :: g_nimports = 0
   !> A set of identifiers, compared without regard to case.
   type :: nameset_t
      character(len=64), allocatable :: v(:)
      integer :: n = 0
   end type nameset_t

   !> The module-scope names the last captured walk declared (g_own) and
   !> made private (g_priv); g_reserved lists the names of a walk without
   !> annotation constants and helper procedures.
   type(nameset_t) :: g_own, g_priv, g_reserved
   !> A procedure this module declares under the name of a constant it also
   !> declares is renamed (g_ren_from -> g_ren_to) wherever it is written.
   !> g_dup_scan is set for the one walk that finds them; g_proc_seen and
   !> g_other_seen hold what that walk has declared so far.
   logical :: g_dup_scan = .false.
   type(nameset_t) :: g_proc_seen, g_other_seen, g_dup_found, g_ren_from
   character(len=64), allocatable :: g_ren_to(:)
   !> While set, w() leaves the procedure names of the lines it writes as
   !> they are: a constant accessor keeps the name of its constant.
   logical :: g_ren_off = .false.
   !> Annotation constants named so far in one walk of the annotation surface.
   type(nameset_t) :: g_ann_used
   !> The module tag that prefixes an annotation constant, and the module
   !> name that the file's own annotations are already named by.
   character(len=64) :: g_ann_tag = '', g_ann_mod = ''
   !> While set, w() records declared names and writes no text.
   logical :: g_capture = .false.
   !> While set, a walk leaves out the annotation constants and helpers.
   logical :: g_probe = .false.
   logical :: g_in_abstract = .false.
   character(len=64) :: g_cap_resolve = 'cap_resolve'
   character(len=64) :: g_cap_resolve_id = 'cap_resolve_id'
   character(len=512) :: g_imp_prefix(64)
   !> Names this file declares itself, renamed where it imports them.
   type :: use_t
      character(len=64) :: module = ''
      character(len=:), allocatable :: text
   end type use_t
   type(use_t) :: g_uses(64)
   integer :: g_nuses = 0
   !> Exported names per walked file, cached for the request, and the part
   !> of them the file declares itself.
   type :: export_t
      character(len=512) :: prefix = ''
      type(nameset_t) :: names
      type(nameset_t) :: declared
      !> The module that declares each name of names, in the same order.
      character(len=64), allocatable :: src(:)
   end type export_t
   type(export_t) :: g_exports(256)
   integer :: g_nexports = 0
   integer :: g_depth = 0
   type(nameset_t) :: g_declared
   !> The declaring module of each name analyze_file last exported.
   character(len=64), allocatable :: g_src(:)
   !> Foreign nodes this file refers to, found while it is walked.
   integer :: g_refs(2048)
   integer :: g_nrefs = 0
   !> A foreign node whose generated names clash here is referred to by an
   !> alias stem, and the `use` statement renames the symbols it needs.
   type :: alias_t
      integer :: idx = 0
      character(len=64) :: stem = ''
   end type alias_t
   type(alias_t) :: g_alias(512)
   integer :: g_nalias = 0
   character(len=64) :: g_pre_mod(3072), g_pre_from(3072), g_pre_to(3072)
   integer :: g_npre = 0
   integer, parameter :: N_REF_SUFFIXES = 7
   character(len=14), parameter :: REF_SUFFIXES(N_REF_SUFFIXES) = [character(len=14) :: &
      '_t', '_dwords', '_pwords', '_client_t', '_server_t', '_dispatch', '_interface_id']
   integer, parameter :: N_RPC_SYMS = 11
   character(len=40), parameter :: RPC_SYMS(N_RPC_SYMS) = [character(len=40) :: &
      'payload_t', 'payload_content_set', 'cap_descriptor_t', &
      'cap_descriptor_which', 'cap_descriptor_sender_hosted_get', &
      'cap_descriptor_sender_promise_get', 'cap_descriptor_receiver_hosted_get', &
      'cap_descriptor_none_tag', 'cap_descriptor_sender_hosted_tag', &
      'cap_descriptor_sender_promise_tag', 'cap_descriptor_receiver_hosted_tag']
   !> The name this module gives each rpc_capnp symbol, blank for its own.
   !> g_rpc_pub marks the symbols the module imports under their own name
   !> and so re-exports.
   character(len=64) :: g_rpc_to(N_RPC_SYMS) = ''
   logical :: g_rpc_pub(N_RPC_SYMS) = .false.
   logical :: g_rpc_imp(N_RPC_SYMS) = .false.
   !> The private names this module gives any_struct_t, any_list_t and
   !> capability_client_t, which the unbranded emitter had no use for.
   character(len=64) :: g_capcli = 'capnp_any_capability_client_t'
   character(len=64) :: g_anystruct = 'capnp_any_any_struct_t'
   character(len=64) :: g_anylist = 'capnp_any_any_list_t'

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
      !> Identifies the instantiation: the generic node and the node id of
      !> every binding, so two nodes that share a name stay apart.
      character(len=:), allocatable :: full
      !> 0-based binding list. Sized to the brand, so arity is not capped.
      type(capnp_ptr_t), allocatable :: bindings(:)
      !> The instantiation whose parameters each binding refers to, or 0.
      integer, allocatable :: bctx(:)
      integer :: nbind = 0
      !> Nonzero when the bindings were inherited from this scope id
      !> (Method.paramBrand is often Scope.inherit of the interface).
      integer(int64) :: inherit_scope = 0_int64
      !> Set for an instantiation the unbranded emitter never wrote: an
      !> interface, or one met while writing one. Its names carry the module.
      logical :: fresh = .false.
      !> The name without the module prefix, which a name built on this
      !> instantiation uses.
      character(len=:), allocatable :: plain
   end type inst_t
   type(inst_t) :: g_insts(64)
   integer :: g_ninsts = 0
   !> Nonzero while emitting an instantiation's accessors.
   integer :: g_cur_inst = 0
   !> While set, an import is for the names a generic instantiation or an
   !> inherited interface uses, and the `use` statement lists only those.
   logical :: g_soft = .false.

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
      integer :: stat
      l = cgr_nodes(root, err)
      if (err /= CAPNP_OK) return
      n = capnp_list_len(l)
      if (.not. associated(l%msg) .or. &
          .not. capnp_host_count_ok(n, capnp_message_nbytes(l%msg))) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      if (allocated(g_nodes)) deallocate (g_nodes)
      allocate (g_nodes(n), stat=stat)
      if (stat /= 0) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      do i = 1_int64, n
         g_nodes(i)%p = capnp_list_get_struct(l, int(i - 1), err)
         if (err /= CAPNP_OK) return
         g_nodes(i)%id = node_id(g_nodes(i)%p)
      end do
      call build_modnames(err)
   end subroutine load_nodes

   !> The text of a file's display name without directory and extension,
   !> when qualify is false, or with the directory kept as a prefix. The
   !> directories join with a double underscore, so a flat file such as
   !> dir_a_sub.capnp does not meet dir_a/sub.capnp.
   function module_stem(dn, qualify) result(o)
      character(len=*), intent(in) :: dn
      logical, intent(in) :: qualify
      character(len=:), allocatable :: o, base, dir
      integer :: i
      i = index(dn, '/', back=.true.)
      base = dn(i + 1:)
      dir = dn(1:i)
      i = index(base, '.')
      if (i > 0) base = base(1:i - 1)
      o = base
      if (qualify) o = dir//base
      do i = 1, len(o)
         if (.not. (o(i:i) >= 'a' .and. o(i:i) <= 'z' .or. o(i:i) >= 'A' .and. o(i:i) <= 'Z' &
                    .or. o(i:i) >= '0' .and. o(i:i) <= '9' .or. o(i:i) == '/')) o(i:i) = '_'
      end do
      if (qualify) call replace_all_slash(o)
      o = snake(o)
      if (len(o) == 0) o = 'f'
      if (.not. (o(1:1) >= 'a' .and. o(1:1) <= 'z')) o = 'f'//o
   end function module_stem

   !> Directory separators as a double underscore; a trailing separator of
   !> the directory part joins the base name the same way.
   subroutine replace_all_slash(o)
      character(len=:), allocatable, intent(inout) :: o
      character(len=:), allocatable :: r
      integer :: i
      r = ''
      do i = 1, len(o)
         if (o(i:i) == '/') then
            r = r//'__'
         else
            r = r//o(i:i)
         end if
      end do
      o = r
   end subroutine replace_all_slash

   !> The module a file gets from its own display name alone, so a file
   !> compiled by itself and in a larger request agree. Files in a directory
   !> carry the directory in the name. The schemas bundled with Cap'n Proto
   !> (capnp/stream.capnp and the others) keep the plain name, which is the
   !> module this library provides for them.
   function primary_module(dn) result(o)
      character(len=*), intent(in) :: dn
      character(len=:), allocatable :: o
      if (index(dn, '/') > 0 .and. index(dn, 'capnp/') /= 1) then
         o = ident_fit(module_stem(dn, .true.)//'_capnp')
      else
         o = module_stem(dn, .false.)//'_capnp'
      end if
   end function primary_module

   !> Name every file's module. A name comes from the display name alone.
   !> A file in a directory also gets a module of its plain basename that
   !> re-exports it, when that name is free in this request. Names that
   !> still meet another module take the file id.
   subroutine build_modnames(err)
      integer, intent(out) :: err
      character(len=:), allocatable :: dn, cand
      character(len=256), allocatable :: dns(:)
      character(len=64), allocatable :: plain(:)
      integer :: i, j
      logical :: clash
      err = CAPNP_OK
      if (allocated(g_fmod)) deallocate (g_fmod)
      if (allocated(g_shim)) deallocate (g_shim)
      allocate (g_fmod(size(g_nodes)), g_shim(size(g_nodes)))
      allocate (dns(size(g_nodes)), plain(size(g_nodes)))
      g_fmod = ''
      g_shim = ''
      plain = ''
      do i = 1, size(g_nodes)
         if (node_which(g_nodes(i)%p) /= NODE_FILE) cycle
         call node_display_name(g_nodes(i)%p, dn, err)
         if (err /= CAPNP_OK) return
         dns(i) = dn
         plain(i) = module_stem(dn, .false.)//'_capnp'
         g_fmod(i) = primary_module(dn)
      end do
      do i = 1, size(g_nodes)
         if (node_which(g_nodes(i)%p) /= NODE_FILE) cycle
         clash = .false.
         do j = 1, size(g_nodes)
            if (j /= i .and. node_which(g_nodes(j)%p) == NODE_FILE) then
               if (trim(g_fmod(j)) == trim(g_fmod(i))) clash = .true.
            end if
         end do
         if (.not. clash) cycle
         cand = ident_fit(module_stem(trim(dns(i)), .true.)// &
                          '_'//hex_of_id(g_nodes(i)%id, 8)//'_capnp')
         g_fmod(i) = cand
      end do
      do i = 1, size(g_nodes)
         if (node_which(g_nodes(i)%p) /= NODE_FILE) cycle
         if (trim(g_fmod(i)) == trim(plain(i))) cycle
         clash = .false.
         do j = 1, size(g_nodes)
            if (j == i .or. node_which(g_nodes(j)%p) /= NODE_FILE) cycle
            if (trim(plain(j)) == trim(plain(i)) .or. trim(g_fmod(j)) == trim(plain(i))) clash = .true.
         end do
         if (.not. clash) g_shim(i) = plain(i)
      end do
   end subroutine build_modnames

   !> The name a file's own constants and its rpc test use for the module:
   !> the plain basename module when there is one.
   function surface_name(fidx, modname) result(o)
      integer, intent(in) :: fidx
      character(len=*), intent(in) :: modname
      character(len=:), allocatable :: o
      o = modname
      if (.not. allocated(g_shim)) return
      if (fidx < 1 .or. fidx > size(g_shim)) return
      if (len_trim(g_shim(fidx)) > 0) o = trim(g_shim(fidx))
   end function surface_name

   !> The low n hex digits of an id.
   pure function hex_of_id(id, n) result(o)
      integer(int64), intent(in) :: id
      integer, intent(in) :: n
      character(len=n) :: o
      integer :: i, d
      integer(int64) :: h
      h = id
      do i = n, 1, -1
         d = int(iand(h, 15_int64))
         if (d < 10) then
            o(i:i) = achar(iachar('0') + d)
         else
            o(i:i) = achar(iachar('a') + d - 10)
         end if
         h = ishft(h, -4)
      end do
   end function hex_of_id

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
         h = ishft(h, -4)
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

   pure function lowcase(s) result(o)
      character(len=*), intent(in) :: s
      character(len=:), allocatable :: o
      integer :: i
      o = s
      do i = 1, len(o)
         if (o(i:i) >= 'A' .and. o(i:i) <= 'Z') o(i:i) = achar(iachar(o(i:i)) + 32)
      end do
   end function lowcase

   pure function upcase(s) result(o)
      character(len=*), intent(in) :: s
      character(len=:), allocatable :: o
      integer :: i
      o = s
      do i = 1, len(o)
         if (o(i:i) >= 'a' .and. o(i:i) <= 'z') o(i:i) = achar(iachar(o(i:i)) - 32)
      end do
   end function upcase

   !> Fortran character expression for an arbitrary string, written after
   !> lead columns of the caller's line. Printable characters stay inside
   !> quoted literals (quotes doubled). Controls, DEL and bytes above 126
   !> become achar() calls, matching the decoded value of a Cap'n Proto
   !> string escape (kj/parse/char.h escapeSequence). Long values continue
   !> on following lines, so no line passes the free-form limit.
   pure function ftext(s, lead) result(o)
      character(len=*), intent(in) :: s
      integer, intent(in) :: lead
      character(len=:), allocatable :: o, run
      integer, parameter :: max_run = 60
      integer :: i, a, col
      logical :: first
      if (len(s) == 0) then
         o = "''"
         return
      end if
      o = ''
      run = ''
      col = lead
      first = .true.
      do i = 1, len(s)
         a = iachar(s(i:i))
         if (a < 32 .or. a >= 127) then
            if (len(run) > 0) then
               call ftext_put(o, col, first, "'"//run//"'")
               run = ''
            end if
            call ftext_put(o, col, first, 'achar('//itoa(int(a, int64))//')')
         else
            run = run//s(i:i)
            if (s(i:i) == "'") run = run//"'"
            if (len(run) >= max_run) then
               call ftext_put(o, col, first, "'"//run//"'")
               run = ''
            end if
         end if
      end do
      if (len(run) > 0) call ftext_put(o, col, first, "'"//run//"'")
   end function ftext

   !> Append one piece of an ftext expression, continuing the line when the
   !> piece would pass column 100.
   pure subroutine ftext_put(o, col, first, piece)
      character(len=:), allocatable, intent(inout) :: o
      integer, intent(inout) :: col
      logical, intent(inout) :: first
      character(len=*), intent(in) :: piece
      integer, parameter :: max_col = 100, indent = 6
      if (first) then
         first = .false.
         if (col + len(piece) > max_col) then
            o = '&'//achar(10)//repeat(' ', indent)//piece
            col = indent + len(piece)
         else
            o = piece
            col = col + len(piece)
         end if
      else if (col + len(piece) + 3 > max_col) then
         o = o//'//&'//achar(10)//repeat(' ', indent)//piece
         col = indent + len(piece)
      else
         o = o//'//'//piece
         col = col + 2 + len(piece)
      end if
   end subroutine ftext_put

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
      if (g_ren_from%n > 0 .and. .not. g_ren_off) then
         call w_renamed(line)
         return
      end if
      if (g_capture) then
         call note_decl(line)
         return
      end if
      call w_fitted(line)
   end subroutine w

   !> Write a line with the renamed procedures spelled under their new names.
   !> Text inside a character literal or a comment is left as written.
   subroutine w_renamed(line)
      character(len=*), intent(in) :: line
      character(len=:), allocatable :: t
      character(len=1) :: quote
      integer :: k, p, n
      logical :: before, after
      t = line
      do k = 1, g_ren_from%n
         n = len_trim(g_ren_from%v(k))
         p = 1
         quote = ' '
         do while (p + n - 1 <= len(t))
            if (quote /= ' ') then
               if (t(p:p) == quote) quote = ' '
               p = p + 1
               cycle
            end if
            if (t(p:p) == "'" .or. t(p:p) == '"') then
               quote = t(p:p)
               p = p + 1
               cycle
            end if
            if (t(p:p) == '!') exit
            if (t(p:p + n - 1) == g_ren_from%v(k)(1:n)) then
               before = p > 1
               if (before) before = is_word_char(t(p - 1:p - 1))
               after = p + n <= len(t)
               if (after) after = is_word_char(t(p + n:p + n))
               if (.not. before .and. .not. after) then
                  t = t(1:p - 1)//trim(g_ren_to(k))//t(p + n:)
                  p = p + len_trim(g_ren_to(k))
                  cycle
               end if
            end if
            p = p + 1
         end do
      end do
      if (g_capture) then
         call note_decl(t)
      else
         call w_fitted(t)
      end if
   end subroutine w_renamed

   pure function is_word_char(c) result(yes)
      character(len=1), intent(in) :: c
      logical :: yes
      yes = (c >= 'a' .and. c <= 'z') .or. (c >= 'A' .and. c <= 'Z') .or. &
            (c >= '0' .and. c <= '9') .or. c == '_'
   end function is_word_char

   !> Note a name a line declares, and when a walk is scanning for them a
   !> procedure that shares its name with a constant or type.
   subroutine declare_name(name, is_proc)
      character(len=*), intent(in) :: name
      logical, intent(in) :: is_proc
      call ns_add(g_own, name)
      if (.not. g_dup_scan) return
      if (is_proc) then
         if (ns_has(g_other_seen, name) .or. ns_has(g_proc_seen, name)) &
            call ns_add(g_dup_found, name)
         call ns_add(g_proc_seen, name)
      else
         if (ns_has(g_proc_seen, name)) call ns_add(g_dup_found, name)
         call ns_add(g_other_seen, name)
      end if
   end subroutine declare_name

   !> The file node with this display name, or 0.
   function find_file_node(prefix) result(idx)
      character(len=*), intent(in) :: prefix
      integer :: idx
      character(len=:), allocatable :: dn
      integer :: i, err
      idx = 0
      do i = 1, size(g_nodes)
         if (node_which(g_nodes(i)%p) /= NODE_FILE) cycle
         call node_display_name(g_nodes(i)%p, dn, err)
         if (err /= CAPNP_OK) cycle
         if (dn == prefix) then
            idx = i
            return
         end if
      end do
   end function find_file_node

   !> Write a line, continuing it with `&` wherever it would pass the
   !> free-form limit of 132 columns. A line breaks at a blank outside a
   !> character literal; comment lines are left alone. Embedded newlines
   !> (from ftext) separate lines that are fitted one at a time.
   subroutine w_fitted(line)
      character(len=*), intent(in) :: line
      integer :: b, e
      b = 1
      do
         e = index(line(b:), achar(10))
         if (e == 0) then
            call w_fit_one(line(b:))
            return
         end if
         call w_fit_one(line(b:b + e - 2))
         b = b + e
      end do
   end subroutine w_fitted

   subroutine w_fit_one(line)
      character(len=*), intent(in) :: line
      integer, parameter :: max_col = 132, indent = 6
      character(len=:), allocatable :: rest
      integer :: lead, k, brk
      logical :: in_q
      character :: q
      rest = line
      lead = verify(rest, ' ') - 1
      if (lead < 0) lead = len(rest)
      if (lead < len(rest)) then
         if (rest(lead + 1:lead + 1) == '!') then
            write (g_out, '(a)') rest
            return
         end if
      end if
      do while (len(rest) > max_col)
         brk = 0
         in_q = .false.
         q = ' '
         do k = 1, max_col - 2
            if (in_q) then
               if (rest(k:k) == q) in_q = .false.
            else if (rest(k:k) == "'" .or. rest(k:k) == '"') then
               in_q = .true.
               q = rest(k:k)
            else if (rest(k:k) == ' ' .and. k > lead + 1) then
               brk = k
            end if
         end do
         if (brk <= lead + indent + 1) exit
         write (g_out, '(a)') rest(1:brk - 1)//' &'
         rest = repeat(' ', lead + indent)//trim(adjustl(rest(brk + 1:)))
      end do
      write (g_out, '(a)') rest
   end subroutine w_fit_one

   !> Module name owning a node: its displayName's file part, snake_cased,
   !> with the extension dropped and '_capnp' appended.
   subroutine node_module(np, o, err)
      type(capnp_ptr_t), intent(in) :: np
      character(len=:), allocatable, intent(out) :: o
      integer, intent(out) :: err
      character(len=:), allocatable :: dn
      integer :: i, fidx
      call node_display_name(np, dn, err)
      if (err /= CAPNP_OK) return
      i = index(dn, ':')
      if (i > 0) dn = dn(1:i - 1)
      fidx = find_file_node(dn)
      if (fidx > 0) then
         if (allocated(g_fmod)) then
            if (len_trim(g_fmod(fidx)) > 0) then
               o = trim(g_fmod(fidx))
               return
            end if
         end if
      end if
      o = primary_module(dn)
   end subroutine node_module

   !> Record a cross-file type reference during pass 1.
   subroutine note_import(idx, err, soft)
      type(capnp_ptr_t) :: np
      integer, intent(in) :: idx
      integer, intent(out) :: err
      logical, intent(in), optional :: soft
      character(len=:), allocatable :: dn, m
      integer :: i
      logical :: sf
      err = CAPNP_OK
      sf = g_soft
      if (present(soft)) sf = sf .or. soft
      np = g_nodes(idx)%p
      call node_display_name(np, dn, err)
      if (err /= CAPNP_OK) return
      i = index(dn, ':')
      if (i > 0) dn = dn(1:i - 1)
      if (dn == g_prefix) return
      do i = 1, g_nrefs
         if (g_refs(i) == idx) exit
      end do
      if (i > g_nrefs .and. g_nrefs < size(g_refs)) then
         g_nrefs = g_nrefs + 1
         g_refs(g_nrefs) = idx
      end if
      call node_module(np, m, err)
      if (err /= CAPNP_OK) return
      do i = 1, g_nimports
         if (trim(g_imports(i)) == m) then
            if (.not. sf) g_imp_soft(i) = .false.
            return
         end if
      end do
      if (g_nimports < size(g_imports)) then
         g_nimports = g_nimports + 1
         g_imports(g_nimports) = m
         g_imp_soft(g_nimports) = sf
         g_imp_prefix(g_nimports) = dn
      end if
   end subroutine note_import

   !> The stem to build the names of a foreign node from: its own, or the
   !> alias this file refers to it by.
   subroutine ref_stem(idx, st)
      integer, intent(in) :: idx
      character(len=:), allocatable, intent(inout) :: st
      integer :: k
      do k = 1, g_nalias
         if (g_alias(k)%idx == idx) then
            st = trim(g_alias(k)%stem)
            return
         end if
      end do
   end subroutine ref_stem

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

   !> Typed constructor. The int8 value -128 is spelled with huge()
   !> because a kind-suffixed 128 does not fit in a signed 8-bit literal.
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
            if (g_blobs(i)%bytes(j) == -huge(0_int8) - 1_int8) then
               line = line//'-huge(0_int8)-1_int8'
            else
               line = line//itoa(int(g_blobs(i)%bytes(j), int64))
            end if
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
      character(len=:), allocatable :: modname, base, dir, outfile
      type(nameset_t) :: exported
      integer :: i, ios, fidx

      call load_nodes(root, err)
      if (err /= CAPNP_OK) return
      g_nexports = 0

      ! capnpc-c++.c++ writeFile keeps the requested file's directory, so
      ! two schemas that share a basename do not overwrite each other.
      ! The file keeps the basename. So does the module name, which is what
      ! callers import, unless another file of the request has the same
      ! basename: then the module is named by its path.
      dir = ''
      base = filename
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

      fidx = find_node(file_id)
      if (fidx == 0) then
         err = CAPNP_ERR_ARG
         return
      end if
      call node_module(g_nodes(fidx)%p, modname, err)
      if (err /= CAPNP_OK) return
      call analyze_file(fidx, modname, exported, err)
      if (err /= CAPNP_OK) return

      outfile = dir//snake(base)//'_capnp.f90'
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
      call pass_write(fidx, modname, filename, err)
      if (err == CAPNP_OK .and. allocated(g_shim)) then
         if (len_trim(g_shim(fidx)) > 0) then
            write (g_out, '(a)') ''
            write (g_out, '(a)') '!> '//trim(g_shim(fidx))//' re-exports '//modname//'.'
            write (g_out, '(a)') 'module '//trim(g_shim(fidx))
            write (g_out, '(a)') '   use '//modname
            write (g_out, '(a)') 'end module '//trim(g_shim(fidx))
         end if
      end if
      close (g_out)
   end subroutine emit_file

   !> Reset the per-file state before a walk of the file with this prefix.
   subroutine begin_file(prefix)
      character(len=*), intent(in) :: prefix
      g_prefix = prefix
      g_nimports = 0
      g_nrefs = 0
      g_nblobs = 0
      g_has_iface = .false.
      g_need_any_struct = .false.
      g_need_any_list = .false.
      g_need_capability = .false.
      g_ninsts = 0
      g_cur_inst = 0
   end subroutine begin_file

   !> Pass 1 (dry): walk the procedures to discover cross-file imports,
   !> instantiations and explicit pointer-default blobs before the header
   !> is written.
   subroutine pass_collect(fidx, modname, err)
      integer, intent(in) :: fidx
      character(len=*), intent(in) :: modname
      integer, intent(out) :: err
      character(len=:), allocatable :: dn
      integer :: i
      logical :: mine
      err = CAPNP_OK
      g_suppress = .true.
      do i = 1, size(g_nodes)
         call node_display_name(g_nodes(i)%p, dn, err)
         if (err /= CAPNP_OK) exit
         mine = len(dn) > len(g_prefix) .and. index(dn, g_prefix//':') == 1
         if (.not. mine) cycle
         select case (node_which(g_nodes(i)%p))
         case (NODE_STRUCT)
            if (.not. node_struct_is_group(g_nodes(i)%p)) then
               call emit_struct_procs(g_nodes(i)%p, err)
               if (err /= CAPNP_OK) exit
            end if
         case (NODE_CONST)
            call emit_const_decl(g_nodes(i)%p, err)
            if (err /= CAPNP_OK) exit
            g_ren_off = .true.
            call emit_const_proc(g_nodes(i)%p, err)
            g_ren_off = .false.
            if (err /= CAPNP_OK) exit
         case (NODE_INTERFACE)
            g_has_iface = .true.
            call emit_interface_decl(g_nodes(i)%p, err)
            if (err /= CAPNP_OK) exit
            call emit_interface_procs(g_nodes(i)%p, err)
            if (err /= CAPNP_OK) exit
         end select
      end do
      ! Converge the instantiation worklist: emitting an instantiation's
      ! accessors can register further instantiations.
      i = 1
      do while (err == CAPNP_OK .and. i <= g_ninsts)
         call emit_inst_procs(i, err)
         i = i + 1
      end do
      ! Annotation Data values are blobs. Record them while collection
      ! is open, before emit_blob_params writes the parameters.
      if (err == CAPNP_OK .and. .not. g_probe) &
         call emit_annotation_surface(surface_name(fidx, modname), fidx, err)
      g_suppress = .false.
   end subroutine pass_collect

   !> Pass 2: the module text, written to g_out, or only its declared
   !> names while g_capture is set.
   subroutine pass_write(fidx, modname, filename, err)
      integer, intent(in) :: fidx
      character(len=*), intent(in) :: modname, filename
      integer, intent(out) :: err
      character(len=:), allocatable :: dn
      integer :: i
      logical :: mine
      err = CAPNP_OK

      call w('!> Generated by capnpc-fortran from '//trim(filename)//'. Do not edit.')
      call w('module '//modname)
      call w('   use capnp')
      call emit_any_use()
      if (g_has_iface) call w('   use capnp_rpc')
      ! rpc_capnp is the schema module. capnp_rpc already uses the
      ! three-party schema, so a capability field must not pull capnp_rpc
      ! back in. Descriptor symbols are enough to read a cap table.
      if (g_has_iface .or. g_need_capability) call emit_rpc_use()
      do i = 1, g_nimports
         call emit_import_use(trim(g_imports(i)))
      end do
      call w('   implicit none')
      call w('   public')
      call w('')

      ! Declarations pass: enums, consts, struct params + handle types.
      do i = 1, size(g_nodes)
         call node_display_name(g_nodes(i)%p, dn, err)
         if (err /= CAPNP_OK) exit
         mine = len(dn) > len(g_prefix) .and. index(dn, g_prefix//':') == 1
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
      if (err == CAPNP_OK .and. .not. g_probe) &
         call emit_annotation_surface(surface_name(fidx, modname), fidx, err)
      if (err == CAPNP_OK) call emit_any_decls()

      ! Interface declarations follow every struct declaration: their
      ! abstract method interfaces IMPORT the param/result handle types,
      ! which must already be declared.
      if (err == CAPNP_OK) call emit_iface_decls_ordered(g_prefix, err)

      call w('contains')
      call w('')

      ! Procedures pass: struct accessors (groups reached via parents).
      if (err == CAPNP_OK) then
         do i = 1, size(g_nodes)
            call node_display_name(g_nodes(i)%p, dn, err)
            if (err /= CAPNP_OK) exit
            mine = len(dn) > len(g_prefix) .and. index(dn, g_prefix//':') == 1
            if (.not. mine) cycle
            select case (node_which(g_nodes(i)%p))
            case (NODE_STRUCT)
               if (.not. node_struct_is_group(g_nodes(i)%p)) then
                  call emit_struct_procs(g_nodes(i)%p, err)
                  if (err /= CAPNP_OK) exit
               end if
            case (NODE_CONST)
               g_ren_off = .true.
               call emit_const_proc(g_nodes(i)%p, err)
               g_ren_off = .false.
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

      if (err == CAPNP_OK .and. g_has_iface .and. .not. g_probe) call emit_cap_resolve()
      if (err == CAPNP_OK .and. g_need_capability .and. .not. g_probe) &
         call emit_cap_resolve_id()
      call w('end module '//modname)
   end subroutine pass_write

   ! --- scoped names ----------------------------------------------------------
   !
   ! Every schema becomes one module, and a module re-exports what it uses, so
   ! a name that two schemas both declare (an annotation of the same name, a
   ! generic instantiated in two files, a struct called Payload next to the
   ! rpc.capnp symbols a capability needs) would clash in the importer. Each
   ! file is walked first to learn the names it declares and exports. An
   ! imported name that the importing module declares itself is renamed on
   ! use, so the module's own names keep their plain spelling.

   !> The names the module of one file exports, as a sorted-by-use set:
   !> its own public declarations and those of everything it imports.
   !> The result is cached for the request.
   recursive subroutine exports_of(prefix, k)
      character(len=*), intent(in) :: prefix
      integer, intent(out) :: k
      character(len=:), allocatable :: modname
      type(nameset_t) :: names
      integer :: fidx, err
      do k = 1, g_nexports
         if (trim(g_exports(k)%prefix) == prefix) return
      end do
      k = g_nexports + 1
      if (k > size(g_exports)) then
         k = 0
         return
      end if
      g_nexports = k
      g_exports(k)%prefix = prefix
      g_exports(k)%names%n = 0
      g_exports(k)%declared%n = 0
      fidx = find_file_node(prefix)
      if (fidx == 0 .or. g_depth >= 32) return
      call node_module(g_nodes(fidx)%p, modname, err)
      if (err /= CAPNP_OK) return
      g_depth = g_depth + 1
      call analyze_file(fidx, modname, names, err)
      g_depth = g_depth - 1
      if (err == CAPNP_OK) then
         g_exports(k)%names = names
         g_exports(k)%declared = g_declared
         g_exports(k)%src = g_src
      end if
   end subroutine exports_of

   !> Learn what the module of file fidx declares and how it must import,
   !> leaving the file state ready for pass_write. The file is walked
   !> without its annotation constants and helper procedures to collect the
   !> names everything else declares, then with them.
   recursive subroutine analyze_file(fidx, modname, exported, err)
      integer, intent(in) :: fidx
      character(len=*), intent(in) :: modname
      type(nameset_t), intent(out) :: exported
      integer, intent(out) :: err
      character(len=:), allocatable :: prefix
      character(len=512) :: ipfx(64)
      character(len=64) :: imod(64)
      integer :: iek(64), nimp, i, k, j
      logical :: rpc_uses, isoft(64)
      type(nameset_t) :: own_pub, need

      call node_display_name(g_nodes(fidx)%p, prefix, err)
      if (err /= CAPNP_OK) return

      g_ren_from%n = 0
      call begin_file(prefix)
      g_probe = .true.
      call pass_collect(fidx, modname, err)
      if (err /= CAPNP_OK) return
      nimp = g_nimports
      do i = 1, nimp
         ipfx(i) = g_imp_prefix(i)
         imod(i) = g_imports(i)
         isoft(i) = g_imp_soft(i)
      end do
      do i = 1, nimp
         call exports_of(trim(ipfx(i)), iek(i))
      end do
      g_nalias = 0
      g_npre = 0

      call begin_file(prefix)
      g_probe = .true.
      call pass_collect(fidx, modname, err)
      if (err /= CAPNP_OK) return
      g_ren_from%n = 0
      g_dup_found%n = 0
      g_proc_seen%n = 0
      g_other_seen%n = 0
      g_dup_scan = .true.
      call capture_names(fidx, modname, err)
      g_dup_scan = .false.
      if (err /= CAPNP_OK) return
      if (allocated(g_ren_to)) deallocate (g_ren_to)
      allocate (g_ren_to(max(g_dup_found%n, 1)))
      do k = 1, g_dup_found%n
         g_ren_to(k) = free_name(trim(g_dup_found%v(k))//'_f', g_own)
      end do
      g_ren_from = g_dup_found
      call capture_names(fidx, modname, err)
      if (err /= CAPNP_OK) return
      g_reserved = g_own
      g_cap_resolve = free_name('cap_resolve', g_reserved)
      if (g_has_iface) call ns_add(g_reserved, trim(g_cap_resolve))
      g_cap_resolve_id = free_name('cap_resolve_id', g_reserved)
      if (g_need_capability) call ns_add(g_reserved, trim(g_cap_resolve_id))

      call begin_file(prefix)
      g_probe = .false.
      call pass_collect(fidx, modname, err)
      if (err /= CAPNP_OK) return
      call capture_names(fidx, modname, err)
      if (err /= CAPNP_OK) return
      rpc_uses = g_has_iface .or. g_need_capability

      exported%n = 0
      do k = 1, g_own%n
         if (.not. ns_has(g_priv, g_own%v(k))) call ns_add(exported, g_own%v(k))
      end do
      own_pub = exported
      do k = 1, size(RPC_SYMS)
         g_rpc_to(k) = ''
         g_rpc_pub(k) = .false.
         g_rpc_imp(k) = .false.
         if (.not. rpc_uses .or. surface_name(fidx, modname) == 'rpc_capnp') cycle
         if (k <= 2 .and. .not. g_has_iface) cycle
         g_rpc_imp(k) = .true.
         if (k > 2 .or. .not. g_has_iface) then
            ! The unbranded emitter never imported this symbol, so it comes
            ! in under a private alias that no importer sees.
            g_rpc_to(k) = free_name('rpc_capnp_'//trim(RPC_SYMS(k)), taken_names(nimp, iek))
         else if (rpc_sym_clash(trim(RPC_SYMS(k)), nimp, iek)) then
            g_rpc_to(k) = free_name('rpc_capnp_'//trim(RPC_SYMS(k)), taken_names(nimp, iek))
            g_rpc_pub(k) = .true.
            call ns_add(exported, trim(g_rpc_to(k)))
         else
            g_rpc_pub(k) = .true.
            call ns_add(exported, trim(RPC_SYMS(k)))
         end if
      end do
      g_capcli = free_name('capnp_any_capability_client_t', taken_names(nimp, iek))
      g_anystruct = free_name('capnp_any_any_struct_t', taken_names(nimp, iek))
      g_anylist = free_name('capnp_any_any_list_t', taken_names(nimp, iek))
      g_declared = exported
      call plan_ref_aliases(nimp, imod, iek, exported)
      g_nuses = 0
      do i = 1, nimp
         if (iek(i) == 0) cycle
         call plan_import_use(trim(imod(i)), g_exports(iek(i))%names, exported, isoft(i))
      end do
      do i = 1, nimp
         if (iek(i) == 0) cycle
         if (isoft(i)) need = ref_names(trim(imod(i)), g_exports(iek(i))%names)
         do j = 1, g_exports(iek(i))%names%n
            if (is_pre_renamed(trim(imod(i)), g_exports(iek(i))%names%v(j))) cycle
            if (isoft(i)) then
               if (.not. ns_has(need, g_exports(iek(i))%names%v(j))) cycle
            end if
            call ns_add(exported, g_exports(iek(i))%names%v(j))
         end do
         do j = 1, g_npre
            if (trim(g_pre_mod(j)) == trim(imod(i))) call ns_add(exported, trim(g_pre_to(j)))
         end do
      end do
      call note_sources(surface_name(fidx, modname), nimp, imod, iek, exported, own_pub)

      call begin_file(prefix)
      g_probe = .false.
      call pass_collect(fidx, modname, err)
   end subroutine analyze_file

   !> Whether this file's use statement for the module renames the name,
   !> which then reaches the file, and whoever imports it, only as its alias.
   function is_pre_renamed(imod, name) result(yes)
      character(len=*), intent(in) :: imod, name
      logical :: yes
      integer :: j
      yes = .false.
      do j = 1, g_npre
         if (trim(g_pre_mod(j)) /= imod) cycle
         if (trim(g_pre_from(j)) == lowcase(name)) then
            yes = .true.
            return
         end if
      end do
   end function is_pre_renamed

   !> Whether an rpc_capnp symbol meets another entity of the same name:
   !> one this file declares, or one an imported module makes visible.
   function rpc_sym_clash(sym, nimp, iek) result(yes)
      character(len=*), intent(in) :: sym
      integer, intent(in) :: nimp
      integer, intent(in) :: iek(:)
      logical :: yes
      integer :: i
      yes = ns_has(g_own, sym)
      do i = 1, nimp
         if (yes) return
         if (iek(i) == 0) cycle
         if (.not. ns_has(g_exports(iek(i))%names, sym)) cycle
         if (export_src(iek(i), sym) /= 'rpc_capnp') yes = .true.
      end do
   end function rpc_sym_clash

   !> Every name this file declares or imports.
   function taken_names(nimp, iek) result(set)
      integer, intent(in) :: nimp
      integer, intent(in) :: iek(:)
      type(nameset_t) :: set
      integer :: i, j
      set = g_own
      do i = 1, nimp
         if (iek(i) == 0) cycle
         do j = 1, g_exports(iek(i))%names%n
            call ns_add(set, g_exports(iek(i))%names%v(j))
         end do
      end do
   end function taken_names

   !> The position of a name in a set, or 0.
   function ns_find(set, name) result(k)
      type(nameset_t), intent(in) :: set
      character(len=*), intent(in) :: name
      integer :: k
      character(len=:), allocatable :: key
      key = lowcase(name)
      do k = 1, set%n
         if (trim(set%v(k)) == key) return
      end do
      k = 0
   end function ns_find

   !> The module that declares a name an imported module exports, or blank.
   function export_src(i, name) result(s)
      integer, intent(in) :: i
      character(len=*), intent(in) :: name
      character(len=64) :: s
      integer :: k
      s = ''
      if (i <= 0) return
      k = ns_find(g_exports(i)%names, name)
      if (k == 0) return
      if (.not. allocated(g_exports(i)%src)) return
      if (k <= size(g_exports(i)%src)) s = g_exports(i)%src(k)
   end function export_src

   !> Record, for each exported name, the module that declares the entity.
   !> One entity reaching a file by two paths does not clash.
   subroutine note_sources(modname, nimp, imod, iek, exported, own_pub)
      character(len=*), intent(in) :: modname
      integer, intent(in) :: nimp
      character(len=64), intent(in) :: imod(:)
      integer, intent(in) :: iek(:)
      type(nameset_t), intent(in) :: exported, own_pub
      character(len=64) :: s
      character(len=:), allocatable :: nm
      integer :: k, q, i, j
      logical :: found
      if (allocated(g_src)) deallocate (g_src)
      allocate (g_src(max(1, exported%n)))
      g_src = ''
      do k = 1, exported%n
         nm = trim(exported%v(k))
         s = ''
         found = ns_has(own_pub, nm)
         if (found) s = modname
         do q = 1, size(RPC_SYMS)
            if (found) exit
            if (.not. g_rpc_pub(q)) cycle
            if (len_trim(g_rpc_to(q)) > 0) then
               found = nm == trim(g_rpc_to(q))
            else
               found = nm == trim(RPC_SYMS(q)) .and. .not. ns_has(g_own, nm)
            end if
            if (found) s = 'rpc_capnp'
         end do
         do j = 1, g_npre
            if (found) exit
            if (trim(g_pre_to(j)) /= nm) cycle
            do i = 1, nimp
               if (trim(imod(i)) == trim(g_pre_mod(j))) s = export_src(iek(i), trim(g_pre_from(j)))
            end do
            found = .true.
         end do
         do i = 1, nimp
            if (found) exit
            if (iek(i) == 0) cycle
            if (.not. ns_has(g_exports(iek(i))%names, nm)) cycle
            s = export_src(iek(i), nm)
            found = .true.
         end do
         g_src(k) = s
      end do
   end subroutine note_sources

   !> Run pass_write for its declarations only: g_own gets every name the
   !> module declares at module scope and g_priv the ones it makes private.
   subroutine capture_names(fidx, modname, err)
      integer, intent(in) :: fidx
      character(len=*), intent(in) :: modname
      integer, intent(out) :: err
      g_own%n = 0
      g_priv%n = 0
      g_in_abstract = .false.
      g_capture = .true.
      call pass_write(fidx, modname, g_prefix, err)
      g_capture = .false.
   end subroutine capture_names

   !> A name built from base that is not in taken.
   function free_name(base, taken) result(o)
      character(len=*), intent(in) :: base
      type(nameset_t), intent(in) :: taken
      character(len=:), allocatable :: o
      integer :: k
      o = ident_fit(base)
      k = 1
      do while (ns_has(taken, o))
         k = k + 1
         o = ident_fit(base//'_'//itoa(int(k, int64)))
      end do
   end function free_name

   !> A foreign node whose generated names meet a name this file declares,
   !> or a name another imported file declares, is referred to by an alias
   !> stem. The renames that bring its symbols in under the alias are kept
   !> for plan_import_use.
   subroutine plan_ref_aliases(nimp, imod, iek, exported)
      integer, intent(in) :: nimp
      character(len=64), intent(in) :: imod(:)
      integer, intent(in) :: iek(:)
      type(nameset_t), intent(inout) :: exported
      character(len=:), allocatable :: m, stem, cand
      integer :: r, i, j, q, k, idx, err
      logical :: clash
      do r = 1, g_nrefs
         idx = g_refs(r)
         call node_module(g_nodes(idx)%p, m, err)
         if (err /= CAPNP_OK) cycle
         i = 0
         do j = 1, nimp
            if (trim(imod(j)) == m .and. iek(j) > 0) i = j
         end do
         if (i == 0) cycle
         call node_fname(g_nodes(idx)%p, stem, err)
         if (err /= CAPNP_OK) cycle
         clash = .false.
         do q = 1, N_REF_SUFFIXES
            cand = stem//trim(REF_SUFFIXES(q))
            if (.not. ns_has(g_exports(iek(i))%names, cand)) cycle
            if (ns_has(g_own, cand)) clash = .true.
            do j = 1, nimp
               if (j == i .or. iek(j) == 0) cycle
               if (.not. ns_has(g_exports(iek(j))%names, cand)) cycle
               if (export_src(iek(j), cand) /= export_src(iek(i), cand) .or. &
                   len_trim(export_src(iek(i), cand)) == 0) clash = .true.
            end do
         end do
         if (.not. clash) cycle
         if (g_nalias >= size(g_alias) .or. g_npre + N_REF_SUFFIXES > size(g_pre_mod)) cycle
         cand = ident_fit(m//'_'//stem, 40)
         k = 1
         do while (alias_taken(cand, nimp, iek, exported))
            k = k + 1
            cand = ident_fit(m//'_'//stem//'_'//itoa(int(k, int64)), 40)
         end do
         g_nalias = g_nalias + 1
         g_alias(g_nalias)%idx = idx
         g_alias(g_nalias)%stem = cand
         do q = 1, N_REF_SUFFIXES
            if (.not. ns_has(g_exports(iek(i))%names, stem//trim(REF_SUFFIXES(q)))) cycle
            g_npre = g_npre + 1
            g_pre_mod(g_npre) = m
            g_pre_from(g_npre) = lowcase(stem//trim(REF_SUFFIXES(q)))
            g_pre_to(g_npre) = lowcase(cand//trim(REF_SUFFIXES(q)))
            call ns_add(exported, trim(g_pre_to(g_npre)))
         end do
      end do
   end subroutine plan_ref_aliases

   !> Whether an alias stem would give a name that is already in use.
   function alias_taken(cand, nimp, iek, exported) result(yes)
      character(len=*), intent(in) :: cand
      integer, intent(in) :: nimp
      integer, intent(in) :: iek(:)
      type(nameset_t), intent(in) :: exported
      logical :: yes
      integer :: q, j
      yes = .false.
      do q = 1, N_REF_SUFFIXES
         if (ns_has(g_own, cand//trim(REF_SUFFIXES(q)))) yes = .true.
         if (ns_has(exported, cand//trim(REF_SUFFIXES(q)))) yes = .true.
         do j = 1, nimp
            if (iek(j) == 0) cycle
            if (ns_has(g_exports(iek(j))%names, cand//trim(REF_SUFFIXES(q)))) yes = .true.
         end do
      end do
   end function alias_taken

   !> Choose the `use` statement for one imported module: names the
   !> importer declares itself come in under a module-qualified alias,
   !> and so do the symbols of the foreign nodes given an alias stem.
   subroutine plan_import_use(imod, theirs, exported, soft)
      character(len=*), intent(in) :: imod
      type(nameset_t), intent(in) :: theirs
      type(nameset_t), intent(inout) :: exported
      logical, intent(in) :: soft
      integer, parameter :: per_stmt = 24
      character(len=:), allocatable :: text, stmts, alias, lead
      type(nameset_t) :: done, need
      integer :: j, n
      stmts = ''
      text = ''
      n = 0
      lead = '   use '//imod//', '
      if (soft) then
         lead = lead//'only: '
         need = ref_names(imod, theirs)
      end if
      do j = 1, g_npre
         if (trim(g_pre_mod(j)) /= imod) cycle
         call ns_add(done, trim(g_pre_from(j)))
         call add_item(trim(g_pre_to(j))//' => '//trim(g_pre_from(j)))
      end do
      do j = 1, theirs%n
         if (soft) then
            if (.not. ns_has(need, theirs%v(j))) cycle
         end if
         if (ns_has(done, theirs%v(j))) cycle
         if (.not. ns_has(g_own, theirs%v(j))) then
            if (soft) call add_item(trim(theirs%v(j)))
            cycle
         end if
         alias = free_name(imod//'_'//trim(theirs%v(j)), g_own)
         do while (ns_has(exported, alias))
            alias = free_name(alias//'_x', g_own)
         end do
         call ns_add(exported, alias)
         call add_item(alias//' => '//trim(theirs%v(j)))
         if (g_npre < size(g_pre_mod)) then
            g_npre = g_npre + 1
            g_pre_mod(g_npre) = imod
            g_pre_from(g_npre) = lowcase(trim(theirs%v(j)))
            g_pre_to(g_npre) = lowcase(alias)
         end if
      end do
      if (n == 0 .and. .not. soft) return
      if (g_nuses >= size(g_uses)) return
      g_nuses = g_nuses + 1
      g_uses(g_nuses)%module = imod
      g_uses(g_nuses)%text = stmts//lead//text
   contains
      subroutine add_item(item)
         character(len=*), intent(in) :: item
         if (n == per_stmt) then
            stmts = stmts//lead//text//achar(10)
            text = ''
            n = 0
         end if
         if (n > 0) text = text//', '
         text = text//item
         n = n + 1
      end subroutine add_item
   end subroutine plan_import_use

   !> The names of the foreign nodes this file refers to in one module that
   !> the generated code uses: a name per suffix a node's symbols take.
   function ref_names(imod, theirs) result(set)
      character(len=*), intent(in) :: imod
      type(nameset_t), intent(in) :: theirs
      type(nameset_t) :: set
      character(len=:), allocatable :: m, stem
      integer :: r, q, err
      do r = 1, g_nrefs
         call node_module(g_nodes(g_refs(r))%p, m, err)
         if (err /= CAPNP_OK) cycle
         if (m /= imod) cycle
         call node_fname(g_nodes(g_refs(r))%p, stem, err)
         if (err /= CAPNP_OK) cycle
         do q = 1, N_REF_SUFFIXES
            if (ns_has(theirs, stem//trim(REF_SUFFIXES(q)))) &
               call ns_add(set, stem//trim(REF_SUFFIXES(q)))
         end do
      end do
   end function ref_names

   subroutine emit_import_use(imod)
      character(len=*), intent(in) :: imod
      integer :: k
      do k = 1, g_nuses
         if (trim(g_uses(k)%module) == imod) then
            call w(g_uses(k)%text)
            return
         end if
      end do
      call w('   use '//imod)
   end subroutine emit_import_use

   !> The typed any-pointer handles this file uses, taken from capnp_any under
   !> private names so that a schema may declare structs called AnyStruct,
   !> AnyList or CapabilityClient.
   subroutine emit_any_use()
      if (g_need_any_struct) call w('   use capnp_any, only: '//trim(g_anystruct)//' => any_struct_t')
      if (g_need_any_list) call w('   use capnp_any, only: '//trim(g_anylist)//' => any_list_t')
      if (g_need_capability) call w('   use capnp_any, only: '//trim(g_capcli)//' => capability_client_t')
   end subroutine emit_any_use

   !> Keep the aliases of the rpc_capnp symbols the module imports for its own
   !> use out of what an importing module sees.
   subroutine emit_rpc_private()
      character(len=:), allocatable :: text
      integer :: k
      text = ''
      do k = 1, size(RPC_SYMS)
         if (len_trim(g_rpc_to(k)) == 0 .or. g_rpc_pub(k)) cycle
         if (len(text) > 0 .and. len(text) + len_trim(g_rpc_to(k)) > 80) then
            call w('   private :: '//text)
            text = ''
         end if
         if (len(text) > 0) text = text//', '
         text = text//trim(g_rpc_to(k))
      end do
      if (g_need_any_struct) then
         if (len(text) > 0) text = text//', '
         text = text//trim(g_anystruct)
      end if
      if (g_need_any_list) then
         if (len(text) > 0) text = text//', '
         text = text//trim(g_anylist)
      end if
      if (g_need_capability) then
         if (len(text) > 0) text = text//', '
         text = text//trim(g_capcli)
      end if
      if (len(text) > 0) call w('   private :: '//text)
   end subroutine emit_rpc_private

   !> The rpc_capnp symbols a capability or interface file reads a cap
   !> table with. A symbol the file declares itself keeps its plain name
   !> there and is renamed on use.
   subroutine emit_rpc_use()
      character(len=:), allocatable :: text, item
      integer :: k
      text = ''
      do k = 1, size(RPC_SYMS)
         if (.not. g_rpc_imp(k)) cycle
         if (len_trim(g_rpc_to(k)) > 0) then
            item = trim(g_rpc_to(k))//' => '//trim(RPC_SYMS(k))
         else
            item = trim(RPC_SYMS(k))
         end if
         if (len(text) > 0 .and. len(text) + len(item) > 80) then
            call w('   use rpc_capnp, only: '//text)
            text = ''
         end if
         if (len(text) > 0) text = text//', '
         text = text//item
      end do
      if (len(text) > 0) call w('   use rpc_capnp, only: '//text)
   end subroutine emit_rpc_use

   !> A symbol of rpc_capnp under the name this module uses for it.
   function rpcn(sym) result(o)
      character(len=*), intent(in) :: sym
      character(len=:), allocatable :: o
      integer :: k
      o = sym
      do k = 1, size(RPC_SYMS)
         if (trim(RPC_SYMS(k)) == sym .and. len_trim(g_rpc_to(k)) > 0) then
            o = trim(g_rpc_to(k))
            return
         end if
      end do
   end function rpcn

   !> Record the module-scope names a written line declares (capture mode).
   subroutine note_decl(line)
      character(len=*), intent(in) :: line
      character(len=:), allocatable :: t
      integer :: p, q
      if (len(line) < 4) return
      if (line(1:3) /= '   ') return
      ! The procedures of an abstract interface are module entities too.
      if (line(4:4) == ' ') then
         if (g_in_abstract .and. len(line) > 6) then
            if (line(1:6) == '      ' .and. line(7:7) /= ' ') then
               t = lowcase(line(7:))
               if (starts(t, 'end ')) return
               p = index(t, 'function ')
               q = 9
               if (p == 0) then
                  p = index(t, 'subroutine ')
                  q = 11
               end if
               if (p > 0) then
                  call ident_at(t, p + q, p, q)
                  if (q >= p) call declare_name(t(p:q), .true.)
               end if
            end if
         end if
         return
      end if
      t = lowcase(line(4:))
      if (starts(t, 'abstract interface')) g_in_abstract = .true.
      if (starts(t, 'end interface')) g_in_abstract = .false.
      if (starts(t, 'use ') .or. starts(t, 'end ') .or. starts(t, 'public') .or. &
          starts(t, 'implicit') .or. starts(t, 'contains') .or. starts(t, 'abstract') .or. &
          starts(t, 'interface')) return
      if (starts(t, 'private')) then
         p = index(t, '::')
         if (p == 0) return
         t = t(p + 2:)
         do
            call ident_at(t, 1, p, q)
            if (q < p) exit
            call ns_add(g_priv, t(p:q))
            t = adjustl(t(q + 1:))
            if (len(t) == 0) exit
            if (t(1:1) /= ',') exit
            t = adjustl(t(2:))
         end do
         return
      end if
      p = index(t, '::')
      if (p > 0) then
         call ident_at(t, p + 2, p, q)
         if (q >= p) call declare_name(t(p:q), .false.)
         return
      end if
      p = index(t, 'function ')
      if (p > 0) then
         if (p == 1) then
            call ident_at(t, p + 9, p, q)
         else if (t(p - 1:p - 1) == ' ') then
            call ident_at(t, p + 9, p, q)
         else
            q = 0
            p = 1
         end if
         if (q >= p) call declare_name(t(p:q), .true.)
         return
      end if
      p = index(t, 'subroutine ')
      if (p > 0) then
         call ident_at(t, p + 11, p, q)
         if (q >= p) call declare_name(t(p:q), .true.)
      end if
   end subroutine note_decl

   pure function starts(s, pre) result(yes)
      character(len=*), intent(in) :: s, pre
      logical :: yes
      yes = len(s) >= len(pre)
      if (yes) yes = s(1:len(pre)) == pre
   end function starts

   !> Bounds of the identifier that starts at or after position from
   !> (blanks skipped); q < p when there is none.
   pure subroutine ident_at(t, from, p, q)
      character(len=*), intent(in) :: t
      integer, intent(in) :: from
      integer, intent(out) :: p, q
      p = from
      do while (p <= len(t))
         if (t(p:p) /= ' ') exit
         p = p + 1
      end do
      q = p - 1
      do while (q + 1 <= len(t))
         if (.not. ((t(q + 1:q + 1) >= 'a' .and. t(q + 1:q + 1) <= 'z') .or. &
                    (t(q + 1:q + 1) >= '0' .and. t(q + 1:q + 1) <= '9') .or. &
                    t(q + 1:q + 1) == '_')) exit
         q = q + 1
      end do
   end subroutine ident_at

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
      character(len=:), allocatable :: cn, s, pre
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
         pre = '   character(len=*), parameter :: '//upcase(cn)//' = '
         call w(pre//ftext(s, len(pre)))
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
         call ref_stem(idx, st)
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
      integer :: disc, gidx, ctx, saved
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
      ! The bound type is read in the instantiation it was written in.
      ctx = g_cur_inst
      if (g_cur_inst > 0) then
         if (type_which(t) == TYPE_ANY_POINTER) then
            if (type_anyptr_which(t) == ANYPTR_PARAMETER) then
               if (inst_scope_hit(g_cur_inst, type_param_scope_id(t))) then
                  if (type_param_index(t) < g_insts(g_cur_inst)%nbind) then
                     ! Unbound parameters keep their AnyPointer accessors.
                     if (g_insts(g_cur_inst)%bindings(type_param_index(t))%kind &
                         /= CAPNP_PK_NULL) then
                        ctx = g_insts(g_cur_inst)%bctx(type_param_index(t))
                        t = g_insts(g_cur_inst)%bindings(type_param_index(t))
                     end if
                  end if
               end if
            end if
         end if
      end if
      dv = field_slot_default(f, err)
      if (err /= CAPNP_OK) return
      off = field_slot_offset(f)

      saved = g_cur_inst
      g_cur_inst = ctx
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
      g_cur_inst = saved
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
         call emit_wrapped_ptr(an, ht, pidx, trim(g_anystruct), gset)
      case (ANYPTR_KIND_LIST)
         g_need_any_list = .true.
         call emit_wrapped_ptr(an, ht, pidx, trim(g_anylist), gset)
      case (ANYPTR_KIND_CAPABILITY)
         ! Use a local client. The three-party schema is used by capnp_rpc,
         ! so this module must not use capnp_rpc in return.
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
      call w('         call '//trim(g_cap_resolve)//'(ctab, q%capidx, c%cap, err)')
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
      call w('      type('//trim(g_capcli)//') :: c')
      call w('      type(capnp_ptr_t), intent(in), optional :: ctab')
      call w('      type(capnp_ptr_t) :: q')
      call w('      c%kind = 0')
      call w('      c%id = 0_int64')
      call w('      q = capnp_getp(h%p, '//itoa(int(pidx, int64))//', err)')
      call w('      if (err /= CAPNP_OK) return')
      call w('      if (q%kind /= CAPNP_PK_CAP) return')
      call w('      if (present(ctab)) then')
      call w('         call '//trim(g_cap_resolve_id)//'(ctab, q%capidx, c%kind, c%id, err)')
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
      call ref_stem(idx, st)
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
         call w('         s = '//ftext(dtext, 14))
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
      call ref_stem(idx, st)
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
         call ref_stem(idx, st)
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
   recursive subroutine inst_from_brand(gidx, b, o, err, key, plain)
      integer, intent(in) :: gidx
      type(capnp_ptr_t), intent(in) :: b
      character(len=:), allocatable, intent(out) :: o
      integer, intent(out) :: err
      character(len=:), allocatable, intent(out), optional :: key, plain
      type(capnp_ptr_t) :: scopes, sc, binds, bd
      type(capnp_ptr_t), allocatable :: btypes(:)
      integer, allocatable :: bctx(:)
      character(len=:), allocatable :: gname, part, mpart, kpart, ikey, pl, ol
      integer(int64) :: i, j, nb
      integer :: n, k, prev
      logical :: was
      err = CAPNP_OK
      o = ''
      if (present(key)) key = ''
      if (present(plain)) plain = ''
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
         if (allocated(bctx)) deallocate (bctx)
         if (nb > 0_int64) then
            allocate (btypes(0:int(nb) - 1), bctx(0:int(nb) - 1))
         else
            allocate (btypes(0:0), bctx(0:0))
         end if
         do j = 0_int64, nb - 1_int64
            bd = capnp_list_get_struct(binds, int(j), err)
            if (err /= CAPNP_OK) return
            bctx(n) = g_cur_inst
            if (binding_which(bd) == BINDING_BOUND_TYPE) then
               btypes(n) = binding_type(bd, err)
               if (err /= CAPNP_OK) return
               ! A binding that is itself a parameter of the generic being
               ! instantiated resolves through the current instantiation,
               ! so nested branded uses (Box(T) inside Nest(T)) settle. The
               ! binding it finds keeps the context it was written in.
               if (g_cur_inst > 0) then
                  if (type_which(btypes(n)) == TYPE_ANY_POINTER) then
                     if (type_anyptr_which(btypes(n)) == ANYPTR_PARAMETER) then
                        if (inst_scope_hit(g_cur_inst, type_param_scope_id(btypes(n))) .and. &
                            type_param_index(btypes(n)) < g_insts(g_cur_inst)%nbind) then
                           bctx(n) = g_insts(g_cur_inst)%bctx(type_param_index(btypes(n)))
                           btypes(n) = g_insts(g_cur_inst)%bindings(type_param_index(btypes(n)))
                        end if
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
      ol = gname
      ikey = itoa(g_nodes(gidx)%id)//'<'
      prev = g_cur_inst
      was = g_soft
      if (node_which(g_nodes(gidx)%p) == NODE_INTERFACE) g_soft = .true.
      do k = 0, n - 1
         g_cur_inst = bctx(k)
         call inst_suffix(btypes(k), part, kpart, err, mpart)
         g_cur_inst = prev
         if (err /= CAPNP_OK) then
            g_soft = was
            return
         end if
         if (k < 4) o = o//'_'//mpart
         ol = ol//'_'//part
         ikey = ikey//kpart//','
      end do
      g_soft = was
      ikey = ikey//'>'
      call register_inst(gidx, n, btypes, bctx, 0_int64, o, ol, ikey, err, pl)
      if (present(key)) key = ikey
      if (present(plain)) plain = pl
   end subroutine inst_from_brand

   !> Whether a node of this file takes the name. A struct instantiation
   !> only meets another struct: other kinds, and a group, never declare
   !> its `_t` type.
   function names_a_node(name, any_kind) result(yes)
      character(len=*), intent(in) :: name
      logical, intent(in) :: any_kind
      logical :: yes
      character(len=:), allocatable :: dn, fn
      integer :: i, err
      yes = .false.
      do i = 1, size(g_nodes)
         call node_display_name(g_nodes(i)%p, dn, err)
         if (err /= CAPNP_OK) cycle
         if (.not. (len(dn) > len(g_prefix) .and. index(dn, g_prefix//':') == 1)) cycle
         if (.not. any_kind) then
            if (node_which(g_nodes(i)%p) /= NODE_STRUCT) cycle
            if (node_struct_is_group(g_nodes(i)%p)) cycle
         end if
         call node_fname(g_nodes(i)%p, fn, err)
         if (err /= CAPNP_OK) cycle
         if (lowcase(fn) == lowcase(name)) then
            yes = .true.
            return
         end if
      end do
   end function names_a_node

   !> Register an instantiation under the name o built from its bindings.
   !> A long name truncates deterministically, a second sighting of the
   !> same instantiation finds its entry, and a different instantiation
   !> that truncates to a taken name gets a counter.
   subroutine register_inst(gidx, n, btypes, bctx, sid, o, ol, full, err, plain)
      integer, intent(in) :: gidx, n
      type(capnp_ptr_t), intent(in) :: btypes(0:)
      integer, intent(in) :: bctx(0:)
      integer(int64), intent(in) :: sid
      character(len=:), allocatable, intent(inout) :: o, ol
      character(len=*), intent(in) :: full
      integer, intent(out) :: err
      character(len=:), allocatable, intent(out), optional :: plain
      character(len=:), allocatable :: base, pl
      integer :: k, j
      logical :: fresh, iface
      err = CAPNP_OK
      o = clip_inst_name(o)
      ol = clip_inst_name(ol)
      iface = node_which(g_nodes(gidx)%p) == NODE_INTERFACE
      fresh = g_soft .or. node_which(g_nodes(gidx)%p) == NODE_INTERFACE
      do k = 1, g_ninsts
         if (g_insts(k)%gidx == gidx .and. g_insts(k)%full == full .and. &
             (g_insts(k)%fresh .eqv. fresh)) then
            o = g_insts(k)%name
            if (present(plain)) plain = g_insts(k)%plain
            return
         end if
      end do
      pl = ol
      if (fresh) then
         o = ol
         call file_tag(base)
         if (len(base) > 0) then
            o = base//'_'//o
         end if
      end if
      ! The name a generator without brand-aware names gave the
      ! instantiation is kept while it is free; a distinct instantiation
      ! that meets a taken name is spelled out in full.
      if (inst_name_taken(o, iface)) then
         if (fresh) then
            base = o
         else
            o = ol
            base = ol
         end if
         j = 1
         do while (inst_name_taken(o, iface))
            j = j + 1
            o = base//'_'//itoa(int(j, int64))
         end do
      end if
      if (g_ninsts >= size(g_insts)) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      if (present(plain)) plain = pl
      g_ninsts = g_ninsts + 1
      g_insts(g_ninsts)%gidx = gidx
      g_insts(g_ninsts)%name = o
      g_insts(g_ninsts)%full = full
      g_insts(g_ninsts)%fresh = fresh
      g_insts(g_ninsts)%plain = pl
      g_insts(g_ninsts)%nbind = n
      g_insts(g_ninsts)%inherit_scope = sid
      if (allocated(g_insts(g_ninsts)%bindings)) &
         deallocate (g_insts(g_ninsts)%bindings)
      allocate (g_insts(g_ninsts)%bindings(0:n - 1))
      if (allocated(g_insts(g_ninsts)%bctx)) deallocate (g_insts(g_ninsts)%bctx)
      allocate (g_insts(g_ninsts)%bctx(0:n - 1))
      do k = 0, n - 1
         g_insts(g_ninsts)%bindings(k) = btypes(k)
         g_insts(g_ninsts)%bctx(k) = bctx(k)
      end do
   end subroutine register_inst

   !> A name of at most 32 characters, with a content checksum for a long one
   !> so a second sighting of the same instantiation gives the same name.
   function clip_inst_name(nm) result(o)
      character(len=*), intent(in) :: nm
      character(len=:), allocatable :: o
      integer :: k, bidx
      o = nm
      if (len(o) > 32) then
         bidx = 0
         do k = 1, len(o)
            bidx = mod(bidx*31 + iachar(o(k:k)), 997)
         end do
         o = o(1:28)//'_'//itoa(int(bidx, int64))
      end if
   end function clip_inst_name

   !> Whether another instantiation or a node of this file has the name.
   function inst_name_taken(nm, iface) result(yes)
      character(len=*), intent(in) :: nm
      logical, intent(in) :: iface
      logical :: yes
      integer :: k
      yes = .false.
      do k = 1, g_ninsts
         if (g_insts(k)%name == nm) yes = .true.
      end do
      if (.not. yes) yes = names_a_node(nm, iface)
   end function inst_name_taken

   !> The module name of the file being walked without its _capnp ending.
   !> A fresh instantiation takes it as a prefix, so two files that
   !> instantiate the same generic, or a file that declares a name another
   !> file instantiates, never declare one name.
   subroutine file_tag(tag)
      character(len=:), allocatable, intent(out) :: tag
      character(len=:), allocatable :: m
      character(len=16) :: hx
      integer :: fidx, err
      tag = ''
      fidx = find_file_node(g_prefix)
      if (fidx == 0) return
      call node_module(g_nodes(fidx)%p, m, err)
      if (err /= CAPNP_OK) return
      tag = m
      if (len(tag) > 6) then
         if (tag(len(tag) - 5:) == '_capnp') tag = tag(1:len(tag) - 6)
      end if
      if (len(tag) > 12) then
         hx = lowcase(ident_hash_hex(m))
         tag = tag(1:7)//'_'//hx(1:4)
      end if
   end subroutine file_tag

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
   !> without repeating the bindings. Copy the current instantiation's
   !> bindings onto the struct so its fields resolve the same way.
   subroutine inst_inherit_brand(gidx, b, o, err)
      integer, intent(in) :: gidx
      type(capnp_ptr_t), intent(in) :: b
      character(len=:), allocatable, intent(out) :: o
      integer, intent(out) :: err
      type(capnp_ptr_t) :: scopes, sc
      type(capnp_ptr_t), allocatable :: btypes(:)
      integer, allocatable :: bctx(:)
      character(len=:), allocatable :: gname, part, kpart, ikey, ol
      integer(int64) :: i, sid
      integer :: n, k, src
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
      allocate (btypes(0:n - 1), bctx(0:n - 1))
      do k = 0, n - 1
         btypes(k) = g_insts(src)%bindings(k)
         bctx(k) = g_insts(src)%bctx(k)
      end do
      call node_fname(g_nodes(gidx)%p, gname, err)
      if (err /= CAPNP_OK) return
      o = gname
      ikey = itoa(g_nodes(gidx)%id)//'<'
      do k = 0, n - 1
         g_cur_inst = bctx(k)
         call inst_suffix(btypes(k), part, kpart, err)
         g_cur_inst = src
         if (err /= CAPNP_OK) return
         o = o//'_'//part
         ikey = ikey//kpart//','
      end do
      ikey = ikey//'>'
      ol = o
      call register_inst(gidx, n, btypes, bctx, sid, o, ol, ikey, err)
   end subroutine inst_inherit_brand

   !> Name fragment for one binding type. Lists recurse into their
   !> element so distinct list bindings get distinct instantiations.
   recursive subroutine inst_suffix(bt, part, key, err, mpart)
      type(capnp_ptr_t), intent(in) :: bt
      character(len=:), allocatable, intent(out) :: part, key
      integer, intent(out) :: err
      character(len=:), allocatable, intent(out), optional :: mpart
      type(capnp_ptr_t) :: et
      character(len=:), allocatable :: ep, ek, pp, epm
      integer :: idx
      err = CAPNP_OK
      if (present(mpart)) mpart = ''
      if (bt%kind == CAPNP_PK_NULL) then
         part = 'any'
         key = part
         if (present(mpart)) mpart = part
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
         call inst_suffix(et, ep, ek, err, epm)
         if (err /= CAPNP_OK) return
         part = 'list_'//ep
         key = 'list_'//ek
         if (present(mpart)) mpart = 'list_'//epm
         return
      case (TYPE_STRUCT, TYPE_ENUM, TYPE_INTERFACE)
         idx = find_node(type_type_id(bt))
         if (idx == 0) then
            err = CAPNP_ERR_ARG
            return
         end if
         call note_import(idx, err, .true.)
         if (err /= CAPNP_OK) return
         call node_fname(g_nodes(idx)%p, part, err)
         if (err /= CAPNP_OK) return
         key = 'n'//itoa(g_nodes(idx)%id)
         if (present(mpart)) mpart = part
         ! A branded argument is its own instantiation: its full name
         ! spells its bindings, so Box(Text) and Box(Data) differ. The
         ! short part is the node name, as the instantiation was named
         ! before.
         if (type_which(bt) /= TYPE_ENUM) then
            et = type_brand(bt, err)
            if (err /= CAPNP_OK) return
            call inst_from_brand(idx, et, ep, err, ek, pp)
            if (err /= CAPNP_OK) return
            if (len(ep) > 0) then
               part = pp
               key = ek
            end if
         end if
         return
      case default
         part = 'any'
      end select
      key = part
      if (present(mpart)) mpart = part
   end subroutine inst_suffix

   !> Handle types and size parameters for every instantiation.
   !> Struct instantiations also get the union tag constants the
   !> unbranded struct declaration emits. Interface instantiations get
   !> a client and a server, declared after the struct handles they import.
   subroutine emit_inst_decls(err)
      integer, intent(out) :: err
      integer :: k
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
   end subroutine emit_inst_decls

   !> Interface declarations of this file and of its branded instantiations.
   !> A server type extends its parent's server type, so a declaration is
   !> written after the declaration of the interface it extends.
   subroutine emit_iface_decls_ordered(prefix, err)
      character(len=*), intent(in) :: prefix
      integer, intent(out) :: err
      integer, parameter :: max_items = 512
      character(len=128), allocatable :: names(:), parents(:)
      integer, allocatable :: node_of(:), inst_of(:)
      logical, allocatable :: done(:)
      character(len=:), allocatable :: dn, tn, parent
      integer :: i, j, k, n, nsuper, left, prev
      logical :: ready, progress
      err = CAPNP_OK
      allocate (names(max_items), parents(max_items), node_of(max_items), &
                inst_of(max_items), done(max_items))
      n = 0
      do k = 1, g_ninsts
         if (node_which(g_nodes(g_insts(k)%gidx)%p) /= NODE_INTERFACE) cycle
         n = n + 1
         if (n > max_items) then
            err = CAPNP_ERR_ARG
            return
         end if
         node_of(n) = g_insts(k)%gidx
         inst_of(n) = k
      end do
      do i = 1, size(g_nodes)
         call node_display_name(g_nodes(i)%p, dn, err)
         if (err /= CAPNP_OK) return
         if (.not. (len(dn) > len(prefix) .and. index(dn, prefix//':') == 1)) cycle
         if (node_which(g_nodes(i)%p) /= NODE_INTERFACE) cycle
         n = n + 1
         if (n > max_items) then
            err = CAPNP_ERR_ARG
            return
         end if
         node_of(n) = i
         inst_of(n) = 0
      end do
      prev = g_cur_inst
      do i = 1, n
         g_cur_inst = inst_of(i)
         call iface_name(g_nodes(node_of(i))%p, tn, err)
         if (err /= CAPNP_OK) exit
         call iface_parent(g_nodes(node_of(i))%p, parent, nsuper, err)
         if (err /= CAPNP_OK) exit
         names(i) = tn
         parents(i) = parent
         done(i) = .false.
      end do
      g_cur_inst = prev
      if (err /= CAPNP_OK) return
      left = n
      do while (left > 0)
         progress = .false.
         do i = 1, n
            if (done(i)) cycle
            ready = .true.
            if (len_trim(parents(i)) > 0) then
               do j = 1, n
                  if (j == i .or. done(j)) cycle
                  if (names(j) == parents(i)) ready = .false.
               end do
            end if
            if (.not. ready) cycle
            call emit_one_iface_decl(i)
            if (err /= CAPNP_OK) return
            progress = .true.
         end do
         if (.not. progress) then
            do i = 1, n
               if (done(i)) cycle
               call emit_one_iface_decl(i)
               if (err /= CAPNP_OK) return
            end do
         end if
      end do
   contains
      subroutine emit_one_iface_decl(i)
         integer, intent(in) :: i
         g_cur_inst = inst_of(i)
         if (inst_of(i) > 0) g_has_iface = .true.
         g_soft = inst_of(i) > 0
         call emit_interface_decl(g_nodes(node_of(i))%p, err)
         g_soft = .false.
         g_cur_inst = prev
         done(i) = .true.
         left = left - 1
      end subroutine emit_one_iface_decl
   end subroutine emit_iface_decls_ordered

   !> Accessors for one instantiation: struct fields with parameters
   !> substituted, or the branded interface's client and server.
   subroutine emit_inst_procs(k, err)
      integer, intent(in) :: k
      integer, intent(out) :: err
      logical :: was
      was = g_soft
      g_soft = g_insts(k)%fresh
      call emit_inst_body(k, err)
      g_soft = was
   end subroutine emit_inst_procs

   subroutine emit_inst_body(k, err)
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
   end subroutine emit_inst_body

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
      call ref_stem(pidx, pn)
      call node_fname(g_nodes(ridx)%p, rn, err)
      if (err /= CAPNP_OK) return
      call ref_stem(ridx, rn)
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
      call note_import(sidx, err, .true.)
      if (err /= CAPNP_OK) return
      call node_fname(g_nodes(sidx)%p, pname, err)
      if (err /= CAPNP_OK) return
      call ref_stem(sidx, pname)
   end subroutine iface_parent

   !> Interface declarations: the interface id, a client handle type, an
   !> abstract server base extending rpc_server_t whose generated dispatch
   !> routes method ordinals to deferred typed procedures.
   subroutine emit_interface_decl(np, err)
      type(capnp_ptr_t), intent(in) :: np
      integer, intent(out) :: err
      type(capnp_ptr_t) :: ml, mp
      character(len=:), allocatable :: tn, mn, bn, pn, rn, parent
      integer(int64) :: i
      character(len=64), allocatable :: own(:), ex(:)
      type(nameset_t) :: seen
      integer :: nsuper, extra_ids(64), nextra, base_ids(64), nbase, n_own, nex
      logical :: has_ifc
      call iface_name(np, tn, err)
      if (err /= CAPNP_OK) return
      call iface_parent(np, parent, nsuper, err)
      if (err /= CAPNP_OK) return
      call collect_extra_supers(np, extra_ids, nextra, base_ids, nbase, err)
      if (err /= CAPNP_OK) return
      ml = node_interface_methods(np, err)
      if (err /= CAPNP_OK) return
      call server_bindings(np, tn, own, n_own, ex, nex, seen, 0, err)
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
         bn = trim(own(i + 1))
         call w('      procedure('//ident_fit(tn//'_'//bn//'_ifc')//'), deferred :: '//bn)
      end do
      call emit_further_supers(np, tn, 'd', err)
      if (err /= CAPNP_OK) return
      call w('      procedure :: dispatch => '//tn//'_dispatch')
      call w('   end type '//tn//'_server_t')
      has_ifc = capnp_list_len(ml) > 0_int64 .or. extra_supers_have_methods(extra_ids, nextra)
      if (has_ifc) then
         call w('   abstract interface')
         do i = 0_int64, capnp_list_len(ml) - 1_int64
            mp = capnp_list_get_struct(ml, int(i), err)
            if (err /= CAPNP_OK) return
            call method_name(mp, mn, err)
            if (err /= CAPNP_OK) return
            bn = trim(own(i + 1))
            call method_types(mp, pn, rn, err)
            if (err /= CAPNP_OK) return
            call emit_method_ifc(tn, bn, pn, rn)
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
      character(len=:), allocatable :: tn, mn, bn, pn, rn, parent
      integer(int64) :: i
      character(len=64), allocatable :: own(:), ex(:)
      type(nameset_t) :: seen
      integer :: nsuper, extra_ids(64), nextra, base_ids(64), nbase, n_own, nex
      call iface_name(np, tn, err)
      if (err /= CAPNP_OK) return
      call iface_parent(np, parent, nsuper, err)
      if (err /= CAPNP_OK) return
      call collect_extra_supers(np, extra_ids, nextra, base_ids, nbase, err)
      if (err /= CAPNP_OK) return
      ml = node_interface_methods(np, err)
      if (err /= CAPNP_OK) return
      call server_bindings(np, tn, own, n_own, ex, nex, seen, 0, err)
      if (err /= CAPNP_OK) return
      do i = 0_int64, capnp_list_len(ml) - 1_int64
         mp = capnp_list_get_struct(ml, int(i), err)
         if (err /= CAPNP_OK) return
         call method_name(mp, mn, err)
         if (err /= CAPNP_OK) return
         mn = snake(mn)
         call method_types(mp, pn, rn, err)
         if (err /= CAPNP_OK) return
         call w('   subroutine '//ident_fit(tn//'_'//mn//'_begin')//'(conn, client, m, params, qid, err)')
         call w('      type(rpc_conn_t), intent(inout), target :: conn')
         call w('      type('//tn//'_client_t), intent(in) :: client')
         call w('      type(capnp_message_t), intent(inout), target :: m')
         call w('      type('//pn//'_t), intent(out) :: params')
         call w('      integer(int64), intent(out) :: qid')
         call w('      integer, intent(out) :: err')
         call w('      type('//rpcn('payload_t')//') :: pl')
         call w('      call rpc_call_begin(conn, client%cap, '//upcase(tn)// &
                '_INTERFACE_ID, '//itoa(i)//', m, pl, qid, err)')
         call w('      if (err /= CAPNP_OK) return')
         call w('      params%p = capnp_new_struct(m, '//upcase(pn)//'_DWORDS, '// &
                upcase(pn)//'_PWORDS, err)')
         call w('      if (err == CAPNP_OK) call '//rpcn('payload_content_set')//'(pl, params%p, err)')
         call w('   end subroutine '//ident_fit(tn//'_'//mn//'_begin'))
         call w('')
         call w('   subroutine '//ident_fit(tn//'_'//mn//'_wait')//'(conn, qid, results, err)')
         call w('      type(rpc_conn_t), intent(inout), target :: conn')
         call w('      integer(int64), intent(in) :: qid')
         call w('      type('//rn//'_t), intent(out) :: results')
         call w('      integer, intent(out) :: err')
         call w('      call rpc_wait(conn, qid, err)')
         call w('      if (err /= CAPNP_OK) return')
         call w('      call rpc_result_content(conn, qid, results%p, err)')
         call w('   end subroutine '//ident_fit(tn//'_'//mn//'_wait'))
         call w('')
         if (method_result_struct_type(mp) == STREAM_RESULT_ID) then
            ! schema.h isStreaming: the result type is StreamResult.
            ! capnpc-c++.c++ emits StreamingRequest / newStreamingCall.
            call w('   subroutine '//ident_fit(tn//'_'//mn//'_stream')//'(conn, stream, m, qid, err)')
            call w('      type(rpc_conn_t), intent(inout), target :: conn')
            call w('      type(rpc_stream_t), intent(inout) :: stream')
            call w('      type(capnp_message_t), intent(inout) :: m')
            call w('      integer(int64), intent(in) :: qid')
            call w('      integer, intent(out) :: err')
            call w('      call rpc_stream_send(conn, stream, m, qid, err)')
            call w('   end subroutine '//ident_fit(tn//'_'//mn//'_stream'))
            call w('')
         end if
      end do
      call w('   subroutine '//tn//'_dispatch(self, ctx, err)')
      call w('      class('//tn//'_server_t), intent(inout) :: self')
      call w('      type(rpc_call_ctx_t), intent(inout) :: ctx')
      call w('      integer, intent(out) :: err')
      call w('      err = CAPNP_ERR_ARG')
      if (nextra > 0) then
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
         bn = trim(own(i + 1))
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
         call w('            call self%'//bn//'(ctx, params, results, err)')
         call w('            if (err == CAPNP_OK) call '//rpcn('payload_content_set')//'(ctx%results, results%p, err)')
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
      character(len=:), allocatable :: ifcn
      ifcn = ident_fit(tn//'_'//mn//'_ifc')
      call w('      subroutine '//ifcn//'(self, ctx, params, results, err)')
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
      call w('      end subroutine '//ifcn)
   end subroutine emit_method_ifc

   !> Methods of superclasses after the first. The first is inherited by
   !> extends; further ones are copied because Fortran has one parent.
   !> Each (interface_id, method_id) keeps its own param/result types
   !> (capnpc-c++.c++ dispatchCall); colliding Fortran binding names are
   !> qualified with the declaring interface name.
   !> mode d = deferred binding, a = abstract interface, c = dispatch.
   subroutine emit_further_supers(np, tn, mode, err)
      type(capnp_ptr_t), intent(in) :: np
      character(len=*), intent(in) :: tn
      character(len=1), intent(in) :: mode
      integer, intent(out) :: err
      logical :: was
      was = g_soft
      g_soft = .true.
      call emit_further_supers_body(np, tn, mode, err)
      g_soft = was
   end subroutine emit_further_supers

   subroutine emit_further_supers_body(np, tn, mode, err)
      type(capnp_ptr_t), intent(in) :: np
      character(len=*), intent(in) :: tn
      character(len=1), intent(in) :: mode
      integer, intent(out) :: err
      type(capnp_ptr_t) :: ml, mp
      character(len=:), allocatable :: sn, mn, bn, pn, rn
      character(len=64), allocatable :: own(:), ex(:)
      type(nameset_t) :: seen
      integer :: extra_ids(64), nextra, base_ids(64), nbase, n_own, nex
      integer :: e, sidx, k
      integer(int64) :: i
      err = CAPNP_OK
      call collect_extra_supers(np, extra_ids, nextra, base_ids, nbase, err)
      if (err /= CAPNP_OK .or. nextra == 0) return
      call server_bindings(np, tn, own, n_own, ex, nex, seen, 0, err)
      if (err /= CAPNP_OK) return
      k = 0
      do e = 1, nextra
         sidx = extra_ids(e)
         call note_import(sidx, err)
         if (err /= CAPNP_OK) return
         call node_fname(g_nodes(sidx)%p, sn, err)
         if (err /= CAPNP_OK) return
         call ref_stem(sidx, sn)
         ml = node_interface_methods(g_nodes(sidx)%p, err)
         if (err /= CAPNP_OK) return
         if (mode == 'c') then
            call w('      if (ctx%interface_id == '//upcase(sn)//'_INTERFACE_ID) then')
            call w('         select case (ctx%method_id)')
         end if
         do i = 0_int64, capnp_list_len(ml) - 1_int64
            mp = capnp_list_get_struct(ml, int(i), err)
            if (err /= CAPNP_OK) return
            k = k + 1
            bn = trim(ex(k))
            call method_types(mp, pn, rn, err)
            if (err /= CAPNP_OK) return
            select case (mode)
            case ('d')
               call w('      procedure('//ident_fit(tn//'_'//bn//'_ifc')//'), deferred :: '//bn)
            case ('a')
               call emit_method_ifc(tn, bn, pn, rn)
            case ('c')
               call w('         case ('//itoa(i)//')')
               call w('            block')
               call w('               type('//pn//'_t) :: params')
               call w('               type('//rn//'_t) :: results')
               call w('               params%p = ctx%params')
               call w('               results%p = capnp_new_struct(ctx%rmsg, '// &
                      upcase(rn)//'_DWORDS, '//upcase(rn)//'_PWORDS, err)')
               call w('               if (err /= CAPNP_OK) return')
               call w('               call self%'//bn//'(ctx, params, results, err)')
               call w('               if (err == CAPNP_OK) &')
               call w('                  call '//rpcn('payload_content_set')//'(ctx%results, results%p, err)')
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
   end subroutine emit_further_supers_body

   function ns_has(set, name) result(yes)
      type(nameset_t), intent(in) :: set
      character(len=*), intent(in) :: name
      logical :: yes
      character(len=:), allocatable :: key
      integer :: k
      yes = .false.
      key = lowcase(name)
      do k = 1, set%n
         if (trim(set%v(k)) == key) then
            yes = .true.
            return
         end if
      end do
   end function ns_has

   subroutine ns_add(set, name)
      type(nameset_t), intent(inout) :: set
      character(len=*), intent(in) :: name
      character(len=64), allocatable :: tmp(:)
      if (ns_has(set, name)) return
      if (.not. allocated(set%v)) allocate (set%v(64))
      if (set%n == size(set%v)) then
         allocate (tmp(2*size(set%v)))
         tmp(1:set%n) = set%v(1:set%n)
         call move_alloc(tmp, set%v)
      end if
      set%n = set%n + 1
      set%v(set%n) = lowcase(name)
   end subroutine ns_add

   !> The Fortran binding of every method of a server type, in the order
   !> the emitters walk them: own lists one name per method of np, ex one
   !> per method of the further superclasses. A binding name must differ
   !> from every name the type inherits or binds itself (not the schema
   !> name: a method renamed in a parent is inherited under its new name).
   !> Names the type already has are qualified with the interface name,
   !> and an ordinal when that still collides. seen ends up holding every
   !> binding of the type, inherited ones included.
   recursive subroutine server_bindings(np, tn, own, n_own, ex, nex, seen, depth, err)
      type(capnp_ptr_t), intent(in) :: np
      character(len=*), intent(in) :: tn
      character(len=64), allocatable, intent(out) :: own(:), ex(:)
      integer, intent(out) :: n_own, nex
      type(nameset_t), intent(inout) :: seen
      integer, intent(in) :: depth
      integer, intent(out) :: err
      type(capnp_ptr_t) :: sl, ml, mp
      character(len=64), allocatable :: raw(:), o2(:), e2(:)
      character(len=:), allocatable :: mn, sn, bn
      integer :: extra_ids(64), nextra, base_ids(64), nbase, n2, ne2
      integer :: fidx, e, k, j, n
      integer(int64) :: i
      err = CAPNP_OK
      n_own = 0
      nex = 0
      seen%n = 0
      sl = node_interface_superclasses(np, err)
      if (err /= CAPNP_OK) return
      fidx = find_first_super(sl)
      if (fidx > 0 .and. depth < 32) then
         call node_fname(g_nodes(fidx)%p, sn, err)
         if (err /= CAPNP_OK) return
         call server_bindings(g_nodes(fidx)%p, sn, o2, n2, e2, ne2, seen, depth + 1, err)
         if (err /= CAPNP_OK) return
      end if
      ml = node_interface_methods(np, err)
      if (err /= CAPNP_OK) return
      n = int(capnp_list_len(ml))
      allocate (raw(max(n, 1)), own(max(n, 1)))
      do i = 0_int64, int(n, int64) - 1_int64
         mp = capnp_list_get_struct(ml, int(i), err)
         if (err /= CAPNP_OK) return
         call method_name(mp, mn, err)
         if (err /= CAPNP_OK) return
         raw(i + 1) = ident_fit(snake(mn))
      end do
      do i = 0_int64, int(n, int64) - 1_int64
         mn = trim(raw(i + 1))
         bn = mn
         if (bn == 'dispatch' .or. ns_has(seen, bn)) then
            bn = ident_fit(tn//'_'//mn)
            k = 0
            do while (bn == 'dispatch' .or. ns_has(seen, bn) .or. is_raw(bn))
               k = k + 1
               bn = ident_fit(tn//'_'//mn//'_'//itoa(i))
               if (k > 1) bn = ident_fit(tn//'_'//mn//'_'//itoa(i)//'_'//itoa(int(k, int64)))
            end do
         end if
         own(i + 1) = bn
         call ns_add(seen, bn)
      end do
      n_own = n
      call collect_extra_supers(np, extra_ids, nextra, base_ids, nbase, err)
      if (err /= CAPNP_OK) return
      allocate (ex(64))
      do e = 1, nextra
         call node_fname(g_nodes(extra_ids(e))%p, sn, err)
         if (err /= CAPNP_OK) return
         ml = node_interface_methods(g_nodes(extra_ids(e))%p, err)
         if (err /= CAPNP_OK) return
         do i = 0_int64, capnp_list_len(ml) - 1_int64
            mp = capnp_list_get_struct(ml, int(i), err)
            if (err /= CAPNP_OK) return
            call method_name(mp, mn, err)
            if (err /= CAPNP_OK) return
            mn = snake(mn)
            bn = ident_fit(mn)
            if (bn == 'dispatch' .or. ns_has(seen, bn)) then
               bn = ident_fit(sn//'_'//mn)
               if (bn == 'dispatch' .or. ns_has(seen, bn)) then
                  bn = ident_fit(sn//'_'//mn//'_'//itoa(i))
                  k = 1
                  do while (ns_has(seen, bn))
                     k = k + 1
                     bn = ident_fit(sn//'_'//mn//'_'//itoa(i)//'_'//itoa(int(k, int64)))
                  end do
               end if
            end if
            nex = nex + 1
            if (nex > size(ex)) call grow_names(ex)
            ex(nex) = bn
            call ns_add(seen, bn)
         end do
      end do
   contains
      logical function is_raw(name)
         character(len=*), intent(in) :: name
         is_raw = .false.
         do j = 1, n
            if (trim(raw(j)) == name) is_raw = .true.
         end do
      end function is_raw
   end subroutine server_bindings

   subroutine grow_names(v)
      character(len=64), allocatable, intent(inout) :: v(:)
      character(len=64), allocatable :: tmp(:)
      allocate (tmp(2*size(v)))
      tmp(1:size(v)) = v
      call move_alloc(tmp, v)
   end subroutine grow_names

   recursive subroutine collect_super_closure(idx, ids, n, excl, nexcl, depth)
      integer, intent(in) :: idx, excl(:), nexcl, depth
      integer, intent(inout) :: ids(:), n
      type(capnp_ptr_t) :: sl, sc
      integer :: err, k, sidx
      integer(int64) :: s
      if (idx <= 0 .or. depth > 32) return
      do k = 1, nexcl
         if (excl(k) == idx) return
      end do
      do k = 1, n
         if (ids(k) == idx) return
      end do
      if (n < size(ids)) then
         n = n + 1
         ids(n) = idx
      end if
      sl = node_interface_superclasses(g_nodes(idx)%p, err)
      if (err /= CAPNP_OK) return
      do s = 0_int64, capnp_list_len(sl) - 1_int64
         sc = capnp_list_get_struct(sl, int(s), err)
         if (err /= CAPNP_OK) return
         sidx = find_node(superclass_id(sc))
         if (sidx > 0) call collect_super_closure(sidx, ids, n, excl, nexcl, depth + 1)
      end do
   end subroutine collect_super_closure

   subroutine collect_extra_supers(np, extra_ids, nextra, base_ids, nbase, err)
      type(capnp_ptr_t), intent(in) :: np
      integer, intent(out) :: extra_ids(:), nextra, base_ids(:), nbase, err
      type(capnp_ptr_t) :: sl, sc
      integer :: fidx, sidx, empty(1)
      integer(int64) :: s
      err = CAPNP_OK
      nextra = 0
      nbase = 0
      empty = 0
      sl = node_interface_superclasses(np, err)
      if (err /= CAPNP_OK) return
      if (capnp_list_len(sl) < 1_int64) return
      fidx = find_first_super(sl)
      if (fidx > 0) call collect_super_closure(fidx, base_ids, nbase, empty, 0, 0)
      do s = 1_int64, capnp_list_len(sl) - 1_int64
         sc = capnp_list_get_struct(sl, int(s), err)
         if (err /= CAPNP_OK) return
         sidx = find_node(superclass_id(sc))
         if (sidx > 0) call collect_super_closure(sidx, extra_ids, nextra, base_ids, nbase, 0)
      end do
   end subroutine collect_extra_supers

   function extra_supers_have_methods(extra_ids, nextra) result(yes)
      integer, intent(in) :: extra_ids(:), nextra
      logical :: yes
      type(capnp_ptr_t) :: ml
      integer :: e, err
      yes = .false.
      do e = 1, nextra
         ml = node_interface_methods(g_nodes(extra_ids(e))%p, err)
         if (err == CAPNP_OK .and. capnp_list_len(ml) > 0_int64) then
            yes = .true.
            return
         end if
      end do
   end function extra_supers_have_methods

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

   !> The any-pointer wrapper types live in the runtime, so schemas that
   !> import each other never declare them twice. The resolver helpers stay
   !> private for the same reason.
   subroutine emit_any_decls()
      if (g_has_iface .and. .not. g_probe) call w('   private :: '//trim(g_cap_resolve))
      if (g_need_capability .and. .not. g_probe) call w('   private :: '//trim(g_cap_resolve_id))
      if (.not. g_probe) call emit_rpc_private()
      if ((g_has_iface .or. g_need_capability) .and. .not. g_probe) call w('')
   end subroutine emit_any_decls

   subroutine emit_annotation_surface(modname, fidx, err)
      character(len=*), intent(in) :: modname
      integer, intent(in) :: fidx
      integer, intent(out) :: err
      character(len=:), allocatable :: dn, owner, ftag
      integer :: i
      logical :: mine
      err = CAPNP_OK
      g_ann_used%n = 0
      call file_tag(ftag)
      g_ann_tag = ftag
      g_ann_mod = modname
      do i = 1, size(g_nodes)
         call node_display_name(g_nodes(i)%p, dn, err)
         if (err /= CAPNP_OK) return
         mine = len(dn) > len(g_prefix) .and. index(dn, g_prefix//':') == 1
         if (.not. mine) cycle
         call node_fname(g_nodes(i)%p, owner, err)
         if (err /= CAPNP_OK) return
         if (node_which(g_nodes(i)%p) == NODE_ANNOTATION) then
            call w('   integer(int64), parameter :: '// &
                   unique_const(ident_fit(upcase(ftag)//'_'//upcase(owner)//'_ID'), &
                                node_id(g_nodes(i)%p))//' = '// &
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

   !> An annotation constant named after its owner and annotation can meet
   !> another name of the module: a union tag, an accessor, a nested type or
   !> constant, or the constant of a different annotation or owner that
   !> spells the same. The constant keeps the plain name unless the module
   !> already has it. Then it takes the annotation's id as a suffix, and a
   !> counter if that is taken too. The walk is deterministic, so every
   !> pass picks the same name.
   function unique_const(cand, id) result(o)
      character(len=*), intent(in) :: cand
      integer(int64), intent(in) :: id
      character(len=:), allocatable :: o
      character(len=16) :: hx
      integer :: k
      o = cand
      if (.not. const_taken(o)) then
         call ns_add(g_ann_used, o)
         return
      end if
      write (hx, '(z16.16)') id
      hx = lowcase(hx)
      o = ident_fit(cand//'_'//hx(9:16))
      k = 1
      do while (const_taken(o))
         k = k + 1
         o = ident_fit(cand//'_'//hx(9:16)//'_'//itoa(int(k, int64)))
      end do
      call ns_add(g_ann_used, o)
   end function unique_const

   function const_taken(name) result(yes)
      character(len=*), intent(in) :: name
      logical :: yes
      yes = ns_has(g_reserved, name) .or. ns_has(g_ann_used, name)
   end function const_taken

   !> File stem of the schema that declares annotation node np, as
   !> identifier characters. It tells apart annotations that share a name.
   subroutine annotation_scope_tag(np, tag, err)
      type(capnp_ptr_t), intent(in) :: np
      character(len=:), allocatable, intent(out) :: tag
      integer, intent(out) :: err
      character(len=:), allocatable :: dn
      integer :: i, b, e
      call node_display_name(np, dn, err)
      if (err /= CAPNP_OK) return
      e = index(dn, ':') - 1
      if (e < 0) e = len(dn)
      dn = dn(1:e)
      b = index(dn, '/', back=.true.) + 1
      e = index(dn(b:), '.', back=.true.)
      if (e > 0) dn = dn(b:b + e - 2)
      if (e == 0) dn = dn(b:)
      tag = ''
      do i = 1, len(dn)
         if ((dn(i:i) >= 'a' .and. dn(i:i) <= 'z') .or. &
             (dn(i:i) >= 'A' .and. dn(i:i) <= 'Z') .or. &
             (dn(i:i) >= '0' .and. dn(i:i) <= '9') .or. dn(i:i) == '_') then
            tag = tag//dn(i:i)
         else if (dn(i:i) == '+') then
            tag = tag//'x'
         end if
      end do
      if (len(tag) == 0) tag = 'file'
   end subroutine annotation_scope_tag

   subroutine emit_ann_list(owner, al, err)
      character(len=*), intent(in) :: owner
      type(capnp_ptr_t), intent(in) :: al
      integer, intent(out) :: err
      type(capnp_ptr_t) :: a, v
      character(len=:), allocatable :: aname, cname, s, tag, pre
      character(len=64), allocatable :: names(:)
      integer, allocatable :: aidxs(:)
      integer(int8), allocatable :: db(:)
      integer :: aidx, n, j
      integer(int64) :: i
      err = CAPNP_OK
      n = int(capnp_list_len(al))
      allocate (names(max(n, 1)), aidxs(max(n, 1)))
      do i = 0_int64, int(n, int64) - 1_int64
         a = capnp_list_get_struct(al, int(i), err)
         if (err /= CAPNP_OK) return
         aidxs(i + 1) = find_node(annotation_id(a))
         names(i + 1) = ''
         if (aidxs(i + 1) == 0) cycle
         call node_fname(g_nodes(aidxs(i + 1))%p, aname, err)
         if (err /= CAPNP_OK) return
         names(i + 1) = aname
      end do
      do i = 0_int64, int(n, int64) - 1_int64
         a = capnp_list_get_struct(al, int(i), err)
         if (err /= CAPNP_OK) return
         aidx = aidxs(i + 1)
         if (aidx == 0) cycle
         aname = trim(names(i + 1))
         do j = 1, n
            if (j == i + 1) cycle
            if (aidxs(j) == 0) cycle
            if (names(j) /= names(i + 1)) cycle
            call annotation_scope_tag(g_nodes(aidx)%p, tag, err)
            if (err /= CAPNP_OK) return
            aname = tag//'_'//aname
            exit
         end do
         v = annotation_value(a, err)
         if (err /= CAPNP_OK) return
         pre = ''
         if (owner /= trim(g_ann_mod) .and. len_trim(g_ann_tag) > 0) &
            pre = upcase(trim(g_ann_tag))//'_'
         cname = unique_const(ident_fit(pre//upcase(owner)//'_'//upcase(aname)), annotation_id(a))
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
            pre = '   character(len=*), parameter :: '//cname//' = '
            call w(pre//ftext(s, len(pre)))
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
      call w('   subroutine '//trim(g_cap_resolve)//'(ctab, idx, cap, err)')
      call w('      type(capnp_ptr_t), intent(in) :: ctab')
      call w('      integer(int64), intent(in) :: idx')
      call w('      type(rpc_cap_t), intent(out) :: cap')
      call w('      integer, intent(out) :: err')
      call w('      type('//rpcn('cap_descriptor_t')//') :: cd')
      call w('      cap%kind = RPC_CAP_NONE')
      call w('      cap%id = 0_int64')
      call w('      err = CAPNP_OK')
      call w('      if (idx < 0_int64 .or. idx >= capnp_list_len(ctab)) then')
      call w('         err = CAPNP_ERR_BOUNDS')
      call w('         return')
      call w('      end if')
      call w('      cd%p = capnp_list_get_struct(ctab, int(idx), err)')
      call w('      if (err /= CAPNP_OK) return')
      call w('      select case ('//rpcn('cap_descriptor_which')//'(cd))')
      call w('      case ('//rpcn('cap_descriptor_none_tag')//')')
      call w('         cap%kind = RPC_CAP_NONE')
      call w('      case ('//rpcn('cap_descriptor_sender_hosted_tag')//')')
      call w('         cap%kind = RPC_CAP_IMPORT')
      call w('         cap%id = '//rpcn('cap_descriptor_sender_hosted_get')//'(cd)')
      call w('      case ('//rpcn('cap_descriptor_sender_promise_tag')//')')
      call w('         cap%kind = RPC_CAP_IMPORT')
      call w('         cap%id = '//rpcn('cap_descriptor_sender_promise_get')//'(cd)')
      call w('      case ('//rpcn('cap_descriptor_receiver_hosted_tag')//')')
      call w('         cap%kind = RPC_CAP_IMPORT')
      call w('         cap%id = '//rpcn('cap_descriptor_receiver_hosted_get')//'(cd)')
      call w('      case default')
      call w('         err = CAPNP_ERR_KIND')
      call w('      end select')
      call w('   end subroutine '//trim(g_cap_resolve))
      call w('')
   end subroutine emit_cap_resolve

   !> Same table walk as cap_resolve, writing kind and id without rpc_cap_t.
   !> kind 0 is none and kind 1 is an import (RPC_CAP_NONE / RPC_CAP_IMPORT).
   subroutine emit_cap_resolve_id()
      call w('   subroutine '//trim(g_cap_resolve_id)//'(ctab, idx, kind, id, err)')
      call w('      type(capnp_ptr_t), intent(in) :: ctab')
      call w('      integer(int64), intent(in) :: idx')
      call w('      integer, intent(out) :: kind')
      call w('      integer(int64), intent(out) :: id')
      call w('      integer, intent(out) :: err')
      call w('      type('//rpcn('cap_descriptor_t')//') :: cd')
      call w('      kind = 0')
      call w('      id = 0_int64')
      call w('      err = CAPNP_OK')
      call w('      if (idx < 0_int64 .or. idx >= capnp_list_len(ctab)) then')
      call w('         err = CAPNP_ERR_BOUNDS')
      call w('         return')
      call w('      end if')
      call w('      cd%p = capnp_list_get_struct(ctab, int(idx), err)')
      call w('      if (err /= CAPNP_OK) return')
      call w('      select case ('//rpcn('cap_descriptor_which')//'(cd))')
      call w('      case ('//rpcn('cap_descriptor_none_tag')//')')
      call w('         kind = 0')
      call w('      case ('//rpcn('cap_descriptor_sender_hosted_tag')//')')
      call w('         kind = 1')
      call w('         id = '//rpcn('cap_descriptor_sender_hosted_get')//'(cd)')
      call w('      case ('//rpcn('cap_descriptor_sender_promise_tag')//')')
      call w('         kind = 1')
      call w('         id = '//rpcn('cap_descriptor_sender_promise_get')//'(cd)')
      call w('      case ('//rpcn('cap_descriptor_receiver_hosted_tag')//')')
      call w('         kind = 1')
      call w('         id = '//rpcn('cap_descriptor_receiver_hosted_get')//'(cd)')
      call w('      case default')
      call w('         err = CAPNP_ERR_KIND')
      call w('      end select')
      call w('   end subroutine '//trim(g_cap_resolve_id))
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
