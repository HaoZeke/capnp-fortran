!> JSON codec with the defaults and annotations of capnp/compat/json.h.
!>
!> Primitives are always encoded. A null pointer outside a union is
!> omitted. The active union arm is still written when its pointer is
!> null, using the schema default for text and data, or empty when the
!> arm has none. Int64 and UInt64 are decimal strings. Data is an array
!> of byte values unless the field carries $Json.base64 or $Json.hex.
!> Enums are names, or the raw number when the ordinal is unknown.
!> $Json.name renames fields, groups, unions, and enumerants.
!> $Json.flatten inlines sub-structs, groups, and discriminated union
!> variants with an optional prefix. $Json.discriminator tags unions
!> with an explicit variant field and optional valueName. An unknown
!> discriminator string does not select an arm. A known tag clears the
!> arm it selects, so a member written earlier does not survive. Plain
!> member keys in that object are still applied in document order. A
!> repeated known tag is rejected, and a void arm accepts any payload.
!> A non-string discriminator is rejected. NaN and infinities are the
!> strings "NaN", "Infinity", and "-Infinity". A high surrogate followed by a low surrogate is one
!> scalar, and an unquoted integer past 2^53 keeps its exact value.
!> These two reads differ from capnp 1.4.0 only by adding precision or
!> correctness, and both accept every input that version accepts.
module capnp_json
   use capnp
   use capnp_schema
   use capnp_codec
   use capnp_dynamic, only: capnp_dyn_schema_t
   use, intrinsic :: ieee_arithmetic, only: ieee_is_nan, ieee_is_finite
   implicit none
   private

   public :: capnp_json_encode, capnp_json_decode, capnp_json_span

   integer(int64), parameter :: ANN_NAME = int(z'FA5B1FD61C2E7C3D', int64)
   integer(int64), parameter :: ANN_FLATTEN = int(z'82D3E852AF0336BF', int64)
   integer(int64), parameter :: ANN_DISCRIMINATOR = int(z'CFA794E8D19A0162', int64)
   integer(int64), parameter :: ANN_B64 = int(z'D7D879450A253E4B', int64)
   integer(int64), parameter :: ANN_HEX = int(z'F061E22F0AE5C7B5', int64)
   integer, parameter :: MAX_DEPTH = 64
   integer, parameter :: UNION_SEEN_MAX = 64
   integer, save :: union_seen_n = 0
   integer, save :: union_seen_seg(UNION_SEEN_MAX)
   integer(int64), save :: union_seen_off(UNION_SEEN_MAX)
   integer, save :: union_seen_disc(UNION_SEEN_MAX)
   integer, parameter :: JN_TAG = 1, JN_VALUE = 2, JN_NORMAL = 3
   integer, parameter :: JN_FLAT = 4, JN_FLAT_U = 5
   integer, parameter :: JK_NULL = 0, JK_BOOL = 1, JK_NUM = 2, JK_STR = 3
   integer, parameter :: JK_ARR = 4, JK_OBJ = 5
   character(len=*), parameter :: B64 = &
      'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'

   type :: piece_t
      character(len=:), allocatable :: s
      logical :: multi = .false.
   end type piece_t

   type :: buf_t
      character(len=:), allocatable :: s
      integer :: n = 0
   end type buf_t

   type :: jname_t
      character(len=:), allocatable :: key
      integer :: kind = 0
   end type jname_t

   type :: disc_opts_t
      logical :: active = .false.
      logical :: has_name = .false.
      character(len=:), allocatable :: name
      logical :: has_val = .false.
      character(len=:), allocatable :: val_name
   end type disc_opts_t

   type :: jval
      integer :: k = JK_NULL
      logical :: b = .false.
      real(real64) :: num = 0.0_real64
      character(len=:), allocatable :: str
      character(len=:), allocatable :: key
      type(jval), allocatable :: kids(:)
   end type jval

   type :: scan_t
      character(len=:), allocatable :: s
      integer :: i = 1
      integer :: n = 0
   end type scan_t

contains

   subroutine capnp_json_encode(schema, idx, p, text, err, pretty, brand_env, &
                                 traversal_words)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      type(capnp_ptr_t), intent(in) :: p
      character(len=:), allocatable, intent(out) :: text
      integer, intent(out) :: err
      logical, intent(in), optional :: pretty
      type(capnp_brand_env_t), intent(in), optional :: brand_env
      integer(int64), intent(in), optional :: traversal_words
      type(capnp_brand_env_t) :: env
      type(disc_opts_t) :: no_disc
      type(jval) :: root
      logical :: do_pretty, multi
      integer(int64) :: limit
      err = CAPNP_OK
      text = ''
      if (.not. schema%loaded) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (idx < 1 .or. idx > size(schema%nodes)) then
         err = CAPNP_ERR_ARG
         return
      end if
      call audit_json(schema, idx, err)
      if (err /= CAPNP_OK) return
      limit = CAPNP_DEFAULT_TRAVERSAL_WORDS
      if (present(traversal_words)) limit = traversal_words
      call guard_traversal(p, limit, err)
      if (err /= CAPNP_OK) return
      if (node_which(schema%nodes(idx)) /= NODE_STRUCT) then
         err = CAPNP_ERR_ARG
         return
      end if
      do_pretty = .true.
      if (present(pretty)) do_pretty = pretty
      if (present(brand_env)) env = brand_env
      call build_struct(schema, idx, p, env, no_disc, 0, root, err)
      if (err /= CAPNP_OK) return
      call emit_jval(root, 0, .false., do_pretty, text, multi, err)
   end subroutine capnp_json_encode

   subroutine capnp_json_decode(schema, idx, text, msg, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      character(len=*), intent(in) :: text
      type(capnp_message_t), intent(inout), target :: msg
      integer, intent(out) :: err
      type(capnp_brand_env_t) :: env
      type(disc_opts_t) :: no_disc
      type(jval) :: root
      type(capnp_ptr_t) :: obj
      type(scan_t) :: sc
      integer :: dw, pw
      err = CAPNP_OK
      if (.not. schema%loaded) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (idx < 1 .or. idx > size(schema%nodes)) then
         err = CAPNP_ERR_ARG
         return
      end if
      call audit_json(schema, idx, err)
      if (err /= CAPNP_OK) return
      if (node_which(schema%nodes(idx)) /= NODE_STRUCT) then
         err = CAPNP_ERR_ARG
         return
      end if
      sc%s = text
      sc%n = len(text)
      sc%i = 1
      call parse_value(sc, root, 0, err)
      if (err /= CAPNP_OK) return
      call jskip(sc)
      if (sc%i <= sc%n) then
         err = CAPNP_ERR_ARG
         return
      end if
      call capnp_message_free(msg)
      call capnp_message_init_builder(msg, err)
      if (err /= CAPNP_OK) return
      dw = node_struct_data_words(schema%nodes(idx))
      pw = node_struct_pointer_count(schema%nodes(idx))
      obj = capnp_new_struct(msg, dw, pw, err)
      if (err /= CAPNP_OK) return
      call capnp_set_root(msg, obj, err)
      if (err /= CAPNP_OK) return
      call union_seen_reset()
      call fill_struct(schema, idx, obj, env, no_disc, root, 0, err)
   end subroutine capnp_json_decode

   !> Locate one top-level JSON value. beg = 0 and err = 0 means no more input.
   subroutine capnp_json_span(text, from, beg, to, err)
      character(len=*), intent(in) :: text
      integer, intent(in) :: from
      integer, intent(out) :: beg, to
      integer, intent(out) :: err
      type(scan_t) :: sc
      type(jval) :: dummy
      err = CAPNP_OK
      beg = 0
      to = 0
      sc%s = text
      sc%n = len(text)
      sc%i = max(from, 1)
      call jskip(sc)
      if (sc%i > sc%n) return
      beg = sc%i
      call parse_value(sc, dummy, 0, err)
      if (err == CAPNP_OK) to = sc%i - 1
   end subroutine capnp_json_span

   recursive subroutine build_struct(schema, idx, p, env, parent_disc, depth, jv, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx, depth
      type(capnp_ptr_t), intent(in) :: p
      type(capnp_brand_env_t), intent(in) :: env
      type(disc_opts_t), intent(in) :: parent_disc
      type(jval), intent(out) :: jv
      integer, intent(out) :: err
      integer :: n
      err = CAPNP_OK
      jv%k = JK_OBJ
      n = 0
      call append_struct_fields(schema, idx, p, env, '', parent_disc, '', depth, jv%kids, n, err)
      if (err /= CAPNP_OK) return
      call trim_kids(jv%kids, n, err)
      if (err /= CAPNP_OK) return
   end subroutine build_struct

   recursive subroutine append_struct_fields(schema, idx, p, env, pfx, parent_disc, union_decl, &
                                             depth, kids, n, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx, depth
      type(capnp_ptr_t), intent(in) :: p
      type(capnp_brand_env_t), intent(in) :: env
      character(len=*), intent(in) :: pfx, union_decl
      type(disc_opts_t), intent(in) :: parent_disc
      type(jval), allocatable, intent(inout) :: kids(:)
      integer, intent(inout) :: n
      integer, intent(out) :: err
      type(capnp_ptr_t) :: fl, f, al, raw_t, t, q, dv
      type(capnp_brand_env_t) :: cenv
      type(disc_opts_t) :: opts, gdisc, no_disc
      type(jval) :: one, dtag
      character(len=:), allocatable :: nm, raw_nm, fpfx, dkey, vkey
      integer :: i, nf, active, fd, gidx, sidx
      logical :: show, is_flat
      err = CAPNP_OK
      if (depth > MAX_DEPTH) then
         err = CAPNP_ERR_DEPTH
         return
      end if
      if (idx < 1 .or. idx > size(schema%nodes)) then
         err = CAPNP_ERR_ARG
         return
      end if
      fl = node_struct_fields(schema%nodes(idx), err)
      if (err /= CAPNP_OK) return
      nf = int(capnp_list_len(fl))
      do i = 0, nf - 1
         f = capnp_list_get_struct(fl, i, err)
         if (err /= CAPNP_OK) return
         if (field_discriminant(f) /= NO_DISCRIMINANT) cycle
         call flatten_opts(f, is_flat, fpfx, err)
         if (err /= CAPNP_OK) return
         if (is_flat) then
            if (field_which(f) == FIELD_GROUP) then
               gidx = capnp_codec_find_id(schema, field_group_type_id(f))
               if (gidx == 0) then
                  err = CAPNP_ERR_ARG
                  return
               end if
               call field_name(f, raw_nm, err)
               if (err /= CAPNP_OK) return
               al = field_annotations(f, err)
               if (err /= CAPNP_OK) return
               call read_disc_opts(al, gdisc, err)
               if (err /= CAPNP_OK) return
               call append_struct_fields(schema, gidx, p, env, pfx//fpfx, gdisc, raw_nm, &
                                         depth + 1, kids, n, err)
               if (err /= CAPNP_OK) return
            else
               show = capnp_codec_field_present(f, p, err)
               if (err /= CAPNP_OK) return
               if (.not. show) cycle
               raw_t = field_slot_type(f, err)
               if (err /= CAPNP_OK) return
               t = capnp_brand_resolve(env, raw_t)
               if (type_which(t) /= TYPE_STRUCT) then
                  err = CAPNP_ERR_KIND
                  return
               end if
               q = capnp_getp(p, int(field_slot_offset(f)), err)
               if (err /= CAPNP_OK) return
               sidx = capnp_codec_find_id(schema, type_type_id(t))
               if (sidx == 0) then
                  err = CAPNP_ERR_ARG
                  return
               end if
               call capnp_brand_enter(env, t, cenv, err)
               if (err /= CAPNP_OK) return
               call append_struct_fields(schema, sidx, q, cenv, pfx//fpfx, no_disc, '', &
                                         depth + 1, kids, n, err)
               if (err /= CAPNP_OK) return
            end if
         else
            show = capnp_codec_field_present(f, p, err)
            if (err /= CAPNP_OK) return
            if (.not. show) cycle
            call json_field_name(f, nm, err)
            if (err /= CAPNP_OK) return
            call build_field_value(schema, f, p, env, depth + 1, one, err)
            if (err /= CAPNP_OK) return
            one%key = pfx//nm
            call append_kid(kids, n, one, err)
            if (err /= CAPNP_OK) return
         end if
      end do

      if (node_struct_discriminant_count(schema%nodes(idx)) <= 0) return
      if (parent_disc%active) then
         opts = parent_disc
      else
         al = node_annotations(schema%nodes(idx), err)
         if (err /= CAPNP_OK) return
         call read_disc_opts(al, opts, err)
         if (err /= CAPNP_OK) return
      end if
      if (p%kind == CAPNP_PK_NULL) then
         active = 0
      else
         active = capnp_which(p, int(node_struct_discriminant_offset(schema%nodes(idx))))
      end if
      do i = 0, nf - 1
         f = capnp_list_get_struct(fl, i, err)
         if (err /= CAPNP_OK) return
         fd = field_discriminant(f)
         if (fd /= active) cycle
         call json_field_name(f, nm, err)
         if (err /= CAPNP_OK) return
         call flatten_opts(f, is_flat, fpfx, err)
         if (err /= CAPNP_OK) return
         if (opts%active .and. opts%has_val .and. is_flat) then
            err = CAPNP_ERR_ARG
            return
         end if
         dkey = ''
         if (opts%active) then
            if (opts%has_name) then
               dkey = pfx//opts%name
            else if (len(union_decl) > 0) then
               dkey = pfx//union_decl
            end if
            if (len(dkey) > 0) then
               dtag%k = JK_STR
               dtag%str = nm
               dtag%key = dkey
               call append_kid(kids, n, dtag, err)
               if (err /= CAPNP_OK) return
            end if
            if (field_which(f) == FIELD_SLOT) then
               raw_t = field_slot_type(f, err)
               if (err /= CAPNP_OK) return
               t = capnp_brand_resolve(env, raw_t)
               if (type_which(t) == TYPE_VOID .and. len(dkey) > 0) exit
            end if
         end if
         if (is_flat) then
            if (field_which(f) == FIELD_GROUP) then
               gidx = capnp_codec_find_id(schema, field_group_type_id(f))
               if (gidx == 0) then
                  err = CAPNP_ERR_ARG
                  return
               end if
               call field_name(f, raw_nm, err)
               if (err /= CAPNP_OK) return
               al = field_annotations(f, err)
               if (err /= CAPNP_OK) return
               call read_disc_opts(al, gdisc, err)
               if (err /= CAPNP_OK) return
               call append_struct_fields(schema, gidx, p, env, pfx//fpfx, gdisc, raw_nm, &
                                         depth + 1, kids, n, err)
               if (err /= CAPNP_OK) return
            else
               raw_t = field_slot_type(f, err)
               if (err /= CAPNP_OK) return
               t = capnp_brand_resolve(env, raw_t)
               if (type_which(t) /= TYPE_STRUCT) then
                  err = CAPNP_ERR_KIND
                  return
               end if
               q = capnp_ptr_t()
               if (p%kind /= CAPNP_PK_NULL) then
                  q = capnp_getp(p, int(field_slot_offset(f)), err)
                  if (err /= CAPNP_OK) return
               end if
               if (q%kind == CAPNP_PK_NULL) then
                  dv = field_slot_default(f, err)
                  if (err /= CAPNP_OK) return
                  if (dv%kind /= CAPNP_PK_NULL) then
                     q = value_pointer(dv, err)
                     if (err /= CAPNP_OK) return
                  end if
               end if
               sidx = capnp_codec_find_id(schema, type_type_id(t))
               if (sidx == 0) then
                  err = CAPNP_ERR_ARG
                  return
               end if
               call capnp_brand_enter(env, t, cenv, err)
               if (err /= CAPNP_OK) return
               call append_struct_fields(schema, sidx, q, cenv, pfx//fpfx, no_disc, '', &
                                         depth + 1, kids, n, err)
               if (err /= CAPNP_OK) return
            end if
         else
            if (opts%active .and. opts%has_val) then
               vkey = pfx//opts%val_name
            else
               vkey = pfx//nm
            end if
            call build_field_value(schema, f, p, env, depth + 1, one, err)
            if (err /= CAPNP_OK) return
            one%key = vkey
            call append_kid(kids, n, one, err)
            if (err /= CAPNP_OK) return
         end if
         exit
      end do
   end subroutine append_struct_fields

   recursive subroutine build_field_value(schema, f, p, env, depth, jv, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      type(capnp_ptr_t), intent(in) :: f, p
      type(capnp_brand_env_t), intent(in) :: env
      integer, intent(in) :: depth
      type(jval), intent(out) :: jv
      integer, intent(out) :: err
      type(capnp_ptr_t) :: raw_t, t, q, et, dv, al
      type(capnp_brand_env_t) :: cenv
      type(disc_opts_t) :: gdisc, no_disc
      integer(int8), allocatable :: raw(:)
      integer :: tw, idx, mode, stat
      integer(int64) :: off
      err = CAPNP_OK
      if (field_which(f) == FIELD_GROUP) then
         idx = capnp_codec_find_id(schema, field_group_type_id(f))
         if (idx == 0) then
            err = CAPNP_ERR_ARG
            return
         end if
         al = field_annotations(f, err)
         if (err /= CAPNP_OK) return
         call read_disc_opts(al, gdisc, err)
         if (err /= CAPNP_OK) return
         call build_struct(schema, idx, p, env, gdisc, depth, jv, err)
         return
      end if
      raw_t = field_slot_type(f, err)
      if (err /= CAPNP_OK) return
      t = capnp_brand_resolve(env, raw_t)
      dv = field_slot_default(f, err)
      if (err /= CAPNP_OK) return
      tw = type_which(t)
      off = field_slot_offset(f)
      mode = data_mode(f, err)
      if (err /= CAPNP_OK) return
      select case (tw)
      case (TYPE_VOID)
         jv%k = JK_NULL
      case (TYPE_BOOL)
         jv%k = JK_BOOL
         jv%b = capnp_get_bool(p, off, default=capnp_def_bool(dv))
      case (TYPE_INT8)
         jv%k = JK_NUM
         call capnp_fmt_i64(int(capnp_get_i8(p, off, default=capnp_def_i8(dv)), int64), jv%str)
      case (TYPE_INT16)
         jv%k = JK_NUM
         call capnp_fmt_i64(int(capnp_get_i16(p, off*2_int64, default=capnp_def_i16(dv)), int64), jv%str)
      case (TYPE_INT32)
         jv%k = JK_NUM
         call capnp_fmt_i64(int(capnp_get_i32(p, off*4_int64, default=capnp_def_i32(dv)), int64), jv%str)
      case (TYPE_INT64)
         jv%k = JK_STR
         call capnp_fmt_i64(capnp_get_i64(p, off*8_int64, default=capnp_def_i64(dv)), jv%str)
      case (TYPE_UINT8)
         jv%k = JK_NUM
         call capnp_fmt_u64(int(capnp_get_u8(p, off, default=capnp_def_u8(dv)), int64), jv%str)
      case (TYPE_UINT16)
         jv%k = JK_NUM
         call capnp_fmt_u64(int(capnp_get_u16(p, off*2_int64, default=capnp_def_u16(dv)), int64), jv%str)
      case (TYPE_UINT32)
         jv%k = JK_NUM
         call capnp_fmt_u64(capnp_get_u32(p, off*4_int64, default=capnp_def_u32(dv)), jv%str)
      case (TYPE_UINT64)
         jv%k = JK_STR
         call capnp_fmt_u64(capnp_get_i64(p, off*8_int64, default=capnp_def_i64(dv)), jv%str)
      case (TYPE_FLOAT32)
         call put_float_jval(real(capnp_get_f32(p, off*4_int64, default=capnp_def_f32(dv)), real64), jv)
      case (TYPE_FLOAT64)
         call put_float_jval(capnp_get_f64(p, off*8_int64, default=capnp_def_f64(dv)), jv)
      case (TYPE_TEXT)
         jv%k = JK_STR
         q = capnp_ptr_t()
         if (p%kind /= CAPNP_PK_NULL) then
            q = capnp_getp(p, int(off), err)
            if (err /= CAPNP_OK) return
         end if
         if (q%kind == CAPNP_PK_NULL) then
            if (dv%kind /= CAPNP_PK_NULL) then
               call value_text(dv, jv%str, err)
            else
               jv%str = ''
            end if
         else
            call capnp_get_text(p, int(off), jv%str, err)
         end if
      case (TYPE_DATA)
         q = capnp_ptr_t()
         if (p%kind /= CAPNP_PK_NULL) then
            q = capnp_getp(p, int(off), err)
            if (err /= CAPNP_OK) return
         end if
         if (q%kind == CAPNP_PK_NULL) then
            if (dv%kind /= CAPNP_PK_NULL) then
               call value_data(dv, raw, err)
               if (err /= CAPNP_OK) return
            else
               allocate (raw(0), stat=stat)
               if (stat /= 0) then
                  err = CAPNP_ERR_ALLOC
                  return
               end if
            end if
         else
            call capnp_get_data(p, int(off), raw, err)
            if (err /= CAPNP_OK) return
         end if
         call build_data_jval(raw, mode, jv, err)
         if (err /= CAPNP_OK) return
      case (TYPE_ENUM)
         call build_enum_jval(schema, t, int(capnp_get_u16(p, off*2_int64, default=capnp_def_u16(dv))), jv, err)
      case (TYPE_LIST)
         q = capnp_ptr_t()
         if (p%kind /= CAPNP_PK_NULL) then
            q = capnp_getp(p, int(off), err)
            if (err /= CAPNP_OK) return
         end if
         if (q%kind == CAPNP_PK_NULL .and. dv%kind /= CAPNP_PK_NULL) then
            q = value_pointer(dv, err)
            if (err /= CAPNP_OK) return
         end if
         et = type_list_element(t, err)
         if (err /= CAPNP_OK) return
         call build_list(schema, et, q, env, depth, jv, err)
      case (TYPE_STRUCT)
         q = capnp_ptr_t()
         if (p%kind /= CAPNP_PK_NULL) then
            q = capnp_getp(p, int(off), err)
            if (err /= CAPNP_OK) return
         end if
         if (q%kind == CAPNP_PK_NULL .and. dv%kind /= CAPNP_PK_NULL) then
            q = value_pointer(dv, err)
            if (err /= CAPNP_OK) return
         end if
         idx = capnp_codec_find_id(schema, type_type_id(t))
         if (idx == 0) then
            err = CAPNP_ERR_ARG
            return
         end if
         call capnp_brand_enter(env, t, cenv, err)
         if (err /= CAPNP_OK) return
         call build_struct(schema, idx, q, cenv, no_disc, depth, jv, err)
      case default
         err = CAPNP_ERR_KIND
      end select
   end subroutine build_field_value

   recursive subroutine build_list(schema, raw_et, lst, env, depth, jv, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      type(capnp_ptr_t), intent(in) :: raw_et, lst
      type(capnp_brand_env_t), intent(in) :: env
      integer, intent(in) :: depth
      type(jval), intent(out) :: jv
      integer, intent(out) :: err
      type(capnp_ptr_t) :: et
      type(jval) :: one
      integer(int64) :: i, nlen
      integer :: n
      err = CAPNP_OK
      jv%k = JK_ARR
      if (depth > MAX_DEPTH) then
         err = CAPNP_ERR_DEPTH
         return
      end if
      if (lst%kind /= CAPNP_PK_LIST) return
      et = capnp_brand_resolve(env, raw_et)
      nlen = capnp_list_len(lst)
      n = 0
      do i = 0_int64, nlen - 1_int64
         call build_element(schema, et, lst, i, env, depth + 1, one, err)
         if (err /= CAPNP_OK) return
         call append_kid(jv%kids, n, one, err)
         if (err /= CAPNP_OK) return
      end do
      call trim_kids(jv%kids, n, err)
      if (err /= CAPNP_OK) return
   end subroutine build_list

   recursive subroutine build_element(schema, et, lst, i, env, depth, jv, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      type(capnp_ptr_t), intent(in) :: et, lst
      integer(int64), intent(in) :: i
      type(capnp_brand_env_t), intent(in) :: env
      integer, intent(in) :: depth
      type(jval), intent(out) :: jv
      integer, intent(out) :: err
      type(capnp_ptr_t) :: q, inner, el
      type(capnp_brand_env_t) :: cenv
      type(disc_opts_t) :: no_disc
      integer(int8), allocatable :: raw(:)
      integer :: tw, idx, ord
      err = CAPNP_OK
      tw = type_which(et)
      select case (tw)
      case (TYPE_VOID)
         jv%k = JK_NULL
      case (TYPE_BOOL)
         jv%k = JK_BOOL
         jv%b = capnp_list_get_bool(lst, i, err)
      case (TYPE_INT8)
         jv%k = JK_NUM
         call capnp_fmt_i64(int(capnp_list_get_i8(lst, i, err), int64), jv%str)
      case (TYPE_INT16)
         jv%k = JK_NUM
         call capnp_fmt_i64(int(capnp_list_get_i16(lst, i, err), int64), jv%str)
      case (TYPE_INT32)
         jv%k = JK_NUM
         call capnp_fmt_i64(int(capnp_list_get_i32(lst, i, err), int64), jv%str)
      case (TYPE_INT64)
         jv%k = JK_STR
         call capnp_fmt_i64(capnp_list_get_i64(lst, i, err), jv%str)
      case (TYPE_UINT8)
         jv%k = JK_NUM
         call capnp_fmt_u64(int(capnp_u_of_i8(capnp_list_get_i8(lst, i, err)), int64), jv%str)
      case (TYPE_UINT16)
         jv%k = JK_NUM
         call capnp_fmt_u64(int(capnp_u_of_i16(capnp_list_get_i16(lst, i, err)), int64), jv%str)
      case (TYPE_UINT32)
         jv%k = JK_NUM
         call capnp_fmt_u64(capnp_u_of_i32(capnp_list_get_i32(lst, i, err)), jv%str)
      case (TYPE_UINT64)
         jv%k = JK_STR
         call capnp_fmt_u64(capnp_list_get_i64(lst, i, err), jv%str)
      case (TYPE_FLOAT32)
         call put_float_jval(real(capnp_list_get_f32(lst, i, err), real64), jv)
      case (TYPE_FLOAT64)
         call put_float_jval(capnp_list_get_f64(lst, i, err), jv)
      case (TYPE_ENUM)
         ord = capnp_u_of_i16(capnp_list_get_i16(lst, i, err))
         if (err /= CAPNP_OK) return
         call build_enum_jval(schema, et, ord, jv, err)
      case (TYPE_TEXT)
         jv%k = JK_STR
         call capnp_list_get_text(lst, int(i), jv%str, err)
      case (TYPE_DATA)
         call capnp_get_data(lst, int(i), raw, err)
         if (err /= CAPNP_OK) return
         call build_data_jval(raw, 0, jv, err)
         if (err /= CAPNP_OK) return
      case (TYPE_LIST)
         q = capnp_list_getp(lst, int(i), err)
         if (err /= CAPNP_OK) return
         inner = type_list_element(et, err)
         if (err /= CAPNP_OK) return
         call build_list(schema, inner, q, env, depth, jv, err)
      case (TYPE_STRUCT)
         el = capnp_list_get_struct(lst, int(i), err)
         if (err /= CAPNP_OK) return
         idx = capnp_codec_find_id(schema, type_type_id(et))
         if (idx == 0) then
            err = CAPNP_ERR_ARG
            return
         end if
         call capnp_brand_enter(env, et, cenv, err)
         if (err /= CAPNP_OK) return
         call build_struct(schema, idx, el, cenv, no_disc, depth, jv, err)
      case default
         err = CAPNP_ERR_KIND
      end select
   end subroutine build_element

   subroutine put_float_jval(x, jv)
      real(real64), intent(in) :: x
      type(jval), intent(out) :: jv
      integer(int64) :: bits
      if (ieee_nan(x)) then
         jv%k = JK_STR
         jv%str = 'NaN'
      else if (.not. ieee_finite(x)) then
         jv%k = JK_STR
         bits = transfer(x, 0_int64)
         if (iand(bits, int(z'8000000000000000', int64)) /= 0_int64) then
            jv%str = '-Infinity'
         else
            jv%str = 'Infinity'
         end if
      else
         jv%k = JK_NUM
         call capnp_fmt_f64(x, jv%str)
      end if
   end subroutine put_float_jval

   subroutine build_data_jval(raw, mode, jv, err)
      integer(int8), intent(in) :: raw(:)
      integer, intent(in) :: mode
      type(jval), intent(out) :: jv
      integer, intent(out) :: err
      type(jval) :: one
      integer :: i, n, stat, lb
      err = CAPNP_OK
      if (mode == 1) then
         jv%k = JK_STR
         call b64_encode(raw, jv%str, err)
         return
      else if (mode == 2) then
         jv%k = JK_STR
         call hex_encode(raw, jv%str, err)
         return
      end if
      jv%k = JK_ARR
      n = size(raw)
      if (n <= 0) return
      allocate (jv%kids(n), stat=stat)
      if (stat /= 0) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      lb = lbound(raw, 1)
      do i = 0, n - 1
         one%k = JK_NUM
         call capnp_fmt_u64(int(capnp_u_of_i8(raw(lb + i)), int64), one%str)
         call take_jval(jv%kids(i + 1), one)
      end do
   end subroutine build_data_jval

   subroutine build_enum_jval(schema, t, ord, jv, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      type(capnp_ptr_t), intent(in) :: t
      integer, intent(in) :: ord
      type(jval), intent(out) :: jv
      integer, intent(out) :: err
      type(capnp_ptr_t) :: el, e, al
      character(len=:), allocatable :: nm
      integer :: idx, n
      err = CAPNP_OK
      idx = capnp_codec_find_id(schema, type_type_id(t))
      if (idx /= 0) then
         el = node_enumerants(schema%nodes(idx), err)
         if (err /= CAPNP_OK) return
         n = int(capnp_list_len(el))
         if (ord >= 0 .and. ord < n) then
            e = capnp_list_get_struct(el, ord, err)
            if (err /= CAPNP_OK) return
            call enumerant_name(e, nm, err)
            if (err /= CAPNP_OK) return
            al = enumerant_annotations(e, err)
            if (err == CAPNP_OK) call name_override(al, nm, err)
            if (err /= CAPNP_OK) return
            jv%k = JK_STR
            jv%str = nm
            return
         end if
      end if
      jv%k = JK_NUM
      call capnp_fmt_i64(int(ord, int64), jv%str)
   end subroutine build_enum_jval

   recursive subroutine emit_jval(jv, indent, has_prefix, pretty, out, multi, err)
      type(jval), intent(in) :: jv
      integer, intent(in) :: indent
      logical, intent(in) :: has_prefix, pretty
      character(len=:), allocatable, intent(out) :: out
      logical, intent(out) :: multi
      integer, intent(out) :: err
      type(piece_t), allocatable :: items(:)
      character(len=:), allocatable :: val, body, colon
      integer :: i, m, n, sub
      logical :: child_multi
      err = CAPNP_OK
      multi = .false.
      select case (jv%k)
      case (JK_NULL)
         out = 'null'
      case (JK_BOOL)
         if (jv%b) then
            out = 'true'
         else
            out = 'false'
         end if
      case (JK_NUM)
         out = jv%str
      case (JK_STR)
         call json_quote(jv%str, out, err)
         if (err /= CAPNP_OK) return
      case (JK_ARR)
         n = 0
         if (allocated(jv%kids)) n = size(jv%kids)
         sub = indent
         if (n > 1) sub = indent + 1
         m = 0
         do i = 1, n
            call emit_jval(jv%kids(i), sub, .false., pretty, val, child_multi, err)
            if (err /= CAPNP_OK) return
            call push(items, m, val, child_multi, err)
            if (err /= CAPNP_OK) return
         end do
         call jjoin(items, m, indent, has_prefix, pretty, multi, body, err)
         if (err /= CAPNP_OK) return
         out = '['//body//']'
      case (JK_OBJ)
         n = 0
         if (allocated(jv%kids)) n = size(jv%kids)
         sub = indent
         if (n > 1) sub = indent + 1
         if (pretty) then
            colon = ': '
         else
            colon = ':'
         end if
         m = 0
         do i = 1, n
            call emit_jval(jv%kids(i), sub, .true., pretty, val, child_multi, err)
            if (err /= CAPNP_OK) return
            call json_quote(jv%kids(i)%key, body, err)
            if (err /= CAPNP_OK) return
            call push(items, m, body//colon//val, child_multi, err)
            if (err /= CAPNP_OK) return
         end do
         call jjoin(items, m, indent, has_prefix, pretty, multi, body, err)
         if (err /= CAPNP_OK) return
         out = '{'//body//'}'
      case default
         out = 'null'
      end select
   end subroutine emit_jval

   subroutine union_seen_reset()
      union_seen_n = 0
   end subroutine union_seen_reset

   !> True when this struct's union discriminant was already selected
   !> while decoding the same object. Flattened keys arrive one at a
   !> time, so the mark has to outlive a single call.
   function union_was_seen(p, disc) result(yes)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(in) :: disc
      logical :: yes
      integer :: i
      yes = .false.
      do i = 1, union_seen_n
         if (union_seen_seg(i) == p%seg .and. union_seen_off(i) == p%off .and. &
             union_seen_disc(i) == disc) then
            yes = .true.
            return
         end if
      end do
   end function union_was_seen

   subroutine union_note(p, disc, err)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(in) :: disc
      integer, intent(out) :: err
      integer :: i
      err = CAPNP_OK
      do i = 1, union_seen_n
         if (union_seen_seg(i) == p%seg .and. union_seen_off(i) == p%off .and. &
             union_seen_disc(i) == disc) return
      end do
      if (union_seen_n >= UNION_SEEN_MAX) then
         err = CAPNP_ERR_DEPTH
         return
      end if
      union_seen_n = union_seen_n + 1
      union_seen_seg(union_seen_n) = p%seg
      union_seen_off(union_seen_n) = p%off
      union_seen_disc(union_seen_n) = disc
   end subroutine union_note

   recursive subroutine fill_struct(schema, idx, p, env, parent_disc, jv, depth, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx, depth
      type(capnp_ptr_t), intent(in) :: p
      type(capnp_brand_env_t), intent(in) :: env
      type(disc_opts_t), intent(in) :: parent_disc
      type(jval), intent(in) :: jv
      integer, intent(out) :: err
      err = CAPNP_OK
      if (jv%k /= JK_OBJ) then
         err = CAPNP_ERR_ARG
         return
      end if
      call fill_struct_pfx(schema, idx, p, env, '', parent_disc, '', jv, depth, err)
   end subroutine fill_struct

   recursive subroutine fill_struct_pfx(schema, idx, p, env, pfx, parent_disc, union_decl, jv, depth, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx, depth
      type(capnp_ptr_t), intent(in) :: p
      type(capnp_brand_env_t), intent(in) :: env
      character(len=*), intent(in) :: pfx, union_decl
      type(disc_opts_t), intent(in) :: parent_disc
      type(jval), intent(in) :: jv
      integer, intent(out) :: err
      type(capnp_ptr_t) :: fl, f, al, raw_t, t, q
      type(capnp_brand_env_t) :: cenv
      type(disc_opts_t) :: opts, gdisc, no_disc
      character(len=:), allocatable :: nm, raw_nm, fpfx, dkey
      integer :: i, nf, gidx, sidx, kidx, dw, pw
      logical :: is_flat, hit
      err = CAPNP_OK
      if (depth > MAX_DEPTH) then
         err = CAPNP_ERR_DEPTH
         return
      end if
      if (.not. allocated(jv%kids)) return
      fl = node_struct_fields(schema%nodes(idx), err)
      if (err /= CAPNP_OK) return
      nf = int(capnp_list_len(fl))

      do i = 0, nf - 1
         f = capnp_list_get_struct(fl, i, err)
         if (err /= CAPNP_OK) return
         if (field_discriminant(f) /= NO_DISCRIMINANT) cycle
         call flatten_opts(f, is_flat, fpfx, err)
         if (err /= CAPNP_OK) return
         if (is_flat) then
            if (field_which(f) == FIELD_GROUP) then
               gidx = capnp_codec_find_id(schema, field_group_type_id(f))
               if (gidx == 0) then
                  err = CAPNP_ERR_ARG
                  return
               end if
               call field_name(f, raw_nm, err)
               if (err /= CAPNP_OK) return
               al = field_annotations(f, err)
               if (err /= CAPNP_OK) return
               call read_disc_opts(al, gdisc, err)
               if (err /= CAPNP_OK) return
               call fill_struct_pfx(schema, gidx, p, env, pfx//fpfx, gdisc, raw_nm, jv, depth + 1, err)
               if (err /= CAPNP_OK) return
            else
               raw_t = field_slot_type(f, err)
               if (err /= CAPNP_OK) return
               t = capnp_brand_resolve(env, raw_t)
               if (type_which(t) /= TYPE_STRUCT) then
                  err = CAPNP_ERR_KIND
                  return
               end if
               sidx = capnp_codec_find_id(schema, type_type_id(t))
               if (sidx == 0) then
                  err = CAPNP_ERR_ARG
                  return
               end if
               call capnp_brand_enter(env, t, cenv, err)
               if (err /= CAPNP_OK) return
               call struct_matches_any(schema, sidx, cenv, pfx//fpfx, no_disc, '', jv, depth + 1, hit, err)
               if (err /= CAPNP_OK) return
               if (.not. hit) cycle
               q = capnp_getp(p, int(field_slot_offset(f)), err)
               if (err /= CAPNP_OK) return
               if (q%kind /= CAPNP_PK_STRUCT) then
                  dw = node_struct_data_words(schema%nodes(sidx))
                  pw = node_struct_pointer_count(schema%nodes(sidx))
                  q = capnp_codec_new_struct(p, dw, pw, err)
                  if (err /= CAPNP_OK) return
                  call capnp_setp(p, int(field_slot_offset(f)), q, err)
                  if (err /= CAPNP_OK) return
               end if
               call fill_struct_pfx(schema, sidx, q, cenv, pfx//fpfx, no_disc, '', jv, depth + 1, err)
               if (err /= CAPNP_OK) return
            end if
         else
            call json_field_name(f, nm, err)
            if (err /= CAPNP_OK) return
            if (find_jkey(jv, pfx//nm, kidx)) then
               call fill_field(schema, f, p, env, jv%kids(kidx), depth + 1, err)
               if (err /= CAPNP_OK) return
            end if
         end if
      end do

      if (node_struct_discriminant_count(schema%nodes(idx)) <= 0) return
      if (parent_disc%active) then
         opts = parent_disc
      else
         al = node_annotations(schema%nodes(idx), err)
         if (err /= CAPNP_OK) return
         call read_disc_opts(al, opts, err)
         if (err /= CAPNP_OK) return
      end if
      dkey = ''
      if (opts%active) then
         if (opts%has_name) then
            dkey = pfx//opts%name
         else if (len(union_decl) > 0) then
            dkey = pfx//union_decl
         end if
      end if

      call decode_union_keys(schema, idx, p, env, pfx, opts, dkey, jv, depth, err)
   end subroutine fill_struct_pfx

!> Zero the storage of one field. A known union tag does this after
!> selecting the arm, which drops a member written earlier in the object.
!> A void arm has no storage. A group clears each of its fields.
   recursive subroutine clear_field(schema, f, p, env, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      type(capnp_ptr_t), intent(in) :: f, p
      type(capnp_brand_env_t), intent(in) :: env
      integer, intent(out) :: err
      type(capnp_ptr_t) :: t, fl, child
      integer(int64) :: off
      integer :: tw, gidx, i, nf
      err = CAPNP_OK
      if (field_which(f) == FIELD_GROUP) then
         gidx = capnp_codec_find_id(schema, field_group_type_id(f))
         if (gidx == 0) then
            err = CAPNP_ERR_ARG
            return
         end if
         fl = node_struct_fields(schema%nodes(gidx), err)
         if (err /= CAPNP_OK) return
         nf = int(capnp_list_len(fl))
         do i = 0, nf - 1
            child = capnp_list_get_struct(fl, i, err)
            if (err /= CAPNP_OK) return
            call clear_field(schema, child, p, env, err)
            if (err /= CAPNP_OK) return
         end do
         return
      end if
      if (field_which(f) /= FIELD_SLOT) return
      t = field_slot_type(f, err)
      if (err /= CAPNP_OK) return
      t = capnp_brand_resolve(env, t)
      tw = type_which(t)
      off = field_slot_offset(f)
      select case (tw)
      case (TYPE_VOID)
      case (TYPE_BOOL)
         call capnp_set_bool(p, off, .false., err)
      case (TYPE_INT8)
         call capnp_set_i8(p, off, 0_int8, err)
      case (TYPE_INT16)
         call capnp_set_i16(p, off*2_int64, 0_int16, err)
      case (TYPE_INT32)
         call capnp_set_i32(p, off*4_int64, 0_int32, err)
      case (TYPE_INT64)
         call capnp_set_i64(p, off*8_int64, 0_int64, err)
      case (TYPE_UINT8)
         call capnp_set_u8(p, off, 0_int16, err)
      case (TYPE_UINT16)
         call capnp_set_u16(p, off*2_int64, 0_int32, err)
      case (TYPE_UINT32)
         call capnp_set_u32(p, off*4_int64, 0_int64, err)
      case (TYPE_UINT64)
         call capnp_set_u64(p, off*8_int64, 0_int64, err)
      case (TYPE_FLOAT32)
         call capnp_set_f32(p, off*4_int64, 0.0_real32, err)
      case (TYPE_FLOAT64)
         call capnp_set_f64(p, off*8_int64, 0.0_real64, err)
      case (TYPE_ENUM)
         call capnp_set_u16(p, off*2_int64, 0_int32, err)
      case (TYPE_TEXT, TYPE_DATA, TYPE_LIST, TYPE_STRUCT, TYPE_INTERFACE, TYPE_ANY_POINTER)
         call capnp_setp(p, int(off), capnp_ptr_t(), err)
      case default
         err = CAPNP_ERR_KIND
      end select
   end subroutine clear_field

!> Apply union keys in document order, the way kj JsonCodec walks an
!> object. A known tag clears the arm it selects. A second known tag
!> is an error. An unknown tag selects nothing, and plain member keys
!> still decode. valueName and flattened-union keys that appear before
!> the tag are retried after it.
   recursive subroutine decode_union_keys(schema, idx, p, env, pfx, opts, dkey, jv, depth, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx, depth
      type(capnp_ptr_t), intent(in) :: p
      type(capnp_brand_env_t), intent(in) :: env
      character(len=*), intent(in) :: pfx, dkey
      type(disc_opts_t), intent(in) :: opts
      type(jval), intent(in) :: jv
      integer, intent(out) :: err
      type(capnp_ptr_t) :: child
      type(capnp_brand_env_t) :: cenv
      integer, allocatable :: pend(:), nxt(:)
      integer :: nk, np, nnext, i, guard, stat, child_which, disc
      logical :: deferred, progressed, tag_set, have_child
      err = CAPNP_OK
      if (.not. allocated(jv%kids)) return
      nk = size(jv%kids)
      disc = int(node_struct_discriminant_offset(schema%nodes(idx)))
      tag_set = union_was_seen(p, disc)
      have_child = .false.
      child_which = -1
      allocate (pend(nk), nxt(nk), stat=stat)
      if (stat /= 0) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      np = 0
      do i = 1, nk
         call apply_key(i, deferred)
         if (err /= CAPNP_OK) return
         if (deferred) then
            np = np + 1
            pend(np) = i
         end if
      end do
      do guard = 1, nk
         if (np == 0) exit
         progressed = .false.
         nnext = 0
         do i = 1, np
            call apply_key(pend(i), deferred)
            if (err /= CAPNP_OK) return
            if (deferred) then
               nnext = nnext + 1
               nxt(nnext) = pend(i)
            else
               progressed = .true.
            end if
         end do
         if (.not. progressed) exit
         pend(1:nnext) = nxt(1:nnext)
         np = nnext
      end do
   contains
      recursive subroutine apply_key(ki, defer)
         integer, intent(in) :: ki
         logical, intent(out) :: defer
         type(capnp_ptr_t) :: f, al, raw_t, t, q
         type(disc_opts_t) :: gdisc, no_disc
         type(jval) :: one
         character(len=:), allocatable :: key, local, raw_nm, fpfx, vkey
         integer :: plen, w, dw, pw, gidx, sidx
         logical :: is_flat, owned
         defer = .false.
         key = ''
         if (allocated(jv%kids(ki)%key)) key = jv%kids(ki)%key
         plen = len(pfx)
         if (len(dkey) > 0 .and. key == dkey) then
            if (jv%kids(ki)%k /= JK_STR) then
               err = CAPNP_ERR_ARG
               return
            end if
            if (.not. find_union_json_field(schema, idx, jv%kids(ki)%str, f)) return
            if (tag_set) then
               err = CAPNP_ERR_ARG
               return
            end if
            call union_note(p, disc, err)
            if (err /= CAPNP_OK) return
            call capnp_set_which(p, int(node_struct_discriminant_offset(schema%nodes(idx))), &
                                  field_discriminant(f), err)
            if (err /= CAPNP_OK) return
            call clear_field(schema, f, p, env, err)
            if (err /= CAPNP_OK) return
            have_child = .false.
            tag_set = .true.
            return
         end if
         if (opts%has_val) then
            vkey = pfx//opts%val_name
            if (key == vkey) then
               if (.not. tag_set) then
                  defer = .true.
                  return
               end if
               if (.not. active_union_field(schema, idx, p, f)) then
                  err = CAPNP_ERR_ARG
                  return
               end if
               if (union_arm_void(f, env)) return
               call fill_field(schema, f, p, env, jv%kids(ki), depth + 1, err)
               return
            end if
         else if (plen <= len(key)) then
            if (plen == 0 .or. key(1:plen) == pfx) then
               local = key(plen + 1:)
               if (find_union_json_field(schema, idx, local, f)) then
                  call flatten_opts(f, is_flat, fpfx, err)
                  if (err /= CAPNP_OK) return
                  if (.not. is_flat) then
                     call capnp_set_which(p, &
                        int(node_struct_discriminant_offset(schema%nodes(idx))), &
                        field_discriminant(f), err)
                     if (err /= CAPNP_OK) return
                     if (union_arm_void(f, env)) return
                     call fill_field(schema, f, p, env, jv%kids(ki), depth + 1, err)
                     return
                  end if
               end if
            end if
         end if
         call flat_union_owns(schema, idx, env, pfx, key, depth, owned, err)
         if (err /= CAPNP_OK .or. .not. owned) return
         if (.not. tag_set) then
            defer = .true.
            return
         end if
         if (.not. active_union_field(schema, idx, p, f)) then
            err = CAPNP_ERR_ARG
            return
         end if
         call flatten_opts(f, is_flat, fpfx, err)
         if (err /= CAPNP_OK) return
         if (.not. is_flat) then
            err = CAPNP_ERR_ARG
            return
         end if
         one%k = JK_OBJ
         allocate (one%kids(1), stat=stat)
         if (stat /= 0) then
            err = CAPNP_ERR_ALLOC
            return
         end if
         one%kids(1) = jv%kids(ki)
         if (field_which(f) == FIELD_GROUP) then
            gidx = capnp_codec_find_id(schema, field_group_type_id(f))
            if (gidx == 0) then
               err = CAPNP_ERR_ARG
               return
            end if
            call field_name(f, raw_nm, err)
            if (err /= CAPNP_OK) return
            al = field_annotations(f, err)
            if (err /= CAPNP_OK) return
            call read_disc_opts(al, gdisc, err)
            if (err /= CAPNP_OK) return
            call fill_struct_pfx(schema, gidx, p, env, pfx//fpfx, gdisc, raw_nm, one, depth + 1, err)
            return
         end if
         w = capnp_which(p, int(node_struct_discriminant_offset(schema%nodes(idx))))
         if (have_child .and. w /= child_which) have_child = .false.
         raw_t = field_slot_type(f, err)
         if (err /= CAPNP_OK) return
         t = capnp_brand_resolve(env, raw_t)
         if (type_which(t) /= TYPE_STRUCT) then
            err = CAPNP_ERR_KIND
            return
         end if
         sidx = capnp_codec_find_id(schema, type_type_id(t))
         if (sidx == 0) then
            err = CAPNP_ERR_ARG
            return
         end if
         call capnp_brand_enter(env, t, cenv, err)
         if (err /= CAPNP_OK) return
         if (.not. have_child) then
            dw = node_struct_data_words(schema%nodes(sidx))
            pw = node_struct_pointer_count(schema%nodes(sidx))
            q = capnp_codec_new_struct(p, dw, pw, err)
            if (err /= CAPNP_OK) return
            call capnp_setp(p, int(field_slot_offset(f)), q, err)
            if (err /= CAPNP_OK) return
            child = q
            child_which = w
            have_child = .true.
         end if
         call fill_struct_pfx(schema, sidx, child, cenv, pfx//fpfx, no_disc, '', one, &
                              depth + 1, err)
      end subroutine apply_key
   end subroutine decode_union_keys

   recursive subroutine struct_matches_any(schema, idx, env, pfx, parent_disc, union_decl, jv, &
                                           depth, hit, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx, depth
      type(capnp_brand_env_t), intent(in) :: env
      character(len=*), intent(in) :: pfx, union_decl
      type(disc_opts_t), intent(in) :: parent_disc
      type(jval), intent(in) :: jv
      logical, intent(out) :: hit
      integer, intent(out) :: err
      type(capnp_ptr_t) :: fl, f, al, raw_t, t
      type(capnp_brand_env_t) :: cenv
      type(disc_opts_t) :: opts, gdisc, no_disc
      character(len=:), allocatable :: nm, raw_nm, fpfx, dkey
      integer :: i, nf, gidx, sidx, kidx
      logical :: is_flat
      err = CAPNP_OK
      hit = .false.
      if (depth > MAX_DEPTH) then
         err = CAPNP_ERR_DEPTH
         return
      end if
      fl = node_struct_fields(schema%nodes(idx), err)
      if (err /= CAPNP_OK) return
      nf = int(capnp_list_len(fl))
      do i = 0, nf - 1
         f = capnp_list_get_struct(fl, i, err)
         if (err /= CAPNP_OK) return
         if (field_discriminant(f) /= NO_DISCRIMINANT) cycle
         call flatten_opts(f, is_flat, fpfx, err)
         if (err /= CAPNP_OK) return
         if (is_flat) then
            if (field_which(f) == FIELD_GROUP) then
               gidx = capnp_codec_find_id(schema, field_group_type_id(f))
               if (gidx == 0) cycle
               call field_name(f, raw_nm, err)
               if (err /= CAPNP_OK) return
               al = field_annotations(f, err)
               if (err /= CAPNP_OK) return
               call read_disc_opts(al, gdisc, err)
               if (err /= CAPNP_OK) return
               call struct_matches_any(schema, gidx, env, pfx//fpfx, gdisc, raw_nm, jv, &
                                       depth + 1, hit, err)
            else
               raw_t = field_slot_type(f, err)
               if (err /= CAPNP_OK) return
               t = capnp_brand_resolve(env, raw_t)
               if (type_which(t) /= TYPE_STRUCT) cycle
               sidx = capnp_codec_find_id(schema, type_type_id(t))
               if (sidx == 0) cycle
               call capnp_brand_enter(env, t, cenv, err)
               if (err /= CAPNP_OK) return
               call struct_matches_any(schema, sidx, cenv, pfx//fpfx, no_disc, '', jv, &
                                       depth + 1, hit, err)
            end if
            if (err /= CAPNP_OK .or. hit) return
         else
            call json_field_name(f, nm, err)
            if (err /= CAPNP_OK) return
            if (find_jkey(jv, pfx//nm, kidx)) then
               hit = .true.
               return
            end if
         end if
      end do

      if (node_struct_discriminant_count(schema%nodes(idx)) <= 0) return
      if (parent_disc%active) then
         opts = parent_disc
      else
         al = node_annotations(schema%nodes(idx), err)
         if (err /= CAPNP_OK) return
         call read_disc_opts(al, opts, err)
         if (err /= CAPNP_OK) return
      end if
      if (opts%active) then
         dkey = ''
         if (opts%has_name) then
            dkey = pfx//opts%name
         else if (len(union_decl) > 0) then
            dkey = pfx//union_decl
         end if
         if (len(dkey) > 0) then
            if (find_jkey(jv, dkey, kidx)) then
               hit = .true.
               return
            end if
         end if
         ! valueName is a key of this union even when the tag is absent
         ! from this object. Member names are not, once valueName is set.
         if (opts%has_val) then
            if (find_jkey(jv, pfx//opts%val_name, kidx)) then
               hit = .true.
               return
            end if
            return
         end if
      end if
      do i = 0, nf - 1
         f = capnp_list_get_struct(fl, i, err)
         if (err /= CAPNP_OK) return
         if (field_discriminant(f) == NO_DISCRIMINANT) cycle
         call json_field_name(f, nm, err)
         if (err /= CAPNP_OK) return
         if (find_jkey(jv, pfx//nm, kidx)) then
            hit = .true.
            return
         end if
      end do
   end subroutine struct_matches_any

   recursive subroutine fill_field(schema, f, p, env, jv, depth, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      type(capnp_ptr_t), intent(in) :: f, p
      type(capnp_brand_env_t), intent(in) :: env
      type(jval), intent(in) :: jv
      integer, intent(in) :: depth
      integer, intent(out) :: err
      type(capnp_ptr_t) :: raw_t, t, q, et, dv, al
      type(capnp_brand_env_t) :: cenv
      type(disc_opts_t) :: gdisc, no_disc
      integer :: tw, idx, dw, pw, mode
      integer(int64) :: off
      err = CAPNP_OK
      if (field_which(f) == FIELD_GROUP) then
         idx = capnp_codec_find_id(schema, field_group_type_id(f))
         if (idx == 0) then
            err = CAPNP_ERR_ARG
            return
         end if
         al = field_annotations(f, err)
         if (err /= CAPNP_OK) return
         call read_disc_opts(al, gdisc, err)
         if (err /= CAPNP_OK) return
         call fill_struct(schema, idx, p, env, gdisc, jv, depth, err)
         return
      end if
      raw_t = field_slot_type(f, err)
      if (err /= CAPNP_OK) return
      t = capnp_brand_resolve(env, raw_t)
      dv = field_slot_default(f, err)
      if (err /= CAPNP_OK) return
      tw = type_which(t)
      off = field_slot_offset(f)
      mode = data_mode(f, err)
      if (err /= CAPNP_OK) return
      select case (tw)
      case (TYPE_LIST)
         et = type_list_element(t, err)
         if (err /= CAPNP_OK) return
         call fill_list(schema, et, p, int(off), env, jv, depth, err)
      case (TYPE_STRUCT)
         if (jv%k /= JK_OBJ) then
            err = CAPNP_ERR_ARG
            return
         end if
         idx = capnp_codec_find_id(schema, type_type_id(t))
         if (idx == 0) then
            err = CAPNP_ERR_ARG
            return
         end if
         call capnp_brand_enter(env, t, cenv, err)
         if (err /= CAPNP_OK) return
         dw = node_struct_data_words(schema%nodes(idx))
         pw = node_struct_pointer_count(schema%nodes(idx))
         q = capnp_codec_new_struct(p, dw, pw, err)
         if (err /= CAPNP_OK) return
         call capnp_setp(p, int(off), q, err)
         if (err /= CAPNP_OK) return
         call fill_struct(schema, idx, q, cenv, no_disc, jv, depth, err)
      case (TYPE_DATA)
         call fill_data(p, int(off), jv, mode, .false., 0_int64, err)
      case default
         call fill_scalar(schema, t, p, off, dv, jv, .false., 0_int64, err)
      end select
   end subroutine fill_field

   recursive subroutine fill_list(schema, raw_et, parent, slot, env, jv, depth, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      type(capnp_ptr_t), intent(in) :: raw_et, parent
      integer, intent(in) :: slot, depth
      type(capnp_brand_env_t), intent(in) :: env
      type(jval), intent(in) :: jv
      integer, intent(out) :: err
      type(capnp_ptr_t) :: lst, et, inner, el
      type(capnp_brand_env_t) :: cenv
      type(disc_opts_t) :: no_disc
      integer :: n, i, tw, idx
      err = CAPNP_OK
      if (depth > MAX_DEPTH) then
         err = CAPNP_ERR_DEPTH
         return
      end if
      if (jv%k /= JK_ARR) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (allocated(jv%kids)) then
         n = size(jv%kids)
      else
         n = 0
      end if
      et = capnp_brand_resolve(env, raw_et)
      lst = capnp_codec_alloc_list(parent, et, n, schema, err, env=env)
      if (err /= CAPNP_OK) return
      call capnp_setp(parent, slot, lst, err)
      if (err /= CAPNP_OK) return
      tw = type_which(et)
      do i = 1, n
         select case (tw)
         case (TYPE_LIST)
            inner = type_list_element(et, err)
            if (err /= CAPNP_OK) return
            call fill_list(schema, inner, lst, i - 1, env, jv%kids(i), depth + 1, err)
         case (TYPE_STRUCT)
            el = capnp_list_get_struct(lst, i - 1, err)
            if (err /= CAPNP_OK) return
            idx = capnp_codec_find_id(schema, type_type_id(et))
            if (idx == 0) then
               err = CAPNP_ERR_ARG
               return
            end if
            call capnp_brand_enter(env, et, cenv, err)
            if (err /= CAPNP_OK) return
            call fill_struct(schema, idx, el, cenv, no_disc, jv%kids(i), depth + 1, err)
         case (TYPE_DATA)
            call fill_data(lst, i - 1, jv%kids(i), 0, .true., int(i - 1, int64), err)
         case default
            call fill_scalar(schema, et, lst, 0_int64, capnp_ptr_t(), jv%kids(i), .true., &
                              int(i - 1, int64), err)
         end select
         if (err /= CAPNP_OK) return
      end do
   end subroutine fill_list

   subroutine fill_scalar(schema, t, p, off, dv, jv, in_list, ix, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      type(capnp_ptr_t), intent(in) :: t, p, dv
      integer(int64), intent(in) :: off, ix
      type(jval), intent(in) :: jv
      logical, intent(in) :: in_list
      integer, intent(out) :: err
      integer(int64) :: iv
      real(real64) :: fv
      integer :: tw, ord
      logical :: bv
      err = CAPNP_OK
      tw = type_which(t)
      select case (tw)
      case (TYPE_VOID)
         ! Any JSON value is void, as in capnp's Void type.
      case (TYPE_BOOL)
         if (jv%k /= JK_BOOL) then
            err = CAPNP_ERR_ARG
            return
         end if
         bv = jv%b
         if (in_list) then
            call capnp_list_set_bool(p, ix, bv, err)
         else
            call capnp_set_bool(p, off, bv, err, default=capnp_def_bool(dv))
         end if
      case (TYPE_INT8, TYPE_INT16, TYPE_INT32, TYPE_INT64)
         call j_signed(jv, iv, err)
         if (err /= CAPNP_OK) return
         call capnp_put_signed(p, off, tw, iv, in_list, ix, dv, err)
      case (TYPE_UINT8, TYPE_UINT16, TYPE_UINT32, TYPE_UINT64)
         call j_unsigned(jv, iv, err)
         if (err /= CAPNP_OK) return
         call capnp_put_unsigned(p, off, tw, iv, in_list, ix, dv, err)
      case (TYPE_FLOAT32, TYPE_FLOAT64)
         call j_float(jv, fv, err)
         if (err /= CAPNP_OK) return
         if (tw == TYPE_FLOAT32) then
            if (in_list) then
               call capnp_list_set_f32(p, ix, real(fv, real32), err)
            else
               call capnp_set_f32(p, off*4_int64, real(fv, real32), err, default=capnp_def_f32(dv))
            end if
         else
            if (in_list) then
               call capnp_list_set_f64(p, ix, fv, err)
            else
               call capnp_set_f64(p, off*8_int64, fv, err, default=capnp_def_f64(dv))
            end if
         end if
      case (TYPE_TEXT)
         if (jv%k /= JK_STR) then
            err = CAPNP_ERR_ARG
            return
         end if
         if (in_list) then
            call capnp_list_set_text(p, int(ix), jv%str, err)
         else
            call capnp_set_text(p, int(off), jv%str, err)
         end if
      case (TYPE_ENUM)
         call j_enum(schema, t, jv, ord, err)
         if (err /= CAPNP_OK) return
         if (in_list) then
            call capnp_list_set_i16(p, ix, capnp_as_i16(ord), err)
         else
            call capnp_set_u16(p, off*2_int64, int(ord, int32), err, default=capnp_def_u16(dv))
         end if
      case default
         err = CAPNP_ERR_KIND
      end select
   end subroutine fill_scalar

   subroutine fill_data(p, slot, jv, mode, in_list, ix, err)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(in) :: slot, mode
      type(jval), intent(in) :: jv
      logical, intent(in) :: in_list
      integer(int64), intent(in) :: ix
      integer, intent(out) :: err
      integer(int8), allocatable :: raw(:)
      integer :: stat
      err = CAPNP_OK
      if (mode == 1) then
         if (jv%k == JK_NULL) then
            allocate (raw(0), stat=stat)
            if (stat /= 0) err = CAPNP_ERR_ALLOC
         else if (jv%k /= JK_STR) then
            err = CAPNP_ERR_ARG
            return
         else
            call b64_decode(jv%str, raw, err)
         end if
      else if (mode == 2) then
         if (jv%k == JK_NULL) then
            allocate (raw(0), stat=stat)
            if (stat /= 0) err = CAPNP_ERR_ALLOC
         else if (jv%k /= JK_STR) then
            err = CAPNP_ERR_ARG
            return
         else
            call hex_decode(jv%str, raw, err)
         end if
      else
         call bytes_from_array(jv, raw, err)
      end if
      if (err /= CAPNP_OK) return
      if (in_list) then
         call capnp_set_data(p, int(ix), raw, err)
      else
         call capnp_set_data(p, slot, raw, err)
      end if
   end subroutine fill_data

   subroutine j_signed(jv, iv, err)
      type(jval), intent(in) :: jv
      integer(int64), intent(out) :: iv
      integer, intent(out) :: err
      err = CAPNP_OK
      iv = 0_int64
      if (jv%k == JK_STR) then
         call kj_parse_i64(jv%str, iv, err)
      else if (jv%k == JK_NUM .and. allocated(jv%str)) then
         if (json_int_token(jv%str)) then
            call kj_parse_i64(jv%str, iv, err)
            if (err /= CAPNP_OK) call capnp_f64_to_i64(jv%num, iv, err)
         else
            call capnp_f64_to_i64(jv%num, iv, err)
         end if
      else if (jv%k == JK_NUM) then
         call capnp_f64_to_i64(jv%num, iv, err)
      else
         err = CAPNP_ERR_ARG
      end if
   end subroutine j_signed

   subroutine j_unsigned(jv, iv, err)
      type(jval), intent(in) :: jv
      integer(int64), intent(out) :: iv
      integer, intent(out) :: err
      err = CAPNP_OK
      iv = 0_int64
      if (jv%k == JK_STR) then
         call kj_parse_u64(jv%str, iv, err)
      else if (jv%k == JK_NUM .and. allocated(jv%str)) then
         if (jv%str == '-0') then
            iv = 0_int64
         else if (json_int_token(jv%str)) then
            call kj_parse_u64(jv%str, iv, err)
            if (err /= CAPNP_OK) call capnp_f64_to_u64(jv%num, iv, err)
         else
            call capnp_f64_to_u64(jv%num, iv, err)
         end if
      else if (jv%k == JK_NUM) then
         call capnp_f64_to_u64(jv%num, iv, err)
      else
         err = CAPNP_ERR_ARG
      end if
   end subroutine j_unsigned

   function json_int_token(tok) result(y)
      character(len=*), intent(in) :: tok
      logical :: y
      integer :: i, n
      y = .false.
      n = len(tok)
      if (n == 0) return
      i = 1
      if (tok(1:1) == '-') then
         if (n == 1) return
         i = 2
      end if
      do while (i <= n)
         if (tok(i:i) < '0' .or. tok(i:i) > '9') return
         i = i + 1
      end do
      y = .true.
   end function json_int_token

   function kj_hex_prefix(tok) result(y)
      character(len=*), intent(in) :: tok
      logical :: y
      integer :: i
      y = .false.
      i = 1
      if (len(tok) >= 1) then
         if (tok(1:1) == '-') i = 2
      end if
      if (i + 1 > len(tok)) return
      y = tok(i:i) == '0' .and. (tok(i + 1:i + 1) == 'x' .or. tok(i + 1:i + 1) == 'X')
   end function kj_hex_prefix

   function kj_is_space(ch) result(y)
      character(len=1), intent(in) :: ch
      logical :: y
      integer :: c
      c = ichar(ch)
      y = c == 9 .or. c == 10 .or. c == 11 .or. c == 12 .or. c == 13 .or. c == 32
   end function kj_is_space

   function kj_digit(ch, base, dig) result(ok)
      character(len=1), intent(in) :: ch
      integer, intent(in) :: base
      integer, intent(out) :: dig
      logical :: ok
      integer :: c
      ok = .false.
      dig = 0
      c = ichar(ch)
      if (c >= ichar('0') .and. c <= ichar('9')) then
         dig = c - ichar('0')
      else if (c >= ichar('a') .and. c <= ichar('f')) then
         dig = c - ichar('a') + 10
      else if (c >= ichar('A') .and. c <= ichar('F')) then
         dig = c - ichar('A') + 10
      else
         return
      end if
      ok = dig < base
   end function kj_digit

   subroutine kj_acc(hi, lo, base, dig, err)
      integer(int64), intent(inout) :: hi, lo
      integer, intent(in) :: base, dig
      integer, intent(out) :: err
      integer(int64) :: prod, carry, b
      integer(int64), parameter :: limb = 4294967295_int64
      integer(int64), parameter :: base32 = 4294967296_int64
      err = CAPNP_OK
      b = int(base, int64)
      if (lo > (huge(lo) - int(dig, int64))/b) then
         err = CAPNP_ERR_ARG
         return
      end if
      prod = lo*b + int(dig, int64)
      carry = prod/base32
      lo = mod(prod, base32)
      if (hi > (limb - carry)/b) then
         err = CAPNP_ERR_ARG
         return
      end if
      hi = hi*b + carry
   end subroutine kj_acc

   function kj_as_i64(hi, lo) result(v)
      integer(int64), intent(in) :: hi, lo
      integer(int64) :: v
      integer(int64), parameter :: base32 = 4294967296_int64
      integer(int64), parameter :: signbit = 2147483648_int64
      if (hi >= signbit) then
         v = (hi - base32)*base32 + lo
      else
         v = hi*base32 + lo
      end if
   end function kj_as_i64

   subroutine kj_parse_mag(tok, neg, hi, lo, err)
      character(len=*), intent(in) :: tok
      logical, intent(out) :: neg
      integer(int64), intent(out) :: hi, lo
      integer, intent(out) :: err
      integer :: i, n, base, dig
      logical :: any, hex
      err = CAPNP_OK
      neg = .false.
      hi = 0_int64
      lo = 0_int64
      n = len(tok)
      i = 1
      do while (i <= n)
         if (.not. kj_is_space(tok(i:i))) exit
         i = i + 1
      end do
      if (i <= n) then
         if (tok(i:i) == '+') then
            i = i + 1
         else if (tok(i:i) == '-') then
            neg = .true.
            i = i + 1
         end if
      end if
      hex = kj_hex_prefix(tok)
      base = 10
      if (hex) base = 16
      if (hex) then
         if (i + 1 > n) then
            err = CAPNP_ERR_ARG
            return
         end if
         i = i + 2
      end if
      any = .false.
      do while (i <= n)
         if (.not. kj_digit(tok(i:i), base, dig)) exit
         any = .true.
         call kj_acc(hi, lo, base, dig, err)
         if (err /= CAPNP_OK) return
         i = i + 1
      end do
      if (.not. any .or. i <= n) err = CAPNP_ERR_ARG
   end subroutine kj_parse_mag

   subroutine kj_parse_i64(tok, v, err)
      character(len=*), intent(in) :: tok
      integer(int64), intent(out) :: v
      integer, intent(out) :: err
      logical :: neg
      integer(int64) :: hi, lo
      integer(int64), parameter :: signbit = 2147483648_int64
      err = CAPNP_OK
      v = 0_int64
      call kj_parse_mag(tok, neg, hi, lo, err)
      if (err /= CAPNP_OK) return
      if (hi > signbit .or. (hi == signbit .and. (lo /= 0_int64 .or. .not. neg))) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (hi == signbit) then
         v = int(z'8000000000000000', int64)
         return
      end if
      v = kj_as_i64(hi, lo)
      if (neg) v = -v
   end subroutine kj_parse_i64

   subroutine kj_parse_u64(tok, v, err)
      character(len=*), intent(in) :: tok
      integer(int64), intent(out) :: v
      integer, intent(out) :: err
      logical :: neg
      integer(int64) :: hi, lo
      integer(int64), parameter :: min_i64 = int(z'8000000000000000', int64)
      err = CAPNP_OK
      v = 0_int64
      if (len(tok) == 0 .or. tok(1:1) == '-') then
         err = CAPNP_ERR_ARG
         return
      end if
      call kj_parse_mag(tok, neg, hi, lo, err)
      if (err /= CAPNP_OK) return
      v = kj_as_i64(hi, lo)
      if (neg .and. v /= 0_int64 .and. v /= min_i64) v = -v
   end subroutine kj_parse_u64

   subroutine j_float(jv, fv, err)
      type(jval), intent(in) :: jv
      real(real64), intent(out) :: fv
      integer, intent(out) :: err
      err = CAPNP_OK
      fv = 0.0_real64
      if (jv%k == JK_NULL) then
         fv = capnp_quiet_nan()
      else if (jv%k == JK_NUM) then
         fv = jv%num
      else if (jv%k == JK_STR) then
         call capnp_parse_f64(jv%str, fv, err)
      else
         err = CAPNP_ERR_ARG
      end if
   end subroutine j_float

   subroutine j_enum(schema, t, jv, ord, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      type(capnp_ptr_t), intent(in) :: t
      type(jval), intent(in) :: jv
      integer, intent(out) :: ord
      integer, intent(out) :: err
      type(capnp_ptr_t) :: el, e, al
      character(len=:), allocatable :: nm
      integer :: idx, i, n
      integer(int64) :: tid
      err = CAPNP_OK
      ord = 0
      if (jv%k == JK_NUM) then
         if (jv%num < 0.0_real64 .or. jv%num > 65535.0_real64) then
            err = CAPNP_ERR_ARG
            return
         end if
         ord = int(jv%num)
         return
      end if
      if (jv%k /= JK_STR) then
         err = CAPNP_ERR_ARG
         return
      end if
      tid = type_type_id(t)
      idx = capnp_codec_find_id(schema, tid)
      if (idx == 0) then
         err = CAPNP_ERR_ARG
         return
      end if
      el = node_enumerants(schema%nodes(idx), err)
      if (err /= CAPNP_OK) return
      n = int(capnp_list_len(el))
      do i = 0, n - 1
         e = capnp_list_get_struct(el, i, err)
         if (err /= CAPNP_OK) return
         call enumerant_name(e, nm, err)
         if (err /= CAPNP_OK) return
         al = enumerant_annotations(e, err)
         if (err /= CAPNP_OK) return
         call name_override(al, nm, err)
         if (err /= CAPNP_OK) return
         if (nm == jv%str) then
            ord = i
            return
         end if
      end do
      err = CAPNP_ERR_ARG
   end subroutine j_enum

   recursive subroutine parse_value(sc, jv, depth, err)
      type(scan_t), intent(inout) :: sc
      type(jval), intent(out) :: jv
      integer, intent(in) :: depth
      integer, intent(out) :: err
      character(len=1) :: c
      err = CAPNP_OK
      if (depth > MAX_DEPTH) then
         err = CAPNP_ERR_DEPTH
         return
      end if
      call jskip(sc)
      c = jpeek(sc)
      if (c == '{') then
         call parse_object(sc, jv, depth, err)
      else if (c == '[') then
         call parse_array(sc, jv, depth, err)
      else if (c == '"') then
         jv%k = JK_STR
         call parse_string(sc, jv%str, err)
      else if (c == 't' .or. c == 'f' .or. c == 'n') then
         call parse_word(sc, jv, err)
      else
         jv%k = JK_NUM
         call parse_number(sc, jv, err)
      end if
   end subroutine parse_value

   recursive subroutine parse_object(sc, jv, depth, err)
      type(scan_t), intent(inout) :: sc
      type(jval), intent(out) :: jv
      integer, intent(in) :: depth
      integer, intent(out) :: err
      type(jval) :: one
      character(len=:), allocatable :: key
      integer :: n
      err = CAPNP_OK
      jv%k = JK_OBJ
      sc%i = sc%i + 1
      n = 0
      call jskip(sc)
      if (jpeek(sc) == '}') then
         sc%i = sc%i + 1
         return
      end if
      do
         if (jpeek(sc) /= '"') then
            err = CAPNP_ERR_ARG
            return
         end if
         call parse_string(sc, key, err)
         if (err /= CAPNP_OK) return
         call jskip(sc)
         if (jpeek(sc) /= ':') then
            err = CAPNP_ERR_ARG
            return
         end if
         sc%i = sc%i + 1
         call parse_value(sc, one, depth + 1, err)
         if (err /= CAPNP_OK) return
         one%key = key
         call append_kid(jv%kids, n, one, err)
         if (err /= CAPNP_OK) return
         call jskip(sc)
         if (jpeek(sc) == ',') then
            sc%i = sc%i + 1
            call jskip(sc)
            cycle
         else if (jpeek(sc) == '}') then
            sc%i = sc%i + 1
            exit
         else
            err = CAPNP_ERR_ARG
            return
         end if
      end do
      call trim_kids(jv%kids, n, err)
      if (err /= CAPNP_OK) return
   end subroutine parse_object

   recursive subroutine parse_array(sc, jv, depth, err)
      type(scan_t), intent(inout) :: sc
      type(jval), intent(out) :: jv
      integer, intent(in) :: depth
      integer, intent(out) :: err
      type(jval) :: one
      integer :: n
      err = CAPNP_OK
      jv%k = JK_ARR
      sc%i = sc%i + 1
      n = 0
      call jskip(sc)
      if (jpeek(sc) == ']') then
         sc%i = sc%i + 1
         return
      end if
      do
         call parse_value(sc, one, depth + 1, err)
         if (err /= CAPNP_OK) return
         call append_kid(jv%kids, n, one, err)
         if (err /= CAPNP_OK) return
         call jskip(sc)
         if (jpeek(sc) == ',') then
            sc%i = sc%i + 1
            call jskip(sc)
            cycle
         else if (jpeek(sc) == ']') then
            sc%i = sc%i + 1
            exit
         else
            err = CAPNP_ERR_ARG
            return
         end if
      end do
      call trim_kids(jv%kids, n, err)
      if (err /= CAPNP_OK) return
   end subroutine parse_array

   subroutine quoted_span(sc, cap, err)
      type(scan_t), intent(in) :: sc
      integer, intent(out) :: cap
      integer, intent(out) :: err
      integer :: i
      character(len=1) :: c
      err = CAPNP_OK
      cap = 1
      if (jpeek(sc) /= '"') then
         err = CAPNP_ERR_ARG
         return
      end if
      i = sc%i + 1
      do
         if (i > sc%n) then
            cap = max(1, sc%n - sc%i)
            return
         end if
         c = sc%s(i:i)
         if (c == '"') then
            cap = max(1, i - (sc%i + 1))
            return
         else if (c == '\') then
            i = i + 2
         else
            i = i + 1
         end if
      end do
   end subroutine quoted_span

   subroutine parse_string(sc, s, err)
      type(scan_t), intent(inout) :: sc
      character(len=:), allocatable, intent(out) :: s
      integer, intent(out) :: err
      character(len=:), allocatable :: buf
      integer :: cp, low, n, cap, stat
      character(len=1) :: c
      err = CAPNP_OK
      s = ''
      if (jpeek(sc) /= '"') then
         err = CAPNP_ERR_ARG
         return
      end if
      call quoted_span(sc, cap, err)
      if (err /= CAPNP_OK) return
      if (cap < 1) cap = 1
      allocate (character(len=cap) :: buf, stat=stat)
      if (stat /= 0) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      sc%i = sc%i + 1
      n = 0
      do
         if (sc%i > sc%n) then
            err = CAPNP_ERR_ARG
            return
         end if
         c = sc%s(sc%i:sc%i)
         sc%i = sc%i + 1
         if (c == '"') then
            if (n == 0) then
               s = ''
            else
               s = buf(1:n)
            end if
            return
         end if
         if (c /= '\') then
            call buf_putc(buf, n, c, err)
            if (err /= CAPNP_OK) return
            cycle
         end if
         if (sc%i > sc%n) then
            err = CAPNP_ERR_ARG
            return
         end if
         c = sc%s(sc%i:sc%i)
         sc%i = sc%i + 1
         select case (c)
         case ('"'); call buf_putc(buf, n, '"', err)
         case ('\'); call buf_putc(buf, n, '\', err)
         case ('/'); call buf_putc(buf, n, '/', err)
         case ('b'); call buf_putc(buf, n, achar(8), err)
         case ('f'); call buf_putc(buf, n, achar(12), err)
         case ('n'); call buf_putc(buf, n, achar(10), err)
         case ('r'); call buf_putc(buf, n, achar(13), err)
         case ('t'); call buf_putc(buf, n, achar(9), err)
         case ('u')
            call read_u4(sc, cp, err)
            if (err /= CAPNP_OK) return
            if (cp >= 55296 .and. cp <= 56319) then
               if (peek_u4(sc, low)) then
                  if (low >= 56320 .and. low <= 57343) then
                     sc%i = sc%i + 6
                     cp = 65536 + ishft(cp - 55296, 10) + (low - 56320)
                  end if
               end if
            end if
            call append_utf8(buf, n, cp, err)
         case default
            err = CAPNP_ERR_ARG
            return
         end select
         if (err /= CAPNP_OK) return
      end do
   end subroutine parse_string

   subroutine read_u4(sc, cp, err)
      type(scan_t), intent(inout) :: sc
      integer, intent(out) :: cp
      integer, intent(out) :: err
      integer :: h, k
      err = CAPNP_OK
      cp = 0
      do k = 1, 4
         if (sc%i > sc%n) then
            err = CAPNP_ERR_ARG
            return
         end if
         h = capnp_hex_digit(sc%s(sc%i:sc%i))
         if (h < 0) then
            err = CAPNP_ERR_ARG
            return
         end if
         cp = cp*16 + h
         sc%i = sc%i + 1
      end do
   end subroutine read_u4

   function peek_u4(sc, cp) result(y)
      type(scan_t), intent(in) :: sc
      integer, intent(out) :: cp
      logical :: y
      integer :: h, k
      y = .false.
      cp = 0
      if (sc%i + 5 > sc%n) return
      if (sc%s(sc%i:sc%i + 1) /= '\u') return
      do k = 1, 4
         h = capnp_hex_digit(sc%s(sc%i + 1 + k:sc%i + 1 + k))
         if (h < 0) return
         cp = cp*16 + h
      end do
      y = .true.
   end function peek_u4

   subroutine parse_word(sc, jv, err)
      type(scan_t), intent(inout) :: sc
      type(jval), intent(inout) :: jv
      integer, intent(out) :: err
      err = CAPNP_OK
      if (sc%i + 3 <= sc%n) then
         if (sc%s(sc%i:sc%i + 3) == 'true') then
            jv%k = JK_BOOL
            jv%b = .true.
            sc%i = sc%i + 4
            return
         else if (sc%s(sc%i:sc%i + 3) == 'null') then
            jv%k = JK_NULL
            sc%i = sc%i + 4
            return
         end if
      end if
      if (sc%i + 4 <= sc%n) then
         if (sc%s(sc%i:sc%i + 4) == 'false') then
            jv%k = JK_BOOL
            jv%b = .false.
            sc%i = sc%i + 5
            return
         end if
      end if
      err = CAPNP_ERR_ARG
   end subroutine parse_word

   subroutine parse_number(sc, jv, err)
      type(scan_t), intent(inout) :: sc
      type(jval), intent(inout) :: jv
      integer, intent(out) :: err
      integer :: a
      err = CAPNP_OK
      a = sc%i
      if (jpeek(sc) == '-') sc%i = sc%i + 1
      if (capnp_dec_digit(jpeek(sc)) < 0) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (jpeek(sc) == '0') then
         sc%i = sc%i + 1
      else
         do while (capnp_dec_digit(jpeek(sc)) >= 0)
            sc%i = sc%i + 1
         end do
      end if
      if (jpeek(sc) == '.') then
         sc%i = sc%i + 1
         if (capnp_dec_digit(jpeek(sc)) < 0) then
            err = CAPNP_ERR_ARG
            return
         end if
         do while (capnp_dec_digit(jpeek(sc)) >= 0)
            sc%i = sc%i + 1
         end do
      end if
      if (jpeek(sc) == 'e' .or. jpeek(sc) == 'E') then
         sc%i = sc%i + 1
         if (jpeek(sc) == '+' .or. jpeek(sc) == '-') sc%i = sc%i + 1
         if (capnp_dec_digit(jpeek(sc)) < 0) then
            err = CAPNP_ERR_ARG
            return
         end if
         do while (capnp_dec_digit(jpeek(sc)) >= 0)
            sc%i = sc%i + 1
         end do
      end if
      jv%str = sc%s(a:sc%i - 1)
      call capnp_parse_f64(jv%str, jv%num, err)
   end subroutine parse_number

   subroutine bytes_from_array(jv, raw, err)
      type(jval), intent(in) :: jv
      integer(int8), allocatable, intent(out) :: raw(:)
      integer, intent(out) :: err
      integer :: i, n, u, stat
      err = CAPNP_OK
      if (jv%k /= JK_ARR) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (.not. allocated(jv%kids)) then
         allocate (raw(0), stat=stat)
         if (stat /= 0) err = CAPNP_ERR_ALLOC
         return
      end if
      n = size(jv%kids)
      if (n == 0) then
         allocate (raw(0), stat=stat)
         if (stat /= 0) err = CAPNP_ERR_ALLOC
         return
      end if
      allocate (raw(0:n - 1), stat=stat)
      if (stat /= 0) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      do i = 1, n
         if (jv%kids(i)%k /= JK_NUM) then
            err = CAPNP_ERR_ARG
            return
         end if
         if (jv%kids(i)%num < 0.0_real64 .or. jv%kids(i)%num > 255.0_real64) then
            err = CAPNP_ERR_ARG
            return
         end if
         u = int(jv%kids(i)%num)
         if (abs(jv%kids(i)%num - real(u, real64)) > 0.0_real64) then
            err = CAPNP_ERR_ARG
            return
         end if
         raw(i - 1) = capnp_as_i8(u)
      end do
   end subroutine bytes_from_array

   subroutine json_quote(s, out, err)
      character(len=*), intent(in) :: s
      character(len=:), allocatable, intent(out) :: out
      integer, intent(out) :: err
      character(len=:), allocatable :: buf
      integer :: i, m, ub, cap, stat
      character(len=2) :: hh
      character(len=*), parameter :: hexd = '0123456789abcdef'
      err = CAPNP_OK
      out = ''
      if (int(len(s), int64)*6_int64 + 2_int64 > int(huge(cap), int64)) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      cap = len(s)*6 + 2
      allocate (character(len=cap) :: buf, stat=stat)
      if (stat /= 0) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      buf(1:1) = '"'
      m = 1
      do i = 1, len(s)
         ub = iachar(s(i:i))
         select case (ub)
         case (34); buf(m + 1:m + 2) = '\"'; m = m + 2
         case (92); buf(m + 1:m + 2) = '\\'; m = m + 2
         case (8); buf(m + 1:m + 2) = '\b'; m = m + 2
         case (12); buf(m + 1:m + 2) = '\f'; m = m + 2
         case (10); buf(m + 1:m + 2) = '\n'; m = m + 2
         case (13); buf(m + 1:m + 2) = '\r'; m = m + 2
         case (9); buf(m + 1:m + 2) = '\t'; m = m + 2
         case default
            if (ub < 32) then
               hh(1:1) = hexd(ub/16 + 1:ub/16 + 1)
               hh(2:2) = hexd(mod(ub, 16) + 1:mod(ub, 16) + 1)
               buf(m + 1:m + 6) = '\u00'//hh
               m = m + 6
            else
               m = m + 1
               buf(m:m) = s(i:i)
            end if
         end select
      end do
      m = m + 1
      buf(m:m) = '"'
      out = buf(1:m)
   end subroutine json_quote

   subroutine b64_encode(raw, s, err)
      integer(int8), intent(in) :: raw(:)
      character(len=:), allocatable, intent(out) :: s
      integer, intent(out) :: err
      integer :: i, n, a, b, c, v, p, m, lb, stat
      err = CAPNP_OK
      n = size(raw)
      if (n <= 0) then
         s = ''
         return
      end if
      if (int(n, int64)*2_int64 > int(huge(n), int64)) then
         err = CAPNP_ERR_ALLOC
         s = ''
         return
      end if
      m = (n/3)*4
      if (mod(n, 3) /= 0) m = m + 4
      allocate (character(len=m) :: s, stat=stat)
      if (stat /= 0) then
         err = CAPNP_ERR_ALLOC
         s = ''
         return
      end if
      lb = lbound(raw, 1)
      p = 1
      i = 0
      do while (i + 2 < n)
         a = capnp_u_of_i8(raw(lb + i))
         b = capnp_u_of_i8(raw(lb + i + 1))
         c = capnp_u_of_i8(raw(lb + i + 2))
         v = ior(ishft(a, 16), ior(ishft(b, 8), c))
         s(p:p) = B64(ishft(v, -18) + 1:ishft(v, -18) + 1)
         s(p + 1:p + 1) = B64(iand(ishft(v, -12), 63) + 1:iand(ishft(v, -12), 63) + 1)
         s(p + 2:p + 2) = B64(iand(ishft(v, -6), 63) + 1:iand(ishft(v, -6), 63) + 1)
         s(p + 3:p + 3) = B64(iand(v, 63) + 1:iand(v, 63) + 1)
         p = p + 4
         i = i + 3
      end do
      if (i < n) then
         a = capnp_u_of_i8(raw(lb + i))
         if (i + 1 == n) then
            v = ishft(a, 16)
            s(p:p) = B64(ishft(v, -18) + 1:ishft(v, -18) + 1)
            s(p + 1:p + 1) = B64(iand(ishft(v, -12), 63) + 1:iand(ishft(v, -12), 63) + 1)
            s(p + 2:p + 3) = '=='
         else
            b = capnp_u_of_i8(raw(lb + i + 1))
            v = ior(ishft(a, 16), ishft(b, 8))
            s(p:p) = B64(ishft(v, -18) + 1:ishft(v, -18) + 1)
            s(p + 1:p + 1) = B64(iand(ishft(v, -12), 63) + 1:iand(ishft(v, -12), 63) + 1)
            s(p + 2:p + 2) = B64(iand(ishft(v, -6), 63) + 1:iand(ishft(v, -6), 63) + 1)
            s(p + 3:p + 3) = '='
         end if
      end if
   end subroutine b64_encode

   subroutine b64_decode(s, raw, err)
      character(len=*), intent(in) :: s
      integer(int8), allocatable, intent(out) :: raw(:)
      integer, intent(out) :: err
      integer :: i, n, v, nd, a, b, c, d, outn, stat
      integer, allocatable :: vals(:)
      character(len=1) :: ch
      err = CAPNP_OK
      n = len(s)
      allocate (vals(n), stat=stat)
      if (stat /= 0) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      nd = 0
      do i = 1, n
         ch = s(i:i)
         if (ch == '=' .or. ch == ' ' .or. ch == achar(9) .or. ch == achar(10) .or. ch == achar(13)) cycle
         v = index(B64, ch) - 1
         if (v < 0) then
            err = CAPNP_ERR_ARG
            return
         end if
         nd = nd + 1
         vals(nd) = v
      end do
      if (mod(nd, 4) == 1) then
         err = CAPNP_ERR_ARG
         return
      end if
      outn = nd/4*3
      if (mod(nd, 4) == 2) outn = outn + 1
      if (mod(nd, 4) == 3) outn = outn + 2
      if (outn == 0) then
         allocate (raw(0), stat=stat)
         if (stat /= 0) err = CAPNP_ERR_ALLOC
         return
      end if
      allocate (raw(0:outn - 1), stat=stat)
      if (stat /= 0) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      outn = 0
      i = 1
      do while (i + 3 <= nd)
         a = vals(i); b = vals(i + 1); c = vals(i + 2); d = vals(i + 3)
         v = ior(ishft(a, 18), ior(ishft(b, 12), ior(ishft(c, 6), d)))
         raw(outn) = capnp_as_i8(iand(ishft(v, -16), 255))
         raw(outn + 1) = capnp_as_i8(iand(ishft(v, -8), 255))
         raw(outn + 2) = capnp_as_i8(iand(v, 255))
         outn = outn + 3
         i = i + 4
      end do
      if (i + 1 == nd) then
         v = ior(ishft(vals(i), 18), ishft(vals(i + 1), 12))
         raw(outn) = capnp_as_i8(iand(ishft(v, -16), 255))
      else if (i + 2 == nd) then
         v = ior(ishft(vals(i), 18), ior(ishft(vals(i + 1), 12), ishft(vals(i + 2), 6)))
         raw(outn) = capnp_as_i8(iand(ishft(v, -16), 255))
         raw(outn + 1) = capnp_as_i8(iand(ishft(v, -8), 255))
      end if
   end subroutine b64_decode

   subroutine hex_encode(raw, s, err)
      integer(int8), intent(in) :: raw(:)
      character(len=:), allocatable, intent(out) :: s
      integer, intent(out) :: err
      integer :: i, n, u, lb, stat
      character(len=*), parameter :: hexd = '0123456789abcdef'
      err = CAPNP_OK
      n = size(raw)
      if (n <= 0) then
         s = ''
         return
      end if
      if (int(n, int64)*2_int64 > int(huge(n), int64)) then
         err = CAPNP_ERR_ALLOC
         s = ''
         return
      end if
      allocate (character(len=n*2) :: s, stat=stat)
      if (stat /= 0) then
         err = CAPNP_ERR_ALLOC
         s = ''
         return
      end if
      lb = lbound(raw, 1)
      do i = 0, n - 1
         u = capnp_u_of_i8(raw(lb + i))
         s(2*i + 1:2*i + 1) = hexd(u/16 + 1:u/16 + 1)
         s(2*i + 2:2*i + 2) = hexd(mod(u, 16) + 1:mod(u, 16) + 1)
      end do
   end subroutine hex_encode

   subroutine hex_decode(s, raw, err)
      character(len=*), intent(in) :: s
      integer(int8), allocatable, intent(out) :: raw(:)
      integer, intent(out) :: err
      integer :: i, n, hi, lo, stat
      err = CAPNP_OK
      n = len(s)
      if (mod(n, 2) /= 0) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (n == 0) then
         allocate (raw(0), stat=stat)
         if (stat /= 0) err = CAPNP_ERR_ALLOC
         return
      end if
      allocate (raw(0:n/2 - 1), stat=stat)
      if (stat /= 0) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      do i = 1, n/2
         hi = capnp_hex_digit(s(2*i - 1:2*i - 1))
         lo = capnp_hex_digit(s(2*i:2*i))
         if (hi < 0 .or. lo < 0) then
            err = CAPNP_ERR_ARG
            return
         end if
         raw(i - 1) = capnp_as_i8(hi*16 + lo)
      end do
   end subroutine hex_decode

   subroutine buf_putc(buf, n, c, err)
      character(len=*), intent(inout) :: buf
      integer, intent(inout) :: n
      character(len=1), intent(in) :: c
      integer, intent(inout) :: err
      if (n >= len(buf)) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      n = n + 1
      buf(n:n) = c
   end subroutine buf_putc

   subroutine append_utf8(buf, n, cp, err)
      character(len=*), intent(inout) :: buf
      integer, intent(inout) :: n
      integer, intent(in) :: cp
      integer, intent(inout) :: err
      integer :: need
      if (cp < 0 .or. cp > 1114111) then
         err = CAPNP_ERR_ARG
         return
      else if (cp <= 127) then
         need = 1
      else if (cp <= 2047) then
         need = 2
      else if (cp <= 65535) then
         need = 3
      else
         need = 4
      end if
      if (n > len(buf) - need) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      if (cp <= 127) then
         n = n + 1
         buf(n:n) = achar(cp)
      else if (cp <= 2047) then
         buf(n + 1:n + 1) = achar(ior(192, ishft(cp, -6)))
         buf(n + 2:n + 2) = achar(ior(128, iand(cp, 63)))
         n = n + 2
      else if (cp <= 65535) then
         buf(n + 1:n + 1) = achar(ior(224, ishft(cp, -12)))
         buf(n + 2:n + 2) = achar(ior(128, iand(ishft(cp, -6), 63)))
         buf(n + 3:n + 3) = achar(ior(128, iand(cp, 63)))
         n = n + 3
      else
         buf(n + 1:n + 1) = achar(ior(240, ishft(cp, -18)))
         buf(n + 2:n + 2) = achar(ior(128, iand(ishft(cp, -12), 63)))
         buf(n + 3:n + 3) = achar(ior(128, iand(ishft(cp, -6), 63)))
         buf(n + 4:n + 4) = achar(ior(128, iand(cp, 63)))
         n = n + 4
      end if
   end subroutine append_utf8

   subroutine json_field_name(f, name, err)
      type(capnp_ptr_t), intent(in) :: f
      character(len=:), allocatable, intent(out) :: name
      integer, intent(out) :: err
      type(capnp_ptr_t) :: al
      call field_name(f, name, err)
      if (err /= CAPNP_OK) return
      al = field_annotations(f, err)
      if (err /= CAPNP_OK) return
      call name_override(al, name, err)
   end subroutine json_field_name

   subroutine name_override(al, name, err)
      type(capnp_ptr_t), intent(in) :: al
      character(len=:), allocatable, intent(inout) :: name
      integer, intent(out) :: err
      type(capnp_ptr_t) :: a, v
      character(len=:), allocatable :: alt
      integer :: i, n
      err = CAPNP_OK
      n = int(capnp_list_len(al))
      do i = 0, n - 1
         a = capnp_list_get_struct(al, i, err)
         if (err /= CAPNP_OK) return
         if (annotation_id(a) == ANN_NAME) then
            v = annotation_value(a, err)
            if (err /= CAPNP_OK) return
            call value_text(v, alt, err)
            if (err /= CAPNP_OK) return
            name = alt
            return
         end if
      end do
   end subroutine name_override

   subroutine flatten_opts(f, is_flat, pfx, err)
      type(capnp_ptr_t), intent(in) :: f
      logical, intent(out) :: is_flat
      character(len=:), allocatable, intent(out) :: pfx
      integer, intent(out) :: err
      type(capnp_ptr_t) :: al, a, v, opt, qp
      integer :: i, n
      err = CAPNP_OK
      is_flat = .false.
      pfx = ''
      al = field_annotations(f, err)
      if (err /= CAPNP_OK) return
      n = int(capnp_list_len(al))
      do i = 0, n - 1
         a = capnp_list_get_struct(al, i, err)
         if (err /= CAPNP_OK) return
         if (annotation_id(a) == ANN_FLATTEN) then
            is_flat = .true.
            v = annotation_value(a, err)
            if (err /= CAPNP_OK) return
            opt = value_pointer(v, err)
            if (err /= CAPNP_OK) return
            if (opt%kind /= CAPNP_PK_NULL) then
               qp = capnp_getp(opt, 0, err)
               if (err /= CAPNP_OK) return
               if (qp%kind /= CAPNP_PK_NULL) call capnp_get_text(opt, 0, pfx, err)
            end if
            return
         end if
      end do
   end subroutine flatten_opts

   subroutine read_disc_opts(al, opts, err)
      type(capnp_ptr_t), intent(in) :: al
      type(disc_opts_t), intent(out) :: opts
      integer, intent(out) :: err
      type(capnp_ptr_t) :: a, v, opt, q0, q1
      integer :: i, n
      err = CAPNP_OK
      opts%active = .false.
      opts%has_name = .false.
      opts%has_val = .false.
      n = int(capnp_list_len(al))
      do i = 0, n - 1
         a = capnp_list_get_struct(al, i, err)
         if (err /= CAPNP_OK) return
         if (annotation_id(a) == ANN_DISCRIMINATOR) then
            opts%active = .true.
            v = annotation_value(a, err)
            if (err /= CAPNP_OK) return
            opt = value_pointer(v, err)
            if (err /= CAPNP_OK) return
            if (opt%kind /= CAPNP_PK_NULL) then
               q0 = capnp_getp(opt, 0, err)
               if (err /= CAPNP_OK) return
               if (q0%kind /= CAPNP_PK_NULL) then
                  opts%has_name = .true.
                  call capnp_get_text(opt, 0, opts%name, err)
                  if (err /= CAPNP_OK) return
               end if
               q1 = capnp_getp(opt, 1, err)
               if (err /= CAPNP_OK) return
               if (q1%kind /= CAPNP_PK_NULL) then
                  opts%has_val = .true.
                  call capnp_get_text(opt, 1, opts%val_name, err)
                  if (err /= CAPNP_OK) return
               end if
            end if
            return
         end if
      end do
   end subroutine read_disc_opts

   function data_mode(f, err) result(mode)
      type(capnp_ptr_t), intent(in) :: f
      integer, intent(out) :: err
      integer :: mode
      type(capnp_ptr_t) :: al, a
      integer :: i, n
      err = CAPNP_OK
      mode = 0
      if (field_which(f) /= FIELD_SLOT) return
      al = field_annotations(f, err)
      if (err /= CAPNP_OK) return
      n = int(capnp_list_len(al))
      do i = 0, n - 1
         a = capnp_list_get_struct(al, i, err)
         if (err /= CAPNP_OK) return
         if (annotation_id(a) == ANN_B64) mode = 1
         if (annotation_id(a) == ANN_HEX) mode = 2
      end do
   end function data_mode

   function find_jkey(jv, key, idx) result(y)
      type(jval), intent(in) :: jv
      character(len=*), intent(in) :: key
      integer, intent(out) :: idx
      logical :: y
      integer :: i
      y = .false.
      idx = 0
      if (.not. allocated(jv%kids)) return
      do i = size(jv%kids), 1, -1
         if (jv%kids(i)%key == key) then
            idx = i
            y = .true.
            return
         end if
      end do
   end function find_jkey

   function active_union_field(schema, idx, p, f) result(y)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      type(capnp_ptr_t), intent(in) :: p
      type(capnp_ptr_t), intent(out) :: f
      logical :: y
      type(capnp_ptr_t) :: fl
      integer :: w, i, n, err
      y = .false.
      w = capnp_which(p, int(node_struct_discriminant_offset(schema%nodes(idx))))
      fl = node_struct_fields(schema%nodes(idx), err)
      if (err /= CAPNP_OK) return
      n = int(capnp_list_len(fl))
      do i = 0, n - 1
         f = capnp_list_get_struct(fl, i, err)
         if (err /= CAPNP_OK) return
         if (field_discriminant(f) == w) then
            y = .true.
            return
         end if
      end do
   end function active_union_field

   function union_arm_void(f, env) result(y)
      type(capnp_ptr_t), intent(in) :: f
      type(capnp_brand_env_t), intent(in) :: env
      logical :: y
      type(capnp_ptr_t) :: raw_t, t
      integer :: err
      y = .false.
      if (field_which(f) /= FIELD_SLOT) return
      raw_t = field_slot_type(f, err)
      if (err /= CAPNP_OK) return
      t = capnp_brand_resolve(env, raw_t)
      y = type_which(t) == TYPE_VOID
   end function union_arm_void

   subroutine flat_union_owns(schema, idx, env, pfx, key, depth, owned, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx, depth
      type(capnp_brand_env_t), intent(in) :: env
      character(len=*), intent(in) :: pfx, key
      logical, intent(out) :: owned
      integer, intent(out) :: err
      type(capnp_ptr_t) :: fl, f, al, raw_t, t
      type(capnp_brand_env_t) :: cenv
      type(disc_opts_t) :: gdisc, no_disc
      type(jval) :: one
      character(len=:), allocatable :: raw_nm, fpfx
      integer :: i, n, gidx, sidx, stat
      logical :: is_flat, hit
      owned = .false.
      err = CAPNP_OK
      if (len(key) == 0) return
      one%k = JK_OBJ
      allocate (one%kids(1), stat=stat)
      if (stat /= 0) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      one%kids(1)%k = JK_NULL
      one%kids(1)%key = key
      fl = node_struct_fields(schema%nodes(idx), err)
      if (err /= CAPNP_OK) return
      n = int(capnp_list_len(fl))
      do i = 0, n - 1
         f = capnp_list_get_struct(fl, i, err)
         if (err /= CAPNP_OK) return
         if (field_discriminant(f) == NO_DISCRIMINANT) cycle
         call flatten_opts(f, is_flat, fpfx, err)
         if (err /= CAPNP_OK) return
         if (.not. is_flat) cycle
         hit = .false.
         if (field_which(f) == FIELD_GROUP) then
            gidx = capnp_codec_find_id(schema, field_group_type_id(f))
            if (gidx == 0) cycle
            call field_name(f, raw_nm, err)
            if (err /= CAPNP_OK) return
            al = field_annotations(f, err)
            if (err /= CAPNP_OK) return
            call read_disc_opts(al, gdisc, err)
            if (err /= CAPNP_OK) return
            call struct_matches_any(schema, gidx, env, pfx//fpfx, gdisc, raw_nm, one, &
                                    depth + 1, hit, err)
         else
            raw_t = field_slot_type(f, err)
            if (err /= CAPNP_OK) return
            t = capnp_brand_resolve(env, raw_t)
            if (type_which(t) /= TYPE_STRUCT) cycle
            sidx = capnp_codec_find_id(schema, type_type_id(t))
            if (sidx == 0) cycle
            call capnp_brand_enter(env, t, cenv, err)
            if (err /= CAPNP_OK) return
            call struct_matches_any(schema, sidx, cenv, pfx//fpfx, no_disc, '', one, &
                                    depth + 1, hit, err)
         end if
         if (err /= CAPNP_OK) return
         if (hit) then
            owned = .true.
            return
         end if
      end do
   end subroutine flat_union_owns

   function find_union_json_field(schema, idx, key, f) result(y)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      character(len=*), intent(in) :: key
      type(capnp_ptr_t), intent(out) :: f
      logical :: y
      type(capnp_ptr_t) :: fl
      character(len=:), allocatable :: nm
      integer :: i, n, err
      y = .false.
      fl = node_struct_fields(schema%nodes(idx), err)
      if (err /= CAPNP_OK) return
      n = int(capnp_list_len(fl))
      do i = 0, n - 1
         f = capnp_list_get_struct(fl, i, err)
         if (err /= CAPNP_OK) return
         if (field_discriminant(f) == NO_DISCRIMINANT) cycle
         call json_field_name(f, nm, err)
         if (err /= CAPNP_OK) return
         if (nm == key) then
            y = .true.
            return
         end if
      end do
   end function find_union_json_field

   subroutine push(items, n, s, multi, err)
      type(piece_t), allocatable, intent(inout) :: items(:)
      integer, intent(inout) :: n
      character(len=*), intent(in) :: s
      logical, intent(in) :: multi
      integer, intent(out) :: err
      type(piece_t), allocatable :: tmp(:)
      integer :: stat
      err = CAPNP_OK
      if (.not. allocated(items)) then
         allocate (items(8), stat=stat)
         if (stat /= 0) then
            err = CAPNP_ERR_ALLOC
            return
         end if
      end if
      if (n >= size(items)) then
         allocate (tmp(size(items)*2), stat=stat)
         if (stat /= 0) then
            err = CAPNP_ERR_ALLOC
            return
         end if
         if (n > 0) tmp(1:n) = items(1:n)
         call move_alloc(tmp, items)
      end if
      n = n + 1
      items(n)%s = s
      items(n)%multi = multi
   end subroutine push

   subroutine take_jval(dst, src)
      type(jval), intent(inout) :: dst, src
      dst%k = src%k
      dst%b = src%b
      dst%num = src%num
      if (allocated(src%str)) call move_alloc(src%str, dst%str)
      if (allocated(src%key)) call move_alloc(src%key, dst%key)
      if (allocated(src%kids)) call move_alloc(src%kids, dst%kids)
      src%k = JK_NULL
   end subroutine take_jval

   subroutine append_kid(kids, n, src, err)
      type(jval), allocatable, intent(inout) :: kids(:)
      integer, intent(inout) :: n
      type(jval), intent(inout) :: src
      integer, intent(out) :: err
      type(jval), allocatable :: tmp(:)
      integer :: i, cap, stat
      err = CAPNP_OK
      if (.not. allocated(kids)) then
         allocate (kids(4), stat=stat)
         if (stat /= 0) then
            err = CAPNP_ERR_ALLOC
            return
         end if
      end if
      if (n >= size(kids)) then
         cap = size(kids)*2
         allocate (tmp(cap), stat=stat)
         if (stat /= 0) then
            err = CAPNP_ERR_ALLOC
            return
         end if
         do i = 1, n
            call take_jval(tmp(i), kids(i))
         end do
         call move_alloc(tmp, kids)
      end if
      n = n + 1
      call take_jval(kids(n), src)
   end subroutine append_kid

   subroutine trim_kids(kids, n, err)
      type(jval), allocatable, intent(inout) :: kids(:)
      integer, intent(in) :: n
      integer, intent(out) :: err
      type(jval), allocatable :: tmp(:)
      integer :: i, stat
      err = CAPNP_OK
      if (n <= 0) then
         if (allocated(kids)) deallocate (kids)
         return
      end if
      allocate (tmp(n), stat=stat)
      if (stat /= 0) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      do i = 1, n
         call take_jval(tmp(i), kids(i))
      end do
      call move_alloc(tmp, kids)
   end subroutine trim_kids

   subroutine jjoin(items, n, indent, has_prefix, pretty, multi, out, err)
      type(piece_t), allocatable, intent(in) :: items(:)
      integer, intent(in) :: n, indent
      logical, intent(in) :: has_prefix, pretty
      logical, intent(inout) :: multi
      character(len=:), allocatable, intent(out) :: out
      integer, intent(out) :: err
      integer :: i, mx
      logical :: any_multi
      character(len=:), allocatable :: delim, prefix, suffix, sp
      type(buf_t) :: buf
      err = CAPNP_OK
      out = ''
      any_multi = .false.
      mx = 0
      if (allocated(items)) then
         do i = 1, n
            if (items(i)%multi) any_multi = .true.
            mx = max(mx, len(items(i)%s))
         end do
      end if
      if (.not. pretty) then
         delim = ','
         prefix = ''
         suffix = ''
      else if (n > 1 .and. (any_multi .or. mx > 50)) then
         sp = repeat(' ', (indent + 1)*2)
         delim = ','//achar(10)//sp
         multi = .true.
         if (has_prefix) then
            prefix = achar(10)//sp
         else
            prefix = ' '
         end if
         suffix = ' '
      else
         delim = ', '
         prefix = ''
         suffix = ''
      end if
      if (n <= 0) then
         out = prefix//suffix
         return
      end if
      call buf_add(buf, prefix//items(1)%s, err)
      if (err /= CAPNP_OK) return
      do i = 2, n
         call buf_add(buf, delim//items(i)%s, err)
         if (err /= CAPNP_OK) return
      end do
      call buf_add(buf, suffix, err)
      if (err /= CAPNP_OK) return
      call buf_take(buf, out)
   end subroutine jjoin

   subroutine jskip(sc)
      type(scan_t), intent(inout) :: sc
      character(len=1) :: c
      do
         if (sc%i > sc%n) return
         c = sc%s(sc%i:sc%i)
         if (c == ' ' .or. c == achar(9) .or. c == achar(10) .or. c == achar(13)) then
            sc%i = sc%i + 1
         else
            return
         end if
      end do
   end subroutine jskip

   function jpeek(sc) result(c)
      type(scan_t), intent(in) :: sc
      character(len=1) :: c
      if (sc%i > sc%n) then
         c = achar(0)
      else
         c = sc%s(sc%i:sc%i)
      end if
   end function jpeek

   function ieee_nan(x) result(y)
      real(real64), intent(in) :: x
      logical :: y
      y = ieee_is_nan(x)
   end function ieee_nan

   function ieee_finite(x) result(y)
      real(real64), intent(in) :: x
      logical :: y
      y = ieee_is_finite(x)
   end function ieee_finite

   subroutine buf_add(buf, piece, err)
      type(buf_t), intent(inout) :: buf
      character(len=*), intent(in) :: piece
      integer, intent(out) :: err
      integer :: need, cap, stat
      character(len=:), allocatable :: tmp
      err = CAPNP_OK
      need = buf%n + len(piece)
      if (need < buf%n) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      if (.not. allocated(buf%s)) then
         cap = max(need, 64)
         allocate (character(len=cap) :: buf%s, stat=stat)
         if (stat /= 0) then
            err = CAPNP_ERR_ALLOC
            return
         end if
      else if (need > len(buf%s)) then
         cap = len(buf%s)
         do while (cap < need)
            if (cap > ishft(huge(cap), -1)) then
               cap = need
               exit
            end if
            cap = cap*2
         end do
         allocate (character(len=cap) :: tmp, stat=stat)
         if (stat /= 0) then
            err = CAPNP_ERR_ALLOC
            return
         end if
         if (buf%n > 0) tmp(1:buf%n) = buf%s(1:buf%n)
         call move_alloc(tmp, buf%s)
      end if
      if (len(piece) > 0) buf%s(buf%n + 1:need) = piece
      buf%n = need
   end subroutine buf_add

   subroutine buf_take(buf, out)
      type(buf_t), intent(in) :: buf
      character(len=:), allocatable, intent(out) :: out
      if (.not. allocated(buf%s) .or. buf%n <= 0) then
         out = ''
      else
         out = buf%s(1:buf%n)
      end if
   end subroutine buf_take

   subroutine guard_traversal(p, limit, err)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: limit
      integer, intent(out) :: err
      integer(int64) :: words
      err = CAPNP_OK
      if (limit < 0_int64) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (.not. associated(p%msg)) return
      if (.not. p%msg%is_builder) return
      words = capnp_total_size(p, err)
      if (err /= CAPNP_OK) return
      if (words > limit) err = CAPNP_ERR_TRAVERSAL
   end subroutine guard_traversal

   !> Reject the name collisions C++ raises while building an annotated
   !> JSON handler: flattened fields, $Json.name, and discriminator keys.
   subroutine audit_json(schema, idx, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      integer, intent(out) :: err
      logical, allocatable :: done(:)
      integer :: stat
      err = CAPNP_OK
      allocate (done(size(schema%nodes)), stat=stat)
      if (stat /= 0) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      done = .false.
      call audit_visit(schema, idx, done, err)
   end subroutine audit_json

   recursive subroutine audit_visit(schema, idx, done, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      logical, intent(inout) :: done(:)
      integer, intent(out) :: err
      type(disc_opts_t) :: no_disc
      type(jname_t), allocatable :: names(:)
      type(capnp_ptr_t) :: fl, f, raw_t
      integer :: stack(128), sp, nn, i, nf, sidx
      err = CAPNP_OK
      if (idx < 1 .or. idx > size(done)) return
      if (done(idx)) return
      if (node_which(schema%nodes(idx)) /= NODE_STRUCT) return
      done(idx) = .true.
      sp = 0
      nn = 0
      call audit_build(schema, idx, no_disc, '', stack, sp, names, nn, err)
      if (err /= CAPNP_OK) return
      fl = node_struct_fields(schema%nodes(idx), err)
      if (err /= CAPNP_OK) return
      nf = int(capnp_list_len(fl))
      do i = 0, nf - 1
         f = capnp_list_get_struct(fl, i, err)
         if (err /= CAPNP_OK) return
         if (field_which(f) == FIELD_GROUP) then
            sidx = capnp_codec_find_id(schema, field_group_type_id(f))
            if (sidx /= 0) call audit_visit(schema, sidx, done, err)
         else
            raw_t = field_slot_type(f, err)
            if (err /= CAPNP_OK) return
            call audit_field_type(schema, raw_t, done, err)
         end if
         if (err /= CAPNP_OK) return
      end do
   end subroutine audit_visit

   recursive subroutine audit_build(schema, idx, parent_disc, union_decl, stack, sp, &
                                    names, nn, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      type(disc_opts_t), intent(in) :: parent_disc
      character(len=*), intent(in) :: union_decl
      integer, intent(inout) :: stack(:)
      integer, intent(inout) :: sp
      type(jname_t), allocatable, intent(out) :: names(:)
      integer, intent(out) :: nn
      integer, intent(out) :: err
      type(disc_opts_t) :: opts, gdisc, no_disc
      type(jname_t), allocatable :: child(:)
      type(capnp_ptr_t) :: fl, f, al, raw_t, t
      character(len=:), allocatable :: nm, raw_nm, fpfx, dkey, udecl
      integer :: i, nf, gidx, sidx, cn, kind, k
      logical :: is_flat, is_union
      err = CAPNP_OK
      nn = 0
      if (idx < 1 .or. idx > size(schema%nodes)) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (sp >= size(stack)) then
         err = CAPNP_ERR_DEPTH
         return
      end if
      do i = 1, sp
         if (stack(i) == idx) then
            err = CAPNP_ERR_ARG
            return
         end if
      end do
      sp = sp + 1
      stack(sp) = idx
      if (parent_disc%active) then
         opts = parent_disc
      else
         al = node_annotations(schema%nodes(idx), err)
         if (err /= CAPNP_OK) then
            sp = sp - 1
            return
         end if
         call read_disc_opts(al, opts, err)
         if (err /= CAPNP_OK) then
            sp = sp - 1
            return
         end if
      end if
      if (opts%active) then
         dkey = ''
         if (opts%has_name) then
            dkey = opts%name
         else if (len(union_decl) > 0) then
            dkey = union_decl
         end if
         if (len(dkey) > 0) then
            call jname_add(names, nn, dkey, JN_TAG, err)
            if (err /= CAPNP_OK) then
               sp = sp - 1
               return
            end if
         end if
         if (opts%has_val) then
            call jname_add(names, nn, opts%val_name, JN_VALUE, err)
            if (err /= CAPNP_OK) then
               sp = sp - 1
               return
            end if
         end if
      end if
      fl = node_struct_fields(schema%nodes(idx), err)
      if (err /= CAPNP_OK) then
         sp = sp - 1
         return
      end if
      nf = int(capnp_list_len(fl))
      do i = 0, nf - 1
         f = capnp_list_get_struct(fl, i, err)
         if (err /= CAPNP_OK) exit
         call json_field_name(f, nm, err)
         if (err /= CAPNP_OK) exit
         call flatten_opts(f, is_flat, fpfx, err)
         if (err /= CAPNP_OK) exit
         is_union = field_discriminant(f) /= NO_DISCRIMINANT
         if (field_which(f) == FIELD_GROUP) then
            gidx = capnp_codec_find_id(schema, field_group_type_id(f))
            if (gidx == 0) then
               err = CAPNP_ERR_ARG
               exit
            end if
            call field_name(f, raw_nm, err)
            if (err /= CAPNP_OK) exit
            al = field_annotations(f, err)
            if (err /= CAPNP_OK) exit
            call read_disc_opts(al, gdisc, err)
            if (err /= CAPNP_OK) exit
            udecl = ''
            if (is_flat) udecl = raw_nm
            call audit_build(schema, gidx, gdisc, udecl, stack, sp, child, cn, err)
            if (err /= CAPNP_OK) exit
            if (is_flat) then
               kind = JN_FLAT
               if (is_union) kind = JN_FLAT_U
               do k = 1, cn
                  call jname_add(names, nn, fpfx//child(k)%key, kind, err)
                  if (err /= CAPNP_OK) exit
               end do
               if (err /= CAPNP_OK) exit
            end if
         else if (is_flat) then
            raw_t = field_slot_type(f, err)
            if (err /= CAPNP_OK) exit
            t = raw_t
            if (type_which(t) /= TYPE_STRUCT) then
               err = CAPNP_ERR_KIND
               exit
            end if
            sidx = capnp_codec_find_id(schema, type_type_id(t))
            if (sidx == 0) then
               err = CAPNP_ERR_ARG
               exit
            end if
            call audit_build(schema, sidx, no_disc, '', stack, sp, child, cn, err)
            if (err /= CAPNP_OK) exit
            kind = JN_FLAT
            if (is_union) kind = JN_FLAT_U
            do k = 1, cn
               call jname_add(names, nn, fpfx//child(k)%key, kind, err)
               if (err /= CAPNP_OK) exit
            end do
            if (err /= CAPNP_OK) exit
         end if
         if (err /= CAPNP_OK) exit
         if (.not. is_flat) then
            if (.not. (is_union .and. opts%active .and. opts%has_val)) then
               call jname_add(names, nn, nm, JN_NORMAL, err)
               if (err /= CAPNP_OK) exit
            end if
         end if
      end do
      sp = sp - 1
   end subroutine audit_build

   recursive subroutine audit_field_type(schema, t, done, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      type(capnp_ptr_t), intent(in) :: t
      logical, intent(inout) :: done(:)
      integer, intent(out) :: err
      type(capnp_ptr_t) :: et
      integer :: sidx
      err = CAPNP_OK
      select case (type_which(t))
      case (TYPE_LIST)
         et = type_list_element(t, err)
         if (err /= CAPNP_OK) return
         call audit_field_type(schema, et, done, err)
      case (TYPE_STRUCT)
         sidx = capnp_codec_find_id(schema, type_type_id(t))
         if (sidx /= 0) call audit_visit(schema, sidx, done, err)
      case (TYPE_ENUM)
         sidx = capnp_codec_find_id(schema, type_type_id(t))
         if (sidx /= 0) call audit_enum(schema, sidx, done, err)
      end select
   end subroutine audit_field_type

   subroutine audit_enum(schema, idx, done, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      logical, intent(inout) :: done(:)
      integer, intent(out) :: err
      type(capnp_ptr_t) :: el, e, al
      type(jname_t), allocatable :: names(:)
      character(len=:), allocatable :: nm
      integer :: i, n, nn
      err = CAPNP_OK
      if (idx < 1 .or. idx > size(done)) return
      if (done(idx)) return
      done(idx) = .true.
      el = node_enumerants(schema%nodes(idx), err)
      if (err /= CAPNP_OK) return
      n = int(capnp_list_len(el))
      nn = 0
      do i = 0, n - 1
         e = capnp_list_get_struct(el, i, err)
         if (err /= CAPNP_OK) return
         call enumerant_name(e, nm, err)
         if (err /= CAPNP_OK) return
         al = enumerant_annotations(e, err)
         if (err /= CAPNP_OK) return
         call name_override(al, nm, err)
         if (err /= CAPNP_OK) return
         call jname_add(names, nn, nm, JN_NORMAL, err)
         if (err /= CAPNP_OK) return
      end do
   end subroutine audit_enum

   subroutine jname_add(names, n, key, kind, err)
      type(jname_t), allocatable, intent(inout) :: names(:)
      integer, intent(inout) :: n
      character(len=*), intent(in) :: key
      integer, intent(in) :: kind
      integer, intent(out) :: err
      type(jname_t), allocatable :: tmp(:)
      integer :: i, cap, stat
      err = CAPNP_OK
      if (allocated(names)) then
         do i = 1, n
            if (names(i)%key == key) then
               if (kind == JN_FLAT_U .and. names(i)%kind == JN_FLAT_U) return
               err = CAPNP_ERR_ARG
               return
            end if
         end do
      end if
      if (.not. allocated(names)) then
         allocate (names(8), stat=stat)
         if (stat /= 0) then
            err = CAPNP_ERR_ALLOC
            return
         end if
      else if (n >= size(names)) then
         cap = size(names)*2
         allocate (tmp(cap), stat=stat)
         if (stat /= 0) then
            err = CAPNP_ERR_ALLOC
            return
         end if
         do i = 1, n
            tmp(i)%key = names(i)%key
            tmp(i)%kind = names(i)%kind
         end do
         call move_alloc(tmp, names)
      end if
      n = n + 1
      names(n)%key = key
      names(n)%kind = kind
   end subroutine jname_add

end module capnp_json
