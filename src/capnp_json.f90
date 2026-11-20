!> JSON codec with the defaults of capnp/compat/json.h.
!>
!> Primitives are always encoded. Null pointers are omitted. Int64 and
!> UInt64 are decimal strings. Data is an array of byte values unless the
!> field carries $Json.base64 or $Json.hex. Enums are names, or the raw
!> number when the ordinal is unknown. $Json.name renames fields and
!> enumerants. NaN and infinities are the strings "NaN", "Infinity", and
!> "-Infinity".
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
   integer(int64), parameter :: ANN_B64 = int(z'D7D879450A253E4B', int64)
   integer(int64), parameter :: ANN_HEX = int(z'F061E22F0AE5C7B5', int64)
   integer, parameter :: MAX_DEPTH = 64
   integer, parameter :: JK_NULL = 0, JK_BOOL = 1, JK_NUM = 2, JK_STR = 3
   integer, parameter :: JK_ARR = 4, JK_OBJ = 5
   character(len=*), parameter :: B64 = &
      'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'

   type :: piece_t
      character(len=:), allocatable :: s
      logical :: multi = .false.
   end type piece_t

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

   subroutine capnp_json_encode(schema, idx, p, text, err, pretty)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      type(capnp_ptr_t), intent(in) :: p
      character(len=:), allocatable, intent(out) :: text
      integer, intent(out) :: err
      logical, intent(in), optional :: pretty
      type(capnp_brand_env_t) :: env
      type(jval) :: root
      logical :: do_pretty, multi
      err = CAPNP_OK
      text = ''
      if (.not. schema%loaded .or. idx < 1 .or. idx > size(schema%nodes)) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (node_which(schema%nodes(idx)) /= NODE_STRUCT) then
         err = CAPNP_ERR_ARG
         return
      end if
      do_pretty = .true.
      if (present(pretty)) do_pretty = pretty
      call build_struct(schema, idx, p, env, 0, root, err)
      if (err /= CAPNP_OK) return
      call emit_jval(root, 0, .false., do_pretty, text, multi)
   end subroutine capnp_json_encode

   subroutine capnp_json_decode(schema, idx, text, msg, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      character(len=*), intent(in) :: text
      type(capnp_message_t), intent(inout), target :: msg
      integer, intent(out) :: err
      type(capnp_brand_env_t) :: env
      type(jval) :: root
      type(capnp_ptr_t) :: obj
      type(scan_t) :: sc
      integer :: dw, pw
      err = CAPNP_OK
      if (.not. schema%loaded .or. idx < 1 .or. idx > size(schema%nodes)) then
         err = CAPNP_ERR_ARG
         return
      end if
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
      call fill_struct(schema, idx, obj, env, root, 0, err)
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

   recursive subroutine build_struct(schema, idx, p, env, depth, jv, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx, depth
      type(capnp_ptr_t), intent(in) :: p
      type(capnp_brand_env_t), intent(in) :: env
      type(jval), intent(out) :: jv
      integer, intent(out) :: err
      type(capnp_ptr_t) :: fl, f
      type(jval) :: one
      character(len=:), allocatable :: nm
      integer :: i, n, nf, active, fd
      logical :: show
      err = CAPNP_OK
      jv%k = JK_OBJ
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
      n = 0
      do i = 0, nf - 1
         f = capnp_list_get_struct(fl, i, err)
         if (err /= CAPNP_OK) return
         if (field_discriminant(f) /= NO_DISCRIMINANT) cycle
         show = capnp_codec_field_present(f, p, err)
         if (err /= CAPNP_OK) return
         if (.not. show) cycle
         call json_field_name(f, nm, err)
         if (err /= CAPNP_OK) return
         call build_field_value(schema, f, p, env, depth + 1, one, err)
         if (err /= CAPNP_OK) return
         one%key = nm
         call append_kid(jv%kids, n, one)
      end do
      if (node_struct_discriminant_count(schema%nodes(idx)) > 0) then
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
            call build_field_value(schema, f, p, env, depth + 1, one, err)
            if (err /= CAPNP_OK) return
            one%key = nm
            call append_kid(jv%kids, n, one)
            exit
         end do
      end if
      call trim_kids(jv%kids, n)
   end subroutine build_struct

   recursive subroutine build_field_value(schema, f, p, env, depth, jv, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      type(capnp_ptr_t), intent(in) :: f, p
      type(capnp_brand_env_t), intent(in) :: env
      integer, intent(in) :: depth
      type(jval), intent(out) :: jv
      integer, intent(out) :: err
      type(capnp_ptr_t) :: raw_t, t, q, et, dv
      type(capnp_brand_env_t) :: cenv
      integer(int8), allocatable :: raw(:)
      integer :: tw, idx, mode
      integer(int64) :: off
      err = CAPNP_OK
      if (field_which(f) == FIELD_GROUP) then
         idx = capnp_codec_find_id(schema, field_group_type_id(f))
         if (idx == 0) then
            err = CAPNP_ERR_ARG
            return
         end if
         call build_struct(schema, idx, p, env, depth, jv, err)
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
               allocate (raw(0))
            end if
         else
            call capnp_get_data(p, int(off), raw, err)
            if (err /= CAPNP_OK) return
         end if
         call build_data_jval(raw, mode, jv)
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
         call build_struct(schema, idx, q, cenv, depth, jv, err)
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
         call append_kid(jv%kids, n, one)
      end do
      call trim_kids(jv%kids, n)
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
         call build_data_jval(raw, 0, jv)
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
         call build_struct(schema, idx, el, cenv, depth, jv, err)
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

   subroutine build_data_jval(raw, mode, jv)
      integer(int8), intent(in) :: raw(:)
      integer, intent(in) :: mode
      type(jval), intent(out) :: jv
      type(jval) :: one
      integer :: i, n
      if (mode == 1) then
         jv%k = JK_STR
         jv%str = b64_encode(raw)
         return
      else if (mode == 2) then
         jv%k = JK_STR
         jv%str = hex_encode(raw)
         return
      end if
      jv%k = JK_ARR
      n = size(raw)
      if (n <= 0) return
      allocate (jv%kids(n))
      do i = 1, n
         one%k = JK_NUM
         call capnp_fmt_u64(int(capnp_u_of_i8(raw(i)), int64), one%str)
         call take_jval(jv%kids(i), one)
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

   recursive subroutine emit_jval(jv, indent, has_prefix, pretty, out, multi)
      type(jval), intent(in) :: jv
      integer, intent(in) :: indent
      logical, intent(in) :: has_prefix, pretty
      character(len=:), allocatable, intent(out) :: out
      logical, intent(out) :: multi
      type(piece_t), allocatable :: items(:)
      character(len=:), allocatable :: val, body, colon
      integer :: i, m, n, sub
      logical :: child_multi
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
         out = json_quote(jv%str)
      case (JK_ARR)
         n = 0
         if (allocated(jv%kids)) n = size(jv%kids)
         sub = indent
         if (n > 1) sub = indent + 1
         m = 0
         do i = 1, n
            call emit_jval(jv%kids(i), sub, .false., pretty, val, child_multi)
            call push(items, m, val, child_multi)
         end do
         call jjoin(items, m, indent, has_prefix, pretty, multi, body)
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
            call emit_jval(jv%kids(i), sub, .true., pretty, val, child_multi)
            call push(items, m, json_quote(jv%kids(i)%key)//colon//val, child_multi)
         end do
         call jjoin(items, m, indent, has_prefix, pretty, multi, body)
         out = '{'//body//'}'
      case default
         out = 'null'
      end select
   end subroutine emit_jval

   recursive subroutine fill_struct(schema, idx, p, env, jv, depth, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx, depth
      type(capnp_ptr_t), intent(in) :: p
      type(capnp_brand_env_t), intent(in) :: env
      type(jval), intent(in) :: jv
      integer, intent(out) :: err
      integer :: i
      type(capnp_ptr_t) :: f
      err = CAPNP_OK
      if (depth > MAX_DEPTH) then
         err = CAPNP_ERR_DEPTH
         return
      end if
      if (jv%k /= JK_OBJ) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (.not. allocated(jv%kids)) return
      do i = 1, size(jv%kids)
         if (.not. find_json_field(schema, idx, jv%kids(i)%key, f)) cycle
         if (field_discriminant(f) /= NO_DISCRIMINANT) then
            call capnp_set_which(p, int(node_struct_discriminant_offset(schema%nodes(idx))), &
                                  field_discriminant(f), err)
            if (err /= CAPNP_OK) return
         end if
         call fill_field(schema, f, p, env, jv%kids(i), depth + 1, err)
         if (err /= CAPNP_OK) return
      end do
   end subroutine fill_struct

   recursive subroutine fill_field(schema, f, p, env, jv, depth, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      type(capnp_ptr_t), intent(in) :: f, p
      type(capnp_brand_env_t), intent(in) :: env
      type(jval), intent(in) :: jv
      integer, intent(in) :: depth
      integer, intent(out) :: err
      type(capnp_ptr_t) :: raw_t, t, q, et, dv
      type(capnp_brand_env_t) :: cenv
      integer :: tw, idx, dw, pw, mode
      integer(int64) :: off
      err = CAPNP_OK
      if (field_which(f) == FIELD_GROUP) then
         idx = capnp_codec_find_id(schema, field_group_type_id(f))
         if (idx == 0) then
            err = CAPNP_ERR_ARG
            return
         end if
         call fill_struct(schema, idx, p, env, jv, depth, err)
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
         call fill_struct(schema, idx, q, cenv, jv, depth, err)
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
            call fill_struct(schema, idx, el, cenv, jv%kids(i), depth + 1, err)
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
         if (jv%k /= JK_NULL) err = CAPNP_ERR_ARG
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
      err = CAPNP_OK
      if (mode == 1) then
         if (jv%k == JK_NULL) return
         if (jv%k /= JK_STR) then
            err = CAPNP_ERR_ARG
            return
         end if
         call b64_decode(jv%str, raw, err)
      else if (mode == 2) then
         if (jv%k == JK_NULL) return
         if (jv%k /= JK_STR) then
            err = CAPNP_ERR_ARG
            return
         end if
         call hex_decode(jv%str, raw, err)
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
      if (jv%k == JK_STR) then
         call capnp_parse_i64(jv%str, iv, err)
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
      if (jv%k == JK_STR) then
         call capnp_parse_u64(jv%str, iv, err)
      else if (jv%k == JK_NUM) then
         call capnp_f64_to_u64(jv%num, iv, err)
      else
         err = CAPNP_ERR_ARG
      end if
   end subroutine j_unsigned

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
         call append_kid(jv%kids, n, one)
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
      call trim_kids(jv%kids, n)
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
         call append_kid(jv%kids, n, one)
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
      call trim_kids(jv%kids, n)
   end subroutine parse_array

   subroutine parse_string(sc, s, err)
      type(scan_t), intent(inout) :: sc
      character(len=:), allocatable, intent(out) :: s
      integer, intent(out) :: err
      integer :: cp, h, k
      character(len=1) :: c
      err = CAPNP_OK
      s = ''
      if (jpeek(sc) /= '"') then
         err = CAPNP_ERR_ARG
         return
      end if
      sc%i = sc%i + 1
      do
         if (sc%i > sc%n) then
            err = CAPNP_ERR_ARG
            return
         end if
         c = sc%s(sc%i:sc%i)
         sc%i = sc%i + 1
         if (c == '"') return
         if (c /= '\') then
            s = s//c
            cycle
         end if
         if (sc%i > sc%n) then
            err = CAPNP_ERR_ARG
            return
         end if
         c = sc%s(sc%i:sc%i)
         sc%i = sc%i + 1
         select case (c)
         case ('"'); s = s//'"'
         case ('\'); s = s//'\'
         case ('/'); s = s//'/'
         case ('b'); s = s//achar(8)
         case ('f'); s = s//achar(12)
         case ('n'); s = s//achar(10)
         case ('r'); s = s//achar(13)
         case ('t'); s = s//achar(9)
         case ('u')
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
            call append_utf8(s, cp, err)
            if (err /= CAPNP_OK) return
         case default
            err = CAPNP_ERR_ARG
            return
         end select
      end do
   end subroutine parse_string

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
      integer :: i, n, u
      err = CAPNP_OK
      if (jv%k /= JK_ARR) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (.not. allocated(jv%kids)) then
         allocate (raw(0))
         return
      end if
      n = size(jv%kids)
      if (n == 0) then
         allocate (raw(0))
         return
      end if
      allocate (raw(0:n - 1))
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

   function json_quote(s) result(out)
      character(len=*), intent(in) :: s
      character(len=:), allocatable :: out
      character(len=:), allocatable :: buf
      integer :: i, m, ub
      character(len=2) :: hh
      character(len=*), parameter :: hexd = '0123456789abcdef'
      allocate (character(len=len(s)*6 + 2) :: buf)
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
   end function json_quote

   function b64_encode(raw) result(s)
      integer(int8), intent(in) :: raw(:)
      character(len=:), allocatable :: s
      integer :: i, n, a, b, c, v
      n = size(raw)
      s = ''
      i = 1
      do while (i + 2 <= n)
         a = capnp_u_of_i8(raw(i))
         b = capnp_u_of_i8(raw(i + 1))
         c = capnp_u_of_i8(raw(i + 2))
         v = ior(ishft(a, 16), ior(ishft(b, 8), c))
         s = s//B64(ishft(v, -18) + 1:ishft(v, -18) + 1)
         s = s//B64(iand(ishft(v, -12), 63) + 1:iand(ishft(v, -12), 63) + 1)
         s = s//B64(iand(ishft(v, -6), 63) + 1:iand(ishft(v, -6), 63) + 1)
         s = s//B64(iand(v, 63) + 1:iand(v, 63) + 1)
         i = i + 3
      end do
      if (i == n) then
         a = capnp_u_of_i8(raw(i))
         v = ishft(a, 16)
         s = s//B64(ishft(v, -18) + 1:ishft(v, -18) + 1)
         s = s//B64(iand(ishft(v, -12), 63) + 1:iand(ishft(v, -12), 63) + 1)//'=='
      else if (i + 1 == n) then
         a = capnp_u_of_i8(raw(i))
         b = capnp_u_of_i8(raw(i + 1))
         v = ior(ishft(a, 16), ishft(b, 8))
         s = s//B64(ishft(v, -18) + 1:ishft(v, -18) + 1)
         s = s//B64(iand(ishft(v, -12), 63) + 1:iand(ishft(v, -12), 63) + 1)
         s = s//B64(iand(ishft(v, -6), 63) + 1:iand(ishft(v, -6), 63) + 1)//'='
      end if
   end function b64_encode

   subroutine b64_decode(s, raw, err)
      character(len=*), intent(in) :: s
      integer(int8), allocatable, intent(out) :: raw(:)
      integer, intent(out) :: err
      integer :: i, n, v, nd, a, b, c, d, outn
      integer, allocatable :: vals(:)
      character(len=1) :: ch
      err = CAPNP_OK
      n = len(s)
      allocate (vals(n))
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
         allocate (raw(0))
         return
      end if
      allocate (raw(0:outn - 1))
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

   function hex_encode(raw) result(s)
      integer(int8), intent(in) :: raw(:)
      character(len=:), allocatable :: s
      integer :: i, u
      character(len=*), parameter :: hexd = '0123456789abcdef'
      s = ''
      do i = 1, size(raw)
         u = capnp_u_of_i8(raw(i))
         s = s//hexd(u/16 + 1:u/16 + 1)//hexd(mod(u, 16) + 1:mod(u, 16) + 1)
      end do
   end function hex_encode

   subroutine hex_decode(s, raw, err)
      character(len=*), intent(in) :: s
      integer(int8), allocatable, intent(out) :: raw(:)
      integer, intent(out) :: err
      integer :: i, n, hi, lo
      err = CAPNP_OK
      n = len(s)
      if (mod(n, 2) /= 0) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (n == 0) then
         allocate (raw(0))
         return
      end if
      allocate (raw(0:n/2 - 1))
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

   subroutine append_utf8(s, cp, err)
      character(len=:), allocatable, intent(inout) :: s
      integer, intent(in) :: cp
      integer, intent(inout) :: err
      if (cp < 0 .or. cp > 1114111) then
         err = CAPNP_ERR_ARG
         return
      else if (cp <= 127) then
         s = s//achar(cp)
      else if (cp <= 2047) then
         s = s//achar(ior(192, ishft(cp, -6)))//achar(ior(128, iand(cp, 63)))
      else if (cp <= 65535) then
         s = s//achar(ior(224, ishft(cp, -12)))//achar(ior(128, iand(ishft(cp, -6), 63)))// &
             achar(ior(128, iand(cp, 63)))
      else
         s = s//achar(ior(240, ishft(cp, -18)))//achar(ior(128, iand(ishft(cp, -12), 63)))// &
             achar(ior(128, iand(ishft(cp, -6), 63)))//achar(ior(128, iand(cp, 63)))
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

   function find_json_field(schema, idx, key, f) result(y)
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
         call json_field_name(f, nm, err)
         if (err /= CAPNP_OK) return
         if (nm == key) then
            y = .true.
            return
         end if
      end do
   end function find_json_field

   subroutine push(items, n, s, multi)
      type(piece_t), allocatable, intent(inout) :: items(:)
      integer, intent(inout) :: n
      character(len=*), intent(in) :: s
      logical, intent(in) :: multi
      type(piece_t), allocatable :: tmp(:)
      if (.not. allocated(items)) allocate (items(8))
      if (n >= size(items)) then
         allocate (tmp(size(items)*2))
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

   subroutine append_kid(kids, n, src)
      type(jval), allocatable, intent(inout) :: kids(:)
      integer, intent(inout) :: n
      type(jval), intent(inout) :: src
      type(jval), allocatable :: tmp(:)
      integer :: i, cap
      if (.not. allocated(kids)) allocate (kids(4))
      if (n >= size(kids)) then
         cap = size(kids)*2
         allocate (tmp(cap))
         do i = 1, n
            call take_jval(tmp(i), kids(i))
         end do
         call move_alloc(tmp, kids)
      end if
      n = n + 1
      call take_jval(kids(n), src)
   end subroutine append_kid

   subroutine trim_kids(kids, n)
      type(jval), allocatable, intent(inout) :: kids(:)
      integer, intent(in) :: n
      type(jval), allocatable :: tmp(:)
      integer :: i
      if (n <= 0) then
         if (allocated(kids)) deallocate (kids)
         return
      end if
      allocate (tmp(n))
      do i = 1, n
         call take_jval(tmp(i), kids(i))
      end do
      call move_alloc(tmp, kids)
   end subroutine trim_kids

   subroutine jjoin(items, n, indent, has_prefix, pretty, multi, out)
      type(piece_t), allocatable, intent(in) :: items(:)
      integer, intent(in) :: n, indent
      logical, intent(in) :: has_prefix, pretty
      logical, intent(inout) :: multi
      character(len=:), allocatable, intent(out) :: out
      integer :: i, mx
      logical :: any_multi
      character(len=:), allocatable :: delim, prefix, suffix, sp
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
      out = prefix//items(1)%s
      do i = 2, n
         out = out//delim//items(i)%s
      end do
      out = out//suffix
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

end module capnp_json
