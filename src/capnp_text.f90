!> Cap'n Proto text format, the struct literals `capnp decode` prints and
!> `capnp encode` reads. Pretty output follows the C++ TextCodec
!> (non-null pointers, every primitive, active union member).
module capnp_text
   use capnp
   use capnp_schema
   use capnp_codec
   use capnp_dynamic, only: capnp_dyn_schema_t
   implicit none
   private

   public :: capnp_text_encode, capnp_text_decode
   public :: capnp_text_encode_value, capnp_text_encode_list, capnp_text_encode_enum
   public :: capnp_text_span

   integer, parameter :: MODE_BARE = 1
   integer, parameter :: MODE_PREF = 2
   integer, parameter :: MAX_DEPTH = 64

   type :: piece_t
      character(len=:), allocatable :: s
   end type piece_t

   type :: buf_t
      character(len=:), allocatable :: s
      integer :: n = 0
   end type buf_t

   type :: scan_t
      character(len=:), allocatable :: s
      integer :: i = 1
      integer :: n = 0
   end type scan_t

contains

   subroutine capnp_text_encode(schema, idx, p, text, err, pretty, brand_env, &
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
      integer :: ind
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
      limit = CAPNP_DEFAULT_TRAVERSAL_WORDS
      if (present(traversal_words)) limit = traversal_words
      call guard_traversal(p, limit, err)
      if (err /= CAPNP_OK) return
      if (node_which(schema%nodes(idx)) /= NODE_STRUCT) then
         err = CAPNP_ERR_ARG
         return
      end if
      ind = 1
      if (present(pretty)) then
         if (.not. pretty) ind = 0
      end if
      if (present(brand_env)) env = brand_env
      call enc_struct(schema, idx, p, env, ind, MODE_BARE, 0, text, err)
   end subroutine capnp_text_encode

   subroutine capnp_text_encode_list(schema, et, lst, env, text, err, pretty, &
                                      traversal_words)
      type(capnp_dyn_schema_t), intent(in) :: schema
      type(capnp_ptr_t), intent(in) :: et, lst
      type(capnp_brand_env_t), intent(in) :: env
      character(len=:), allocatable, intent(out) :: text
      integer, intent(out) :: err
      logical, intent(in), optional :: pretty
      integer(int64), intent(in), optional :: traversal_words
      integer :: ind
      integer(int64) :: limit
      ind = 1
      if (present(pretty)) then
         if (.not. pretty) ind = 0
      end if
      limit = CAPNP_DEFAULT_TRAVERSAL_WORDS
      if (present(traversal_words)) limit = traversal_words
      call guard_traversal(lst, limit, err)
      if (err /= CAPNP_OK) return
      call enc_list(schema, et, lst, env, ind, MODE_BARE, 0, text, err)
   end subroutine capnp_text_encode_list

   subroutine capnp_text_encode_enum(schema, tid, ord, text, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer(int64), intent(in) :: tid
      integer, intent(in) :: ord
      character(len=:), allocatable, intent(out) :: text
      integer, intent(out) :: err
      call enum_text(schema, tid, ord, text, err)
   end subroutine capnp_text_encode_enum

   subroutine capnp_text_decode(schema, idx, text, msg, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      character(len=*), intent(in) :: text
      type(capnp_message_t), intent(inout), target :: msg
      integer, intent(out) :: err
      type(capnp_ptr_t) :: root
      type(capnp_brand_env_t) :: env
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
      if (node_which(schema%nodes(idx)) /= NODE_STRUCT) then
         err = CAPNP_ERR_ARG
         return
      end if
      call capnp_message_free(msg)
      call capnp_message_init_builder(msg, err)
      if (err /= CAPNP_OK) return
      dw = node_struct_data_words(schema%nodes(idx))
      pw = node_struct_pointer_count(schema%nodes(idx))
      root = capnp_new_struct(msg, dw, pw, err)
      if (err /= CAPNP_OK) return
      call capnp_set_root(msg, root, err)
      if (err /= CAPNP_OK) return
      sc%s = text
      sc%n = len(text)
      sc%i = 1
      call parse_struct(schema, idx, root, env, sc, 0, err)
      if (err /= CAPNP_OK) return
      call skip_ws(sc)
      if (sc%i <= sc%n) err = CAPNP_ERR_ARG
   end subroutine capnp_text_decode

   !> Render a schema Value (a const, or a field default) in text form.
   subroutine capnp_text_encode_value(schema, typ, val, text, err, pretty)
      type(capnp_dyn_schema_t), intent(in) :: schema
      type(capnp_ptr_t), intent(in) :: typ, val
      character(len=:), allocatable, intent(out) :: text
      integer, intent(out) :: err
      logical, intent(in), optional :: pretty
      type(capnp_brand_env_t) :: env, cenv
      integer :: ind, tw, ord, idx
      type(capnp_ptr_t) :: q, et
      integer(int8), allocatable :: raw(:)
      character(len=:), allocatable :: body, esc
      err = CAPNP_OK
      text = ''
      ind = 1
      if (present(pretty)) then
         if (.not. pretty) ind = 0
      end if
      tw = type_which(typ)
      select case (tw)
      case (TYPE_VOID)
         text = 'void'
      case (TYPE_BOOL)
         if (val%kind /= CAPNP_PK_NULL .and. value_bool(val)) then
            text = 'true'
         else
            text = 'false'
         end if
      case (TYPE_INT8)
         call capnp_fmt_i64(int(capnp_def_i8(val), int64), text)
      case (TYPE_INT16)
         call capnp_fmt_i64(int(capnp_def_i16(val), int64), text)
      case (TYPE_INT32)
         call capnp_fmt_i64(int(capnp_def_i32(val), int64), text)
      case (TYPE_INT64)
         call capnp_fmt_i64(capnp_def_i64(val), text)
      case (TYPE_UINT8)
         call capnp_fmt_u64(int(capnp_def_u8(val), int64), text)
      case (TYPE_UINT16)
         call capnp_fmt_u64(int(capnp_def_u16(val), int64), text)
      case (TYPE_UINT32)
         call capnp_fmt_u64(capnp_def_u32(val), text)
      case (TYPE_UINT64)
         call capnp_fmt_u64(capnp_def_i64(val), text)
      case (TYPE_FLOAT32)
         call capnp_fmt_f32(capnp_def_f32(val), text)
      case (TYPE_FLOAT64)
         call capnp_fmt_f64(capnp_def_f64(val), text)
      case (TYPE_TEXT)
         if (val%kind == CAPNP_PK_NULL) then
            body = ''
         else
            call value_text(val, body, err)
            if (err /= CAPNP_OK) return
         end if
         call capnp_cescape_text(body, esc)
         text = '"'//esc//'"'
      case (TYPE_DATA)
         if (val%kind == CAPNP_PK_NULL) then
            esc = ''
         else
            call value_data(val, raw, err)
            if (err /= CAPNP_OK) return
            call capnp_cescape_data(raw, esc)
         end if
         text = '"'//esc//'"'
      case (TYPE_ENUM)
         ord = 0
         if (val%kind /= CAPNP_PK_NULL) ord = value_enum(val)
         call enum_text(schema, type_type_id(typ), ord, text, err)
      case (TYPE_LIST)
         q = capnp_ptr_t()
         if (val%kind /= CAPNP_PK_NULL) then
            q = value_pointer(val, err)
            if (err /= CAPNP_OK) return
         end if
         et = type_list_element(typ, err)
         if (err /= CAPNP_OK) return
         call enc_list(schema, et, q, env, ind, MODE_BARE, 0, text, err)
      case (TYPE_STRUCT)
         q = capnp_ptr_t()
         if (val%kind /= CAPNP_PK_NULL) then
            q = value_pointer(val, err)
            if (err /= CAPNP_OK) return
         end if
         idx = capnp_codec_find_id(schema, type_type_id(typ))
         if (idx == 0) then
            err = CAPNP_ERR_ARG
            return
         end if
         call capnp_brand_enter(env, typ, cenv, err)
         if (err /= CAPNP_OK) return
         call enc_struct(schema, idx, q, cenv, ind, MODE_BARE, 0, text, err)
      case (TYPE_ANY_POINTER)
         text = '<opaque pointer>'
      case (TYPE_INTERFACE)
         text = '<external capability>'
      case default
         err = CAPNP_ERR_KIND
      end select
   end subroutine capnp_text_encode_value

   !> Locate one top-level text value. beg = 0 and err = 0 means no more input.
   subroutine capnp_text_span(text, from, beg, to, err)
      character(len=*), intent(in) :: text
      integer, intent(in) :: from
      integer, intent(out) :: beg, to
      integer, intent(out) :: err
      type(scan_t) :: sc
      character(len=:), allocatable :: junk
      character(len=1) :: c
      err = CAPNP_OK
      beg = 0
      to = 0
      sc%s = text
      sc%n = len(text)
      sc%i = max(from, 1)
      call skip_ws(sc)
      if (sc%i > sc%n) return
      beg = sc%i
      c = text(sc%i:sc%i)
      if (c == '(' .or. c == '[') then
         call match_close(text, sc%i, to, err)
      else if (c == '"') then
         call read_string(sc, junk, err)
         to = sc%i - 1
      else
         call read_numeric(sc, junk, err)
         to = sc%i - 1
      end if
   end subroutine capnp_text_span

   recursive subroutine enc_struct(schema, idx, p, env, indent, mode, depth, out, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx, indent, mode, depth
      type(capnp_ptr_t), intent(in) :: p
      type(capnp_brand_env_t), intent(in) :: env
      character(len=:), allocatable, intent(out) :: out
      integer, intent(out) :: err
      type(piece_t), allocatable :: items(:)
      type(capnp_ptr_t) :: fl, f
      character(len=:), allocatable :: nm, val, body
      integer :: i, n, nf, active, fd
      logical :: show
      err = CAPNP_OK
      out = ''
      if (depth > MAX_DEPTH) then
         err = CAPNP_ERR_DEPTH
         return
      end if
      if (idx < 1 .or. idx > size(schema%nodes)) then
         err = CAPNP_ERR_ARG
         return
      end if
      active = -1
      if (node_struct_discriminant_count(schema%nodes(idx)) > 0) then
         if (p%kind == CAPNP_PK_NULL) then
            active = 0
         else
            active = capnp_which(p, int(node_struct_discriminant_offset(schema%nodes(idx))))
         end if
      end if
      fl = node_struct_fields(schema%nodes(idx), err)
      if (err /= CAPNP_OK) return
      nf = int(capnp_list_len(fl))
      n = 0
      do i = 0, nf - 1
         f = capnp_list_get_struct(fl, i, err)
         if (err /= CAPNP_OK) return
         fd = field_discriminant(f)
         show = capnp_codec_field_present(f, p, err)
         if (err /= CAPNP_OK) return
         if (fd /= NO_DISCRIMINANT) then
            if (fd /= active) cycle
            if (fd == 0 .and. .not. show) cycle
         else if (.not. show) then
            cycle
         end if
         call field_name(f, nm, err)
         if (err /= CAPNP_OK) return
         call enc_field_value(schema, f, p, env, next_ind(indent), MODE_PREF, depth + 1, val, err)
         if (err /= CAPNP_OK) return
         call push(items, n, nm//' = '//val, err)
         if (err /= CAPNP_OK) return
      end do
      call join_items(items, n, indent, mode, .true., body, err)
      if (err /= CAPNP_OK) return
      out = '('//body//')'
   end subroutine enc_struct

   recursive subroutine enc_field_value(schema, f, p, env, indent, mode, depth, out, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      type(capnp_ptr_t), intent(in) :: f, p
      type(capnp_brand_env_t), intent(in) :: env
      integer, intent(in) :: indent, mode, depth
      character(len=:), allocatable, intent(out) :: out
      integer, intent(out) :: err
      type(capnp_ptr_t) :: raw_t, t, q, et, dv
      type(capnp_brand_env_t) :: cenv
      character(len=:), allocatable :: s, esc
      integer(int8), allocatable :: raw(:)
      integer :: tw, idx, ord
      integer(int64) :: off, bits
      err = CAPNP_OK
      out = ''
      if (field_which(f) == FIELD_GROUP) then
         idx = capnp_codec_find_id(schema, field_group_type_id(f))
         if (idx == 0) then
            err = CAPNP_ERR_ARG
            return
         end if
         call enc_struct(schema, idx, p, env, indent, mode, depth, out, err)
         return
      end if
      raw_t = field_slot_type(f, err)
      if (err /= CAPNP_OK) return
      t = capnp_brand_resolve(env, raw_t)
      dv = field_slot_default(f, err)
      if (err /= CAPNP_OK) return
      tw = type_which(t)
      off = field_slot_offset(f)
      select case (tw)
      case (TYPE_VOID)
         out = 'void'
      case (TYPE_BOOL)
         if (capnp_get_bool(p, off, default=capnp_def_bool(dv))) then
            out = 'true'
         else
            out = 'false'
         end if
      case (TYPE_INT8)
         call capnp_fmt_i64(int(capnp_get_i8(p, off, default=capnp_def_i8(dv)), int64), out)
      case (TYPE_INT16)
         call capnp_fmt_i64(int(capnp_get_i16(p, off*2_int64, default=capnp_def_i16(dv)), int64), out)
      case (TYPE_INT32)
         call capnp_fmt_i64(int(capnp_get_i32(p, off*4_int64, default=capnp_def_i32(dv)), int64), out)
      case (TYPE_INT64)
         call capnp_fmt_i64(capnp_get_i64(p, off*8_int64, default=capnp_def_i64(dv)), out)
      case (TYPE_UINT8)
         call capnp_fmt_u64(int(capnp_get_u8(p, off, default=capnp_def_u8(dv)), int64), out)
      case (TYPE_UINT16, TYPE_ENUM)
         ord = int(capnp_get_u16(p, off*2_int64, default=capnp_def_u16(dv)))
         if (tw == TYPE_ENUM) then
            call enum_text(schema, type_type_id(t), ord, out, err)
         else
            call capnp_fmt_u64(int(ord, int64), out)
         end if
      case (TYPE_UINT32)
         call capnp_fmt_u64(capnp_get_u32(p, off*4_int64, default=capnp_def_u32(dv)), out)
      case (TYPE_UINT64)
         bits = capnp_get_i64(p, off*8_int64, default=capnp_def_i64(dv))
         call capnp_fmt_u64(bits, out)
      case (TYPE_FLOAT32)
         call capnp_fmt_f32(capnp_get_f32(p, off*4_int64, default=capnp_def_f32(dv)), out)
      case (TYPE_FLOAT64)
         call capnp_fmt_f64(capnp_get_f64(p, off*8_int64, default=capnp_def_f64(dv)), out)
      case (TYPE_TEXT)
         q = capnp_ptr_t()
         if (p%kind /= CAPNP_PK_NULL) then
            q = capnp_getp(p, int(off), err)
            if (err /= CAPNP_OK) return
         end if
         if (q%kind == CAPNP_PK_NULL) then
            if (dv%kind /= CAPNP_PK_NULL) then
               call value_text(dv, s, err)
               if (err /= CAPNP_OK) return
            else
               s = ''
            end if
         else
            call capnp_get_text(p, int(off), s, err)
            if (err /= CAPNP_OK) return
         end if
         call capnp_cescape_text(s, esc)
         out = '"'//esc//'"'
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
               call capnp_cescape_data(raw, esc)
            else
               esc = ''
            end if
         else
            call capnp_get_data(p, int(off), raw, err)
            if (err /= CAPNP_OK) return
            call capnp_cescape_data(raw, esc)
         end if
         out = '"'//esc//'"'
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
         call enc_list(schema, et, q, env, indent, mode, depth, out, err)
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
         call enc_struct(schema, idx, q, cenv, indent, mode, depth, out, err)
      case (TYPE_ANY_POINTER)
         out = '<opaque pointer>'
      case (TYPE_INTERFACE)
         out = '<external capability>'
      case default
         err = CAPNP_ERR_KIND
      end select
   end subroutine enc_field_value

   recursive subroutine enc_list(schema, raw_et, lst, env, indent, mode, depth, out, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      type(capnp_ptr_t), intent(in) :: raw_et, lst
      type(capnp_brand_env_t), intent(in) :: env
      integer, intent(in) :: indent, mode, depth
      character(len=:), allocatable, intent(out) :: out
      integer, intent(out) :: err
      type(piece_t), allocatable :: items(:)
      type(capnp_ptr_t) :: et
      character(len=:), allocatable :: el, body
      integer(int64) :: i, nlen
      integer :: n
      err = CAPNP_OK
      out = ''
      if (depth > MAX_DEPTH) then
         err = CAPNP_ERR_DEPTH
         return
      end if
      if (lst%kind /= CAPNP_PK_LIST) then
         out = '[]'
         return
      end if
      et = capnp_brand_resolve(env, raw_et)
      nlen = capnp_list_len(lst)
      n = 0
      do i = 0_int64, nlen - 1_int64
         call enc_element(schema, et, lst, i, env, next_ind(indent), MODE_BARE, depth + 1, el, err)
         if (err /= CAPNP_OK) return
         call push(items, n, el, err)
         if (err /= CAPNP_OK) return
      end do
      call join_items(items, n, indent, mode, .false., body, err)
      if (err /= CAPNP_OK) return
      out = '['//body//']'
   end subroutine enc_list

   recursive subroutine enc_element(schema, et, lst, i, env, indent, mode, depth, out, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      type(capnp_ptr_t), intent(in) :: et, lst
      integer(int64), intent(in) :: i
      type(capnp_brand_env_t), intent(in) :: env
      integer, intent(in) :: indent, mode, depth
      character(len=:), allocatable, intent(out) :: out
      integer, intent(out) :: err
      type(capnp_ptr_t) :: q, inner, el
      type(capnp_brand_env_t) :: cenv
      character(len=:), allocatable :: s, esc
      integer(int8), allocatable :: raw(:)
      integer :: tw, idx, ord
      integer(int64) :: bits
      err = CAPNP_OK
      out = ''
      tw = type_which(et)
      select case (tw)
      case (TYPE_VOID)
         out = 'void'
      case (TYPE_BOOL)
         if (capnp_list_get_bool(lst, i, err)) then
            out = 'true'
         else
            out = 'false'
         end if
      case (TYPE_INT8)
         call capnp_fmt_i64(int(capnp_list_get_i8(lst, i, err), int64), out)
      case (TYPE_INT16)
         call capnp_fmt_i64(int(capnp_list_get_i16(lst, i, err), int64), out)
      case (TYPE_INT32)
         call capnp_fmt_i64(int(capnp_list_get_i32(lst, i, err), int64), out)
      case (TYPE_INT64)
         call capnp_fmt_i64(capnp_list_get_i64(lst, i, err), out)
      case (TYPE_UINT8)
         call capnp_fmt_u64(int(capnp_u_of_i8(capnp_list_get_i8(lst, i, err)), int64), out)
      case (TYPE_UINT16)
         call capnp_fmt_u64(int(capnp_u_of_i16(capnp_list_get_i16(lst, i, err)), int64), out)
      case (TYPE_UINT32)
         bits = capnp_u_of_i32(capnp_list_get_i32(lst, i, err))
         call capnp_fmt_u64(bits, out)
      case (TYPE_UINT64)
         call capnp_fmt_u64(capnp_list_get_i64(lst, i, err), out)
      case (TYPE_FLOAT32)
         call capnp_fmt_f32(capnp_list_get_f32(lst, i, err), out)
      case (TYPE_FLOAT64)
         call capnp_fmt_f64(capnp_list_get_f64(lst, i, err), out)
      case (TYPE_ENUM)
         ord = capnp_u_of_i16(capnp_list_get_i16(lst, i, err))
         if (err /= CAPNP_OK) return
         call enum_text(schema, type_type_id(et), ord, out, err)
      case (TYPE_TEXT)
         call capnp_list_get_text(lst, int(i), s, err)
         if (err /= CAPNP_OK) return
         call capnp_cescape_text(s, esc)
         out = '"'//esc//'"'
      case (TYPE_DATA)
         call capnp_get_data(lst, int(i), raw, err)
         if (err /= CAPNP_OK) return
         call capnp_cescape_data(raw, esc)
         out = '"'//esc//'"'
      case (TYPE_LIST)
         q = capnp_list_getp(lst, int(i), err)
         if (err /= CAPNP_OK) return
         inner = type_list_element(et, err)
         if (err /= CAPNP_OK) return
         call enc_list(schema, inner, q, env, indent, mode, depth, out, err)
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
         call enc_struct(schema, idx, el, cenv, indent, mode, depth, out, err)
      case (TYPE_ANY_POINTER)
         out = '<opaque pointer>'
      case (TYPE_INTERFACE)
         out = '<external capability>'
      case default
         err = CAPNP_ERR_KIND
      end select
   end subroutine enc_element

   recursive subroutine parse_struct(schema, idx, p, env, sc, depth, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx, depth
      type(capnp_ptr_t), intent(in) :: p
      type(capnp_brand_env_t), intent(in) :: env
      type(scan_t), intent(inout) :: sc
      integer, intent(out) :: err
      type(capnp_ptr_t) :: f
      character(len=:), allocatable :: name
      integer :: fd
      err = CAPNP_OK
      if (depth > MAX_DEPTH) then
         err = CAPNP_ERR_DEPTH
         return
      end if
      call expect_ch(sc, '(', err)
      if (err /= CAPNP_OK) return
      do
         call skip_ws(sc)
         if (peek(sc) == ')') then
            sc%i = sc%i + 1
            return
         end if
         if (peek(sc) == achar(0)) then
            err = CAPNP_ERR_ARG
            return
         end if
         call read_ident(sc, name, err)
         if (err /= CAPNP_OK) return
         call expect_ch(sc, '=', err)
         if (err /= CAPNP_OK) return
         call capnp_codec_find_field(schema, idx, name, f, err)
         if (err /= CAPNP_OK) return
         fd = field_discriminant(f)
         if (fd /= NO_DISCRIMINANT) then
            call capnp_set_which(p, int(node_struct_discriminant_offset(schema%nodes(idx))), fd, err)
            if (err /= CAPNP_OK) return
         end if
         call parse_field_value(schema, f, p, env, sc, depth + 1, err)
         if (err /= CAPNP_OK) return
         call skip_ws(sc)
         if (peek(sc) == ',') then
            sc%i = sc%i + 1
            cycle
         else if (peek(sc) == ')') then
            sc%i = sc%i + 1
            return
         else
            err = CAPNP_ERR_ARG
            return
         end if
      end do
   end subroutine parse_struct

   recursive subroutine parse_field_value(schema, f, p, env, sc, depth, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      type(capnp_ptr_t), intent(in) :: f, p
      type(capnp_brand_env_t), intent(in) :: env
      type(scan_t), intent(inout) :: sc
      integer, intent(in) :: depth
      integer, intent(out) :: err
      type(capnp_ptr_t) :: raw_t, t, q, et, dv
      type(capnp_brand_env_t) :: cenv
      integer :: tw, idx, dw, pw
      integer(int64) :: off
      err = CAPNP_OK
      if (field_which(f) == FIELD_GROUP) then
         idx = capnp_codec_find_id(schema, field_group_type_id(f))
         if (idx == 0) then
            err = CAPNP_ERR_ARG
            return
         end if
         call parse_struct(schema, idx, p, env, sc, depth, err)
         return
      end if
      raw_t = field_slot_type(f, err)
      if (err /= CAPNP_OK) return
      t = capnp_brand_resolve(env, raw_t)
      tw = type_which(t)
      off = field_slot_offset(f)
      select case (tw)
      case (TYPE_LIST)
         et = type_list_element(t, err)
         if (err /= CAPNP_OK) return
         call parse_list(schema, et, p, int(off), env, sc, depth, err)
      case (TYPE_STRUCT)
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
         call parse_struct(schema, idx, q, cenv, sc, depth, err)
      case default
         dv = field_slot_default(f, err)
         if (err /= CAPNP_OK) return
         call store_scalar(schema, t, p, off, sc, .false., 0_int64, dv, err)
      end select
   end subroutine parse_field_value

   recursive subroutine parse_list(schema, raw_et, parent, slot, env, sc, depth, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      type(capnp_ptr_t), intent(in) :: raw_et, parent
      integer, intent(in) :: slot, depth
      type(capnp_brand_env_t), intent(in) :: env
      type(scan_t), intent(inout) :: sc
      integer, intent(out) :: err
      type(capnp_ptr_t) :: lst, et
      integer :: n, i, at
      err = CAPNP_OK
      if (depth > MAX_DEPTH) then
         err = CAPNP_ERR_DEPTH
         return
      end if
      call skip_ws(sc)
      if (peek(sc) /= '[') then
         err = CAPNP_ERR_ARG
         return
      end if
      n = count_items(sc%s, sc%i)
      if (n < 0) then
         err = CAPNP_ERR_ARG
         return
      end if
      et = capnp_brand_resolve(env, raw_et)
      lst = capnp_codec_alloc_list(parent, et, n, schema, err, env=env)
      if (err /= CAPNP_OK) return
      call capnp_setp(parent, slot, lst, err)
      if (err /= CAPNP_OK) return
      sc%i = sc%i + 1
      if (n == 0) then
         call expect_ch(sc, ']', err)
         return
      end if
      do i = 0, n - 1
         call parse_element(schema, et, lst, int(i, int64), env, sc, depth + 1, err)
         if (err /= CAPNP_OK) return
         call skip_ws(sc)
         if (i < n - 1) then
            if (peek(sc) /= ',') then
               err = CAPNP_ERR_ARG
               return
            end if
            sc%i = sc%i + 1
         else if (peek(sc) == ',') then
            sc%i = sc%i + 1
         end if
      end do
      at = sc%i
      call expect_ch(sc, ']', err)
      if (err /= CAPNP_OK) sc%i = at
   end subroutine parse_list

   recursive subroutine parse_element(schema, et, lst, i, env, sc, depth, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      type(capnp_ptr_t), intent(in) :: et, lst
      integer(int64), intent(in) :: i
      type(capnp_brand_env_t), intent(in) :: env
      type(scan_t), intent(inout) :: sc
      integer, intent(in) :: depth
      integer, intent(out) :: err
      type(capnp_ptr_t) :: inner, el, dummy
      type(capnp_brand_env_t) :: cenv
      integer :: tw, idx
      err = CAPNP_OK
      tw = type_which(et)
      select case (tw)
      case (TYPE_LIST)
         inner = type_list_element(et, err)
         if (err /= CAPNP_OK) return
         call parse_list(schema, inner, lst, int(i), env, sc, depth, err)
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
         call parse_struct(schema, idx, el, cenv, sc, depth, err)
      case (TYPE_TEXT, TYPE_DATA, TYPE_VOID, TYPE_BOOL, TYPE_ENUM, &
            TYPE_INT8, TYPE_INT16, TYPE_INT32, TYPE_INT64, &
            TYPE_UINT8, TYPE_UINT16, TYPE_UINT32, TYPE_UINT64, &
            TYPE_FLOAT32, TYPE_FLOAT64)
         dummy = capnp_ptr_t()
         call store_scalar(schema, et, lst, 0_int64, sc, .true., i, dummy, err)
      case default
         err = CAPNP_ERR_KIND
      end select
   end subroutine parse_element

   subroutine store_scalar(schema, t, p, off, sc, in_list, ix, dv, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      type(capnp_ptr_t), intent(in) :: t, p, dv
      integer(int64), intent(in) :: off, ix
      type(scan_t), intent(inout) :: sc
      logical, intent(in) :: in_list
      integer, intent(out) :: err
      character(len=:), allocatable :: tok, s
      integer(int8), allocatable :: raw(:)
      integer(int64) :: iv, uv
      real(real64) :: fv
      real(real32) :: f32v
      integer :: tw, ord
      logical :: bv
      err = CAPNP_OK
      tw = type_which(t)
      select case (tw)
      case (TYPE_VOID)
         call read_ident(sc, tok, err)
         if (err /= CAPNP_OK) return
         if (tok /= 'void') err = CAPNP_ERR_ARG
      case (TYPE_BOOL)
         call read_ident(sc, tok, err)
         if (err /= CAPNP_OK) return
         if (tok == 'true') then
            bv = .true.
         else if (tok == 'false') then
            bv = .false.
         else
            err = CAPNP_ERR_ARG
            return
         end if
         if (in_list) then
            call capnp_list_set_bool(p, ix, bv, err)
         else
            call capnp_set_bool(p, off, bv, err, default=capnp_def_bool(dv))
         end if
      case (TYPE_INT8, TYPE_INT16, TYPE_INT32, TYPE_INT64)
         call read_numeric(sc, tok, err)
         if (err /= CAPNP_OK) return
         call capnp_parse_i64(tok, iv, err, allow_octal=.true.)
         if (err /= CAPNP_OK) return
         call capnp_put_signed(p, off, tw, iv, in_list, ix, dv, err)
      case (TYPE_UINT8, TYPE_UINT16, TYPE_UINT32, TYPE_UINT64)
         call read_numeric(sc, tok, err)
         if (err /= CAPNP_OK) return
         call capnp_parse_u64(tok, uv, err, allow_octal=.true.)
         if (err /= CAPNP_OK) return
         call capnp_put_unsigned(p, off, tw, uv, in_list, ix, dv, err)
      case (TYPE_FLOAT32)
         call read_numeric(sc, tok, err)
         if (err /= CAPNP_OK) return
         call parse_text_f32(tok, f32v, err)
         if (err /= CAPNP_OK) return
         if (in_list) then
            call capnp_list_set_f32(p, ix, f32v, err)
         else
            call capnp_set_f32(p, off*4_int64, f32v, err, default=capnp_def_f32(dv))
         end if
      case (TYPE_FLOAT64)
         call read_numeric(sc, tok, err)
         if (err /= CAPNP_OK) return
         call parse_text_f64(tok, fv, err)
         if (err /= CAPNP_OK) return
         if (in_list) then
            call capnp_list_set_f64(p, ix, fv, err)
         else
            call capnp_set_f64(p, off*8_int64, fv, err, default=capnp_def_f64(dv))
         end if
      case (TYPE_TEXT)
         call read_string(sc, s, err)
         if (err /= CAPNP_OK) return
         if (in_list) then
            call capnp_list_set_text(p, int(ix), s, err)
         else
            call capnp_set_text(p, int(off), s, err)
         end if
      case (TYPE_DATA)
         call read_data(sc, raw, err)
         if (err /= CAPNP_OK) return
         if (in_list) then
            call capnp_set_data(p, int(ix), raw, err)
         else
            call capnp_set_data(p, int(off), raw, err)
         end if
      case (TYPE_ENUM)
         call parse_enum(schema, t, sc, ord, err)
         if (err /= CAPNP_OK) return
         if (in_list) then
            call capnp_list_set_i16(p, ix, capnp_as_i16(ord), err)
         else
            call capnp_set_u16(p, off*2_int64, int(ord, int32), err, default=capnp_def_u16(dv))
         end if
      case default
         err = CAPNP_ERR_KIND
      end select
   end subroutine store_scalar

   subroutine parse_text_f64(tok, fv, err)
      character(len=*), intent(in) :: tok
      real(real64), intent(out) :: fv
      integer, intent(out) :: err
      integer(int64) :: iv, uv
      err = CAPNP_OK
      fv = 0.0_real64
      if (has_alpha_non_hex(tok)) then
         if (tok == 'nan') then
            fv = capnp_quiet_nan()
         else if (tok == 'inf' .or. tok == '-inf') then
            call capnp_parse_f64(tok, fv, err)
         else
            err = CAPNP_ERR_ARG
         end if
         return
      end if
      if (float_from_int(tok)) then
         if (tok(1:1) == '-') then
            call capnp_parse_i64(tok, iv, err, allow_octal=.true.)
            if (err == CAPNP_OK) fv = real(iv, real64)
         else
            call capnp_parse_u64(tok, uv, err, allow_octal=.true.)
            if (err == CAPNP_OK) fv = capnp_u64_to_f64(uv)
         end if
      else
         call capnp_parse_f64(tok, fv, err)
      end if
   end subroutine parse_text_f64

   subroutine parse_text_f32(tok, fv, err)
      character(len=*), intent(in) :: tok
      real(real32), intent(out) :: fv
      integer, intent(out) :: err
      integer(int64) :: iv, uv
      character(len=:), allocatable :: dec
      err = CAPNP_OK
      fv = 0.0_real32
      if (has_alpha_non_hex(tok)) then
         if (tok == 'nan') then
            fv = real(capnp_quiet_nan(), real32)
         else if (tok == 'inf' .or. tok == '-inf') then
            call capnp_parse_f32(tok, fv, err)
         else
            err = CAPNP_ERR_ARG
         end if
         return
      end if
      if (float_from_int(tok)) then
         if (tok(1:1) == '-') then
            call capnp_parse_i64(tok, iv, err, allow_octal=.true.)
            if (err /= CAPNP_OK) return
            if (iv == 0_int64) then
               fv = 0.0_real32
            else
               call capnp_fmt_i64(iv, dec)
               call capnp_parse_f32(dec, fv, err)
            end if
         else
            call capnp_parse_u64(tok, uv, err, allow_octal=.true.)
            if (err /= CAPNP_OK) return
            call capnp_fmt_u64(uv, dec)
            call capnp_parse_f32(dec, fv, err)
         end if
      else
         call capnp_parse_f32(tok, fv, err)
      end if
   end subroutine parse_text_f32

   subroutine parse_enum(schema, t, sc, ord, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      type(capnp_ptr_t), intent(in) :: t
      type(scan_t), intent(inout) :: sc
      integer, intent(out) :: ord
      integer, intent(out) :: err
      character(len=:), allocatable :: tok
      err = CAPNP_OK
      ord = 0
      call skip_ws(sc)
      if (peek(sc) == '(') then
         sc%i = sc%i + 1
         call read_ident(sc, tok, err)
         if (err /= CAPNP_OK) return
         call expect_ch(sc, ')', err)
         if (err /= CAPNP_OK) return
         ord = enum_by_name(schema, type_type_id(t), tok, err)
         return
      end if
      if (is_num_start(peek(sc))) then
         err = CAPNP_ERR_ARG
         return
      end if
      call read_ident(sc, tok, err)
      if (err /= CAPNP_OK) return
      ord = enum_by_name(schema, type_type_id(t), tok, err)
   end subroutine parse_enum

   subroutine enum_text(schema, tid, ord, out, err)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer(int64), intent(in) :: tid
      integer, intent(in) :: ord
      character(len=:), allocatable, intent(out) :: out
      integer, intent(out) :: err
      type(capnp_ptr_t) :: el, e
      character(len=:), allocatable :: num
      integer :: idx, n
      err = CAPNP_OK
      out = ''
      idx = capnp_codec_find_id(schema, tid)
      if (idx /= 0) then
         el = node_enumerants(schema%nodes(idx), err)
         if (err /= CAPNP_OK) return
         n = int(capnp_list_len(el))
         if (ord >= 0 .and. ord < n) then
            e = capnp_list_get_struct(el, ord, err)
            if (err /= CAPNP_OK) return
            call enumerant_name(e, out, err)
            return
         end if
      end if
      call capnp_fmt_i64(int(ord, int64), num)
      out = '('//num//')'
   end subroutine enum_text

   function enum_by_name(schema, tid, name, err) result(ord)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer(int64), intent(in) :: tid
      character(len=*), intent(in) :: name
      integer, intent(out) :: err
      integer :: ord
      type(capnp_ptr_t) :: el, e
      character(len=:), allocatable :: nm
      integer :: idx, i, n
      err = CAPNP_OK
      ord = -1
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
         if (nm == name) then
            ord = i
            return
         end if
      end do
      err = CAPNP_ERR_ARG
   end function enum_by_name

   pure function next_ind(indent) result(n)
      integer, intent(in) :: indent
      integer :: n
      if (indent == 0) then
         n = 0
      else
         n = indent + 1
      end if
   end function next_ind

   subroutine push(items, n, s, err)
      type(piece_t), allocatable, intent(inout) :: items(:)
      integer, intent(inout) :: n
      character(len=*), intent(in) :: s
      integer, intent(out) :: err
      type(piece_t), allocatable :: tmp(:)
      integer :: cap, stat
      err = CAPNP_OK
      if (.not. allocated(items)) then
         allocate (items(8), stat=stat)
         if (stat /= 0) then
            err = CAPNP_ERR_ALLOC
            return
         end if
      end if
      if (n >= size(items)) then
         cap = size(items)*2
         allocate (tmp(cap), stat=stat)
         if (stat /= 0) then
            err = CAPNP_ERR_ALLOC
            return
         end if
         if (n > 0) tmp(1:n) = items(1:n)
         call move_alloc(tmp, items)
      end if
      n = n + 1
      items(n)%s = s
   end subroutine push

   subroutine join_items(items, n, indent, mode, is_record, out, err)
      type(piece_t), allocatable, intent(in) :: items(:)
      integer, intent(in) :: n, indent, mode
      logical, intent(in) :: is_record
      character(len=:), allocatable, intent(out) :: out
      integer, intent(out) :: err
      integer :: i, total
      logical :: inline
      character(len=:), allocatable :: sp, delim
      type(buf_t) :: buf
      err = CAPNP_OK
      out = ''
      if (n <= 0) return
      inline = indent == 0
      if (.not. inline) then
         inline = .true.
         total = 0
         do i = 1, n
            if (len(items(i)%s) > 24) inline = .false.
            if (index(items(i)%s, achar(10)) > 0) inline = .false.
            if (is_record) total = total + len(items(i)%s)
         end do
         if (is_record .and. total > 64) inline = .false.
      end if
      if (inline) then
         call buf_add(buf, items(1)%s, err)
         if (err /= CAPNP_OK) return
         do i = 2, n
            call buf_add(buf, ', '//items(i)%s, err)
            if (err /= CAPNP_OK) return
         end do
         call buf_take(buf, out)
         return
      end if
      sp = repeat(' ', indent*2)
      delim = ','//achar(10)//sp
      if (mode == MODE_BARE) then
         call buf_add(buf, ' ', err)
      else
         call buf_add(buf, achar(10)//sp, err)
      end if
      if (err /= CAPNP_OK) return
      call buf_add(buf, items(1)%s, err)
      if (err /= CAPNP_OK) return
      do i = 2, n
         call buf_add(buf, delim//items(i)%s, err)
         if (err /= CAPNP_OK) return
      end do
      call buf_add(buf, ' ', err)
      if (err /= CAPNP_OK) return
      call buf_take(buf, out)
   end subroutine join_items

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

   !> Builders are exempt from the reader charge, so the encoder applies
   !> the same default word budget before it walks the tree.
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

   subroutine skip_ws(sc)
      type(scan_t), intent(inout) :: sc
      character(len=1) :: c
      do
         if (sc%i > sc%n) return
         c = sc%s(sc%i:sc%i)
         if (c == ' ' .or. c == achar(9) .or. c == achar(10) .or. c == achar(13)) then
            sc%i = sc%i + 1
         else if (c == '#') then
            do
               sc%i = sc%i + 1
               if (sc%i > sc%n) return
               if (sc%s(sc%i:sc%i) == achar(10)) exit
            end do
         else
            return
         end if
      end do
   end subroutine skip_ws

   function peek(sc) result(c)
      type(scan_t), intent(in) :: sc
      character(len=1) :: c
      if (sc%i > sc%n) then
         c = achar(0)
      else
         c = sc%s(sc%i:sc%i)
      end if
   end function peek

   subroutine expect_ch(sc, ch, err)
      type(scan_t), intent(inout) :: sc
      character(len=1), intent(in) :: ch
      integer, intent(out) :: err
      err = CAPNP_OK
      call skip_ws(sc)
      if (peek(sc) /= ch) then
         err = CAPNP_ERR_ARG
         return
      end if
      sc%i = sc%i + 1
   end subroutine expect_ch

   subroutine read_ident(sc, name, err)
      type(scan_t), intent(inout) :: sc
      character(len=:), allocatable, intent(out) :: name
      integer, intent(out) :: err
      integer :: a
      err = CAPNP_OK
      name = ''
      call skip_ws(sc)
      if (.not. is_ident_start(peek(sc))) then
         err = CAPNP_ERR_ARG
         return
      end if
      a = sc%i
      sc%i = sc%i + 1
      do while (is_ident_cont(peek(sc)))
         sc%i = sc%i + 1
      end do
      name = sc%s(a:sc%i - 1)
   end subroutine read_ident

   subroutine read_numeric(sc, tok, err)
      type(scan_t), intent(inout) :: sc
      character(len=:), allocatable, intent(out) :: tok
      integer, intent(out) :: err
      integer :: a
      err = CAPNP_OK
      tok = ''
      call skip_ws(sc)
      a = sc%i
      if (peek(sc) == '+') then
         err = CAPNP_ERR_ARG
         return
      end if
      if (peek(sc) == '-') sc%i = sc%i + 1
      if (is_ident_start(peek(sc))) then
         sc%i = sc%i + 1
         do while (is_ident_cont(peek(sc)))
            sc%i = sc%i + 1
         end do
         tok = sc%s(a:sc%i - 1)
         return
      end if
      call scan_number(sc, err)
      if (err /= CAPNP_OK) return
      if (sc%i <= a) then
         err = CAPNP_ERR_ARG
         return
      end if
      tok = sc%s(a:sc%i - 1)
   end subroutine read_numeric

   subroutine scan_number(sc, err)
      type(scan_t), intent(inout) :: sc
      integer, intent(out) :: err
      integer :: nd
      character(len=1) :: c
      err = CAPNP_OK
      nd = 0
      if (peek(sc) == '.') then
         err = CAPNP_ERR_ARG
         return
      end if
      if (sc%i < sc%n) then
         if (sc%s(sc%i:sc%i) == '0' .and. sc%s(sc%i + 1:sc%i + 1) == 'x') then
            sc%i = sc%i + 2
            if (capnp_hex_digit(peek(sc)) < 0) then
               err = CAPNP_ERR_ARG
               return
            end if
            do while (capnp_hex_digit(peek(sc)) >= 0)
               sc%i = sc%i + 1
            end do
            return
         end if
      end if
      do while (capnp_dec_digit(peek(sc)) >= 0)
         sc%i = sc%i + 1
         nd = nd + 1
      end do
      if (peek(sc) == '.') then
         sc%i = sc%i + 1
         do while (capnp_dec_digit(peek(sc)) >= 0)
            sc%i = sc%i + 1
            nd = nd + 1
         end do
      end if
      c = peek(sc)
      if (c == 'e' .or. c == 'E') then
         sc%i = sc%i + 1
         c = peek(sc)
         if (c == '+' .or. c == '-') sc%i = sc%i + 1
         if (capnp_dec_digit(peek(sc)) < 0) then
            err = CAPNP_ERR_ARG
            return
         end if
         do while (capnp_dec_digit(peek(sc)) >= 0)
            sc%i = sc%i + 1
         end do
      end if
      if (nd == 0) err = CAPNP_ERR_ARG
   end subroutine scan_number

   subroutine read_string(sc, out, err)
      type(scan_t), intent(inout) :: sc
      character(len=:), allocatable, intent(out) :: out
      integer, intent(out) :: err
      character(len=:), allocatable :: piece, raw
      integer :: a
      err = CAPNP_OK
      out = ''
      call skip_ws(sc)
      if (peek(sc) /= '"') then
         err = CAPNP_ERR_ARG
         return
      end if
      do
         sc%i = sc%i + 1
         a = sc%i
         do
            if (sc%i > sc%n) then
               err = CAPNP_ERR_ARG
               return
            end if
            if (sc%s(sc%i:sc%i) == achar(10)) then
               err = CAPNP_ERR_ARG
               return
            end if
            if (sc%s(sc%i:sc%i) == '\') then
               if (sc%i + 1 > sc%n) then
                  err = CAPNP_ERR_ARG
                  return
               end if
               if (sc%s(sc%i + 1:sc%i + 1) == achar(10)) then
                  err = CAPNP_ERR_ARG
                  return
               end if
               sc%i = sc%i + 2
               cycle
            end if
            if (sc%s(sc%i:sc%i) == '"') exit
            sc%i = sc%i + 1
         end do
         if (sc%i > a) then
            raw = sc%s(a:sc%i - 1)
         else
            raw = ''
         end if
         sc%i = sc%i + 1
         call capnp_cunescape(raw, piece, err)
         if (err /= CAPNP_OK) return
         out = out//piece
         call skip_ws(sc)
         if (peek(sc) /= '"') return
      end do
   end subroutine read_string

   subroutine read_data(sc, raw, err)
      type(scan_t), intent(inout) :: sc
      integer(int8), allocatable, intent(out) :: raw(:)
      integer, intent(out) :: err
      character(len=:), allocatable :: s
      integer :: i, n, d1, d2, cap
      integer(int8), allocatable :: tmp(:)
      err = CAPNP_OK
      call skip_ws(sc)
      if (sc%i + 2 <= sc%n) then
         if ((sc%s(sc%i:sc%i + 2) == '0x"' .or. sc%s(sc%i:sc%i + 2) == '0X"')) then
            sc%i = sc%i + 3
            cap = 8
            allocate (tmp(0:cap - 1))
            n = 0
            do
               do while (peek(sc) == ' ' .or. peek(sc) == achar(9))
                  sc%i = sc%i + 1
               end do
               if (peek(sc) == '"') then
                  sc%i = sc%i + 1
                  exit
               end if
               d1 = capnp_hex_digit(peek(sc))
               sc%i = sc%i + 1
               d2 = capnp_hex_digit(peek(sc))
               if (d1 < 0 .or. d2 < 0) then
                  err = CAPNP_ERR_ARG
                  return
               end if
               sc%i = sc%i + 1
               if (n >= cap) then
                  allocate (raw(0:cap*2 - 1))
                  if (n > 0) raw(0:n - 1) = tmp(0:n - 1)
                  call move_alloc(raw, tmp)
                  cap = cap*2
               end if
               tmp(n) = capnp_as_i8(d1*16 + d2)
               n = n + 1
            end do
            if (n == 0) then
               allocate (raw(0))
            else
               allocate (raw(0:n - 1))
               raw(0:n - 1) = tmp(0:n - 1)
            end if
            return
         end if
      end if
      call read_string(sc, s, err)
      if (err /= CAPNP_OK) return
      n = len(s)
      if (n == 0) then
         allocate (raw(0))
         return
      end if
      allocate (raw(0:n - 1))
      do i = 1, n
         raw(i - 1) = capnp_as_i8(iachar(s(i:i)))
      end do
   end subroutine read_data

   function count_items(s, at) result(n)
      character(len=*), intent(in) :: s
      integer, intent(in) :: at
      integer :: n
      integer :: i, depth
      logical :: in_str, in_cmt, item
      character(len=1) :: c
      character(len=1) :: stk(MAX_DEPTH + 4)
      n = 0
      i = at + 1
      depth = 1
      stk(1) = '['
      in_str = .false.
      in_cmt = .false.
      item = .false.
      do while (i <= len(s))
         c = s(i:i)
         if (in_cmt) then
            if (c == achar(10)) in_cmt = .false.
            i = i + 1
            cycle
         end if
         if (in_str) then
            if (c == achar(10)) then
               n = -1
               return
            else if (c == '\') then
               i = i + 2
               cycle
            else if (c == '"') then
               in_str = .false.
            end if
            item = .true.
            i = i + 1
            cycle
         end if
         if (c == '#') then
            in_cmt = .true.
         else if (c == '"') then
            in_str = .true.
            item = .true.
         else if (c == '(' .or. c == '[') then
            if (depth >= size(stk)) then
               n = -1
               return
            end if
            depth = depth + 1
            stk(depth) = c
            item = .true.
         else if (c == ')' .or. c == ']') then
            if ((c == ')' .and. stk(depth) /= '(') .or. (c == ']' .and. stk(depth) /= '[')) then
               n = -1
               return
            end if
            depth = depth - 1
            if (depth == 0) then
               if (item) n = n + 1
               return
            end if
            item = .true.
         else if (c == ',' .and. depth == 1) then
            if (.not. item) then
               n = -1
               return
            end if
            n = n + 1
            item = .false.
         else if (c /= ' ' .and. c /= achar(9) .and. c /= achar(10) .and. c /= achar(13)) then
            item = .true.
         end if
         i = i + 1
      end do
      n = -1
   end function count_items

   subroutine match_close(text, beg, to, err)
      character(len=*), intent(in) :: text
      integer, intent(in) :: beg
      integer, intent(out) :: to
      integer, intent(out) :: err
      integer :: i, depth
      logical :: in_str, in_cmt
      character(len=1) :: c
      character(len=1) :: stk(MAX_DEPTH + 4)
      err = CAPNP_OK
      to = beg
      i = beg
      depth = 0
      in_str = .false.
      in_cmt = .false.
      do while (i <= len(text))
         c = text(i:i)
         if (in_cmt) then
            if (c == achar(10)) in_cmt = .false.
            i = i + 1
            cycle
         end if
         if (in_str) then
            if (c == achar(10)) then
               err = CAPNP_ERR_ARG
               return
            else if (c == '\') then
               i = i + 2
               cycle
            else if (c == '"') then
               in_str = .false.
            end if
            i = i + 1
            cycle
         end if
         if (c == '#') then
            in_cmt = .true.
         else if (c == '"') then
            in_str = .true.
         else if (c == '(' .or. c == '[') then
            if (depth >= size(stk)) then
               err = CAPNP_ERR_DEPTH
               return
            end if
            depth = depth + 1
            stk(depth) = c
         else if (c == ')' .or. c == ']') then
            if (depth <= 0) then
               err = CAPNP_ERR_ARG
               return
            end if
            if ((c == ')' .and. stk(depth) /= '(') .or. (c == ']' .and. stk(depth) /= '[')) then
               err = CAPNP_ERR_ARG
               return
            end if
            depth = depth - 1
            if (depth == 0) then
               to = i
               return
            end if
         end if
         i = i + 1
      end do
      err = CAPNP_ERR_ARG
   end subroutine match_close

   pure function is_ident_start(c) result(y)
      character(len=1), intent(in) :: c
      logical :: y
      y = (c >= 'A' .and. c <= 'Z') .or. (c >= 'a' .and. c <= 'z') .or. c == '_'
   end function is_ident_start

   pure function is_ident_cont(c) result(y)
      character(len=1), intent(in) :: c
      logical :: y
      y = is_ident_start(c) .or. (c >= '0' .and. c <= '9')
   end function is_ident_cont

   pure function has_alpha_non_hex(tok) result(y)
      character(len=*), intent(in) :: tok
      logical :: y
      integer :: k
      y = .false.
      if (len(tok) == 0) return
      k = 1
      if (tok(1:1) == '+' .or. tok(1:1) == '-') k = 2
      if (k > len(tok)) return
      if (is_ident_start(tok(k:k))) y = .true.
   end function has_alpha_non_hex

   pure function float_from_int(tok) result(y)
      character(len=*), intent(in) :: tok
      logical :: y
      integer :: i, k
      character(len=1) :: c
      y = .false.
      if (len(tok) == 0) return
      k = 1
      if (tok(1:1) == '+' .or. tok(1:1) == '-') k = 2
      if (k > len(tok)) return
      if (k + 1 <= len(tok)) then
         if (tok(k:k) == '0' .and. (tok(k + 1:k + 1) == 'x' .or. tok(k + 1:k + 1) == 'X')) then
            y = .true.
            return
         end if
      end if
      do i = k, len(tok)
         c = tok(i:i)
         if (c < '0' .or. c > '9') return
      end do
      y = .true.
   end function float_from_int

   pure function is_num_start(c) result(y)
      character(len=1), intent(in) :: c
      logical :: y
      y = (c >= '0' .and. c <= '9') .or. c == '+' .or. c == '-' .or. c == '.'
   end function is_num_start

end module capnp_text
