!> capnpf: encode, decode, convert, and eval, the Fortran counterparts of
!> the capnp tool. Schemas are compiled with the capnp driver
!> (`capnp compile -o-`) and loaded as a CodeGeneratorRequest.
program capnpf
   use, intrinsic :: iso_c_binding, only: c_int
   use capnp
   use capnp_schema
   use capnp_codec
   use capnp_dynamic
   use capnp_text
   use capnp_json
   implicit none

   interface
      function c_getpid() bind(c, name="getpid") result(p)
         import c_int
         integer(c_int) :: p
      end function c_getpid
   end interface

   character(len=4096) :: arg
   character(len=:), allocatable :: cmd, schema_path, name, spec, ofmt
   logical :: short, packed_in, flat_in
   integer :: i, n, err
   type(capnp_dyn_schema_t), target :: schema
   integer :: type_idx

   cmd = ''
   schema_path = ''
   name = ''
   spec = ''
   ofmt = 'text'
   short = .false.
   packed_in = .false.
   flat_in = .false.
   n = command_argument_count()
   if (n < 1) call usage()
   do i = 1, n
      call get_command_argument(i, arg)
      if (trim(arg) == '--short') then
         short = .true.
      else if (trim(arg) == '--packed' .or. trim(arg) == '-p') then
         packed_in = .true.
         ofmt = 'packed'
      else if (trim(arg) == '--flat') then
         flat_in = .true.
         ofmt = 'flat'
      else if (trim(arg) == '-b' .or. trim(arg) == '--binary') then
         ofmt = 'binary'
      else if (len_trim(arg) > 2 .and. arg(1:2) == '-o') then
         ofmt = trim(arg(3:))
      else if (index(trim(arg), '--output=') == 1) then
         ofmt = trim(arg(10:))
      else if (arg(1:1) == '-') then
         call die('unknown option '//trim(arg))
      else if (len(cmd) == 0) then
         cmd = trim(arg)
      else if (cmd == 'convert' .and. len(spec) == 0) then
         spec = trim(arg)
      else if (len(schema_path) == 0) then
         schema_path = trim(arg)
      else if (len(name) == 0) then
         name = trim(arg)
      else
         call die('too many arguments')
      end if
   end do

   if (cmd /= 'encode' .and. cmd /= 'decode' .and. cmd /= 'convert' .and. cmd /= 'eval') &
      call die('unknown command '//cmd)
   if (len(schema_path) == 0 .or. (cmd /= 'convert' .and. len(name) == 0)) call usage()
   if (cmd == 'convert') then
      if (len(spec) == 0 .or. len(name) == 0) call usage()
   end if

   call load_schema(schema_path, schema, err)
   if (err /= CAPNP_OK) call die('cannot load schema')

   select case (cmd)
   case ('encode')
      type_idx = capnp_dyn_find(schema, name, err)
      if (type_idx <= 0) call die('unknown type '//name)
      call cmd_encode(schema, type_idx, packed_in)
   case ('decode')
      type_idx = capnp_dyn_find(schema, name, err)
      if (type_idx <= 0) call die('unknown type '//name)
      call cmd_decode(schema, type_idx, packed_in, flat_in, short, 'text')
   case ('convert')
      type_idx = capnp_dyn_find(schema, name, err)
      if (type_idx <= 0) call die('unknown type '//name)
      call cmd_convert(schema, type_idx, spec, short)
   case ('eval')
      call cmd_eval(schema, name, ofmt, short)
   end select
   call capnp_dyn_free(schema)

contains

   subroutine usage()
      write (*, '(a)') 'usage: capnpf encode|decode|convert|eval [options] <schema> <type|name>'
      write (*, '(a)') '  convert <from>:<to> <schema> <type>'
      write (*, '(a)') 'formats: binary packed flat canonical text json'
      error stop 1
   end subroutine usage

   subroutine die(msg)
      character(len=*), intent(in) :: msg
      write (*, '(a)') 'capnpf: '//msg
      error stop 1
   end subroutine die

   subroutine remove_file(path)
      character(len=*), intent(in) :: path
      integer :: u, ios
      open (newunit=u, file=path, status='old', iostat=ios)
      if (ios == 0) close (u, status='delete', iostat=ios)
   end subroutine remove_file

   subroutine load_schema(path, sch, err)
      character(len=*), intent(in) :: path
      type(capnp_dyn_schema_t), intent(inout), target :: sch
      integer, intent(out) :: err
      character(len=64) :: tmp
      integer(int8), allocatable :: bytes(:)
      integer :: st, cs
      write (tmp, '(a,i0,a)') '/tmp/capnpf-', c_getpid(), '.cgr'
      call execute_command_line('INC=$(dirname $(dirname $(command -v capnp)))/include && '&
         //'capnp compile -I"$INC" -o- '//trim(path)//' > '//trim(tmp), &
         exitstat=st, cmdstat=cs, wait=.true.)
      if (cs /= 0 .or. st /= 0) then
         call remove_file(trim(tmp))
         err = CAPNP_ERR_IO
         return
      end if
      call capnp_read_file(trim(tmp), bytes, err)
      call remove_file(trim(tmp))
      if (err /= CAPNP_OK) return
      call capnp_dyn_load(sch, bytes, err)
   end subroutine load_schema

   subroutine cmd_encode(sch, idx, pack)
      type(capnp_dyn_schema_t), intent(in) :: sch
      integer, intent(in) :: idx
      logical, intent(in) :: pack
      character(len=:), allocatable :: text
      integer(int8), allocatable :: raw(:), framed(:)
      type(capnp_message_t), target :: msg
      integer :: beg, to, from, err, unit
      call read_stdin(raw, err)
      if (err /= CAPNP_OK) call die('empty input')
      text = bytes_to_text(raw)
      from = 1
      open (newunit=unit, file='/dev/stdout', access='stream', form='unformatted', &
            action='write', status='old', iostat=err)
      if (err /= 0) call die('cannot write stdout')
      do
         call capnp_text_span(text, from, beg, to, err)
         if (err /= CAPNP_OK) call die('bad text')
         if (beg == 0) exit
         call capnp_text_decode(sch, idx, text(beg:to), msg, err)
         if (err /= CAPNP_OK) call die('decode failed')
         if (pack) then
            call capnp_serialize_packed_bytes(msg, framed, err)
         else
            call capnp_serialize_bytes(msg, framed, err)
         end if
         if (err /= CAPNP_OK) call die('serialize failed')
         write (unit) framed
         call capnp_message_free(msg)
         from = to + 1
      end do
      close (unit)
   end subroutine cmd_encode

   subroutine cmd_decode(sch, idx, pack, flat, short, kind)
      type(capnp_dyn_schema_t), intent(in) :: sch
      integer, intent(in) :: idx
      logical, intent(in) :: pack, flat, short
      character(len=*), intent(in) :: kind
      integer(int8), allocatable :: raw(:)
      integer :: err
      call read_stdin(raw, err)
      if (err /= CAPNP_OK) call die('empty input')
      call emit_messages(sch, idx, raw, pack, flat, kind, .not. short)
   end subroutine cmd_decode

   subroutine cmd_convert(sch, idx, conv, short)
      type(capnp_dyn_schema_t), intent(in) :: sch
      integer, intent(in) :: idx
      character(len=*), intent(in) :: conv
      logical, intent(in) :: short
      integer :: colon
      character(len=:), allocatable :: from, to
      integer(int8), allocatable :: raw(:)
      logical :: pack, flat
      integer :: err
      colon = index(conv, ':')
      if (colon <= 1) call die('convert expects from:to')
      from = conv(1:colon - 1)
      to = conv(colon + 1:)
      pack = from == 'packed' .or. from == 'flat-packed'
      flat = from == 'flat' .or. from == 'flat-packed' .or. from == 'canonical'
      call read_stdin(raw, err)
      if (err /= CAPNP_OK) call die('empty input')
      if (from == 'text') then
         call text_to_out(sch, idx, bytes_to_text(raw), to)
      else if (from == 'json') then
         call json_to_out(sch, idx, bytes_to_text(raw), to)
      else
         call bytes_to_out(sch, idx, raw, pack, flat, to, .not. short)
      end if
   end subroutine cmd_convert

   subroutine text_to_out(sch, idx, text, to)
      type(capnp_dyn_schema_t), intent(in) :: sch
      integer, intent(in) :: idx
      character(len=*), intent(in) :: text, to
      type(capnp_message_t), target :: msg
      integer :: beg, topos, from, err, unit
      from = 1
      open (newunit=unit, file='/dev/stdout', access='stream', form='unformatted', &
            action='write', status='old', iostat=err)
      if (err /= 0) call die('cannot write stdout')
      do
         call capnp_text_span(text, from, beg, topos, err)
         if (err /= CAPNP_OK) call die('bad text')
         if (beg == 0) exit
         call capnp_text_decode(sch, idx, text(beg:topos), msg, err)
         if (err /= CAPNP_OK) call die('text decode failed')
         call write_message(unit, msg, to, err)
         if (err /= CAPNP_OK) call die('convert failed')
         call capnp_message_free(msg)
         from = topos + 1
         if (to == 'text' .or. to == 'json') exit
      end do
      close (unit)
   end subroutine text_to_out

   subroutine json_to_out(sch, idx, text, to)
      type(capnp_dyn_schema_t), intent(in) :: sch
      integer, intent(in) :: idx
      character(len=*), intent(in) :: text, to
      type(capnp_message_t), target :: msg
      integer :: err, unit
      call capnp_json_decode(sch, idx, text, msg, err)
      if (err /= CAPNP_OK) call die('json decode failed')
      open (newunit=unit, file='/dev/stdout', access='stream', form='unformatted', &
            action='write', status='old', iostat=err)
      if (err /= 0) call die('cannot write stdout')
      call write_message(unit, msg, to, err)
      if (err /= CAPNP_OK) call die('convert failed')
      close (unit)
      call capnp_message_free(msg)
   end subroutine json_to_out

   subroutine bytes_to_out(sch, idx, raw, pack, flat, to, pretty)
      type(capnp_dyn_schema_t), intent(in) :: sch
      integer, intent(in) :: idx
      integer(int8), intent(in) :: raw(0:)
      logical, intent(in) :: pack, flat, pretty
      character(len=*), intent(in) :: to
      type(capnp_message_t), target :: msg
      integer(int8), allocatable :: one(:), wrapped(:)
      integer(int64) :: off, n, total
      integer :: err
      character(len=:), allocatable :: text
      type(capnp_ptr_t) :: root
      if (flat) then
         wrapped = wrap_flat(raw)
         call capnp_deserialize_bytes(wrapped, msg, err)
         if (err /= CAPNP_OK) call die('bad flat input')
         root = capnp_root(msg, err)
         call emit_one(sch, idx, msg, root, to, pretty)
         call capnp_message_free(msg)
         return
      end if
      if (pack) then
         call capnp_deserialize_packed_bytes(raw, msg, err)
         if (err /= CAPNP_OK) call die('bad packed input')
         root = capnp_root(msg, err)
         call emit_one(sch, idx, msg, root, to, pretty)
         call capnp_message_free(msg)
         return
      end if
      off = 0_int64
      total = size(raw, kind=int64)
      do while (off < total)
         n = frame_nbytes(raw, off, err)
         if (err /= CAPNP_OK) call die('bad framing')
         allocate (one(0:n - 1))
         one = raw(off:off + n - 1)
         call capnp_deserialize_bytes(one, msg, err)
         if (err /= CAPNP_OK) call die('bad message')
         root = capnp_root(msg, err)
         call emit_one(sch, idx, msg, root, to, pretty)
         call capnp_message_free(msg)
         deallocate (one)
         off = off + n
      end do
      if (allocated(text)) text = ''
   end subroutine bytes_to_out

   subroutine emit_messages(sch, idx, raw, pack, flat, kind, pretty)
      type(capnp_dyn_schema_t), intent(in) :: sch
      integer, intent(in) :: idx
      integer(int8), intent(in) :: raw(0:)
      logical, intent(in) :: pack, flat, pretty
      character(len=*), intent(in) :: kind
      call bytes_to_out(sch, idx, raw, pack, flat, kind, pretty)
   end subroutine emit_messages

   subroutine emit_one(sch, idx, msg, root, to, pretty)
      type(capnp_dyn_schema_t), intent(in) :: sch
      integer, intent(in) :: idx
      type(capnp_message_t), intent(inout), target :: msg
      type(capnp_ptr_t), intent(in) :: root
      character(len=*), intent(in) :: to
      logical, intent(in) :: pretty
      character(len=:), allocatable :: text
      integer(int8), allocatable :: framed(:)
      integer :: err, unit
      if (to == 'text') then
         call capnp_text_encode(sch, idx, root, text, err, pretty=pretty)
         if (err /= CAPNP_OK) call die('text encode failed')
         write (*, '(a)') text
      else if (to == 'json') then
         call capnp_json_encode(sch, idx, root, text, err, pretty=pretty)
         if (err /= CAPNP_OK) call die('json encode failed')
         write (*, '(a)') text
      else
         open (newunit=unit, file='/dev/stdout', access='stream', form='unformatted', &
               action='write', status='old', iostat=err)
         if (err /= 0) call die('cannot write stdout')
         call write_message(unit, msg, to, err)
         if (err /= CAPNP_OK) call die('binary encode failed')
         close (unit)
      end if
      if (allocated(framed)) deallocate (framed)
   end subroutine emit_one

   subroutine write_message(unit, msg, to, err)
      integer, intent(in) :: unit
      type(capnp_message_t), intent(inout), target :: msg
      character(len=*), intent(in) :: to
      integer, intent(out) :: err
      integer(int8), allocatable :: framed(:)
      character(len=:), allocatable :: text
      type(capnp_ptr_t) :: root
      err = CAPNP_OK
      if (to == 'canonical') then
         call capnp_canonicalize(msg, framed, err)
      else if (to == 'packed') then
         call capnp_serialize_packed_bytes(msg, framed, err)
      else if (to == 'flat') then
         call capnp_serialize_bytes(msg, framed, err)
         if (err /= CAPNP_OK) return
         framed = strip_header(framed)
      else if (to == 'text' .or. to == 'json') then
         root = capnp_root(msg, err)
         if (err /= CAPNP_OK) return
         if (to == 'text') then
            call capnp_text_encode(schema, type_at(), root, text, err, pretty=.true.)
         else
            call capnp_json_encode(schema, type_at(), root, text, err, pretty=.true.)
         end if
         if (err /= CAPNP_OK) return
         write (*, '(a)') text
         return
      else
         call capnp_serialize_bytes(msg, framed, err)
      end if
      if (err /= CAPNP_OK) return
      write (unit) framed
   end subroutine write_message

   function type_at() result(idx)
      integer :: idx
      integer :: err
      idx = capnp_dyn_find(schema, name, err)
   end function type_at

   subroutine cmd_eval(sch, spec, fmt, short)
      type(capnp_dyn_schema_t), intent(in) :: sch
      character(len=*), intent(in) :: spec, fmt
      logical, intent(in) :: short
      integer :: idx, best, i, err, nlen
      character(len=:), allocatable :: trial, rest, text
      logical :: as_json
      idx = 0
      best = 0
      nlen = len(spec)
      i = 1
      do while (i <= nlen)
         if (spec(i:i) == '[') then
            trial = spec(1:i - 1)
            if (capnp_dyn_find(sch, trial, err) > 0) then
               idx = capnp_dyn_find(sch, trial, err)
               best = i - 1
            end if
            exit
         end if
         if (spec(i:i) == '.') then
            trial = spec(1:i - 1)
            if (capnp_dyn_find(sch, trial, err) > 0) then
               idx = capnp_dyn_find(sch, trial, err)
               best = i - 1
            end if
         end if
         i = i + 1
      end do
      trial = spec(1:nlen)
      if (capnp_dyn_find(sch, trial, err) > 0) then
         idx = capnp_dyn_find(sch, trial, err)
         best = nlen
      end if
      if (idx <= 0) call die('unknown name '//spec)
      if (best < nlen) then
         rest = spec(best + 1:)
      else
         rest = ''
      end if
      as_json = fmt == 'json'
      call render_node(sch, idx, rest, .not. short, as_json, text, err)
      if (err /= CAPNP_OK) call die('eval failed')
      if (fmt == 'text' .or. fmt == 'json') then
         write (*, '(a)') text
      else
         call die('eval binary output is not implemented for this value')
      end if
   end subroutine cmd_eval

   subroutine render_node(sch, idx, rest, pretty, as_json, text, err)
      type(capnp_dyn_schema_t), intent(in) :: sch
      integer, intent(in) :: idx
      character(len=*), intent(in) :: rest
      logical, intent(in) :: pretty, as_json
      character(len=:), allocatable, intent(out) :: text
      integer, intent(out) :: err
      type(capnp_ptr_t) :: typ, val, f
      integer :: gidx
      err = CAPNP_OK
      text = ''
      if (node_which(sch%nodes(idx)) == NODE_CONST) then
         typ = node_const_type(sch%nodes(idx), err)
         if (err /= CAPNP_OK) return
         val = node_const_value(sch%nodes(idx), err)
         if (err /= CAPNP_OK) return
         if (len(rest) == 0) then
            if (as_json .and. type_which(typ) == TYPE_STRUCT) then
               gidx = find_id(sch, type_type_id(typ))
               f = value_pointer(val, err)
               if (err /= CAPNP_OK) return
               call capnp_json_encode(sch, gidx, f, text, err, pretty=pretty)
            else if (as_json) then
               call capnp_text_encode_value(sch, typ, val, text, err, pretty=.false.)
            else
               call capnp_text_encode_value(sch, typ, val, text, err, pretty=pretty)
            end if
            return
         end if
         call walk_const(sch, typ, val, rest, pretty, as_json, text, err)
         return
      end if
      if (node_which(sch%nodes(idx)) /= NODE_STRUCT .or. len(rest) < 2 .or. rest(1:1) /= '.') then
         err = CAPNP_ERR_ARG
         return
      end if
      if (index(rest(2:), '.') /= 0 .or. index(rest, '[') /= 0) then
         err = CAPNP_ERR_ARG
         return
      end if
      call find_field(sch, idx, rest(2:), f, err)
      if (err /= CAPNP_OK) return
      if (field_which(f) == FIELD_GROUP) then
         gidx = find_id(sch, field_group_type_id(f))
         if (as_json) then
            call capnp_json_encode(sch, gidx, capnp_ptr_t(), text, err, pretty=pretty)
         else
            call capnp_text_encode(sch, gidx, capnp_ptr_t(), text, err, pretty=pretty)
         end if
         return
      end if
      typ = field_slot_type(f, err)
      if (err /= CAPNP_OK) return
      val = field_slot_default(f, err)
      if (err /= CAPNP_OK) return
      if (as_json) then
         call json_value(sch, typ, val, text, err, pretty)
      else
         call capnp_text_encode_value(sch, typ, val, text, err, pretty=pretty)
      end if
   end subroutine render_node

   subroutine walk_const(sch, typ, val, rest, pretty, as_json, text, err)
      type(capnp_dyn_schema_t), intent(in) :: sch
      type(capnp_ptr_t), intent(in) :: typ, val
      character(len=*), intent(in) :: rest
      logical, intent(in) :: pretty, as_json
      character(len=:), allocatable, intent(out) :: text
      integer, intent(out) :: err
      type(capnp_ptr_t) :: obj, f, et
      integer :: sidx, ib, ie, idx
      integer(int64) :: iv
      err = CAPNP_OK
      text = ''
      if (type_which(typ) == TYPE_STRUCT .and. rest(1:1) == '.' .and. index(rest(2:), '.') == 0 &
          .and. index(rest, '[') == 0) then
         obj = value_pointer(val, err)
         if (err /= CAPNP_OK) return
         sidx = find_id(sch, type_type_id(typ))
         call find_field(sch, sidx, rest(2:), f, err)
         if (err /= CAPNP_OK) return
         call render_field(sch, f, obj, pretty, as_json, text, err)
         return
      end if
      if (type_which(typ) == TYPE_LIST .and. rest(1:1) == '[') then
         ie = index(rest, ']')
         if (ie <= 2) then
            err = CAPNP_ERR_ARG
            return
         end if
         call capnp_parse_i64(rest(2:ie - 1), iv, err)
         if (err /= CAPNP_OK) return
         obj = value_pointer(val, err)
         if (err /= CAPNP_OK) return
         et = type_list_element(typ, err)
         if (err /= CAPNP_OK) return
         call render_element(sch, et, obj, iv, pretty, as_json, text, err)
         if (err /= CAPNP_OK) return
         if (ie < len(rest)) then
            err = CAPNP_ERR_ARG
         end if
         return
      end if
      ib = 0
      idx = 0
      err = CAPNP_ERR_ARG
   end subroutine walk_const

   subroutine render_field(sch, f, obj, pretty, as_json, text, err)
      type(capnp_dyn_schema_t), intent(in) :: sch
      type(capnp_ptr_t), intent(in) :: f, obj
      logical, intent(in) :: pretty, as_json
      character(len=:), allocatable, intent(out) :: text
      integer, intent(out) :: err
      type(capnp_ptr_t) :: t, dv, q
      integer :: tw, gidx
      integer(int64) :: off
      err = CAPNP_OK
      if (field_which(f) == FIELD_GROUP) then
         gidx = find_id(sch, field_group_type_id(f))
         if (as_json) then
            call capnp_json_encode(sch, gidx, obj, text, err, pretty=pretty)
         else
            call capnp_text_encode(sch, gidx, obj, text, err, pretty=pretty)
         end if
         return
      end if
      t = field_slot_type(f, err)
      if (err /= CAPNP_OK) return
      dv = field_slot_default(f, err)
      if (err /= CAPNP_OK) return
      tw = type_which(t)
      off = field_slot_offset(f)
      if (tw == TYPE_STRUCT) then
         q = capnp_getp(obj, int(off), err)
         if (err /= CAPNP_OK) return
         gidx = find_id(sch, type_type_id(t))
         if (as_json) then
            call capnp_json_encode(sch, gidx, q, text, err, pretty=pretty)
         else
            call capnp_text_encode(sch, gidx, q, text, err, pretty=pretty)
         end if
         return
      end if
      call format_scalar(obj, t, dv, off, .false., 0_int64, text, err)
      if (as_json .and. (tw == TYPE_TEXT .or. tw == TYPE_ENUM .or. tw == TYPE_DATA &
                         .or. tw == TYPE_INT64 .or. tw == TYPE_UINT64)) then
         if (tw == TYPE_INT64 .or. tw == TYPE_UINT64) text = '"'//text//'"'
      end if
   end subroutine render_field

   subroutine render_element(sch, et, lst, i, pretty, as_json, text, err)
      type(capnp_dyn_schema_t), intent(in) :: sch
      type(capnp_ptr_t), intent(in) :: et, lst
      integer(int64), intent(in) :: i
      logical, intent(in) :: pretty, as_json
      character(len=:), allocatable, intent(out) :: text
      integer, intent(out) :: err
      type(capnp_ptr_t) :: el
      integer :: tw, gidx
      err = CAPNP_OK
      tw = type_which(et)
      if (tw == TYPE_STRUCT) then
         el = capnp_list_get_struct(lst, int(i), err)
         if (err /= CAPNP_OK) return
         gidx = find_id(sch, type_type_id(et))
         if (as_json) then
            call capnp_json_encode(sch, gidx, el, text, err, pretty=pretty)
         else
            call capnp_text_encode(sch, gidx, el, text, err, pretty=pretty)
         end if
         return
      end if
      call format_scalar(lst, et, capnp_ptr_t(), 0_int64, .true., i, text, err)
      if (as_json .and. (tw == TYPE_INT64 .or. tw == TYPE_UINT64)) text = '"'//text//'"'
   end subroutine render_element

   subroutine format_scalar(p, t, dv, off, in_list, ix, text, err)
      type(capnp_ptr_t), intent(in) :: p, t, dv
      integer(int64), intent(in) :: off, ix
      logical, intent(in) :: in_list
      character(len=:), allocatable, intent(out) :: text
      integer, intent(out) :: err
      integer :: tw
      character(len=:), allocatable :: s, esc
      integer(int8), allocatable :: raw(:)
      err = CAPNP_OK
      tw = type_which(t)
      select case (tw)
      case (TYPE_VOID)
         text = 'void'
      case (TYPE_BOOL)
         if (in_list) then
            if (capnp_list_get_bool(p, ix, err)) then
               text = 'true'
            else
               text = 'false'
            end if
         else if (capnp_get_bool(p, off, default=defb(dv))) then
            text = 'true'
         else
            text = 'false'
         end if
      case (TYPE_INT32)
         if (in_list) then
            call capnp_fmt_i64(int(capnp_list_get_i32(p, ix, err), int64), text)
         else
            call capnp_fmt_i64(int(capnp_get_i32(p, off*4_int64, default=defi32(dv)), int64), text)
         end if
      case (TYPE_INT64)
         if (in_list) then
            call capnp_fmt_i64(capnp_list_get_i64(p, ix, err), text)
         else
            call capnp_fmt_i64(capnp_get_i64(p, off*8_int64, default=defi64(dv)), text)
         end if
      case (TYPE_FLOAT32)
         if (in_list) then
            call capnp_fmt_f32(capnp_list_get_f32(p, ix, err), text)
         else
            call capnp_fmt_f32(capnp_get_f32(p, off*4_int64, default=deff32(dv)), text)
         end if
      case (TYPE_FLOAT64)
         if (in_list) then
            call capnp_fmt_f64(capnp_list_get_f64(p, ix, err), text)
         else
            call capnp_fmt_f64(capnp_get_f64(p, off*8_int64, default=deff64(dv)), text)
         end if
      case (TYPE_TEXT)
         if (in_list) then
            call capnp_list_get_text(p, int(ix), s, err)
         else
            call capnp_get_text(p, int(off), s, err)
         end if
         if (err /= CAPNP_OK) return
         call capnp_cescape_text(s, esc)
         text = '"'//esc//'"'
      case (TYPE_DATA)
         call capnp_get_data(p, int(off), raw, err)
         if (err /= CAPNP_OK) return
         call capnp_cescape_data(raw, esc)
         text = '"'//esc//'"'
      case default
         err = CAPNP_ERR_KIND
      end select
   end subroutine format_scalar

   function defb(dv) result(d)
      type(capnp_ptr_t), intent(in) :: dv
      logical :: d
      d = .false.
      if (dv%kind /= CAPNP_PK_NULL) d = value_bool(dv)
   end function defb
   function defi32(dv) result(d)
      type(capnp_ptr_t), intent(in) :: dv
      integer(int32) :: d
      d = 0_int32
      if (dv%kind /= CAPNP_PK_NULL) d = value_i32(dv)
   end function defi32
   function defi64(dv) result(d)
      type(capnp_ptr_t), intent(in) :: dv
      integer(int64) :: d
      d = 0_int64
      if (dv%kind /= CAPNP_PK_NULL) d = value_i64(dv)
   end function defi64
   function deff32(dv) result(d)
      type(capnp_ptr_t), intent(in) :: dv
      real(real32) :: d
      d = 0.0_real32
      if (dv%kind /= CAPNP_PK_NULL) d = value_f32(dv)
   end function deff32
   function deff64(dv) result(d)
      type(capnp_ptr_t), intent(in) :: dv
      real(real64) :: d
      d = 0.0_real64
      if (dv%kind /= CAPNP_PK_NULL) d = value_f64(dv)
   end function deff64

   subroutine json_value(sch, typ, val, text, err, pretty)
      type(capnp_dyn_schema_t), intent(in) :: sch
      type(capnp_ptr_t), intent(in) :: typ, val
      character(len=:), allocatable, intent(out) :: text
      integer, intent(out) :: err
      logical, intent(in) :: pretty
      character(len=:), allocatable :: body
      integer :: tw
      ! JSON of a non-struct value: render text then that is not JSON.
      ! Struct and list values are encoded by building nothing; use text
      ! spelling only for scalars here, and the struct path above.
      tw = type_which(typ)
      call capnp_text_encode_value(sch, typ, val, body, err, pretty=.false.)
      if (err /= CAPNP_OK) return
      if (tw == TYPE_TEXT .or. tw == TYPE_ENUM .or. tw == TYPE_DATA) then
         text = body
      else if (tw == TYPE_STRUCT .or. tw == TYPE_LIST) then
         text = body
      else
         text = body
      end if
      if (.not. pretty) text = text
   end subroutine json_value

   subroutine find_field(sch, idx, fname, f, err)
      type(capnp_dyn_schema_t), intent(in) :: sch
      integer, intent(in) :: idx
      character(len=*), intent(in) :: fname
      type(capnp_ptr_t), intent(out) :: f
      integer, intent(out) :: err
      type(capnp_ptr_t) :: fl
      character(len=:), allocatable :: nm
      integer :: i, n
      fl = node_struct_fields(sch%nodes(idx), err)
      if (err /= CAPNP_OK) return
      n = int(capnp_list_len(fl))
      do i = 0, n - 1
         f = capnp_list_get_struct(fl, i, err)
         if (err /= CAPNP_OK) return
         call field_name(f, nm, err)
         if (err /= CAPNP_OK) return
         if (nm == fname) return
      end do
      err = CAPNP_ERR_ARG
   end subroutine find_field

   function find_id(sch, id) result(idx)
      type(capnp_dyn_schema_t), intent(in) :: sch
      integer(int64), intent(in) :: id
      integer :: idx, i
      idx = 0
      do i = 1, size(sch%nodes)
         if (node_id(sch%nodes(i)) == id) then
            idx = i
            return
         end if
      end do
   end function find_id

   function frame_nbytes(raw, off, err) result(n)
      integer(int8), intent(in) :: raw(0:)
      integer(int64), intent(in) :: off
      integer, intent(out) :: err
      integer(int64) :: n, nsegs, header, i, words
      err = CAPNP_OK
      n = 0_int64
      if (off + 8_int64 > size(raw, kind=int64)) then
         err = CAPNP_ERR_FRAMING
         return
      end if
      nsegs = iand(int(cp_get_i32(raw, off), int64), 4294967295_int64) + 1_int64
      if (nsegs < 1_int64 .or. nsegs > 512_int64) then
         err = CAPNP_ERR_FRAMING
         return
      end if
      header = ((1_int64 + nsegs)*4_int64 + 7_int64)/8_int64*8_int64
      if (off + header > size(raw, kind=int64)) then
         err = CAPNP_ERR_FRAMING
         return
      end if
      n = header
      do i = 1_int64, nsegs
         words = iand(int(cp_get_i32(raw, off + i*4_int64), int64), 4294967295_int64)
         n = n + words*8_int64
      end do
      if (off + n > size(raw, kind=int64)) err = CAPNP_ERR_FRAMING
   end function frame_nbytes

   function wrap_flat(raw) result(framed)
      integer(int8), intent(in) :: raw(0:)
      integer(int8), allocatable :: framed(:)
      integer(int64) :: n, words
      n = size(raw, kind=int64)
      words = n/8_int64
      allocate (framed(0:n + 7))
      framed = 0_int8
      framed(4) = int(iand(words, 255_int64), int8)
      framed(5) = int(iand(shiftr(words, 8), 255_int64), int8)
      framed(6) = int(iand(shiftr(words, 16), 255_int64), int8)
      framed(7) = int(iand(shiftr(words, 24), 255_int64), int8)
      if (n > 0_int64) framed(8:n + 7) = raw
   end function wrap_flat

   function strip_header(framed) result(raw)
      integer(int8), intent(in) :: framed(0:)
      integer(int8), allocatable :: raw(:)
      integer(int64) :: nsegs, header, n
      nsegs = iand(int(cp_get_i32(framed, 0_int64), int64), 4294967295_int64) + 1_int64
      header = ((1_int64 + nsegs)*4_int64 + 7_int64)/8_int64*8_int64
      n = size(framed, kind=int64) - header
      allocate (raw(0:max(n, 1_int64) - 1))
      if (n > 0_int64) raw = framed(header:header + n - 1)
   end function strip_header

   function bytes_to_text(raw) result(s)
      integer(int8), intent(in) :: raw(0:)
      character(len=:), allocatable :: s
      integer(int64) :: i, n
      n = size(raw, kind=int64)
      allocate (character(len=n) :: s)
      do i = 0_int64, n - 1_int64
         s(int(i) + 1:int(i) + 1) = char(capnp_u_of_i8(raw(i)))
      end do
   end function bytes_to_text

   subroutine read_stdin(b, err)
      integer(int8), allocatable, intent(out) :: b(:)
      integer, intent(out) :: err
      integer(int8), allocatable :: tmp(:)
      integer(int8) :: byte
      integer(int64) :: n, cap
      integer :: unit, ios
      err = CAPNP_OK
      open (newunit=unit, file='/dev/stdin', access='stream', form='unformatted', &
            action='read', iostat=ios)
      if (ios /= 0) then
         err = CAPNP_ERR_IO
         return
      end if
      cap = 65536_int64
      allocate (b(0:cap - 1))
      n = 0_int64
      do
         read (unit, iostat=ios) byte
         if (ios /= 0) exit
         if (n == cap) then
            allocate (tmp(0:2*cap - 1))
            tmp(0:cap - 1) = b
            call move_alloc(tmp, b)
            cap = 2*cap
         end if
         b(n) = byte
         n = n + 1
      end do
      close (unit)
      allocate (tmp(0:max(n, 1_int64) - 1))
      if (n > 0_int64) tmp(0:n - 1) = b(0:n - 1)
      call move_alloc(tmp, b)
      if (n == 0_int64) err = CAPNP_ERR_IO
   end subroutine read_stdin

end program capnpf
