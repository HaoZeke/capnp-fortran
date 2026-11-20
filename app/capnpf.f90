!> capnpf: encode, decode, convert, and eval, the Fortran counterparts of
!> the capnp tool. Schemas are compiled with the capnp driver
!> (`capnp compile -o-`) and loaded as a CodeGeneratorRequest.
program capnpf
   use, intrinsic :: iso_c_binding, only: c_int, c_char, c_ptr, c_loc, c_size_t, &
      c_null_char, c_associated, c_intptr_t, c_long
   use os_errno, only: os_errno_value, os_eintr
   use capnp
   use capnp_schema
   use capnp_codec
   use capnp_dynamic
   use capnp_text
   use capnp_json
   implicit none

   interface
      subroutine c_exit(code) bind(c, name="exit")
         import c_int
         integer(c_int), value :: code
      end subroutine c_exit
      function c_popen(cmd, mode) bind(c, name="popen") result(fp)
         import c_ptr
         type(c_ptr), value :: cmd, mode
         type(c_ptr) :: fp
      end function c_popen
      function c_fread(buf, sz, nmemb, fp) bind(c, name="fread") result(n)
         import c_ptr, c_size_t
         type(c_ptr), value :: buf, fp
         integer(c_size_t), value :: sz, nmemb
         integer(c_size_t) :: n
      end function c_fread
      function c_pclose(fp) bind(c, name="pclose") result(st)
         import c_ptr, c_int
         type(c_ptr), value :: fp
         integer(c_int) :: st
      end function c_pclose
      function c_ferror(fp) bind(c, name="ferror") result(st)
         import c_ptr, c_int
         type(c_ptr), value :: fp
         integer(c_int) :: st
      end function c_ferror
      function c_write(fd, buf, n) bind(c, name="write") result(nw)
         import c_int, c_ptr, c_size_t, c_long
         integer(c_int), value :: fd
         type(c_ptr), value :: buf
         integer(c_size_t), value :: n
         integer(c_long) :: nw
      end function c_write
   end interface

   integer :: exit_code
   exit_code = 0
   call main_run(exit_code)
   if (exit_code /= 0) call c_exit(int(exit_code, c_int))

contains

   !> The capnp tool reads with both limits at the maximum.
   function tool_limits() result(opt)
      type(capnp_reader_options_t) :: opt
      opt%traversal_words = huge(0_int64)
      opt%nesting_limit = huge(0)
   end function tool_limits

   subroutine main_run(rc)
      integer, intent(out) :: rc
      character(len=:), allocatable :: arg, next_arg
      character(len=:), allocatable :: cmd, schema_path, name, spec, ofmt, inc_flags
      character(len=:), allocatable :: from_fmt, to_fmt
      logical :: short, packed_flag, flat_flag, ofmt_explicit, no_std_inc
      integer(int64) :: seg_words
      integer :: i, n, err, colon, type_idx
      type(capnp_dyn_schema_t), target :: schema

      rc = 0
      cmd = ''
      schema_path = ''
      name = ''
      spec = ''
      ofmt = 'text'
      inc_flags = ''
      short = .false.
      packed_flag = .false.
      flat_flag = .false.
      ofmt_explicit = .false.
      no_std_inc = .false.
      seg_words = 0_int64

      n = command_argument_count()
      if (n < 1) then
         call print_usage(2, err)
         rc = 1
         return
      end if
      i = 1
      do while (i <= n)
         call take_arg(i, arg)
         if (trim(arg) == '--help' .or. trim(arg) == '-h') then
            call print_usage(1, err)
            return
         else if (trim(arg) == '--version') then
            call write_fd_line(1, "Cap'n Proto (capnpf) version 0.3.1", err)
            return
         else if (trim(arg) == '--short') then
            short = .true.
         else if (trim(arg) == '--quiet' .or. trim(arg) == '--verbose') then
            continue
         else if (trim(arg) == '--no-standard-import') then
            no_std_inc = .true.
         else if (trim(arg) == '-I') then
            if (i + 1 > n) then
               call fail('-I requires a directory', rc)
               return
            end if
            i = i + 1
            call take_arg(i, next_arg)
            inc_flags = inc_flags//' -I'//shell_quote(next_arg)
         else if (starts_with(arg, '-I') .and. len(arg) > 2) then
            inc_flags = inc_flags//' -I'//shell_quote(trim(arg(3:)))
         else if (index(trim(arg), '--import-path=') == 1) then
            inc_flags = inc_flags//' -I'//shell_quote(trim(arg(15:)))
         else if (index(trim(arg), '--segment-size=') == 1) then
            call capnp_parse_i64(trim(arg(16:)), seg_words, err)
            if (err /= CAPNP_OK .or. seg_words < 1_int64) then
               call fail('bad --segment-size', rc)
               return
            end if
         else if (trim(arg) == '--packed' .or. trim(arg) == '-p') then
            packed_flag = .true.
         else if (trim(arg) == '--flat') then
            flat_flag = .true.
         else if (trim(arg) == '-b' .or. trim(arg) == '--binary') then
            ofmt = 'binary'
            ofmt_explicit = .true.
         else if (trim(arg) == '-o') then
            if (i + 1 > n) then
               call fail('-o requires a format', rc)
               return
            end if
            i = i + 1
            call take_arg(i, next_arg)
            ofmt = next_arg
            ofmt_explicit = .true.
         else if (starts_with(arg, '-o') .and. len(arg) > 2) then
            ofmt = trim(arg(3:))
            ofmt_explicit = .true.
         else if (index(trim(arg), '--output=') == 1) then
            ofmt = trim(arg(10:))
            ofmt_explicit = .true.
         else if (starts_with(arg, '-')) then
            call fail('unknown option '//trim(arg), rc)
            return
         else if (len(cmd) == 0) then
            cmd = trim(arg)
            if (cmd == 'help') then
               call print_usage(1, err)
               return
            end if
         else if (cmd == 'convert' .and. len(spec) == 0) then
            spec = trim(arg)
         else if (len(schema_path) == 0) then
            schema_path = trim(arg)
         else if (len(name) == 0) then
            name = trim(arg)
         else
            call fail('too many arguments', rc)
            return
         end if
         i = i + 1
      end do

      if (.not. ofmt_explicit) then
         if (flat_flag .and. packed_flag) then
            ofmt = 'flat-packed'
         else if (flat_flag) then
            ofmt = 'flat'
         else if (packed_flag) then
            ofmt = 'packed'
         end if
      end if

      if (cmd /= 'encode' .and. cmd /= 'decode' .and. cmd /= 'convert' .and. cmd /= 'eval') then
         call fail('unknown command '//cmd, rc)
         return
      end if

      if (cmd == 'convert') then
         colon = index(spec, ':')
         if (colon <= 1 .or. colon >= len(spec)) then
            call fail('convert expects <from>:<to>', rc)
            return
         end if
         from_fmt = spec(1:colon - 1)
         to_fmt = spec(colon + 1:)
         if (.not. valid_fmt(from_fmt)) then
            call fail('unknown input format '//from_fmt, rc)
            return
         end if
         if (.not. valid_fmt(to_fmt)) then
            call fail('unknown output format '//to_fmt, rc)
            return
         end if
         if (needs_schema(from_fmt) .or. needs_schema(to_fmt)) then
            if (len(schema_path) == 0 .or. len(name) == 0) then
               call print_usage(2, err)
               rc = 1
               return
            end if
            call load_schema(schema_path, inc_flags, no_std_inc, schema, err)
            if (err /= CAPNP_OK) then
               call fail('cannot load schema', rc)
               return
            end if
            type_idx = capnp_dyn_find(schema, name, err)
            if (type_idx <= 0) then
               call capnp_dyn_free(schema)
               call fail('unknown type '//name, rc)
               return
            end if
         else
            type_idx = 0
         end if
         call cmd_convert(schema, type_idx, from_fmt, to_fmt, short, seg_words, rc)
         call capnp_dyn_free(schema)
         return
      end if

      if (len(schema_path) == 0 .or. len(name) == 0) then
         call print_usage(2, err)
         rc = 1
         return
      end if
      call load_schema(schema_path, inc_flags, no_std_inc, schema, err)
      if (err /= CAPNP_OK) then
         call fail('cannot load schema', rc)
         return
      end if

      select case (cmd)
      case ('encode')
         type_idx = capnp_dyn_find(schema, name, err)
         if (type_idx <= 0) then
            call capnp_dyn_free(schema)
            call fail('unknown type '//name, rc)
            return
         end if
         if (flat_flag .and. packed_flag) then
            call cmd_encode(schema, type_idx, 'flat-packed', seg_words, rc)
         else if (flat_flag) then
            call cmd_encode(schema, type_idx, 'flat', seg_words, rc)
         else if (packed_flag) then
            call cmd_encode(schema, type_idx, 'packed', seg_words, rc)
         else
            call cmd_encode(schema, type_idx, 'binary', seg_words, rc)
         end if
      case ('decode')
         type_idx = capnp_dyn_find(schema, name, err)
         if (type_idx <= 0) then
            call capnp_dyn_free(schema)
            call fail('unknown type '//name, rc)
            return
         end if
         if (flat_flag .and. packed_flag) then
            from_fmt = 'flat-packed'
         else if (flat_flag) then
            from_fmt = 'flat'
         else if (packed_flag) then
            from_fmt = 'packed'
         else
            from_fmt = 'binary'
         end if
         call cmd_decode(schema, type_idx, from_fmt, short, rc)
      case ('eval')
         if (.not. valid_fmt(ofmt)) then
            call capnp_dyn_free(schema)
            call fail('unknown output format '//ofmt, rc)
            return
         end if
         call cmd_eval(schema, name, ofmt, short, seg_words, rc)
      end select
      call capnp_dyn_free(schema)
   end subroutine main_run

   subroutine print_usage(fd, err)
      integer, intent(in) :: fd
      integer, intent(out) :: err
      call write_fd_line(fd, &
         'usage: capnpf encode|decode|convert|eval [options] <schema> <type|name>', err)
      if (err /= CAPNP_OK) return
      call write_fd_line(fd, '  convert <from>:<to> [<schema> <type>]', err)
      if (err /= CAPNP_OK) return
      call write_fd_line(fd, 'formats: binary packed flat flat-packed canonical text json', err)
   end subroutine print_usage

   subroutine fail(msg, rc)
      character(len=*), intent(in) :: msg
      integer, intent(out) :: rc
      integer :: ignored
      call write_fd_line(2, 'capnpf: '//msg, ignored)
      rc = 1
   end subroutine fail

   pure function valid_fmt(fmt) result(y)
      character(len=*), intent(in) :: fmt
      logical :: y
      y = fmt == 'binary' .or. fmt == 'packed' .or. fmt == 'flat' .or. &
          fmt == 'flat-packed' .or. fmt == 'canonical' .or. fmt == 'text' .or. fmt == 'json'
   end function valid_fmt

   pure function needs_schema(fmt) result(y)
      character(len=*), intent(in) :: fmt
      logical :: y
      y = fmt == 'text' .or. fmt == 'json'
   end function needs_schema

   function shell_quote(s) result(q)
      character(len=*), intent(in) :: s
      character(len=:), allocatable :: q
      integer :: i
      q = "'"
      do i = 1, len(s)
         if (s(i:i) == "'") then
            q = q//"'\''"
         else
            q = q//s(i:i)
         end if
      end do
      q = q//"'"
   end function shell_quote

   subroutine load_schema(path, extra_inc, no_std, sch, err)
      character(len=*), intent(in) :: path, extra_inc
      logical, intent(in) :: no_std
      type(capnp_dyn_schema_t), intent(inout), target :: sch
      integer, intent(out) :: err
      character(len=:), allocatable :: shcmd
      integer(int8), allocatable :: bytes(:)
      if (no_std) then
         shcmd = 'capnp compile --no-standard-import'//extra_inc//' -o- ' &
                 //shell_quote(trim(path))
      else
         shcmd = 'INC=$(dirname $(dirname $(command -v capnp)))/include && ' &
                 //'capnp compile -I"$INC"'//extra_inc//' -o- ' &
                 //shell_quote(trim(path))
      end if
      call read_pipe(shcmd, bytes, err)
      if (err /= CAPNP_OK) return
      call capnp_dyn_load(sch, bytes, err)
   end subroutine load_schema

   !> Read the stdout of a shell command. No temporary path is created.
   subroutine read_pipe(shcmd, bytes, err)
      character(len=*), intent(in) :: shcmd
      integer(int8), allocatable, intent(out) :: bytes(:)
      integer, intent(out) :: err
      character(kind=c_char), allocatable, target :: cmd(:), mode(:)
      integer(int8), allocatable, target :: acc(:)
      integer(int8), allocatable :: grown(:)
      type(c_ptr) :: fp
      integer(c_size_t) :: nread
      integer(c_intptr_t) :: addr
      integer :: got, cap, stat, st
      err = CAPNP_OK
      call to_cchars(shcmd, cmd)
      call to_cchars('r', mode)
      fp = c_popen(c_loc(cmd(1)), c_loc(mode(1)))
      if (.not. c_associated(fp)) then
         err = CAPNP_ERR_IO
         return
      end if
      cap = 65536
      got = 0
      allocate (acc(0:cap - 1), stat=stat)
      if (stat /= 0) then
         st = c_pclose(fp)
         err = CAPNP_ERR_ALLOC
         return
      end if
      do
         if (got + 4096 > cap) then
            if (cap > ishft(huge(cap), -1)) then
               st = c_pclose(fp)
               err = CAPNP_ERR_ALLOC
               return
            end if
            allocate (grown(0:cap*2 - 1), stat=stat)
            if (stat /= 0) then
               st = c_pclose(fp)
               err = CAPNP_ERR_ALLOC
               return
            end if
            grown(0:got - 1) = acc(0:got - 1)
            call move_alloc(grown, acc)
            cap = cap*2
         end if
         addr = transfer(c_loc(acc(got)), addr)
         nread = c_fread(transfer(addr, fp), 1_c_size_t, 4096_c_size_t, fp)
         if (nread == 0_c_size_t) exit
         got = got + int(nread)
      end do
      if (c_ferror(fp) /= 0) then
         st = c_pclose(fp)
         err = CAPNP_ERR_IO
         return
      end if
      st = c_pclose(fp)
      if (st /= 0) then
         err = CAPNP_ERR_IO
         return
      end if
      if (got <= 0) then
         allocate (bytes(0))
      else
         allocate (bytes(0:got - 1), stat=stat)
         if (stat /= 0) then
            err = CAPNP_ERR_ALLOC
            return
         end if
         bytes = acc(0:got - 1)
      end if
   end subroutine read_pipe

   subroutine to_cchars(s, a)
      character(len=*), intent(in) :: s
      character(kind=c_char), allocatable, intent(out) :: a(:)
      integer :: i
      allocate (a(len(s) + 1))
      do i = 1, len(s)
         a(i) = s(i:i)
      end do
      a(len(s) + 1) = c_null_char
   end subroutine to_cchars

   subroutine take_arg(i, s)
      integer, intent(in) :: i
      character(len=:), allocatable, intent(out) :: s
      integer :: n
      call get_command_argument(i, length=n)
      if (n < 0) n = 0
      allocate (character(len=n) :: s)
      if (n > 0) call get_command_argument(i, s)
   end subroutine take_arg

   pure function starts_with(s, pfx) result(y)
      character(len=*), intent(in) :: s, pfx
      logical :: y
      y = .false.
      if (len(s) < len(pfx)) return
      y = s(1:len(pfx)) == pfx
   end function starts_with

   subroutine cmd_encode(sch, idx, to_fmt, seg_words, rc)
      type(capnp_dyn_schema_t), intent(in) :: sch
      integer, intent(in) :: idx
      character(len=*), intent(in) :: to_fmt
      integer(int64), intent(in) :: seg_words
      integer, intent(out) :: rc
      integer(int8), allocatable :: raw(:)
      integer :: err
      rc = 0
      call read_stdin(raw, err)
      if (err /= CAPNP_OK) then
         call fail('empty input', rc)
         return
      end if
      call text_to_out(sch, idx, bytes_to_text(raw), to_fmt, .true., seg_words, rc)
   end subroutine cmd_encode

   subroutine cmd_decode(sch, idx, from_fmt, short, rc)
      type(capnp_dyn_schema_t), intent(in) :: sch
      integer, intent(in) :: idx
      character(len=*), intent(in) :: from_fmt
      logical, intent(in) :: short
      integer, intent(out) :: rc
      integer(int8), allocatable :: raw(:)
      integer :: err
      rc = 0
      call read_stdin(raw, err)
      if (err /= CAPNP_OK) then
         call fail('empty input', rc)
         return
      end if
      call bytes_to_out(sch, idx, raw, from_fmt, 'text', .not. short, 0_int64, rc)
   end subroutine cmd_decode

   subroutine cmd_convert(sch, idx, from, to, short, seg_words, rc)
      type(capnp_dyn_schema_t), intent(in) :: sch
      integer, intent(in) :: idx
      character(len=*), intent(in) :: from, to
      logical, intent(in) :: short
      integer(int64), intent(in) :: seg_words
      integer, intent(out) :: rc
      integer(int8), allocatable :: raw(:)
      integer :: err
      rc = 0
      call read_stdin(raw, err)
      if (err /= CAPNP_OK) then
         call fail('empty input', rc)
         return
      end if
      if (from == 'text') then
         call text_to_out(sch, idx, bytes_to_text(raw), to, .not. short, seg_words, rc)
      else if (from == 'json') then
         call json_to_out(sch, idx, bytes_to_text(raw), to, .not. short, seg_words, rc)
      else
         call bytes_to_out(sch, idx, raw, from, to, .not. short, seg_words, rc)
      end if
   end subroutine cmd_convert

   subroutine text_to_out(sch, idx, text, to, pretty, seg_words, rc)
      type(capnp_dyn_schema_t), intent(in) :: sch
      integer, intent(in) :: idx
      character(len=*), intent(in) :: text, to
      logical, intent(in) :: pretty
      integer(int64), intent(in) :: seg_words
      integer, intent(out) :: rc
      type(capnp_message_t), target :: msg
      type(capnp_brand_env_t) :: empty_env
      type(capnp_ptr_t) :: root
      integer :: beg, topos, from, count, err
      rc = 0
      from = 1
      count = 0
      do
         call capnp_text_span(text, from, beg, topos, err)
         if (err /= CAPNP_OK) then
            call fail('bad text', rc)
            return
         end if
         if (beg == 0) exit
         call capnp_text_decode(sch, idx, text(beg:topos), msg, err)
         if (err /= CAPNP_OK) then
            call capnp_message_free(msg)
            call fail('text decode failed', rc)
            return
         end if
         root = capnp_root(msg, err)
         if (err /= CAPNP_OK .or. .not. root_is_struct(root)) then
            call capnp_message_free(msg)
            call fail('bad root', rc)
            return
         end if
         call emit_one(sch, idx, empty_env, msg, root, to, pretty, seg_words, rc)
         call capnp_message_free(msg)
         if (rc /= 0) return
         count = count + 1
         from = topos + 1
      end do
      if (count == 0) call fail('empty input', rc)
   end subroutine text_to_out

   subroutine json_to_out(sch, idx, text, to, pretty, seg_words, rc)
      type(capnp_dyn_schema_t), intent(in) :: sch
      integer, intent(in) :: idx
      character(len=*), intent(in) :: text, to
      logical, intent(in) :: pretty
      integer(int64), intent(in) :: seg_words
      integer, intent(out) :: rc
      type(capnp_message_t), target :: msg
      type(capnp_brand_env_t) :: empty_env
      type(capnp_ptr_t) :: root
      integer :: beg, topos, from, count, err
      logical :: soft
      rc = 0
      from = 1
      count = 0
      soft = .false.
      do
         call capnp_json_span(text, from, beg, topos, err)
         if (err /= CAPNP_OK) then
            call fail('bad json', rc)
            return
         end if
         if (beg == 0) exit
         call capnp_json_decode(sch, idx, text(beg:topos), msg, err)
         if (err /= CAPNP_OK) then
            if (capnp_json_number_failed()) then
               root = capnp_root(msg, err)
               if (err == CAPNP_OK .and. root_is_struct(root)) then
                  call emit_one(sch, idx, empty_env, msg, root, to, pretty, seg_words, rc)
                  if (rc /= 0) then
                     call capnp_message_free(msg)
                     return
                  end if
               end if
               call capnp_message_free(msg)
               soft = .true.
               count = count + 1
               from = topos + 1
               cycle
            end if
            call capnp_message_free(msg)
            call fail('json decode failed', rc)
            return
         end if
         root = capnp_root(msg, err)
         if (err /= CAPNP_OK .or. .not. root_is_struct(root)) then
            call capnp_message_free(msg)
            call fail('bad root', rc)
            return
         end if
         call emit_one(sch, idx, empty_env, msg, root, to, pretty, seg_words, rc)
         call capnp_message_free(msg)
         if (rc /= 0) return
         count = count + 1
         from = topos + 1
      end do
      if (soft) then
         call fail('json decode failed', rc)
         return
      end if
      if (count == 0) call fail('empty input', rc)
   end subroutine json_to_out

   subroutine bytes_to_out(sch, idx, raw, from, to, pretty, seg_words, rc)
      type(capnp_dyn_schema_t), intent(in) :: sch
      integer, intent(in) :: idx
      integer(int8), intent(in) :: raw(0:)
      character(len=*), intent(in) :: from, to
      logical, intent(in) :: pretty
      integer(int64), intent(in) :: seg_words
      integer, intent(out) :: rc
      type(capnp_message_t), target :: msg
      type(capnp_brand_env_t) :: empty_env
      integer(int8), allocatable :: one(:), wrapped(:), unpacked(:)
      integer(int64) :: off, n, total
      integer :: err, count
      type(capnp_ptr_t) :: root
      rc = 0
      if (from == 'flat-packed') then
         call capnp_unpack(raw, unpacked, err)
         if (err /= CAPNP_OK) then
            call fail('bad flat-packed input', rc)
            return
         end if
         if (size(unpacked, kind=int64) == 0_int64 .or. &
             mod(size(unpacked, kind=int64), 8_int64) /= 0_int64) then
            call fail('bad flat-packed input', rc)
            return
         end if
         wrapped = wrap_flat(unpacked)
         call capnp_deserialize_bytes(wrapped, msg, err, options=tool_limits())
         if (err /= CAPNP_OK) then
            call capnp_message_free(msg)
            call fail('bad flat-packed message', rc)
            return
         end if
         root = capnp_root(msg, err)
         if (err /= CAPNP_OK) then
            call capnp_message_free(msg)
            call fail('bad root', rc)
            return
         end if
         call emit_one(sch, idx, empty_env, msg, root, to, pretty, seg_words, rc)
         call capnp_message_free(msg)
         return
      end if
      if (from == 'flat' .or. from == 'canonical') then
         if (size(raw, kind=int64) == 0_int64 .or. mod(size(raw, kind=int64), 8_int64) /= 0_int64) then
            call fail('bad flat input', rc)
            return
         end if
         wrapped = wrap_flat(raw)
         call capnp_deserialize_bytes(wrapped, msg, err, options=tool_limits())
         if (err /= CAPNP_OK) then
            call capnp_message_free(msg)
            call fail('bad flat input', rc)
            return
         end if
         root = capnp_root(msg, err)
         if (err /= CAPNP_OK) then
            call capnp_message_free(msg)
            call fail('bad root', rc)
            return
         end if
         call emit_one(sch, idx, empty_env, msg, root, to, pretty, seg_words, rc)
         call capnp_message_free(msg)
         return
      end if
      if (from == 'packed') then
         off = 0_int64
         total = size(raw, kind=int64)
         count = 0
         do while (off < total)
            call unpack_next_message(raw, off, one, err)
            if (err /= CAPNP_OK) then
               call fail('bad packed input', rc)
               return
            end if
            call capnp_deserialize_bytes(one, msg, err, options=tool_limits())
            deallocate (one)
            if (err /= CAPNP_OK) then
               call capnp_message_free(msg)
               call fail('bad packed message', rc)
               return
            end if
            root = capnp_root(msg, err)
            if (err /= CAPNP_OK .or. .not. root_is_struct(root)) then
               call capnp_message_free(msg)
               call fail('bad root', rc)
               return
            end if
            call emit_one(sch, idx, empty_env, msg, root, to, pretty, seg_words, rc)
            call capnp_message_free(msg)
            if (rc /= 0) return
            count = count + 1
         end do
         if (count == 0) call fail('empty input', rc)
         return
      end if
      off = 0_int64
      total = size(raw, kind=int64)
      count = 0
      do while (off < total)
         n = frame_nbytes(raw, off, err)
         if (err /= CAPNP_OK) then
            call fail('bad framing', rc)
            return
         end if
         allocate (one(0:n - 1))
         one = raw(off:off + n - 1)
         call capnp_deserialize_bytes(one, msg, err, options=tool_limits())
         deallocate (one)
         if (err /= CAPNP_OK) then
            call capnp_message_free(msg)
            call fail('bad message', rc)
            return
         end if
         root = capnp_root(msg, err)
         if (err /= CAPNP_OK .or. .not. root_is_struct(root)) then
            call capnp_message_free(msg)
            call fail('bad root', rc)
            return
         end if
         call emit_one(sch, idx, empty_env, msg, root, to, pretty, seg_words, rc)
         call capnp_message_free(msg)
         if (rc /= 0) return
         count = count + 1
         off = off + n
      end do
      if (count == 0) call fail('empty input', rc)
   end subroutine bytes_to_out

   pure function root_is_struct(root) result(ok)
      type(capnp_ptr_t), intent(in) :: root
      logical :: ok
      ok = root%kind == CAPNP_PK_NULL .or. root%kind == CAPNP_PK_STRUCT
   end function root_is_struct

   subroutine emit_one(sch, idx, env, msg, root, to, pretty, seg_words, rc)
      type(capnp_dyn_schema_t), intent(in) :: sch
      integer, intent(in) :: idx
      type(capnp_brand_env_t), intent(in) :: env
      type(capnp_message_t), intent(inout), target :: msg
      type(capnp_ptr_t), intent(in) :: root
      character(len=*), intent(in) :: to
      logical, intent(in) :: pretty
      integer(int64), intent(in) :: seg_words
      integer, intent(out) :: rc
      character(len=:), allocatable :: text
      integer(int8), allocatable :: outb(:)
      integer :: err
      rc = 0
      if (to == 'text') then
         call capnp_text_encode(sch, idx, root, text, err, pretty=pretty, brand_env=env, &
                                traversal_words=huge(0_int64))
         if (err == CAPNP_ERR_KIND .and. len(text) > 0) then
            call write_fd_line(1, text, err)
            if (err /= CAPNP_OK) then
               call fail('cannot write stdout', rc)
               return
            end if
            call fail('text encode failed', rc)
            return
         end if
         if (err /= CAPNP_OK) then
            call fail('text encode failed', rc)
            return
         end if
         call write_fd_line(1, text, err)
         if (err /= CAPNP_OK) then
            call fail('cannot write stdout', rc)
            return
         end if
      else if (to == 'json') then
         call capnp_json_encode(sch, idx, root, text, err, pretty=pretty, brand_env=env, &
                                traversal_words=huge(0_int64))
         if (err == CAPNP_ERR_KIND .and. len(text) > 0) then
            call write_fd_line(1, text, err)
            if (err /= CAPNP_OK) then
               call fail('cannot write stdout', rc)
               return
            end if
            call fail('json encode failed', rc)
            return
         end if
         if (err /= CAPNP_OK) then
            call fail('json encode failed', rc)
            return
         end if
         call write_fd_line(1, text, err)
         if (err /= CAPNP_OK) then
            call fail('cannot write stdout', rc)
            return
         end if
      else
         call write_binary_message(msg, to, seg_words, outb, err)
         if (err /= CAPNP_OK) then
            call fail('binary encode failed', rc)
            return
         end if
         call write_stdout(outb, err)
         if (err /= CAPNP_OK) call fail('cannot write stdout', rc)
      end if
   end subroutine emit_one

   !> Append bytes to stdout. One descriptor for every message, so a
   !> redirected file keeps each message in order.
   subroutine write_stdout(buf, err)
      integer(int8), intent(in), target :: buf(:)
      integer, intent(out) :: err
      call write_fd(1, buf, err)
   end subroutine write_stdout

   !> Write raw bytes to a file descriptor, including a short count.
   subroutine write_fd(fd, buf, err)
      integer, intent(in) :: fd
      integer(int8), intent(in), target :: buf(:)
      integer, intent(out) :: err
      integer(c_intptr_t) :: addr
      integer(c_size_t) :: left
      integer(c_long) :: nw
      integer :: lb
      type(c_ptr) :: p
      err = CAPNP_OK
      if (size(buf) <= 0) return
      lb = lbound(buf, 1)
      addr = transfer(c_loc(buf(lb)), addr)
      left = int(size(buf), c_size_t)
      do while (left > 0_c_size_t)
         p = transfer(addr, p)
         nw = c_write(int(fd, c_int), p, left)
         if (nw < 0_c_long) then
            ! A signal during write is retried, as kj does.
            if (os_errno_value() == os_eintr()) cycle
            err = CAPNP_ERR_IO
            return
         else if (nw == 0_c_long) then
            err = CAPNP_ERR_IO
            return
         end if
         addr = addr + int(nw, c_intptr_t)
         left = left - int(nw, c_size_t)
      end do
   end subroutine write_fd

   !> Write one text line and a newline. A full stdout is an I/O error.
   subroutine write_fd_line(fd, text, err)
      integer, intent(in) :: fd
      character(len=*), intent(in) :: text
      integer, intent(out) :: err
      integer(int8), allocatable, target :: buf(:)
      integer :: i, n, u, stat
      n = len(text)
      allocate (buf(n + 1), stat=stat)
      if (stat /= 0) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      do i = 1, n
         u = iachar(text(i:i))
         if (u >= 128) then
            buf(i) = int(u - 256, int8)
         else
            buf(i) = int(u, int8)
         end if
      end do
      buf(n + 1) = 10_int8
      call write_fd(fd, buf, err)
   end subroutine write_fd_line

   subroutine write_binary_message(msg, to, seg_words, outb, err)
      type(capnp_message_t), intent(inout), target :: msg
      integer(int8), allocatable, intent(out) :: outb(:)
      character(len=*), intent(in) :: to
      integer(int64), intent(in) :: seg_words
      integer, intent(out) :: err
      type(capnp_message_t), target :: reseg
      type(capnp_ptr_t) :: root, croot
      type(capnp_builder_options_t) :: bopt
      integer(int8), allocatable :: flatb(:)
      err = CAPNP_OK
      if (to == 'canonical') then
         call capnp_canonicalize(msg, outb, err)
      else if (to == 'flat') then
         call to_flat_bytes(msg, outb, err)
      else if (to == 'flat-packed') then
         call to_flat_bytes(msg, flatb, err)
         if (err /= CAPNP_OK) return
         call capnp_pack(flatb, outb, err)
      else if (seg_words > 0_int64 .and. (to == 'binary' .or. to == 'packed')) then
         root = capnp_root(msg, err)
         if (err /= CAPNP_OK) return
         bopt%first_segment_words = seg_words
         bopt%allocation_strategy = CAPNP_ALLOC_FIXED_SIZE
         call capnp_message_init_builder(reseg, err, options=bopt)
         if (err /= CAPNP_OK) return
         croot = capnp_copy(reseg, root, err)
         if (err /= CAPNP_OK) then
            call capnp_message_free(reseg)
            return
         end if
         call capnp_set_root(reseg, croot, err)
         if (err /= CAPNP_OK) then
            call capnp_message_free(reseg)
            return
         end if
         if (to == 'packed') then
            call capnp_serialize_packed_bytes(reseg, outb, err)
         else
            call capnp_serialize_bytes(reseg, outb, err)
         end if
         call capnp_message_free(reseg)
      else if (to == 'binary' .or. to == 'packed') then
         ! Copy the root into a fresh builder. This gives builders and readers
         ! the deterministic object order used by the capnp tool.
         root = capnp_root(msg, err)
         if (err /= CAPNP_OK) return
         call capnp_message_init_builder(reseg, err)
         if (err /= CAPNP_OK) return
         croot = capnp_copy(reseg, root, err)
         if (err /= CAPNP_OK) then
            call capnp_message_free(reseg)
            return
         end if
         call capnp_set_root(reseg, croot, err)
         if (err /= CAPNP_OK) then
            call capnp_message_free(reseg)
            return
         end if
         if (to == 'packed') then
            call capnp_serialize_packed_bytes(reseg, outb, err)
         else
            call capnp_serialize_bytes(reseg, outb, err)
         end if
         call capnp_message_free(reseg)
      else if (to == 'packed') then
         call capnp_serialize_packed_bytes(msg, outb, err)
      else
         call capnp_serialize_bytes(msg, outb, err)
      end if
   end subroutine write_binary_message

   subroutine to_flat_bytes(msg, flatb, err)
      type(capnp_message_t), intent(inout), target :: msg
      integer(int8), allocatable, intent(out) :: flatb(:)
      integer, intent(out) :: err
      type(capnp_message_t), target :: single
      type(capnp_ptr_t) :: root, croot
      integer(int64) :: n, need
      err = CAPNP_OK
      root = capnp_root(msg, err)
      if (err /= CAPNP_OK) return
      need = 1_int64 + capnp_total_size(root, err)
      if (err /= CAPNP_OK) return
      call capnp_message_init_builder(single, err, first_words=max(need, 8_int64))
      if (err /= CAPNP_OK) return
      croot = capnp_copy(single, root, err)
      if (err /= CAPNP_OK) then
         call capnp_message_free(single)
         return
      end if
      call capnp_set_root(single, croot, err)
      if (err /= CAPNP_OK) then
         call capnp_message_free(single)
         return
      end if
      if (single%nsegs /= 1) then
         call capnp_message_free(single)
         err = CAPNP_ERR_BOUNDS
         return
      end if
      n = single%segs(1)%len
      allocate (flatb(0:n - 1))
      flatb = single%segs(1)%bytes(0:n - 1)
      call capnp_message_free(single)
   end subroutine to_flat_bytes

   subroutine unpack_next_message(raw, off, framed, err)
      integer(int8), intent(in) :: raw(0:)
      integer(int64), intent(inout) :: off
      integer(int8), allocatable, intent(out) :: framed(:)
      integer, intent(out) :: err
      integer(int8) :: w8(0:7)
      integer(int8) :: hdr(0:2055)
      integer(int64) :: nsegs64, header_words, total_words, nwords, i, n
      integer(int64) :: rem, room
      integer :: zero_left, raw_left, stat
      err = CAPNP_OK
      zero_left = 0
      raw_left = 0
      call pull_packed_word(raw, off, zero_left, raw_left, w8, err)
      if (err /= CAPNP_OK) return
      ! capnp 1.4.0 reads the first word, the segment table, and the
      ! segments separately. A run may not cross the end of one read.
      if (zero_left /= 0 .or. raw_left /= 0) then
         err = CAPNP_ERR_PACKED
         return
      end if
      nsegs64 = iand(int(cp_get_i32(w8, 0_int64), int64), 4294967295_int64) + 1_int64
      if (nsegs64 < 1_int64 .or. nsegs64 > 512_int64) then
         err = CAPNP_ERR_FRAMING
         return
      end if
      header_words = ((1_int64 + nsegs64)*4_int64 + 7_int64)/8_int64
      hdr(0:7) = w8
      do i = 2_int64, header_words
         call pull_packed_word(raw, off, zero_left, raw_left, w8, err)
         if (err /= CAPNP_OK) return
         hdr((i - 1_int64)*8_int64:(i - 1_int64)*8_int64 + 7_int64) = w8
      end do
      if (zero_left /= 0 .or. raw_left /= 0) then
         err = CAPNP_ERR_PACKED
         return
      end if
      total_words = 0_int64
      do i = 1_int64, nsegs64
         n = iand(int(cp_get_i32(hdr, i*4_int64), int64), 4294967295_int64)
         call capnp_add_segment_words(total_words, n, err)
         if (err /= CAPNP_OK) return
      end do
      rem = size(raw, kind=int64) - off
      call capnp_packed_room(rem, zero_left, raw_left, room, err)
      if (err /= CAPNP_OK) return
      if (total_words > room) then
         err = CAPNP_ERR_FRAMING
         return
      end if
      nwords = header_words + total_words
      allocate (framed(0:nwords*8_int64 - 1), stat=stat)
      if (stat /= 0) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      framed = 0_int8
      framed(0:header_words*8_int64 - 1) = hdr(0:header_words*8_int64 - 1)
      do i = header_words + 1_int64, nwords
         call pull_packed_word(raw, off, zero_left, raw_left, w8, err)
         if (err /= CAPNP_OK) return
         framed((i - 1_int64)*8_int64:(i - 1_int64)*8_int64 + 7_int64) = w8
      end do
      if (zero_left /= 0 .or. raw_left /= 0) err = CAPNP_ERR_PACKED
   end subroutine unpack_next_message

   subroutine pull_packed_word(raw, off, zero_left, raw_left, w8, err)
      integer(int8), intent(in) :: raw(0:)
      integer(int64), intent(inout) :: off
      integer, intent(inout) :: zero_left, raw_left
      integer(int8), intent(out) :: w8(0:7)
      integer, intent(out) :: err
      integer(int64) :: total
      integer :: tag, i
      err = CAPNP_OK
      w8 = 0_int8
      total = size(raw, kind=int64)
      if (zero_left > 0) then
         zero_left = zero_left - 1
         return
      end if
      if (raw_left > 0) then
         if (off + 8_int64 > total) then
            err = CAPNP_ERR_PACKED
            return
         end if
         w8 = raw(off:off + 7_int64)
         off = off + 8_int64
         raw_left = raw_left - 1
         return
      end if
      if (off >= total) then
         err = CAPNP_ERR_PACKED
         return
      end if
      tag = capnp_u_of_i8(raw(off))
      off = off + 1_int64
      if (tag == 0) then
         if (off >= total) then
            err = CAPNP_ERR_PACKED
            return
         end if
         zero_left = capnp_u_of_i8(raw(off))
         off = off + 1_int64
      else if (tag == 255) then
         if (off + 9_int64 > total) then
            err = CAPNP_ERR_PACKED
            return
         end if
         w8 = raw(off:off + 7_int64)
         raw_left = capnp_u_of_i8(raw(off + 8_int64))
         off = off + 9_int64
      else
         do i = 0, 7
            if (btest(tag, i)) then
               if (off >= total) then
                  err = CAPNP_ERR_PACKED
                  return
               end if
               w8(i) = raw(off)
               off = off + 1_int64
            end if
         end do
      end if
   end subroutine pull_packed_word

   ! --- eval ------------------------------------------------------------

   subroutine cmd_eval(sch, spec, fmt, short, seg_words, rc)
      type(capnp_dyn_schema_t), intent(in) :: sch
      character(len=*), intent(in) :: spec, fmt
      logical, intent(in) :: short
      integer(int64), intent(in) :: seg_words
      integer, intent(out) :: rc
      integer :: idx, best, i, err, nlen, pos, sidx
      character(len=:), allocatable :: trial, rest, text
      type(capnp_ptr_t) :: typ, val, obj, et
      type(capnp_brand_env_t) :: empty_env, cenv
      rc = 0
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
      if (idx <= 0) then
         call fail('unknown name '//spec, rc)
         return
      end if
      if (best < nlen) then
         rest = spec(best + 1:)
      else
         rest = ''
      end if

      pos = 1
      if (node_which(sch%nodes(idx)) == NODE_CONST) then
         typ = node_const_type(sch%nodes(idx), err)
         if (err /= CAPNP_OK) then
            call fail('eval failed', rc)
            return
         end if
         val = node_const_value(sch%nodes(idx), err)
         if (err /= CAPNP_OK) then
            call fail('eval failed', rc)
            return
         end if
         if (len(rest) == 0) then
            if (type_which(typ) == TYPE_STRUCT) then
               sidx = capnp_codec_find_id(sch, type_type_id(typ))
               if (sidx == 0) then
                  call fail('eval failed', rc)
                  return
               end if
               call capnp_brand_enter(empty_env, typ, cenv, err)
               if (err /= CAPNP_OK) then
                  call fail('eval failed', rc)
                  return
               end if
               obj = capnp_ptr_t()
               if (val%kind /= CAPNP_PK_NULL) then
                  obj = value_pointer(val, err)
                  if (err /= CAPNP_OK) then
                     call fail('eval failed', rc)
                     return
                  end if
               end if
               call emit_eval_struct(sch, sidx, obj, cenv, fmt, .not. short, seg_words, rc)
            else if (fmt /= 'text') then
               call fail(spec//': not a struct; binary/json output is only available on structs', rc)
            else
               call capnp_text_encode_value(sch, typ, val, text, err, pretty=.not. short)
               if (err /= CAPNP_OK) then
                  call fail('eval failed', rc)
                  return
               end if
               call write_fd_line(1, text, err)
               if (err /= CAPNP_OK) return
            end if
            return
         end if
         if (type_which(typ) == TYPE_STRUCT) then
            sidx = capnp_codec_find_id(sch, type_type_id(typ))
            if (sidx == 0) then
               call fail('eval failed', rc)
               return
            end if
            call capnp_brand_enter(empty_env, typ, cenv, err)
            if (err /= CAPNP_OK) then
               call fail('eval failed', rc)
               return
            end if
            obj = capnp_ptr_t()
            if (val%kind /= CAPNP_PK_NULL) then
               obj = value_pointer(val, err)
               if (err /= CAPNP_OK) then
                  call fail('eval failed', rc)
                  return
               end if
            end if
            call walk_struct_eval(sch, sidx, obj, cenv, rest, pos, fmt, .not. short, seg_words, rc)
            return
         else if (type_which(typ) == TYPE_LIST) then
            et = type_list_element(typ, err)
            if (err /= CAPNP_OK) then
               call fail('eval failed', rc)
               return
            end if
            obj = capnp_ptr_t()
            if (val%kind /= CAPNP_PK_NULL) then
               obj = value_pointer(val, err)
               if (err /= CAPNP_OK) then
                  call fail('eval failed', rc)
                  return
               end if
            end if
            call walk_list_eval(sch, et, obj, empty_env, rest, pos, fmt, .not. short, seg_words, rc)
            return
         else
            call fail('cannot index into scalar constant '//spec, rc)
            return
         end if
      end if

      if (idx < 1 .or. idx > size(sch%nodes)) then
         call fail('eval requires a constant or struct field', rc)
         return
      end if
      if (node_which(sch%nodes(idx)) /= NODE_STRUCT) then
         call fail('eval requires a constant or struct field', rc)
         return
      end if
      if (len(rest) < 2) then
         call fail('eval requires a constant or struct field', rc)
         return
      end if
      if (rest(1:1) /= '.') then
         call fail('eval requires a constant or struct field', rc)
         return
      end if
      call walk_struct_eval(sch, idx, capnp_ptr_t(), empty_env, rest, pos, fmt, .not. short, seg_words, rc)
   end subroutine cmd_eval

   recursive subroutine walk_struct_eval(sch, idx, obj, env, rest, pos, fmt, pretty, seg_words, rc)
      type(capnp_dyn_schema_t), intent(in) :: sch
      integer, intent(in) :: idx
      type(capnp_ptr_t), intent(in) :: obj
      type(capnp_brand_env_t), intent(in) :: env
      character(len=*), intent(in) :: rest, fmt
      integer, intent(inout) :: pos
      logical, intent(in) :: pretty
      integer(int64), intent(in) :: seg_words
      integer, intent(out) :: rc
      character(len=:), allocatable :: fname, text
      type(capnp_ptr_t) :: f, raw_t, t, dv, q, et
      type(capnp_brand_env_t) :: cenv
      integer :: err, gidx, sidx, tw
      integer(int64) :: off
      rc = 0
      if (pos > len(rest)) then
         call emit_eval_struct(sch, idx, obj, env, fmt, pretty, seg_words, rc)
         return
      end if
      call step_field(rest, pos, fname, err)
      if (err /= CAPNP_OK) then
         call fail('bad eval field path', rc)
         return
      end if
      call capnp_codec_find_field(sch, idx, fname, f, err)
      if (err /= CAPNP_OK) then
         call fail('unknown field '//fname, rc)
         return
      end if
      if (field_which(f) == FIELD_GROUP) then
         gidx = capnp_codec_find_id(sch, field_group_type_id(f))
         if (gidx == 0) then
            call fail('eval failed', rc)
            return
         end if
         call walk_struct_eval(sch, gidx, obj, env, rest, pos, fmt, pretty, seg_words, rc)
         return
      end if
      raw_t = field_slot_type(f, err)
      if (err /= CAPNP_OK) then
         call fail('eval failed', rc)
         return
      end if
      t = capnp_brand_resolve(env, raw_t)
      dv = field_slot_default(f, err)
      if (err /= CAPNP_OK) then
         call fail('eval failed', rc)
         return
      end if
      off = field_slot_offset(f)
      tw = type_which(t)
      if (tw == TYPE_STRUCT) then
         q = capnp_ptr_t()
         if (obj%kind /= CAPNP_PK_NULL) then
            q = capnp_getp(obj, int(off), err)
            if (err /= CAPNP_OK) then
               call fail('eval failed', rc)
               return
            end if
         end if
         if (q%kind == CAPNP_PK_NULL .and. dv%kind /= CAPNP_PK_NULL) then
            q = value_pointer(dv, err)
            if (err /= CAPNP_OK) then
               call fail('eval failed', rc)
               return
            end if
         end if
         sidx = capnp_codec_find_id(sch, type_type_id(t))
         if (sidx == 0) then
            call fail('eval failed', rc)
            return
         end if
         call capnp_brand_enter(env, t, cenv, err)
         if (err /= CAPNP_OK) then
            call fail('eval failed', rc)
            return
         end if
         call walk_struct_eval(sch, sidx, q, cenv, rest, pos, fmt, pretty, seg_words, rc)
      else if (tw == TYPE_LIST) then
         q = capnp_ptr_t()
         if (obj%kind /= CAPNP_PK_NULL) then
            q = capnp_getp(obj, int(off), err)
            if (err /= CAPNP_OK) then
               call fail('eval failed', rc)
               return
            end if
         end if
         if (q%kind == CAPNP_PK_NULL .and. dv%kind /= CAPNP_PK_NULL) then
            q = value_pointer(dv, err)
            if (err /= CAPNP_OK) then
               call fail('eval failed', rc)
               return
            end if
         end if
         et = type_list_element(t, err)
         if (err /= CAPNP_OK) then
            call fail('eval failed', rc)
            return
         end if
         call walk_list_eval(sch, et, q, env, rest, pos, fmt, pretty, seg_words, rc)
      else
         if (pos <= len(rest)) then
            call fail('cannot index into non-aggregate field '//fname, rc)
            return
         end if
         if (fmt /= 'text') then
            call fail('not a struct; binary/json output is only available on structs', rc)
            return
         end if
         call format_slot_scalar(sch, obj, t, dv, off, text, err)
         if (err /= CAPNP_OK) then
            call fail('eval failed', rc)
            return
         end if
         call write_fd_line(1, text, err)
         if (err /= CAPNP_OK) return
      end if
   end subroutine walk_struct_eval

   recursive subroutine walk_list_eval(sch, raw_et, lst, env, rest, pos, fmt, pretty, seg_words, rc)
      type(capnp_dyn_schema_t), intent(in) :: sch
      type(capnp_ptr_t), intent(in) :: raw_et, lst
      type(capnp_brand_env_t), intent(in) :: env
      character(len=*), intent(in) :: rest, fmt
      integer, intent(inout) :: pos
      logical, intent(in) :: pretty
      integer(int64), intent(in) :: seg_words
      integer, intent(out) :: rc
      type(capnp_ptr_t) :: et, el, inner_lst, inner_et
      type(capnp_brand_env_t) :: cenv
      character(len=:), allocatable :: text
      integer(int64) :: ix
      integer :: err, sidx, tw
      rc = 0
      et = capnp_brand_resolve(env, raw_et)
      if (pos > len(rest)) then
         if (fmt /= 'text') then
            call fail('not a struct; binary/json output is only available on structs', rc)
            return
         end if
         call capnp_text_encode_list(sch, et, lst, env, text, err, pretty=pretty)
         if (err /= CAPNP_OK) then
            call fail('eval failed', rc)
            return
         end if
         call write_fd_line(1, text, err)
         if (err /= CAPNP_OK) return
         return
      end if
      call step_index(rest, pos, ix, err)
      if (err /= CAPNP_OK) then
         call fail('bad eval list index', rc)
         return
      end if
      if (lst%kind /= CAPNP_PK_LIST .or. ix < 0_int64 .or. ix >= capnp_list_len(lst)) then
         call fail('list index out of bounds', rc)
         return
      end if
      tw = type_which(et)
      if (tw == TYPE_STRUCT) then
         el = capnp_list_get_struct(lst, int(ix), err)
         if (err /= CAPNP_OK) then
            call fail('eval failed', rc)
            return
         end if
         sidx = capnp_codec_find_id(sch, type_type_id(et))
         if (sidx == 0) then
            call fail('eval failed', rc)
            return
         end if
         call capnp_brand_enter(env, et, cenv, err)
         if (err /= CAPNP_OK) then
            call fail('eval failed', rc)
            return
         end if
         call walk_struct_eval(sch, sidx, el, cenv, rest, pos, fmt, pretty, seg_words, rc)
      else if (tw == TYPE_LIST) then
         inner_lst = capnp_list_getp(lst, int(ix), err)
         if (err /= CAPNP_OK) then
            call fail('eval failed', rc)
            return
         end if
         inner_et = type_list_element(et, err)
         if (err /= CAPNP_OK) then
            call fail('eval failed', rc)
            return
         end if
         call walk_list_eval(sch, inner_et, inner_lst, env, rest, pos, fmt, pretty, seg_words, rc)
      else
         if (pos <= len(rest)) then
            call fail('cannot index into scalar list element', rc)
            return
         end if
         if (fmt /= 'text') then
            call fail('not a struct; binary/json output is only available on structs', rc)
            return
         end if
         call format_elem_scalar(sch, lst, et, ix, text, err)
         if (err /= CAPNP_OK) then
            call fail('eval failed', rc)
            return
         end if
         call write_fd_line(1, text, err)
         if (err /= CAPNP_OK) return
      end if
   end subroutine walk_list_eval

   subroutine emit_eval_struct(sch, idx, obj, env, fmt, pretty, seg_words, rc)
      type(capnp_dyn_schema_t), intent(in) :: sch
      integer, intent(in) :: idx
      type(capnp_ptr_t), intent(in) :: obj
      type(capnp_brand_env_t), intent(in) :: env
      character(len=*), intent(in) :: fmt
      logical, intent(in) :: pretty
      integer(int64), intent(in) :: seg_words
      integer, intent(out) :: rc
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root
      character(len=:), allocatable :: text
      integer(int64) :: need
      integer :: err
      rc = 0
      if (fmt == 'text') then
         call capnp_text_encode(sch, idx, obj, text, err, pretty=pretty, brand_env=env, &
                                traversal_words=huge(0_int64))
         if (err /= CAPNP_OK) then
            call fail('text encode failed', rc)
            return
         end if
         call write_fd_line(1, text, err)
         if (err /= CAPNP_OK) return
         return
      end if
      if (fmt == 'json') then
         call capnp_json_encode(sch, idx, obj, text, err, pretty=pretty, brand_env=env, &
                                traversal_words=huge(0_int64))
         if (err /= CAPNP_OK) then
            call fail('json encode failed', rc)
            return
         end if
         call write_fd_line(1, text, err)
         if (err /= CAPNP_OK) then
            call fail('cannot write stdout', rc)
            return
         end if
         return
      end if
      if (node_struct_is_group(sch%nodes(idx))) then
         call fail('binary output is not available on groups', rc)
         return
      end if
      if (obj%kind == CAPNP_PK_NULL) then
         call capnp_message_init_builder(msg, err)
         if (err /= CAPNP_OK) then
            call fail('alloc failed', rc)
            return
         end if
         root = capnp_dyn_new_root(sch, idx, msg, err)
         if (err /= CAPNP_OK) then
            call capnp_message_free(msg)
            call fail('alloc failed', rc)
            return
         end if
      else
         need = 1_int64 + capnp_total_size(obj, err)
         if (err /= CAPNP_OK) then
            call fail('eval failed', rc)
            return
         end if
         call capnp_message_init_builder(msg, err, first_words=max(need, 8_int64))
         if (err /= CAPNP_OK) then
            call fail('alloc failed', rc)
            return
         end if
         root = capnp_copy(msg, obj, err)
         if (err /= CAPNP_OK) then
            call capnp_message_free(msg)
            call fail('copy failed', rc)
            return
         end if
         call capnp_set_root(msg, root, err)
         if (err /= CAPNP_OK) then
            call capnp_message_free(msg)
            call fail('set_root failed', rc)
            return
         end if
      end if
      call emit_one(sch, idx, env, msg, root, fmt, pretty, seg_words, rc)
      call capnp_message_free(msg)
   end subroutine emit_eval_struct

   subroutine step_field(rest, pos, fname, err)
      character(len=*), intent(in) :: rest
      integer, intent(inout) :: pos
      character(len=:), allocatable, intent(out) :: fname
      integer, intent(out) :: err
      integer :: a, n
      character(len=1) :: c
      err = CAPNP_OK
      fname = ''
      n = len(rest)
      if (pos > n) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (rest(pos:pos) /= '.') then
         err = CAPNP_ERR_ARG
         return
      end if
      pos = pos + 1
      if (pos > n) then
         err = CAPNP_ERR_ARG
         return
      end if
      c = rest(pos:pos)
      if (.not. ((c >= 'A' .and. c <= 'Z') .or. (c >= 'a' .and. c <= 'z') .or. c == '_')) then
         err = CAPNP_ERR_ARG
         return
      end if
      a = pos
      pos = pos + 1
      do while (pos <= n)
         c = rest(pos:pos)
         if ((c >= 'A' .and. c <= 'Z') .or. (c >= 'a' .and. c <= 'z') .or. &
             (c >= '0' .and. c <= '9') .or. c == '_') then
            pos = pos + 1
         else if (c == '.' .or. c == '[') then
            exit
         else
            err = CAPNP_ERR_ARG
            return
         end if
      end do
      fname = rest(a:pos - 1)
   end subroutine step_field

   subroutine step_index(rest, pos, ix, err)
      character(len=*), intent(in) :: rest
      integer, intent(inout) :: pos
      integer(int64), intent(out) :: ix
      integer, intent(out) :: err
      integer :: close_at, n
      err = CAPNP_OK
      ix = -1_int64
      n = len(rest)
      if (pos > n) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (rest(pos:pos) /= '[') then
         err = CAPNP_ERR_ARG
         return
      end if
      close_at = index(rest(pos + 1:), ']')
      if (close_at <= 1) then
         err = CAPNP_ERR_ARG
         return
      end if
      close_at = pos + close_at
      call capnp_parse_i64(rest(pos + 1:close_at - 1), ix, err)
      if (err /= CAPNP_OK .or. ix < 0_int64) then
         err = CAPNP_ERR_ARG
         return
      end if
      pos = close_at + 1
   end subroutine step_index

   subroutine format_slot_scalar(sch, p, t, dv, off, text, err)
      type(capnp_dyn_schema_t), intent(in) :: sch
      type(capnp_ptr_t), intent(in) :: p, t, dv
      integer(int64), intent(in) :: off
      character(len=:), allocatable, intent(out) :: text
      integer, intent(out) :: err
      type(capnp_ptr_t) :: q
      character(len=:), allocatable :: s, esc
      integer(int8), allocatable :: raw(:)
      integer :: tw, ord
      err = CAPNP_OK
      tw = type_which(t)
      select case (tw)
      case (TYPE_VOID)
         text = 'void'
      case (TYPE_BOOL)
         if (capnp_get_bool(p, off, default=capnp_def_bool(dv))) then
            text = 'true'
         else
            text = 'false'
         end if
      case (TYPE_INT8)
         call capnp_fmt_i64(int(capnp_get_i8(p, off, default=capnp_def_i8(dv)), int64), text)
      case (TYPE_INT16)
         call capnp_fmt_i64(int(capnp_get_i16(p, off*2_int64, default=capnp_def_i16(dv)), int64), text)
      case (TYPE_INT32)
         call capnp_fmt_i64(int(capnp_get_i32(p, off*4_int64, default=capnp_def_i32(dv)), int64), text)
      case (TYPE_INT64)
         call capnp_fmt_i64(capnp_get_i64(p, off*8_int64, default=capnp_def_i64(dv)), text)
      case (TYPE_UINT8)
         call capnp_fmt_u64(int(capnp_get_u8(p, off, default=capnp_def_u8(dv)), int64), text)
      case (TYPE_UINT16)
         call capnp_fmt_u64(int(capnp_get_u16(p, off*2_int64, default=capnp_def_u16(dv)), int64), text)
      case (TYPE_UINT32)
         call capnp_fmt_u64(capnp_get_u32(p, off*4_int64, default=capnp_def_u32(dv)), text)
      case (TYPE_UINT64)
         call capnp_fmt_u64(capnp_get_i64(p, off*8_int64, default=capnp_def_i64(dv)), text)
      case (TYPE_FLOAT32)
         call capnp_fmt_f32(capnp_get_f32(p, off*4_int64, default=capnp_def_f32(dv)), text)
      case (TYPE_FLOAT64)
         call capnp_fmt_f64(capnp_get_f64(p, off*8_int64, default=capnp_def_f64(dv)), text)
      case (TYPE_ENUM)
         ord = int(capnp_get_u16(p, off*2_int64, default=capnp_def_u16(dv)))
         call capnp_text_encode_enum(sch, type_type_id(t), ord, text, err)
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
         call capnp_cescape_text(s, esc, err)
         if (err /= CAPNP_OK) return
         text = '"'//esc//'"'
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
               call capnp_cescape_data(raw, esc, err)
               if (err /= CAPNP_OK) return
            else
               esc = ''
            end if
         else
            call capnp_get_data(p, int(off), raw, err)
            if (err /= CAPNP_OK) return
            call capnp_cescape_data(raw, esc, err)
            if (err /= CAPNP_OK) return
         end if
         text = '"'//esc//'"'
      case (TYPE_ANY_POINTER)
         text = '<opaque pointer>'
      case (TYPE_INTERFACE)
         text = '<external capability>'
      case default
         err = CAPNP_ERR_KIND
      end select
   end subroutine format_slot_scalar

   subroutine format_elem_scalar(sch, lst, et, ix, text, err)
      type(capnp_dyn_schema_t), intent(in) :: sch
      type(capnp_ptr_t), intent(in) :: lst, et
      integer(int64), intent(in) :: ix
      character(len=:), allocatable, intent(out) :: text
      integer, intent(out) :: err
      character(len=:), allocatable :: s, esc
      integer(int8), allocatable :: raw(:)
      integer :: tw, ord
      err = CAPNP_OK
      tw = type_which(et)
      select case (tw)
      case (TYPE_VOID)
         text = 'void'
      case (TYPE_BOOL)
         if (capnp_list_get_bool(lst, ix, err)) then
            text = 'true'
         else
            text = 'false'
         end if
      case (TYPE_INT8)
         call capnp_fmt_i64(int(capnp_list_get_i8(lst, ix, err), int64), text)
      case (TYPE_INT16)
         call capnp_fmt_i64(int(capnp_list_get_i16(lst, ix, err), int64), text)
      case (TYPE_INT32)
         call capnp_fmt_i64(int(capnp_list_get_i32(lst, ix, err), int64), text)
      case (TYPE_INT64)
         call capnp_fmt_i64(capnp_list_get_i64(lst, ix, err), text)
      case (TYPE_UINT8)
         call capnp_fmt_u64(int(capnp_u_of_i8(capnp_list_get_i8(lst, ix, err)), int64), text)
      case (TYPE_UINT16)
         call capnp_fmt_u64(int(capnp_u_of_i16(capnp_list_get_i16(lst, ix, err)), int64), text)
      case (TYPE_UINT32)
         call capnp_fmt_u64(capnp_u_of_i32(capnp_list_get_i32(lst, ix, err)), text)
      case (TYPE_UINT64)
         call capnp_fmt_u64(capnp_list_get_i64(lst, ix, err), text)
      case (TYPE_FLOAT32)
         call capnp_fmt_f32(capnp_list_get_f32(lst, ix, err), text)
      case (TYPE_FLOAT64)
         call capnp_fmt_f64(capnp_list_get_f64(lst, ix, err), text)
      case (TYPE_ENUM)
         ord = capnp_u_of_i16(capnp_list_get_i16(lst, ix, err))
         if (err /= CAPNP_OK) return
         call capnp_text_encode_enum(sch, type_type_id(et), ord, text, err)
      case (TYPE_TEXT)
         call capnp_list_get_text(lst, int(ix), s, err)
         if (err /= CAPNP_OK) return
         call capnp_cescape_text(s, esc, err)
         if (err /= CAPNP_OK) return
         text = '"'//esc//'"'
      case (TYPE_DATA)
         call capnp_get_data(lst, int(ix), raw, err)
         if (err /= CAPNP_OK) return
         call capnp_cescape_data(raw, esc, err)
         if (err /= CAPNP_OK) return
         text = '"'//esc//'"'
      case (TYPE_ANY_POINTER)
         text = '<opaque pointer>'
      case (TYPE_INTERFACE)
         text = '<external capability>'
      case default
         err = CAPNP_ERR_KIND
      end select
   end subroutine format_elem_scalar

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
      integer :: unit, ios, stat
      err = CAPNP_OK
      open (newunit=unit, file='/dev/stdin', access='stream', form='unformatted', &
            action='read', iostat=ios)
      if (ios /= 0) then
         err = CAPNP_ERR_IO
         return
      end if
      cap = 65536_int64
      allocate (b(0:cap - 1), stat=stat)
      if (stat /= 0) then
         close (unit)
         err = CAPNP_ERR_ALLOC
         return
      end if
      n = 0_int64
      do
         read (unit, iostat=ios) byte
         if (ios /= 0) exit
         if (n == cap) then
            allocate (tmp(0:2*cap - 1), stat=stat)
            if (stat /= 0) then
               close (unit)
               err = CAPNP_ERR_ALLOC
               return
            end if
            tmp(0:cap - 1) = b
            call move_alloc(tmp, b)
            cap = 2*cap
         end if
         b(n) = byte
         n = n + 1
      end do
      close (unit)
      allocate (tmp(0:max(n, 1_int64) - 1), stat=stat)
      if (stat /= 0) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      if (n > 0_int64) tmp(0:n - 1) = b(0:n - 1)
      call move_alloc(tmp, b)
      if (n == 0_int64) err = CAPNP_ERR_IO
   end subroutine read_stdin

end program capnpf
