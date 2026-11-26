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
      function c_strtol(s, endp, base) bind(c, name="strtol") result(v)
         import c_ptr, c_int, c_long
         type(c_ptr), value :: s
         type(c_ptr), intent(out) :: endp
         integer(c_int), value :: base
         integer(c_long) :: v
      end function c_strtol
   end interface

   type :: arg_t
      character(len=:), allocatable :: s
   end type arg_t

   type :: cli_opts_t
      character(len=:), allocatable :: inc_flags
      character(len=:), allocatable :: ofmt
      logical :: short = .false., quiet = .false., flat = .false., packed = .false.
      logical :: binary = .false., no_std = .false.
      integer(int64) :: seg_words = 0_int64
   end type cli_opts_t

   integer :: exit_code
   !> Writes to stdout so far. A failed message that wrote nothing
   !> ends the stream, as a thrown exception does in capnp 1.4.0.
   integer(int64) :: stdout_writes = 0_int64
   integer, parameter :: P_IMPOSSIBLE = 0, P_IMPLAUSIBLE = 1, P_WRONG_TYPE = 2, &
                         P_PLAUSIBLE = 3
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
      character(len=:), allocatable :: cmd, schema_path, name, spec, ofmt, inc_flags
      character(len=:), allocatable :: from_fmt, to_fmt
      logical :: short, packed_flag, flat_flag, no_std_inc, quiet, done
      integer(int64) :: seg_words
      integer :: i, n, err, colon, type_idx
      type(arg_t), allocatable :: args(:), pos(:)
      type(cli_opts_t) :: opts
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_brand_env_t) :: root_env

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
      no_std_inc = .false.
      quiet = .false.
      seg_words = 0_int64
      opts%inc_flags = ''
      opts%ofmt = 'text'

      n = command_argument_count()
      if (n < 1) then
         call print_usage(2, err)
         rc = 1
         return
      end if
      allocate (args(n))
      do i = 1, n
         call take_arg(i, args(i)%s)
      end do

      ! kj::MainBuilder: global options, then the command, then its own
      ! options and arguments in any order until `--`.
      i = 1
      do while (i <= n)
         if (args(i)%s == '--') then
            call fail('missing command', rc)
            return
         else if (is_option(args(i)%s)) then
            call take_option(opts, '', args, i, done, rc)
            if (done) return
         else
            cmd = args(i)%s
            exit
         end if
         i = i + 1
      end do
      if (len(cmd) == 0) then
         call fail('missing command', rc)
         return
      end if
      if (cmd == 'help') then
         if (i + 1 <= n) then
            if (.not. known_command(args(i + 1)%s)) then
               call fail(args(i + 1)%s//': unknown command', rc)
               return
            end if
         end if
         call print_usage(1, err)
         return
      end if
      if (.not. known_command(cmd)) then
         call fail(cmd//': unknown command', rc)
         return
      end if
      allocate (pos(0))
      i = i + 1
      do while (i <= n)
         if (args(i)%s == '--') then
            pos = [pos, args(i + 1:n)]
            exit
         else if (is_option(args(i)%s)) then
            call take_option(opts, cmd, args, i, done, rc)
            if (done) return
         else
            pos = [pos, args(i)]
         end if
         i = i + 1
      end do
      if (size(pos) < 1 .or. (cmd /= 'convert' .and. size(pos) < 2)) then
         call fail('missing argument', rc)
         return
      end if
      if (size(pos) > 3 .or. (cmd /= 'convert' .and. size(pos) > 2)) then
         call fail(pos(size(pos))%s//': too many arguments', rc)
         return
      end if
      if (cmd == 'convert') then
         spec = pos(1)%s
         if (size(pos) >= 2) schema_path = pos(2)%s
         if (size(pos) >= 3) name = pos(3)%s
      else
         schema_path = pos(1)%s
         name = pos(2)%s
      end if
      short = opts%short
      quiet = opts%quiet
      packed_flag = opts%packed
      flat_flag = opts%flat
      no_std_inc = opts%no_std
      inc_flags = opts%inc_flags
      seg_words = opts%seg_words
      ofmt = opts%ofmt
      ! formatFromDeprecatedFlags: --flat, -p and -b win over -o.
      if (flat_flag .and. packed_flag) then
         ofmt = 'flat-packed'
      else if (flat_flag) then
         ofmt = 'flat'
      else if (packed_flag) then
         ofmt = 'packed'
      else if (opts%binary) then
         ofmt = 'binary'
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
         ! capnp compiles a schema and resolves a type whenever they are
         ! given, even for a binary-only conversion.
         type_idx = 0
         if (len(schema_path) > 0) then
            call load_schema(schema_path, inc_flags, no_std_inc, name, &
               from_fmt == 'json' .or. to_fmt == 'json', schema, err)
            if (err /= CAPNP_OK) then
               call fail('cannot load schema', rc)
               return
            end if
         end if
         if (len(name) > 0) then
            call bind_root(schema, name, type_idx, root_env, err)
            if (err /= CAPNP_OK .or. type_idx <= 0) then
               call capnp_dyn_free(schema)
               call fail('unknown type '//name, rc)
               return
            end if
         end if
         if ((needs_schema(from_fmt) .or. needs_schema(to_fmt)) .and. type_idx <= 0) then
            call capnp_dyn_free(schema)
            call print_usage(2, err)
            rc = 1
            return
         end if
         call cmd_convert(schema, type_idx, root_env, from_fmt, to_fmt, short, seg_words, quiet, rc)
         call capnp_dyn_free(schema)
         return
      end if

      call load_schema(schema_path, inc_flags, no_std_inc, name, .false., schema, err)
      if (err /= CAPNP_OK) then
         call fail('cannot load schema', rc)
         return
      end if

      select case (cmd)
      case ('encode')
         call bind_root(schema, name, type_idx, root_env, err)
         if (err /= CAPNP_OK .or. type_idx <= 0) then
            call capnp_dyn_free(schema)
            call fail('unknown type '//name, rc)
            return
         end if
         if (flat_flag .and. packed_flag) then
            call cmd_encode(schema, type_idx, root_env, 'flat-packed', seg_words, rc)
         else if (flat_flag) then
            call cmd_encode(schema, type_idx, root_env, 'flat', seg_words, rc)
         else if (packed_flag) then
            call cmd_encode(schema, type_idx, root_env, 'packed', seg_words, rc)
         else
            call cmd_encode(schema, type_idx, root_env, 'binary', seg_words, rc)
         end if
      case ('decode')
         call bind_root(schema, name, type_idx, root_env, err)
         if (err /= CAPNP_OK .or. type_idx <= 0) then
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
         call cmd_decode(schema, type_idx, root_env, from_fmt, short, quiet, rc)
      case ('eval')
         call cmd_eval(schema, name, ofmt, short, 0_int64, rc)
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

   !> A plain name is a schema node. A generic expression is the field
   !> type of `CapnpfBrandHost`, which schema-request compiled for us.
   subroutine bind_root(sch, name, idx, benv, err)
      type(capnp_dyn_schema_t), intent(in) :: sch
      character(len=*), intent(in) :: name
      integer, intent(out) :: idx
      type(capnp_brand_env_t), intent(out) :: benv
      integer, intent(out) :: err
      type(capnp_brand_env_t) :: empty
      type(capnp_ptr_t) :: fields, fld, typ
      integer :: host, i
      integer(int64) :: tid
      err = CAPNP_OK
      idx = 0
      if (index(name, '(') > 0) then
         host = capnp_dyn_find(sch, 'CapnpfBrandHost', err)
         if (err /= CAPNP_OK .or. host <= 0) then
            err = CAPNP_ERR_ARG
            return
         end if
         fields = node_struct_fields(sch%nodes(host), err)
         if (err /= CAPNP_OK) return
         if (fields%kind /= CAPNP_PK_LIST .or. capnp_list_len(fields) < 1) then
            err = CAPNP_ERR_ARG
            return
         end if
         fld = capnp_list_get_struct(fields, 0, err)
         if (err /= CAPNP_OK) return
         typ = field_slot_type(fld, err)
         if (err /= CAPNP_OK) return
         if (type_which(typ) /= TYPE_STRUCT) then
            err = CAPNP_ERR_KIND
            return
         end if
         tid = type_type_id(typ)
         do i = 1, size(sch%nodes)
            if (node_id(sch%nodes(i)) == tid) then
               idx = i
               exit
            end if
         end do
         if (idx <= 0) then
            err = CAPNP_ERR_ARG
            return
         end if
         call capnp_brand_enter(empty, typ, benv, err)
         return
      end if
      idx = capnp_dyn_find(sch, name, err)
      if (err /= CAPNP_OK .or. idx <= 0) err = CAPNP_ERR_ARG
   end subroutine bind_root

   !> Shell text that sets INC to the standard include directory.
   !> CAPNP_PREFIX/include is used when CAPNP_PREFIX is non-empty and that
   !> directory exists. Otherwise command -v capnp is followed with readlink
   !> and cd -P, and INC is the parent of the resolved bin directory with
   !> /include appended.
   function std_include_sh() result(s)
      character(len=:), allocatable :: s
      s = ' if [ -n "${CAPNP_PREFIX:-}" ] && [ -d "$CAPNP_PREFIX/include" ]; then' &
         //' INC="${CAPNP_PREFIX}/include"; else' &
         //' p=$(command -v capnp) || exit 1;' &
         //' while [ -L "$p" ]; do t=$(readlink "$p") || exit 1;' &
         //' case "$t" in /*) p="$t";; *) p="$(dirname "$p")/$t";; esac; done;' &
         //' d=$(cd -P "$(dirname "$p")" && pwd -P) || exit 1;' &
         //' INC="$(dirname "$d")/include"; fi'
   end function std_include_sh

   subroutine load_schema(path, extra_inc, no_std, type_name, annotations, sch, err)
      character(len=*), intent(in) :: path, extra_inc, type_name
      logical, intent(in) :: no_std, annotations
      type(capnp_dyn_schema_t), intent(inout), target :: sch
      integer, intent(out) :: err
      character(len=:), allocatable :: shcmd, ann, std
      integer(int8), allocatable :: bytes(:)
      ! User -I directories come before the install include, matching
      ! capnp 1.4.0. schema-request compiles only the requested type, so
      ! an error in some other declaration is ignored. The fallback
      ! wrapper does the same for a plain type name.
      if (annotations) then
         ann = 'keep'
      else
         ann = 'drop'
      end if
      if (no_std) then
         std = ' --no-standard-import'
      else
         std = ''
      end if
      ! The helper may sit next to this executable when the working
      ! directory is not the repository. capnp compile is the fallback.
      shcmd = 'REQ=""; if [ -n "${CAPNP_SCHEMA_REQUEST:-}" ] && [ -x "$CAPNP_SCHEMA_REQUEST" ];' &
              //' then REQ=$CAPNP_SCHEMA_REQUEST;' &
              //' elif [ -x ./build/schema-request ]; then REQ=./build/schema-request;' &
              //' else exe=$(readlink -f /proc/$PPID/exe 2>/dev/null || true);' &
              //' if [ -n "$exe" ]; then base=$(dirname "$exe");' &
              //' for cand in "$base/schema-request" "$base/../schema-request"' &
              //' "$base/../../schema-request"; do' &
              //' if [ -x "$cand" ]; then REQ=$cand; break; fi; done; fi;' &
              //' if [ -z "$REQ" ] && command -v capnp-schema-request >/dev/null 2>&1;' &
              //' then REQ=capnp-schema-request; fi; fi;'
      if (.not. no_std) then
         ! The helper is not installed beside the capnp headers, so the
         ! standard import path is the same one `capnp compile` uses.
         ! User -I paths stay in front of it.
         shcmd = shcmd//std_include_sh()//';'
      end if
      shcmd = shcmd//' if test -n "$REQ"; then exec "$REQ" --annotations '//ann//std &
              //extra_inc
      if (.not. no_std) shcmd = shcmd//' -I"$INC"'
      shcmd = shcmd//' '//shell_quote(trim(path))//' '//shell_quote(trim(type_name))//'; fi; '
      if (index(type_name, '(') > 0) then
         ! Without the helper, a wrapper file names the generic root as a
         ! field type. Its names are looked up in the original file.
         if (.not. no_std) shcmd = shcmd//std_include_sh()//'; '
         shcmd = shcmd//'ABS=$(readlink -f '//shell_quote(trim(path))//') || exit 1; ' &
                 //'W=$(mktemp /tmp/capnpf-brand-XXXXXX.capnp) || exit 1; ' &
                 //'printf ''%s\n'' ''@0x8e1e4a11b22c3d4f;'' "using CapnpfOrig = import \"$ABS\";" ' &
                 //'''struct CapnpfBrandHost {'' ' &
                 //shell_quote('  branded @0 :'//brand_expr(type_name)//';') &
                 //' ''}'' > "$W"; capnp compile'
         if (no_std) then
            shcmd = shcmd//' --no-standard-import'//extra_inc
         else
            shcmd = shcmd//extra_inc//' -I"$INC"'
         end if
         shcmd = shcmd//' -I/ -o- "$W" "$ABS"; rc=$?; rm -f "$W"; exit $rc'
      else
         ! A wrapper names only the requested type, so an unused
         ! declaration in the imported file stays uncompiled. A const
         ! or other value name fails that compile, and the whole file
         ! is compiled afterwards.
         shcmd = shcmd//'dir=$(mktemp -d) || exit 1; ' &
                 //'trap ''rm -rf "$dir"'' EXIT INT TERM; ' &
                 //'ABS=$(readlink -f '//shell_quote(trim(path))//') || exit 1; ' &
                 //'W=$dir/capnpf-lazy.capnp; ' &
                 //'O=$dir/capnpf-lazy.bin; E=$dir/capnpf-lazy.err; ' &
                 //'printf ''%s\n'' ''@0x8e1e4a11b22c3d4f;'' ' &
                 //'"using CapnpfOrig = import \"$ABS\";" ' &
                 //'''struct CapnpfBrandHost {'' ' &
                 //shell_quote('  branded @0 :'//brand_expr(type_name)//';') &
                 //' ''}'' > "$W"; if capnp compile'
         if (no_std) then
            shcmd = shcmd//' --no-standard-import'//extra_inc
         else
            shcmd = shcmd//extra_inc//' -I"$INC"'
         end if
         shcmd = shcmd//' -I/ -o- "$W" >"$O" 2>"$E"; then cat "$E" >&2; ' &
                 //'cat "$O"; rc=0; else '
         if (no_std) then
            shcmd = shcmd//'capnp compile --no-standard-import'//extra_inc//' -o- ' &
                    //shell_quote(trim(path))
         else
            shcmd = shcmd//'capnp compile'//extra_inc//' -I"$INC" -o- ' &
                    //shell_quote(trim(path))
         end if
         shcmd = shcmd//'; rc=$?; fi; exit $rc'
      end if
      call read_pipe(shcmd, bytes, err)
      if (err /= CAPNP_OK) return
      call capnp_dyn_load(sch, bytes, err)
      if (err == CAPNP_OK .and. .not. annotations) call drop_annotations(sch)
   end subroutine load_schema

   !> Clear node, field and enumerant annotations, as the capnp tool's
   !> DROP_ANNOTATIONS compile does. `capnp compile` keeps them.
   subroutine drop_annotations(sch)
      type(capnp_dyn_schema_t), intent(inout), target :: sch
      type(capnp_ptr_t) :: lst, el
      integer :: i, j, err
      if (.not. allocated(sch%nodes)) return
      do i = 1, size(sch%nodes)
         call clear_slot(sch%nodes(i), 2)
         select case (node_which(sch%nodes(i)))
         case (NODE_STRUCT)
            lst = node_struct_fields(sch%nodes(i), err)
         case (NODE_ENUM)
            lst = node_enumerants(sch%nodes(i), err)
         case default
            cycle
         end select
         if (err /= CAPNP_OK .or. lst%kind /= CAPNP_PK_LIST) cycle
         do j = 0, int(capnp_list_len(lst)) - 1
            el = capnp_list_get_struct(lst, j, err)
            if (err == CAPNP_OK) call clear_slot(el, 1)
         end do
      end do
   end subroutine drop_annotations

   subroutine clear_slot(p, k)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(in) :: k
      integer(int64) :: at
      if (p%kind /= CAPNP_PK_STRUCT .or. k >= p%pwords) return
      at = p%off + int(p%dwords + k, int64)*8_int64
      p%msg%segs(p%seg)%bytes(at:at + 7_int64) = 0_int8
   end subroutine clear_slot

   !> A type expression with each leading name moved into the scope of
   !> `CapnpfOrig`, the original file. Built-in type names stay as they are.
   function brand_expr(expr) result(out)
      character(len=*), intent(in) :: expr
      character(len=:), allocatable :: out
      character(len=*), parameter :: builtins = ' Void Bool Int8 Int16 Int32 Int64 UInt8' &
         //' UInt16 UInt32 UInt64 Float32 Float64 Text Data List Object AnyPointer' &
         //' AnyStruct AnyList Capability '
      integer :: i, j
      logical :: head
      character :: c
      out = ''
      head = .true.
      i = 1
      do while (i <= len(expr))
         c = expr(i:i)
         if (is_ident_char(c)) then
            j = i
            do while (j < len(expr))
               if (.not. is_ident_char(expr(j + 1:j + 1))) exit
               j = j + 1
            end do
            if (head .and. index(builtins, ' '//expr(i:j)//' ') == 0) then
               out = out//'CapnpfOrig.'//expr(i:j)
            else
               out = out//expr(i:j)
            end if
            head = .false.
            i = j + 1
            cycle
         end if
         if (c == '.' .and. head) then
            out = out//'CapnpfOrig.'
            head = .false.
         else
            out = out//c
            if (c == '(' .or. c == ',') head = .true.
         end if
         i = i + 1
      end do
   end function brand_expr

   pure function is_ident_char(c) result(y)
      character, intent(in) :: c
      logical :: y
      y = (c >= 'a' .and. c <= 'z') .or. (c >= 'A' .and. c <= 'Z') .or. &
          (c >= '0' .and. c <= '9') .or. c == '_'
   end function is_ident_char

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
      integer(int64) :: got, cap
      integer :: stat, st
      err = CAPNP_OK
      call to_cchars(shcmd, cmd)
      call to_cchars('r', mode)
      fp = c_popen(c_loc(cmd(1)), c_loc(mode(1)))
      if (.not. c_associated(fp)) then
         err = CAPNP_ERR_IO
         return
      end if
      cap = 65536_int64
      got = 0_int64
      allocate (acc(0:cap - 1_int64), stat=stat)
      if (stat /= 0) then
         st = c_pclose(fp)
         err = CAPNP_ERR_ALLOC
         return
      end if
      do
         if (got + 4096_int64 > cap) then
            if (cap > ishft(huge(cap), -1)) then
               st = c_pclose(fp)
               err = CAPNP_ERR_ALLOC
               return
            end if
            allocate (grown(0:cap*2_int64 - 1_int64), stat=stat)
            if (stat /= 0) then
               st = c_pclose(fp)
               err = CAPNP_ERR_ALLOC
               return
            end if
            grown(0:got - 1_int64) = acc(0:got - 1_int64)
            call move_alloc(grown, acc)
            cap = cap*2_int64
         end if
         addr = transfer(c_loc(acc(got)), addr)
         nread = c_fread(transfer(addr, fp), 1_c_size_t, 4096_c_size_t, fp)
         if (nread == 0_c_size_t) exit
         got = got + int(nread, int64)
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
      if (got <= 0_int64) then
         allocate (bytes(0))
      else
         allocate (bytes(0:got - 1_int64), stat=stat)
         if (stat /= 0) then
            err = CAPNP_ERR_ALLOC
            return
         end if
         bytes = acc(0:got - 1_int64)
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

   pure function known_command(s) result(y)
      character(len=*), intent(in) :: s
      logical :: y
      y = s == 'encode' .or. s == 'decode' .or. s == 'convert' .or. s == 'eval'
   end function known_command

   pure function is_option(s) result(y)
      character(len=*), intent(in) :: s
      logical :: y
      y = .false.
      if (len(s) > 1) y = s(1:1) == '-'
   end function is_option

   !> 0: unknown to this command, 1: a flag, 2: takes a value.
   pure function option_kind(cmd, name) result(k)
      character(len=*), intent(in) :: cmd, name
      integer :: k
      k = 0
      select case (name)
      case ('import-path')
         k = 2
      case ('no-standard-import', 'verbose', 'version')
         k = 1
      case ('short')
         if (cmd == 'convert' .or. cmd == 'decode' .or. cmd == 'eval') k = 1
      case ('quiet')
         if (cmd == 'convert' .or. cmd == 'decode') k = 1
      case ('segment-size')
         if (cmd == 'convert' .or. cmd == 'encode') k = 2
      case ('flat', 'packed')
         if (cmd == 'decode' .or. cmd == 'encode' .or. cmd == 'eval') k = 1
      case ('binary')
         if (cmd == 'eval') k = 1
      case ('output')
         if (cmd == 'eval') k = 2
      end select
   end function option_kind

   function short_option(cmd, c) result(name)
      character(len=*), intent(in) :: cmd
      character, intent(in) :: c
      character(len=:), allocatable :: name
      select case (c)
      case ('I')
         name = 'import-path'
      case ('p')
         name = 'packed'
      case ('o')
         name = 'output'
      case ('b')
         name = 'binary'
      case default
         name = ''
      end select
      if (len(name) > 0) then
         if (option_kind(cmd, name) == 0) name = ''
      end if
   end function short_option

   !> One option argument, parsed the way kj::MainBuilder parses it. A
   !> value may be attached or be the next argument when that argument
   !> is not itself an option.
   subroutine take_option(o, cmd, args, i, done, rc)
      type(cli_opts_t), intent(inout) :: o
      character(len=*), intent(in) :: cmd
      type(arg_t), intent(in) :: args(:)
      integer, intent(inout) :: i
      logical, intent(out) :: done
      integer, intent(inout) :: rc
      character(len=:), allocatable :: p, name, val, msg
      integer :: eq, j
      logical :: has_val
      done = .false.
      p = args(i)%s
      if (starts_with(p, '--')) then
         eq = index(p, '=')
         has_val = eq > 0
         if (has_val) then
            name = p(3:eq - 1)
            val = p(eq + 1:)
         else
            name = p(3:)
            val = ''
         end if
         select case (option_kind(cmd, name))
         case (0)
            if (p == '--help') then
               call print_usage(1, eq)
               rc = 0
            else
               call fail('--'//name//': unrecognized option', rc)
            end if
            done = .true.
         case (1)
            if (has_val) then
               call fail('--'//name//': option does not accept an argument', rc)
               done = .true.
               return
            end if
            call set_option(o, name, '', msg, done, rc)
            if (len(msg) > 0) call fail(p//': '//msg, rc)
            if (len(msg) > 0) done = .true.
         case (2)
            if (.not. has_val .and. i < size(args)) then
               if (.not. is_option(args(i + 1)%s)) then
                  i = i + 1
                  val = args(i)%s
                  has_val = .true.
               end if
            end if
            if (.not. has_val) then
               call fail('--'//name//': missing argument', rc)
               done = .true.
               return
            end if
            call set_option(o, name, val, msg, done, rc)
            if (len(msg) > 0) call fail(p//': '//msg, rc)
            if (len(msg) > 0) done = .true.
         end select
         return
      end if
      j = 2
      do while (j <= len(p))
         name = short_option(cmd, p(j:j))
         if (len(name) == 0) then
            call fail('-'//p(j:j)//': unrecognized option', rc)
            done = .true.
            return
         end if
         if (option_kind(cmd, name) == 2) then
            if (j < len(p)) then
               val = p(j + 1:)
            else if (i < size(args)) then
               if (is_option(args(i + 1)%s)) then
                  call fail('-'//p(j:j)//': missing argument', rc)
                  done = .true.
                  return
               end if
               i = i + 1
               val = args(i)%s
            else
               call fail('-'//p(j:j)//': missing argument', rc)
               done = .true.
               return
            end if
            call set_option(o, name, val, msg, done, rc)
            if (len(msg) > 0) call fail('-'//p(j:j)//' '//val//': '//msg, rc)
            if (len(msg) > 0) done = .true.
            return
         end if
         call set_option(o, name, '', msg, done, rc)
         if (len(msg) > 0) call fail('-'//p(j:j)//': '//msg, rc)
         if (len(msg) > 0) done = .true.
         if (done) return
         j = j + 1
      end do
   end subroutine take_option

   !> The validity callbacks of capnp.c++. A non-empty msg rejects the
   !> option; done stops parsing after --version.
   subroutine set_option(o, name, val, msg, done, rc)
      type(cli_opts_t), intent(inout) :: o
      character(len=*), intent(in) :: name, val
      character(len=:), allocatable, intent(out) :: msg
      logical, intent(inout) :: done
      integer, intent(inout) :: rc
      integer(int64) :: v
      logical :: ok, ex
      integer :: err
      msg = ''
      select case (name)
      case ('import-path')
         inquire (file=val//'/.', exist=ex)
         if (.not. ex) then
            msg = 'no such directory'
            return
         end if
         o%inc_flags = o%inc_flags//' -I'//shell_quote(val)
      case ('no-standard-import')
         o%no_std = .true.
      case ('verbose')
         continue
      case ('version')
         call write_fd_line(1, "Cap'n Proto (capnpf) version 0.4.0", err)
         rc = 0
         done = .true.
      case ('short')
         o%short = .true.
      case ('quiet')
         o%quiet = .true.
      case ('segment-size')
         if (o%flat) then
            msg = 'cannot be used with --flat'
            return
         end if
         call c_long_value(val, v, ok)
         if (.not. ok) then
            msg = 'not an integer'
            return
         end if
         ! capnp.c++ keeps the value in a uint.
         o%seg_words = iand(v, 4294967295_int64)
      case ('flat')
         if (o%binary) then
            msg = 'cannot be used with --binary'
            return
         end if
         o%flat = .true.
      case ('packed')
         if (o%binary) then
            msg = 'cannot be used with --binary'
            return
         end if
         o%packed = .true.
      case ('binary')
         if (o%packed) then
            msg = 'cannot be used with --packed'
            return
         end if
         if (o%flat) then
            msg = 'cannot be used with --flat'
            return
         end if
         o%binary = .true.
      case ('output')
         if (.not. valid_fmt(val)) then
            msg = 'unknown format: '//val
            return
         end if
         o%ofmt = val
      end select
   end subroutine set_option

   !> strtol(s, &end, 0) with the whole string consumed, as setSegmentSize
   !> requires. Out-of-range values saturate like strtol.
   subroutine c_long_value(s, v, ok)
      character(len=*), intent(in) :: s
      integer(int64), intent(out) :: v
      logical, intent(out) :: ok
      character(kind=c_char), allocatable, target :: buf(:)
      type(c_ptr) :: endp
      integer(c_intptr_t) :: a0, a1
      v = 0_int64
      ok = .false.
      if (len(s) == 0) return
      call to_cchars(s, buf)
      v = int(c_strtol(c_loc(buf(1)), endp, 0_c_int), int64)
      a0 = transfer(c_loc(buf(1)), a0)
      a1 = transfer(endp, a1)
      ok = a1 - a0 == int(len(s), c_intptr_t)
   end subroutine c_long_value

   pure function starts_with(s, pfx) result(y)
      character(len=*), intent(in) :: s, pfx
      logical :: y
      y = .false.
      if (len(s) < len(pfx)) return
      y = s(1:len(pfx)) == pfx
   end function starts_with

   subroutine cmd_encode(sch, idx, benv, to_fmt, seg_words, rc)
      type(capnp_dyn_schema_t), intent(in) :: sch
      integer, intent(in) :: idx
      type(capnp_brand_env_t), intent(in) :: benv
      character(len=*), intent(in) :: to_fmt
      integer(int64), intent(in) :: seg_words
      integer, intent(out) :: rc
      integer(int8), allocatable :: raw(:)
      integer :: err
      logical :: ok
      rc = 0
      call read_stdin(raw, err)
      if (err /= CAPNP_OK) then
         call fail('empty input', rc)
         return
      end if
      call check_plausible('text', raw, ok)
      if (.not. ok) then
         rc = 1
         return
      end if
      call text_to_out(sch, idx, benv, bytes_to_text(raw), to_fmt, .true., seg_words, rc)
   end subroutine cmd_encode

   subroutine cmd_decode(sch, idx, benv, from_fmt, short, quiet, rc)
      type(capnp_dyn_schema_t), intent(in) :: sch
      integer, intent(in) :: idx
      type(capnp_brand_env_t), intent(in) :: benv
      character(len=*), intent(in) :: from_fmt
      logical, intent(in) :: short, quiet
      integer, intent(out) :: rc
      integer(int8), allocatable :: raw(:)
      integer :: err
      logical :: ok
      rc = 0
      call read_stdin(raw, err)
      if (err /= CAPNP_OK) then
         call empty_stream(quiet, rc)
         return
      end if
      if (.not. quiet) then
         call check_plausible(from_fmt, raw, ok)
         if (.not. ok) then
            rc = 1
            return
         end if
      end if
      call bytes_to_out(sch, idx, benv, raw, from_fmt, 'text', .not. short, 0_int64, rc)
   end subroutine cmd_decode

   subroutine cmd_convert(sch, idx, benv, from, to, short, seg_words, quiet, rc)
      type(capnp_dyn_schema_t), intent(in) :: sch
      integer, intent(in) :: idx
      type(capnp_brand_env_t), intent(in) :: benv
      character(len=*), intent(in) :: from, to
      logical, intent(in) :: short
      integer(int64), intent(in) :: seg_words
      logical, intent(in) :: quiet
      integer, intent(out) :: rc
      integer(int8), allocatable :: raw(:)
      integer :: err
      logical :: ok
      rc = 0
      call read_stdin(raw, err)
      if (err /= CAPNP_OK) then
         ! capnp 1.4.0 reads the first buffer to judge plausibility.
         ! --quiet skips that read, so an empty stream exits 0.
         call empty_stream(quiet, rc)
         return
      end if
      if (.not. quiet) then
         call check_plausible(from, raw, ok)
         if (.not. ok) then
            rc = 1
            return
         end if
      end if
      if (from == 'text') then
         call text_to_out(sch, idx, benv, bytes_to_text(raw), to, .not. short, seg_words, rc)
      else if (from == 'json') then
         call json_to_out(sch, idx, benv, bytes_to_text(raw), to, .not. short, seg_words, rc)
      else
         call bytes_to_out(sch, idx, benv, raw, from, to, .not. short, seg_words, rc)
      end if
   end subroutine cmd_convert

   !> checkPlausibility in capnp 1.4.0: an input prefix that cannot be in
   !> the named format is rejected before any message is read. Only
   !> IMPOSSIBLE is an error, so the schema size checks, which only tell
   !> PLAUSIBLE from WRONG_TYPE, are left out.
   subroutine check_plausible(fmt, raw, ok)
      character(len=*), intent(in) :: fmt
      integer(int8), intent(in) :: raw(0:)
      logical, intent(out) :: ok
      character(len=11), parameter :: guesses(6) = [character(len=11) :: &
         'binary', 'text', 'packed', 'json', 'flat', 'flat-packed']
      integer, allocatable :: b(:)
      integer(int64) :: i, n
      integer :: g, rc
      n = capnp_prefix_bytes(size(raw, kind=int64), 8192_int64)
      allocate (b(0:n - 1_int64))
      do i = 0_int64, n - 1_int64
         b(i) = iand(int(raw(i)), 255)
      end do
      ok = plausibility(fmt, raw, b) /= P_IMPOSSIBLE
      if (ok) return
      do g = 1, size(guesses)
         if (plausibility(trim(guesses(g)), raw, b) >= P_WRONG_TYPE) then
            call fail('The input is not in "'//fmt//'" format. It looks like it is in "' &
                      //trim(guesses(g))//'" format. Try that instead.', rc)
            return
         end if
      end do
      call fail('The input is not in "'//fmt//'" format.', rc)
   end subroutine check_plausible

   function plausibility(fmt, raw, b) result(r)
      character(len=*), intent(in) :: fmt
      integer(int8), intent(in) :: raw(0:)
      integer, intent(in) :: b(0:)
      integer :: r
      select case (fmt)
      case ('binary')
         r = plausible_binary(b)
      case ('packed')
         r = plausible_packed(b, .false.)
      case ('flat', 'canonical')
         r = plausible_flat(b, 1_int64)
      case ('flat-packed')
         r = plausible_packed(b, .true.)
      case ('text')
         r = P_PLAUSIBLE
         if (.not. text_plausible(raw)) r = P_IMPOSSIBLE
      case ('json')
         r = plausible_json(b)
      case default
         r = P_PLAUSIBLE
      end select
   end function plausibility

   pure function le32(b, at) result(v)
      integer, intent(in) :: b(0:)
      integer, intent(in) :: at
      integer(int64) :: v
      v = int(b(at), int64) + ishft(int(b(at + 1), int64), 8) + &
          ishft(int(b(at + 2), int64), 16) + ishft(int(b(at + 3), int64), 24)
   end function le32

   pure function plausible_flat(b, nsegs) result(r)
      integer, intent(in) :: b(0:)
      integer(int64), intent(in) :: nsegs
      integer :: r
      integer(int64) :: seg
      r = P_PLAUSIBLE
      if (size(b) < 8) return
      if (iand(b(0), 3) == 2) then
         seg = le32(b, 4)
         if (seg == 0_int64 .or. seg >= nsegs) r = P_IMPOSSIBLE
         return
      end if
      if (iand(b(0), 3) /= 0) then
         r = P_IMPOSSIBLE
         return
      end if
      if (iand(b(3), 128) /= 0) then
         if (.not. (all(b(0:3) == 255) .and. all(b(4:7) == 0))) then
            r = P_IMPOSSIBLE
            return
         end if
      end if
      if (iand(b(3), 224) /= 0) then
         r = P_IMPLAUSIBLE
         return
      end if
      if (b(4) + 256*b(5) + b(6) + 256*b(7) > 2048) r = P_IMPLAUSIBLE
   end function plausible_flat

   pure function plausible_binary(b) result(r)
      integer, intent(in) :: b(0:)
      integer :: r
      integer(int64) :: nsegs, off
      r = P_PLAUSIBLE
      if (size(b) < 8) return
      ! The count is stored minus one in a uint32, so it can wrap to zero.
      nsegs = mod(le32(b, 0) + 1_int64, 4294967296_int64)
      if (nsegs > 65536_int64) then
         r = P_IMPOSSIBLE
         return
      else if (nsegs > 256_int64) then
         r = P_IMPLAUSIBLE
         return
      end if
      if (le32(b, 4) > 134217728_int64) then
         r = P_IMPLAUSIBLE
         return
      end if
      off = 4_int64 + nsegs*4_int64
      if (mod(off, 8_int64) /= 0_int64) off = off + 4_int64
      if (size(b, kind=int64) < off + 8_int64) return
      r = plausible_flat(b(off:), nsegs)
   end function plausible_binary

   !> Unpack at most 1 KiB of the prefix, then check it as binary or flat.
   pure function plausible_packed(b, flat) result(r)
      integer, intent(in) :: b(0:)
      logical, intent(in) :: flat
      integer :: r
      integer, allocatable :: u(:)
      integer :: pos, last, tag, i, cnt, nu, avail
      allocate (u(0:min(size(b), 1024)*1100 + 16))
      nu = 0
      pos = 0
      last = min(size(b), 1024)
      do while (pos < last)
         tag = b(pos)
         pos = pos + 1
         i = 0
         do while (i < 8 .and. pos < last)
            if (btest(tag, i)) then
               if (b(pos) == 0) then
                  r = P_IMPOSSIBLE
                  return
               end if
               u(nu) = b(pos)
               pos = pos + 1
            else
               u(nu) = 0
            end if
            nu = nu + 1
            i = i + 1
         end do
         if (pos == last) exit
         if (tag == 0) then
            cnt = b(pos)*8
            pos = pos + 1
            u(nu:nu + cnt - 1) = 0
            nu = nu + cnt
         else if (tag == 255) then
            cnt = b(pos)*8
            pos = pos + 1
            avail = min(cnt, last - pos)
            u(nu:nu + avail - 1) = b(pos:pos + avail - 1)
            nu = nu + avail
            pos = pos + avail
         end if
      end do
      if (flat) then
         r = plausible_flat(u(0:nu - 1), 1_int64)
      else
         r = plausible_binary(u(0:nu - 1))
      end if
   end function plausible_packed

   !> isPlausiblyJson reads signed chars, so NUL and bytes from 0x80 up
   !> are not unprintable there.
   pure function plausible_json(b) result(r)
      integer, intent(in) :: b(0:)
      integer :: r
      integer :: i, c, state
      integer, parameter :: st_pre = 0, st_com = 1, st_body = 2
      r = P_PLAUSIBLE
      state = st_pre
      do i = 0, size(b) - 1
         c = b(i)
         select case (state)
         case (st_pre)
            if (c == iachar('{')) then
               state = st_body
               cycle
            else if (c == iachar('#') .or. c == iachar('/')) then
               state = st_com
               cycle
            else if (c /= 32 .and. c /= 10 .and. c /= 13 .and. c /= 9 .and. c /= 11) then
               r = P_IMPOSSIBLE
               return
            end if
         case (st_com)
            if (c == 10) then
               state = st_pre
               cycle
            end if
         case (st_body)
            if (c == iachar('"')) return
         end select
         if ((c > 0 .and. c < 32 .and. c /= 10 .and. c /= 13 .and. c /= 9 .and. c /= 11) &
             .or. c == 127) then
            r = P_IMPOSSIBLE
            return
         end if
      end do
   end function plausible_json

   !> True when the first bytes can be Cap'n Proto text. Mirrors
   !> isPlausiblyText in capnp 1.4.0: preamble is whitespace, `#`, or `(`,
   !> and an unprintable byte is rejected. A quote in the body stops the scan.
   function text_plausible(raw) result(ok)
      integer(int8), intent(in) :: raw(0:)
      logical :: ok
      integer :: state, c
      integer(int64) :: i, n
      integer, parameter :: st_pre = 0, st_com = 1, st_body = 2
      ok = .true.
      state = st_pre
      n = capnp_prefix_bytes(size(raw, kind=int64), 8192_int64)
      do i = 0_int64, n - 1_int64
         c = int(raw(i))
         if (c < 0) c = c + 256
         select case (state)
         case (st_pre)
            if (c == iachar('(')) then
               state = st_body
               cycle
            else if (c == iachar('#')) then
               state = st_com
               cycle
            else if (c /= iachar(' ') .and. c /= 10 .and. c /= 13 .and. &
                     c /= 9 .and. c /= 11) then
               ok = .false.
               return
            end if
         case (st_com)
            if (c == 10) then
               state = st_pre
               cycle
            end if
         case (st_body)
            if (c == iachar('"') .or. c == iachar("'")) return
         end select
         if ((c < 32 .and. c /= 10 .and. c /= 13 .and. c /= 9 .and. c /= 11) .or. &
             c == 127) then
            ok = .false.
            return
         end if
      end do
   end function text_plausible

   subroutine empty_stream(quiet, rc)
      logical, intent(in) :: quiet
      integer, intent(out) :: rc
      if (quiet) then
         rc = 0
      else
         call fail('empty input', rc)
      end if
   end subroutine empty_stream

   subroutine text_to_out(sch, idx, benv, text, to, pretty, seg_words, rc)
      type(capnp_dyn_schema_t), intent(in) :: sch
      integer, intent(in) :: idx
      type(capnp_brand_env_t), intent(in) :: benv
      character(len=*), intent(in) :: text, to
      logical, intent(in) :: pretty
      integer(int64), intent(in) :: seg_words
      integer, intent(out) :: rc
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root
      integer :: beg, topos, from, count, err, nbeg, nto, last
      logical :: hard
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
         ! capnp 1.4.0 lexes a message with the text up to the next one. A
         ! NUL its lexer cannot read there fails before the message prints.
         call capnp_text_span(text, topos + 1, nbeg, nto, err)
         last = len(text)
         if (err == CAPNP_OK .and. nbeg > 0) last = nbeg - 1
         if (.not. nul_lexes(text(from:last))) then
            call fail('text decode failed', rc)
            return
         end if
         call capnp_text_decode(sch, idx, text(beg:topos), msg, err, brand_env=benv, &
                                hard_failed=hard)
         if (err /= CAPNP_OK) then
            ! A reported error still prints the recovered message. An
            ! unresolved name throws before any output.
            if (.not. hard) then
               root = capnp_root(msg, err)
               if (err == CAPNP_OK .and. root_is_struct(root)) then
                  call emit_one(sch, idx, benv, msg, root, to, pretty, seg_words, rc)
                  call capnp_message_free(msg)
                  if (rc /= 0) return
                  call fail('text decode failed', rc)
                  return
               end if
            end if
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
         call emit_one(sch, idx, benv, msg, root, to, pretty, seg_words, rc)
         call capnp_message_free(msg)
         if (rc /= 0) return
         count = count + 1
         from = topos + 1
      end do
      if (count == 0) call fail('empty input', rc)
   end subroutine text_to_out

   !> Whether every NUL in s is one the Cap'n Proto lexer reads: a raw
   !> byte of a double-quoted string or of a # comment.
   pure function nul_lexes(s) result(ok)
      character(len=*), intent(in) :: s
      logical :: ok
      integer, parameter :: st_code = 0, st_com = 1, st_str = 2, st_esc = 3, st_hex = 4
      integer :: i, state
      ok = .true.
      if (index(s, achar(0)) == 0) return
      state = st_code
      do i = 1, len(s)
         select case (state)
         case (st_code)
            if (s(i:i) == '#') then
               state = st_com
            else if (s(i:i) == '"') then
               state = st_str
               if (i > 2) then
                  if (s(i - 2:i - 1) == '0x') state = st_hex
               end if
            end if
         case (st_com)
            if (s(i:i) == achar(10)) state = st_code
            cycle
         case (st_str)
            if (s(i:i) == '\') then
               state = st_esc
            else if (s(i:i) == '"') then
               state = st_code
            end if
            cycle
         case (st_esc)
            state = st_str
         case (st_hex)
            if (s(i:i) == '"') state = st_code
         end select
         if (s(i:i) == achar(0)) then
            ok = .false.
            return
         end if
      end do
   end function nul_lexes

   subroutine json_to_out(sch, idx, benv, text, to, pretty, seg_words, rc)
      type(capnp_dyn_schema_t), intent(in) :: sch
      integer, intent(in) :: idx
      type(capnp_brand_env_t), intent(in) :: benv
      character(len=*), intent(in) :: text, to
      logical, intent(in) :: pretty
      integer(int64), intent(in) :: seg_words
      integer, intent(out) :: rc
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root
      integer :: beg, topos, from, count, err
      logical :: soft, number_failed
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
         call capnp_json_decode(sch, idx, text(beg:topos), msg, err, brand_env=benv, &
                                number_failed=number_failed)
         if (err /= CAPNP_OK) then
            if (number_failed) then
               root = capnp_root(msg, err)
               if (err == CAPNP_OK .and. root_is_struct(root)) then
                  call emit_one(sch, idx, benv, msg, root, to, pretty, seg_words, rc)
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
         call emit_one(sch, idx, benv, msg, root, to, pretty, seg_words, rc)
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

   subroutine bytes_to_out(sch, idx, benv, raw, from, to, pretty, seg_words, rc)
      type(capnp_dyn_schema_t), intent(in) :: sch
      integer, intent(in) :: idx
      type(capnp_brand_env_t), intent(in) :: benv
      integer(int8), intent(in) :: raw(0:)
      character(len=*), intent(in) :: from, to
      logical, intent(in) :: pretty
      integer(int64), intent(in) :: seg_words
      integer, intent(out) :: rc
      type(capnp_message_t), target :: msg
      integer(int8), allocatable :: one(:), wrapped(:), unpacked(:), chopped(:)
      integer(int64) :: off, n, total, words
      integer :: err, count
      integer(int64) :: writes
      logical :: recover
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
         if (err /= CAPNP_OK .or. .not. root_is_struct(root)) then
            call capnp_message_free(msg)
            call emit_recovered(sch, idx, benv, to, pretty, seg_words, rc)
            return
         end if
         call emit_one(sch, idx, benv, msg, root, to, pretty, seg_words, rc)
         call capnp_message_free(msg)
         return
      end if
      if (from == 'flat' .or. from == 'canonical') then
         ! A partial trailing word is dropped. An empty remainder is
         ! not a message.
         words = size(raw, kind=int64)/8_int64
         if (words == 0_int64) then
            ! No whole word. capnp 1.4.0 still writes the default value
            ! for flat input, then reports the read error. Canonical
            ! input stops with no output.
            if (from == 'flat') then
               call emit_recovered(sch, idx, benv, to, pretty, seg_words, rc)
            else
               call fail('bad flat input', rc)
            end if
            return
         end if
         if (words*8_int64 /= size(raw, kind=int64)) then
            allocate (chopped(0:words*8_int64 - 1_int64))
            chopped = raw(0:words*8_int64 - 1_int64)
            wrapped = wrap_flat(chopped)
         else
            wrapped = wrap_flat(raw)
         end if
         call capnp_deserialize_bytes(wrapped, msg, err, options=tool_limits())
         if (err /= CAPNP_OK) then
            call capnp_message_free(msg)
            call fail('bad flat input', rc)
            return
         end if
         if (from == 'canonical') then
            if (.not. capnp_is_canonical(msg, err)) then
               call capnp_message_free(msg)
               call fail('not canonical', rc)
               return
            end if
         end if
         root = capnp_root(msg, err)
         if (err /= CAPNP_OK .or. .not. root_is_struct(root)) then
            call capnp_message_free(msg)
            call emit_recovered(sch, idx, benv, to, pretty, seg_words, rc)
            return
         end if
         call emit_one(sch, idx, benv, msg, root, to, pretty, seg_words, rc)
         call capnp_message_free(msg)
         return
      end if
      if (from == 'packed') then
         off = 0_int64
         total = size(raw, kind=int64)
         count = 0
         recover = .false.
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
               ! getRoot recovers a struct pointer error with an empty
               ! struct, and the stream goes on.
               call capnp_message_free(msg)
               call emit_recovered(sch, idx, benv, to, pretty, seg_words, rc)
            else
               writes = stdout_writes
               call emit_one(sch, idx, benv, msg, root, to, pretty, seg_words, rc)
               call capnp_message_free(msg)
               ! A failure with no output is a thrown exception in capnp 1.4.0,
               ! which ends the run.
               if (rc /= 0 .and. stdout_writes == writes) return
            end if
            ! A wrong pointer still yields the recovered message. capnp
            ! 1.4.0 prints it and reads the next message in the stream.
            if (rc /= 0) recover = .true.
            count = count + 1
         end do
         if (count == 0) then
            call fail('empty input', rc)
         else if (recover) then
            rc = 1
         end if
         return
      end if
      off = 0_int64
      total = size(raw, kind=int64)
      count = 0
      recover = .false.
      ! Shorter than a segment table. capnp 1.4.0 writes the default
      ! value and then reports the short read.
      if (total > 0_int64 .and. total < 8_int64) then
         call emit_recovered(sch, idx, benv, to, pretty, seg_words, rc)
         return
      end if
      do while (off < total)
         ! Too many segments: the reader goes on with one empty segment
         ! after the first word, so the root is the default value.
         if (off + 8_int64 <= total) then
            if (iand(int(cp_get_i32(raw, off), int64), 4294967295_int64) >= 511_int64) then
               call emit_recovered(sch, idx, benv, to, pretty, seg_words, rc)
               recover = .true.
               count = count + 1
               off = off + 8_int64
               cycle
            end if
         end if
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
            ! getRoot recovers a struct pointer error with an empty
            ! struct, and the stream goes on.
            call capnp_message_free(msg)
            call emit_recovered(sch, idx, benv, to, pretty, seg_words, rc)
         else
            writes = stdout_writes
            call emit_one(sch, idx, benv, msg, root, to, pretty, seg_words, rc)
            call capnp_message_free(msg)
            ! A failure with no output is a thrown exception in capnp 1.4.0,
            ! which ends the run.
            if (rc /= 0 .and. stdout_writes == writes) return
         end if
         ! A wrong pointer still yields the recovered message. capnp
         ! 1.4.0 prints it and reads the next message in the stream.
         if (rc /= 0) recover = .true.
         count = count + 1
         off = off + n
      end do
      if (count == 0) then
         call fail('empty input', rc)
      else if (recover) then
         rc = 1
      end if
   end subroutine bytes_to_out

   !> Write the default value of a root that could not be read, then
   !> fail. capnp 1.4.0 does this when a binary or flat read throws
   !> after getRoot returns a null struct.
   subroutine emit_recovered(sch, idx, benv, to, pretty, seg_words, rc)
      type(capnp_dyn_schema_t), intent(in) :: sch
      integer, intent(in) :: idx
      type(capnp_brand_env_t), intent(in) :: benv
      character(len=*), intent(in) :: to
      logical, intent(in) :: pretty
      integer(int64), intent(in) :: seg_words
      integer, intent(out) :: rc
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root
      integer :: err
      call capnp_message_init_builder(msg, err)
      if (err /= CAPNP_OK) then
         call fail('bad message', rc)
         return
      end if
      root = capnp_root(msg, err)
      if (err /= CAPNP_OK) then
         call capnp_message_free(msg)
         call fail('bad root', rc)
         return
      end if
      call emit_one(sch, idx, benv, msg, root, to, pretty, seg_words, rc)
      call capnp_message_free(msg)
      if (rc == 0) rc = 1
   end subroutine emit_recovered

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
      integer :: err, caps
      logical :: saw_cap
      rc = 0
      if (to == 'text') then
         call capnp_text_encode(sch, idx, root, text, err, pretty=pretty, brand_env=env, &
                                traversal_words=huge(0_int64), saw_cap=saw_cap)
         if (saw_cap) then
            ! capnp 1.4.0 throws on reading a capability and stops.
            call fail('cannot read a capability without a capability table', rc)
            return
         end if
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
         call write_binary_message(msg, to, seg_words, outb, err, caps)
         if (err /= CAPNP_OK) then
            call fail('binary encode failed', rc)
            return
         end if
         if (caps == 2) then
            call fail('Cannot create a canonical message with a capability', rc)
            return
         end if
         call write_stdout(outb, err)
         if (err /= CAPNP_OK) then
            call fail('cannot write stdout', rc)
         else if (caps == 1) then
            call fail('Message contained invalid capability pointer', rc)
         end if
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
      integer(int64) :: left
      integer(c_long) :: nw
      type(c_ptr) :: p
      err = CAPNP_OK
      left = size(buf, kind=int64)
      if (left <= 0_int64) return
      if (fd == 1) stdout_writes = stdout_writes + 1_int64
      addr = transfer(c_loc(buf(1)), addr)
      do while (left > 0_int64)
         p = transfer(addr, p)
         nw = c_write(int(fd, c_int), p, int(capnp_write_chunk_bytes(left), c_size_t))
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
         left = left - int(nw, int64)
      end do
   end subroutine write_fd

   !> Write one text line and a newline. A full stdout is an I/O error.
   subroutine write_fd_line(fd, text, err)
      integer, intent(in) :: fd
      character(len=*), intent(in) :: text
      integer, intent(out) :: err
      integer(int8), allocatable, target :: buf(:)
      integer(int64) :: i, n
      integer :: u, stat
      n = len(text, kind=int64)
      allocate (buf(n + 1_int64), stat=stat)
      if (stat /= 0) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      do i = 1_int64, n
         u = iachar(text(i:i))
         if (u >= 128) then
            buf(i) = int(u - 256, int8)
         else
            buf(i) = int(u, int8)
         end if
      end do
      buf(n + 1_int64) = 10_int8
      call write_fd(fd, buf, err)
   end subroutine write_fd_line

   !> caps is 1 when a capability was written as null and 2 when the
   !> canonical form could not be made, which stops capnp 1.4.0.
   subroutine write_binary_message(msg, to, seg_words, outb, err, caps)
      type(capnp_message_t), intent(inout), target :: msg
      integer(int8), allocatable, intent(out) :: outb(:)
      character(len=*), intent(in) :: to
      integer(int64), intent(in) :: seg_words
      integer, intent(out) :: err
      integer, intent(out) :: caps
      type(capnp_message_t), target :: check
      logical :: found
      type(capnp_message_t), target :: reseg
      type(capnp_ptr_t) :: root, croot
      type(capnp_builder_options_t) :: bopt
      integer(int8), allocatable :: flatb(:)
      integer(int64) :: need
      err = CAPNP_OK
      caps = 0
      found = .false.
      if (to == 'canonical') then
         call capnp_canonicalize(msg, outb, err, null_caps=.true., saw_cap=found)
         if (err == CAPNP_OK .and. found) then
            ! A trailing capability leaves a null pointer that is not
            ! truncated. capnp 1.4.0 then fails its isCanonical assert.
            caps = 1
            call capnp_deserialize_bytes(wrap_flat(outb), check, err, options=tool_limits())
            if (err == CAPNP_OK) then
               if (.not. capnp_is_canonical(check, err)) caps = 2
            end if
            call capnp_message_free(check)
            if (err /= CAPNP_OK) caps = 2
            err = CAPNP_OK
         end if
         return
      else if (to == 'flat') then
         call to_flat_bytes(msg, outb, err, found)
      else if (to == 'flat-packed') then
         call to_flat_bytes(msg, flatb, err, found)
         if (err /= CAPNP_OK) return
         call capnp_pack(flatb, outb, err)
      else if (seg_words > 0_int64 .and. (to == 'binary' .or. to == 'packed')) then
         ! MallocMessageBuilder rejects a first segment above
         ! MAX_SEGMENT_WORDS. A segment that holds the whole message gives
         ! the same output at any larger size, so no more is allocated.
         if (seg_words > 536870911_int64) then
            err = CAPNP_ERR_ALLOC
            return
         end if
         root = capnp_root(msg, err)
         if (err /= CAPNP_OK) return
         need = capnp_total_size(root, err)
         if (err /= CAPNP_OK) return
         bopt%first_segment_words = min(seg_words, 2_int64*need + 1024_int64)
         bopt%allocation_strategy = CAPNP_ALLOC_FIXED_SIZE
         call capnp_message_init_builder(reseg, err, options=bopt)
         if (err /= CAPNP_OK) return
         croot = copy_root(reseg, root, err)
         if (err /= CAPNP_OK) then
            call capnp_message_free(reseg)
            return
         end if
         call null_caps(croot, found)
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
         croot = copy_root(reseg, root, err)
         if (err /= CAPNP_OK) then
            call capnp_message_free(reseg)
            return
         end if
         call null_caps(croot, found)
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
      if (found) caps = 1
   end subroutine write_binary_message

   !> Write each capability under p as a null pointer. capnp 1.4.0 has no
   !> capability table, so its copy reports the pointer and writes null.
   recursive subroutine null_caps(p, found)
      type(capnp_ptr_t), intent(in) :: p
      logical, intent(inout) :: found
      type(capnp_ptr_t) :: q
      integer :: k, err
      integer(int64) :: i
      select case (p%kind)
      case (CAPNP_PK_STRUCT)
         do k = 0, p%pwords - 1
            q = capnp_getp(p, k, err)
            if (err /= CAPNP_OK) cycle
            if (q%kind == CAPNP_PK_CAP) then
               call clear_slot(p, k)
               found = .true.
            else
               call null_caps(q, found)
            end if
         end do
      case (CAPNP_PK_LIST)
         if (p%esize == CAPNP_SZ_PTR) then
            do i = 0_int64, p%nelem - 1_int64
               q = capnp_getp(p, int(i), err)
               if (err /= CAPNP_OK) cycle
               if (q%kind == CAPNP_PK_CAP) then
                  p%msg%segs(p%seg)%bytes(p%off + i*8_int64:p%off + i*8_int64 + 7_int64) = 0_int8
                  found = .true.
               else
                  call null_caps(q, found)
               end if
            end do
         else if (p%esize == CAPNP_SZ_COMPOSITE .and. p%pwords > 0) then
            do i = 0_int64, p%nelem - 1_int64
               q = capnp_list_get_struct(p, int(i), err)
               if (err == CAPNP_OK) call null_caps(q, found)
            end do
         end if
      end select
   end subroutine null_caps

   subroutine to_flat_bytes(msg, flatb, err, found)
      type(capnp_message_t), intent(inout), target :: msg
      integer(int8), allocatable, intent(out) :: flatb(:)
      integer, intent(out) :: err
      logical, intent(inout) :: found
      type(capnp_message_t), target :: single
      type(capnp_ptr_t) :: root, croot
      integer(int64) :: n, need
      err = CAPNP_OK
      root = capnp_root(msg, err)
      if (err /= CAPNP_OK) return
      if (root%kind == CAPNP_PK_NULL) then
         ! A null root is written as a zero-size struct pointer.
         need = 1_int64
      else
         need = 1_int64 + capnp_total_size(root, err)
         if (err /= CAPNP_OK) return
      end if
      call capnp_message_init_builder(single, err, first_words=max(need, 8_int64))
      if (err /= CAPNP_OK) return
      croot = copy_root(single, root, err)
      if (err /= CAPNP_OK) then
         call capnp_message_free(single)
         return
      end if
      call null_caps(croot, found)
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

   !> Copy a root. A null pointer becomes a zero-word struct, which is
   !> the pointer capnp 1.4.0 writes for getRoot<AnyStruct>() on null.
   function copy_root(dst, src, err) result(croot)
      type(capnp_message_t), intent(inout), target :: dst
      type(capnp_ptr_t), intent(in) :: src
      integer, intent(out) :: err
      type(capnp_ptr_t) :: croot
      if (src%kind == CAPNP_PK_NULL) then
         croot = capnp_new_struct(dst, 0, 0, err)
      else
         croot = capnp_copy(dst, src, err)
      end if
   end function copy_root

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
      integer :: err, gidx, sidx, tw, fd, tag
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
      fd = field_discriminant(f)
      if (fd /= NO_DISCRIMINANT) then
         tag = capnp_dyn_which(sch, idx, obj, err)
         if (err /= CAPNP_OK .or. tag /= fd) then
            call fail('eval failed', rc)
            return
         end if
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
         s(i + 1_int64:i + 1_int64) = char(capnp_u_of_i8(raw(i)))
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
