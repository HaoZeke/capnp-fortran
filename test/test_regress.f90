!> Regressions for the text and JSON codec review.
module test_regress
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed, skip_test
   use, intrinsic :: iso_fortran_env, only: int64, real64
   use, intrinsic :: iso_c_binding, only: c_int
   use capnp
   use capnp_dynamic
   use capnp_text
   use capnp_json
   use capnp_codec, only: capnp_brand_env_t, capnp_codec_find_field
   use cli_path, only: capnpf_setup, capnpc_setup
   implicit none
   private
   public :: collect_regress, release_regress_tmp

   character(len=:), allocatable :: tmp

   interface
      function c_getpid() bind(c, name="getpid") result(pid)
         import :: c_int
         integer(c_int) :: pid
      end function c_getpid
   end interface

contains

   subroutine init_tmp()
      character(len=16) :: buf
      integer(c_int) :: pid
      if (allocated(tmp)) return
      pid = c_getpid()
      write (buf, '(i0)') pid
      tmp = '/tmp/capnpf-regress-'//trim(buf)
   end subroutine init_tmp

   !> Remove this process's regress directory, including the plugin files
   !> created under it.
   subroutine release_regress_tmp()
      if (.not. allocated(tmp)) return
      if (.not. sh('rm -rf '//tmp)) return
   end subroutine release_regress_tmp

   !> True only when both arrays have the same length. The element compare
   !> runs after that check, so a short reference fails the test instead of
   !> aborting under a bounds check.
   function same_i8(a, b) result(y)
      integer(int8), intent(in) :: a(:), b(:)
      logical :: y
      y = size(a) == size(b)
      if (y) y = all(a == b)
   end function same_i8

   function same_int(a, b) result(y)
      integer, intent(in) :: a(:), b(:)
      logical :: y
      y = size(a) == size(b)
      if (y) y = all(a == b)
   end function same_int

   subroutine collect_regress(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      call init_tmp()
      testsuite = [ &
         new_unittest("comment-eof", run_comment_eof), &
         new_unittest("segment-bomb", run_segment_bomb), &
         new_unittest("binary-stdout", run_binary_stdout), &
         new_unittest("list-linear", run_list_linear), &
         new_unittest("canonical-release", run_canonical_release), &
         new_unittest("json-collisions", run_json_collisions), &
         new_unittest("json-void-and-missing", run_json_void_missing), &
         new_unittest("json-surrogate", run_json_surrogate), &
         new_unittest("json-exact-int", run_json_exact_int), &
         new_unittest("json-null-disc", run_json_null_disc), &
         new_unittest("json-union-diff", run_json_union_diff), &
         new_unittest("text-leniency", run_text_leniency), &
         new_unittest("unloaded-guard", run_unloaded), &
         new_unittest("json-unloaded-guard", run_json_unloaded), &
         new_unittest("long-argument", run_long_arg), &
         new_unittest("schema-pipe", run_schema_pipe), &
         new_unittest("plugin-release", run_plugin_release), &
         new_unittest("cli-locate", run_cli_locate), &
         new_unittest("string-linear", run_string_linear), &
         new_unittest("write-error", run_write_error), &
         new_unittest("f32-text", run_f32_text), &
         new_unittest("regress-tmp", run_regress_tmp), &
         new_unittest("cli-sources", run_cli_sources), &
         new_unittest("json-union-many", run_json_union_many), &
         new_unittest("json-union-nested", run_json_union_nested), &
         new_unittest("json-schema-chain", run_json_schema_chain), &
         new_unittest("group-clear-depth", run_group_clear_depth), &
         new_unittest("packed-zero-cap", run_packed_zero_cap), &
         new_unittest("json-trailing-spaces", run_json_trailing_spaces), &
         new_unittest("json-union-clear", run_json_union_clear), &
         new_unittest("json-nul-eof", run_json_nul_eof), &
         new_unittest("json-nest-limit", run_json_nest_limit), &
         new_unittest("cli-json-stream", run_cli_json_stream), &
         new_unittest("text-shorthand", run_text_shorthand), &
         new_unittest("text-group-replace", run_text_group_replace), &
         new_unittest("text-ws-vt-ff", run_text_ws_vt_ff), &
         new_unittest("text-inactive-arm", run_text_inactive_arm), &
         new_unittest("json-dup-bad", run_json_dup_bad) &
         ]
   end subroutine collect_regress

   subroutine check_(error, cond, name)
      type(error_type), allocatable, intent(inout) :: error
      logical, intent(in) :: cond
      character(len=*), intent(in) :: name
      if (allocated(error)) return
      call check(error, cond, name)
   end subroutine check_

   function have_capnp() result(y)
      logical :: y
      y = sh('command -v capnp >/dev/null 2>&1')
   end function have_capnp

   function sh(cmd) result(ok)
      character(len=*), intent(in) :: cmd
      logical :: ok
      integer :: st, cs
      call execute_command_line(cmd, exitstat=st, cmdstat=cs, wait=.true.)
      ok = cs == 0 .and. st == 0
   end function sh

   function cap_setup() result(s)
      character(len=:), allocatable :: s
      s = capnpf_setup()
   end function cap_setup

   subroutine run_comment_eof(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root
      integer(int8), allocatable :: bytes(:)
      integer :: err, idx
      integer(int64) :: count
      call capnp_read_file('test/fixtures/kitchen.cgr.bin', bytes, err)
      if (err /= CAPNP_OK) then
         call skip_test(error, 'kitchen cgr missing')
         return
      end if
      call capnp_dyn_load(schema, bytes, err)
      call check_(error, err == CAPNP_OK, 'comment: schema')
      idx = capnp_dyn_find(schema, 'Sink', err)
      call check_(error, idx > 0, 'comment: Sink')
      call capnp_text_decode(schema, idx, '(count = 1) # c', msg, err)
      call check_(error, err == CAPNP_OK, 'comment: decode at eof')
      root = capnp_root(msg, err)
      count = capnp_dyn_get_int(schema, idx, root, 'count', err)
      call check_(error, err == CAPNP_OK .and. count == 1_int64, 'comment: count is 1')
      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end subroutine run_comment_eof

   subroutine run_segment_bomb(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg
      integer(int8) :: packed(0:4), plain(0:7)
      integer :: u, err
      if (.not. sh('mkdir -p '//tmp)) then
         call test_failed(error, 'mkdir')
         return
      end if
      ! One segment of 0x7FFFFF words: under the traversal budget, and
      ! more words than the four packed bytes can expand to.
      packed = 0_int8
      packed(0) = 112_int8
      packed(1) = -1_int8
      packed(2) = -1_int8
      packed(3) = 127_int8
      open (newunit=u, file=tmp//'/bomb.packed', access='stream', form='unformatted', &
            status='replace')
      write (u) packed(0:3)
      close (u)
      open (newunit=u, file=tmp//'/bomb.packed', access='stream', form='unformatted', &
            status='old', action='read')
      call capnp_read_message_packed_unit(u, msg, err)
      close (u)
      call check_(error, err == CAPNP_ERR_FRAMING, 'bomb: packed stream is framing')
      plain = 0_int8
      plain(4:7) = -1_int8
      open (newunit=u, file=tmp//'/bomb.plain', access='stream', form='unformatted', &
            status='replace')
      write (u) plain
      close (u)
      open (newunit=u, file=tmp//'/bomb.plain', access='stream', form='unformatted', &
            status='old', action='read')
      call capnp_read_message_unit(u, msg, err)
      close (u)
      call check_(error, err == CAPNP_ERR_TRAVERSAL .or. err == CAPNP_ERR_FRAMING, &
                  'bomb: plain stream rejects')
      if (.not. have_capnp()) return
      call check_(error, sh(cap_setup()//' && printf "\360\377\377\377\377" | "$CAP" decode' &
         //' schema/common.capnp Vec3 >/dev/null 2>'//tmp//'/bomb.err; test $? -ne 0'), &
         'bomb: cli packed header exits')
   end subroutine run_segment_bomb

   subroutine run_binary_stdout(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int8), allocatable :: got(:), ref(:)
      integer :: err
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp//' && '//cap_setup() &
         //' && printf "(x = 1, y = 0, z = 0)\n(x = 2, y = 0, z = 0)\n"' &
         //' | "$CAP" encode schema/common.capnp Vec3 > '//tmp//'/two.bin' &
         //' && printf "(x = 1, y = 0, z = 0)\n(x = 2, y = 0, z = 0)\n"' &
         //' | capnp encode schema/common.capnp Vec3 > '//tmp//'/two.ref' &
         //' && printf "(x = 3, y = 0, z = 0)\n"' &
         //' | "$CAP" encode schema/common.capnp Vec3 > '//tmp//'/app.bin' &
         //' && printf "(x = 4, y = 0, z = 0)\n"' &
         //' | "$CAP" encode schema/common.capnp Vec3 >> '//tmp//'/app.bin' &
         //' && printf "(x = 3, y = 0, z = 0)\n(x = 4, y = 0, z = 0)\n"' &
         //' | capnp encode schema/common.capnp Vec3 > '//tmp//'/app.ref')) then
         call test_failed(error, 'binary stdout encode failed')
         return
      end if
      call capnp_read_file(tmp//'/two.bin', got, err)
      call capnp_read_file(tmp//'/two.ref', ref, err)
      if (err == CAPNP_OK .and. allocated(got) .and. allocated(ref)) then
         call check_(error, same_i8(got, ref), 'stdout: two messages match capnp')
      else
         call check_(error, .false., 'stdout: two messages match capnp')
      end if
      call capnp_read_file(tmp//'/app.bin', got, err)
      call capnp_read_file(tmp//'/app.ref', ref, err)
      if (err == CAPNP_OK .and. allocated(got) .and. allocated(ref)) then
         call check_(error, same_i8(got, ref), 'stdout: append keeps both messages')
      else
         call check_(error, .false., 'stdout: append keeps both messages')
      end if
   end subroutine run_binary_stdout

   subroutine run_list_linear(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root, lst
      character(len=:), allocatable :: text, src
      integer(int8), allocatable :: bytes(:)
      integer :: err, idx
      integer(int64) :: c1, c2, rate, n
      real :: elapsed
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp)) then
         call test_failed(error, 'mkdir')
         return
      end if
      src = '@0x8f0c3d2b1a947e65;'//achar(10)// &
            'struct IntList { xs @0 :List(Int32); }'//achar(10)
      call spit(tmp//'/intlist.capnp', src, err)
      if (.not. sh('capnp compile -o- '//tmp//'/intlist.capnp > '//tmp//'/intlist.cgr')) then
         call test_failed(error, 'compile IntList')
         return
      end if
      call capnp_read_file(tmp//'/intlist.cgr', bytes, err)
      call capnp_dyn_load(schema, bytes, err)
      call check_(error, err == CAPNP_OK, 'list: schema')
      idx = capnp_dyn_find(schema, 'IntList', err)
      call check_(error, idx > 0, 'list: type')
      call capnp_message_init_builder(msg, err)
      root = capnp_dyn_new_root(schema, idx, msg, err)
      n = 50000_int64
      lst = capnp_dyn_init_list(schema, idx, root, 'xs', n, err)
      call check_(error, err == CAPNP_OK, 'list: small allocated')
      call system_clock(c1, rate)
      call capnp_text_encode(schema, idx, root, text, err, pretty=.false.)
      call system_clock(c2)
      elapsed = 0.0
      if (rate > 0_int64) elapsed = real(c2 - c1)/real(rate)
      call capnp_message_free(msg)
      call capnp_message_init_builder(msg, err)
      root = capnp_dyn_new_root(schema, idx, msg, err)
      n = 200000_int64
      lst = capnp_dyn_init_list(schema, idx, root, 'xs', n, err)
      call check_(error, err == CAPNP_OK .and. capnp_list_len(lst) == n, 'list: allocated')
      call capnp_text_encode(schema, idx, root, text, err, pretty=.false., &
                             traversal_words=1_int64)
      call check_(error, err == CAPNP_ERR_TRAVERSAL, 'list: tiny traversal budget')
      call system_clock(c1, rate)
      call capnp_text_encode(schema, idx, root, text, err, pretty=.false.)
      call system_clock(c2)
      call check_(error, err == CAPNP_OK, 'list: encode')
      call check_(error, subquad(elapsed, clock_span(c1, c2, rate)), &
                  'list: encode grows sub-quadratically')
      call check_(error, index(text, '(xs = [0, 0, 0,') == 1, 'list: prefix')
      call check_(error, index(text, '1') == 0, 'list: only zeros')
      call spit(tmp//'/big.txt', text, err)
      call check_(error, sh('capnp encode '//tmp//'/intlist.capnp IntList < ' &
         //tmp//'/big.txt > '//tmp//'/big.bin'), 'list: capnp accepts the text')
      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end subroutine run_list_linear

   subroutine run_canonical_release(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root, cap
      integer(int8), allocatable :: bytes(:)
      integer :: err
      call capnp_message_init_builder(msg, err)
      root = capnp_new_struct(msg, 0, 1, err)
      call check_(error, err == CAPNP_OK, 'canon: struct')
      call capnp_set_root(msg, root, err)
      cap = capnp_ptr_t()
      cap%kind = CAPNP_PK_CAP
      cap%capidx = 1_int64
      cap%msg => msg
      call capnp_setp(root, 0, cap, err)
      call check_(error, err == CAPNP_OK, 'canon: set capability')
      call capnp_canonicalize(msg, bytes, err)
      call check_(error, err == CAPNP_ERR_KIND, 'canon: capability has no canonical form')
      call capnp_message_free(msg)
      call capnp_message_init_builder(msg, err)
      root = capnp_new_struct(msg, 1, 0, err)
      call capnp_set_root(msg, root, err)
      call capnp_set_i32(root, 0_int64, 7_int32, err)
      call capnp_canonicalize(msg, bytes, err)
      call check_(error, err == CAPNP_OK .and. allocated(bytes) .and. size(bytes) > 0, &
                  'canon: later call still works')
      call capnp_message_free(msg)
      call check_canon_alloc(error)
   end subroutine run_canonical_release

   subroutine run_json_collisions(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root
      character(len=:), allocatable :: src, text
      integer(int8), allocatable :: bytes(:)
      integer :: err, idx
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp)) then
         call test_failed(error, 'mkdir')
         return
      end if
      src = clash_schema()
      call spit(tmp//'/clash.capnp', src, err)
      if (.not. sh('INC=$(dirname $(dirname $(command -v capnp)))/include' &
         //' && capnp compile -I"$INC" -o- '//tmp//'/clash.capnp > '//tmp//'/clash.cgr' &
         //' && printf "()\n" | capnp encode -I"$INC" '//tmp//'/clash.capnp NameClash > ' &
         //tmp//'/name.bin' &
         //' && ! capnp convert -I"$INC" binary:json '//tmp//'/clash.capnp NameClash < ' &
         //tmp//'/name.bin >/dev/null 2>&1' &
         //' && printf "()\n" | capnp encode -I"$INC" '//tmp//'/clash.capnp FlatClash > ' &
         //tmp//'/flat.bin' &
         //' && ! capnp convert -I"$INC" binary:json '//tmp//'/clash.capnp FlatClash < ' &
         //tmp//'/flat.bin >/dev/null 2>&1' &
         //' && printf "()\n" | capnp encode -I"$INC" '//tmp//'/clash.capnp DiscClash > ' &
         //tmp//'/disc.bin' &
         //' && ! capnp convert -I"$INC" binary:json '//tmp//'/clash.capnp DiscClash < ' &
         //tmp//'/disc.bin >/dev/null 2>&1')) then
         call test_failed(error, 'capnp did not reject a JSON name collision')
         return
      end if
      call capnp_read_file(tmp//'/clash.cgr', bytes, err)
      call capnp_dyn_load(schema, bytes, err)
      call check_(error, err == CAPNP_OK, 'json: clash schema')
      call capnp_read_file(tmp//'/name.bin', bytes, err)
      call capnp_deserialize_bytes(bytes, msg, err)
      root = capnp_root(msg, err)
      idx = capnp_dyn_find(schema, 'NameClash', err)
      call capnp_json_encode(schema, idx, root, text, err, pretty=.false.)
      call check_(error, err /= CAPNP_OK, 'json: $Json.name collision')
      call capnp_message_free(msg)
      call capnp_read_file(tmp//'/flat.bin', bytes, err)
      call capnp_deserialize_bytes(bytes, msg, err)
      root = capnp_root(msg, err)
      idx = capnp_dyn_find(schema, 'FlatClash', err)
      call capnp_json_encode(schema, idx, root, text, err, pretty=.false.)
      call check_(error, err /= CAPNP_OK, 'json: flatten collision')
      call capnp_message_free(msg)
      call capnp_read_file(tmp//'/disc.bin', bytes, err)
      call capnp_deserialize_bytes(bytes, msg, err)
      root = capnp_root(msg, err)
      idx = capnp_dyn_find(schema, 'DiscClash', err)
      call capnp_json_encode(schema, idx, root, text, err, pretty=.false.)
      call check_(error, err /= CAPNP_OK, 'json: discriminator collision')
      call capnp_json_decode(schema, idx, '{}', msg, err)
      call check_(error, err /= CAPNP_OK, 'json: collision rejected on decode')
      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end subroutine run_json_collisions

   subroutine run_json_void_missing(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg, decoded
      type(capnp_ptr_t) :: root
      character(len=:), allocatable :: text, ref
      integer(int8), allocatable :: bytes(:), ours(:), cref(:)
      integer :: err, idx
      integer(int64) :: count
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp)) then
         call test_failed(error, 'mkdir')
         return
      end if
      call spit(tmp//'/clash.capnp', clash_schema(), err)
      if (.not. sh('INC=$(dirname $(dirname $(command -v capnp)))/include' &
         //' && capnp compile -I"$INC" -o- '//tmp//'/clash.capnp > '//tmp//'/clash.cgr' &
         //' && printf "()\n" | capnp encode -I"$INC" '//tmp//'/clash.capnp VoidVal > ' &
         //tmp//'/void.bin' &
         //' && capnp convert -I"$INC" binary:json --short '//tmp//'/clash.capnp VoidVal < ' &
         //tmp//'/void.bin > '//tmp//'/void.json' &
         //' && printf ''{"count":3}'' | capnp convert -I"$INC" json:canonical ' &
         //tmp//'/clash.capnp MissDisc > '//tmp//'/miss.can')) then
         call test_failed(error, 'capnp void/missing reference failed')
         return
      end if
      call capnp_read_file(tmp//'/clash.cgr', bytes, err)
      call capnp_dyn_load(schema, bytes, err)
      call capnp_read_file(tmp//'/void.bin', bytes, err)
      call capnp_deserialize_bytes(bytes, msg, err)
      root = capnp_root(msg, err)
      idx = capnp_dyn_find(schema, 'VoidVal', err)
      call capnp_json_encode(schema, idx, root, text, err, pretty=.false.)
      call slurp(tmp//'/void.json', ref, err)
      if (len(ref) > 0) then
         if (ref(len(ref):len(ref)) == achar(10)) ref = ref(1:len(ref) - 1)
      end if
      call check_(error, text == ref .and. index(text, '"payload":null') > 0, &
                  'json: valueName without name keeps the void')
      call capnp_message_free(msg)
      idx = capnp_dyn_find(schema, 'MissDisc', err)
      call capnp_json_decode(schema, idx, '{"count":3}', decoded, err)
      call check_(error, err == CAPNP_OK, 'json: missing discriminator still decodes')
      root = capnp_root(decoded, err)
      count = capnp_dyn_get_int(schema, idx, root, 'count', err)
      call check_(error, err == CAPNP_OK .and. count == 3_int64, 'json: count field kept')
      call capnp_canonicalize(decoded, ours, err)
      call capnp_read_file(tmp//'/miss.can', cref, err)
      if (err == CAPNP_OK .and. allocated(ours) .and. allocated(cref)) then
         call check_(error, same_i8(ours, cref), 'json: missing discriminator matches capnp')
      else
         call check_(error, .false., 'json: missing discriminator matches capnp')
      end if
      call capnp_message_free(decoded)
      call capnp_dyn_free(schema)
   end subroutine run_json_void_missing

   subroutine run_json_surrogate(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: decoded
      type(capnp_ptr_t) :: root
      character(len=:), allocatable :: text
      integer :: err, idx
      integer, allocatable :: got(:)
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      call load_codec(error, schema)
      if (allocated(error)) return
      idx = capnp_dyn_find(schema, 'Sample', err)
      call check_(error, idx > 0, 'surrogate: Sample')
      call capnp_json_decode(schema, idx, '{"display_name":"\uD83D\uDE00"}', decoded, err)
      call check_(error, err == CAPNP_OK, 'surrogate: pair decodes')
      root = capnp_root(decoded, err)
      call capnp_dyn_get_text(schema, idx, root, 'title', text, err)
      call check_(error, err == CAPNP_OK, 'surrogate: title')
      got = codes(text)
      call check_(error, same_int(got, [240, 159, 152, 128]), &
                  'surrogate: grinning face is one scalar')
      call capnp_message_free(decoded)
      call capnp_json_decode(schema, idx, '{"display_name":"\uD83D"}', decoded, err)
      call check_(error, err == CAPNP_OK, 'surrogate: lone high decodes')
      root = capnp_root(decoded, err)
      call capnp_dyn_get_text(schema, idx, root, 'title', text, err)
      got = codes(text)
      call check_(error, err == CAPNP_OK .and. same_int(got, [237, 160, 189]), &
                  'surrogate: lone high is WTF-8')
      call capnp_message_free(decoded)
      call capnp_json_decode(schema, idx, '{"display_name":"\uDE00"}', decoded, err)
      call check_(error, err == CAPNP_OK, 'surrogate: lone low decodes')
      root = capnp_root(decoded, err)
      call capnp_dyn_get_text(schema, idx, root, 'title', text, err)
      got = codes(text)
      call check_(error, err == CAPNP_OK .and. same_int(got, [237, 184, 128]), &
                  'surrogate: lone low is WTF-8')
      call capnp_message_free(decoded)
      call capnp_json_decode(schema, idx, '{"display_name":"\uD800\uD800\uDC00"}', decoded, err)
      call check_(error, err == CAPNP_OK, 'surrogate: high high low decodes')
      root = capnp_root(decoded, err)
      call capnp_dyn_get_text(schema, idx, root, 'title', text, err)
      got = codes(text)
      call check_(error, err == CAPNP_OK .and. &
                  same_int(got, [237, 160, 128, 240, 144, 128, 128]), &
                  'surrogate: only the adjacent pair is combined')
      call capnp_message_free(decoded)
      call match_capnp_json(error, schema, idx, '{"display_name":"\uD83D"}', &
                            'surrogate: lone high matches capnp')
      call match_capnp_json(error, schema, idx, '{"display_name":"\u00e9"}', &
                            'surrogate: bmp escape matches capnp')
      call capnp_dyn_free(schema)
   end subroutine run_json_surrogate

   subroutine run_json_exact_int(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: decoded
      type(capnp_ptr_t) :: root, scores
      integer(int64) :: v
      integer :: err, idx
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      call load_codec(error, schema)
      if (allocated(error)) return
      idx = capnp_dyn_find(schema, 'Sample', err)
      call check_(error, idx > 0, 'exact: Sample')
      call read_int(error, schema, idx, '{"big":9007199254740993}', 'big', &
                    9007199254740993_int64, 'exact: 2^53+1')
      call read_int(error, schema, idx, '{"big":-9007199254740993}', 'big', &
                    -9007199254740993_int64, 'exact: -(2^53+1)')
      call read_int(error, schema, idx, '{"ubig":18446744073709551615}', 'ubig', &
                    -1_int64, 'exact: uint64 max')
      call read_int(error, schema, idx, '{"ubig":"18446744073709551615"}', 'ubig', &
                    -1_int64, 'exact: quoted uint64 max value')
      call read_int(error, schema, idx, '{"ubig":9223372036854775808}', 'ubig', &
                    int(z'8000000000000000', int64), 'exact: int64 max plus one')
      call read_int(error, schema, idx, '{"ubig":"9223372036854775808"}', 'ubig', &
                    int(z'8000000000000000', int64), 'exact: quoted int64 max plus one')
      call read_int(error, schema, idx, '{"big":"-9223372036854775808"}', 'big', &
                    int(z'8000000000000000', int64), 'exact: int64 min value')
      call read_int(error, schema, idx, '{"big":-9223372036854775808}', 'big', &
                    int(z'8000000000000000', int64), 'exact: unquoted int64 min')
      call read_int(error, schema, idx, '{"big":-9223372036854775809}', 'big', &
                    int(z'8000000000000000', int64), 'exact: unquoted int64 min minus one')
      call read_int(error, schema, idx, '{"ubig":" -1"}', 'ubig', -1_int64, &
                    'exact: quoted space minus one')
      call read_int(error, schema, idx, '{"big":" 42"}', 'big', 42_int64, &
                    'exact: quoted leading space')
      call read_int(error, schema, idx, '{"big":"+42"}', 'big', 42_int64, &
                    'exact: quoted plus')
      call read_int(error, schema, idx, '{"ubig":" -0"}', 'ubig', 0_int64, &
                    'exact: quoted space minus zero')
      call read_int(error, schema, idx, '{"ubig":-0}', 'ubig', 0_int64, &
                    'exact: numeric minus zero')
      call read_int(error, schema, idx, '{"big":1.0}', 'big', 1_int64, 'exact: 1.0')
      call read_int(error, schema, idx, '{"big":1e2}', 'big', 100_int64, 'exact: 1e2')
      call capnp_json_decode(schema, idx, '{"scores":[9007199254740993]}', decoded, err)
      call check_(error, err == CAPNP_OK, 'exact: list decodes')
      root = capnp_root(decoded, err)
      scores = capnp_dyn_getp(schema, idx, root, 'scores', err)
      v = 0_int64
      if (err == CAPNP_OK) v = capnp_list_get_i64(scores, 0_int64, err)
      call check_(error, err == CAPNP_OK .and. v == 9007199254740993_int64, &
                  'exact: list element')
      call capnp_message_free(decoded)
      call reject_json(error, schema, idx, '{"big":"+0x10"}', 'exact: reject +0x10')
      call reject_json(error, schema, idx, '{"ubig":"-0"}', 'exact: reject quoted -0')
      call reject_json(error, schema, idx, '{"big":1.5}', 'exact: reject 1.5')
      call reject_json(error, schema, idx, '{"where":{"zip":65536}}', 'exact: reject uint16')
      call reject_json(error, schema, idx, '{"big":9223372036854775808}', &
                       'exact: reject int64 overflow')
      call reject_json(error, schema, idx, '{"big":"-9223372036854775809"}', &
                       'exact: reject int64 min minus one')
      call reject_json(error, schema, idx, '{"ubig":18446744073709551616}', &
                       'exact: reject uint64 max plus one')
      call reject_json(error, schema, idx, '{"ubig":"-1"}', &
                       'exact: reject uint64 minus one')
      call match_capnp_json(error, schema, idx, '{"big":"9007199254740993"}', &
                            'exact: quoted 2^53+1')
      call match_capnp_json(error, schema, idx, '{"ubig":"18446744073709551615"}', &
                            'exact: quoted uint64 max')
      call match_capnp_json(error, schema, idx, '{"big":"0777"}', 'exact: quoted 0777')
      call match_capnp_json(error, schema, idx, '{"big":"0x10"}', 'exact: quoted hex')
      call match_capnp_json(error, schema, idx, '{"big":"-0x10"}', 'exact: quoted neg hex')
      call match_capnp_json(error, schema, idx, '{"big":"-9223372036854775808"}', &
                            'exact: quoted int64 min')
      call match_capnp_json(error, schema, idx, '{"ubig":9223372036854775808}', &
                            'exact: uint64 2^63')
      call match_capnp_json(error, schema, idx, '{"big":1e2}', 'exact: 1e2 matches capnp')
      call capnp_dyn_free(schema)
   end subroutine run_json_exact_int

   subroutine run_json_null_disc(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg, decoded
      type(capnp_ptr_t) :: root
      integer(int8), allocatable :: bytes(:)
      character(len=:), allocatable :: text
      integer(int64) :: seq, tail
      integer :: err, uidx, tidx, disc
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      call load_codec(error, schema)
      if (allocated(error)) return
      uidx = capnp_dyn_find(schema, 'UnionDefaults', err)
      tidx = capnp_dyn_find(schema, 'TaggedEvent', err)
      call check_(error, uidx > 0 .and. tidx > 0, 'null-disc: nodes')
      do disc = 0, 1
         call capnp_message_init_builder(msg, err)
         root = capnp_dyn_new_root(schema, uidx, msg, err)
         call capnp_set_i32(root, 0_int64, 11_int32, err)
         call capnp_set_which(root, 2, disc, err)
         call capnp_set_i32(root, 8_int64, 22_int32, err)
         call capnp_serialize_bytes(msg, bytes, err)
         call capnp_write_file(tmp//'/nd.bin', bytes, err)
         if (.not. sh('INC=$(dirname $(dirname $(command -v capnp)))/include' &
            //' && capnp convert -I"$INC" binary:json --short schema/codec.capnp' &
            //' UnionDefaults < '//tmp//'/nd.bin > '//tmp//'/nd.json')) then
            call test_failed(error, 'null-disc: capnp convert')
            call capnp_message_free(msg)
            call capnp_dyn_free(schema)
            return
         end if
         call capnp_json_encode(schema, uidx, root, text, err, pretty=.false.)
         call check_(error, err == CAPNP_OK, 'null-disc: encode')
         call check_file_text(error, text, tmp//'/nd.json', 'null-disc: matches capnp')
         if (disc == 0) then
            call check_(error, index(text, '"u0ptr":""') > 0 .and. index(text, '"u0ptr":null') == 0, &
                        'null-disc: zero arm is an empty string')
         else
            call check_(error, index(text, '"u1ptr":"fallback"') > 0, &
                        'null-disc: null arm keeps its default')
         end if
         call capnp_message_free(msg)
      end do
      call capnp_json_decode(schema, tidx, &
                             '{"seq":5,"kind":"nope","tail":9,"count":3}', decoded, err)
      call check_(error, err == CAPNP_OK, 'null-disc: unknown tag decodes')
      root = capnp_root(decoded, err)
      seq = capnp_dyn_get_int(schema, tidx, root, 'seq', err)
      call check_(error, err == CAPNP_OK .and. seq == 5_int64, 'null-disc: seq kept')
      tail = capnp_dyn_get_int(schema, tidx, root, 'tail', err)
      call check_(error, err == CAPNP_OK .and. tail == 9_int64, 'null-disc: tail kept')
      seq = capnp_dyn_get_int(schema, tidx, root, 'count', err)
      call check_(error, err == CAPNP_OK .and. seq == 3_int64, &
                  'null-disc: unknown tag still selects count')
      call capnp_json_decode(schema, tidx, '{"kind":1}', decoded, err)
      call check_(error, err /= CAPNP_OK, 'null-disc: non-string tag is rejected')
      call capnp_message_free(decoded)
      call capnp_dyn_free(schema)
   end subroutine run_json_null_disc

   !> Every case is capnp 1.4.0 json:canonical versus our decoder.
   subroutine run_json_union_diff(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      integer :: n
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      call load_codec(error, schema)
      if (allocated(error)) return
      n = 0
      call udiff(error, schema, n, 'TaggedEvent', '{"kind":"future","count":7}')
      call udiff(error, schema, n, 'TaggedEvent', &
         '{"kind":"future","message":"hi","count":2,"seq":3,"tail":9}')
      call udiff(error, schema, n, 'TaggedEvent', '{"kind":"","count":2}')
      call udiff(error, schema, n, 'TaggedEvent', '{"kind":"Count","count":2}')
      call udiff(error, schema, n, 'TaggedEvent', '{"kind":"idle","count":3}')
      call udiff(error, schema, n, 'TaggedEvent', '{"count":3,"kind":"idle"}')
      call udiff(error, schema, n, 'TaggedEvent', '{"kind":"note","message":5}')
      call udiff(error, schema, n, 'TaggedEvent', '{"kind":"message","message":5}')
      call udiff(error, schema, n, 'TaggedEvent', '{"kind":"message","message":"hi"}')
      call udiff(error, schema, n, 'TaggedEvent', '{"message":"hi"}')
      call udiff(error, schema, n, 'TaggedEvent', '{"count":287454020,"idle":null}')
      call udiff(error, schema, n, 'TaggedEvent', '{"idle":5}')
      call udiff(error, schema, n, 'TaggedEvent', '{"kind":"idle"}')
      call udiff(error, schema, n, 'TaggedEvent', '{"kind":"count","kind":"idle"}')
      call udiff(error, schema, n, 'TaggedEvent', '{"kind":"idle","kind":"count","count":4}')
      call udiff(error, schema, n, 'TaggedEvent', '{"kind":1}')
      call udiff(error, schema, n, 'TaggedEvent', '{"kind":null}')
      call udiff(error, schema, n, 'TaggedEvent', '{"seq":1,"tail":2}')
      call udiff(error, schema, n, 'TaggedEvent', '{"kind":"future"}')
      call udiff(error, schema, n, 'TaggedEvent', '{"count":3,"kind":"future"}')
      call udiff(error, schema, n, 'TaggedEvent', '{"kind":"moved","city":"Oslo","code":5}')
      call udiff(error, schema, n, 'TaggedEvent', '{"city":"Oslo","kind":"moved","code":5}')
      call udiff(error, schema, n, 'TaggedEvent', '{"kind":"idle","city":"Oslo"}')
      call udiff(error, schema, n, 'TaggedEvent', '{"city":"Oslo"}')
      call udiff(error, schema, n, 'TaggedEvent', '{"kind":"count","count":8,"count":9}')
      call udiff(error, schema, n, 'TaggedEvent', '{"kind":"future","kind":"count","count":4}')
      call udiff(error, schema, n, 'TaggedEvent', '{"kind":"count","kind":"future","count":4}')
      call udiff(error, schema, n, 'TaggedEvent', '{"kind":"future","kind":"nope"}')
      call udiff(error, schema, n, 'TaggedEvent', '{"kind":"idle","kind":"idle"}')
      call udiff(error, schema, n, 'TaggedEvent', '{"kind":"moved","code":5,"city":"Oslo"}')
      call udiff(error, schema, n, 'TaggedEvent', '{"unknown":1,"count":2}')
      call udiff(error, schema, n, 'TaggedEvent', '{"kind":"count","count":"6"}')
      call udiff(error, schema, n, 'TaggedEvent', '{"kind":"message"}')
      call udiff(error, schema, n, 'TaggedEvent', '{"kind":"count","message":"x","count":4}')
      call udiff(error, schema, n, 'TaggedEvent', '{"city":"A","kind":"moved","city":"B"}')
      call udiff(error, schema, n, 'TaggedEvent', '{"kind":"moved","city":"A","city":"B"}')
      call udiff(error, schema, n, 'TaggedEvent', '{"count":7,"kind":"count"}')
      call udiff(error, schema, n, 'TaggedEvent', '{"message":"hi","kind":"message"}')
      call udiff(error, schema, n, 'TaggedEvent', '{"note":"x","kind":"note"}')
      call udiff(error, schema, n, 'ValueEvent', '{"type":"empty","payload":1}')
      call udiff(error, schema, n, 'ValueEvent', '{"payload":4,"type":"num","payload":5}')
      call udiff(error, schema, n, 'ValueEvent', '{"type":"num","payload":4,"payload":5}')
      call udiff(error, schema, n, 'ValueEvent', '{"type":"num","payload":5}')
      call udiff(error, schema, n, 'ValueEvent', '{"payload":5}')
      call udiff(error, schema, n, 'ValueEvent', '{"type":"text_tag","payload":"hi"}')
      call udiff(error, schema, n, 'ValueEvent', '{"type":"renamed","payload":"hi"}')
      call udiff(error, schema, n, 'ValueEvent', '{"num":4}')
      call udiff(error, schema, n, 'ValueEvent', '{"type":"empty","payload":null}')
      call udiff(error, schema, n, 'ValueEvent', '{"type":"loc","payload":{"city":"Lima","code":1}}')
      call udiff(error, schema, n, 'ValueEvent', '{"type":1}')
      call udiff(error, schema, n, 'ValueEvent', '{"type":"empty","payload":{"a":1}}')
      call udiff(error, schema, n, 'ValueEvent', '{"type":"num","payload":"4"}')
      call udiff(error, schema, n, 'ValueEvent', '{"type":"loc","payload":1}')
      call udiff(error, schema, n, 'ValueEvent', '{"payload":"A","type":"text_tag","payload":"B"}')
      call udiff(error, schema, n, 'ValueEvent', '{"type":"text_tag","payload":"A","payload":"B"}')
      call udiff(error, schema, n, 'MultiUnionDoc', '{"mode":"future","fast":4}')
      call udiff(error, schema, n, 'MultiUnionDoc', '{"mode":"custom","fast":3}')
      call udiff(error, schema, n, 'MultiUnionDoc', '{"mode":"fast","c_city":"x"}')
      call udiff(error, schema, n, 'MultiUnionDoc', '{"mode":"custom","c_city":"Riga","c_code":1}')
      call udiff(error, schema, n, 'MultiUnionDoc', '{"c_city":"Riga","mode":"custom","c_code":1}')
      call udiff(error, schema, n, 'MultiUnionDoc', '{"fast":3,"c_city":"x"}')
      call udiff(error, schema, n, 'MultiUnionDoc', '{"fast":3,"mode":"custom"}')
      call udiff(error, schema, n, 'MultiUnionDoc', '{"mode":"custom","c_city":"x","fast":3}')
      call udiff(error, schema, n, 'MultiUnionDoc', '{"mode":"custom","fast":3,"c_city":"x"}')
      call udiff(error, schema, n, 'MultiUnionDoc', '{"ext":{"tag":"off","body":"r"}}')
      call udiff(error, schema, n, 'MultiUnionDoc', '{"ext":{"body":"A","tag":"on","body":"B"}}')
      call udiff(error, schema, n, 'MultiUnionDoc', '{"fast":3,"mode":"fast"}')
      call udiff(error, schema, n, 'GroupArm', '{"box":{"x":4,"y":5},"kind":"box"}')
      call udiff(error, schema, n, 'GroupArm', '{"kind":"box","box":{"x":4,"y":5}}')
      call udiff(error, schema, n, 'GroupArm', '{"n":5,"box":{}}')
      call udiff(error, schema, n, 'GroupUnion', '{"gu":{"q":5},"kind":"gu"}')
      call udiff(error, schema, n, 'UnionOfMid', &
         '{"kind":"mid","midId":7,"pl_city":"Oslo","pl_code":500}')
      call udiff(error, schema, n, 'UnionOfMid', &
         '{"pl_city":"Oslo","kind":"mid","midId":7,"pl_code":500}')
      call udiff(error, schema, n, 'UnionOfGroupFlat', &
         '{"kind":"g","pl_city":"Oslo","pl_code":500}')
      call udiff(error, schema, n, 'UnionOfGroupFlat', &
         '{"pl_code":500,"kind":"g","pl_city":"Oslo"}')
      call udiff(error, schema, n, 'NestedFlatDisc', &
         '{"outerKind":"g","innerKind":"a","innerVal":42}')
      call udiff(error, schema, n, 'NestedFlatDisc', &
         '{"outerKind":"g","innerKind":"b","innerVal":"hi"}')
      call udiff(error, schema, n, 'NestedFlatDisc', &
         '{"innerKind":"a","outerKind":"g","innerVal":5}')
      call udiff(error, schema, n, 'NestedFlatDisc', &
         '{"innerKind":"b","outerKind":"g","innerVal":"x"}')
      call udiff(error, schema, n, 'NestedFlatDisc', &
         '{"outerKind":"g","innerVal":{"z":{}}}')
      call udiff(error, schema, n, 'NestedFlatDisc', &
         '{"outerKind":"g","innerKind":"a","innerKind":"b"}')
      call udiff(error, schema, n, 'NestedFlatStructDisc', &
         '{"outerKind":"g","innerKind":"a","innerVal":42}')
      call udiff(error, schema, n, 'NestedFlatStructDisc', &
         '{"innerVal":"hi","outerKind":"g","innerKind":"b"}')
      call udiff(error, schema, n, 'NestedFlatStructDisc', &
         '{"outerKind":"g","innerKind":"a","innerKind":"b"}')
      call udiff(error, schema, n, 'VoidPayload', '{"v":123}')
      call udiff(error, schema, n, 'VoidPayload', '{"v":{"a":1}}')
      call udiff(error, schema, n, 'VoidPayload', '{"vls":[1,"a"]}')
      call udiff(error, schema, n, 'VoidPayload', '{"v":null,"n":3}')
      call tdiff(error, schema, n, 'GroupArm', '(n = 5, box = (y = 1))')
      call tdiff(error, schema, n, 'GroupArm', '(n = 5, box = ())')
      call tdiff(error, schema, n, 'GroupUnion', '(n = 5, gu = (p = 1))')
      call check_(error, n == 91, 'union-diff: case count')
      call capnp_dyn_free(schema)
   end subroutine run_json_union_diff

   subroutine udiff(error, schema, n, tname, js)
      type(error_type), allocatable, intent(inout) :: error
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(inout) :: n
      character(len=*), intent(in) :: tname, js
      type(capnp_message_t), target :: decoded
      integer(int8), allocatable :: ours(:), ref(:)
      integer :: err, idx
      logical :: cpp_ok, ours_ok
      n = n + 1
      if (allocated(error)) return
      idx = capnp_dyn_find(schema, tname, err)
      if (idx <= 0) then
         call test_failed(error, 'union-diff: missing '//tname)
         return
      end if
      call spit(tmp//'/uj.json', js, err)
      cpp_ok = sh('INC=$(dirname $(dirname $(command -v capnp)))/include' &
         //' && capnp convert -I"$INC" json:canonical schema/codec.capnp '//tname &
         //' < '//tmp//'/uj.json > '//tmp//'/uj.can 2>'//tmp//'/uj.err')
      call capnp_json_decode(schema, idx, js, decoded, err)
      ours_ok = err == CAPNP_OK
      if (cpp_ok .neqv. ours_ok) then
         call test_failed(error, 'union-diff: status '//tname//' '//js)
         call capnp_message_free(decoded)
         return
      end if
      if (.not. ours_ok) then
         call capnp_message_free(decoded)
         return
      end if
      call capnp_canonicalize(decoded, ours, err)
      call capnp_read_file(tmp//'/uj.can', ref, err)
      if (err /= CAPNP_OK .or. .not. allocated(ours) .or. .not. allocated(ref)) then
         call test_failed(error, 'union-diff: bytes '//tname//' '//js)
      else if (.not. same_i8(ours, ref)) then
         call test_failed(error, 'union-diff: bytes '//tname//' '//js)
      end if
      call capnp_message_free(decoded)
   end subroutine udiff

   subroutine tdiff(error, schema, n, tname, text)
      type(error_type), allocatable, intent(inout) :: error
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(inout) :: n
      character(len=*), intent(in) :: tname, text
      type(capnp_message_t), target :: decoded
      integer(int8), allocatable :: ours(:), ref(:)
      integer :: err, idx
      logical :: cpp_ok, ours_ok
      n = n + 1
      if (allocated(error)) return
      idx = capnp_dyn_find(schema, tname, err)
      if (idx <= 0) then
         call test_failed(error, 'union-diff: missing '//tname)
         return
      end if
      call spit(tmp//'/ut.txt', text, err)
      cpp_ok = sh('capnp convert text:canonical schema/codec.capnp '//tname &
         //' < '//tmp//'/ut.txt > '//tmp//'/ut.can 2>'//tmp//'/ut.err')
      call capnp_text_decode(schema, idx, text, decoded, err)
      ours_ok = err == CAPNP_OK
      if (cpp_ok .neqv. ours_ok) then
         call test_failed(error, 'union-diff: text status '//tname//' '//text)
         call capnp_message_free(decoded)
         return
      end if
      if (.not. ours_ok) then
         call capnp_message_free(decoded)
         return
      end if
      call capnp_canonicalize(decoded, ours, err)
      call capnp_read_file(tmp//'/ut.can', ref, err)
      if (err /= CAPNP_OK .or. .not. allocated(ours) .or. .not. allocated(ref)) then
         call test_failed(error, 'union-diff: text bytes '//tname//' '//text)
      else if (.not. same_i8(ours, ref)) then
         call test_failed(error, 'union-diff: text bytes '//tname//' '//text)
      end if
      call capnp_message_free(decoded)
   end subroutine tdiff

   subroutine check_file_text(error, text, path, label)
      type(error_type), allocatable, intent(inout) :: error
      character(len=*), intent(in) :: text, path, label
      character(len=:), allocatable :: ref
      integer :: err
      if (allocated(error)) return
      call slurp(path, ref, err)
      if (err /= CAPNP_OK) then
         call test_failed(error, label//': read')
         return
      end if
      if (len(ref) > 0) then
         if (ref(len(ref):len(ref)) == achar(10)) ref = ref(1:len(ref) - 1)
      end if
      call check_(error, text == ref, label)
   end subroutine check_file_text

   subroutine read_int(error, schema, idx, js, fname, expect, label)
      type(error_type), allocatable, intent(inout) :: error
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      character(len=*), intent(in) :: js, fname, label
      integer(int64), intent(in) :: expect
      type(capnp_message_t), target :: decoded
      type(capnp_ptr_t) :: root
      integer(int64) :: v
      integer :: err
      if (allocated(error)) return
      call capnp_json_decode(schema, idx, js, decoded, err)
      call check_(error, err == CAPNP_OK, label//': decode')
      root = capnp_root(decoded, err)
      v = capnp_dyn_get_int(schema, idx, root, fname, err)
      call check_(error, err == CAPNP_OK .and. v == expect, label)
      call capnp_message_free(decoded)
   end subroutine read_int

   subroutine reject_json(error, schema, idx, js, label)
      type(error_type), allocatable, intent(inout) :: error
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      character(len=*), intent(in) :: js, label
      type(capnp_message_t), target :: decoded
      integer :: err
      if (allocated(error)) return
      call capnp_json_decode(schema, idx, js, decoded, err)
      call check_(error, err /= CAPNP_OK, label)
      call capnp_message_free(decoded)
      call spit(tmp//'/bad.json', js, err)
      if (.not. sh('INC=$(dirname $(dirname $(command -v capnp)))/include' &
         //' && capnp convert -I"$INC" json:canonical schema/codec.capnp Sample < ' &
         //tmp//'/bad.json > '//tmp//'/bad.can 2>/dev/null')) then
         return
      end if
      call test_failed(error, label//': capnp accepted it')
   end subroutine reject_json

   subroutine run_text_leniency(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root
      integer(int8), allocatable :: bytes(:)
      integer :: err, idx
      integer(int64) :: count
      real(real64) :: ratio
      call capnp_read_file('test/fixtures/kitchen.cgr.bin', bytes, err)
      if (err /= CAPNP_OK) then
         call skip_test(error, 'kitchen cgr missing')
         return
      end if
      call capnp_dyn_load(schema, bytes, err)
      idx = capnp_dyn_find(schema, 'Sink', err)
      call capnp_text_decode(schema, idx, '(count = +10)', msg, err)
      call check_(error, err /= CAPNP_OK, 'text: reject unary plus')
      call capnp_text_decode(schema, idx, '(ratio = .5)', msg, err)
      call check_(error, err /= CAPNP_OK, 'text: reject leading dot')
      call capnp_text_decode(schema, idx, '(state = 1)', msg, err)
      call check_(error, err /= CAPNP_OK, 'text: reject numeric enum')
      call capnp_text_decode(schema, idx, '(state = (1))', msg, err)
      call check_(error, err /= CAPNP_OK, 'text: reject parenthesised ordinal')
      call capnp_text_decode(schema, idx, '(count = 010)', msg, err)
      call check_(error, err == CAPNP_OK, 'text: octal still accepted')
      root = capnp_root(msg, err)
      count = capnp_dyn_get_int(schema, idx, root, 'count', err)
      call check_(error, err == CAPNP_OK .and. count == 8_int64, 'text: 010 is eight')
      call capnp_message_free(msg)
      call capnp_text_decode(schema, idx, '(ratio = 5., state = (busy))', msg, err)
      call check_(error, err == CAPNP_OK, 'text: trailing dot and (name)')
      root = capnp_root(msg, err)
      ratio = capnp_dyn_get_f64(schema, idx, root, 'ratio', err)
      call check_(error, err == CAPNP_OK .and. abs(ratio - 5.0_real64) < 1.0e-12_real64, &
                  'text: 5. is five')
      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end subroutine run_text_leniency

   subroutine run_unloaded(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: p
      type(capnp_brand_env_t) :: env
      character(len=:), allocatable :: text
      integer :: err
      p = capnp_ptr_t()
      call check_text_bounds(error)
      if (allocated(error)) return
      call capnp_text_encode(schema, 1, p, text, err)
      call check_(error, err == CAPNP_ERR_ARG, 'guard: text encode unloaded schema')
      call capnp_text_decode(schema, 1, '()', msg, err)
      call check_(error, err == CAPNP_ERR_ARG, 'guard: text decode unloaded schema')
      call capnp_json_encode(schema, 1, p, text, err)
      call check_(error, err == CAPNP_ERR_ARG, 'guard: json encode unloaded schema')
      call capnp_json_decode(schema, 1, '{}', msg, err)
      call check_(error, err == CAPNP_ERR_ARG, 'guard: json decode unloaded schema')
      call capnp_text_encode_enum(schema, 1_int64, 0, text, err)
      call check_(error, err == CAPNP_ERR_ARG, 'guard: text enum unloaded schema')
      call capnp_text_encode_value(schema, p, p, text, err)
      call check_(error, err == CAPNP_ERR_ARG, 'guard: text value unloaded schema')
      call capnp_text_encode_list(schema, p, p, env, text, err)
      call check_(error, err == CAPNP_ERR_ARG, 'guard: text list unloaded schema')
   end subroutine run_unloaded

   subroutine run_long_arg(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=5000) :: opt
      integer :: i
      if (.not. sh('mkdir -p '//tmp//' && '//cap_setup())) then
         call skip_test(error, 'capnpf binary not built')
         return
      end if
      opt = '--'
      do i = 3, 4994
         opt(i:i) = 'y'
      end do
      opt(4995:5000) = 'MARKER'
      call spit(tmp//'/longarg.txt', trim(opt), err_ignored=error)
      if (allocated(error)) return
      call check_(error, sh(cap_setup()//' && opt=$(cat '//tmp//'/longarg.txt)' &
         //' && "$CAP" "$opt" >'//tmp//'/long.out 2>'//tmp//'/long.err; test $? -ne 0' &
         //' && grep -q MARKER '//tmp//'/long.err'), 'args: 5000-character option is intact')
   end subroutine run_long_arg

   subroutine spit(path, s, err, err_ignored)
      character(len=*), intent(in) :: path, s
      integer, intent(out), optional :: err
      type(error_type), allocatable, intent(inout), optional :: err_ignored
      integer(int8), allocatable :: b(:)
      integer :: i, local
      allocate (b(0:len(s) - 1))
      do i = 1, len(s)
         b(i - 1) = i8_of(iachar(s(i:i)))
      end do
      call capnp_write_file(path, b, local)
      if (present(err)) err = local
      if (present(err_ignored)) then
         if (local /= CAPNP_OK) call test_failed(err_ignored, 'write '//path)
      end if
   end subroutine spit

   !> A decoy profile directory must not hide the binary. The runner's
   !> path is used when it is set. With that variable unset, the same
   !> executable is chosen from this process's build directory, so a
   !> release or sanitized run does not fall back to another profile.
   !> The decoy name includes the shell pid so two suites do not share it.
   subroutine run_cli_locate(error)
      type(error_type), allocatable, intent(out) :: error
      if (.not. sh('decoy=build/gfortran_decoy.$$' &
         //' && rm -rf "$decoy" build/gfortran_decoy' &
         //' && mkdir -p "$decoy/app"' &
         //' && printf "%s\n" "#!/bin/sh" "echo decoy" > "$decoy/app/capnpf"' &
         //' && chmod +x "$decoy/app/capnpf"' &
         //' && ok=0; if '//capnpf_setup() &
         //' && test "$CAP" = "$CAPNPF"' &
         //' && "$CAP" --version >/dev/null' &
         //' && saved=$CAPNPF && unset CAPNPF && '//capnpf_setup() &
         //' && test "$CAP" = "$saved"' &
         //' && case "$CAP" in *gfortran_decoy*) false;; *) true;; esac' &
         //' && "$CAP" --version >/dev/null; then ok=1; fi' &
         //'; rm -rf "$decoy"; test "$ok" = 1 && test ! -d build/gfortran_decoy')) then
         call test_failed(error, 'cli locate failed')
      end if
   end subroutine run_cli_locate

   !> The schema plugin must release the command argument, the request
   !> buffer, the file name, and the message on the success path and on
   !> every error path. A sanitized build exits nonzero if any remain.
   subroutine run_plugin_release(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=:), allocatable :: ptmp
      ptmp = tmp//'/plugin'
      if (.not. sh('mkdir -p '//ptmp &
         //' && '//capnpc_setup() &
         //' && cp test/fixtures/addressbook.cgr.bin '//ptmp//'/req.bin' &
         //' && (cd '//ptmp//' && "$CAP" req.bin)' &
         //' && test -s '//ptmp//'/addressbook_capnp.f90' &
         //' && export ASAN_OPTIONS="${ASAN_OPTIONS:+$ASAN_OPTIONS:}exitcode=23"' &
         //' && rc=0; "$CAP" '//ptmp//'/missing.bin >/dev/null 2>'//ptmp//'/miss.err || rc=$?' &
         //'; test "$rc" -eq 1 && ! grep -q LeakSanitizer '//ptmp//'/miss.err' &
         //' && ! grep -q "ERROR STOP" '//ptmp//'/miss.err' &
         //' && rc=0; printf "" | "$CAP" >/dev/null 2>'//ptmp//'/empty.err || rc=$?' &
         //'; test "$rc" -eq 1 && ! grep -q LeakSanitizer '//ptmp//'/empty.err' &
         //' && ! grep -q "ERROR STOP" '//ptmp//'/empty.err')) then
         call test_failed(error, 'plugin release paths failed')
         return
      end if
   end subroutine run_plugin_release

   subroutine run_schema_pipe(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int8), allocatable :: got(:), ref(:)
      integer :: err
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp//' && echo kept > '//tmp//'/kept' &
         //' && '//cap_setup() &
         //' && python3 - <<''PY'' &&'//achar(10)// &
         'import os'//achar(10)// &
         'from pathlib import Path'//achar(10)// &
         'pid = os.getpid()'//achar(10)// &
         'Path("'//tmp//'/pid").write_text(str(pid))'//achar(10)// &
         'kept = Path("'//tmp//'/kept")'//achar(10)// &
         'for i in range(1, 81):'//achar(10)// &
         '    p = Path("/tmp/capnpf-%d.cgr" % (pid + i))'//achar(10)// &
         '    if p.is_symlink() or p.exists():'//achar(10)// &
         '        p.unlink()'//achar(10)// &
         '    p.symlink_to(kept)'//achar(10)// &
         'PY'//achar(10)// &
         'ok=0' &
         //' && printf "(x = 1, y = 2, z = 3)\n" | "$CAP" encode' &
         //' schema/common.capnp Vec3 > '//tmp//'/pipe.bin' &
         //' && test "$(cat '//tmp//'/kept)" = kept && ok=1'//achar(10)// &
         'python3 - <<''PY'''//achar(10)// &
         'from pathlib import Path'//achar(10)// &
         'pid = int(Path("'//tmp//'/pid").read_text())'//achar(10)// &
         'for i in range(1, 81):'//achar(10)// &
         '    p = Path("/tmp/capnpf-%d.cgr" % (pid + i))'//achar(10)// &
         '    if p.is_symlink() or p.exists():'//achar(10)// &
         '        p.unlink()'//achar(10)// &
         'PY'//achar(10)// &
         'test "$ok" = 1' &
         //' && printf "(x = 1, y = 2, z = 3)\n" | capnp encode' &
         //' schema/common.capnp Vec3 > '//tmp//'/pipe.ref')) then
         call test_failed(error, 'schema pipe encode failed')
         return
      end if
      call capnp_read_file(tmp//'/pipe.bin', got, err)
      call capnp_read_file(tmp//'/pipe.ref', ref, err)
      if (err == CAPNP_OK .and. allocated(got) .and. allocated(ref)) then
         call check_(error, same_i8(got, ref), &
                     'schema: pipe matches capnp and leaves the symlink target')
      else
         call check_(error, .false., 'schema: pipe matches capnp and leaves the symlink target')
      end if
   end subroutine run_schema_pipe

   subroutine run_string_linear(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root
      character(len=:), allocatable :: js, tx, got
      integer(int8), allocatable :: raw(:)
      integer :: err, idx, i, np, nt
      character(len=*), parameter :: jpfx = '{"display_name":"'
      character(len=*), parameter :: tpfx = '(title = '
      integer(int64) :: c1, c2, rate
      real :: elapsed, small
      integer, parameter :: nch = 160000
      integer, parameter :: ntext = 640000
      integer, parameter :: nbytes = 640000
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      call load_codec(error, schema)
      if (allocated(error)) return
      idx = capnp_dyn_find(schema, 'Sample', err)
      call check_(error, idx > 0, 'linear: Sample')
      call capnp_json_decode(schema, idx, '{"display_name":"a\nb"}', msg, err)
      call check_(error, err == CAPNP_OK, 'linear: short escape decodes')
      root = capnp_root(msg, err)
      call capnp_dyn_get_text(schema, idx, root, 'title', got, err)
      call check_(error, err == CAPNP_OK .and. got == 'a'//achar(10)//'b', 'linear: newline kept')
      call capnp_message_free(msg)
      np = len(jpfx)
      allocate (character(len=np + nch + 2) :: js)
      js(1:np) = jpfx
      js(np + 1:np + nch) = repeat('a', nch)
      js(np + nch + 1:np + nch + 1) = '"'
      js(np + nch + 2:np + nch + 2) = '}'
      call system_clock(c1, rate)
      call capnp_json_decode(schema, idx, jpfx//repeat('a', nch/4)//'"}', msg, err)
      call system_clock(c2)
      small = clock_span(c1, c2, rate)
      call capnp_message_free(msg)
      call system_clock(c1, rate)
      call capnp_json_decode(schema, idx, js, msg, err)
      call system_clock(c2)
      elapsed = clock_span(c1, c2, rate)
      call check_(error, err == CAPNP_OK, 'linear: json string decodes')
      root = capnp_root(msg, err)
      call capnp_dyn_get_text(schema, idx, root, 'title', got, err)
      call check_(error, err == CAPNP_OK .and. len(got) == nch .and. got == repeat('a', nch), &
                  'linear: json string bytes')
      call check_(error, subquad(small, elapsed), 'linear: json string grows sub-quadratically')
      call capnp_message_free(msg)
      nt = len(tpfx)
      allocate (character(len=nt + 4*ntext + 1) :: tx)
      tx(1:nt) = tpfx
      do i = 1, ntext
         tx(nt + 4*(i - 1) + 1:nt + 4*i) = '"a" '
      end do
      tx(nt + 4*ntext + 1:nt + 4*ntext + 1) = ')'
      call system_clock(c1, rate)
      call capnp_text_decode(schema, idx, tpfx//repeat('"a" ', ntext/4)//')', msg, err)
      call system_clock(c2)
      small = clock_span(c1, c2, rate)
      call capnp_message_free(msg)
      call system_clock(c1, rate)
      call capnp_text_decode(schema, idx, tx(1:nt + 4*ntext + 1), msg, err)
      call system_clock(c2)
      elapsed = clock_span(c1, c2, rate)
      call check_(error, err == CAPNP_OK, 'linear: adjacent text decodes')
      root = capnp_root(msg, err)
      call capnp_dyn_get_text(schema, idx, root, 'title', got, err)
      call check_(error, err == CAPNP_OK .and. got == repeat('a', ntext), 'linear: adjacent text bytes')
      call check_(error, subquad(small, elapsed), 'linear: adjacent text grows sub-quadratically')
      call capnp_message_free(msg)
      call capnp_message_init_builder(msg, err)
      root = capnp_dyn_new_root(schema, idx, msg, err)
      allocate (raw(0:nbytes - 1))
      raw = 65_int8
      raw(0) = 0_int8
      raw(1) = 1_int8
      raw(2) = -1_int8
      call capnp_dyn_set_data(schema, idx, root, 'hexed', raw, err)
      call check_(error, err == CAPNP_OK, 'linear: set hex data')
      call capnp_dyn_set_data(schema, idx, root, 'hexed', raw(0:nbytes/4 - 1), err)
      call system_clock(c1, rate)
      call capnp_json_encode(schema, idx, root, js, err, pretty=.false.)
      call system_clock(c2)
      small = clock_span(c1, c2, rate)
      call capnp_dyn_set_data(schema, idx, root, 'hexed', raw, err)
      call system_clock(c1, rate)
      call capnp_json_encode(schema, idx, root, js, err, pretty=.false.)
      call system_clock(c2)
      elapsed = clock_span(c1, c2, rate)
      call check_(error, err == CAPNP_OK .and. allocated(js) .and. index(js, '"hexed":"0001ff') > 0 .and. &
                  index(js, repeat('41', 8)) > 0, 'linear: hex prefix')
      call check_(error, subquad(small, elapsed), 'linear: hex encode grows sub-quadratically')
      call capnp_dyn_set_data(schema, idx, root, 'b64', raw(0:nbytes/4 - 1), err)
      call system_clock(c1, rate)
      call capnp_json_encode(schema, idx, root, js, err, pretty=.false.)
      call system_clock(c2)
      small = clock_span(c1, c2, rate)
      call capnp_dyn_set_data(schema, idx, root, 'b64', raw, err)
      call system_clock(c1, rate)
      call capnp_json_encode(schema, idx, root, js, err, pretty=.false.)
      call system_clock(c2)
      elapsed = clock_span(c1, c2, rate)
      call check_(error, err == CAPNP_OK .and. index(js, '"b64":"AAH/') > 0, 'linear: base64 prefix')
      call check_(error, subquad(small, elapsed), 'linear: base64 encode grows sub-quadratically')
      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end subroutine run_string_linear

   subroutine run_f32_text(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      integer :: err, idx, i
      character(len=*), parameter :: sch = &
         '@0x8c1e4a7b2d9056f3;'//achar(10)//'struct F { x @0 :Float32; }'//achar(10)
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp)) then
         call test_failed(error, 'mkdir')
         return
      end if
      call spit(tmp//'/f32.capnp', sch, err)
      if (.not. sh('capnp compile -o- '//tmp//'/f32.capnp > '//tmp//'/f32.cgr')) then
         call test_failed(error, 'f32: compile schema')
         return
      end if
      call load_cgr(error, schema, tmp//'/f32.cgr')
      if (allocated(error)) return
      idx = capnp_dyn_find(schema, 'F', err)
      call check_(error, idx > 0, 'f32: type F')
      do i = 1, 7
         call one_f32(error, schema, idx, f32_bits(i), f32_expect(i))
      end do
      call f32_match_encode(error, schema, idx, '(x = -0)')
      call f32_match_encode(error, schema, idx, '(x = -0x0)')
      call f32_match_encode(error, schema, idx, '(x = -00)')
      call f32_match_encode(error, schema, idx, '(x = 3.4028235677973366e38)')
      call f32_match_encode(error, schema, idx, '(x = -3.4028235677973366e38)')
      call f32_match_encode(error, schema, idx, '(x = 1.1756341303441039e-38)')
      call f32_match_encode(error, schema, idx, '(x = 1.1757738398009971e-38)')
      call f32_match_encode(error, schema, idx, '(x = 1.1760532587147835e-38)')
      call f32_match_encode(error, schema, idx, '(x = -1.176472387085463e-38)')
      call f32_match_encode(error, schema, idx, '(x = -0.0)')
      call f32_match_encode(error, schema, idx, '(x = 4611686843061108735)')
      call f32_match_encode(error, schema, idx, '(x = 9223373686122217471)')
      call f32_match_encode(error, schema, idx, '(x = 0x8000017fffffffff)')
      call capnp_dyn_free(schema)
   end subroutine run_f32_text

   function f32_bits(i) result(b)
      integer, intent(in) :: i
      integer(int32) :: b
      select case (i)
      case (1); b = int(z'00000001', int32)
      case (2); b = int(z'007fffff', int32)
      case (3); b = int(z'00800000', int32)
      case (4); b = int(z'80000000', int32)
      case (5); b = int(z'7f800000', int32)
      case (6); b = int(z'ff800000', int32)
      case default; b = int(z'7fc00000', int32)
      end select
   end function f32_bits

   function f32_expect(i) result(s)
      integer, intent(in) :: i
      character(len=:), allocatable :: s
      select case (i)
      case (1); s = '1.4012985e-45'
      case (2); s = '1.1754942e-38'
      case (3); s = '1.1754944e-38'
      case (4); s = '-0'
      case (5); s = 'inf'
      case (6); s = '-inf'
      case default; s = 'nan'
      end select
   end function f32_expect

   subroutine one_f32(error, schema, idx, bits, expect)
      type(error_type), allocatable, intent(inout) :: error
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      integer(int32), intent(in) :: bits
      character(len=*), intent(in) :: expect
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root
      integer(int8), allocatable :: raw(:)
      character(len=:), allocatable :: text, ref
      integer(int32) :: u, gotb
      integer :: err, i
      real(real64) :: d
      real(real32) :: x32
      if (allocated(error)) return
      allocate (raw(0:23))
      raw = 0_int8
      raw(4) = 2_int8
      raw(12) = 1_int8
      u = bits
      do i = 0, 3
         raw(16 + i) = i8_of(int(iand(u, 255_int32)))
         u = ishft(u, -8)
      end do
      call capnp_write_file(tmp//'/f32.bin', raw, err)
      if (.not. sh('capnp decode '//tmp//'/f32.capnp F < '//tmp//'/f32.bin > '//tmp//'/f32.txt')) then
         call test_failed(error, 'f32: capnp decode '//expect)
         return
      end if
      call slurp(tmp//'/f32.txt', ref, err)
      ref = chomp(ref)
      call capnp_deserialize_bytes(raw, msg, err)
      root = capnp_root(msg, err)
      call capnp_text_encode(schema, idx, root, text, err, pretty=.false.)
      text = chomp(text)
      call check_(error, err == CAPNP_OK .and. text == ref .and. index(text, expect) > 0, &
                  'f32 text '//expect//' got ['//text//'] capnp ['//ref//']')
      call capnp_message_free(msg)
      call capnp_text_decode(schema, idx, ref, msg, err)
      call check_(error, err == CAPNP_OK, 'f32: text decode '//expect)
      root = capnp_root(msg, err)
      d = capnp_dyn_get_f64(schema, idx, root, 'x', err)
      x32 = real(d, real32)
      gotb = transfer(x32, 0_int32)
      if (expect == 'nan') then
         call check_(error, err == CAPNP_OK .and. gotb /= 0_int32, 'f32: nan decodes')
      else if (expect == '-0') then
         call capnp_message_free(msg)
         call f32_match_encode(error, schema, idx, ref)
         return
      else
         call check_(error, err == CAPNP_OK .and. gotb == bits, 'f32 bits '//expect)
      end if
      call capnp_message_free(msg)
   end subroutine one_f32

   !> Text such as "(x = -0)" decodes to the same bytes as capnp encode.
   subroutine f32_match_encode(error, schema, idx, text)
      type(error_type), allocatable, intent(inout) :: error
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      character(len=*), intent(in) :: text
      type(capnp_message_t), target :: msg
      integer(int8), allocatable :: ours(:), ref(:)
      integer :: err
      if (allocated(error)) return
      call spit(tmp//'/f32.in', text//achar(10), err)
      if (.not. sh('capnp encode '//tmp//'/f32.capnp F < '//tmp//'/f32.in > '//tmp//'/f32.enc' &
         //' && capnp convert binary:canonical '//tmp//'/f32.capnp F < '//tmp//'/f32.enc > ' &
         //tmp//'/f32.can')) then
         call test_failed(error, 'f32: capnp encode '//text)
         return
      end if
      call capnp_text_decode(schema, idx, text, msg, err)
      call check_(error, err == CAPNP_OK, 'f32: decode '//text)
      call capnp_canonicalize(msg, ours, err)
      call capnp_read_file(tmp//'/f32.can', ref, err)
      if (err == CAPNP_OK .and. allocated(ours) .and. allocated(ref)) then
         call check_(error, same_i8(ours, ref), 'f32: encode bytes '//text)
      else
         call check_(error, .false., 'f32: encode bytes '//text)
      end if
      call capnp_message_free(msg)
   end subroutine f32_match_encode

   function chomp(s) result(t)
      character(len=*), intent(in) :: s
      character(len=:), allocatable :: t
      if (len(s) > 0) then
         if (s(len(s):len(s)) == achar(10)) then
            if (len(s) == 1) then
               t = ''
            else
               t = s(1:len(s) - 1)
            end if
            return
         end if
      end if
      t = s
   end function chomp

   subroutine load_cgr(error, schema, path)
      type(error_type), allocatable, intent(inout) :: error
      type(capnp_dyn_schema_t), intent(out) :: schema
      character(len=*), intent(in) :: path
      integer(int8), allocatable :: bytes(:)
      integer :: err
      if (allocated(error)) return
      call capnp_read_file(path, bytes, err)
      call capnp_dyn_load(schema, bytes, err)
      if (err /= CAPNP_OK) call test_failed(error, 'load '//path)
   end subroutine load_cgr

   subroutine run_write_error(error)
      type(error_type), allocatable, intent(out) :: error
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('test -e /dev/full && test -w /dev/full')) then
         call skip_test(error, '/dev/full is not writable')
         return
      end if
      if (.not. sh('mkdir -p '//tmp//' && '//cap_setup() &
         //' && printf "(x = 1, y = 0, z = 0)\n"' &
         //' | capnp encode schema/common.capnp Vec3 > '//tmp//'/one.bin')) then
         call test_failed(error, 'io: reference encode failed')
         return
      end if
      call io_fail(error, '"$CAP" decode schema/common.capnp Vec3 < '//tmp//'/one.bin', &
                   'io: text decode')
      call io_fail(error, '"$CAP" convert binary:text schema/common.capnp Vec3 < '//tmp//'/one.bin', &
                   'io: text convert')
      call io_fail(error, '"$CAP" convert binary:json schema/common.capnp Vec3 < '//tmp//'/one.bin', &
                   'io: json convert')
      call io_ok(error, '"$CAP" --help', 'io: help')
      call io_ok(error, '"$CAP" --version', 'io: version')
      call io_ok(error, '"$CAP" eval schema/kitchen.capnp answer', 'io: text eval')
      call io_fail(error, 'INC=$(dirname $(dirname $(command -v capnp)))/include' &
         //' && "$CAP" eval -I "$INC" -ojson schema/codec.capnp evalConst', &
         'io: json eval')
      call io_fail(error, 'INC=$(dirname $(dirname $(command -v capnp)))/include' &
         //' && "$CAP" eval -I "$INC" -b schema/codec.capnp evalConst', &
         'io: binary eval')
      call io_fail(error, 'INC=$(dirname $(dirname $(command -v capnp)))/include' &
         //' && "$CAP" eval -I "$INC" -p schema/codec.capnp evalConst', &
         'io: packed eval')
      call io_fail(error, 'printf "(x = 1, y = 0, z = 0)\n" | "$CAP" encode schema/common.capnp Vec3', &
                   'io: binary encode')
      call check_(error, sh(cap_setup()//' && "$CAP" decode schema/common.capnp Vec3 < ' &
         //tmp//'/one.bin >/dev/null'), 'io: text decode still succeeds')
   end subroutine run_write_error

   !> Status 1 and the write diagnostic. A crash or a missing message fails.
   subroutine io_fail(error, cmd, label)
      type(error_type), allocatable, intent(inout) :: error
      character(len=*), intent(in) :: cmd, label
      if (allocated(error)) return
      call check_(error, sh(cap_setup()//' && '//cmd//' >/dev/full 2>'//tmp//'/io.err; rc=$?; ' &
         //'test "$rc" -eq 1 && grep -q "cannot write stdout" '//tmp//'/io.err'), label)
   end subroutine io_fail

   subroutine io_ok(error, cmd, label)
      type(error_type), allocatable, intent(inout) :: error
      character(len=*), intent(in) :: cmd, label
      if (allocated(error)) return
      call check_(error, sh(cap_setup()//' && '//cmd//' >/dev/full 2>'//tmp//'/io.err; test $? -eq 0'), &
                  label)
   end subroutine io_ok

   function clock_span(c1, c2, rate) result(elapsed)
      integer(int64), intent(in) :: c1, c2, rate
      real :: elapsed
      elapsed = 0.0
      if (rate > 0_int64) elapsed = real(c2 - c1)/real(rate)
   end function clock_span

   !> Four times the input of a quadratic walk is about sixteen times the
   !> time. Ten times, plus a quarter second, is still under that and
   !> absorbs a noisy sample.
   function subquad(small, big) result(y)
      real, intent(in) :: small, big
      logical :: y
      y = big < small*10.0 + 0.25
   end function subquad


   subroutine slurp(path, s, err)
      character(len=*), intent(in) :: path
      character(len=:), allocatable, intent(out) :: s
      integer, intent(out) :: err
      integer(int8), allocatable :: b(:)
      integer :: i, n, lb, u
      call capnp_read_file(path, b, err)
      if (err /= CAPNP_OK) return
      n = size(b)
      lb = lbound(b, 1)
      allocate (character(len=n) :: s)
      do i = 1, n
         u = int(b(lb + i - 1))
         if (u < 0) u = u + 256
         s(i:i) = achar(u)
      end do
   end subroutine slurp

   pure function i8_of(u) result(v)
      integer, intent(in) :: u
      integer(int8) :: v
      integer :: x
      x = iand(u, 255)
      if (x >= 128) then
         v = int(x - 256, int8)
      else
         v = int(x, int8)
      end if
   end function i8_of

   function codes(s) result(b)
      character(len=*), intent(in) :: s
      integer, allocatable :: b(:)
      integer :: i
      allocate (b(len(s)))
      do i = 1, len(s)
         b(i) = iachar(s(i:i))
      end do
   end function codes

   subroutine load_codec(error, schema)
      type(error_type), allocatable, intent(inout) :: error
      type(capnp_dyn_schema_t), intent(out) :: schema
      integer(int8), allocatable :: bytes(:)
      integer :: err
      if (allocated(error)) return
      if (.not. sh('mkdir -p '//tmp)) then
         call test_failed(error, 'mkdir')
         return
      end if
      if (.not. sh('INC=$(dirname $(dirname $(command -v capnp)))/include' &
         //' && capnp compile -I"$INC" -o- schema/codec.capnp > '//tmp//'/codec.cgr')) then
         call test_failed(error, 'capnp compile codec schema')
         return
      end if
      call capnp_read_file(tmp//'/codec.cgr', bytes, err)
      call capnp_dyn_load(schema, bytes, err)
      if (err /= CAPNP_OK) call test_failed(error, 'load codec schema')
   end subroutine load_codec

   subroutine match_capnp_json(error, schema, idx, js, label)
      type(error_type), allocatable, intent(inout) :: error
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      character(len=*), intent(in) :: js, label
      type(capnp_message_t), target :: decoded
      integer(int8), allocatable :: ours(:), ref(:)
      integer :: err
      if (allocated(error)) return
      call spit(tmp//'/one.json', js, err)
      if (.not. sh('INC=$(dirname $(dirname $(command -v capnp)))/include' &
         //' && capnp convert -I"$INC" json:canonical schema/codec.capnp Sample < ' &
         //tmp//'/one.json > '//tmp//'/one.can')) then
         call test_failed(error, label//': capnp')
         return
      end if
      call capnp_json_decode(schema, idx, js, decoded, err)
      call check_(error, err == CAPNP_OK, label//': decode')
      call capnp_canonicalize(decoded, ours, err)
      call capnp_read_file(tmp//'/one.can', ref, err)
      if (err == CAPNP_OK .and. allocated(ours) .and. allocated(ref)) then
         call check_(error, same_i8(ours, ref), label)
      else
         call check_(error, .false., label)
      end if
      call capnp_message_free(decoded)
   end subroutine match_capnp_json

   function clash_schema() result(s)
      character(len=:), allocatable :: s
      s = '@0xe31a90c4b75d28f6;'//achar(10)// &
          'using Json = import "/capnp/compat/json.capnp";'//achar(10)// &
          'struct NameClash {'//achar(10)// &
          '  a @0 :Int32 $Json.name("same");'//achar(10)// &
          '  b @1 :Int32 $Json.name("same");'//achar(10)// &
          '}'//achar(10)// &
          'struct FlatClash {'//achar(10)// &
          '  left :group $Json.flatten() { x @0 :Int32; }'//achar(10)// &
          '  right :group $Json.flatten() { x @1 :Int32; }'//achar(10)// &
          '}'//achar(10)// &
          'struct DiscClash {'//achar(10)// &
          '  a @0 :Int32;'//achar(10)// &
          '  u :union $Json.flatten() $Json.discriminator(name = "a") {'//achar(10)// &
          '    v @1 :Int32;'//achar(10)// &
          '    w @2 :Int32;'//achar(10)// &
          '  }'//achar(10)// &
          '}'//achar(10)// &
          'struct VoidVal $Json.discriminator(valueName = "payload") {'//achar(10)// &
          '  union {'//achar(10)// &
          '    empty @0 :Void;'//achar(10)// &
          '    n @1 :Int32;'//achar(10)// &
          '  }'//achar(10)// &
          '}'//achar(10)// &
          'struct MissDisc $Json.discriminator(name = "kind") {'//achar(10)// &
          '  seq @0 :Int32;'//achar(10)// &
          '  union {'//achar(10)// &
          '    idle @1 :Void;'//achar(10)// &
          '    count @2 :Int32;'//achar(10)// &
          '  }'//achar(10)// &
          '}'//achar(10)
   end function clash_schema

   !> Recompile the text encoder with bounds checks. The parent evaluates
   !> size() on an unallocated node array and aborts. The fix returns
   !> an argument error before that call.
   subroutine check_text_bounds(error)
      type(error_type), allocatable, intent(inout) :: error
      if (allocated(error)) return
      if (.not. sh('mkdir -p '//tmp//'/mods' &
         //' && mod=$(find build -name capnp_text.mod | xargs ls -t | head -1)' &
         //' && lib=$(find build -name libcapnp.a | xargs ls -t | head -1)' &
         //' && test -n "$mod" && test -n "$lib"' &
         //' && dir=$(dirname "$mod")' &
         //' && san=; if [ -n "${ASAN_OPTIONS:-}" ]; then san="-fsanitize=address,undefined"; fi' &
         //' && gfortran $san -fcheck=all -J '//tmp//'/mods -I "$dir" -c src/capnp_text.f90 -o ' &
         //tmp//'/text_check.o' &
         //' && cat > '//tmp//'/ubounds.f90 <<''EOF'' &&'//achar(10)// &
         'program u'//achar(10)// &
         '   use capnp_text, only: capnp_text_encode'//achar(10)// &
         '   use capnp_dynamic, only: capnp_dyn_schema_t'//achar(10)// &
         '   use capnp_message, only: capnp_ptr_t'//achar(10)// &
         '   use capnp_kinds, only: capnp_err_arg'//achar(10)// &
         '   implicit none'//achar(10)// &
         '   type(capnp_dyn_schema_t) :: schema'//achar(10)// &
         '   type(capnp_ptr_t) :: p'//achar(10)// &
         '   character(len=:), allocatable :: text'//achar(10)// &
         '   integer :: err'//achar(10)// &
         '   call capnp_text_encode(schema, 1, p, text, err)'//achar(10)// &
         '   if (err /= capnp_err_arg) stop 1'//achar(10)// &
         'end program'//achar(10)// &
         'EOF'//achar(10)// &
         'gfortran $san -fcheck=all -I '//tmp//'/mods -I "$dir" '//tmp//'/ubounds.f90 ' &
         //tmp//'/text_check.o "$lib" -o '//tmp//'/ubounds' &
         //' && ASAN_OPTIONS=detect_leaks=0 '//tmp//'/ubounds')) then
         call test_failed(error, 'guard: bounds-checked text encode')
      end if
   end subroutine check_text_bounds
   subroutine run_json_unloaded(error)
      type(error_type), allocatable, intent(out) :: error
      if (.not. sh('mkdir -p '//tmp//'/mods' &
         //' && mod=$(find build -name capnp_json.mod | xargs ls -t | head -1)' &
         //' && lib=$(find build -name libcapnp.a | xargs ls -t | head -1)' &
         //' && test -n "$mod" && test -n "$lib"' &
         //' && dir=$(dirname "$mod")' &
         //' && san=; if [ -n "${ASAN_OPTIONS:-}" ]; then san="-fsanitize=address,undefined"; fi' &
         //' && gfortran $san -fcheck=all -J '//tmp//'/mods -I "$dir" -c src/capnp_json.f90 -o ' &
         //tmp//'/json_check.o' &
         //' && cat > '//tmp//'/jbounds.f90 <<''EOF'' &&'//achar(10)// &
         'program u'//achar(10)// &
         '   use capnp_json, only: capnp_json_encode'//achar(10)// &
         '   use capnp_dynamic, only: capnp_dyn_schema_t'//achar(10)// &
         '   use capnp_message, only: capnp_ptr_t'//achar(10)// &
         '   use capnp_kinds, only: capnp_err_arg'//achar(10)// &
         '   implicit none'//achar(10)// &
         '   type(capnp_dyn_schema_t) :: schema'//achar(10)// &
         '   type(capnp_ptr_t) :: p'//achar(10)// &
         '   character(len=:), allocatable :: text'//achar(10)// &
         '   integer :: err'//achar(10)// &
         '   call capnp_json_encode(schema, 1, p, text, err)'//achar(10)// &
         '   if (err /= capnp_err_arg) stop 1'//achar(10)// &
         'end program'//achar(10)// &
         'EOF'//achar(10)// &
         'gfortran $san -fcheck=all -I '//tmp//'/mods -I "$dir" '//tmp//'/jbounds.f90 ' &
         //tmp//'/json_check.o "$lib" -o '//tmp//'/jbounds' &
         //' && ASAN_OPTIONS=detect_leaks=0 '//tmp//'/jbounds')) then
         call test_failed(error, 'guard: bounds-checked json encode')
      end if
   end subroutine run_json_unloaded
   !> The second 200-byte malloc is the canonical byte buffer. The parent
   !> allocates it without a status check and aborts. The fix returns
   !> an allocation error and a later canonicalization still works.
   subroutine check_canon_alloc(error)
      type(error_type), allocatable, intent(inout) :: error
      if (allocated(error)) return
      ! ASan replaces malloc, so the preload hook cannot see the byte buffer.
      if (.not. sh('test -z "${ASAN_OPTIONS:-}"')) return
      if (.not. sh('mkdir -p '//tmp &
         //' && cat > '//tmp//'/faili.c <<''EOF'' &&'//achar(10)// &
         '#define _GNU_SOURCE'//achar(10)// &
         '#include <dlfcn.h>'//achar(10)// &
         '#include <stdlib.h>'//achar(10)// &
         '#include <stddef.h>'//achar(10)// &
         'static int seen;'//achar(10)// &
         'void *malloc(size_t s) {'//achar(10)// &
         '  static void *(*real)(size_t);'//achar(10)// &
         '  char *es, *ei; int want, index;'//achar(10)// &
         '  if (!real) real = dlsym(RTLD_NEXT, "malloc");'//achar(10)// &
         '  es = getenv("FAIL_SIZE"); ei = getenv("FAIL_INDEX");'//achar(10)// &
         '  want = es ? atoi(es) : 0; index = ei ? atoi(ei) : 1;'//achar(10)// &
         '  if (want > 0 && (int)s == want) {'//achar(10)// &
         '    seen++; if (seen == index) return 0;'//achar(10)// &
         '  }'//achar(10)// &
         '  return real(s);'//achar(10)// &
         '}'//achar(10)// &
         'EOF'//achar(10)// &
         'gcc -shared -fPIC -o '//tmp//'/faili.so '//tmp//'/faili.c -ldl' &
         //' && cat > '//tmp//'/once.f90 <<''EOF'' &&'//achar(10)// &
         'program c'//achar(10)// &
         '  use iso_fortran_env, only: int8, int32, int64'//achar(10)// &
         '  use capnp_arena, only: capnp_message_t, capnp_message_init_builder,' &
         //' capnp_message_free'//achar(10)// &
         '  use capnp_message, only: capnp_ptr_t, capnp_new_struct,' &
         //' capnp_set_root, capnp_set_i32'//achar(10)// &
         '  use capnp_canonical, only: capnp_canonicalize'//achar(10)// &
         '  use capnp_kinds, only: capnp_err_alloc, capnp_ok'//achar(10)// &
         '  implicit none'//achar(10)// &
         '  type(capnp_message_t), target :: msg'//achar(10)// &
         '  type(capnp_ptr_t) :: root'//achar(10)// &
         '  integer(int8), allocatable :: bytes(:)'//achar(10)// &
         '  integer :: err'//achar(10)// &
         '  call capnp_message_init_builder(msg, err)'//achar(10)// &
         '  root = capnp_new_struct(msg, 24, 0, err)'//achar(10)// &
         '  call capnp_set_root(msg, root, err)'//achar(10)// &
         '  call capnp_set_i32(root, 184_int64, 7_int32, err)'//achar(10)// &
         '  call capnp_canonicalize(msg, bytes, err)'//achar(10)// &
         '  if (err /= capnp_err_alloc) stop 2'//achar(10)// &
         '  call capnp_message_free(msg)'//achar(10)// &
         '  call capnp_message_init_builder(msg, err)'//achar(10)// &
         '  root = capnp_new_struct(msg, 1, 0, err)'//achar(10)// &
         '  call capnp_set_root(msg, root, err)'//achar(10)// &
         '  call capnp_set_i32(root, 0_int64, 9_int32, err)'//achar(10)// &
         '  call capnp_canonicalize(msg, bytes, err)'//achar(10)// &
         '  if (err /= capnp_ok .or. .not. allocated(bytes)) stop 3'//achar(10)// &
         'end program'//achar(10)// &
         'EOF'//achar(10)// &
         'mod=$(find build -name capnp_canonical.mod | xargs ls -t | head -1)' &
         //' && lib=$(find build -name libcapnp.a | xargs ls -t | head -1)' &
         //' && dir=$(dirname "$mod")' &
         //' && san=; if [ -n "${ASAN_OPTIONS:-}" ]; then san="-fsanitize=address,undefined"; fi' &
         //' && gfortran $san -I "$dir" '//tmp//'/once.f90 "$lib" -o '//tmp//'/once' &
         //' && FAIL_SIZE=200 FAIL_INDEX=2 LD_PRELOAD='//tmp//'/faili.so ' &
         //tmp//'/once')) then
         call test_failed(error, 'canon: failed byte allocation is reported')
      end if
   end subroutine check_canon_alloc
   !> The regress directory is removed when the suite finishes. This fails
   !> if the release leaves the directory in place.
   subroutine run_regress_tmp(error)
      type(error_type), allocatable, intent(out) :: error
      call init_tmp()
      if (.not. sh('mkdir -p '//tmp//'/left && test -d '//tmp)) then
         call test_failed(error, 'regress temp dir was not created')
         return
      end if
      call release_regress_tmp()
      if (.not. sh('test ! -d '//tmp)) then
         call test_failed(error, 'regress temp dir remains')
      end if
   end subroutine run_regress_tmp

   !> The command line no longer carries a dead write switch, and the
   !> interrupted-write check uses the platform errno helper.
   subroutine run_cli_sources(error)
      type(error_type), allocatable, intent(out) :: error
      if (.not. sh('! grep -q strict_io app/capnpf.f90' &
         //' && ! grep -q __errno_location app/capnpf.f90' &
         //' && ! grep -q " == 4" app/capnpf.f90' &
         //' && grep -q os_eintr app/capnpf.f90' &
         //' && grep -q os_errno_value app/capnpf.f90' &
         //' && grep -q __error app/os_errno.c' &
         //' && grep -q __errno_location app/os_errno.c' &
         //' && grep -q EINTR app/os_errno.c')) then
         call test_failed(error, 'cli source checks failed')
      end if
   end subroutine run_cli_sources

   !> Sixty-five discriminated objects in one list. Each object pops its
   !> union note on the way out, so the 64-slot record does not fill up.
   subroutine run_json_union_many(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root, rows, el
      character(len=:), allocatable :: src, js
      integer(int8), allocatable :: bytes(:)
      integer :: err, idx, i, iidx
      integer(int64) :: aval
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp)) then
         call test_failed(error, 'mkdir')
         return
      end if
      src = '@0x8011223344556677;'//achar(10)// &
            'using Json = import "/capnp/compat/json.capnp";'//achar(10)// &
            'struct Item $Json.discriminator(name = "kind") {'//achar(10)// &
            '  union {'//achar(10)// &
            '    a @0 :Int32;'//achar(10)// &
            '    b @1 :Int32;'//achar(10)// &
            '  }'//achar(10)// &
            '}'//achar(10)// &
            'struct Bag { rows @0 :List(Item); }'//achar(10)
      call spit(tmp//'/many.capnp', src, err)
      if (.not. sh('INC=$(dirname $(dirname $(command -v capnp)))/include' &
         //' && capnp compile -I"$INC" -o- '//tmp//'/many.capnp > '//tmp//'/many.cgr')) then
         call test_failed(error, 'union-many: compile')
         return
      end if
      call capnp_read_file(tmp//'/many.cgr', bytes, err)
      call capnp_dyn_load(schema, bytes, err)
      idx = capnp_dyn_find(schema, 'Bag', err)
      call check_(error, err == CAPNP_OK .and. idx > 0, 'union-many: schema')
      if (allocated(error)) then
         call capnp_dyn_free(schema)
         return
      end if
      js = '{"rows":['
      do i = 1, 65
         if (i > 1) js = js//','
         js = js//'{"kind":"a","a":1}'
      end do
      js = js//']}'
      call capnp_json_decode(schema, idx, js, msg, err)
      call check_(error, err == CAPNP_OK, 'union-many: 65 items decode')
      if (allocated(error)) then
         call capnp_message_free(msg)
         call capnp_dyn_free(schema)
         return
      end if
      root = capnp_root(msg, err)
      rows = capnp_getp(root, 0, err)
      call check_(error, err == CAPNP_OK .and. capnp_list_len(rows) == 65_int64, &
                  'union-many: list length')
      iidx = capnp_dyn_find(schema, 'Item', err)
      el = capnp_list_get_struct(rows, 64, err)
      aval = capnp_dyn_get_int(schema, iidx, el, 'a', err)
      call check_(error, err == CAPNP_OK .and. aval == 1_int64, 'union-many: last value')
      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end subroutine run_json_union_many

   !> Forty nested objects, each with two flattened discriminated unions.
   !> Every open object keeps its notes, so the record holds eighty at the
   !> deepest level. capnp 1.4.0 decodes the same text.
   subroutine run_json_union_nested(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: cur
      character(len=:), allocatable :: src, js
      integer(int8), allocatable :: bytes(:)
      integer :: err, idx, i
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp)) then
         call test_failed(error, 'mkdir')
         return
      end if
      src = '@0x8011223344556678;'//achar(10)// &
            'using Json = import "/capnp/compat/json.capnp";'//achar(10)// &
            'struct Node {'//achar(10)// &
            '  u1 :union $Json.flatten() $Json.discriminator(name = "k1") {'//achar(10)// &
            '    a @0 :Int32;'//achar(10)// &
            '    b @1 :Int32;'//achar(10)// &
            '  }'//achar(10)// &
            '  u2 :union $Json.flatten() $Json.discriminator(name = "k2") {'//achar(10)// &
            '    c @2 :Int32;'//achar(10)// &
            '    d @3 :Int32;'//achar(10)// &
            '  }'//achar(10)// &
            '  child @4 :Node;'//achar(10)// &
            '}'//achar(10)
      call spit(tmp//'/nested.capnp', src, err)
      if (.not. sh('INC=$(dirname $(dirname $(command -v capnp)))/include' &
         //' && capnp compile -I"$INC" -o- '//tmp//'/nested.capnp > '//tmp//'/nested.cgr')) then
         call test_failed(error, 'union-nested: compile')
         return
      end if
      call capnp_read_file(tmp//'/nested.cgr', bytes, err)
      call capnp_dyn_load(schema, bytes, err)
      idx = capnp_dyn_find(schema, 'Node', err)
      call check_(error, err == CAPNP_OK .and. idx > 0, 'union-nested: schema')
      if (allocated(error)) then
         call capnp_dyn_free(schema)
         return
      end if
      js = '{"k1":"a","a":1,"k2":"c","c":2}'
      do i = 2, 40
         js = '{"k1":"a","a":1,"k2":"c","c":2,"child":'//js//'}'
      end do
      call capnp_json_decode(schema, idx, js, msg, err)
      call check_(error, err == CAPNP_OK, 'union-nested: 40 levels decode')
      if (allocated(error)) then
         call capnp_message_free(msg)
         call capnp_dyn_free(schema)
         return
      end if
      cur = capnp_root(msg, err)
      do i = 2, 40
         cur = capnp_dyn_getp(schema, idx, cur, 'child', err)
         if (err /= CAPNP_OK .or. cur%kind /= CAPNP_PK_STRUCT) exit
      end do
      call check_(error, err == CAPNP_OK .and. i == 41 .and. cur%kind == CAPNP_PK_STRUCT, &
                  'union-nested: all 40 levels present')
      cur = capnp_dyn_getp(schema, idx, cur, 'child', err)
      call check_(error, err == CAPNP_OK .and. cur%kind == CAPNP_PK_NULL, &
                  'union-nested: nothing below the deepest level')
      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end subroutine run_json_union_nested
   !> A chain of 4100 struct types, each a field of the one before. The
   !> request is built here, so the audit runs even when capnp compile
   !> cannot finish the same chain on a small stack.
   subroutine run_json_schema_chain(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg
      integer(int8), allocatable :: bytes(:)
      integer :: err, idx
      call build_type_chain(4100, bytes, err)
      call check_(error, err == CAPNP_OK, 'schema-chain: build')
      if (allocated(error)) return
      call capnp_dyn_load(schema, bytes, err)
      idx = capnp_dyn_find(schema, 'S1', err)
      call check_(error, err == CAPNP_OK .and. idx > 0, 'schema-chain: schema')
      if (allocated(error)) then
         call capnp_dyn_free(schema)
         return
      end if
      call capnp_json_decode(schema, idx, '{"v":7}', msg, err)
      call check_(error, err == CAPNP_OK, 'schema-chain: 4100 types decode')
      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end subroutine run_json_schema_chain

   !> CodeGeneratorRequest for struct S1 through SN. A flat node list
   !> stays inside the schema reader depth limit.
   subroutine build_type_chain(ntypes, bytes, err)
      integer, intent(in) :: ntypes
      integer(int8), allocatable, intent(out) :: bytes(:)
      integer, intent(out) :: err
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root, nodes, node, fields, field, typ
      character(len=16) :: name
      integer :: i, nfields
      call capnp_message_init_builder(msg, err)
      if (err /= CAPNP_OK) return
      root = capnp_new_struct(msg, 0, 4, err)
      if (err == CAPNP_OK) call capnp_set_root(msg, root, err)
      if (err == CAPNP_OK) nodes = capnp_new_composite_list(msg, int(ntypes, int64), 6, 6, err)
      if (err == CAPNP_OK) call capnp_setp(root, 0, nodes, err)
      do i = 1, ntypes
         if (err /= CAPNP_OK) exit
         node = capnp_list_get_struct(nodes, i - 1, err)
         if (err /= CAPNP_OK) exit
         call capnp_set_i64(node, 0_int64, 1000_int64 + int(i, int64), err)
         if (err /= CAPNP_OK) exit
         write (name, '(a,i0)') 'S', i
         call capnp_set_text(node, 0, trim(name), err)
         if (err /= CAPNP_OK) exit
         call capnp_set_u16(node, 12_int64, 1, err)
         if (err /= CAPNP_OK) exit
         call capnp_set_u16(node, 14_int64, 1, err)
         if (err /= CAPNP_OK) exit
         nfields = 1
         if (i < ntypes) nfields = 2
         call capnp_set_u16(node, 24_int64, nfields - 1, err)
         if (err /= CAPNP_OK) exit
         fields = capnp_new_composite_list(msg, int(nfields, int64), 3, 4, err)
         if (err /= CAPNP_OK) exit
         field = capnp_list_get_struct(fields, 0, err)
         if (err /= CAPNP_OK) exit
         call capnp_set_text(field, 0, 'v', err)
         if (err /= CAPNP_OK) exit
         call capnp_set_u16(field, 2_int64, 65535, err)
         if (err /= CAPNP_OK) exit
         typ = capnp_new_struct(msg, 3, 1, err)
         if (err /= CAPNP_OK) exit
         call capnp_set_u16(typ, 0_int64, 4, err)
         if (err /= CAPNP_OK) exit
         call capnp_setp(field, 2, typ, err)
         if (err /= CAPNP_OK) exit
         if (nfields == 2) then
            field = capnp_list_get_struct(fields, 1, err)
            if (err /= CAPNP_OK) exit
            call capnp_set_u16(field, 0_int64, 1, err)
            if (err /= CAPNP_OK) exit
            call capnp_set_text(field, 0, 'next', err)
            if (err /= CAPNP_OK) exit
            call capnp_set_u16(field, 2_int64, 65535, err)
            if (err /= CAPNP_OK) exit
            typ = capnp_new_struct(msg, 3, 1, err)
            if (err /= CAPNP_OK) exit
            call capnp_set_u16(typ, 0_int64, 16, err)
            if (err /= CAPNP_OK) exit
            call capnp_set_i64(typ, 8_int64, 1000_int64 + int(i + 1, int64), err)
            if (err /= CAPNP_OK) exit
            call capnp_setp(field, 2, typ, err)
            if (err /= CAPNP_OK) exit
         end if
         call capnp_setp(node, 3, fields, err)
      end do
      if (err == CAPNP_OK) call capnp_serialize_bytes(msg, bytes, err)
      call capnp_message_free(msg)
   end subroutine build_type_chain



   !> A chain of 100 nested groups still clears. Groups add no pointer
   !> nesting, so the reader limit of 64 does not apply, and capnp 1.4.0
   !> compiles the same schema.
   subroutine run_group_clear_depth(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root
      character(len=:), allocatable :: src
      integer(int8), allocatable :: bytes(:)
      integer :: err, idx, i
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp)) then
         call test_failed(error, 'mkdir')
         return
      end if
      src = '@0x8e9c0b1a2d3f4051;'//achar(10)// &
            'struct Deep {'//achar(10)// &
            '  union {'//achar(10)// &
            '    nest :group {'//achar(10)
      do i = 1, 100
         src = src//'      g :group {'//achar(10)
      end do
      src = src//'        n @0 :Int32;'//achar(10)
      do i = 1, 100
         src = src//'      }'//achar(10)
      end do
      src = src//'    }'//achar(10)// &
            '    other @1 :Void;'//achar(10)// &
            '  }'//achar(10)// &
            '}'//achar(10)
      call spit(tmp//'/deep.capnp', src, err)
      if (.not. sh('capnp compile -o- '//tmp//'/deep.capnp > '//tmp//'/deep.cgr')) then
         call test_failed(error, 'group-depth: compile')
         return
      end if
      call capnp_read_file(tmp//'/deep.cgr', bytes, err)
      call capnp_dyn_load(schema, bytes, err)
      idx = capnp_dyn_find(schema, 'Deep', err)
      call check_(error, err == CAPNP_OK .and. idx > 0, 'group-depth: schema')
      if (allocated(error)) then
         call capnp_dyn_free(schema)
         return
      end if
      call capnp_message_init_builder(msg, err)
      root = capnp_dyn_new_root(schema, idx, msg, err)
      call capnp_dyn_select(schema, idx, root, 'other', err)
      call check_(error, err == CAPNP_OK, 'group-depth: clear a deep group')
      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end subroutine run_group_clear_depth

   !> Streaming pack and unpack must grow an already-allocated empty
   !> output buffer instead of looping forever in ensure.
   subroutine run_packed_zero_cap(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_packer_t) :: pk
      type(capnp_unpacker_t) :: uk
      integer(int8), allocatable :: pbuf(:), ubuf(:)
      integer(int8) :: word(0:7)
      integer(int64) :: pn, un
      integer :: err
      word = [1_int8, 0_int8, 2_int8, 0_int8, 3_int8, 0_int8, 4_int8, 0_int8]
      allocate (pbuf(0))
      pn = 0_int64
      call capnp_pack_push(pk, word, pbuf, pn, err)
      call check_(error, err == CAPNP_OK, 'packed-zero-cap: pack push')
      call capnp_pack_finish(pk, pbuf, pn, err)
      call check_(error, err == CAPNP_OK .and. pn > 0_int64, 'packed-zero-cap: pack finish')
      allocate (ubuf(0))
      un = 0_int64
      call capnp_unpack_push(uk, pbuf(0:pn - 1), ubuf, un, err)
      call check_(error, err == CAPNP_OK .and. un == 8_int64, 'packed-zero-cap: unpack push')
      if (un == 8_int64) then
         call check_(error, all(ubuf(0:7) == word), 'packed-zero-cap: roundtrip')
      end if
   end subroutine run_packed_zero_cap

   !> JSON keys, schema names, and enum values must match with exact length
   !> so trailing spaces are never ignored.
   subroutine run_json_trailing_spaces(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root, fld
      character(len=:), allocatable :: src, out, ref
      integer(int8), allocatable :: bytes(:)
      integer :: err, idx
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp)) then
         call test_failed(error, 'mkdir')
         return
      end if
      src = '@0x9a1b2c3d4e5f6071;'//achar(10)// &
            'using Json = import "/capnp/compat/json.capnp";'//achar(10)// &
            'enum Color { red @0; blue @1; }'//achar(10)// &
            'struct Sp {'//achar(10)// &
            '  a @0 :Int32;'//achar(10)// &
            '  b @1 :Int32 $Json.name("a ");'//achar(10)// &
            '  e @2 :Color;'//achar(10)// &
            '  u :union {'//achar(10)// &
            '    none @3 :Void;'//achar(10)// &
            '    num @4 :Int32;'//achar(10)// &
            '  }'//achar(10)// &
            '  d :union $Json.discriminator(name = "kind") {'//achar(10)// &
            '    da @5 :Int32;'//achar(10)// &
            '    db @6 :Text;'//achar(10)// &
            '  }'//achar(10)// &
            '}'//achar(10)
      call spit(tmp//'/sp.capnp', src, err)
      if (.not. sh('INC=$(dirname $(dirname $(command -v capnp)))/include' &
         //' && capnp compile -I"$INC" -o- '//tmp//'/sp.capnp > '//tmp//'/sp.cgr' &
         //' && printf ''{"a": 1, "a ": 2}\n''' &
         //' | capnp convert -I"$INC" json:json --short '//tmp//'/sp.capnp Sp > '//tmp//'/sp.ref')) then
         call test_failed(error, 'trailing-spaces: compile')
         return
      end if
      call capnp_read_file(tmp//'/sp.cgr', bytes, err)
      call capnp_dyn_load(schema, bytes, err)
      idx = capnp_dyn_find(schema, 'Sp', err)
      call check_(error, err == CAPNP_OK .and. idx > 0, 'trailing-spaces: schema')
      if (allocated(error)) then
         call capnp_dyn_free(schema)
         return
      end if
      call capnp_codec_find_field(schema, idx, 'a ', fld, err)
      call check_(error, err /= CAPNP_OK, 'trailing-spaces: find_field rejects trailing blank')
      call capnp_json_decode(schema, idx, '{"a": 1, "a ": 2}', msg, err)
      call check_(error, err == CAPNP_OK, 'trailing-spaces: decode distinct a and "a "')
      if (.not. allocated(error)) then
         root = capnp_root(msg, err)
         call capnp_json_encode(schema, idx, root, out, err, pretty=.false.)
         call slurp(tmp//'/sp.ref', ref, err)
         call check_(error, err == CAPNP_OK .and. out//achar(10) == ref, &
                     'trailing-spaces: encode matches capnp 1.4.0')
      end if
      call capnp_message_free(msg)
      call capnp_json_decode(schema, idx, '{"a  ": 5}', msg, err)
      call check_(error, err == CAPNP_OK, 'trailing-spaces: unknown key "a  " ignored')
      if (.not. allocated(error)) then
         root = capnp_root(msg, err)
         call check_(error, capnp_get_i32(root, 0_int64) == 0_int32, &
                     'trailing-spaces: "a  " does not set a')
      end if
      call capnp_message_free(msg)
      call capnp_json_decode(schema, idx, '{"e": "blue "}', msg, err)
      call check_(error, err /= CAPNP_OK, 'trailing-spaces: enum "blue " rejected')
      call capnp_message_free(msg)
      call capnp_json_decode(schema, idx, '{"u": {"num ": 4}}', msg, err)
      call check_(error, err == CAPNP_OK, 'trailing-spaces: union key "num " ignored')
      if (.not. allocated(error)) then
         root = capnp_root(msg, err)
         call capnp_json_encode(schema, idx, root, out, err, pretty=.false.)
         call check_(error, index(out, '"none":null') > 0, &
                     'trailing-spaces: union stays on none')
      end if
      call capnp_message_free(msg)
      call capnp_json_decode(schema, idx, '{"d": {"kind ": "da", "kind": "db", "db": "ok"}}', &
                             msg, err)
      call check_(error, err == CAPNP_OK, 'trailing-spaces: "kind " ignored beside "kind"')
      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end subroutine run_json_trailing_spaces

   !> Named unions and group-contained unions drop earlier arms when a later
   !> key selects another arm, matching Cap'n Proto C++ orphan/adopt.
   !> Anonymous unions on a struct and root-level flattened unions keep them.
   subroutine run_json_union_clear(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      character(len=:), allocatable :: src
      integer(int8), allocatable :: bytes(:)
      integer :: err
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp)) then
         call test_failed(error, 'mkdir')
         return
      end if
      src = '@0x9a1b2c3d4e5f6072;'//achar(10)// &
            'using Json = import "/capnp/compat/json.capnp";'//achar(10)// &
            'struct Sub { a @0 :Int32; b @1 :Int32; }'//achar(10)// &
            'struct Anon {'//achar(10)// &
            '  union { none @0 :Void; num @1 :Int32; sub @2 :Sub; }'//achar(10)// &
            '}'//achar(10)// &
            'struct Root {'//achar(10)// &
            '  u :union {'//achar(10)// &
            '    none @0 :Void;'//achar(10)// &
            '    num @1 :Int32;'//achar(10)// &
            '    sub @2 :Sub;'//achar(10)// &
            '    grp :group {'//achar(10)// &
            '      x @3 :Int32;'//achar(10)// &
            '      iu :union { inone @4 :Void; isub @5 :Sub; }'//achar(10)// &
            '    }'//achar(10)// &
            '  }'//achar(10)// &
            '  d :union $Json.discriminator(name = "kind") {'//achar(10)// &
            '    dnone @6 :Void; dnum @7 :Int32; dsub @8 :Sub;'//achar(10)// &
            '  }'//achar(10)// &
            '  g :group {'//achar(10)// &
            '    s @9 :Anon;'//achar(10)// &
            '    fu :union $Json.flatten() {'//achar(10)// &
            '      fnone @10 :Void; fnum @11 :Int32; fsub @12 :Sub;'//achar(10)// &
            '    }'//achar(10)// &
            '  }'//achar(10)// &
            '  rfu :union $Json.flatten() {'//achar(10)// &
            '    rnone @13 :Void; rnum @14 :Int32; rsub @15 :Sub;'//achar(10)// &
            '  }'//achar(10)// &
            '}'//achar(10)
      call spit(tmp//'/uc.capnp', src, err)
      if (.not. sh('INC=$(dirname $(dirname $(command -v capnp)))/include' &
         //' && capnp compile -I"$INC" -o- '//tmp//'/uc.capnp > '//tmp//'/uc.cgr')) then
         call test_failed(error, 'union-clear: compile')
         return
      end if
      call capnp_read_file(tmp//'/uc.cgr', bytes, err)
      call capnp_dyn_load(schema, bytes, err)
      call check_(error, err == CAPNP_OK, 'union-clear: load')
      call check_case('Root', '{"u":{"sub":{"a":3},"none":null}}')
      call check_case('Root', '{"u":{"sub":{"a":3},"num":42}}')
      call check_case('Root', '{"u":{"num":42,"sub":{"a":3}}}')
      call check_case('Root', '{"u":{"grp":{"x":1,"iu":{"isub":{"a":2}}},"none":null}}')
      call check_case('Root', '{"d":{"dsub":{"a":3},"kind":"dnone"}}')
      call check_case('Root', '{"d":{"dnum":42,"kind":"dnone"}}')
      call check_case('Root', '{"g":{"fsub":{"a":3},"fnone":null}}')
      call check_case('Root', '{"g":{"s":{"sub":{"a":3},"none":null}}}')
      call check_case('Root', '{"rsub":{"a":3},"rnone":null}')
      call check_case('Anon', '{"sub":{"a":3},"none":null}')
      call capnp_dyn_free(schema)
   contains
      subroutine check_case(tname, js)
         character(len=*), intent(in) :: tname, js
         type(capnp_message_t) :: decoded
         integer(int8), allocatable :: ours(:), ref(:)
         integer :: idx, e2
         logical :: cpp_ok, ours_ok
         if (allocated(error)) return
         idx = capnp_dyn_find(schema, tname, e2)
         if (e2 /= CAPNP_OK .or. idx == 0) then
            call test_failed(error, 'union-clear: missing '//tname)
            return
         end if
         call spit(tmp//'/uc.json', js, e2)
         cpp_ok = sh('INC=$(dirname $(dirname $(command -v capnp)))/include' &
            //' && capnp convert -I"$INC" json:canonical '//tmp//'/uc.capnp '//tname &
            //' < '//tmp//'/uc.json > '//tmp//'/uc.can 2>'//tmp//'/uc.err')
         call capnp_json_decode(schema, idx, js, decoded, e2)
         ours_ok = e2 == CAPNP_OK
         if (cpp_ok .neqv. ours_ok) then
            call test_failed(error, 'union-clear: status '//tname//' '//js)
            call capnp_message_free(decoded)
            return
         end if
         if (.not. ours_ok) then
            call capnp_message_free(decoded)
            return
         end if
         call capnp_canonicalize(decoded, ours, e2)
         call capnp_read_file(tmp//'/uc.can', ref, e2)
         if (e2 /= CAPNP_OK .or. .not. allocated(ours) .or. .not. allocated(ref)) then
            call test_failed(error, 'union-clear: bytes '//tname//' '//js)
         else if (.not. same_i8(ours, ref)) then
            call test_failed(error, 'union-clear: bytes '//tname//' '//js)
         end if
         call capnp_message_free(decoded)
      end subroutine check_case
   end subroutine run_json_union_clear

   !> Cap'n Proto C++ treats a literal NUL byte as end-of-input in JSON:
   !> trailing NUL (and bytes after it) are accepted, a NUL inside a string
   !> is rejected, and \u0000 inside a string is decoded to a NUL byte.
   subroutine run_json_nul_eof(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t) :: msg
      type(capnp_ptr_t) :: root
      character(len=:), allocatable :: out
      integer :: err, idx
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      call load_codec(error, schema)
      if (allocated(error)) return
      idx = capnp_dyn_find(schema, 'Sample', err)
      call check_(error, err == CAPNP_OK .and. idx > 0, 'nul-eof: find Sample')
      if (allocated(error)) then
         call capnp_dyn_free(schema)
         return
      end if
      call capnp_json_decode(schema, idx, '{"display_name":"ab"}'//achar(0), msg, err)
      call check_(error, err == CAPNP_OK, 'nul-eof: trailing NUL accepted')
      if (.not. allocated(error)) then
         root = capnp_root(msg, err)
         call capnp_json_encode(schema, idx, root, out, err, pretty=.false.)
         call check_(error, err == CAPNP_OK .and. index(out, '"display_name":"ab"') > 0, &
                     'nul-eof: trailing NUL value')
      end if
      call capnp_message_free(msg)
      call capnp_json_decode(schema, idx, '{"display_name":"ab"}'//achar(0)//'garbage', msg, err)
      call check_(error, err == CAPNP_OK, 'nul-eof: bytes after NUL ignored')
      call capnp_message_free(msg)
      call capnp_json_decode(schema, idx, '{"display_name":"a'//achar(0)//'b"}', msg, err)
      call check_(error, err /= CAPNP_OK, 'nul-eof: literal NUL in string rejected')
      call capnp_message_free(msg)
      call capnp_json_decode(schema, idx, '{"display_name":"a\u0000b"}', msg, err)
      call check_(error, err == CAPNP_OK, 'nul-eof: escaped u0000 accepted')
      call capnp_message_free(msg)
      call capnp_json_decode(schema, idx, achar(0), msg, err)
      call check_(error, err /= CAPNP_OK, 'nul-eof: bare NUL rejected')
      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end subroutine run_json_nul_eof

   !> Cap'n Proto C++ enforces nestingDepth <= 64 on JSON containers ({ and [)
   !> only, allowing scalars inside container 64 and rejecting container 65.
   subroutine run_json_nest_limit(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t) :: msg
      character(len=:), allocatable :: js
      integer :: err, idx
      call load_codec(error, schema)
      if (allocated(error)) return
      idx = capnp_dyn_find(schema, 'Sample', err)
      call check_(error, err == CAPNP_OK .and. idx > 0, 'nest-limit: find Sample')
      if (allocated(error)) then
         call capnp_dyn_free(schema)
         return
      end if
      js = repeat('{"x":', 63)//'{}'//repeat('}', 63)
      call capnp_json_decode(schema, idx, js, msg, err)
      call check_(error, err == CAPNP_OK, 'nest-limit: 64 empty objects accepted')
      call capnp_message_free(msg)
      js = repeat('{"x":', 63)//'{"y":1}'//repeat('}', 63)
      call capnp_json_decode(schema, idx, js, msg, err)
      call check_(error, err == CAPNP_OK, 'nest-limit: scalar inside 64th object accepted')
      call capnp_message_free(msg)
      js = repeat('{"x":', 64)//'{}'//repeat('}', 64)
      call capnp_json_decode(schema, idx, js, msg, err)
      call check_(error, err == CAPNP_ERR_DEPTH, 'nest-limit: 65 empty objects rejected')
      call capnp_message_free(msg)
      js = '{"x":'//repeat('[', 63)//'1'//repeat(']', 63)//'}'
      call capnp_json_decode(schema, idx, js, msg, err)
      call check_(error, err == CAPNP_OK, 'nest-limit: 64 mixed containers with scalar accepted')
      call capnp_message_free(msg)
      js = '{"x":'//repeat('[', 64)//repeat(']', 64)//'}'
      call capnp_json_decode(schema, idx, js, msg, err)
      call check_(error, err == CAPNP_ERR_DEPTH, 'nest-limit: 65 mixed containers rejected')
      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end subroutine run_json_nest_limit

   !> JSON stream splitting recognizes punctuation inside quoted strings.
   !> Cap'n Proto 1.4.0 readOneJson does not enter its quote state and
   !> rejects these valid streams.
   subroutine run_cli_json_stream(error)
      type(error_type), allocatable, intent(out) :: error
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp)) then
         call test_failed(error, 'mkdir')
         return
      end if
      call check_stream('url', '{"display_name":"http://x"}'//achar(10)//'{"big":"3"}')
      call check_stream('hash', '{"display_name":"#x"}'//achar(10)//'{"big":"3"}')
      call check_stream('brace', '{"display_name":"{"}'//achar(10)//'{"big":"3"}')
   contains
      subroutine check_stream(label, payload)
         character(len=*), intent(in) :: label, payload
         integer :: e2
         if (allocated(error)) return
         call spit(tmp//'/js.in', payload, e2)
         if (e2 /= CAPNP_OK) then
            call test_failed(error, 'cli-json-stream: write '//label)
            return
         end if
         if (.not. sh(cap_setup() &
            //' && "$CAP" convert json:json --short schema/codec.capnp Sample' &
            //' < '//tmp//'/js.in > '//tmp//'/js.out 2>'//tmp//'/js.ferr' &
            //' && test "$(wc -l < '//tmp//'/js.out)" = 2' &
            //' && grep -q ''"big":"3"'' '//tmp//'/js.out')) then
            call test_failed(error, 'cli-json-stream: valid '//label)
         end if
      end subroutine check_stream
   end subroutine run_cli_json_stream



   subroutine run_text_shorthand(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema, book
      integer :: sample, garm, people
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      call load_codec(error, schema)
      if (allocated(error)) return
      sample = capnp_dyn_find(schema, 'Sample', people)
      garm = capnp_dyn_find(schema, 'GroupArm', people)
      call check_(error, sample > 0 .and. garm > 0, 'shorthand: schema')
      call round_text(schema, garm, '(box = 5)', 'x = 5')
      call round_text(schema, sample, '(job = void)', 'none = void')
      call round_text(schema, sample, '(where = "a (b) # c")', 'a (b) # c')
      call round_text(schema, sample, '(where = ("hi"))', 'city = "hi"')
      call round_text(schema, garm, '(box = (5))', 'x = 5')
      call round_text(schema, sample, '(where = (("hi")))', 'city = "hi"')
      call reject_text(schema, garm, '(box = "no")')
      call reject_text(schema, garm, '(box = void)')
      call reject_text(schema, sample, '(where = void)')
      call reject_text(schema, sample, '(job = "hi")')
      call reject_text(schema, sample, '(where = (1, 2))')
      call load_people(error, book, people)
      call round_text(book, people, '(people = [1, 2])', 'id = 1')
      call round_text(book, people, '(people = [(1), (2)])', 'id = 2')
      call reject_text(book, people, '(people = [1, "no"])')
      call capnp_dyn_free(schema)
      call capnp_dyn_free(book)
   contains
      subroutine round_text(sch, idx, text, frag)
         type(capnp_dyn_schema_t), intent(in) :: sch
         integer, intent(in) :: idx
         character(len=*), intent(in) :: text, frag
         type(capnp_message_t) :: msg
         type(capnp_ptr_t) :: root
         character(len=:), allocatable :: out
         integer :: err
         if (allocated(error) .or. idx <= 0) return
         call capnp_text_decode(sch, idx, text, msg, err)
         call check_(error, err == CAPNP_OK, 'shorthand decode '//text)
         if (allocated(error)) then
            call capnp_message_free(msg)
            return
         end if
         root = capnp_root(msg, err)
         call capnp_text_encode(sch, idx, root, out, err, pretty=.false.)
         call check_(error, err == CAPNP_OK .and. index(out, frag) > 0, &
                     'shorthand encode '//text)
         call capnp_message_free(msg)
      end subroutine round_text

      subroutine reject_text(sch, idx, text)
         type(capnp_dyn_schema_t), intent(in) :: sch
         integer, intent(in) :: idx
         character(len=*), intent(in) :: text
         type(capnp_message_t) :: msg
         integer :: err
         if (allocated(error) .or. idx <= 0) return
         call capnp_text_decode(sch, idx, text, msg, err)
         call check_(error, err /= CAPNP_OK, 'shorthand reject '//text)
         call capnp_message_free(msg)
      end subroutine reject_text
   end subroutine run_text_shorthand


   subroutine load_people(error, schema, idx)
      type(error_type), allocatable, intent(inout) :: error
      type(capnp_dyn_schema_t), intent(out) :: schema
      integer, intent(out) :: idx
      character(len=:), allocatable :: src
      integer(int8), allocatable :: bytes(:)
      integer :: err
      idx = 0
      if (allocated(error)) return
      src = '@0x9eb32e19f86ee174;'//achar(10)// &
            'struct Person { id @0 :UInt32; name @1 :Text; }'//achar(10)// &
            'struct Book { people @0 :List(Person); }'//achar(10)
      call spit(tmp//'/people.capnp', src, err)
      if (.not. sh('capnp compile -o- '//tmp//'/people.capnp > '//tmp//'/people.cgr')) then
         call test_failed(error, 'shorthand: compile people')
         return
      end if
      call capnp_read_file(tmp//'/people.cgr', bytes, err)
      call capnp_dyn_load(schema, bytes, err)
      idx = capnp_dyn_find(schema, 'Book', err)
      if (err /= CAPNP_OK .or. idx <= 0) call test_failed(error, 'shorthand: book')
   end subroutine load_people


   subroutine run_text_group_replace(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      integer :: sample, garm, err
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      call load_codec(error, schema)
      if (allocated(error)) return
      sample = capnp_dyn_find(schema, 'Sample', err)
      garm = capnp_dyn_find(schema, 'GroupArm', err)
      call show_has(schema, sample, '(where = (city = "a"), where = (zip = 5))', 'zip = 5')
      call show_lacks(schema, sample, '(where = (city = "a"), where = (zip = 5))', 'city =')
      call show_has(schema, sample, '(where = (zip = 1), where = (city = "a"))', 'zip = 411')
      call show_has(schema, sample, '(where = (zip = 1), where = (city = "a"))', 'city = "a"')
      call show_has(schema, garm, '(box = (y = 7), box = ())', 'y = 0')
      call show_has(schema, garm, '(box = (x = 1, y = 2), box = (y = 3))', 'x = 0')
      call show_has(schema, garm, '(box = (x = 1, y = 2), box = (y = 3))', 'y = 3')
      call capnp_dyn_free(schema)
   contains
      subroutine show_has(sch, idx, text, frag)
         type(capnp_dyn_schema_t), intent(in) :: sch
         integer, intent(in) :: idx
         character(len=*), intent(in) :: text, frag
         character(len=:), allocatable :: out
         call render(sch, idx, text, out)
         call check_(error, index(out, frag) > 0, 'group replace has '//frag)
      end subroutine show_has

      subroutine show_lacks(sch, idx, text, frag)
         type(capnp_dyn_schema_t), intent(in) :: sch
         integer, intent(in) :: idx
         character(len=*), intent(in) :: text, frag
         character(len=:), allocatable :: out
         call render(sch, idx, text, out)
         call check_(error, index(out, frag) == 0, 'group replace lacks '//frag)
      end subroutine show_lacks

      subroutine render(sch, idx, text, out)
         type(capnp_dyn_schema_t), intent(in) :: sch
         integer, intent(in) :: idx
         character(len=*), intent(in) :: text
         character(len=:), allocatable, intent(out) :: out
         type(capnp_message_t) :: msg
         type(capnp_ptr_t) :: root
         integer :: e2
         out = ''
         if (allocated(error) .or. idx <= 0) return
         call capnp_text_decode(sch, idx, text, msg, e2)
         if (e2 /= CAPNP_OK) then
            call test_failed(error, 'group replace decode')
            call capnp_message_free(msg)
            return
         end if
         root = capnp_root(msg, e2)
         call capnp_text_encode(sch, idx, root, out, e2, pretty=.false.)
         if (e2 /= CAPNP_OK) call test_failed(error, 'group replace encode')
         call capnp_message_free(msg)
      end subroutine render
   end subroutine run_text_group_replace


   subroutine run_text_ws_vt_ff(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      integer :: idx, err
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      call load_codec(error, schema)
      if (allocated(error)) return
      idx = capnp_dyn_find(schema, 'Sample', err)
      call one(achar(11)//'(big = -88)', '"-88"')
      call one('(big = 55, title = "hi", hue ='//achar(12)//'red)', 'display_name')
      call capnp_dyn_free(schema)
   contains
      subroutine one(text, frag)
         character(len=*), intent(in) :: text, frag
         type(capnp_message_t) :: msg
         type(capnp_ptr_t) :: root
         character(len=:), allocatable :: out
         integer :: e2
         if (allocated(error)) return
         call capnp_text_decode(schema, idx, text, msg, e2)
         call check_(error, e2 == CAPNP_OK, 'vt ff decode')
         if (allocated(error)) then
            call capnp_message_free(msg)
            return
         end if
         root = capnp_root(msg, e2)
         call capnp_json_encode(schema, idx, root, out, e2, pretty=.false.)
         call check_(error, e2 == CAPNP_OK .and. index(out, frag) > 0, 'vt ff json '//frag)
         call capnp_message_free(msg)
      end subroutine one
   end subroutine run_text_ws_vt_ff


   subroutine run_text_inactive_arm(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t) :: msg
      type(capnp_ptr_t) :: root
      character(len=:), allocatable :: out
      integer(int8) :: raw(0:31)
      integer :: idx, err
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      raw = [0_int8, 0_int8, 0_int8, 0_int8, 3_int8, 0_int8, 0_int8, 0_int8, &
             0_int8, 0_int8, 0_int8, 0_int8, 1_int8, 0_int8, 1_int8, 0_int8, &
             0_int8, 0_int8, 0_int8, 0_int8, 0_int8, 0_int8, 0_int8, 0_int8, &
             31_int8, 0_int8, 0_int8, 0_int8, -1_int8, -1_int8, -1_int8, -1_int8]
      call load_codec(error, schema)
      if (allocated(error)) return
      idx = capnp_dyn_find(schema, 'InnerDisc', err)
      call capnp_deserialize_bytes(raw, msg, err)
      root = capnp_root(msg, err)
      call capnp_text_encode(schema, idx, root, out, err, pretty=.false.)
      call check_(error, err == CAPNP_OK, 'inactive arm encodes')
      call check_(error, index(out, 'a = 0') > 0, 'inactive arm text')
      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end subroutine run_text_inactive_arm


   subroutine run_json_dup_bad(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      integer :: idx, err
      call load_codec(error, schema)
      if (allocated(error)) return
      idx = capnp_dyn_find(schema, 'Sample', err)
      call check_dup('{"big":"bad","big":"1"}', .true., '"big":"1"')
      call check_dup('{"big":"1","big":"bad"}', .true., '"big":"0"')
      call check_dup('{"big":"bad"}', .true., '"big":"0"')
      call check_dup('{"big":"1","big":"2"}', .false., '"big":"2"')
      call check_dup('{"big":{}}', .true., '')
      call capnp_dyn_free(schema)
   contains
      subroutine check_dup(text, bad, frag)
         character(len=*), intent(in) :: text, frag
         logical, intent(in) :: bad
         type(capnp_message_t) :: msg
         type(capnp_ptr_t) :: root
         character(len=:), allocatable :: out
         integer :: e2
         if (allocated(error)) return
         call capnp_json_decode(schema, idx, text, msg, e2)
         if (bad) then
            call check_(error, e2 /= CAPNP_OK, 'dup rejected '//text)
         else
            call check_(error, e2 == CAPNP_OK, 'dup accepted '//text)
         end if
         if (allocated(error) .or. len(frag) == 0) then
            call capnp_message_free(msg)
            return
         end if
         if (bad .and. .not. capnp_json_number_failed()) then
            call capnp_message_free(msg)
            return
         end if
         root = capnp_root(msg, e2)
         call capnp_json_encode(schema, idx, root, out, e2, pretty=.false.)
         call check_(error, e2 == CAPNP_OK .and. index(out, frag) > 0, 'dup value '//text)
         call capnp_message_free(msg)
      end subroutine check_dup
   end subroutine run_json_dup_bad


end module test_regress
