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
      ! One assignment per case. An array constructor of these values
      ! leaves the case-name temporaries allocated on gfortran.
      allocate (testsuite(62))
      testsuite(1) = new_unittest("comment-eof", run_comment_eof)
      testsuite(2) = new_unittest("segment-bomb", run_segment_bomb)
      testsuite(3) = new_unittest("binary-stdout", run_binary_stdout)
      testsuite(4) = new_unittest("list-linear", run_list_linear)
      testsuite(5) = new_unittest("canonical-release", run_canonical_release)
      testsuite(6) = new_unittest("json-collisions", run_json_collisions)
      testsuite(7) = new_unittest("json-void-and-missing", run_json_void_missing)
      testsuite(8) = new_unittest("json-surrogate", run_json_surrogate)
      testsuite(9) = new_unittest("json-exact-int", run_json_exact_int)
      testsuite(10) = new_unittest("json-null-disc", run_json_null_disc)
      testsuite(11) = new_unittest("json-union-diff", run_json_union_diff)
      testsuite(12) = new_unittest("text-leniency", run_text_leniency)
      testsuite(13) = new_unittest("unloaded-guard", run_unloaded)
      testsuite(14) = new_unittest("json-unloaded-guard", run_json_unloaded)
      testsuite(15) = new_unittest("long-argument", run_long_arg)
      testsuite(16) = new_unittest("schema-pipe", run_schema_pipe)
      testsuite(17) = new_unittest("plugin-release", run_plugin_release)
      testsuite(18) = new_unittest("cli-locate", run_cli_locate)
      testsuite(19) = new_unittest("eval-annotations", run_eval_annotations)
      testsuite(20) = new_unittest("string-linear", run_string_linear)
      testsuite(21) = new_unittest("write-error", run_write_error)
      testsuite(22) = new_unittest("f32-text", run_f32_text)
      testsuite(23) = new_unittest("regress-tmp", run_regress_tmp)
      testsuite(24) = new_unittest("cli-sources", run_cli_sources)
      testsuite(25) = new_unittest("json-union-many", run_json_union_many)
      testsuite(26) = new_unittest("json-union-nested", run_json_union_nested)
      testsuite(27) = new_unittest("json-schema-chain", run_json_schema_chain)
      testsuite(28) = new_unittest("group-clear-depth", run_group_clear_depth)
      testsuite(29) = new_unittest("packed-zero-cap", run_packed_zero_cap)
      testsuite(30) = new_unittest("json-trailing-spaces", run_json_trailing_spaces)
      testsuite(31) = new_unittest("json-union-clear", run_json_union_clear)
      testsuite(32) = new_unittest("json-nul-eof", run_json_nul_eof)
      testsuite(33) = new_unittest("json-nest-limit", run_json_nest_limit)
      testsuite(34) = new_unittest("cli-json-stream", run_cli_json_stream)
      testsuite(35) = new_unittest("cli-text-nul", run_cli_text_nul)
      testsuite(36) = new_unittest("text-shorthand", run_text_shorthand)
      testsuite(37) = new_unittest("text-group-replace", run_text_group_replace)
      testsuite(38) = new_unittest("text-ws-vt-ff", run_text_ws_vt_ff)
      testsuite(39) = new_unittest("json-dup-bad", run_json_dup_bad)
      testsuite(40) = new_unittest("canonical-null-root", run_canonical_null_root)
      testsuite(41) = new_unittest("json-ptr-kind", run_json_ptr_kind)
      testsuite(42) = new_unittest("bin-ptr-kind", run_bin_ptr_kind)
      testsuite(43) = new_unittest("text-inactive-arm", run_text_inactive_arm)
      testsuite(44) = new_unittest("codec-parity", run_codec_parity)
      testsuite(45) = new_unittest("shorthand-first-field", run_shorthand_first)
      testsuite(46) = new_unittest("stream-pointer-kind", run_stream_kind)
      testsuite(47) = new_unittest("cycle-guard", run_cycle_guard)
      testsuite(48) = new_unittest("quiet-empty", run_quiet_empty)
      testsuite(49) = new_unittest("json-value", run_json_value)
      testsuite(50) = new_unittest("void-list-json", run_void_list_json)
      testsuite(51) = new_unittest("schema-lenient", run_schema_lenient)
      testsuite(52) = new_unittest("flat-tail", run_flat_tail)
      testsuite(53) = new_unittest("null-root-wire", run_null_root_wire)
      testsuite(54) = new_unittest("json-value-object", run_json_value_object)
      testsuite(55) = new_unittest("json-value-depth", run_json_value_depth)
      testsuite(56) = new_unittest("text-plausible", run_text_plausible)
      testsuite(57) = new_unittest("canonical-reject", run_canonical_reject)
      testsuite(58) = new_unittest("request-beside-exe", run_request_beside_exe)
      testsuite(59) = new_unittest("generic-root", run_generic_root)
      testsuite(60) = new_unittest("zero-size-list", run_zero_size_list)
      testsuite(61) = new_unittest("cli-options", run_cli_options)
      testsuite(62) = new_unittest("cap-pointers", run_cap_pointers)
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
      st = 0
      cs = 0
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
                    9007199254740992_int64, 'exact: 2^53+1 rounds')
      call read_int(error, schema, idx, '{"big":-9007199254740993}', 'big', &
                    -9007199254740992_int64, 'exact: -(2^53+1) rounds')
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
      call check_(error, err == CAPNP_OK .and. v == 9007199254740992_int64, &
                  'exact: list element rounds')
      call capnp_message_free(decoded)
      call reject_json(error, schema, idx, '{"big":"+0x10"}', 'exact: reject +0x10')
      call reject_json(error, schema, idx, '{"ubig":"-0"}', 'exact: reject quoted -0')
      call reject_json(error, schema, idx, '{"big":1.5}', 'exact: reject 1.5')
      call reject_json(error, schema, idx, '{"where":{"zip":65536}}', 'exact: reject uint16')
      call reject_json(error, schema, idx, '{"big":9223372036854775807}', &
                       'exact: reject unquoted int64 max')
      call reject_json(error, schema, idx, '{"ubig":18446744073709551615}', &
                       'exact: reject unquoted uint64 max')
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

   !> eval drops annotations, with and without the helper.
   subroutine run_eval_annotations(error)
      type(error_type), allocatable, intent(out) :: error
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp//'/eval-annotations && '//cap_setup() &
         //' && export CAP TMPD='//tmp//'/eval-annotations INC=$(dirname $(dirname $(command -v capnp)))/include' &
         //' && python3 - <<''PY'''//achar(10)// &
         'import os, shutil, subprocess, sys'//achar(10)// &
         'cap = os.environ["CAP"]'//achar(10)// &
         'tmp = os.environ["TMPD"]'//achar(10)// &
         'inc = os.environ["INC"]'//achar(10)// &
         'def run(tool, args, cwd, env=None):'//achar(10)// &
         '    p = subprocess.run([tool, *args], stdout=subprocess.PIPE, stderr=subprocess.PIPE,'//achar(10)// &
         '                       cwd=cwd, env=env)'//achar(10)// &
         '    return p.returncode, p.stdout'//achar(10)// &
         'with open(os.path.join(tmp, "ann.capnp"), "w") as f:'//achar(10)// &
         '    f.write(''@0x8123456789abcde0;\n'''//achar(10)// &
         '            ''using Json = import "/capnp/compat/json.capnp";\n'''//achar(10)// &
         '            ''struct S {\n'''//achar(10)// &
         '            ''  foo @0 :Int32 $Json.name("renamedFoo");\n'''//achar(10)// &
         '            ''  raw @1 :Data $Json.hex;\n'''//achar(10)// &
         '            ''}\n'''//achar(10)// &
         '            ''const s :S = (foo = 42, raw = 0x"deadbeef");\n'')'//achar(10)// &
         'bare = os.path.join(tmp, "bare")'//achar(10)// &
         'os.makedirs(os.path.join(bare, "bin"), exist_ok=True)'//achar(10)// &
         'shutil.copy(cap, os.path.join(bare, "bin", "capnpf"))'//achar(10)// &
         'env = {k: v for k, v in os.environ.items() if k != "CAPNP_SCHEMA_REQUEST"}'//achar(10)// &
         'for args in (["eval", "-I", inc, "-o", "json", "ann.capnp", "s"],'//achar(10)// &
         '             ["eval", "-I", inc, "-o", "text", "ann.capnp", "s"],'//achar(10)// &
         '             ["eval", "-I", inc, "ann.capnp", "s"]):'//achar(10)// &
         '    ref = run("capnp", args, tmp)'//achar(10)// &
         '    if b"renamedFoo" in ref[1] or ref != run(cap, args, tmp):'//achar(10)// &
         '        print("mismatch:", args)'//achar(10)// &
         '        sys.exit(1)'//achar(10)// &
         '    if ref != run(os.path.join(bare, "bin", "capnpf"), args, tmp, env):'//achar(10)// &
         '        print("fallback mismatch:", args)'//achar(10)// &
         '        sys.exit(1)'//achar(10)// &
         'PY')) then
         call test_failed(error, 'eval annotations')
      end if
   end subroutine run_eval_annotations

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
         'root = Path("'//tmp//'")'//achar(10)// &
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
         //' && san=; if [ -n "${ASAN_OPTIONS:-}" ] || nm -A "$lib" 2>/dev/null | grep -q " __asan";' &
         //' then san="-fsanitize=address,undefined"; fi' &
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
         //' && san=; if [ -n "${ASAN_OPTIONS:-}" ] || nm -A "$lib" 2>/dev/null | grep -q " __asan";' &
         //' then san="-fsanitize=address,undefined"; fi' &
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
      if (.not. sh('lib=$(find build -name libcapnp.a | xargs ls -t | head -1)' &
         //' && test -n "$lib" && ! nm -A "$lib" 2>/dev/null | grep -q " __asan"')) return
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
         //' && san=; if [ -n "${ASAN_OPTIONS:-}" ] || nm -A "$lib" 2>/dev/null | grep -q " __asan";' &
         //' then san="-fsanitize=address,undefined"; fi' &
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
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
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

   !> A NUL in a string or comment is text, as for capnp 1.4.0. One its
   !> lexer cannot read fails that message before it prints.
   subroutine run_cli_text_nul(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: nl = achar(10), nul = achar(0)
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp)) then
         call test_failed(error, 'mkdir')
         return
      end if
      call check_nul('string', '(title = "ab'//nul//'cd")'//nl)
      call check_nul('comment', '(big = 3) # c'//nul//nl//'(big = 4)'//nl)
      call check_nul('trailing', '(big = 3)'//nul//nl)
      call check_nul('between', '(big = 3)'//nl//nul//'(big = 4)'//nl)
      call check_nul('second', '(big = 3)'//nl//'(big = 4'//nul//')'//nl)
      call check_nul('escape', '(title = "a\'//nul//'")'//nl)
      call check_nul('quote', "(title = 'a"//nul//"b')"//nl)
      call check_nul('hex', '(blob = 0x"'//nul//'")'//nl)
   contains
      subroutine check_nul(label, payload)
         character(len=*), intent(in) :: label, payload
         integer :: e2
         if (allocated(error)) return
         call spit(tmp//'/tn.in', payload, e2)
         if (e2 /= CAPNP_OK) then
            call test_failed(error, 'cli-text-nul: write '//label)
            return
         end if
         if (.not. sh(cap_setup() &
            //' && { "$CAP" convert --quiet text:text schema/codec.capnp Sample' &
            //' < '//tmp//'/tn.in > '//tmp//'/tn.o 2>/dev/null; echo $? >> '//tmp//'/tn.o; }' &
            //' && { capnp convert --quiet text:text schema/codec.capnp Sample' &
            //' < '//tmp//'/tn.in > '//tmp//'/tn.r 2>/dev/null; echo $? >> '//tmp//'/tn.r; }' &
            //' && cmp -s '//tmp//'/tn.o '//tmp//'/tn.r')) then
            call test_failed(error, 'cli-text-nul: '//label)
         end if
      end subroutine check_nul
   end subroutine run_cli_text_nul

   subroutine run_text_shorthand(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema, book, extra
      integer :: sample, garm, people, place, onefield, deepi, holdi, wrapi, bagi
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
      call round_text(schema, sample, '(where = ("hi",))', 'city = "hi"')
      call round_text(schema, sample, '(where = (("hi",)))', 'city = "hi"')
      call round_text(schema, sample, '(where = (city = "a", zip = 9), where = ("b",))', 'city = "b"')
      call round_text(schema, sample, '(job = (label = "hi"), job = (void,))', 'none = void')
      call round_text(schema, garm, '(box = (5,))', 'x = 5')
      call reject_text(schema, garm, '(box = "no")')
      call reject_text(schema, garm, '(box = void)')
      call reject_text(schema, sample, '(where = void)')
      call reject_text(schema, sample, '(job = "hi")')
      call reject_text(schema, sample, '(where = (1, 2))')
      call reject_text(schema, sample, '(where = (zip = 5, "hi"))')
      call reject_text(schema, sample, '(ubig = 2, 1)')
      call reject_text(schema, sample, '(big = 1, 2)')
      call reject_text(schema, garm, '(box = (y = 7, 5))')
      call reject_text(schema, sample, '(1)')
      call reject_text(schema, garm, '(-1)')
      place = capnp_dyn_find(schema, 'Place', people)
      call reject_text(schema, place, '(("a (b) # c"),)')
      call load_people(error, book, people)
      call round_text(book, people, '(people = [1, 2])', 'id = 1')
      call round_text(book, people, '(people = [(1), (2)])', 'id = 2')
      call round_text(book, people, '(people = [(1,)])', 'id = 1')
      call round_text(book, people, '(people = [((1,))])', 'id = 1')
      call reject_text(book, people, '(people = [1, "no"])')
      call reject_text(book, people, '(people = [(name = "A", 1)])')
      call load_short_extra(error)
      call reject_text(extra, onefield, '(5)')
      call reject_text(extra, deepi, '(deep = 5)')
      call reject_text(extra, deepi, '(deep = (5))')
      call round_text(extra, deepi, '(deep = (inner = (v = 5)))', 'v = 5')
      call reject_text(extra, holdi, '(nu = 5)')
      call round_text(extra, holdi, '(nu = (u = (a = 5)))', 'a = 5')
      call round_text(extra, wrapi, '(g = (5,))', 'v = 5')
      call round_text(extra, wrapi, '(s = (5,))', 's = (v = 5)')
      call round_text(extra, wrapi, '(o = (5,))', 'o = (v = 5)')
      call round_text(extra, wrapi, '(g = 5)', 'v = 5')
      call round_text(extra, wrapi, '(s = 5)', 's = (v = 5)')
      call reject_text(extra, bagi, '(s1 = 5)')
      call reject_text(extra, bagi, '(s1 = (5))')
      call reject_text(extra, bagi, '(sl = [1, 2])')
      call reject_text(extra, bagi, '(se = red)')
      call reject_text(extra, bagi, '(sd = "ab")')
      call reject_text(extra, bagi, '(ls1 = [5])')
      call reject_text(extra, bagi, '(g = 5)')
      call reject_text(extra, bagi, '(sll = [[1]])')
      call round_text(extra, bagi, '(one = 5)', 'one = (v = 5)')
      call round_text(extra, bagi, '(st = "ab")', 't = "ab"')
      call capnp_dyn_free(extra)
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

      subroutine load_short_extra(err_out)
         type(error_type), allocatable, intent(inout) :: err_out
         character(len=:), allocatable :: src
         integer(int8), allocatable :: bytes(:)
         integer :: err
         if (allocated(err_out)) return
         src = '@0x8122334455667788;'//achar(10)// &
               'struct OneField { v @0 :Int32; }'//achar(10)// &
               'struct Deep { deep :group { inner :group { v @0 :Int32; } } }'//achar(10)// &
               'struct Named { u :union { a @0 :Int32; b @1 :Text; } }'//achar(10)// &
               'struct Hold { nu @0 :Named; }'//achar(10)// &
               'struct Wrap { g :group { v @0 :Int32; } s @1 :OneField; o @2 :OneField; }'//achar(10)// &
               'struct Inner { v @0 :Int32; }'//achar(10)// &
               'struct S1 { i @0 :Inner; }'//achar(10)// &
               'struct SL { l @0 :List(Int32); }'//achar(10)// &
               'enum Hue { red @0; blue @1; }'//achar(10)// &
               'struct SE { e @0 :Hue; }'//achar(10)// &
               'struct SD { d @0 :Data; }'//achar(10)// &
               'struct ST { t @0 :Text; }'//achar(10)// &
               'struct SLL { ll @0 :List(List(Int32)); }'//achar(10)// &
               'struct GS { g :group { i @0 :Inner; } }'//achar(10)// &
               'struct Bag { s1 @0 :S1; sl @1 :SL; se @2 :SE; sd @3 :SD;'// &
               ' st @4 :ST; one @5 :OneField; ls1 @6 :List(S1);'// &
               ' sll @7 :SLL; g @8 :GS; }'//achar(10)
         call spit(tmp//'/short.capnp', src, err)
         if (.not. sh('capnp compile -o- '//tmp//'/short.capnp > '//tmp//'/short.cgr')) then
            call test_failed(err_out, 'shorthand: compile extra')
            return
         end if
         call capnp_read_file(tmp//'/short.cgr', bytes, err)
         call capnp_dyn_load(extra, bytes, err)
         onefield = capnp_dyn_find(extra, 'OneField', err)
         deepi = capnp_dyn_find(extra, 'Deep', err)
         holdi = capnp_dyn_find(extra, 'Hold', err)
         wrapi = capnp_dyn_find(extra, 'Wrap', err)
         bagi = capnp_dyn_find(extra, 'Bag', err)
         call check_(err_out, onefield > 0 .and. deepi > 0 .and. holdi > 0 .and. wrapi > 0 &
                     .and. bagi > 0, 'shorthand: extra schema')
      end subroutine load_short_extra
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

   subroutine run_json_dup_bad(error)
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
      call check_dup('{"big":"bad","big":"1"}', .true., '"big":"1"')
      call check_dup('{"big":"1","big":"bad"}', .true., '"big":"0"')
      call check_dup('{"big":"bad"}', .true., '"big":"0"')
      call check_dup('{"big":"1","big":"2"}', .false., '"big":"2"')
      call check_dup('{"big":{}}', .true., '')
      call check_dup('{"big":"bad","big":{}}', .true., '')
      call check_quiet('{"big":"bad","big":{}}')
      call check_stream('{"big":"bad"}{"big":"2"}')
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

      subroutine check_quiet(text)
         character(len=*), intent(in) :: text
         type(capnp_message_t) :: msg
         integer :: e2
         if (allocated(error)) return
         call capnp_json_decode(schema, idx, text, msg, e2)
         call check_(error, e2 /= CAPNP_OK, 'hard error after a bad number')
         call check_(error, .not. capnp_json_number_failed(), &
                     'hard error clears the number flag')
         call capnp_message_free(msg)
      end subroutine check_quiet

      subroutine check_stream(payload)
         character(len=*), intent(in) :: payload
         integer :: e2
         if (allocated(error)) return
         call spit(tmp//'/num.in', payload, e2)
         if (e2 /= CAPNP_OK) then
            call test_failed(error, 'number stream: write')
            return
         end if
         if (.not. sh(cap_setup() &
            //' && INC=$(dirname $(dirname $(command -v capnp)))/include' &
            //' && capnp convert -I"$INC" json:text --short schema/codec.capnp Sample' &
            //' < '//tmp//'/num.in > '//tmp//'/num.ref 2>/dev/null; echo $? > '//tmp//'/num.ref.rc' &
            //' && "$CAP" convert -I"$INC" json:text --short schema/codec.capnp Sample' &
            //' < '//tmp//'/num.in > '//tmp//'/num.ours 2>/dev/null; echo $? > '//tmp//'/num.ours.rc' &
            //' && cmp -s '//tmp//'/num.ref '//tmp//'/num.ours' &
            //' && cmp -s '//tmp//'/num.ref.rc '//tmp//'/num.ours.rc' &
            //' && grep -qx 1 '//tmp//'/num.ours.rc')) then
            call test_failed(error, 'number stream does not match capnp')
         end if
      end subroutine check_stream
   end subroutine run_json_dup_bad

   subroutine run_canonical_null_root(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t) :: msg
      integer(int8), allocatable :: bytes(:)
      integer(int8) :: raw(0:15), want(0:7)
      integer :: err
      raw = [0_int8, 0_int8, 0_int8, 0_int8, 1_int8, 0_int8, 0_int8, 0_int8, &
             0_int8, 0_int8, 0_int8, 0_int8, 0_int8, 0_int8, 0_int8, 0_int8]
      want = [-4_int8, -1_int8, -1_int8, -1_int8, 0_int8, 0_int8, 0_int8, 0_int8]
      call capnp_deserialize_bytes(raw, msg, err)
      call capnp_canonicalize(msg, bytes, err)
      call check_(error, err == CAPNP_OK .and. allocated(bytes), 'null root canonical')
      if (.not. allocated(error) .and. allocated(bytes)) then
         call check_(error, size(bytes) == 8 .and. all(bytes == want), &
                     'null root bytes')
      end if
      call capnp_message_free(msg)
   end subroutine run_canonical_null_root

   subroutine run_json_ptr_kind(error)
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
             0_int8, 0_int8, 0_int8, 32_int8, 0_int8, 0_int8, 4_int8, 0_int8, &
             1_int8, 0_int8, 0_int8, 0_int8, 0_int8, 0_int8, 0_int8, 0_int8]
      call load_codec(error, schema)
      if (allocated(error)) return
      idx = capnp_dyn_find(schema, 'NestedFlatStructDisc', err)
      call capnp_deserialize_bytes(raw, msg, err)
      root = capnp_root(msg, err)
      call capnp_json_encode(schema, idx, root, out, err, pretty=.false.)
      call check_(error, err == CAPNP_ERR_KIND, 'ptr kind status')
      call check_(error, index(out, '"outerKind":"g"') > 0, 'ptr kind text')
      call check_(error, index(out, '"innerVal":0') > 0, 'ptr kind default')
      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end subroutine run_json_ptr_kind

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

   !> A pointer of the wrong kind still prints the recovered value and fails.
   subroutine run_bin_ptr_kind(error)
      type(error_type), allocatable, intent(out) :: error
      integer :: err
      character(len=*), parameter :: nest = &
         '0000000005000000000000000100010000000000000000000300000001000100' &
         //'07000000000000000000000000000000'
      character(len=*), parameter :: people = &
         '000000000f0000000000000000000100030000002f0000000400000001000400' &
         //'01000000020000000d000000120000000d000000220000000d00000017000000' &
         //'1900000012000000410000000000000061406200000000000400000001000100' &
         //'0000000000000000010000002200000031323300000000005800000000008000'
      character(len=*), parameter :: wrong = &
         '0000000005000000000000000000030001000000000000000400000001000000' &
         //'00000000010000000000000000000000'
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp)) then
         call test_failed(error, 'mkdir')
         return
      end if
      call cmp_bin('schema/codec.capnp', 'NestedFlatStructDisc', 'text', nest, 'struct cap text')
      call cmp_bin('schema/addressbook.capnp', 'AddressBook', 'json', people, 'list cap json')
      call cmp_bin('schema/addressbook.capnp', 'AddressBook', 'text', people, 'list cap text')
      call spit(tmp//'/wrong.capnp', &
                '@0xabcdeffedcba0011;'//achar(10)// &
                'struct Child { n @0 :Int32; }'//achar(10)// &
                'struct MixedKinds { s @0 :Child; li @1 :List(Int32); ls @2 :List(Child); }' &
                //achar(10), err)
      if (err /= 0) then
         call test_failed(error, 'wrong ptr schema')
         return
      end if
      call cmp_bin(tmp//'/wrong.capnp', 'MixedKinds', 'text', wrong, 'list in struct text')
      call cmp_bin(tmp//'/wrong.capnp', 'MixedKinds', 'json', wrong, 'struct in list json')
   contains
      subroutine cmp_bin(schema, typ, fmt, hex, label)
         character(len=*), intent(in) :: schema, typ, fmt, hex, label
         integer :: e2
         if (allocated(error)) return
         call spit(tmp//'/pk.hex', hex, e2)
         if (e2 /= CAPNP_OK) then
            call test_failed(error, 'ptr kind write '//label)
            return
         end if
         if (.not. sh(cap_setup() &
            //' && INC=$(dirname $(dirname $(command -v capnp)))/include' &
            //' && python3 -c "open('''//tmp//'/pk.bin'',''wb'').write(bytes.fromhex(open(''' &
            //tmp//'/pk.hex'').read()))"' &
            //' && capnp convert -I"$INC" binary:'//fmt//' --short '//schema//' '//typ &
            //' < '//tmp//'/pk.bin > '//tmp//'/pk.ref 2>/dev/null; echo $? > '//tmp//'/pk.ref.rc' &
            //' && "$CAP" convert -I"$INC" binary:'//fmt//' --short '//schema//' '//typ &
            //' < '//tmp//'/pk.bin > '//tmp//'/pk.ours 2>/dev/null; echo $? > '//tmp//'/pk.ours.rc' &
            //' && cmp -s '//tmp//'/pk.ref '//tmp//'/pk.ours' &
            //' && cmp -s '//tmp//'/pk.ref.rc '//tmp//'/pk.ours.rc' &
            //' && grep -qx 1 '//tmp//'/pk.ours.rc')) then
            call test_failed(error, 'ptr kind '//label)
         end if
      end subroutine cmp_bin
   end subroutine run_bin_ptr_kind

   !> Conversions, lists, and constants compared with capnp 1.4.0.
   subroutine run_codec_parity(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=:), allocatable :: src
      integer :: err
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp)) then
         call test_failed(error, 'mkdir')
         return
      end if
      src = '@0x8011223344556690;'//achar(10)// &
            'struct Wire { l32 @0 :List(Int32); bl @1 :List(Bool);' &
            //' tx @2 :List(Text); vd @3 :List(Void); }'//achar(10)// &
            'struct Num { big @0 :Int64; ubig @1 :UInt64; i8 @2 :Int8;' &
            //' u8 @3 :UInt8; u16 @4 :UInt16; scores @5 :List(Int64);' &
            //' people @6 :List(Text); }'//achar(10)// &
            'struct Inner { v @0 :Int32; }'//achar(10)// &
            'struct S1 { i @0 :Inner; }'//achar(10)// &
            'struct SL { l @0 :List(Int32); }'//achar(10)// &
            'enum Hue { red @0; blue @1; }'//achar(10)// &
            'struct SE { e @0 :Hue; }'//achar(10)// &
            'struct SD { d @0 :Data; }'//achar(10)// &
            'struct ST { t @0 :Text; }'//achar(10)// &
            'struct One { v @0 :Int32; }'//achar(10)// &
            'struct Bag { s1 @0 :S1; sl @1 :SL; se @2 :SE; sd @3 :SD;' &
            //' st @4 :ST; one @5 :One; }'//achar(10)// &
            'struct Hold { u :union { a @0 :Int32; b @1 :Text; } }'//achar(10)// &
            'const on :Hold = (u = (a = 7));'//achar(10)// &
            'const off :Hold = (u = (b = "z"));'//achar(10)// &
            'const tiny :Float32 = 1.401298464324817e-45;'//achar(10)
      call spit(tmp//'/parity.capnp', src, err)
      if (err /= CAPNP_OK) then
         call test_failed(error, 'parity schema')
         return
      end if
      call spit(tmp//'/l32.hex', '00000000040000000000000000000100' &
                //'010000001500000001020304050607081112131415161718', err)
      call spit(tmp//'/bl.hex', '00000000040000000000000000000200' &
                //'000000000000000001000000220000000200000000000000', err)
      call spit(tmp//'/tx.hex', '00000000060000000000000000000300' &
                //'00000000000000000000000000000000010000000f000000' &
                //'04000000010000000000000000000000', err)
      call spit(tmp//'/vd.hex', '00000000060000000000000000000400' &
                //'000000000000000000000000000000000000000000000000' &
                //'01000000190000000700000000000000', err)
      call spit(tmp//'/vn.hex', '00000000060000000000000000000400' &
                //'000000000000000000000000000000000000000000000000' &
                //'010000001a0000000102030000000000', err)
      call spit(tmp//'/nr.hex', '00000000030000000000000000000100' &
                //'01000000130000000201040300000000', err)
      call spit(tmp//'/mkhex.py', 'from pathlib import Path'//achar(10)// &
                'd = Path("'//tmp//'")'//achar(10)// &
                'for name in ("l32", "bl", "tx", "vd", "vn", "nr"):'//achar(10)// &
                '    h = d.joinpath(name + ".hex").read_text().strip()'//achar(10)// &
                '    d.joinpath(name + ".bin").write_bytes(bytes.fromhex(h))'//achar(10), err)
      if (.not. sh(cap_setup()//' && export CAP && bash -s <<''END'''//achar(10)// &
         'set +e'//achar(10)// &
         'SCH='//tmp//'/parity.capnp'//achar(10)// &
         'bad=0'//achar(10)// &
         'cmp_in() {'//achar(10)// &
         '  printf "%s" "$3" | capnp convert "$1" --short "$SCH" "$2" > '//tmp//'/ref 2>/dev/null'//achar(10)// &
         '  echo $? > '//tmp//'/ref.rc'//achar(10)// &
         '  printf "%s" "$3" | "$CAP" convert "$1" --short "$SCH" "$2" > '//tmp//'/ours 2>/dev/null'//achar(10)// &
         '  echo $? > '//tmp//'/ours.rc'//achar(10)// &
         '  cmp -s '//tmp//'/ref '//tmp//'/ours && cmp -s '//tmp//'/ref.rc '//tmp//'/ours.rc || bad=1'//achar(10)// &
         '}'//achar(10)// &
         'empty_err() {'//achar(10)// &
         '  printf "" | capnp convert "$1" --short "$SCH" "$2" > '//tmp//'/ref 2>'//tmp//'/ref.err'//achar(10)// &
         '  echo $? > '//tmp//'/ref.rc'//achar(10)// &
         '  printf "" | "$CAP" convert "$1" --short "$SCH" "$2" > '//tmp//'/ours 2>'//tmp//'/ours.err'//achar(10)// &
         '  echo $? > '//tmp//'/ours.rc'//achar(10)// &
         '  test "$(cat '//tmp//'/ref.rc)" = 1 && test "$(cat '//tmp//'/ours.rc)" = 1 || bad=1'//achar(10)// &
         '  test -s '//tmp//'/ours.err || bad=1'//achar(10)// &
         '}'//achar(10)// &
         'empty_err text:text Bag'//achar(10)// &
         'empty_err json:text Bag'//achar(10)// &
         'empty_err text:json Bag'//achar(10)// &
         'empty_err binary:text Bag'//achar(10)// &
         'empty_err binary:json Bag'//achar(10)// &
         'empty_err packed:text Bag'//achar(10)// &
         'empty_err flat:text Bag'//achar(10)// &
         'empty_err canonical:text Bag'//achar(10)// &
         'empty_err binary:binary Bag'//achar(10)// &
         'empty_err json:binary Bag'//achar(10)// &
         'printf "" | capnp encode "$SCH" Num > '//tmp//'/ref 2>/dev/null'//achar(10)// &
         'echo $? > '//tmp//'/ref.rc'//achar(10)// &
         'printf "" | "$CAP" encode "$SCH" Num > '//tmp//'/ours 2>/dev/null'//achar(10)// &
         'echo $? > '//tmp//'/ours.rc'//achar(10)// &
         'cmp -s '//tmp//'/ref '//tmp//'/ours && cmp -s '//tmp//'/ref.rc '//tmp//'/ours.rc || bad=1'//achar(10)// &
         'printf "" | capnp decode "$SCH" Num > '//tmp//'/ref 2>/dev/null'//achar(10)// &
         'echo $? > '//tmp//'/ref.rc'//achar(10)// &
         'printf "" | "$CAP" decode "$SCH" Num > '//tmp//'/ours 2>/dev/null'//achar(10)// &
         'echo $? > '//tmp//'/ours.rc'//achar(10)// &
         'cmp -s '//tmp//'/ref '//tmp//'/ours && cmp -s '//tmp//'/ref.rc '//tmp//'/ours.rc || bad=1'//achar(10)// &
         'cmp_in text:text Bag "   "'//achar(10)// &
         'printf "(\0)" | capnp convert text:text --short "$SCH" Bag > '//tmp//'/ref 2>/dev/null'//achar(10)// &
         'echo $? > '//tmp//'/ref.rc'//achar(10)// &
         'printf "(\0)" | "$CAP" convert text:text --short "$SCH" Bag > '//tmp//'/ours 2>/dev/null'//achar(10)// &
         'echo $? > '//tmp//'/ours.rc'//achar(10)// &
         'cmp -s '//tmp//'/ref '//tmp//'/ours && cmp -s '//tmp//'/ref.rc '//tmp//'/ours.rc || bad=1'//achar(10)// &
         'cmp_in text:text Bag "(s1 = 5)"'//achar(10)// &
         'cmp_in text:text Bag "(sl = [1, 2])"'//achar(10)// &
         'cmp_in text:text Bag "(se = red)"'//achar(10)// &
         'cmp_in text:text Bag "(sd = \"ab\")"'//achar(10)// &
         'cmp_in text:text Bag "(one = 5)"'//achar(10)// &
         'cmp_in text:text Bag "(st = \"ab\")"'//achar(10)// &
         'cmp_in text:text Num "(i8 = 128)"'//achar(10)// &
         'cmp_in text:text Num "(i8 = -129)"'//achar(10)// &
         'cmp_in text:text Num "(u8 = 256)"'//achar(10)// &
         'cmp_in text:text Num "(big = 9223372036854775808)"'//achar(10)// &
         'cmp_in json:text Num "{\"big\":1.5}"'//achar(10)// &
         'cmp_in json:text Num "{\"big\":9.3e18}"'//achar(10)// &
         'cmp_in json:text Num "{\"big\":1e400}"'//achar(10)// &
         'cmp_in json:text Num "{\"ubig\":-1}"'//achar(10)// &
         'cmp_in json:text Num "{\"ubig\":1e20}"'//achar(10)// &
         'cmp_in json:text Num "{\"ubig\":18446744073709551616}"'//achar(10)// &
         'cmp_in json:text Num "{\"i8\":128}"'//achar(10)// &
         'cmp_in json:text Num "{\"u16\":65536}"'//achar(10)// &
         'cmp_in json:text Num "{\"scores\":null}"'//achar(10)// &
         'cmp_in json:text Num "{\"people\":{}}"'//achar(10)// &
         'cmp_in json:text Num "{\"big\":{}}"'//achar(10)// &
         'cmp_in json:text Num "{\"big\":\"bad\",\"big\":{}}"'//achar(10)// &
         'python3 '//tmp//'/mkhex.py'//achar(10)// &
         'for f in l32 bl tx vd vn nr; do'//achar(10)// &
         '  capnp convert binary:text --short "$SCH" Wire < '//tmp//'/$f.bin > '//tmp//'/ref 2>/dev/null'//achar(10)// &
         '  echo $? > '//tmp//'/ref.rc'//achar(10)// &
         '  "$CAP" convert binary:text --short "$SCH" Wire < '//tmp//'/$f.bin > '//tmp//'/ours 2>/dev/null'//achar(10)// &
         '  echo $? > '//tmp//'/ours.rc'//achar(10)// &
         '  cmp -s '//tmp//'/ref '//tmp//'/ours && cmp -s '//tmp//'/ref.rc '//tmp//'/ours.rc || bad=1'//achar(10)// &
         'done'//achar(10)// &
         'capnp eval "$SCH" tiny > '//tmp//'/ref 2>/dev/null; echo $? > '//tmp//'/ref.rc'//achar(10)// &
         '"$CAP" eval "$SCH" tiny > '//tmp//'/ours 2>/dev/null; echo $? > '//tmp//'/ours.rc'//achar(10)// &
         'cmp -s '//tmp//'/ref '//tmp//'/ours && cmp -s '//tmp//'/ref.rc '//tmp//'/ours.rc || bad=1'//achar(10)// &
         'capnp eval "$SCH" on.u.a > '//tmp//'/ref 2>/dev/null; echo $? > '//tmp//'/ref.rc'//achar(10)// &
         '"$CAP" eval "$SCH" on.u.a > '//tmp//'/ours 2>/dev/null; echo $? > '//tmp//'/ours.rc'//achar(10)// &
         'cmp -s '//tmp//'/ref '//tmp//'/ours && cmp -s '//tmp//'/ref.rc '//tmp//'/ours.rc || bad=1'//achar(10)// &
         'capnp eval "$SCH" off.u.a > '//tmp//'/ref 2>/dev/null; echo $? > '//tmp//'/ref.rc'//achar(10)// &
         '"$CAP" eval "$SCH" off.u.a > '//tmp//'/ours 2>/dev/null; echo $? > '//tmp//'/ours.rc'//achar(10)// &
         'cmp -s '//tmp//'/ref '//tmp//'/ours && cmp -s '//tmp//'/ref.rc '//tmp//'/ours.rc || bad=1'//achar(10)// &
         'test "$bad" = 0'//achar(10)// &
         'END')) then
         call test_failed(error, 'codec parity')
      end if
   end subroutine run_codec_parity

   !> Struct, list, enum, and data first fields are not single-value
   !> shorthand. The recovered text matches capnp 1.4.0, including a
   !> bool written into a group whose first field is a struct.
   subroutine run_shorthand_first(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=:), allocatable :: src
      integer :: err
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp)) then
         call test_failed(error, 'mkdir')
         return
      end if
      src = '@0x80112233445566a0;'//achar(10)// &
            'struct OneField { v @0 :Int32; }'//achar(10)// &
            'struct OuterOne { inner @0 :OneField; }'//achar(10)// &
            'struct HoldOuter { o @0 :OuterOne; }'//achar(10)// &
            'struct WithList { items @0 :List(Int32); tag @1 :Int32; }'//achar(10)// &
            'struct HoldWL { w @0 :WithList; }'//achar(10)// &
            'enum Color { red @0; green @1; blue @2; }'//achar(10)// &
            'struct WithEnum { c @0 :Color; x @1 :Int32; }'//achar(10)// &
            'struct WithData { d @0 :Data; x @1 :Int32; }'//achar(10)// &
            'struct WithBool { b @0 :Bool; x @1 :Int32; }'//achar(10)// &
            'struct ST { t @0 :Text; }'//achar(10)// &
            'struct Bag { st @0 :ST; one @1 :Int32; }'//achar(10)// &
            'struct Hold {'//achar(10)// &
            '  we @0 :WithEnum; wd @1 :WithData;'//achar(10)// &
            '  ge :group { c @2 :Color; x @3 :Int32; }'//achar(10)// &
            '  gd :group { d @4 :Data; x @5 :Int32; }'//achar(10)// &
            '  gl :group { l @6 :List(Int32); x @7 :Int32; }'//achar(10)// &
            '  gs :group { s @8 :WithBool; x @9 :Int32; }'//achar(10)// &
            '}'//achar(10)
      call spit(tmp//'/first.capnp', src, err)
      if (err /= CAPNP_OK) then
         call test_failed(error, 'first-field schema')
         return
      end if
      if (.not. sh(cap_setup()//' && export CAP && bash -s <<''END'''//achar(10)// &
         'set +e'//achar(10)// &
         'SCH='//tmp//'/first.capnp'//achar(10)// &
         'bad=0'//achar(10)// &
         'cmp_in() {'//achar(10)// &
         '  printf "%s" "$3" | capnp convert text:text --short "$SCH" "$2" > '//tmp//'/ref 2>/dev/null'//achar(10)// &
         '  echo $? > '//tmp//'/ref.rc'//achar(10)// &
         '  printf "%s" "$3" | "$CAP" convert text:text --short "$SCH" "$2" > '//tmp//'/ours 2>/dev/null'//achar(10)// &
         '  echo $? > '//tmp//'/ours.rc'//achar(10)// &
         '  cmp -s '//tmp//'/ref '//tmp//'/ours && cmp -s '//tmp//'/ref.rc '//tmp//'/ours.rc || bad=1'//achar(10)// &
         '}'//achar(10)// &
         'cmp_in text:text HoldOuter "(o = 5)"'//achar(10)// &
         'cmp_in text:text HoldWL "(w = [1, 2])"'//achar(10)// &
         'cmp_in text:text Hold "(we = green)"'//achar(10)// &
         'cmp_in text:text Hold "(wd = \"abc\")"'//achar(10)// &
         'cmp_in text:text Hold "(wd = 0x\"6162\")"'//achar(10)// &
         'cmp_in text:text Hold "(ge = green)"'//achar(10)// &
         'cmp_in text:text Hold "(gd = \"abc\")"'//achar(10)// &
         'cmp_in text:text Hold "(gl = [1, 2])"'//achar(10)// &
         'cmp_in text:text Hold "(gs = true)"'//achar(10)// &
         'cmp_in text:text Bag "(st = \"ab\")"'//achar(10)// &
         'test "$bad" = 0'//achar(10)// &
         'END')) then
         call test_failed(error, 'shorthand first field')
      end if
   end subroutine run_shorthand_first

   !> Empty lists with the wrong element size, a second message after a
   !> bad pointer, and text or data holding the wrong pointer kind.
   subroutine run_stream_kind(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=:), allocatable :: src
      integer :: err
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp)) then
         call test_failed(error, 'mkdir')
         return
      end if
      src = '@0x80112233445566b1;'//achar(10)// &
            'struct Child { n @0 :Int32; }'//achar(10)// &
            'struct AllPtrs { s @0 :Child; }'//achar(10)// &
            'struct TD { t @0 :Text; d @1 :Data; }'//achar(10)// &
            'struct LI { li @0 :List(Int32); }'//achar(10)// &
            'struct FL { scores @0 :List(Float64); }'//achar(10)// &
            'struct TL { tx @0 :List(Text); }'//achar(10)
      call spit(tmp//'/kind.capnp', src, err)
      call spit(tmp//'/scores.hex', '000000000f000000000000000500070007000000000000000900000000000000' &
                //'000040c000000100000000000000f83fff010000000000000000000000000000000000000000000000' &
                //'000000000000000000000000000000090000001a000000090000002200000001000000000000006f6b' &
                //'0000000000004e59430000000000', err)
      if (err /= CAPNP_OK) then
         call test_failed(error, 'stream kind schema')
         return
      end if
      if (.not. sh(cap_setup()//' && export CAP && bash -s <<''END'''//achar(10)// &
         'set +e'//achar(10)// &
         'python3 - <<''PY'''//achar(10)// &
         'import struct'//achar(10)// &
         'from pathlib import Path'//achar(10)// &
         'd = Path("'//tmp//'")'//achar(10)// &
         'def frame(words):'//achar(10)// &
         '    body = b"".join(struct.pack("<Q", w) for w in words)'//achar(10)// &
         '    return struct.pack("<II", 0, len(words)) + body'//achar(10)// &
         'root = 1 << 48'//achar(10)// &
         'd.joinpath("two.bin").write_bytes(frame([root, 3]) + frame([root, 0]))'//achar(10)// &
         'd.joinpath("text.bin").write_bytes(frame([root, 1 << 32, 0]))'//achar(10)// &
         'd.joinpath("data.bin").write_bytes(frame([1 << 49, 0, 1 << 32, 0]))'//achar(10)// &
         'd.joinpath("li.bin").write_bytes(frame([root, 1 | (1 << 35)]))'//achar(10)// &
         'd.joinpath("void0.bin").write_bytes(frame([root, 1]))'//achar(10)// &
         'd.joinpath("bit0.bin").write_bytes(frame([root, 1 | (1 << 32)]))'//achar(10)// &
         'd.joinpath("ptr0.bin").write_bytes(frame([root, 1 | (6 << 32)]))'//achar(10)// &
         'd.joinpath("byte0.bin").write_bytes(frame([root, 1 | (2 << 32)]))'//achar(10)// &
         'h = d.joinpath("scores.hex").read_text().strip()'//achar(10)// &
         'd.joinpath("scores.bin").write_bytes(bytes.fromhex(h))'//achar(10)// &
         'PY'//achar(10)// &
         'SCH='//tmp//'/kind.capnp'//achar(10)// &
         'INC=$(dirname $(dirname $(command -v capnp)))/include'//achar(10)// &
         'bad=0'//achar(10)// &
         'cmpb() {'//achar(10)// &
         '  capnp convert "binary:$2" --short "$SCH" "$3" < "$1" > '//tmp//'/ref 2>/dev/null'//achar(10)// &
         '  echo $? > '//tmp//'/ref.rc'//achar(10)// &
         '  "$CAP" convert "binary:$2" --short "$SCH" "$3" < "$1" > '//tmp//'/ours 2>/dev/null'//achar(10)// &
         '  echo $? > '//tmp//'/ours.rc'//achar(10)// &
         '  cmp -s '//tmp//'/ref '//tmp//'/ours && cmp -s '//tmp//'/ref.rc '//tmp//'/ours.rc || bad=1'//achar(10)// &
         '}'//achar(10)// &
         'cmpb '//tmp//'/two.bin json AllPtrs'//achar(10)// &
         'cmpb '//tmp//'/two.bin text AllPtrs'//achar(10)// &
         'cmpb '//tmp//'/text.bin json TD'//achar(10)// &
         'cmpb '//tmp//'/text.bin text TD'//achar(10)// &
         'cmpb '//tmp//'/data.bin json TD'//achar(10)// &
         'cmpb '//tmp//'/data.bin text TD'//achar(10)// &
         'cmpb '//tmp//'/li.bin json LI'//achar(10)// &
         'cmpb '//tmp//'/li.bin text LI'//achar(10)// &
         'cmpb '//tmp//'/void0.bin json LI'//achar(10)// &
         'cmpb '//tmp//'/void0.bin text LI'//achar(10)// &
         'cmpb '//tmp//'/bit0.bin json LI'//achar(10)// &
         'cmpb '//tmp//'/ptr0.bin json LI'//achar(10)// &
         'cmpb '//tmp//'/byte0.bin json TL'//achar(10)// &
         'cmpb '//tmp//'/void0.bin json FL'//achar(10)// &
         'capnp convert -I"$INC" binary:text --short schema/codec.capnp Sample < '//tmp//'/scores.bin > ' &
         //tmp//'/ref 2>/dev/null'//achar(10)// &
         'echo $? > '//tmp//'/ref.rc'//achar(10)// &
         '"$CAP" convert -I"$INC" binary:text --short schema/codec.capnp Sample < '//tmp//'/scores.bin > ' &
         //tmp//'/ours 2>/dev/null'//achar(10)// &
         'echo $? > '//tmp//'/ours.rc'//achar(10)// &
         'cmp -s '//tmp//'/ref '//tmp//'/ours && cmp -s '//tmp//'/ref.rc '//tmp//'/ours.rc || bad=1'//achar(10)// &
         'capnp convert -I"$INC" binary:json --short schema/codec.capnp Sample < '//tmp//'/scores.bin > ' &
         //tmp//'/ref 2>/dev/null'//achar(10)// &
         'echo $? > '//tmp//'/ref.rc'//achar(10)// &
         '"$CAP" convert -I"$INC" binary:json --short schema/codec.capnp Sample < '//tmp//'/scores.bin > ' &
         //tmp//'/ours 2>/dev/null'//achar(10)// &
         'echo $? > '//tmp//'/ours.rc'//achar(10)// &
         'cmp -s '//tmp//'/ref '//tmp//'/ours && cmp -s '//tmp//'/ref.rc '//tmp//'/ours.rc || bad=1'//achar(10)// &
         'test "$bad" = 0'//achar(10)// &
         'END')) then
         call test_failed(error, 'stream pointer kind')
      end if
   end subroutine run_stream_kind

   !> A struct whose pointer names itself. capnp 1.4.0 exits 1 after
   !> signal 11. A nest of 100 structs still converts.
   subroutine run_cycle_guard(error)
      type(error_type), allocatable, intent(out) :: error
      if (.not. sh('mkdir -p '//tmp//' && '//cap_setup() &
         //' && python3 - <<''PY'' &&'//achar(10)// &
         'import struct'//achar(10)// &
         'from pathlib import Path'//achar(10)// &
         'def sp(off, dw, pw):'//achar(10)// &
         '    return (off & 0x3FFFFFFF) << 2 | dw << 32 | pw << 48'//achar(10)// &
         'def msg(words):'//achar(10)// &
         '    body = b"".join(struct.pack("<Q", w) for w in words)'//achar(10)// &
         '    return struct.pack("<II", 0, len(words)) + body'//achar(10)// &
         'd = Path("'//tmp//'")'//achar(10)// &
         'd.joinpath("cycle.bin").write_bytes(msg([sp(0,0,1), sp(-1,0,1)]))'//achar(10)// &
         'words = [sp(0,0,1)]'//achar(10)// &
         'for i in range(100):'//achar(10)// &
         '    words.append(0 if i == 99 else sp(0,0,1))'//achar(10)// &
         'd.joinpath("deep.bin").write_bytes(msg(words))'//achar(10)// &
         'def pack(data):'//achar(10)// &
         '    out = bytearray(); i = 0; n = len(data)'//achar(10)// &
         '    while i < n:'//achar(10)// &
         '        word = data[i:i + 8]'//achar(10)// &
         '        if len(word) < 8: word = word + bytes(8 - len(word))'//achar(10)// &
         '        if word == bytes(8):'//achar(10)// &
         '            z = 0; i += 8'//achar(10)// &
         '            while i < n and data[i:i + 8] == bytes(8) and z < 255:'//achar(10)// &
         '                z += 1; i += 8'//achar(10)// &
         '            out.append(0); out.append(z); continue'//achar(10)// &
         '        tag = 0; nz = bytearray()'//achar(10)// &
         '        for b in range(8):'//achar(10)// &
         '            if word[b]: tag |= 1 << b; nz.append(word[b])'//achar(10)// &
         '        out.append(tag); out.extend(nz); i += 8'//achar(10)// &
         '    return bytes(out)'//achar(10)// &
         'd.joinpath("cycle.pk").write_bytes(pack(d.joinpath("cycle.bin").read_bytes()))'//achar(10)// &
         'd.joinpath("deep.pk").write_bytes(pack(d.joinpath("deep.bin").read_bytes()))'//achar(10)// &
         'PY'//achar(10)// &
         'rc=0; "$CAP" convert binary:binary < '//tmp//'/cycle.bin > ' &
         //tmp//'/cycle.out 2>'//tmp//'/cycle.err || rc=$?' &
         //'; test "$rc" -eq 1 && test -s '//tmp//'/cycle.err' &
         //' && "$CAP" convert binary:binary < '//tmp//'/deep.bin > ' &
         //tmp//'/deep.out && test -s '//tmp//'/deep.out' &
         //' && for spec in binary:canonical binary:flat binary:flat-packed; do' &
         //' rc=0; "$CAP" convert "$spec" < '//tmp//'/cycle.bin > /dev/null 2>' &
         //tmp//'/cycle.err || rc=$?; test "$rc" -eq 1 || exit 1;' &
         //' "$CAP" convert "$spec" < '//tmp//'/deep.bin > '//tmp//'/deep.out || exit 1;' &
         //' test -s '//tmp//'/deep.out || exit 1; done' &
         //' && rc=0; "$CAP" convert packed:canonical < '//tmp//'/cycle.pk > /dev/null 2>' &
         //tmp//'/cycle.err || rc=$?; test "$rc" -eq 1' &
         //' && "$CAP" convert packed:canonical < '//tmp//'/deep.pk > '//tmp//'/deep.out' &
         //' && test -s '//tmp//'/deep.out')) then
         call test_failed(error, 'cycle: binary copy exits 1')
      end if
   end subroutine run_cycle_guard

   !> Empty convert and decode match capnp 1.4.0, including --quiet.
   !> encode rejects --quiet. Whitespace is not an empty stream.
   subroutine run_quiet_empty(error)
      type(error_type), allocatable, intent(out) :: error
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp//' && '//cap_setup() &
         //' && export CAP INC=$(dirname $(dirname $(command -v capnp)))/include' &
         //' && python3 - <<''PY'''//achar(10)// &
         'import os, subprocess, sys'//achar(10)// &
         'inc = os.environ["INC"]'//achar(10)// &
         'cap = os.environ["CAP"]'//achar(10)// &
         'S, T = "schema/codec.capnp", "Sample"'//achar(10)// &
         'specs = ("text:text text:binary text:json binary:text binary:binary "'//achar(10)// &
         '         "binary:packed binary:flat binary:flat-packed binary:canonical "'//achar(10)// &
         '         "packed:text packed:canonical flat:text flat-packed:text "'//achar(10)// &
         '         "canonical:text json:text json:json").split()'//achar(10)// &
         'def once(tool, args, data):'//achar(10)// &
         '    p = subprocess.run([tool, *args], input=data, stdout=subprocess.PIPE, stderr=subprocess.PIPE)'//achar(10)// &
         '    return p.returncode, p.stdout'//achar(10)// &
         'for spec in specs:'//achar(10)// &
         '    for quiet in (False, True):'//achar(10)// &
         '        for data in (b"", b" ", b"\n"):'//achar(10)// &
         '            args = ["convert", "-I", inc]'//achar(10)// &
         '            if quiet: args.append("--quiet")'//achar(10)// &
         '            args += [spec, S, T]'//achar(10)// &
         '            if once("capnp", args, data) != once(cap, args, data): sys.exit(1)'//achar(10)// &
         'pairs = ['//achar(10)// &
         '    ["decode", "-I", inc, S, T],'//achar(10)// &
         '    ["decode", "-I", inc, "--quiet", S, T],'//achar(10)// &
         '    ["encode", "-I", inc, S, T],'//achar(10)// &
         '    ["encode", "-I", inc, "--quiet", S, T],'//achar(10)// &
         ']'//achar(10)// &
         'for args in pairs:'//achar(10)// &
         '    if once("capnp", args, b"") != once(cap, args, b""): sys.exit(1)'//achar(10)// &
         'if once(cap, ["encode", "-I", inc, "--quiet", S, T], b"")[0] != 1: sys.exit(1)'//achar(10)// &
         'z = bytes(8)'//achar(10)// &
         'for spec in ("binary:text", "binary:binary", "binary:flat", "binary:packed"):'//achar(10)// &
         '    args = ["convert", spec, S, T]'//achar(10)// &
         '    if once("capnp", args, z) != once(cap, args, z): sys.exit(1)'//achar(10)// &
         'PY')) then
         call test_failed(error, 'quiet empty stream')
      end if
   end subroutine run_quiet_empty

   !> Json.Value is the JSON value, including arrays, strings, and null.
   subroutine run_json_value(error)
      type(error_type), allocatable, intent(out) :: error
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp//' && '//cap_setup() &
         //' && INC=$(dirname $(dirname $(command -v capnp)))/include' &
         //' && J=$INC/capnp/compat/json.capnp' &
         //' && cat > '//tmp//'/wrap.capnp <<''EOF'''//achar(10)// &
         '@0x91b3c4d5e6f70819;'//achar(10)// &
         'using Json = import "/capnp/compat/json.capnp";'//achar(10)// &
         'struct Wrap { innerJson @0 :Json.Value; }'//achar(10)// &
         'EOF'//achar(10)// &
         'export CAP INC J="$INC/capnp/compat/json.capnp" WRAP='//tmp//'/wrap.capnp'//achar(10)// &
         'python3 - <<''PY'''//achar(10)// &
         'import os, subprocess, sys'//achar(10)// &
         'inc = os.environ["INC"]'//achar(10)// &
         'cap = os.environ["CAP"]'//achar(10)// &
         'j = os.environ["J"]'//achar(10)// &
         'wrap = os.environ["WRAP"]'//achar(10)// &
         'def run(tool, args, data):'//achar(10)// &
         '    p = subprocess.run([tool, *args], input=data, stdout=subprocess.PIPE, stderr=subprocess.PIPE)'//achar(10)// &
         '    return p.returncode, p.stdout'//achar(10)// &
         'raw = b''{"innerJson":[123,"hello",null,{"object":true}]}'''//achar(10)// &
         'args = ["convert", "-I", inc, "json:binary", wrap, "Wrap"]'//achar(10)// &
         'if run("capnp", args, raw) != run(cap, args, raw): sys.exit(1)'//achar(10)// &
         'blob = run("capnp", args, raw)[1]'//achar(10)// &
         'args = ["convert", "-I", inc, "binary:json", "--short", wrap, "Wrap"]'//achar(10)// &
         'if run("capnp", args, blob) != run(cap, args, blob): sys.exit(1)'//achar(10)// &
         'for raw in (b"null", b''"hello"'', b''[1,"x",null]''):'//achar(10)// &
         '    args = ["convert", "-I", inc, "--quiet", "json:json", "--short", j, "Value"]'//achar(10)// &
         '    if run("capnp", args, raw) != run(cap, args, raw): sys.exit(1)'//achar(10)// &
         'PY')) then
         call test_failed(error, 'json value')
      end if
   end subroutine run_json_value

   !> List(Void) is a JSON array of null, in short and pretty form.
   subroutine run_void_list_json(error)
      type(error_type), allocatable, intent(out) :: error
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp//' && '//cap_setup() &
         //' && export CAP' &
         //' && python3 - <<''PY'''//achar(10)// &
         'import os, subprocess, sys'//achar(10)// &
         'cap = os.environ["CAP"]'//achar(10)// &
         'raw = b"(vls = [" + b", ".join([b"void"] * 64) + b"])\n"'//achar(10)// &
         'def run(tool, args, data):'//achar(10)// &
         '    p = subprocess.run([tool, *args], input=data, stdout=subprocess.PIPE, stderr=subprocess.PIPE)'//achar(10)// &
         '    return p.returncode, p.stdout'//achar(10)// &
         'for extra in ([], ["--short"]):'//achar(10)// &
         '    args = ["convert", *extra, "text:json", "schema/codec.capnp", "VoidPayload"]'//achar(10)// &
         '    if run("capnp", args, raw) != run(cap, args, raw): sys.exit(1)'//achar(10)// &
         'PY')) then
         call test_failed(error, 'void list json')
      end if
   end subroutine run_void_list_json

   !> convert accepts a type whose file has an unused error, and a user
   !> -I that overlaps the standard schemas.
   subroutine run_schema_lenient(error)
      type(error_type), allocatable, intent(out) :: error
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('test -x ./build/schema-request')) then
         call skip_test(error, 'build/schema-request is missing; scripts/fypp-gen.sh builds it')
         return
      end if
      if (.not. sh('mkdir -p '//tmp//' && '//cap_setup() &
         //' && test -x ./build/schema-request' &
         //' && INC=$(dirname $(dirname $(command -v capnp)))/include' &
         //' && SRC="${CAPNP_SRC:-}"' &
         //' && if [ -z "$SRC" ]; then SRC=$(cd "$(dirname "$(command -v capnp)")/../../src/capnproto-c++-1.4.0/src" && pwd); fi' &
         //' && test -f "$SRC/capnp/test.capnp"' &
         //' && printf ''(int32Field = 1)\n'' | capnp convert -I"$SRC" text:binary' &
         //' "$SRC/capnp/test.capnp" TestAllTypes > '//tmp//'/ref.bin' &
         //' && printf ''(int32Field = 1)\n'' | "$CAP" convert -I"$SRC" text:binary' &
         //' "$SRC/capnp/test.capnp" TestAllTypes > '//tmp//'/ours.bin' &
         //' && cmp -s '//tmp//'/ref.bin '//tmp//'/ours.bin' &
         //' && cat > '//tmp//'/bad.capnp <<''EOF'''//achar(10)// &
         '@0x8123456789abcdef;'//achar(10)// &
         'const bad :Data = embed "nope.bin";'//achar(10)// &
         'struct Good { x @0 :Int32; }'//achar(10)// &
         'EOF'//achar(10)// &
         '{ capnp compile -I"$INC" -o- '//tmp//'/bad.capnp >/dev/null 2>'//tmp//'/bad.err; test $? -eq 1; }' &
         //' && printf ''(x = 1)\n'' | capnp convert text:binary '//tmp//'/bad.capnp Good > ' &
         //tmp//'/ref.bin' &
         //' && printf ''(x = 1)\n'' | "$CAP" convert text:binary '//tmp//'/bad.capnp Good > ' &
         //tmp//'/ours.bin' &
         //' && cmp -s '//tmp//'/ref.bin '//tmp//'/ours.bin' &
         //' && printf ''(title = missing)\n'' | capnp convert -I"$INC" text:text' &
         //' schema/codec.capnp Sample >/dev/null 2>'//tmp//'/ref.err; echo $? > '//tmp//'/ref.rc' &
         //' && printf ''(title = missing)\n'' | "$CAP" convert -I"$INC" text:text' &
         //' schema/codec.capnp Sample >/dev/null 2>'//tmp//'/ours.err; echo $? > '//tmp//'/ours.rc' &
         //' && cmp -s '//tmp//'/ref.rc '//tmp//'/ours.rc && test "$(cat '//tmp//'/ref.rc)" = 1')) then
         call test_failed(error, 'schema lenient')
      end if
   end subroutine run_schema_lenient

   !> A partial trailing word of flat input is ignored.
   subroutine run_flat_tail(error)
      type(error_type), allocatable, intent(out) :: error
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp//' && '//cap_setup() &
         //' && INC=$(dirname $(dirname $(command -v capnp)))/include' &
         //' && printf ''(big = 1)\n'' | capnp convert -I"$INC" text:flat' &
         //' schema/codec.capnp Sample > '//tmp//'/msg.flat' &
         //' && python3 - <<''PY'''//achar(10)// &
         'from pathlib import Path'//achar(10)// &
         'p = Path("'//tmp//'/msg.flat")'//achar(10)// &
         'p.write_bytes(p.read_bytes() + b"\x01\x02\x03")'//achar(10)// &
         'PY'//achar(10)// &
         'capnp convert -I"$INC" flat:text schema/codec.capnp Sample < '//tmp//'/msg.flat > ' &
         //tmp//'/ref; echo $? > '//tmp//'/ref.rc' &
         //' && "$CAP" convert -I"$INC" flat:text schema/codec.capnp Sample < '//tmp//'/msg.flat > ' &
         //tmp//'/ours; echo $? > '//tmp//'/ours.rc' &
         //' && cmp -s '//tmp//'/ref '//tmp//'/ours && cmp -s '//tmp//'/ref.rc '//tmp//'/ours.rc' &
         //' && test "$(cat '//tmp//'/ref.rc)" = 0')) then
         call test_failed(error, 'flat trailing word')
      end if
   end subroutine run_flat_tail

   !> A null root is a zero-size struct pointer on binary, packed, and flat.
   subroutine run_null_root_wire(error)
      type(error_type), allocatable, intent(out) :: error
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp//' && '//cap_setup() &
         //' && export CAP INC=$(dirname $(dirname $(command -v capnp)))/include' &
         //' && python3 - <<''PY'''//achar(10)// &
         'import os, subprocess, sys'//achar(10)// &
         'from pathlib import Path'//achar(10)// &
         'd = Path("'//tmp//'")'//achar(10)// &
         'd.joinpath("null.bin").write_bytes(bytes(4) + bytes([1, 0, 0, 0]) + bytes(8))'//achar(10)// &
         'd.joinpath("null.flat").write_bytes(bytes(8))'//achar(10)// &
         'inc = os.environ["INC"]; cap = os.environ["CAP"]'//achar(10)// &
         'def run(tool, spec, data):'//achar(10)// &
         '    p = subprocess.run([tool, "convert", "-I", inc, spec, "schema/codec.capnp", "Sample"],'//achar(10)// &
         '                       input=data, stdout=subprocess.PIPE, stderr=subprocess.PIPE)'//achar(10)// &
         '    return p.returncode, p.stdout'//achar(10)// &
         'framed = (d / "null.bin").read_bytes(); flat = (d / "null.flat").read_bytes()'//achar(10)// &
         'for spec, data in (('//achar(10)// &
         '    "binary:binary", framed), ("binary:packed", framed), ("binary:flat", framed),'//achar(10)// &
         '    ("binary:text", framed), ("flat:binary", flat), ("flat:text", flat)):'//achar(10)// &
         '    if run("capnp", spec, data) != run(cap, spec, data): sys.exit(1)'//achar(10)// &
         'PY')) then
         call test_failed(error, 'null root wire')
      end if
   end subroutine run_null_root_wire

   !> A Json.Value object whose field holds an array must round-trip.
   !> A shallow copy of the child frees that array twice.
   subroutine run_json_value_object(error)
      type(error_type), allocatable, intent(out) :: error
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp//' && '//cap_setup() &
         //' && export CAP INC=$(dirname $(dirname $(command -v capnp)))/include' &
         //' && python3 - <<''PY'''//achar(10)// &
         'import os, subprocess, sys'//achar(10)// &
         'inc = os.environ["INC"]; cap = os.environ["CAP"]'//achar(10)// &
         'j = inc + "/capnp/compat/json.capnp"'//achar(10)// &
         'def run(tool, args, data):'//achar(10)// &
         '    p = subprocess.run([tool, *args], input=data, stdout=subprocess.PIPE, stderr=subprocess.PIPE)'//achar(10)// &
         '    return p.returncode, p.stdout'//achar(10)// &
         'cases = [b''{"a":[1,"x",null,true]}\n'', b''{"a":null,"b":[1]}\n'','//achar(10)// &
         '         b''(object = [(name = "a", value = (array = [(number = 1)]))])\n'']'//achar(10)// &
         'specs = [("json:json", cases[0]), ("json:json", cases[1]), ("text:json", cases[2])]'//achar(10)// &
         'for spec, raw in specs:'//achar(10)// &
         '    args = ["convert", "--quiet", "-I", inc, spec, j, "Value"]'//achar(10)// &
         '    if run("capnp", args, raw) != run(cap, args, raw): sys.exit(1)'//achar(10)// &
         'PY')) then
         call test_failed(error, 'json value object')
      end if
   end subroutine run_json_value_object

   !> binary:json of a 70-deep Json.Value follows the message depth limit.
   subroutine run_json_value_depth(error)
      type(error_type), allocatable, intent(out) :: error
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp//' && '//cap_setup() &
         //' && export CAP INC=$(dirname $(dirname $(command -v capnp)))/include' &
         //' && python3 - <<''PY'''//achar(10)// &
         'import os, subprocess, sys'//achar(10)// &
         'inc = os.environ["INC"]; cap = os.environ["CAP"]'//achar(10)// &
         'j = inc + "/capnp/compat/json.capnp"'//achar(10)// &
         'body = "(number = 1)"'//achar(10)// &
         'for _ in range(70):'//achar(10)// &
         '    body = "(array = [" + body + "])"'//achar(10)// &
         'raw = (body + "\n").encode()'//achar(10)// &
         'def run(tool, args, data):'//achar(10)// &
         '    p = subprocess.run([tool, *args], input=data, stdout=subprocess.PIPE, stderr=subprocess.PIPE)'//achar(10)// &
         '    return p.returncode, p.stdout'//achar(10)// &
         'args = ["convert", "--quiet", "-I", inc, "text:binary", j, "Value"]'//achar(10)// &
         'ref = run("capnp", args, raw)'//achar(10)// &
         'if ref[0] != 0: sys.exit(2)'//achar(10)// &
         'if run(cap, args, raw) != ref: sys.exit(1)'//achar(10)// &
         'args = ["convert", "--quiet", "-I", inc, "binary:json", "--short", j, "Value"]'//achar(10)// &
         'if run("capnp", args, ref[1]) != run(cap, args, ref[1]): sys.exit(1)'//achar(10)// &
         'PY')) then
         call test_failed(error, 'json value depth')
      end if
   end subroutine run_json_value_depth

   !> Malformed text that cannot be text prints nothing, matching capnp 1.4.0.
   !> --quiet parses it. A plausible fragment still prints the recovered value.
   subroutine run_text_plausible(error)
      type(error_type), allocatable, intent(out) :: error
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp//' && '//cap_setup() &
         //' && export CAP' &
         //' && python3 - <<''PY'''//achar(10)// &
         'import os, subprocess, sys'//achar(10)// &
         'cap = os.environ["CAP"]'//achar(10)// &
         'sch, typ = "schema/box.capnp", "Box"'//achar(10)// &
         'def run(tool, args, data):'//achar(10)// &
         '    p = subprocess.run([tool, *args], input=data, stdout=subprocess.PIPE, stderr=subprocess.PIPE)'//achar(10)// &
         '    return p.returncode, p.stdout'//achar(10)// &
         'cases = [b"hello\n", bytes([1, 2, 255, 40]), b"(\x0c)\n", b"(!!!)\n", b"(\n"]'//achar(10)// &
         'for raw in cases:'//achar(10)// &
         '    for quiet in (False, True):'//achar(10)// &
         '        args = ["convert"]'//achar(10)// &
         '        if quiet: args.append("--quiet")'//achar(10)// &
         '        args += ["text:text", sch, typ]'//achar(10)// &
         '        if run("capnp", args, raw) != run(cap, args, raw): sys.exit(1)'//achar(10)// &
         'PY')) then
         call test_failed(error, 'text plausible')
      end if
   end subroutine run_text_plausible

   !> canonical input that is not canonical exits 1 with an empty stdout.
   subroutine run_canonical_reject(error)
      type(error_type), allocatable, intent(out) :: error
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp//' && '//cap_setup() &
         //' && export CAP' &
         //' && python3 - <<''PY'''//achar(10)// &
         'import os, subprocess, sys'//achar(10)// &
         'cap = os.environ["CAP"]'//achar(10)// &
         'def run(tool, data):'//achar(10)// &
         '    p = subprocess.run([tool, "convert", "--quiet", "canonical:text",'//achar(10)// &
         '                        "schema/common.capnp", "Vec3"],'//achar(10)// &
         '                       input=data, stdout=subprocess.PIPE, stderr=subprocess.PIPE)'//achar(10)// &
         '    return p.returncode, p.stdout'//achar(10)// &
         'raw = bytes(16)'//achar(10)// &
         'if run("capnp", raw) != run(cap, raw): sys.exit(1)'//achar(10)// &
         'if run("capnp", raw)[0] != 1 or run("capnp", raw)[1] != b"": sys.exit(1)'//achar(10)// &
         'PY')) then
         call test_failed(error, 'canonical reject')
      end if
   end subroutine run_canonical_reject

   !> From another directory, a type beside an unused broken one loads
   !> through the schema-request found next to the executable.
   subroutine run_request_beside_exe(error)
      type(error_type), allocatable, intent(out) :: error
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('test -x ./build/schema-request')) then
         call skip_test(error, 'build/schema-request is missing; scripts/fypp-gen.sh builds it')
         return
      end if
      if (.not. sh('mkdir -p '//tmp//'/beside && '//cap_setup() &
         //' && test -x ./build/schema-request && unset CAPNP_SCHEMA_REQUEST' &
         //' && cd '//tmp//'/beside' &
         //' && printf ''@0x8123456789abcde1;\nstruct P { x @0 :Int32; }\n' &
         //'struct UnusedBroken { bad @0 :NonExistentType; }\nconst p :P = (x = 7);\n''' &
         //' > partial.capnp' &
         //' && printf ''(x = 7)\n'' | "$CAP" convert text:text partial.capnp P > f.out' &
         //' && printf ''(x = 7)\n'' | capnp convert text:text partial.capnp P > r.out' &
         //' && "$CAP" eval partial.capnp p >> f.out && capnp eval partial.capnp p >> r.out' &
         //' && cmp -s f.out r.out')) then
         call test_failed(error, 'schema-request beside the executable')
      end if
   end subroutine run_request_beside_exe

   !> A generic root expression matches capnp 1.4.0, with and without the helper.
   subroutine run_generic_root(error)
      type(error_type), allocatable, intent(out) :: error
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp//'/generic-root && '//cap_setup() &
         //' && export CAP TMPD='//tmp//'/generic-root' &
         //' && python3 - <<''PY'''//achar(10)// &
         'import os, shutil, subprocess, sys'//achar(10)// &
         'cap = os.environ["CAP"]'//achar(10)// &
         'tmp = os.environ["TMPD"]'//achar(10)// &
         'def run(tool, args, data, cwd=None, env=None):'//achar(10)// &
         '    p = subprocess.run([tool, *args], input=data, stdout=subprocess.PIPE,'//achar(10)// &
         '                       stderr=subprocess.PIPE, cwd=cwd, env=env)'//achar(10)// &
         '    return p.returncode, p.stdout'//achar(10)// &
         'sch = os.path.join(tmp, "g.capnp")'//achar(10)// &
         'with open(sch, "w") as f:'//achar(10)// &
         '    f.write("@0xa1b2c3d4e5f60718;\n"'//achar(10)// &
         '            "struct G(A, B) { a @0 :A; b @1 :B; rev @2 :G(B, A);\n"'//achar(10)// &
         '            "  struct Inner(C) { x @0 :A; y @1 :C; } }\n"'//achar(10)// &
         '            "struct P { x @0 :Int32; }\n"'//achar(10)// &
         '            "enum E { one @0; two @1; }\n"'//achar(10)// &
         '            "struct R { g @0 :G(Text, Data); }\n")'//achar(10)// &
         'box = os.path.abspath("schema/box.capnp")'//achar(10)// &
         'cases = ['//achar(10)// &
         '    (["convert", "text:text", sch, "R"], b''(g = (a = "x", rev = (a = 0x"0102", b = "t")))\n''),'//achar(10)// &
         '    (["convert", "text:json", sch, "R"], b''(g = (rev = (rev = (a = "deep"))))\n''),'//achar(10)// &
         '    (["convert", "text:text", sch, "G(P, Text)"], b''(a = (x = 5), rev = (b = (x = 1)))\n''),'//achar(10)// &
         '    (["convert", "text:json", sch, "G(E, Text)"], b"(a = two)\n"),'//achar(10)// &
         '    (["convert", "text:text", sch, "G(List(Int32), Text)"], b"(a = [1, 2])\n"),'//achar(10)// &
         '    (["convert", "json:text", sch, "G(Text, Data)"], b''{"a": "x", "rev": {"b": "y"}}\n''),'//achar(10)// &
         '    (["convert", "text:text", sch, ".G(.P, Text)"], b"(a = (x = 7))\n"),'//achar(10)// &
         '    (["convert", "text:text", sch, "G(Text)"], b"(a = \"x\")\n"),'//achar(10)// &
         '    (["convert", "text:text", sch, "Nope(Text)"], b"(a = \"x\")\n"),'//achar(10)// &
         '    (["encode", sch, "G(Text, Data)"], b''(a = "x")\n''),'//achar(10)// &
         '    (["convert", "text:text", box, "Box(Text)"], b''(value = "hi")\n''),'//achar(10)// &
         '    (["convert", "json:text", box, "Box(Text)"], b''{"value":"hi"}\n''),'//achar(10)// &
         ']'//achar(10)// &
         'cases += ['//achar(10)// &
         '    (["convert", "text:text", sch, "G(Text, Data).Inner(List(Int32))"], b''(x = "hi", y = [5])\n''),'//achar(10)// &
         '    (["convert", "text:json", sch, "G(P, Text).Inner(List(Text))"], b''(x = (x = 3), y = ["a"])\n''),'//achar(10)// &
         ']'//achar(10)// &
         '# The fallback wrapper runs when no schema-request helper can be found.'//achar(10)// &
         'bare = os.path.join(tmp, "bare")'//achar(10)// &
         'os.makedirs(os.path.join(bare, "bin"), exist_ok=True)'//achar(10)// &
         'shutil.copy(cap, os.path.join(bare, "bin", "capnpf"))'//achar(10)// &
         'env = {k: v for k, v in os.environ.items() if k != "CAPNP_SCHEMA_REQUEST"}'//achar(10)// &
         'for args, raw in cases:'//achar(10)// &
         '    ref = run("capnp", args, raw)'//achar(10)// &
         '    if ref != run(cap, args, raw):'//achar(10)// &
         '        print("mismatch:", args)'//achar(10)// &
         '        sys.exit(1)'//achar(10)// &
         '    if ref != run(os.path.join(bare, "bin", "capnpf"), args, raw, cwd=bare, env=env):'//achar(10)// &
         '        print("fallback mismatch:", args)'//achar(10)// &
         '        sys.exit(1)'//achar(10)// &
         'PY')) then
         call test_failed(error, 'generic root')
      end if
   end subroutine run_generic_root

   !> A composite list of 2**28-1 zero-word elements converts in linear time.
   subroutine run_zero_size_list(error)
      type(error_type), allocatable, intent(out) :: error
      if (.not. sh('mkdir -p '//tmp//' && '//cap_setup() &
         //' && printf ''\0\0\0\0\3\0\0\0\0\0\0\0\0\0\1\0\1\0\0\0\7\0\0\0\374\377\377\77\0\0\0\0''' &
         //' > '//tmp//'/zl.bin' &
         //' && for f in canonical binary flat packed; do' &
         //' timeout 10 "$CAP" convert binary:$f < '//tmp//'/zl.bin > /dev/null || exit 1; done')) then
         call test_failed(error, 'zero-size list conversion')
      end if
   end subroutine run_zero_size_list

   !> Options, values and positionals parse the way kj::MainBuilder does.
   subroutine run_cli_options(error)
      type(error_type), allocatable, intent(out) :: error
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp//'/cli-options && '//cap_setup() &
         //' && export CAP ' &
         //' && python3 - <<''PY'''//achar(10)// &
         'import os, subprocess, sys'//achar(10)// &
         'cap = os.environ["CAP"]'//achar(10)// &
         'def run(tool, args, data):'//achar(10)// &
         '    try:'//achar(10)// &
         '        p = subprocess.run([tool, *args], input=data, stdout=subprocess.PIPE,'//achar(10)// &
         '                           stderr=subprocess.PIPE, timeout=10)'//achar(10)// &
         '    except subprocess.TimeoutExpired:'//achar(10)// &
         '        return None'//achar(10)// &
         '    return p.returncode, p.stdout'//achar(10)// &
         'sch, typ = "schema/common.capnp", "Vec3"'//achar(10)// &
         'inc = os.path.abspath("schema")'//achar(10)// &
         'txt = b"(x = 1, y = 2, z = 3)\n"'//achar(10)// &
         'cases = ['//achar(10)// &
         '    ["encode", "--segment-size=0", sch, typ],'//achar(10)// &
         '    ["encode", "--segment-size=00", sch, typ],'//achar(10)// &
         '    ["encode", "--segment-size=4294967296", sch, typ],'//achar(10)// &
         '    ["encode", "--segment-size=4294967297", sch, typ],'//achar(10)// &
         '    ["encode", "--segment-size", "1", sch, typ],'//achar(10)// &
         '    ["encode", "--segment-size= 1", sch, typ],'//achar(10)// &
         '    ["encode", "--segment-size=1 ", sch, typ],'//achar(10)// &
         '    ["encode", "--segment-size=0x10", sch, typ],'//achar(10)// &
         '    ["encode", "--segment-size=-1", sch, typ],'//achar(10)// &
         '    ["encode", "--segment-size=536870911", sch, typ],'//achar(10)// &
         '    ["encode", "--segment-size=536870912", sch, typ],'//achar(10)// &
         '    ["convert", "--segment-size=536870912", "text:text", sch, typ],'//achar(10)// &
         '    ["convert", "--segment-size=536870912", "text:packed", sch, typ],'//achar(10)// &
         '    ["convert", "--segment-size", "1", "text:binary", sch, typ],'//achar(10)// &
         '    ["convert", "--import-path", inc, "text:binary", sch, typ],'//achar(10)// &
         '    ["--import-path", inc, "convert", "text:binary", sch, typ],'//achar(10)// &
         '    ["-I", inc, "convert", "text:binary", sch, typ],'//achar(10)// &
         '    ["convert", "-I", "/no/such/dir", "text:binary", sch, typ],'//achar(10)// &
         '    ["encode", "--flat", "--segment-size=1", sch, typ],'//achar(10)// &
         '    ["encode", "--segment-size=1", "--flat", sch, typ],'//achar(10)// &
         '    ["encode", "--packed", "--flat", sch, typ],'//achar(10)// &
         '    ["encode", "-p", sch, typ],'//achar(10)// &
         '    ["convert", "text:binary", sch, typ, "extra"],'//achar(10)// &
         '    ["convert", "text:binary", "--", sch, typ],'//achar(10)// &
         '    ["encode", sch],'//achar(10)// &
         '    ["encode", "--short", sch, typ],'//achar(10)// &
         '    ["decode", "--segment-size=1", sch, typ],'//achar(10)// &
         '    ["eval", "-o", "nope", sch, "Vec3"],'//achar(10)// &
         '    ["convert", "--bogus", "text:binary", sch, typ],'//achar(10)// &
         ']'//achar(10)// &
         'for args in cases:'//achar(10)// &
         '    if run("capnp", args, txt) != run(cap, args, txt):'//achar(10)// &
         '        print("mismatch:", args)'//achar(10)// &
         '        sys.exit(1)'//achar(10)// &
         'PY')) then
         call test_failed(error, 'cli options')
      end if
   end subroutine run_cli_options

   !> A capability pointer fails the way capnp 1.4.0 fails without a cap table.
   subroutine run_cap_pointers(error)
      type(error_type), allocatable, intent(out) :: error
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp//'/cap-pointers && '//cap_setup() &
         //' && export CAP TMPD='//tmp//'/cap-pointers' &
         //' && python3 - <<''PY'''//achar(10)// &
         'import os, struct, subprocess, sys'//achar(10)// &
         'cap = os.environ["CAP"]'//achar(10)// &
         'tmp = os.environ["TMPD"]'//achar(10)// &
         'def run(tool, args, data):'//achar(10)// &
         '    try:'//achar(10)// &
         '        p = subprocess.run([tool, *args], input=data, stdout=subprocess.PIPE,'//achar(10)// &
         '                           stderr=subprocess.PIPE, timeout=10)'//achar(10)// &
         '    except subprocess.TimeoutExpired:'//achar(10)// &
         '        return None'//achar(10)// &
         '    return p.returncode, p.stdout'//achar(10)// &
         'sch = os.path.join(tmp, "caps.capnp")'//achar(10)// &
         'with open(sch, "w") as f:'//achar(10)// &
         '    f.write("@0x9b1c2d3e4f506172;\ninterface I {}\n"'//achar(10)// &
         '            "struct H { i @0 :I; a @1 :AnyPointer; l @2 :List(I); n @3 :Int32; }\n")'//achar(10)// &
         'def msg(words):'//achar(10)// &
         '    return struct.pack("<II", 0, len(words)) + b"".join(words)'//achar(10)// &
         'root = bytes.fromhex("0000000001000300")'//achar(10)// &
         'data = struct.pack("<Q", 7)'//achar(10)// &
         'capp = bytes.fromhex("0300000000000000")'//achar(10)// &
         'z = bytes(8)'//achar(10)// &
         'lst = bytes.fromhex("010000000e000000")'//achar(10)// &
         'm_field = msg([root, data, capp, z, z])'//achar(10)// &
         'm_any = msg([root, data, z, capp, z])'//achar(10)// &
         'm_list = msg([root, data, z, z, lst, z])'//achar(10)// &
         'm_plain = msg([root, data, z, z, z])'//achar(10)// &
         'inputs = [("binary", m) for m in'//achar(10)// &
         '          (m_field, m_any, m_list, m_field + m_plain, m_list + m_plain, m_any + m_plain)]'//achar(10)// &
         'inputs += [("flat", m_field[8:]), ("flat", m_any[8:])]'//achar(10)// &
         'for fmt, raw in inputs:'//achar(10)// &
         '    for to in ("binary", "packed", "flat", "flat-packed", "canonical", "text", "json"):'//achar(10)// &
         '        args = ["convert", fmt + ":" + to, sch, "H"]'//achar(10)// &
         '        ref = run("capnp", args, raw)'//achar(10)// &
         '        if ref is not None and ref != run(cap, args, raw):'//achar(10)// &
         '            print("mismatch:", fmt, to, raw.hex())'//achar(10)// &
         '            sys.exit(1)'//achar(10)// &
         'PY')) then
         call test_failed(error, 'capability pointers')
      end if
   end subroutine run_cap_pointers

end module test_regress
