!> Regressions for the text and JSON codec review.
module test_regress
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed, skip_test
   use, intrinsic :: iso_fortran_env, only: int64, real64
   use capnp
   use capnp_dynamic
   use capnp_text
   use capnp_json
   use capnp_codec, only: capnp_brand_env_t
   use cli_path, only: capnpf_setup, capnpc_setup
   implicit none
   private
   public :: collect_regress

   character(len=*), parameter :: tmp = '/tmp/capnpf-regress'

contains

   subroutine collect_regress(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
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
         new_unittest("text-leniency", run_text_leniency), &
         new_unittest("unloaded-guard", run_unloaded), &
         new_unittest("long-argument", run_long_arg), &
         new_unittest("schema-pipe", run_schema_pipe), &
         new_unittest("plugin-release", run_plugin_release), &
         new_unittest("cli-locate", run_cli_locate) &
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
      call capnp_text_decode(schema, idx, '(count = 1)#', msg, err)
      call check_(error, err == CAPNP_OK, 'comment: hash ends the input')
      root = capnp_root(msg, err)
      count = capnp_dyn_get_int(schema, idx, root, 'count', err)
      call check_(error, err == CAPNP_OK .and. count == 1_int64, 'comment: count is 1')
      call capnp_text_decode(schema, idx, '(count = 1 # hide'//achar(10)//', ratio = 9.0)', msg, err)
      call check_(error, err == CAPNP_OK, 'comment: field after a hash')
      root = capnp_root(msg, err)
      count = capnp_dyn_get_int(schema, idx, root, 'count', err)
      call check_(error, err == CAPNP_OK .and. count == 1_int64, 'comment: count survives the hash')
      block
         real(real64) :: ratio
         ratio = capnp_dyn_get_f64(schema, idx, root, 'ratio', err)
         call check_(error, err == CAPNP_OK .and. abs(ratio - 9.0_real64) < 1.0e-12_real64, &
                     'comment: ratio follows the hash')
      end block
      call capnp_text_decode(schema, idx, '(scores = [3, # c'//achar(10)//'1, 4])', msg, err)
      call check_(error, err == CAPNP_OK, 'comment: list element after a hash')
      root = capnp_root(msg, err)
      block
         character(len=:), allocatable :: text
         call capnp_text_encode(schema, idx, root, text, err, pretty=.false.)
         call check_(error, err == CAPNP_OK .and. index(text, 'scores = [3, 1, 4]') > 0, &
                     'comment: list keeps 3, 1, 4')
      end block
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
      packed(0) = -16_int8
      packed(1:4) = -1_int8
      open (newunit=u, file=tmp//'/bomb.packed', access='stream', form='unformatted', &
            status='replace')
      write (u) packed
      close (u)
      open (newunit=u, file=tmp//'/bomb.packed', access='stream', form='unformatted', &
            status='old', action='read')
      call capnp_read_message_packed_unit(u, msg, err)
      close (u)
      call check_(error, err == CAPNP_ERR_TRAVERSAL .or. err == CAPNP_ERR_FRAMING .or. &
                  err == CAPNP_ERR_PACKED, 'bomb: packed stream rejects')
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
      call check_(error, err == CAPNP_OK .and. size(got) == size(ref) .and. all(got == ref), &
                  'stdout: two messages match capnp')
      call capnp_read_file(tmp//'/app.bin', got, err)
      call capnp_read_file(tmp//'/app.ref', ref, err)
      call check_(error, err == CAPNP_OK .and. size(got) == size(ref) .and. all(got == ref), &
                  'stdout: append keeps both messages')
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
      n = 200000_int64
      lst = capnp_dyn_init_list(schema, idx, root, 'xs', n, err)
      call check_(error, err == CAPNP_OK .and. capnp_list_len(lst) == n, 'list: allocated')
      call capnp_text_encode(schema, idx, root, text, err, pretty=.false., &
                             traversal_words=1_int64)
      call check_(error, err == CAPNP_ERR_TRAVERSAL, 'list: tiny traversal budget')
      call system_clock(c1, rate)
      call capnp_text_encode(schema, idx, root, text, err, pretty=.false.)
      call system_clock(c2)
      elapsed = real(c2 - c1)/real(rate)
      call check_(error, err == CAPNP_OK, 'list: encode')
      call check_(error, elapsed < 3.0, 'list: encode under 3s')
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
      integer :: err, n0, n1
      n0 = capnp_arena_live_blocks()
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
      n1 = capnp_arena_live_blocks()
      call capnp_canonicalize(msg, bytes, err)
      call check_(error, err == CAPNP_ERR_KIND, 'canon: capability has no canonical form')
      call check_(error, capnp_arena_live_blocks() == n1, &
                  'canon: failed copy releases the builder')
      call capnp_message_free(msg)
      call check_(error, capnp_arena_live_blocks() == n0, 'canon: source message is released')
      call capnp_message_init_builder(msg, err)
      root = capnp_new_struct(msg, 1, 0, err)
      call capnp_set_root(msg, root, err)
      call capnp_set_i32(root, 0_int64, 7_int32, err)
      call capnp_canonicalize(msg, bytes, err)
      call check_(error, err == CAPNP_OK .and. allocated(bytes) .and. size(bytes) > 0, &
                  'canon: later call still works')
      call capnp_message_free(msg)
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
      call check_(error, err == CAPNP_OK .and. size(ours) == size(cref) .and. all(ours == cref), &
                  'json: missing discriminator matches capnp')
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
      call check_(error, size(got) == 4 .and. all(got == [240, 159, 152, 128]), &
                  'surrogate: grinning face is one scalar')
      call capnp_message_free(decoded)
      call capnp_json_decode(schema, idx, '{"display_name":"\uD83D"}', decoded, err)
      call check_(error, err == CAPNP_OK, 'surrogate: lone high decodes')
      root = capnp_root(decoded, err)
      call capnp_dyn_get_text(schema, idx, root, 'title', text, err)
      got = codes(text)
      call check_(error, err == CAPNP_OK .and. size(got) == 3 .and. all(got == [237, 160, 189]), &
                  'surrogate: lone high is WTF-8')
      call capnp_message_free(decoded)
      call capnp_json_decode(schema, idx, '{"display_name":"\uDE00"}', decoded, err)
      call check_(error, err == CAPNP_OK, 'surrogate: lone low decodes')
      root = capnp_root(decoded, err)
      call capnp_dyn_get_text(schema, idx, root, 'title', text, err)
      got = codes(text)
      call check_(error, err == CAPNP_OK .and. size(got) == 3 .and. all(got == [237, 184, 128]), &
                  'surrogate: lone low is WTF-8')
      call capnp_message_free(decoded)
      call capnp_json_decode(schema, idx, '{"display_name":"\uD800\uD800\uDC00"}', decoded, err)
      call check_(error, err == CAPNP_OK, 'surrogate: high high low decodes')
      root = capnp_root(decoded, err)
      call capnp_dyn_get_text(schema, idx, root, 'title', text, err)
      got = codes(text)
      call check_(error, err == CAPNP_OK .and. size(got) == 7 .and. &
                  all(got == [237, 160, 128, 240, 144, 128, 128]), &
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
      call check_(error, err /= CAPNP_OK, 'null-disc: unknown tag does not select count')
      call capnp_json_decode(schema, tidx, '{"kind":1}', decoded, err)
      call check_(error, err /= CAPNP_OK, 'null-disc: non-string tag is rejected')
      call capnp_message_free(decoded)
      call capnp_dyn_free(schema)
   end subroutine run_json_null_disc

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
      character(len=*), parameter :: ptmp = '/tmp/capnpf-regress/plugin'
      if (.not. sh('mkdir -p '//ptmp &
         //' && '//capnpc_setup() &
         //' && cp test/fixtures/addressbook.cgr.bin '//ptmp//'/req.bin' &
         //' && (cd '//ptmp//' && "$CAP" req.bin)' &
         //' && test -s '//ptmp//'/addressbook_capnp.f90' &
         //' && export ASAN_OPTIONS="${ASAN_OPTIONS:+$ASAN_OPTIONS:}exitcode=23"' &
         //' && rc=0; "$CAP" '//ptmp//'/missing.bin >/dev/null 2>'//ptmp//'/miss.err || rc=$?' &
         //'; test "$rc" -eq 1 && ! grep -q LeakSanitizer '//ptmp//'/miss.err' &
         //' && rc=0; printf "" | "$CAP" >/dev/null 2>'//ptmp//'/empty.err || rc=$?' &
         //'; test "$rc" -eq 1 && ! grep -q LeakSanitizer '//ptmp//'/empty.err')) then
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
         //' && printf "(x = 1, y = 2, z = 3)\n" > '//tmp//'/pipe.in' &
         //' && '//cap_setup() &
         //' && export CAP' &
         //' && printf "%s\n"' &
         //' "echo \$\$ > \"\$1\""' &
         //' "ln -sfn \"\$2\" /tmp/capnpf-\$\$.cgr"' &
         //' "exec \"\$CAP\" encode schema/common.capnp Vec3 < \"\$3\" > \"\$4\""' &
         //' > '//tmp//'/pipe.sh' &
         //' && sh '//tmp//'/pipe.sh '//tmp//'/pipe.pid '//tmp//'/kept ' &
         //tmp//'/pipe.in '//tmp//'/pipe.bin' &
         //' && pid=$(cat '//tmp//'/pipe.pid)' &
         //' && test -L /tmp/capnpf-$pid.cgr' &
         //' && test "$(cat '//tmp//'/kept)" = kept' &
         //' && rm -f /tmp/capnpf-$pid.cgr' &
         //' && printf "(x = 1, y = 2, z = 3)\n" | capnp encode' &
         //' schema/common.capnp Vec3 > '//tmp//'/pipe.ref')) then
         call test_failed(error, 'schema pipe encode failed')
         return
      end if
      call capnp_read_file(tmp//'/pipe.bin', got, err)
      call capnp_read_file(tmp//'/pipe.ref', ref, err)
      call check_(error, err == CAPNP_OK .and. size(got) == size(ref) .and. all(got == ref), &
                  'schema: pipe matches capnp and leaves the symlink target')
   end subroutine run_schema_pipe

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
      call check_(error, err == CAPNP_OK .and. allocated(ours) .and. allocated(ref) .and. &
                  size(ours) == size(ref) .and. all(ours == ref), label)
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

end module test_regress
