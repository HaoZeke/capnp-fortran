!> Text, JSON, and CLI codecs differential-tested against the capnp 1.4 tool.
module test_codecs
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed, skip_test
   use capnp
   use capnp_codec, only: capnp_u_of_i8
   use capnp_dynamic
   use capnp_text
   use capnp_json
   implicit none
   private
   public :: collect_codecs

   character(len=*), parameter :: tmp = '/tmp/capnpf-codec'

contains

   subroutine collect_codecs(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [ &
         new_unittest("text-addressbook", run_addressbook), &
         new_unittest("text-kitchen", run_kitchen), &
         new_unittest("text-edge-cases", run_text_edges), &
         new_unittest("text-generics", run_text_generics), &
         new_unittest("json-addressbook", run_json_addressbook), &
         new_unittest("json-kitchen", run_json_kitchen), &
         new_unittest("json-sample", run_json_sample), &
         new_unittest("json-edge-cases", run_json_edges), &
         new_unittest("json-flatten-discriminator", run_json_flatten_disc), &
         new_unittest("cli", run_cli), &
         new_unittest("cli-extended", run_cli_extended) &
         ]
   end subroutine collect_codecs

   subroutine check_(error, cond, name)
      type(error_type), allocatable, intent(inout) :: error
      logical, intent(in) :: cond
      character(len=*), intent(in) :: name
      if (allocated(error)) return
      call check(error, cond, name)
   end subroutine check_

   subroutine run_addressbook(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg, decoded
      type(capnp_ptr_t) :: root
      integer(int8), allocatable :: bytes(:), ours(:), ref(:)
      character(len=:), allocatable :: text, file
      integer :: err, idx
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp// &
         ' && capnp decode schema/addressbook.capnp AddressBook < test/fixtures/addressbook.bin > '//tmp//'/ab.txt' &
         //' && capnp decode --short schema/addressbook.capnp AddressBook' &
         //' < test/fixtures/addressbook.bin > '//tmp//'/ab.short.txt')) then
         call test_failed(error, 'capnp decode addressbook')
         return
      end if
      call capnp_read_file('test/fixtures/addressbook.cgr.bin', bytes, err)
      if (err /= CAPNP_OK) then
         call skip_test(error, 'addressbook cgr missing')
         return
      end if
      call capnp_dyn_load(schema, bytes, err)
      call check_(error, err == CAPNP_OK, 'text: addressbook schema')
      idx = capnp_dyn_find(schema, 'AddressBook', err)
      call check_(error, idx > 0, 'text: AddressBook node')
      call capnp_read_file('test/fixtures/addressbook.bin', bytes, err)
      call capnp_deserialize_bytes(bytes, msg, err)
      root = capnp_root(msg, err)
      call capnp_text_encode(schema, idx, root, text, err, pretty=.true.)
      call check_(error, err == CAPNP_OK, 'text: pretty encode')
      call check_file(error, text, tmp//'/ab.txt', 'text: addressbook pretty')
      call capnp_text_encode(schema, idx, root, text, err, pretty=.false.)
      call check_(error, err == CAPNP_OK, 'text: short encode')
      call check_file(error, text, tmp//'/ab.short.txt', 'text: addressbook short')
      call slurp(tmp//'/ab.txt', file, err)
      call capnp_text_decode(schema, idx, file, decoded, err)
      call check_(error, err == CAPNP_OK, 'text: addressbook decode')
      call capnp_canonicalize(decoded, ours, err)
      call capnp_read_file('test/fixtures/addressbook.canonical.bin', ref, err)
      call check_bytes(error, ours, ref, 'text: addressbook canonical')
      call capnp_message_free(msg)
      call capnp_message_free(decoded)
      call capnp_dyn_free(schema)
   end subroutine run_addressbook

   subroutine run_kitchen(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg, decoded
      type(capnp_ptr_t) :: root
      integer(int8), allocatable :: bytes(:), ours(:), ref(:)
      character(len=:), allocatable :: text, src
      integer :: err, idx
      src = kitchen_text()
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp)) then
         call test_failed(error, 'mkdir')
         return
      end if
      call spit(tmp//'/k.txt', src, err)
      if (.not. sh('capnp encode schema/kitchen.capnp Sink < '//tmp//'/k.txt > '//tmp//'/k.bin' &
         //' && capnp decode schema/kitchen.capnp Sink < '//tmp//'/k.bin > '//tmp//'/k.txt.out' &
         //' && capnp decode --short schema/kitchen.capnp Sink < '//tmp//'/k.bin > '//tmp//'/k.short.txt' &
         //' && capnp convert binary:canonical schema/kitchen.capnp Sink < '//tmp//'/k.bin > '//tmp//'/k.can')) then
         call test_failed(error, 'capnp encode/decode kitchen')
         return
      end if
      call capnp_read_file('test/fixtures/kitchen.cgr.bin', bytes, err)
      if (err /= CAPNP_OK) then
         call skip_test(error, 'kitchen cgr missing')
         return
      end if
      call capnp_dyn_load(schema, bytes, err)
      call check_(error, err == CAPNP_OK, 'text: kitchen schema')
      idx = capnp_dyn_find(schema, 'Sink', err)
      call capnp_read_file(tmp//'/k.bin', bytes, err)
      call capnp_deserialize_bytes(bytes, msg, err)
      root = capnp_root(msg, err)
      call capnp_text_encode(schema, idx, root, text, err, pretty=.true.)
      call check_(error, err == CAPNP_OK, 'text: kitchen pretty encode')
      call check_file(error, text, tmp//'/k.txt.out', 'text: kitchen pretty')
      call capnp_text_encode(schema, idx, root, text, err, pretty=.false.)
      call check_(error, err == CAPNP_OK, 'text: kitchen short encode')
      call check_file(error, text, tmp//'/k.short.txt', 'text: kitchen short')
      call capnp_text_decode(schema, idx, src, decoded, err)
      call check_(error, err == CAPNP_OK, 'text: kitchen decode')
      call capnp_canonicalize(decoded, ours, err)
      call capnp_read_file(tmp//'/k.can', ref, err)
      call check_bytes(error, ours, ref, 'text: kitchen canonical')
      call capnp_message_free(msg)
      call capnp_message_free(decoded)
      call capnp_dyn_free(schema)
   end subroutine run_kitchen

   subroutine run_text_edges(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg, decoded
      type(capnp_ptr_t) :: root
      integer(int8), allocatable :: bytes(:), ours(:), ref(:)
      character(len=:), allocatable :: src, text
      integer :: err, idx, uidx, beg, topos, disc
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      call compile_codec_schema(error, schema)
      if (allocated(error)) return
      idx = capnp_dyn_find(schema, 'Sample', err)
      uidx = capnp_dyn_find(schema, 'UnionDefaults', err)
      call check_(error, idx > 0 .and. uidx > 0, 'text-edges: nodes')

      ! Escape \xHH followed by hex char, \?, octal 0777, float from UINT64_MAX, 8-digit f32.
      src = '( big = -0777,'//achar(10)// &
            '  ubig = 0777,'//achar(10)// &
            '  tiny = 1.5038984e-36,'//achar(10)// &
            '  wide = 18446744073709551615,'//achar(10)// &
            '  title = "\x41B\?''caf'//achar(195)//achar(169)//'",'//achar(10)// &
            '  blob = "\x00\xffA" )'//achar(10)
      call spit(tmp//'/te.txt', src, err)
      if (.not. sh('INC=$(dirname $(dirname $(command -v capnp)))/include' &
         //' && capnp encode -I"$INC" schema/codec.capnp Sample < '//tmp//'/te.txt > '//tmp//'/te.bin' &
         //' && capnp decode -I"$INC" schema/codec.capnp Sample < '//tmp//'/te.bin > '//tmp//'/te.out' &
         //' && capnp convert -I"$INC" binary:canonical schema/codec.capnp Sample < '//tmp//'/te.bin > '//tmp//'/te.can')) then
         call test_failed(error, 'capnp encode/decode text-edges')
         call capnp_dyn_free(schema)
         return
      end if
      call capnp_text_decode(schema, idx, src, decoded, err)
      call check_(error, err == CAPNP_OK, 'text-edges: decode sample')
      call capnp_canonicalize(decoded, ours, err)
      call capnp_read_file(tmp//'/te.can', ref, err)
      call check_bytes(error, ours, ref, 'text-edges: canonical match')
      call capnp_read_file(tmp//'/te.bin', bytes, err)
      call capnp_deserialize_bytes(bytes, msg, err)
      root = capnp_root(msg, err)
      call capnp_text_encode(schema, idx, root, text, err, pretty=.true.)
      call check_file(error, text, tmp//'/te.out', 'text-edges: pretty match')
      call capnp_message_free(msg)
      call capnp_message_free(decoded)

      ! Active union pointer fields with null wire pointers must render schema defaults.
      do disc = 0, 4
         call capnp_message_init_builder(msg, err)
         root = capnp_dyn_new_root(schema, uidx, msg, err)
         call capnp_set_i32(root, 0_int64, 10_int32, err)
         call capnp_set_which(root, 2, disc, err)
         call capnp_set_i32(root, 8_int64, 20_int32, err)
         call capnp_serialize_bytes(msg, bytes, err)
         call capnp_write_file(tmp//'/ud.bin', bytes, err)
         if (.not. sh('INC=$(dirname $(dirname $(command -v capnp)))/include' &
            //' && capnp decode -I"$INC" schema/codec.capnp UnionDefaults < '//tmp//'/ud.bin > '//tmp//'/ud.txt')) then
            call test_failed(error, 'capnp decode UnionDefaults')
            call capnp_message_free(msg)
            call capnp_dyn_free(schema)
            return
         end if
         call capnp_text_encode(schema, uidx, root, text, err, pretty=.true.)
         call check_file(error, text, tmp//'/ud.txt', 'text-edges: union null ptr default')
         call capnp_message_free(msg)
      end do

      ! Malformed text inputs must be rejected cleanly.
      call capnp_text_decode(schema, idx, '(title = "\x4")', decoded, err)
      call check_(error, err /= CAPNP_OK, 'text-edges: reject 1-digit \x')
      call capnp_text_decode(schema, idx, '(title = "\q")', decoded, err)
      call check_(error, err /= CAPNP_OK, 'text-edges: reject unknown escape')
      call capnp_text_decode(schema, idx, '(title = "\u00e9")', decoded, err)
      call check_(error, err /= CAPNP_OK, 'text-edges: reject \u escape')
      call capnp_text_decode(schema, idx, '(title = "line1'//achar(10)//'line2")', decoded, err)
      call check_(error, err /= CAPNP_OK, 'text-edges: reject raw newline in string')
      call capnp_text_decode(schema, idx, '(ubig = 08)', decoded, err)
      call check_(error, err /= CAPNP_OK, 'text-edges: reject bad octal digit')
      call capnp_text_decode(schema, idx, '(wide = +inf)', decoded, err)
      call check_(error, err /= CAPNP_OK, 'text-edges: reject +inf')
      call capnp_text_decode(schema, idx, '(wide = infinity)', decoded, err)
      call check_(error, err /= CAPNP_OK, 'text-edges: reject infinity')
      call capnp_text_decode(schema, idx, '(scores = [1,,2])', decoded, err)
      call check_(error, err /= CAPNP_OK, 'text-edges: reject empty list item')
      call capnp_text_decode(schema, idx, '(scores = [1, 2))', decoded, err)
      call check_(error, err /= CAPNP_OK, 'text-edges: reject mismatched bracket')
      call capnp_text_span('(ubig = 1]', 1, beg, topos, err)
      call check_(error, err /= CAPNP_OK, 'text-edges: span rejects (]')

      call capnp_message_free(decoded)
      call capnp_dyn_free(schema)
   end subroutine run_text_edges

   subroutine run_text_generics(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg, decoded
      type(capnp_ptr_t) :: root
      integer(int8), allocatable :: bytes(:), ours(:), ref(:)
      character(len=:), allocatable :: src, text, jtxt
      integer :: err, idx
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      src = '( textBox = (value = "hello", label = "l1"),'//achar(10)// &
            '  boxes = [(value = "a"), (value = "b")],'//achar(10)// &
            '  listBox = (value = ["x", "y"]),'//achar(10)// &
            '  nest = (inner = (value = "deep"), boxes = [(value = "n1")]) )'//achar(10)
      if (.not. sh('mkdir -p '//tmp)) then
         call test_failed(error, 'mkdir')
         return
      end if
      call spit(tmp//'/bu.txt', src, err)
      if (.not. sh('capnp encode schema/box.capnp BoxUse < '//tmp//'/bu.txt > '//tmp//'/bu.bin' &
         //' && capnp decode schema/box.capnp BoxUse < '//tmp//'/bu.bin > '//tmp//'/bu.out' &
         //' && capnp convert binary:json schema/box.capnp BoxUse < '//tmp//'/bu.bin > '//tmp//'/bu.json' &
         //' && capnp convert binary:canonical schema/box.capnp BoxUse < '//tmp//'/bu.bin > '//tmp//'/bu.can')) then
         call test_failed(error, 'capnp generic BoxUse')
         return
      end if
      call capnp_read_file('test/fixtures/box.cgr.bin', bytes, err)
      call capnp_dyn_load(schema, bytes, err)
      idx = capnp_dyn_find(schema, 'BoxUse', err)
      call check_(error, idx > 0, 'generics: BoxUse node')
      call capnp_read_file(tmp//'/bu.bin', bytes, err)
      call capnp_deserialize_bytes(bytes, msg, err)
      root = capnp_root(msg, err)
      call capnp_text_encode(schema, idx, root, text, err, pretty=.true.)
      call check_(error, err == CAPNP_OK, 'generics: text encode')
      call check_file(error, text, tmp//'/bu.out', 'generics: text pretty')
      call capnp_text_decode(schema, idx, src, decoded, err)
      call check_(error, err == CAPNP_OK, 'generics: text decode')
      call capnp_canonicalize(decoded, ours, err)
      call capnp_read_file(tmp//'/bu.can', ref, err)
      call check_bytes(error, ours, ref, 'generics: text canonical')

      call capnp_json_encode(schema, idx, root, jtxt, err, pretty=.true.)
      call check_(error, err == CAPNP_OK, 'generics: json encode')
      call check_file(error, jtxt, tmp//'/bu.json', 'generics: json pretty')
      call slurp(tmp//'/bu.json', jtxt, err)
      call capnp_json_decode(schema, idx, jtxt, decoded, err)
      call check_(error, err == CAPNP_OK, 'generics: json decode')
      call capnp_canonicalize(decoded, ours, err)
      call check_bytes(error, ours, ref, 'generics: json canonical')

      call capnp_message_free(msg)
      call capnp_message_free(decoded)
      call capnp_dyn_free(schema)
   end subroutine run_text_generics

   subroutine run_json_addressbook(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg, decoded
      type(capnp_ptr_t) :: root
      integer(int8), allocatable :: bytes(:), ours(:), ref(:)
      character(len=:), allocatable :: text, file
      integer :: err, idx
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp// &
         ' && capnp convert binary:json schema/addressbook.capnp AddressBook' &
         //' < test/fixtures/addressbook.bin > '//tmp//'/ab.json' &
         //' && capnp convert binary:json --short schema/addressbook.capnp AddressBook' &
         //' < test/fixtures/addressbook.bin > '//tmp//'/ab.short.json')) then
         call test_failed(error, 'capnp convert addressbook json')
         return
      end if
      call capnp_read_file('test/fixtures/addressbook.cgr.bin', bytes, err)
      call capnp_dyn_load(schema, bytes, err)
      idx = capnp_dyn_find(schema, 'AddressBook', err)
      call capnp_read_file('test/fixtures/addressbook.bin', bytes, err)
      call capnp_deserialize_bytes(bytes, msg, err)
      root = capnp_root(msg, err)
      call capnp_json_encode(schema, idx, root, text, err, pretty=.true.)
      call check_(error, err == CAPNP_OK, 'json: pretty encode')
      call check_file(error, text, tmp//'/ab.json', 'json: addressbook pretty')
      call capnp_json_encode(schema, idx, root, text, err, pretty=.false.)
      call check_(error, err == CAPNP_OK, 'json: short encode')
      call check_file(error, text, tmp//'/ab.short.json', 'json: addressbook short')
      call slurp(tmp//'/ab.json', file, err)
      call capnp_json_decode(schema, idx, file, decoded, err)
      call check_(error, err == CAPNP_OK, 'json: addressbook decode')
      call capnp_canonicalize(decoded, ours, err)
      call capnp_read_file('test/fixtures/addressbook.canonical.bin', ref, err)
      call check_bytes(error, ours, ref, 'json: addressbook canonical')
      call capnp_message_free(msg)
      call capnp_message_free(decoded)
      call capnp_dyn_free(schema)
   end subroutine run_json_addressbook

   subroutine run_json_kitchen(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg, decoded
      type(capnp_ptr_t) :: root
      integer(int8), allocatable :: bytes(:), ours(:), ref(:)
      character(len=:), allocatable :: text, src
      integer :: err, idx
      src = kitchen_text()
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp)) then
         call test_failed(error, 'mkdir')
         return
      end if
      call spit(tmp//'/k.txt', src, err)
      if (.not. sh('capnp encode schema/kitchen.capnp Sink < '//tmp//'/k.txt > '//tmp//'/k.bin' &
         //' && capnp convert binary:json schema/kitchen.capnp Sink < '//tmp//'/k.bin > '//tmp//'/k.json' &
         //' && capnp convert binary:json --short schema/kitchen.capnp Sink < '//tmp//'/k.bin > '//tmp//'/k.short.json' &
         //' && capnp convert binary:canonical schema/kitchen.capnp Sink < '//tmp//'/k.bin > '//tmp//'/k.can')) then
         call test_failed(error, 'capnp json kitchen')
         return
      end if
      call capnp_read_file('test/fixtures/kitchen.cgr.bin', bytes, err)
      call capnp_dyn_load(schema, bytes, err)
      idx = capnp_dyn_find(schema, 'Sink', err)
      call capnp_read_file(tmp//'/k.bin', bytes, err)
      call capnp_deserialize_bytes(bytes, msg, err)
      root = capnp_root(msg, err)
      call capnp_json_encode(schema, idx, root, text, err, pretty=.true.)
      call check_(error, err == CAPNP_OK, 'json: kitchen encode')
      call check_file(error, text, tmp//'/k.json', 'json: kitchen pretty')
      call capnp_json_encode(schema, idx, root, text, err, pretty=.false.)
      call check_file(error, text, tmp//'/k.short.json', 'json: kitchen short')
      call slurp(tmp//'/k.json', text, err)
      call capnp_json_decode(schema, idx, text, decoded, err)
      call check_(error, err == CAPNP_OK, 'json: kitchen decode')
      call capnp_canonicalize(decoded, ours, err)
      call capnp_read_file(tmp//'/k.can', ref, err)
      call check_bytes(error, ours, ref, 'json: kitchen canonical')
      call capnp_message_free(msg)
      call capnp_message_free(decoded)
      call capnp_dyn_free(schema)
   end subroutine run_json_kitchen

   subroutine run_json_sample(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg, decoded
      type(capnp_ptr_t) :: root
      integer(int8), allocatable :: bytes(:), ours(:), ref(:)
      character(len=:), allocatable :: text, src
      integer :: err, idx
      src = '( big = -9223372036854775808,'//achar(10)// &
         '  ubig = 18446744073709551615,'//achar(10)// &
         '  tiny = inf,'//achar(10)// &
         '  wide = nan,'//achar(10)// &
         '  title = "n",'//achar(10)// &
         '  blob = "\000\377",'//achar(10)// &
         '  b64 = 0x"00 ff",'//achar(10)// &
         '  hexed = "AB",'//achar(10)// &
         '  hue = blue,'//achar(10)// &
         '  job = (label = "dev"),'//achar(10)// &
         '  where = (city = "Pune", zip = 411),'//achar(10)// &
         '  scores = [1, -1] )'//achar(10)
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      call compile_codec_schema(error, schema)
      if (allocated(error)) return
      call spit(tmp//'/s.txt', src, err)
      if (.not. sh('INC=$(dirname $(dirname $(command -v capnp)))/include' &
         //' && capnp encode -I"$INC" schema/codec.capnp Sample < '//tmp//'/s.txt > '//tmp//'/s.bin' &
         //' && capnp convert -I"$INC" binary:json schema/codec.capnp Sample < '//tmp//'/s.bin > '//tmp//'/s.json' &
         //' && capnp convert -I"$INC" binary:json --short schema/codec.capnp Sample < '//tmp//'/s.bin > '//tmp//'/s.short.json' &
         //' && capnp convert -I"$INC" binary:canonical schema/codec.capnp Sample < '//tmp//'/s.bin > '//tmp//'/s.can')) then
         call test_failed(error, 'capnp sample json')
         call capnp_dyn_free(schema)
         return
      end if
      idx = capnp_dyn_find(schema, 'Sample', err)
      call check_(error, idx > 0, 'json: Sample node')
      call capnp_read_file(tmp//'/s.bin', bytes, err)
      call capnp_deserialize_bytes(bytes, msg, err)
      root = capnp_root(msg, err)
      call capnp_json_encode(schema, idx, root, text, err, pretty=.true.)
      call check_(error, err == CAPNP_OK, 'json: sample encode runs')
      call check_file(error, text, tmp//'/s.json', 'json: sample pretty')
      call capnp_json_encode(schema, idx, root, text, err, pretty=.false.)
      call check_file(error, text, tmp//'/s.short.json', 'json: sample short')
      call slurp(tmp//'/s.json', text, err)
      call capnp_json_decode(schema, idx, text, decoded, err)
      call check_(error, err == CAPNP_OK, 'json: sample decode')
      call capnp_canonicalize(decoded, ours, err)
      call capnp_read_file(tmp//'/s.can', ref, err)
      call check_bytes(error, ours, ref, 'json: sample canonical')
      call capnp_message_free(msg)
      call capnp_message_free(decoded)
      call capnp_dyn_free(schema)
   end subroutine run_json_sample

   subroutine run_json_edges(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg, decoded
      type(capnp_ptr_t) :: root
      integer(int8), allocatable :: bytes(:), ours(:), ref(:)
      character(len=:), allocatable :: text, js
      integer :: err, idx, uidx, disc
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      call compile_codec_schema(error, schema)
      if (allocated(error)) return
      idx = capnp_dyn_find(schema, 'Sample', err)
      uidx = capnp_dyn_find(schema, 'UnionDefaults', err)

      ! UnionDefaults: variant 0 null pointer, non-0 null pointers with defaults, and after-union ordering.
      do disc = 0, 4
         call capnp_message_init_builder(msg, err)
         root = capnp_dyn_new_root(schema, uidx, msg, err)
         call capnp_set_i32(root, 0_int64, 11_int32, err)
         call capnp_set_which(root, 2, disc, err)
         call capnp_set_i32(root, 8_int64, 22_int32, err)
         call capnp_serialize_bytes(msg, bytes, err)
         call capnp_write_file(tmp//'/jud.bin', bytes, err)
         if (.not. sh('INC=$(dirname $(dirname $(command -v capnp)))/include' &
            //' && capnp convert -I"$INC" binary:json schema/codec.capnp' &
            //' UnionDefaults < '//tmp//'/jud.bin > '//tmp//'/jud.json' &
            //' && capnp convert -I"$INC" binary:json --short schema/codec.capnp' &
            //' UnionDefaults < '//tmp//'/jud.bin > '//tmp//'/jud.s.json')) then
            call test_failed(error, 'capnp convert UnionDefaults json')
            call capnp_message_free(msg)
            call capnp_dyn_free(schema)
            return
         end if
         call capnp_json_encode(schema, uidx, root, text, err, pretty=.true.)
         call check_file(error, text, tmp//'/jud.json', 'json-edges: union defaults pretty')
         call capnp_json_encode(schema, uidx, root, text, err, pretty=.false.)
         call check_file(error, text, tmp//'/jud.s.json', 'json-edges: union defaults short')
         call capnp_message_free(msg)
      end do

      ! UInt64 JSON number >= 2^63, decimal "0777" string, and null base64/hex Data.
      js = '{"ubig":10000000000000000000,"big":"0777","b64":null,"hexed":null,"hue":"azure"}'
      call spit(tmp//'/je.json', js, err)
      if (.not. sh('INC=$(dirname $(dirname $(command -v capnp)))/include' &
         //' && capnp convert -I"$INC" json:canonical schema/codec.capnp Sample < '//tmp//'/je.json > '//tmp//'/je.can')) then
         call test_failed(error, 'capnp convert json-edges')
         call capnp_dyn_free(schema)
         return
      end if
      call capnp_json_decode(schema, idx, js, decoded, err)
      call check_(error, err == CAPNP_OK, 'json-edges: decode u64>=2^63 + null b64/hex')
      call capnp_canonicalize(decoded, ours, err)
      call capnp_read_file(tmp//'/je.can', ref, err)
      call check_bytes(error, ours, ref, 'json-edges: canonical match')

      ! Negative JSON inputs.
      call capnp_json_decode(schema, idx, '{"hue":"blue"}', decoded, err)
      call check_(error, err /= CAPNP_OK, 'json-edges: reject overridden enum name')
      call capnp_json_decode(schema, idx, '{"big":1.5}', decoded, err)
      call check_(error, err /= CAPNP_OK, 'json-edges: reject fractional int')
      call capnp_json_decode(schema, idx, '{"ubig":-1}', decoded, err)
      call check_(error, err /= CAPNP_OK, 'json-edges: reject negative uint')
      call capnp_json_decode(schema, uidx, '{"u2sub":null}', decoded, err)
      call check_(error, err /= CAPNP_OK, 'json-edges: reject null struct')
      call capnp_json_decode(schema, idx, '{"display_name":null}', decoded, err)
      call check_(error, err /= CAPNP_OK, 'json-edges: reject null text')

      call capnp_message_free(decoded)
      call capnp_dyn_free(schema)
   end subroutine run_json_edges

   subroutine run_json_flatten_disc(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: decoded
      integer :: err, idx
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      call compile_codec_schema(error, schema)
      if (allocated(error)) return

      ! 1. Nested $Json.flatten (populated and null sub-structs).
      call verify_json_case(error, schema, 'FlatEnvelope', &
         '(id = 7, direct = (city = "Bern", code = 3000), '// &
         'nested = (midId = 9, place = (city = "Graz", code = 8010)), '// &
         'grp = (flag = true), renamedGrp = (val = 42))', &
         'flatten: populated')
      call verify_json_case(error, schema, 'FlatEnvelope', &
         '(id = 7)', 'flatten: null substructs')

      ! 2. $Json.discriminator(name = "kind") across Void, primitive, flattened, and $Json.name variants.
      call verify_json_case(error, schema, 'TaggedEvent', &
         '(seq = 1, idle = void, tail = 9)', 'disc: void')
      call verify_json_case(error, schema, 'TaggedEvent', &
         '(seq = 2, count = 55, tail = 9)', 'disc: primitive')
      call verify_json_case(error, schema, 'TaggedEvent', &
         '(seq = 3, moved = (city = "Oslo", code = 150), tail = 9)', 'disc: flattened variant')
      call verify_json_case(error, schema, 'TaggedEvent', &
         '(seq = 4, note = "hello", tail = 9)', 'disc: renamed variant')

      ! 3. $Json.discriminator(name = "type", valueName = "payload").
      call verify_json_case(error, schema, 'ValueEvent', &
         '(seq = 1, empty = void, tail = 8)', 'disc-val: void')
      call verify_json_case(error, schema, 'ValueEvent', &
         '(seq = 2, num = -12, tail = 8)', 'disc-val: primitive')
      call verify_json_case(error, schema, 'ValueEvent', &
         '(seq = 3, loc = (city = "Lima", code = 51), tail = 8)', 'disc-val: struct')
      call verify_json_case(error, schema, 'ValueEvent', &
         '(seq = 4, renamed = "tagged", tail = 8)', 'disc-val: renamed')

      ! 4. Named unions with $Json.flatten + $Json.discriminator() and $Json.name + $Json.discriminator(...).
      call verify_json_case(error, schema, 'MultiUnionDoc', &
         '(id = 10, mode = (fast = 3), extra = (off = void))', 'multi-union: fast/off')
      call verify_json_case(error, schema, 'MultiUnionDoc', &
         '(id = 11, mode = (custom = (city = "Riga", code = 1050)), extra = (on = "ready"))', &
         'multi-union: custom/on')

      ! Non-string discriminator must be rejected; unknown discriminator tag must be ignored like C++.
      idx = capnp_dyn_find(schema, 'TaggedEvent', err)
      call capnp_json_decode(schema, idx, '{"kind":123}', decoded, err)
      call check_(error, err /= CAPNP_OK, 'disc: reject non-string tag')
      call capnp_json_decode(schema, idx, '{"seq":5,"kind":"future_variant","x":1}', decoded, err)
      call check_(error, err == CAPNP_OK, 'disc: ignore unknown variant tag')

      call capnp_message_free(decoded)
      call capnp_dyn_free(schema)
   end subroutine run_json_flatten_disc

   subroutine verify_json_case(error, schema, tname, src_txt, label)
      type(error_type), allocatable, intent(inout) :: error
      type(capnp_dyn_schema_t), intent(in) :: schema
      character(len=*), intent(in) :: tname, src_txt, label
      type(capnp_message_t), target :: msg, decoded
      type(capnp_ptr_t) :: root
      integer(int8), allocatable :: bytes(:), ours(:), ref(:)
      character(len=:), allocatable :: text, jsrc
      integer :: err, idx
      if (allocated(error)) return
      idx = capnp_dyn_find(schema, tname, err)
      call check_(error, idx > 0, label//': node')
      if (allocated(error)) return
      call spit(tmp//'/vc.txt', src_txt//achar(10), err)
      if (.not. sh('INC=$(dirname $(dirname $(command -v capnp)))/include' &
         //' && capnp encode -I"$INC" schema/codec.capnp '//tname//' < '//tmp//'/vc.txt > '//tmp//'/vc.bin' &
         //' && capnp convert -I"$INC" binary:json schema/codec.capnp '//tname//' < '//tmp//'/vc.bin > '//tmp//'/vc.json' &
         //' && capnp convert -I"$INC" binary:json --short schema/codec.capnp ' &
         //tname//' < '//tmp//'/vc.bin > '//tmp//'/vc.s.json' &
         //' && capnp convert -I"$INC" binary:canonical schema/codec.capnp '//tname//' < '//tmp//'/vc.bin > '//tmp//'/vc.can')) then
         call test_failed(error, label//': capnp reference failed')
         return
      end if
      call capnp_read_file(tmp//'/vc.bin', bytes, err)
      call capnp_deserialize_bytes(bytes, msg, err)
      root = capnp_root(msg, err)
      call capnp_json_encode(schema, idx, root, text, err, pretty=.true.)
      call check_(error, err == CAPNP_OK, label//': encode pretty')
      call check_file(error, text, tmp//'/vc.json', label//': pretty match')
      call capnp_json_encode(schema, idx, root, text, err, pretty=.false.)
      call check_(error, err == CAPNP_OK, label//': encode short')
      call check_file(error, text, tmp//'/vc.s.json', label//': short match')
      call slurp(tmp//'/vc.json', jsrc, err)
      call capnp_json_decode(schema, idx, jsrc, decoded, err)
      call check_(error, err == CAPNP_OK, label//': decode')
      call capnp_canonicalize(decoded, ours, err)
      call capnp_read_file(tmp//'/vc.can', ref, err)
      call check_bytes(error, ours, ref, label//': canonical match')
      call capnp_message_free(msg)
      call capnp_message_free(decoded)
   end subroutine verify_json_case

   subroutine run_cli(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=:), allocatable :: got, exp
      integer :: err
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp// &
         ' && set -- build/gfortran_*/app/capnpf && test $# -eq 1 && CAP=$1' &
         //' && test -n "$CAP"' &
         //' && "$CAP" decode schema/addressbook.capnp AddressBook < test/fixtures/addressbook.bin > '//tmp//'/cli.txt' &
         //' && capnp decode schema/addressbook.capnp AddressBook < test/fixtures/addressbook.bin > '//tmp//'/cli.ref' &
         //' && "$CAP" convert binary:json --short schema/addressbook.capnp AddressBook' &
         //' < test/fixtures/addressbook.bin > '//tmp//'/cli.json' &
         //' && capnp convert binary:json --short schema/addressbook.capnp AddressBook' &
         //' < test/fixtures/addressbook.bin > '//tmp//'/cli.json.ref' &
         //' && "$CAP" eval schema/kitchen.capnp answer > '//tmp//'/cli.eval' &
         //' && capnp eval schema/kitchen.capnp answer > '//tmp//'/cli.eval.ref' &
         //' && "$CAP" eval schema/kitchen.capnp home > '//tmp//'/cli.home' &
         //' && capnp eval schema/kitchen.capnp home > '//tmp//'/cli.home.ref')) then
         call test_failed(error, 'capnpf did not run')
         return
      end if
      call slurp(tmp//'/cli.txt', got, err)
      call slurp(tmp//'/cli.ref', exp, err)
      call check_(error, got == exp, 'cli: decode addressbook')
      call slurp(tmp//'/cli.json', got, err)
      call slurp(tmp//'/cli.json.ref', exp, err)
      call check_(error, got == exp, 'cli: convert json')
      call slurp(tmp//'/cli.eval', got, err)
      call slurp(tmp//'/cli.eval.ref', exp, err)
      call check_(error, got == exp, 'cli: eval answer')
      call slurp(tmp//'/cli.home', got, err)
      call slurp(tmp//'/cli.home.ref', exp, err)
      call check_(error, got == exp, 'cli: eval home')
   end subroutine run_cli

   subroutine run_cli_extended(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=:), allocatable :: got, exp
      integer(int8), allocatable :: b_got(:), b_exp(:)
      integer :: err
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if

      ! 1. flat-packed encode, decode, convert, and multi-segment schema-less binary:flat / binary:flat-packed.
      if (.not. sh('mkdir -p '//tmp// &
         ' && set -- build/gfortran_*/app/capnpf && test $# -eq 1 && CAP=$1' &
         //' && INC=$(dirname $(dirname $(command -v capnp)))/include' &
         //' && printf "(x = 1.5, y = -2.5, z = 4)\n"' &
         //' | "$CAP" encode --flat -p schema/common.capnp Vec3 > '//tmp//'/fp.ours' &
         //' && printf "(x = 1.5, y = -2.5, z = 4)\n"' &
         //' | capnp encode --flat -p schema/common.capnp Vec3 > '//tmp//'/fp.ref' &
         //' && "$CAP" decode --flat -p --short schema/common.capnp Vec3 < '//tmp//'/fp.ref > '//tmp//'/fp.dec.ours' &
         //' && capnp decode --flat -p --short schema/common.capnp Vec3 < '//tmp//'/fp.ref > '//tmp//'/fp.dec.ref' &
         //' && printf "(people = [(id = 1, name = \"Alice\", email = \"a@b.c\")])\n"' &
         //' | capnp encode --segment-size=2 schema/addressbook.capnp AddressBook > '//tmp//'/mseg.bin' &
         //' && "$CAP" convert binary:flat-packed < '//tmp//'/mseg.bin > '//tmp//'/mseg.fp' &
         //' && "$CAP" convert flat-packed:canonical < '//tmp//'/mseg.fp > '//tmp//'/mseg.can.ours' &
         //' && capnp convert binary:canonical < '//tmp//'/mseg.bin > '//tmp//'/mseg.can.ref')) then
         call test_failed(error, 'cli-ext: flat-packed / multi-seg failed')
         return
      end if
      call capnp_read_file(tmp//'/fp.ours', b_got, err)
      call capnp_read_file(tmp//'/fp.ref', b_exp, err)
      call check_bytes(error, b_got, b_exp, 'cli-ext: encode --flat -p bytes')
      call slurp(tmp//'/fp.dec.ours', got, err)
      call slurp(tmp//'/fp.dec.ref', exp, err)
      call check_(error, got == exp, 'cli-ext: decode --flat -p text')
      call capnp_read_file(tmp//'/mseg.can.ours', b_got, err)
      call capnp_read_file(tmp//'/mseg.can.ref', b_exp, err)
      call check_bytes(error, b_got, b_exp, 'cli-ext: multi-segment flat-packed roundtrip')

      ! 2. Multi-message packed, text, and JSON streams.
      if (.not. sh('set -- build/gfortran_*/app/capnpf && test $# -eq 1 && CAP=$1' &
         //' && printf "(x = 1, y = 2, z = 3)\n(x = 4, y = 5, z = 6)\n"' &
         //' | "$CAP" encode -p schema/common.capnp Vec3 > '//tmp//'/two.pbin' &
         //' && "$CAP" decode -p --short schema/common.capnp Vec3 < '//tmp//'/two.pbin > '//tmp//'/two.pdec.ours' &
         //' && capnp decode -p --short schema/common.capnp Vec3 < '//tmp//'/two.pbin > '//tmp//'/two.pdec.ref' &
         //' && "$CAP" convert packed:json --short schema/common.capnp Vec3 < '//tmp//'/two.pbin > '//tmp//'/two.json' &
         //' && "$CAP" convert json:text --short schema/common.capnp Vec3 < '//tmp//'/two.json > '//tmp//'/two.jdec.ours')) then
         call test_failed(error, 'cli-ext: multi-message streams failed')
         return
      end if
      call slurp(tmp//'/two.pdec.ours', got, err)
      call slurp(tmp//'/two.pdec.ref', exp, err)
      call check_(error, got == exp, 'cli-ext: multi-message packed decode')
      call slurp(tmp//'/two.jdec.ours', got, err)
      call check_(error, got == exp, 'cli-ext: multi-message json->text convert')

      ! 3. Deep eval paths and binary/packed/flat/flat-packed/canonical/json eval output formats.
      if (.not. sh('set -- build/gfortran_*/app/capnpf && test $# -eq 1 && CAP=$1' &
         //' && INC=$(dirname $(dirname $(command -v capnp)))/include' &
         //' && "$CAP" eval -I "$INC" schema/codec.capnp "evalConst.items[1].matrix[1][0]" > '//tmp//'/ev1.ours' &
         //' && capnp eval -I"$INC" schema/codec.capnp "evalConst.items[1].matrix[1][0]" > '//tmp//'/ev1.ref' &
         //' && "$CAP" eval -I"$INC" schema/codec.capnp "EvalRoot.items[1].matrix[1][1]" > '//tmp//'/ev2.ours' &
         //' && capnp eval -I"$INC" schema/codec.capnp "EvalRoot.items[1].matrix[1][1]" > '//tmp//'/ev2.ref' &
         //' && "$CAP" eval -I"$INC" schema/codec.capnp "EvalRoot.nestedPlace.city" > '//tmp//'/ev3.ours' &
         //' && capnp eval -I"$INC" schema/codec.capnp "EvalRoot.nestedPlace.city" > '//tmp//'/ev3.ref' &
         //' && "$CAP" eval -I"$INC" schema/codec.capnp "Sample.where.zip" > '//tmp//'/ev4.ours' &
         //' && capnp eval -I"$INC" schema/codec.capnp "Sample.where.zip" > '//tmp//'/ev4.ref' &
         //' && "$CAP" eval -I"$INC" -ojson --short schema/codec.capnp "evalConst.home" > '//tmp//'/evj.ours' &
         //' && capnp eval -I"$INC" -ojson --short schema/codec.capnp "evalConst.home" > '//tmp//'/evj.ref' &
         //' && "$CAP" eval -I"$INC" -b schema/codec.capnp "evalConst.home" | capnp convert binary:canonical > '//tmp//'/evb.ours' &
         //' && capnp eval -I"$INC" -b schema/codec.capnp "evalConst.home" | capnp convert binary:canonical > '//tmp//'/evb.ref' &
         //' && "$CAP" eval -I"$INC" -p schema/codec.capnp "evalConst.home" | capnp convert packed:canonical > '//tmp//'/evp.ours' &
         //' && "$CAP" eval -I"$INC" --flat schema/codec.capnp "evalConst.home"' &
         //' | capnp convert flat:canonical > '//tmp//'/evf.ours' &
         //' && "$CAP" eval -I"$INC" -oflat-packed schema/codec.capnp "evalConst.home"' &
         //' | capnp convert flat-packed:canonical > '//tmp//'/evfp.ours' &
         //' && "$CAP" eval -I"$INC" -ocanonical schema/codec.capnp "evalConst.home" > '//tmp//'/evc.ours')) then
         call test_failed(error, 'cli-ext: deep/binary eval failed')
         return
      end if
      call slurp(tmp//'/ev1.ours', got, err)
      call slurp(tmp//'/ev1.ref', exp, err)
      call check_(error, got == exp, 'cli-ext: eval const deep list/field/matrix')
      call slurp(tmp//'/ev2.ours', got, err)
      call slurp(tmp//'/ev2.ref', exp, err)
      call check_(error, got == exp, 'cli-ext: eval struct default deep path')
      call slurp(tmp//'/ev3.ours', got, err)
      call slurp(tmp//'/ev3.ref', exp, err)
      call check_(error, got == exp, 'cli-ext: eval nested const field')
      call slurp(tmp//'/ev4.ours', got, err)
      call slurp(tmp//'/ev4.ref', exp, err)
      call check_(error, got == exp, 'cli-ext: eval group uint16 default')
      call slurp(tmp//'/evj.ours', got, err)
      call slurp(tmp//'/evj.ref', exp, err)
      call check_(error, got == exp, 'cli-ext: eval -ojson struct')
      call capnp_read_file(tmp//'/evb.ref', b_exp, err)
      call capnp_read_file(tmp//'/evb.ours', b_got, err)
      call check_bytes(error, b_got, b_exp, 'cli-ext: eval -b')
      call capnp_read_file(tmp//'/evp.ours', b_got, err)
      call check_bytes(error, b_got, b_exp, 'cli-ext: eval -p')
      call capnp_read_file(tmp//'/evf.ours', b_got, err)
      call check_bytes(error, b_got, b_exp, 'cli-ext: eval --flat')
      call capnp_read_file(tmp//'/evfp.ours', b_got, err)
      call check_bytes(error, b_got, b_exp, 'cli-ext: eval -oflat-packed')
      call capnp_read_file(tmp//'/evc.ours', b_got, err)
      call check_bytes(error, b_got, b_exp, 'cli-ext: eval -ocanonical')

      ! 4. Flags, exit codes, stderr separation, and zero leaked /tmp/capnpf-*.cgr files.
      call check_(error, sh('set -- build/gfortran_*/app/capnpf && test $# -eq 1 && CAP=$1' &
         //' && "$CAP" --help >/dev/null && "$CAP" --version >/dev/null'), 'cli-ext: --help/--version exit 0')
      call check_(error, sh('set -- build/gfortran_*/app/capnpf && test $# -eq 1 && CAP=$1' &
         //' && ! "$CAP" encode schema/common.capnp Vec3 < /dev/null 2>/dev/null' &
         //' && ! (printf "   \n" | "$CAP" encode schema/common.capnp Vec3 2>/dev/null)' &
         //' && ! "$CAP" eval -b schema/kitchen.capnp answer 2>/dev/null' &
         //' && ! "$CAP" eval -ojson schema/kitchen.capnp answer 2>/dev/null' &
         //' && ! "$CAP" eval schema/kitchen.capnp "primes[9]" 2>/dev/null' &
         //' && ! "$CAP" convert bogus:text schema/common.capnp Vec3 < /dev/null 2>/dev/null'), &
         'cli-ext: error exit codes')
      call check_(error, sh('! ls /tmp/capnpf-*.cgr >/dev/null 2>&1'), &
         'cli-ext: no leaked /tmp/capnpf-*.cgr files')
   end subroutine run_cli_extended

   subroutine compile_codec_schema(error, schema)
      type(error_type), allocatable, intent(inout) :: error
      type(capnp_dyn_schema_t), intent(inout), target :: schema
      integer(int8), allocatable :: bytes(:)
      integer :: err
      if (.not. sh('mkdir -p '//tmp// &
         ' && INC=$(dirname $(dirname $(command -v capnp)))/include' &
         //' && capnp compile -I"$INC" -o- schema/codec.capnp > '//tmp//'/codec.cgr')) then
         call test_failed(error, 'capnp compile codec schema')
         return
      end if
      call capnp_read_file(tmp//'/codec.cgr', bytes, err)
      call capnp_dyn_load(schema, bytes, err)
      call check_(error, err == CAPNP_OK, 'load codec.cgr')
   end subroutine compile_codec_schema

   function kitchen_text() result(src)
      character(len=:), allocatable :: src
      src = '( flag = false,'//achar(10)// &
         '  count = 0,'//achar(10)// &
         '  ratio = nan,'//achar(10)// &
         '  label = "a\"b\\c",'//achar(10)// &
         '  payload = "\001\177\200",'//achar(10)// &
         '  origin = (x = -inf, y = nan, z = 1e306),'//achar(10)// &
         '  state = down,'//achar(10)// &
         '  tags = ["hi", "a\n"],'//achar(10)// &
         '  blobs = [0x"00 ff"],'//achar(10)// &
         '  grid = [[1, -2], [], [3]],'//achar(10)// &
         '  spots = [(x = 1.5, y = 0, z = -0)],'//achar(10)// &
         '  scores = [-9223372036854775808, 9223372036854775807] )'//achar(10)
   end function kitchen_text

   subroutine check_file(error, got, path, name)
      type(error_type), allocatable, intent(inout) :: error
      character(len=*), intent(in) :: got, path, name
      character(len=:), allocatable :: exp, mine
      integer :: err, i, n
      if (allocated(error)) return
      call slurp(path, exp, err)
      if (err /= CAPNP_OK) then
         call test_failed(error, name//' missing reference')
         return
      end if
      mine = got//achar(10)
      if (mine == exp) return
      call test_failed(error, name)
      n = min(len(mine), len(exp))
      do i = 1, n
         if (mine(i:i) /= exp(i:i)) exit
      end do
      print '(a,i0)', 'first text diff at ', i
      print '(a)', 'got: '//mine(max(1, i - 40):min(len(mine), i + 80))
      print '(a)', 'exp: '//exp(max(1, i - 40):min(len(exp), i + 80))
   end subroutine check_file

   subroutine check_bytes(error, got, exp, name)
      type(error_type), allocatable, intent(inout) :: error
      integer(int8), intent(in) :: got(:), exp(:)
      character(len=*), intent(in) :: name
      integer(int64) :: i
      if (allocated(error)) return
      if (size(got) /= size(exp)) then
         call test_failed(error, name)
         print '(a,i0,a,i0)', 'size got ', size(got), ' exp ', size(exp)
         return
      end if
      if (all(got == exp)) return
      call test_failed(error, name)
      do i = 0_int64, size(got, kind=int64) - 1_int64
         if (got(lbound(got, 1) + int(i)) /= exp(lbound(exp, 1) + int(i))) then
            print '(a,i0)', 'first byte diff at ', i
            exit
         end if
      end do
   end subroutine check_bytes

   subroutine slurp(path, s, err)
      character(len=*), intent(in) :: path
      character(len=:), allocatable, intent(out) :: s
      integer, intent(out) :: err
      integer(int8), allocatable :: b(:)
      integer :: i, n, lb
      call capnp_read_file(path, b, err)
      if (err /= CAPNP_OK) return
      n = size(b)
      lb = lbound(b, 1)
      allocate (character(len=n) :: s)
      do i = 1, n
         s(i:i) = char(capnp_u_of_i8(b(lb + i - 1)))
      end do
   end subroutine slurp

   subroutine spit(path, s, err)
      character(len=*), intent(in) :: path, s
      integer, intent(out) :: err
      integer(int8), allocatable :: b(:)
      integer :: i
      allocate (b(0:len(s) - 1))
      do i = 1, len(s)
         b(i - 1) = capnp_as_i8_local(iachar(s(i:i)))
      end do
      call capnp_write_file(path, b, err)
   end subroutine spit

   pure function capnp_as_i8_local(u) result(v)
      integer, intent(in) :: u
      integer(int8) :: v
      integer :: x
      x = iand(u, 255)
      if (x >= 128) then
         v = int(x - 256, int8)
      else
         v = int(x, int8)
      end if
   end function capnp_as_i8_local

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

end module test_codecs
