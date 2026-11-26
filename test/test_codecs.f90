!> Text, JSON, and CLI codecs differential-tested against the capnp 1.4 tool.
module test_codecs
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed, skip_test
   use capnp
   use capnp_codec, only: capnp_u_of_i8, capnp_parse_f64, capnp_parse_f32, &
      capnp_fmt_f64, capnp_fmt_f32
   use capnp_dynamic
   use capnp_text
   use capnp_json
   use cli_path, only: capnpf_setup
   use, intrinsic :: iso_c_binding, only: c_int, c_char, c_ptr, c_double, c_float, &
      c_null_char, c_loc, c_associated
   implicit none
   private
   public :: collect_codecs, release_codecs_tmp

   character(len=:), allocatable :: tmp

   !> glibc's LC_ALL. Elsewhere the probe below finds no comma radix and
   !> the locale case is skipped.
   integer(c_int), parameter :: GLIBC_LC_ALL = 6_c_int

   interface
      function c_getpid() bind(c, name="getpid") result(pid)
         import :: c_int
         integer(c_int) :: pid
      end function c_getpid
      function c_setlocale(cat, name) bind(c, name="setlocale") result(p)
         import :: c_int, c_char, c_ptr
         integer(c_int), value :: cat
         character(kind=c_char), intent(in) :: name(*)
         type(c_ptr) :: p
      end function c_setlocale
      function c_strtod(nptr, endptr) bind(c, name="strtod") result(v)
         import :: c_char, c_ptr, c_double
         character(kind=c_char), intent(in), target :: nptr(*)
         type(c_ptr), intent(out) :: endptr
         real(c_double) :: v
      end function c_strtod
      function c_strtof(nptr, endptr) bind(c, name="strtof") result(v)
         import :: c_char, c_ptr, c_float
         character(kind=c_char), intent(in), target :: nptr(*)
         type(c_ptr), intent(out) :: endptr
         real(c_float) :: v
      end function c_strtof
   end interface

contains

   subroutine init_codec_tmp()
      character(len=16) :: buf
      integer(c_int) :: pid
      if (allocated(tmp)) return
      pid = c_getpid()
      write (buf, '(i0)') pid
      tmp = '/tmp/capnpf-codec-'//trim(buf)
   end subroutine init_codec_tmp

   !> Remove this process's codec directory. A shared /tmp name collides
   !> when two suites run together.
   subroutine release_codecs_tmp()
      if (.not. allocated(tmp)) return
      if (.not. sh('rm -rf '//tmp)) return
   end subroutine release_codecs_tmp

   subroutine collect_codecs(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      call init_codec_tmp()
      ! One assignment per case. An array constructor of these values
      ! leaves the case-name temporaries allocated on gfortran.
      allocate (testsuite(24))
      testsuite(1) = new_unittest("text-addressbook", run_addressbook)
      testsuite(2) = new_unittest("text-kitchen", run_kitchen)
      testsuite(3) = new_unittest("text-edge-cases", run_text_edges)
      testsuite(4) = new_unittest("text-generics", run_text_generics)
      testsuite(5) = new_unittest("json-addressbook", run_json_addressbook)
      testsuite(6) = new_unittest("json-kitchen", run_json_kitchen)
      testsuite(7) = new_unittest("json-sample", run_json_sample)
      testsuite(8) = new_unittest("json-edge-cases", run_json_edges)
      testsuite(9) = new_unittest("json-flatten-discriminator", run_json_flatten_disc)
      testsuite(10) = new_unittest("cli", run_cli)
      testsuite(11) = new_unittest("cli-extended", run_cli_extended)
      testsuite(12) = new_unittest("codec-tmp", run_codec_tmp)
      testsuite(13) = new_unittest("text-multimsg-growth", run_text_multimsg_growth)
      testsuite(14) = new_unittest("json-multimsg-growth", run_json_multimsg_growth)
      testsuite(15) = new_unittest("text-comments-bom", run_text_comments_bom)
      testsuite(16) = new_unittest("text-backtick-lines", run_text_backtick_lines)
      testsuite(17) = new_unittest("text-backtick-delimiters", run_text_backtick_delimiters)
      testsuite(18) = new_unittest("text-list-bom", run_text_list_bom)
      testsuite(19) = new_unittest("text-list-depth-growth", run_text_list_depth_growth)
      testsuite(20) = new_unittest("json-nest-depth-growth", run_json_nest_depth_growth)
      testsuite(21) = new_unittest("json-empty-numbers", run_json_empty_numbers)
      testsuite(22) = new_unittest("float-parse-strtod", run_float_parse_strtod)
      testsuite(23) = new_unittest("float-comma-locale", run_float_comma_locale)
      testsuite(24) = new_unittest("json-raw-controls", run_json_raw_controls)
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
      call spit(tmp//'/box_use.txt', src, err)
      if (.not. sh('capnp encode schema/box.capnp BoxUse < '//tmp//'/box_use.txt > '//tmp//'/box_use.bin' &
         //' && capnp decode schema/box.capnp BoxUse < '//tmp//'/box_use.bin > '//tmp//'/box_use.out' &
         //' && capnp convert binary:json schema/box.capnp BoxUse < '//tmp//'/box_use.bin > '//tmp//'/box_use.json' &
         //' && capnp convert binary:canonical schema/box.capnp BoxUse < '//tmp//'/box_use.bin > '//tmp//'/box_use.can')) then
         call test_failed(error, 'capnp generic BoxUse')
         return
      end if
      call capnp_read_file('test/fixtures/box.cgr.bin', bytes, err)
      call capnp_dyn_load(schema, bytes, err)
      idx = capnp_dyn_find(schema, 'BoxUse', err)
      call check_(error, idx > 0, 'generics: BoxUse node')
      call capnp_read_file(tmp//'/box_use.bin', bytes, err)
      call capnp_deserialize_bytes(bytes, msg, err)
      root = capnp_root(msg, err)
      call capnp_text_encode(schema, idx, root, text, err, pretty=.true.)
      call check_(error, err == CAPNP_OK, 'generics: text encode')
      call check_file(error, text, tmp//'/box_use.out', 'generics: text pretty')
      call capnp_text_decode(schema, idx, src, decoded, err)
      call check_(error, err == CAPNP_OK, 'generics: text decode')
      call capnp_canonicalize(decoded, ours, err)
      call capnp_read_file(tmp//'/box_use.can', ref, err)
      call check_bytes(error, ours, ref, 'generics: text canonical')

      call capnp_json_encode(schema, idx, root, jtxt, err, pretty=.true.)
      call check_(error, err == CAPNP_OK, 'generics: json encode')
      call check_file(error, jtxt, tmp//'/box_use.json', 'generics: json pretty')
      call slurp(tmp//'/box_use.json', jtxt, err)
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

   !> A quoted number that is empty, blank, or a bare sign. capnp 1.4.0
   !> stores zero, prints the message, and exits 1.
   subroutine run_json_empty_numbers(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      integer :: idx, err
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      call compile_codec_schema(error, schema)
      if (allocated(error)) return
      idx = capnp_dyn_find(schema, 'Sample', err)
      call json_soft_case(error, schema, idx, '{"ubig":""}', 'uint64 empty')
      call json_soft_case(error, schema, idx, '{"big":""}', 'int64 empty')
      call json_soft_case(error, schema, idx, '{"where":{"zip":""}}', 'uint16 empty')
      call json_soft_case(error, schema, idx, '{"scores":["", "7"]}', 'int64 list empty')
      call json_soft_case(error, schema, idx, '{"wide":""}', 'float64 empty')
      call json_soft_case(error, schema, idx, '{"tiny":""}', 'float32 empty')
      call json_soft_case(error, schema, idx, '{"ubig":"-"}', 'uint64 bare minus')
      call json_soft_case(error, schema, idx, '{"ubig":"+"}', 'uint64 bare plus')
      call json_soft_case(error, schema, idx, '{"ubig":" "}', 'uint64 blank')
      call capnp_dyn_free(schema)
   end subroutine run_json_empty_numbers

   !> capnp 1.4.0 keeps raw control bytes inside a JSON string and prints
   !> them back escaped. A raw NUL ends its input, so that message fails.
   subroutine run_json_raw_controls(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: decoded
      type(capnp_ptr_t) :: root
      integer(int8), allocatable :: got(:), exp(:)
      character(len=:), allocatable :: js, text
      character(len=8) :: label
      integer :: idx, err, b
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      call compile_codec_schema(error, schema)
      if (allocated(error)) return
      idx = capnp_dyn_find(schema, 'Sample', err)
      do b = 0, 32
         if (allocated(error)) exit
         if (b == 32) then
            js = '{"display_name":"a'//achar(127)//'b"}'
         else
            js = '{"display_name":"a'//achar(b)//'b"}'
         end if
         write (label, '(a,z2.2)') 'raw 0x', merge(127, b, b == 32)
         call spit(tmp//'/raw.json', js, err)
         call capnp_json_decode(schema, idx, js, decoded, err)
         if (b == 0) then
            call check_(error, err /= CAPNP_OK, label//': decode fails')
            call check_(error, .not. sh('INC=$(dirname $(dirname $(command -v capnp)))/include' &
               //' && capnp convert -I"$INC" json:binary schema/codec.capnp Sample < ' &
               //tmp//'/raw.json > /dev/null 2>&1'), label//': capnp fails')
            call capnp_message_free(decoded)
            cycle
         end if
         call check_(error, err == CAPNP_OK, label//': decode')
         if (.not. sh('INC=$(dirname $(dirname $(command -v capnp)))/include' &
            //' && capnp convert -I"$INC" json:binary schema/codec.capnp Sample < ' &
            //tmp//'/raw.json > '//tmp//'/raw.bin' &
            //' && capnp convert -I"$INC" binary:json schema/codec.capnp Sample < ' &
            //tmp//'/raw.bin > '//tmp//'/raw.out')) then
            call test_failed(error, label//': capnp convert')
            exit
         end if
         call capnp_serialize_bytes(decoded, got, err)
         call capnp_read_file(tmp//'/raw.bin', exp, err)
         call check_bytes(error, got, exp, label//': message bytes')
         root = capnp_root(decoded, err)
         call capnp_json_encode(schema, idx, root, text, err)
         call check_(error, err == CAPNP_OK, label//': encode')
         call check_file(error, text, tmp//'/raw.out', label//': escaped output')
         call capnp_message_free(decoded)
      end do
      call capnp_dyn_free(schema)
   end subroutine run_json_raw_controls

   subroutine json_soft_case(error, schema, idx, js, label)
      type(error_type), allocatable, intent(inout) :: error
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: idx
      character(len=*), intent(in) :: js, label
      type(capnp_message_t), target :: decoded
      type(capnp_ptr_t) :: root
      character(len=:), allocatable :: text
      integer :: err
      logical :: soft
      if (allocated(error)) return
      call spit(tmp//'/soft.json', js, err)
      if (.not. sh('INC=$(dirname $(dirname $(command -v capnp)))/include' &
         //' && ! capnp convert -I"$INC" json:text schema/codec.capnp Sample < ' &
         //tmp//'/soft.json > '//tmp//'/soft.txt 2>/dev/null')) then
         call test_failed(error, label//': capnp did not report the number')
         return
      end if
      call capnp_json_decode(schema, idx, js, decoded, err, number_failed=soft)
      call check_(error, err == CAPNP_ERR_ARG, label//': decode error')
      call check_(error, soft, label//': recoverable')
      call check_(error, capnp_json_number_failed(), label//': legacy query')
      root = capnp_root(decoded, err)
      call capnp_text_encode(schema, idx, root, text, err, pretty=.true.)
      call check_(error, err == CAPNP_OK, label//': encode')
      call check_file(error, text, tmp//'/soft.txt', label//': message kept')
      call capnp_message_free(decoded)
   end subroutine json_soft_case

   !> capnp_parse_f64 and capnp_parse_f32 match strtod and strtof in the C
   !> locale: the same rejections and the same bits, NaN payloads included.
   subroutine run_float_parse_strtod(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: fixed(*) = [character(len=40) :: &
         '1.5', '-0', '+0.0', '.5', '5.', '1e5', '1E-5', '1e+400', '1e-400', &
         '4.9406564584124654e-324', '2.4703282292062327e-324', '2.4703282292062328e-324', &
         '1.7976931348623157e308', '1.7976931348623159e308', '0.1', '3.4028235e38', &
         '3.4028236e38', '1.4e-45', '7e-46', '7.1e-46', '1e99999999999999999999', &
         '0x1p3', '0X1.8P+1', '0x.8p0', '0x1.fffffffffffffp1023', &
         '0x1.fffffffffffff8p1023', '0x1p-1074', '0x1p-1075', '0x1.0000000000001p-1075', &
         '0x3p-1076', '0x1.00000000000008p0', '0x1.000000000000081p0', &
         '0x1.00000000000018p0', '0x1p-149', '0x1p-150', '0x1.000002p-150', &
         '0x1.fffffep127', '0x1.ffffffp127', '0x123456789abcdef0123p-20', &
         '0x0.0000000000000000001p0', '0x1', '0x1.', '-0x0p0', '0x1p-99999999999', &
         '0x1p99999999999', '0xp1', '0x', '0x.p1', '0x1p', '0x1p+', '0x1g', &
         'inf', '-INF', '+Inf', 'Infinity', '-infinity', 'infinit', 'infx', &
         'nan', '-NaN', '+nan', 'nan()', 'nan(123)', 'nan(0x7)', 'nan(0X7fffffffffffff)', &
         'nan(017)', 'nan(08)', 'nan(abc_9)', 'nan(0xffffffffffffffffffff)', &
         'nan(', 'nan(1', 'nan(-1)', 'nan( 1)', 'nanx', &
         ' 1.5', '  -2', '1.5 ', '1,5', ' ', '+', '-', '.', 'e5', '1e', &
         '1e+', '1.5x', '--1', '1..2', '+-1', '1e5.5', '0..', '00012.500', '0', '1']
      integer :: i, nbad
      integer :: seed
      character(len=:), allocatable :: fmt
      real(real64) :: one
      integer :: ferr
      call capnp_fmt_f64(1.0e-7_real64, fmt)
      call check_(error, fmt == '1e-07', 'float format: trim 1e-07')
      call capnp_fmt_f64(1.25_real64, fmt)
      call check_(error, fmt == '1.25', 'float format: trim 1.25')
      call capnp_parse_f64('0', one, ferr)
      call check_(error, ferr == CAPNP_OK .and. one == 0.0_real64, 'float parse: 0')
      call capnp_parse_f64('1', one, ferr)
      call check_(error, ferr == CAPNP_OK .and. one == 1.0_real64, 'float parse: 1')
      nbad = 0
      do i = 1, size(fixed)
         call strtod_case(trim(fixed(i)), nbad)
      end do
      call strtod_case('', nbad)
      call strtod_case(achar(9)//'7', nbad)
      call strtod_case(achar(10)//achar(11)//achar(12)//achar(13)//'-0x1p1', nbad)
      call strtod_case(repeat('9', 400)//'.5e-100', nbad)
      call strtod_case('0.'//repeat('0', 340)//'49406564584124654', nbad)
      call strtod_case('0x'//repeat('f', 300)//'p-2000', nbad)
      seed = 20261123
      do i = 1, 4000
         call strtod_case(random_decimal(seed), nbad)
         call strtod_case(random_hex(seed), nbad)
      end do
      call check_(error, nbad == 0, 'float parse: matches strtod in the C locale')
   contains
      function next_rand(s, n) result(r)
         integer, intent(inout) :: s
         integer, intent(in) :: n
         integer :: r
         s = int(mod(int(s, int64)*48271_int64, 2147483647_int64))
         r = mod(s, n)
      end function next_rand

      function random_decimal(s) result(t)
         integer, intent(inout) :: s
         character(len=:), allocatable :: t
         integer :: k, nd, dot
         character(len=8) :: ebuf
         t = ''
         if (next_rand(s, 2) == 1) t = '-'
         nd = 1 + next_rand(s, 24)
         dot = next_rand(s, nd + 1)
         do k = 1, nd
            if (k - 1 == dot) t = t//'.'
            t = t//achar(iachar('0') + next_rand(s, 10))
         end do
         write (ebuf, '(i0)') next_rand(s, 700) - 360
         t = t//'e'//trim(ebuf)
      end function random_decimal

      function random_hex(s) result(t)
         integer, intent(inout) :: s
         character(len=:), allocatable :: t
         character(len=*), parameter :: hx = '0123456789abcdefABCDEF'
         integer :: k, nd, dot, c
         character(len=8) :: ebuf
         t = ''
         if (next_rand(s, 2) == 1) t = '-'
         t = t//'0x'
         nd = 1 + next_rand(s, 20)
         dot = next_rand(s, nd + 1)
         do k = 1, nd
            if (k - 1 == dot) t = t//'.'
            c = 1 + next_rand(s, len(hx))
            t = t//hx(c:c)
         end do
         write (ebuf, '(i0)') next_rand(s, 2300) - 1150
         t = t//'p'//trim(ebuf)
      end function random_hex
   end subroutine run_float_parse_strtod

   subroutine strtod_case(s, nbad)
      character(len=*), intent(in) :: s
      integer, intent(inout) :: nbad
      character(kind=c_char), allocatable, target :: cs(:)
      type(c_ptr) :: endp
      real(c_double) :: cd
      real(c_float) :: cf
      real(real64) :: x
      real(real32) :: y
      integer :: i, n, err64, err32
      logical :: ok_c64, ok_c32
      n = len(s)
      allocate (cs(n + 1))
      do i = 1, n
         cs(i) = s(i:i)
      end do
      cs(n + 1) = c_null_char
      cd = c_strtod(cs, endp)
      ok_c64 = n > 0 .and. c_associated(endp, c_loc(cs(n + 1)))
      cf = c_strtof(cs, endp)
      ok_c32 = n > 0 .and. c_associated(endp, c_loc(cs(n + 1)))
      call capnp_parse_f64(s, x, err64)
      call capnp_parse_f32(s, y, err32)
      if ((err64 == CAPNP_OK) .neqv. ok_c64) then
         call strtod_miss('f64 accept', s)
      else if (ok_c64) then
         if (transfer(x, 0_int64) /= transfer(real(cd, real64), 0_int64)) &
            call strtod_miss('f64 bits', s)
      end if
      if ((err32 == CAPNP_OK) .neqv. ok_c32) then
         call strtod_miss('f32 accept', s)
      else if (ok_c32) then
         if (transfer(y, 0_int32) /= transfer(real(cf, real32), 0_int32)) &
            call strtod_miss('f32 bits', s)
      end if
   contains
      subroutine strtod_miss(what, s)
         character(len=*), intent(in) :: what, s
         nbad = nbad + 1
         if (nbad <= 10) print '(a)', 'strtod mismatch ('//what//'): "'//s(1:min(len(s), 80))//'"'
      end subroutine strtod_miss
   end subroutine strtod_case

   !> Floats read and print with a '.' radix while the C locale uses ','.
   !> The codecs give the same bytes and text as in the C locale.
   subroutine run_float_comma_locale(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: decoded
      type(capnp_ptr_t) :: root
      character(len=:), allocatable :: s, ref_txt, ref_json, got_txt, got_json
      character(len=*), parameter :: js = '{"wide":1.25,"tiny":"2.5","big":"-3"}'
      character(len=*), parameter :: txt = '(wide = 0.1, tiny = 1e-7, scores = [1, -2])'
      real(real64) :: x
      real(real32) :: y
      integer :: err, idx
      type(c_ptr) :: prev
      logical :: codecs
      codecs = have_capnp()
      if (codecs) then
         call compile_codec_schema(error, schema)
         if (allocated(error)) return
         idx = capnp_dyn_find(schema, 'Sample', err)
         call locale_round_trip(schema, idx, js, txt, ref_txt, ref_json, err)
         call check_(error, err == CAPNP_OK, 'comma locale: C reference')
         if (allocated(error)) return
      end if
      if (.not. comma_locale()) then
         prev = c_setlocale(GLIBC_LC_ALL, 'C'//c_null_char)
         if (codecs) call capnp_dyn_free(schema)
         call skip_test(error, 'no locale with a comma radix')
         return
      end if
      call capnp_parse_f64('1.5', x, err)
      call check_(error, err == CAPNP_OK .and. x == 1.5_real64, 'comma locale: parse 1.5')
      call capnp_parse_f64('1,5', x, err)
      call check_(error, err /= CAPNP_OK, 'comma locale: reject 1,5')
      call capnp_parse_f32('0.1', y, err)
      call check_(error, err == CAPNP_OK .and. y == 0.1_real32, 'comma locale: parse f32')
      call capnp_parse_f64('0x1.8p1', x, err)
      call check_(error, err == CAPNP_OK .and. x == 3.0_real64, 'comma locale: parse hex')
      call capnp_fmt_f64(0.1_real64, s)
      call check_(error, s == '0.1', 'comma locale: print 0.1')
      call capnp_fmt_f64(1.0e-7_real64, s)
      call check_(error, s == '1e-07', 'comma locale: print 1e-07')
      call capnp_fmt_f32(0.1_real32, s)
      call check_(error, s == '0.1', 'comma locale: print f32 0.1')
      if (codecs) then
         call locale_round_trip(schema, idx, js, txt, got_txt, got_json, err)
         call check_(error, err == CAPNP_OK, 'comma locale: codecs')
         if (err == CAPNP_OK) then
            call check_(error, got_txt == ref_txt, 'comma locale: json to text')
            call check_(error, got_json == ref_json, 'comma locale: text to json')
         end if
         call capnp_dyn_free(schema)
      end if
      prev = c_setlocale(GLIBC_LC_ALL, 'C'//c_null_char)
   contains
      subroutine locale_round_trip(schema, idx, js, txt, out_txt, out_json, err)
         type(capnp_dyn_schema_t), intent(in) :: schema
         integer, intent(in) :: idx
         character(len=*), intent(in) :: js, txt
         character(len=:), allocatable, intent(out) :: out_txt, out_json
         integer, intent(out) :: err
         call capnp_json_decode(schema, idx, js, decoded, err)
         if (err /= CAPNP_OK) return
         root = capnp_root(decoded, err)
         if (err /= CAPNP_OK) return
         call capnp_text_encode(schema, idx, root, out_txt, err, pretty=.false.)
         if (err /= CAPNP_OK) return
         call capnp_text_decode(schema, idx, txt, decoded, err)
         if (err /= CAPNP_OK) return
         root = capnp_root(decoded, err)
         if (err /= CAPNP_OK) return
         call capnp_json_encode(schema, idx, root, out_json, err, pretty=.false.)
         call capnp_message_free(decoded)
      end subroutine locale_round_trip
   end subroutine run_float_comma_locale

   !> Switch to a locale whose radix is ','. The environment comes first,
   !> so LC_NUMERIC=de_DE.UTF-8 selects it; then a few German names.
   function comma_locale() result(ok)
      logical :: ok
      character(len=*), parameter :: names(*) = [character(len=12) :: &
         '', 'de_DE.UTF-8', 'de_DE.utf8', 'de_DE']
      type(c_ptr) :: p
      integer :: i
      ok = .false.
      do i = 1, size(names)
         p = c_setlocale(GLIBC_LC_ALL, trim(names(i))//c_null_char)
         if (.not. c_associated(p)) cycle
         if (radix_is_comma()) then
            ok = .true.
            return
         end if
      end do
   end function comma_locale

   function radix_is_comma() result(y)
      logical :: y
      character(kind=c_char), target :: cs(4)
      type(c_ptr) :: endp
      real(c_double) :: v
      cs = [character(kind=c_char) :: '1', ',', '5', c_null_char]
      v = c_strtod(cs, endp)
      y = v == 1.5_c_double .and. c_associated(endp, c_loc(cs(4)))
   end function radix_is_comma

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

      ! A non-string discriminator is an error. An unknown string selects
      ! no arm by itself; member keys are still applied.
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

   subroutine run_text_multimsg_growth(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64) :: tsmall, tlarge
      real :: bytes_ratio, time_ratio
      call span_n(4000, tsmall)
      if (allocated(error)) return
      call span_n(16000, tlarge)
      if (allocated(error)) return
      bytes_ratio = 4.0
      time_ratio = real(tlarge)/real(max(1_int64, tsmall))
      write (*, '(a,3(1x,f0.3))') 'text-multimsg ratios bytes time normalized', &
         bytes_ratio, time_ratio, time_ratio/bytes_ratio
      call check_(error, tsmall > 0_int64 .and. time_ratio/bytes_ratio < 3.0, &
                  'text multimsg: linear growth')
   contains
      subroutine span_n(n, elapsed)
         integer, intent(in) :: n
         integer(int64), intent(out) :: elapsed
         character(len=:), allocatable :: text
         integer :: i, p, beg, last, err, rep
         double precision :: t0, t1
         text = repeat('(big=1)'//achar(10), n)
         ! Fastest repetition in process CPU time, so time spent descheduled
         ! on a busy machine does not count.
         elapsed = huge(1_int64)
         do rep = 1, 10
            p = 1
            call cpu_time(t0)
            do i = 1, n
               call capnp_text_span(text, p, beg, last, err)
               if (err /= CAPNP_OK .or. beg == 0) then
                  call test_failed(error, 'text multimsg: span')
                  return
               end if
               p = last + 1
            end do
            call cpu_time(t1)
            elapsed = min(elapsed, int((t1 - t0)*1.0d9, int64))
         end do
      end subroutine span_n
   end subroutine run_text_multimsg_growth

   subroutine run_json_multimsg_growth(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64) :: tsmall, tlarge
      real :: bytes_ratio, time_ratio
      call span_n(4000, tsmall)
      if (allocated(error)) return
      call span_n(16000, tlarge)
      if (allocated(error)) return
      bytes_ratio = 4.0
      time_ratio = real(tlarge)/real(max(1_int64, tsmall))
      write (*, '(a,3(1x,f0.3))') 'json-multimsg ratios bytes time normalized', &
         bytes_ratio, time_ratio, time_ratio/bytes_ratio
      call check_(error, tsmall > 0_int64 .and. time_ratio/bytes_ratio < 3.0, &
                  'json multimsg: linear growth')
   contains
      subroutine span_n(n, elapsed)
         integer, intent(in) :: n
         integer(int64), intent(out) :: elapsed
         character(len=:), allocatable :: text
         integer :: i, p, beg, last, err, rep
         double precision :: t0, t1
         text = repeat('{}'//achar(10), n)
         ! Fastest repetition in process CPU time, so time spent descheduled
         ! on a busy machine does not count.
         elapsed = huge(1_int64)
         do rep = 1, 10
            p = 1
            call cpu_time(t0)
            do i = 1, n
               call capnp_json_span(text, p, beg, last, err)
               if (err /= CAPNP_OK .or. beg == 0) then
                  call test_failed(error, 'json multimsg: span')
                  return
               end if
               p = last + 1
            end do
            call cpu_time(t1)
            elapsed = min(elapsed, int((t1 - t0)*1.0d9, int64))
         end do
      end subroutine span_n
   end subroutine run_json_multimsg_growth

   subroutine run_text_comments_bom(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=:), allocatable :: input, bom
      integer :: beg, last, err
      bom = achar(239)//achar(187)//achar(191)
      input = '# lead'//achar(10)//'(big=1) # tail'//achar(10)//bom//'(big=2)'
      call capnp_text_span(input, 1, beg, last, err)
      call check_(error, err == CAPNP_OK .and. input(beg:last) == '(big=1)', &
                  'text comments: leading and trailing')
      if (allocated(error)) return
      call capnp_text_span(input, last + 1, beg, last, err)
      call check_(error, err == CAPNP_OK .and. input(beg:last) == '(big=2)', &
                  'text comments: BOM between messages')
   end subroutine run_text_comments_bom

   subroutine run_text_backtick_lines(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root
      character(len=:), allocatable :: got
      integer :: err, idx
      call compile_codec_schema(error, schema)
      if (allocated(error)) return
      idx = capnp_dyn_find(schema, 'Sample', err)
      call check_(error, err == CAPNP_OK .and. idx > 0, 'text backtick: schema')
      if (allocated(error)) return
      call capnp_text_decode(schema, idx, '(title=`raw'//achar(10)//')', msg, err)
      call check_(error, err == CAPNP_OK, 'text backtick: LF decode')
      if (allocated(error)) return
      root = capnp_root(msg, err)
      call capnp_get_text(root, 0, got, err)
      call check_(error, err == CAPNP_OK .and. got == 'raw'//achar(10), 'text backtick: LF value')
      call capnp_message_free(msg)
      if (allocated(error)) return
      call capnp_text_decode(schema, idx, '(title=`raw'//achar(13)//achar(10)//')', msg, err)
      call check_(error, err == CAPNP_OK, 'text backtick: CRLF decode')
      if (allocated(error)) return
      root = capnp_root(msg, err)
      call capnp_get_text(root, 0, got, err)
      call check_(error, err == CAPNP_OK .and. got == 'raw'//achar(10), 'text backtick: CRLF value')
      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end subroutine run_text_backtick_lines

   subroutine run_text_backtick_delimiters(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg
      character(len=:), allocatable :: input
      integer :: err, idx, beg, last
      input = '(values=[`x,])'//achar(10)//'])'
      call capnp_text_span(input, 1, beg, last, err)
      call check_(error, err == CAPNP_OK .and. last == len(input), &
                  'text backtick delimiters: span')
      if (allocated(error)) return
      call compile_codec_schema(error, schema)
      if (allocated(error)) return
      idx = capnp_dyn_find(schema, 'TextList', err)
      call check_(error, err == CAPNP_OK .and. idx > 0, 'text backtick delimiters: schema')
      if (allocated(error)) return
      call capnp_text_decode(schema, idx, input, msg, err)
      call check_(error, err == CAPNP_OK, 'text backtick delimiters: list')
      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end subroutine run_text_backtick_delimiters

   subroutine run_text_list_bom(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root, values
      character(len=:), allocatable :: bom
      integer :: err, idx
      bom = achar(239)//achar(187)//achar(191)
      call compile_codec_schema(error, schema)
      if (allocated(error)) return
      idx = capnp_dyn_find(schema, 'TextList', err)
      call check_(error, err == CAPNP_OK .and. idx > 0, 'text list BOM: schema')
      if (allocated(error)) return
      call capnp_text_decode(schema, idx, '(values=['//bom//'])', msg, err)
      call check_(error, err == CAPNP_OK, 'text list BOM: decode')
      if (.not. allocated(error)) then
         root = capnp_root(msg, err)
         values = capnp_getp(root, 0, err)
         call check_(error, err == CAPNP_OK .and. capnp_list_len(values) == 0, 'text list BOM: empty')
      end if
      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end subroutine run_text_list_bom

   subroutine run_text_list_depth_growth(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      integer :: err, idx
      integer(int64) :: tsmall, tlarge
      integer :: bsmall, blarge
      real :: bytes_ratio, time_ratio
      call compile_codec_schema(error, schema)
      if (allocated(error)) return
      idx = capnp_dyn_find(schema, 'ListNest', err)
      call check_(error, err == CAPNP_OK .and. idx > 0, 'text list depth: schema')
      if (allocated(error)) return
      call decode_n(400, tsmall, bsmall)
      if (allocated(error)) return
      call decode_n(1600, tlarge, blarge)
      if (allocated(error)) return
      bytes_ratio = real(blarge)/real(bsmall)
      time_ratio = real(tlarge)/real(max(1_int64, tsmall))
      write (*, '(a,3(1x,f0.3))') 'text-list-depth ratios bytes time normalized', &
         bytes_ratio, time_ratio, time_ratio/bytes_ratio
      call check_(error, tsmall > 0_int64 .and. time_ratio/bytes_ratio < 2.0, &
                  'text list depth: linear growth')
      call capnp_dyn_free(schema)
   contains
      subroutine decode_n(depth, elapsed, bytes)
         integer, intent(in) :: depth
         integer(int64), intent(out) :: elapsed
         integer, intent(out) :: bytes
         type(capnp_message_t), target :: msg
         character(len=:), allocatable :: text
         integer :: i
         double precision :: t0, t1
         text = repeat('(sub=[', depth)//'()'//repeat('])', depth)
         bytes = len(text)
         ! Fastest repetition in process CPU time, so time spent descheduled
         ! on a busy machine does not count.
         elapsed = huge(1_int64)
         do i = 1, 8
            call cpu_time(t0)
            call capnp_text_decode(schema, idx, text, msg, err)
            call cpu_time(t1)
            elapsed = min(elapsed, int((t1 - t0)*1.0d9, int64))
            call check_(error, err == CAPNP_OK, 'text list depth: decode')
            call capnp_message_free(msg)
            if (allocated(error)) return
         end do
      end subroutine decode_n
   end subroutine run_text_list_depth_growth

   subroutine run_json_nest_depth_growth(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root
      integer :: err, idx
      integer(int64) :: tsmall, tlarge
      integer :: bsmall, blarge
      real :: bytes_ratio, time_ratio
      call compile_codec_schema(error, schema)
      if (allocated(error)) return
      idx = capnp_dyn_find(schema, 'Chain', err)
      call check_(error, err == CAPNP_OK .and. idx > 0, 'json nest: schema')
      if (allocated(error)) return
      call capnp_message_init_builder(msg, err)
      msg%depth_limit = 10000
      root = capnp_dyn_new_root(schema, idx, msg, err)
      call fill_chain(root, 8)
      call check_(error, err == CAPNP_OK, 'json nest: build')
      call capnp_message_free(msg)
      if (allocated(error)) return
      call render_n(400, tsmall, bsmall)
      if (allocated(error)) return
      call render_n(1600, tlarge, blarge)
      if (allocated(error)) return
      bytes_ratio = real(blarge)/real(max(1, bsmall))
      time_ratio = real(tlarge)/real(max(1_int64, tsmall))
      write (*, '(a,3(1x,f0.3))') 'json-nest-depth ratios bytes time normalized', &
         bytes_ratio, time_ratio, time_ratio/bytes_ratio
      write (*, '(a)') 'json nest: pretty indent makes output quadratic in depth'
      call check_(error, tsmall > 0_int64 .and. time_ratio/bytes_ratio < 2.0, &
                  'json nest: linear growth')
      call capnp_dyn_free(schema)
   contains
      recursive subroutine fill_chain(p, left)
         type(capnp_ptr_t), intent(in) :: p
         integer, intent(in) :: left
         type(capnp_ptr_t) :: child
         call capnp_set_i32(p, 0_int64, 1_int32, err)
         if (err /= CAPNP_OK .or. left <= 0) return
         child = capnp_new_struct(msg, 1, 1, err)
         if (err /= CAPNP_OK) return
         call capnp_setp(p, 0, child, err)
         if (err /= CAPNP_OK) return
         call fill_chain(child, left - 1)
      end subroutine fill_chain
      subroutine render_n(depth, elapsed, bytes)
         integer, intent(in) :: depth
         integer(int64), intent(out) :: elapsed
         integer, intent(out) :: bytes
         character(len=:), allocatable :: rendered
         integer :: i
         double precision :: t0, t1
         call capnp_message_init_builder(msg, err)
         msg%depth_limit = depth + 10
         root = capnp_dyn_new_root(schema, idx, msg, err)
         call fill_chain(root, depth)
         call check_(error, err == CAPNP_OK, 'json nest: chain')
         if (allocated(error)) return
         ! Fastest repetition in process CPU time, so time spent descheduled
         ! on a busy machine does not count.
         elapsed = huge(1_int64)
         do i = 1, 5
            call cpu_time(t0)
            call capnp_json_encode(schema, idx, root, rendered, err, pretty=.true.)
            call cpu_time(t1)
            elapsed = min(elapsed, int((t1 - t0)*1.0d9, int64))
            call check_(error, err == CAPNP_OK, 'json nest: render')
            if (allocated(error)) return
         end do
         bytes = len(rendered)
         call capnp_message_free(msg)
      end subroutine render_n
   end subroutine run_json_nest_depth_growth

   subroutine run_cli(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=:), allocatable :: got, exp
      integer :: err
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp// &
         ' && '//capnpf_setup() &
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
         ' && '//capnpf_setup() &
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
      if (.not. sh(capnpf_setup() &
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
      if (.not. sh(capnpf_setup() &
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

      ! 4. Flags, exit codes, stderr separation, and no capnpf cgr in this
      ! process directory. A sibling file under /tmp must not count.
      call check_(error, sh(capnpf_setup() &
         //' && "$CAP" --help >/dev/null && "$CAP" --version >/dev/null'), 'cli-ext: --help/--version exit 0')
      call check_(error, sh(capnpf_setup() &
         //' && ! "$CAP" encode schema/common.capnp Vec3 < /dev/null 2>/dev/null' &
         //' && ! (printf "   \n" | "$CAP" encode schema/common.capnp Vec3 2>/dev/null)' &
         //' && ! "$CAP" eval -b schema/kitchen.capnp answer 2>/dev/null' &
         //' && ! "$CAP" eval -ojson schema/kitchen.capnp answer 2>/dev/null' &
         //' && ! "$CAP" eval schema/kitchen.capnp "primes[9]" 2>/dev/null' &
         //' && ! "$CAP" convert bogus:text schema/common.capnp Vec3 < /dev/null 2>/dev/null'), &
         'cli-ext: error exit codes')
      call check_(error, index(tmp, '/tmp/capnpf-codec-') == 1, &
         'cli-ext: codec files live in a per-process directory')
      call check_(error, sh(capnpf_setup() &
         //' && export CAP' &
         //' && printf "%s\n" "echo \$\$ > \"\$1\"" "exec \"\$CAP\" --version >/dev/null"' &
         //' > '//tmp//'/id.sh' &
         //' && sh '//tmp//'/id.sh '//tmp//'/clipid' &
         //' && pid=$(cat '//tmp//'/clipid) && test ! -e /tmp/capnpf-$pid.cgr'), &
         'cli-ext: no leaked capnpf cgr for the child pid')
   end subroutine run_cli_extended

   !> The codec directory is removed when the suite finishes. This fails
   !> if the release leaves the directory in place.
   subroutine run_codec_tmp(error)
      type(error_type), allocatable, intent(out) :: error
      call init_codec_tmp()
      if (.not. sh('mkdir -p '//tmp//'/left && test -d '//tmp)) then
         call test_failed(error, 'codec temp dir was not created')
         return
      end if
      call release_codecs_tmp()
      if (.not. sh('test ! -d '//tmp)) then
         call test_failed(error, 'codec temp dir remains')
      end if
   end subroutine run_codec_tmp

   !> Without the capnp tool (emulated big-endian CI) the checked-in
   !> fixture stands in. With it, a fresh compile must decode to the same
   !> schema. capnp 1.4.0 stores declaration spans on each node and
   !> requestedFiles.fileSourceInfo, so schema/codec.capnp compiles to
   !> 32160 bytes; capnp 1.0.1 has no such fields and writes 26208 bytes.
   !> capnpVersion names the compiler. Drop that field and compare the
   !> decoded requests.
   subroutine compile_codec_schema(error, schema)
      type(error_type), allocatable, intent(inout) :: error
      type(capnp_dyn_schema_t), intent(inout), target :: schema
      character(len=*), parameter :: fixture = 'test/fixtures/codec.cgr.hex'
      integer(int8), allocatable :: bytes(:), kept(:)
      integer :: err
      if (.not. have_capnp()) then
         call read_hex_file(fixture, bytes, err)
         call check_(error, err == CAPNP_OK, 'read '//fixture)
         if (allocated(error)) return
         call capnp_dyn_load(schema, bytes, err)
         call check_(error, err == CAPNP_OK, 'load '//fixture)
         return
      end if
      if (.not. sh('mkdir -p '//tmp)) then
         call test_failed(error, 'mkdir')
         return
      end if
      call read_hex_file(fixture, kept, err)
      call check_(error, err == CAPNP_OK, 'read '//fixture)
      if (allocated(error)) return
      call capnp_write_file(tmp//'/codec.fixture.cgr', kept, err)
      call check_(error, err == CAPNP_OK, 'write '//fixture)
      if (allocated(error)) return
      if (.not. sh('INC=$(dirname $(dirname $(command -v capnp)))/include' &
         //' && capnp compile -I"$INC" -o- schema/codec.capnp > '//tmp//'/codec.cgr' &
         //' && capnp decode "$INC/capnp/schema.capnp" CodeGeneratorRequest < ' &
         //tmp//'/codec.cgr > '//tmp//'/codec.cgr.txt' &
         //' && capnp decode "$INC/capnp/schema.capnp" CodeGeneratorRequest < ' &
         //tmp//'/codec.fixture.cgr > '//tmp//'/codec.fix.txt' &
         //' && sed "/^  capnpVersion = /d" '//tmp//'/codec.cgr.txt > '//tmp//'/codec.cgr.sem' &
         //' && sed "/^  capnpVersion = /d" '//tmp//'/codec.fix.txt > '//tmp//'/codec.fix.sem' &
         //' && cmp -s '//tmp//'/codec.cgr.sem '//tmp//'/codec.fix.sem')) then
         call test_failed(error, fixture//' schema differs from capnp compile')
         return
      end if
      call capnp_read_file(tmp//'/codec.cgr', bytes, err)
      call check_(error, err == CAPNP_OK, 'read codec.cgr')
      if (allocated(error)) return
      call capnp_dyn_load(schema, bytes, err)
      call check_(error, err == CAPNP_OK, 'load codec.cgr')
   end subroutine compile_codec_schema

   !> Read lowercase hex digit pairs (as `od -An -tx1 -v` prints them),
   !> skipping whitespace and line breaks.
   subroutine read_hex_file(path, bytes, err)
      character(len=*), intent(in) :: path
      integer(int8), allocatable, intent(out) :: bytes(:)
      integer, intent(out) :: err
      character(len=*), parameter :: digits = '0123456789abcdef'
      character(len=1024) :: line
      integer(int8), allocatable :: buf(:)
      integer :: u, ios, i, d, hi, n
      err = CAPNP_ERR_IO
      open (newunit=u, file=path, status='old', action='read', iostat=ios)
      if (ios /= 0) return
      allocate (buf(4096))
      n = 0
      hi = -1
      do
         read (u, '(a)', iostat=ios) line
         if (ios /= 0) exit
         do i = 1, len_trim(line)
            if (line(i:i) == ' ') cycle
            d = index(digits, line(i:i)) - 1
            if (d < 0) then
               close (u)
               return
            end if
            if (hi < 0) then
               hi = d
               cycle
            end if
            if (n == size(buf)) buf = [buf, buf]
            n = n + 1
            d = 16*hi + d
            if (d > 127) d = d - 256
            buf(n) = int(d, int8)
            hi = -1
         end do
      end do
      close (u)
      if (.not. is_iostat_end(ios) .or. hi >= 0) return
      bytes = buf(:n)
      err = CAPNP_OK
   end subroutine read_hex_file

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
      st = 0
      cs = 0
      call execute_command_line(cmd, exitstat=st, cmdstat=cs, wait=.true.)
      ok = cs == 0 .and. st == 0
   end function sh

end module test_codecs
