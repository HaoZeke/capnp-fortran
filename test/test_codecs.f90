!> Text codec against the capnp 1.4 tool: pretty and short text, and
!> canonical bytes of a decoded literal.
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
         new_unittest("json-addressbook", run_json_addressbook), &
         new_unittest("json-kitchen", run_json_kitchen), &
         new_unittest("json-sample", run_json_sample), &
         new_unittest("cli", run_cli) &
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
         ' && capnp decode schema/addressbook.capnp AddressBook'//' < test/fixtures/addressbook.bin > '//tmp//'/ab.txt' &
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
      if (.not. sh('mkdir -p '//tmp// &
         ' && INC=$(dirname $(dirname $(command -v capnp)))/include' &
         //' && capnp compile -I"$INC" -o- schema/codec.capnp > '//tmp//'/codec.cgr')) then
         call test_failed(error, 'capnp compile codec schema')
         return
      end if
      call spit(tmp//'/s.txt', src, err)
      if (.not. sh('INC=$(dirname $(dirname $(command -v capnp)))/include' &
         //' && capnp encode -I"$INC" schema/codec.capnp Sample < '//tmp//'/s.txt > '//tmp//'/s.bin' &
         //' && capnp convert -I"$INC" binary:json schema/codec.capnp Sample < '//tmp//'/s.bin > '//tmp//'/s.json' &
         //' && capnp convert -I"$INC" binary:json --short schema/codec.capnp Sample < '//tmp//'/s.bin > '//tmp//'/s.short.json' &
         //' && capnp convert -I"$INC" binary:canonical schema/codec.capnp Sample < '//tmp//'/s.bin > '//tmp//'/s.can')) then
         call test_failed(error, 'capnp sample json')
         return
      end if
      call capnp_read_file(tmp//'/codec.cgr', bytes, err)
      call capnp_dyn_load(schema, bytes, err)
      call check_(error, err == CAPNP_OK, 'json: sample schema')
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

   subroutine run_cli(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=:), allocatable :: got, exp
      integer :: err
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp// &
         ' && CAP=$(find build -type f -name capnpf -perm -111 | head -1)' &
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
