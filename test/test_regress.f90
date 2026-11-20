!> Regressions for the text and JSON codec review.
module test_regress
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed, skip_test
   use, intrinsic :: iso_fortran_env, only: int64
   use capnp
   use capnp_dynamic
   use capnp_text
   use capnp_json
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
         new_unittest("text-leniency", run_text_leniency), &
         new_unittest("unloaded-guard", run_unloaded), &
         new_unittest("json-unloaded-guard", run_json_unloaded), &
         new_unittest("long-argument", run_long_arg), &
         new_unittest("schema-pipe", run_schema_pipe) &
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
      s = 'set -- build/gfortran_*/app/capnpf && test $# -eq 1 && CAP=$1'
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
      call check_(error, err == CAPNP_OK .and. size(ours) == size(cref) .and. all(ours == cref), &
                  'json: missing discriminator matches capnp')
      call capnp_message_free(decoded)
      call capnp_dyn_free(schema)
   end subroutine run_json_void_missing

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
end module test_regress
