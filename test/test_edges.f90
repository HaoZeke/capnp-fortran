!> Differentials against capnp 1.4.0 for the inputs the codecs used to
!> reject or reshape, plus a message just past the default reader budget.
module test_edges
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed, skip_test
   use, intrinsic :: iso_fortran_env, only: int64
   use, intrinsic :: iso_c_binding, only: c_int
   use capnp
   use capnp_dynamic
   use cli_path, only: capnpf_setup
   implicit none
   private
   public :: collect_edges, release_edges_tmp

   character(len=:), allocatable :: tmp

   interface
      function c_getpid() bind(c, name="getpid") result(pid)
         import :: c_int
         integer(c_int) :: pid
      end function c_getpid
   end interface

contains

   subroutine collect_edges(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      character(len=16) :: buf
      write (buf, '(i0)') int(c_getpid())
      tmp = '/tmp/capnpf-edges-'//trim(buf)
      testsuite = [ &
         new_unittest("cpp-match", run_cpp_match), &
         new_unittest("text-layout", run_text_layout), &
         new_unittest("nest-depth", run_nest_depth), &
         new_unittest("text-depth", run_text_depth), &
         new_unittest("segment-sweep", run_segment_sweep), &
         new_unittest("far-landing-pad", run_far_pad), &
         new_unittest("huge-list", run_huge_list), &
         new_unittest("oversize-flat", run_oversize_flat) &
         ]
   end subroutine collect_edges

   subroutine release_edges_tmp()
      if (allocated(tmp)) call execute_command_line('rm -rf -- '//tmp, wait=.true.)
   end subroutine release_edges_tmp

   subroutine check_(error, cond, name)
      type(error_type), allocatable, intent(inout) :: error
      logical, intent(in) :: cond
      character(len=*), intent(in) :: name
      if (allocated(error)) return
      call check(error, cond, name)
   end subroutine check_

   function sh(cmd) result(ok)
      character(len=*), intent(in) :: cmd
      logical :: ok
      integer :: st, cs
      call execute_command_line(cmd, exitstat=st, cmdstat=cs, wait=.true.)
      ok = cs == 0 .and. st == 0
   end function sh

   function have_capnp() result(y)
      logical :: y
      y = sh('command -v capnp >/dev/null 2>&1')
   end function have_capnp

   subroutine spit(path, text, err)
      character(len=*), intent(in) :: path, text
      integer, intent(out) :: err
      integer :: u, ios
      err = 0
      open (newunit=u, file=path, status='replace', action='write', iostat=ios)
      if (ios /= 0) then
         err = ios
         return
      end if
      write (u, '(a)', iostat=ios) text
      if (ios /= 0) err = ios
      close (u)
   end subroutine spit

   subroutine run_cpp_match(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: sch = &
         'using Json = import "/capnp/compat/json.capnp";'//achar(10)// &
         '@0x9a1b2c3d4e5f6071;'//achar(10)// &
         'struct All {'//achar(10)// &
         '  i8 @0 :Int8; u8 @1 :UInt8; i32 @2 :Int32; i64 @3 :Int64; u64 @4 :UInt64;'//achar(10)// &
         '  f64 @5 :Float64; txt @6 :Text; dat @7 :Data;'//achar(10)// &
         '  hx @8 :Data $Json.hex; b64 @9 :Data $Json.base64; col @10 :Color;'//achar(10)// &
         '  li32 @11 :List(Int32);'//achar(10)// &
         '}'//achar(10)// &
         'enum Color { red @0; blue @1; }'//achar(10)
      integer :: err
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp)) then
         call test_failed(error, 'mkdir')
         return
      end if
      call spit(tmp//'/all.capnp', sch, err)
      call check_(error, err == 0, 'edges: wrote schema')
      call match(error, '(i32 = - 1)', 'text:text', 'space after minus')
      call match(error, '(i32=-1)', 'text:text', 'glued minus fails')
      call match(error, '(u64 = -0)', 'text:text', 'unsigned negative zero')
      call match(error, '(u64 = - 0)', 'text:text', 'unsigned spaced zero')
      call match(error, '(u8 = -1)', 'text:text', 'unsigned minus one fails')
      call match(error, '(i8 = 18446744073709551616)', 'text:text', 'i8 wraps 2^64')
      call match(error, '(i8 = 18446744073709551617)', 'text:text', 'i8 wraps 2^64+1')
      call match(error, '(i8 = 128)', 'text:text', 'i8 128 fails')
      call match(error, '(i32 = 010)', 'text:text', 'octal literal')
      call match(error, '(i32 = 0X10)', 'text:text', 'uppercase hex fails')
      call match(error, '(f64 = 5.)', 'text:text', 'text trailing dot')
      call match(error, '(f64 = 5.e1)', 'text:text', 'text dot exponent')
      call match(error, '(f64 = .5)', 'text:text', 'text leading dot fails')
      call match(error, '(txt = "\400\101")', 'text:text', 'octal escape low byte')
      call match(error, '(dat = 0x"")', 'text:text', 'empty hex blob fails')
      call match(error, '(dat = 0X"00")', 'text:text', 'uppercase blob fails')
      call match(error, '(dat = 0x"ab cd")', 'text:text', 'spaced hex blob')
      call match(error, '(i32 = 1)'//achar(10)//'(i32 = 2)', 'text:flat', 'two flat messages')
      call match(error, '{"f64":5.}', 'json:text', 'json trailing dot')
      call match(error, '{"f64":5.e1}', 'json:text', 'json dot exponent')
      call match(error, '{"f64":.5}', 'json:text', 'json leading dot fails')
      call match(error, '{"col":65537}', 'json:text', 'enum 65537')
      call match(error, '{"col":-2}', 'json:text', 'enum minus two')
      call match(error, '{"col":70000}', 'json:text', 'enum 70000')
      call match(error, '{"col":1.9}', 'json:text', 'enum 1.9')
      call match(error, '{"col":-1.9}', 'json:text', 'enum -1.9')
      call match(error, '{"col":4294967295}', 'json:text', 'enum 2^32-1')
      call match(error, '{"col":-2147483649}', 'json:text', 'enum below int32')
      call match(error, '{"hx":1}', 'json:text', 'hex number is empty')
      call match(error, '{"b64":true}', 'json:text', 'base64 bool is empty')
      call match(error, '{"hx":null}', 'json:text', 'hex null is empty')
      call match(error, '{"hx":"zz"}', 'json:text', 'hex zz')
      call match(error, '{"hx":"010"}', 'json:text', 'hex odd length')
      call match(error, '{"hx":"0g"}', 'json:text', 'hex bad nibble')
      call match(error, '{"hx":""}', 'json:text', 'hex empty')
      call match(error, '{"b64":"A"}', 'json:text', 'base64 one char')
      call match(error, '{"b64":"QQ"}', 'json:text', 'base64 QQ')
      call match(error, '{"b64":"*"}', 'json:text', 'base64 star')
      call match(error, '{"b64":"AAAAA"}', 'json:text', 'base64 extra')
      call match(error, '{"dat":["x"]}', 'json:text', 'data array string')
      call match(error, '{"dat":[true]}', 'json:text', 'data array bool')
      call match(error, '{"dat":[1]}', 'json:text', 'data array one')
      call match(error, '{"dat":[1.5]}', 'json:text', 'data array fraction fails')
      call match(error, '{"dat":[256]}', 'json:text', 'data array 256 fails')
      call match(error, &
         '{"txt":"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx","li32":[1,2,3,4,5,6,7,8,9]}', &
         '--segment-size=2 json:binary', 'fixed segment size')
   end subroutine run_cpp_match

   subroutine match(error, payload, conv, label)
      type(error_type), allocatable, intent(inout) :: error
      character(len=*), intent(in) :: payload, conv, label
      integer :: err
      character(len=:), allocatable :: cmd
      if (allocated(error)) return
      call spit(tmp//'/in.txt', payload, err)
      if (err /= 0) then
         call test_failed(error, 'edges: write '//label)
         return
      end if
      cmd = capnpf_setup()//' && INC=$(dirname $(dirname $(command -v capnp)))/include' &
         //' && capnp convert -I"$INC" '//conv//' '//tmp//'/all.capnp All < ' &
         //tmp//'/in.txt > '//tmp//'/ref.out 2>'//tmp//'/ref.err; echo $? > '//tmp//'/ref.rc' &
         //' && "$CAP" convert -I"$INC" '//conv//' '//tmp//'/all.capnp All < ' &
         //tmp//'/in.txt > '//tmp//'/ours.out 2>'//tmp//'/ours.err; echo $? > '//tmp//'/ours.rc' &
         //' && cmp -s '//tmp//'/ref.rc '//tmp//'/ours.rc' &
         //' && if grep -qx 0 '//tmp//'/ref.rc; then cmp -s '//tmp &
         //'/ref.out '//tmp//'/ours.out; else true; fi'
      call check_(error, sh(cmd), 'edges: '//label)
   end subroutine match

   subroutine run_text_layout(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: sch = &
         '@0xe82731ac4d9bf560;'//achar(10)// &
         'struct Two { a @0 :Text; b @1 :Text; }'//achar(10)
      integer :: err
      character(len=:), allocatable :: cmd
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp)) then
         call test_failed(error, 'mkdir')
         return
      end if
      call spit(tmp//'/two.capnp', sch, err)
      call check_(error, err == 0, 'layout: schema')
      call spit(tmp//'/two.txt', '(b = "B", a = "A")', err)
      if (err /= 0) then
         call test_failed(error, 'layout: input')
         return
      end if
      cmd = capnpf_setup()//' && INC=$(dirname $(dirname $(command -v capnp)))/include' &
         //' && capnp convert -I"$INC" text:binary '//tmp//'/two.capnp Two < ' &
         //tmp//'/two.txt > '//tmp//'/two.binary.ref' &
         //' && "$CAP" convert -I"$INC" text:binary '//tmp//'/two.capnp Two < ' &
         //tmp//'/two.txt > '//tmp//'/two.binary.ours' &
         //' && cmp -s '//tmp//'/two.binary.ref '//tmp//'/two.binary.ours' &
         //' && capnp convert -I"$INC" text:flat '//tmp//'/two.capnp Two < ' &
         //tmp//'/two.txt > '//tmp//'/two.flat.ref' &
         //' && "$CAP" convert -I"$INC" text:flat '//tmp//'/two.capnp Two < ' &
         //tmp//'/two.txt > '//tmp//'/two.flat.ours' &
         //' && cmp -s '//tmp//'/two.flat.ref '//tmp//'/two.flat.ours'
      call check_(error, sh(cmd), 'edges: shuffled text layout')
   end subroutine run_text_layout

   subroutine run_nest_depth(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: sch = &
         '@0x8e7d6c5b4a392817;'//achar(10)// &
         'struct Node { kid @0 :Node; v @1 :Int32; }'//achar(10)
      integer :: err
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp)) then
         call test_failed(error, 'mkdir')
         return
      end if
      call spit(tmp//'/node.capnp', sch, err)
      call check_(error, err == 0, 'nest: schema')
      call one_depth(error, 100, 'text')
      call one_depth(error, 100, 'json')
      call one_depth(error, 300, 'text')
      call one_depth(error, 300, 'json')
      call json_depth(error, 64, .true.)
      call json_depth(error, 65, .false.)
   end subroutine run_nest_depth

   subroutine one_depth(error, depth, fmt)
      type(error_type), allocatable, intent(inout) :: error
      integer, intent(in) :: depth
      character(len=*), intent(in) :: fmt
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root, cur, kid
      integer(int8), allocatable :: bytes(:)
      integer :: err, i
      character(len=16) :: dbuf
      if (allocated(error)) return
      call capnp_message_init_builder(msg, err)
      root = capnp_new_struct(msg, 1, 1, err)
      call capnp_set_i32(root, 0_int64, 1_int32, err)
      cur = root
      do i = 2, depth
         kid = capnp_new_struct(msg, 1, 1, err)
         call capnp_set_i32(kid, 0_int64, int(i, int32), err)
         call capnp_setp(cur, 0, kid, err)
         cur = kid
      end do
      call capnp_set_root(msg, root, err)
      call capnp_serialize_bytes(msg, bytes, err)
      call capnp_write_file(tmp//'/nest.bin', bytes, err)
      call capnp_message_free(msg)
      write (dbuf, '(i0)') depth
      call check_(error, err == 0 .and. sh(capnpf_setup() &
         //' && capnp convert binary:'//fmt//' '//tmp//'/node.capnp Node < ' &
         //tmp//'/nest.bin > '//tmp//'/nest.ref 2>'//tmp//'/nest.ref.err; echo $? > ' &
         //tmp//'/nest.ref.rc' &
         //' && "$CAP" convert binary:'//fmt//' '//tmp//'/node.capnp Node < ' &
         //tmp//'/nest.bin > '//tmp//'/nest.ours 2>'//tmp//'/nest.ours.err; echo $? > ' &
         //tmp//'/nest.ours.rc' &
         //' && cmp -s '//tmp//'/nest.ref.rc '//tmp//'/nest.ours.rc' &
         //' && grep -qx 0 '//tmp//'/nest.ref.rc' &
         //' && cmp -s '//tmp//'/nest.ref '//tmp//'/nest.ours'), &
         'edges: depth '//trim(dbuf)//' binary:'//fmt)
   end subroutine one_depth

   subroutine json_depth(error, depth, expect_ok)
      type(error_type), allocatable, intent(inout) :: error
      integer, intent(in) :: depth
      logical, intent(in) :: expect_ok
      character(len=:), allocatable :: json, cmd
      character(len=16) :: buf
      integer :: err, i
      if (allocated(error)) return
      json = '{"v":1}'
      do i = 0, depth - 2
         write (buf, '(i0)') i
         json = '{"kid":'//json//',"v":'//trim(buf)//'}'
      end do
      call spit(tmp//'/nest.json', json, err)
      if (err /= 0) then
         call test_failed(error, 'json depth: write')
         return
      end if
      cmd = capnpf_setup()//' && capnp convert json:binary '//tmp//'/node.capnp Node < ' &
         //tmp//'/nest.json > '//tmp//'/jd.ref 2>'//tmp//'/jd.ref.err; echo $? > ' &
         //tmp//'/jd.ref.rc' &
         //' && "$CAP" convert json:binary '//tmp//'/node.capnp Node < ' &
         //tmp//'/nest.json > '//tmp//'/jd.ours 2>'//tmp//'/jd.ours.err; echo $? > ' &
         //tmp//'/jd.ours.rc' &
         //' && cmp -s '//tmp//'/jd.ref.rc '//tmp//'/jd.ours.rc'
      if (expect_ok) then
         cmd = cmd//' && grep -qx 0 '//tmp//'/jd.ref.rc' &
            //' && cmp -s '//tmp//'/jd.ref '//tmp//'/jd.ours'
      else
         cmd = cmd//' && ! grep -qx 0 '//tmp//'/jd.ref.rc'
      end if
      write (buf, '(i0)') depth
      call check_(error, sh(cmd), 'edges: json depth '//trim(buf))
   end subroutine json_depth

   !> Check a shallow value and a deeper value without pinning a
   !> stack-size-dependent capnp implementation detail.
   subroutine run_text_depth(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: sch = &
         '@0x8e7d6c5b4a392817;'//achar(10)// &
         'struct Node { kid @0 :Node; v @1 :Int32; }'//achar(10)
      integer :: err
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp)) then
         call test_failed(error, 'mkdir')
         return
      end if
      call spit(tmp//'/node.capnp', sch, err)
      call check_(error, err == 0, 'text-depth: schema')
      call text_depth(error, 101)
      call text_depth(error, 1000)
   end subroutine run_text_depth

   subroutine text_depth(error, depth)
      type(error_type), allocatable, intent(inout) :: error
      integer, intent(in) :: depth
      character(len=:), allocatable :: text
      character(len=16) :: buf, dbuf
      integer :: err, i
      if (allocated(error)) return
      text = '(v = 1)'
      do i = 0, depth - 2
         write (buf, '(i0)') i
         text = '(kid = '//text//', v = '//trim(buf)//')'
      end do
      call spit(tmp//'/nest.txt', text, err)
      if (err /= 0) then
         call test_failed(error, 'text-depth: write')
         return
      end if
      write (dbuf, '(i0)') depth
      call check_(error, sh(capnpf_setup() &
         //' && capnp convert text:binary '//tmp//'/node.capnp Node < ' &
         //tmp//'/nest.txt > '//tmp//'/td.ref 2>'//tmp//'/td.ref.err; echo $? > ' &
         //tmp//'/td.ref.rc' &
         //' && "$CAP" convert text:binary '//tmp//'/node.capnp Node < ' &
         //tmp//'/nest.txt > '//tmp//'/td.ours 2>'//tmp//'/td.ours.err; echo $? > ' &
         //tmp//'/td.ours.rc' &
         //' && cmp -s '//tmp//'/td.ref.rc '//tmp//'/td.ours.rc' &
         //' && grep -qx 0 '//tmp//'/td.ref.rc' &
         //' && cmp -s '//tmp//'/td.ref '//tmp//'/td.ours'), &
         'edges: text depth '//trim(dbuf))
   end subroutine text_depth

   subroutine run_segment_sweep(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: sch = &
         'using Json = import "/capnp/compat/json.capnp";'//achar(10)// &
         '@0x9a1b2c3d4e5f6071;'//achar(10)// &
         'struct All {'//achar(10)// &
         '  i8 @0 :Int8; u8 @1 :UInt8; i32 @2 :Int32; i64 @3 :Int64; u64 @4 :UInt64;'//achar(10)// &
         '  f64 @5 :Float64; txt @6 :Text; dat @7 :Data;'//achar(10)// &
         '  hx @8 :Data $Json.hex; b64 @9 :Data $Json.base64; col @10 :Color;'//achar(10)// &
         '  li32 @11 :List(Int32);'//achar(10)// &
         '}'//achar(10)// &
         'enum Color { red @0; blue @1; }'//achar(10)
      character(len=*), parameter :: payload = &
         '{"txt":"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789",' &
         //'"li32":[1,2,3,4,5,6,7,8,9,10,11,12]}'
      integer :: err
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp)) then
         call test_failed(error, 'mkdir')
         return
      end if
      call spit(tmp//'/all.capnp', sch, err)
      call check_(error, err == 0, 'sweep: schema')
      call spit(tmp//'/seg.json', payload, err)
      call check_(error, err == 0, 'sweep: payload')
      call check_(error, sh(capnpf_setup() &
         //' && INC=$(dirname $(dirname $(command -v capnp)))/include' &
         //' && nerr=0' &
         //' && for n in $(seq 1 64); do' &
         //' capnp convert -I"$INC" --segment-size=$n json:binary '//tmp &
         //'/all.capnp All < '//tmp//'/seg.json > '//tmp//'/s.ref' &
         //' && "$CAP" convert -I"$INC" --segment-size=$n json:binary '//tmp &
         //'/all.capnp All < '//tmp//'/seg.json > '//tmp//'/s.ours' &
         //' && cmp -s '//tmp//'/s.ref '//tmp//'/s.ours || nerr=$((nerr+1));' &
         //' done && test "$nerr" = 0'), 'edges: segment-size 1..64')
   end subroutine run_segment_sweep

   !> Far pointer whose landing pad is the first word of the next segment.
   !> Segment size 2 fills the root segment with the root pointer and the
   !> one-word struct, so the text opens a new segment.
   subroutine run_far_pad(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: sch = &
         '@0xa1b2c3d4e5f60718;'//achar(10)// &
         'struct Pad { txt @0 :Text; }'//achar(10)
      integer :: err
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp)) then
         call test_failed(error, 'mkdir')
         return
      end if
      call spit(tmp//'/pad.capnp', sch, err)
      call check_(error, err == 0, 'pad: schema')
      call spit(tmp//'/pad.txt', '(txt = "hello")', err)
      call check_(error, sh(capnpf_setup() &
         //' && INC=$(dirname $(dirname $(command -v capnp)))/include' &
         //' && capnp convert -I"$INC" --segment-size=2 text:binary '//tmp &
         //'/pad.capnp Pad < '//tmp//'/pad.txt > '//tmp//'/pad.ref' &
         //' && "$CAP" convert -I"$INC" --segment-size=2 text:binary '//tmp &
         //'/pad.capnp Pad < '//tmp//'/pad.txt > '//tmp//'/pad.ours' &
         //' && cmp -s '//tmp//'/pad.ref '//tmp//'/pad.ours'), &
         'edges: far landing pad')
   end subroutine run_far_pad

   subroutine run_huge_list(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root, lst
      integer(int8), allocatable :: bytes(:), cgr(:)
      integer :: err, idx
      character(len=*), parameter :: sch = &
         '@0x8f0c3d2b1a947e66;'//achar(10)// &
         'struct IntList { xs @0 :List(Int32); }'//achar(10)
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp)) then
         call test_failed(error, 'mkdir')
         return
      end if
      call spit(tmp//'/intlist.capnp', sch, err)
      if (.not. sh('capnp compile -o- '//tmp//'/intlist.capnp > '//tmp//'/intlist.cgr')) then
         call test_failed(error, 'huge: compile')
         return
      end if
      call capnp_read_file(tmp//'/intlist.cgr', cgr, err)
      call capnp_dyn_load(schema, cgr, err)
      idx = capnp_dyn_find(schema, 'IntList', err)
      call capnp_message_init_builder(msg, err)
      root = capnp_dyn_new_root(schema, idx, msg, err)
      lst = capnp_dyn_init_list(schema, idx, root, 'xs', 17000000_int64, err)
      call check_(error, err == CAPNP_OK .and. capnp_list_len(lst) == 17000000_int64, &
                  'huge: allocated')
      call capnp_serialize_bytes(msg, bytes, err)
      call capnp_write_file(tmp//'/huge.bin', bytes, err)
      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
      call check_(error, err == 0, 'huge: wrote')
      call check_(error, sh(capnpf_setup() &
         //' && capnp convert binary:json '//tmp//'/intlist.capnp IntList < ' &
         //tmp//'/huge.bin > '//tmp//'/huge.json.ref; echo $? > '//tmp//'/huge.json.ref.rc' &
         //' && "$CAP" convert binary:json '//tmp//'/intlist.capnp IntList < ' &
         //tmp//'/huge.bin > '//tmp//'/huge.json.ours; echo $? > '//tmp//'/huge.json.ours.rc' &
         //' && cmp -s '//tmp//'/huge.json.ref.rc '//tmp//'/huge.json.ours.rc' &
         //' && grep -qx 0 '//tmp//'/huge.json.ref.rc' &
         //' && cmp -s '//tmp//'/huge.json.ref '//tmp//'/huge.json.ours'), &
         'edges: 17M list binary:json')
      call check_(error, sh(capnpf_setup() &
         //' && capnp convert binary:text '//tmp//'/intlist.capnp IntList < ' &
         //tmp//'/huge.bin > '//tmp//'/huge.txt.ref; echo $? > '//tmp//'/huge.txt.ref.rc' &
         //' && "$CAP" convert binary:text '//tmp//'/intlist.capnp IntList < ' &
         //tmp//'/huge.bin > '//tmp//'/huge.txt.ours; echo $? > '//tmp//'/huge.txt.ours.rc' &
         //' && cmp -s '//tmp//'/huge.txt.ref.rc '//tmp//'/huge.txt.ours.rc' &
         //' && grep -qx 0 '//tmp//'/huge.txt.ref.rc' &
         //' && cmp -s '//tmp//'/huge.txt.ref '//tmp//'/huge.txt.ours'), &
         'edges: 17M list binary:text')
   end subroutine run_huge_list

   subroutine run_oversize_flat(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int8), allocatable :: big(:)
      integer(int64), parameter :: nwords = 8388609_int64
      integer(int64) :: nbytes
      integer :: err
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp)) then
         call test_failed(error, 'mkdir')
         return
      end if
      nbytes = 8_int64 + nwords*CAPNP_WORD_BYTES
      allocate (big(0:nbytes - 1))
      big = 0_int8
      big(0) = 0_int8
      big(4) = 1_int8
      big(6) = int(-128, int8)
      call cp_put_i64(big, 8_int64, wp_make_struct(0_int32, 1_int32, 0_int32))
      call cp_put_i64(big, 16_int64, 42_int64)
      call capnp_write_file(tmp//'/over.bin', big, err)
      call check_(error, err == 0 .and. sh(capnpf_setup() &
         //' && capnp convert binary:binary < '//tmp//'/over.bin > '//tmp//'/over.ref' &
         //'; echo $? > '//tmp//'/over.ref.rc' &
         //' && "$CAP" convert binary:binary < '//tmp//'/over.bin > '//tmp//'/over.ours' &
         //'; echo $? > '//tmp//'/over.ours.rc' &
         //' && cmp -s '//tmp//'/over.ref.rc '//tmp//'/over.ours.rc' &
         //' && grep -qx 0 '//tmp//'/over.ref.rc' &
         //' && cmp -s '//tmp//'/over.ref '//tmp//'/over.ours'), &
         'edges: binary:binary past eight mebiwords')
   end subroutine run_oversize_flat

end module test_edges
