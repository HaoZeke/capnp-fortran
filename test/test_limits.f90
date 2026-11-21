!> Depth and stream regressions that must fail on the parent of their fix.
module test_limits
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed, &
        skip_test
   use, intrinsic :: iso_c_binding, only: c_int
   use capnp
   use capnp_dynamic
   use capnp_text
   use capnp_json
   use cli_path, only: capnpf_setup
   implicit none
   private
   public :: collect_limits, release_limits_tmp

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
      tmp = '/tmp/capnpf-limits-'//trim(buf)
   end subroutine init_tmp

   subroutine release_limits_tmp()
      if (.not. allocated(tmp)) return
      if (.not. sh('rm -rf '//tmp)) return
   end subroutine release_limits_tmp

   subroutine collect_limits(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      call init_tmp()
      ! One assignment per case. An array constructor of these values
      ! leaves the case-name temporaries allocated on gfortran.
      allocate (testsuite(7))
      testsuite(1) = new_unittest("json-audit-depth", run_json_audit_depth)
      testsuite(2) = new_unittest("group-encode-depth", run_group_encode_depth)
      testsuite(3) = new_unittest("json-flat-depth", run_json_flat_depth)
      testsuite(4) = new_unittest("json-nul-stream", run_json_nul_stream)
      testsuite(5) = new_unittest("json-dfud70", run_json_dfud70)
      testsuite(6) = new_unittest("json-dfu65", run_json_dfu65)
      testsuite(7) = new_unittest("json-fs70", run_json_fs70)
   end subroutine collect_limits

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

   subroutine write_text(path, s, err)
      character(len=*), intent(in) :: path, s
      integer, intent(out) :: err
      integer :: u
      open (newunit=u, file=path, status='replace', action='write', iostat=err)
      if (err /= 0) return
      write (u, '(a)', iostat=err) s
      close (u)
   end subroutine write_text

   subroutine load_src(error, schema, src, fname, tname)
      type(error_type), allocatable, intent(inout) :: error
      type(capnp_dyn_schema_t), intent(out) :: schema
      character(len=*), intent(in) :: src, fname, tname
      integer(int8), allocatable :: bytes(:)
      integer :: err
      if (allocated(error)) return
      if (.not. sh('mkdir -p '//tmp)) then
         call test_failed(error, 'mkdir')
         return
      end if
      call write_text(tmp//'/'//fname, src, err)
      if (err /= 0) then
         call test_failed(error, 'write '//tname)
         return
      end if
      if (.not. sh('INC=$(dirname $(dirname $(command -v capnp)))/include' &
         //' && capnp compile -I"$INC" -o- '//tmp//'/'//fname &
         //' > '//tmp//'/'//tname//'.cgr')) then
         call test_failed(error, 'compile '//tname)
         return
      end if
      call capnp_read_file(tmp//'/'//tname//'.cgr', bytes, err)
      call capnp_dyn_load(schema, bytes, err)
      if (err /= CAPNP_OK) call test_failed(error, 'load '//tname)
   end subroutine load_src

   !> A flattened arm deeper than the old fixed audit list still decodes
   !> the other arm. Cap'n Proto C++ 1.4.0 accepts this schema.
   subroutine run_json_audit_depth(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t) :: msg
      character(len=:), allocatable :: src, num
      character(len=8) :: buf
      integer :: err, idx, i, n
      n = 135
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      src = '@0xb1a2c3d4e5f60718;'//achar(10)// &
            'using Json = import "/capnp/compat/json.capnp";'//achar(10)// &
            'struct DeepAudit {'//achar(10)// &
            '  union {'//achar(10)// &
            '    simple @0 :UInt32;'//achar(10)// &
            '    nest :group $Json.flatten() {'//achar(10)
      do i = 0, n - 1
         write (buf, '(i0)') i
         num = trim(buf)
         src = src//'      g'//num//' :group $Json.flatten() {'//achar(10)
      end do
      src = src//'      v @1 :UInt32;'//achar(10)
      do i = 1, n + 1
         src = src//'    }'//achar(10)
      end do
      src = src//'  }'//achar(10)//'}'//achar(10)
      call load_src(error, schema, src, 'audit.capnp', 'audit')
      if (allocated(error)) return
      idx = capnp_dyn_find(schema, 'DeepAudit', err)
      call check_(error, err == CAPNP_OK .and. idx > 0, 'audit schema')
      if (allocated(error)) then
         call capnp_dyn_free(schema)
         return
      end if
      call capnp_json_decode(schema, idx, '{"simple":1}', msg, err)
      call check_(error, err == CAPNP_OK, 'audit decode of the other arm')
      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end subroutine run_json_audit_depth

   !> Inline groups do not consume pointer depth while encoding.
   subroutine run_group_encode_depth(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t) :: msg
      type(capnp_ptr_t) :: root
      character(len=:), allocatable :: src, body, out, num
      character(len=8) :: buf
      integer(int8), allocatable :: bytes(:)
      integer :: err, idx, i, n
      n = 70
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      src = '@0xc1d2e3f405162738;'//achar(10)//'struct DeepG {'//achar(10)
      body = '('
      do i = 0, n - 1
         write (buf, '(i0)') i
         num = trim(buf)
         src = src//'  g'//num//' :group {'//achar(10)
         body = body//'g'//num//' = ('
      end do
      src = src//'  v @0 :UInt32;'//achar(10)
      body = body//'v = 1'
      do i = 1, n
         src = src//'}'//achar(10)
         body = body//')'
      end do
      src = src//'}'//achar(10)
      body = body//')'
      call load_src(error, schema, src, 'deepg.capnp', 'deepg')
      if (allocated(error)) return
      idx = capnp_dyn_find(schema, 'DeepG', err)
      call check_(error, err == CAPNP_OK .and. idx > 0, 'group schema')
      if (allocated(error)) then
         call capnp_dyn_free(schema)
         return
      end if
      call capnp_text_decode(schema, idx, body, msg, err)
      call check_(error, err == CAPNP_OK, 'group text decode')
      if (allocated(error)) then
         call capnp_message_free(msg)
         call capnp_dyn_free(schema)
         return
      end if
      ! Builders ignore the default depth cap. A reader uses 64.
      call capnp_serialize_bytes(msg, bytes, err)
      call capnp_message_free(msg)
      call capnp_deserialize_bytes(bytes, msg, err)
      call check_(error, err == CAPNP_OK, 'group reader')
      if (allocated(error)) then
         call capnp_message_free(msg)
         call capnp_dyn_free(schema)
         return
      end if
      root = capnp_root(msg, err)
      call capnp_text_encode(schema, idx, root, out, err, pretty=.false.)
      call check_(error, err == CAPNP_OK .and. index(out, 'v = 1') > 0, 'group text encode')
      call capnp_json_encode(schema, idx, root, out, err, pretty=.false.)
      call check_(error, err == CAPNP_OK .and. index(out, '"v":1') > 0, 'group json encode')
      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end subroutine run_group_encode_depth

   !> Flattened groups do not consume JSON container depth.
   subroutine run_json_flat_depth(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t) :: msg
      type(capnp_ptr_t) :: root
      character(len=:), allocatable :: src, out, num
      character(len=8) :: buf
      integer :: err, idx, i, n
      n = 70
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      src = '@0x9c8b7a6f5e4d3c2b;'//achar(10)// &
            'using Json = import "/capnp/compat/json.capnp";'//achar(10)// &
            'struct DeepFlat {'//achar(10)
      do i = 0, n - 1
         write (buf, '(i0)') i
         num = trim(buf)
         src = src//'  g'//num//' :group $Json.flatten() {'//achar(10)
      end do
      src = src//'  v @0 :UInt32;'//achar(10)
      do i = 1, n
         src = src//'}'//achar(10)
      end do
      src = src//'}'//achar(10)
      call load_src(error, schema, src, 'flat.capnp', 'flat')
      if (allocated(error)) return
      idx = capnp_dyn_find(schema, 'DeepFlat', err)
      call check_(error, err == CAPNP_OK .and. idx > 0, 'flat schema')
      if (allocated(error)) then
         call capnp_dyn_free(schema)
         return
      end if
      call capnp_json_decode(schema, idx, '{"v":7}', msg, err)
      call check_(error, err == CAPNP_OK, 'flat json decode')
      if (.not. allocated(error)) then
         root = capnp_root(msg, err)
         call capnp_json_encode(schema, idx, root, out, err, pretty=.false.)
         call check_(error, err == CAPNP_OK .and. index(out, '"v":7') > 0, 'flat json value')
      end if
      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end subroutine run_json_flat_depth

   !> Flattened groups inside a named union do not consume container depth.
   subroutine run_json_dfud70(error)
      type(error_type), allocatable, intent(out) :: error
      call flat_union_case(error, 70, .true., 'DFUD70', '{"kind":"g0","v":7}', 'dfud70')
   end subroutine run_json_dfud70

   !> Flattened groups inside an anonymous union do not consume container depth.
   subroutine run_json_dfu65(error)
      type(error_type), allocatable, intent(out) :: error
      call flat_union_case(error, 65, .false., 'DFU65', '{"v": 7}', 'dfu65')
   end subroutine run_json_dfu65

   subroutine flat_union_case(error, n, named, typ, payload, tag)
      type(error_type), allocatable, intent(inout) :: error
      integer, intent(in) :: n
      logical, intent(in) :: named
      character(len=*), intent(in) :: typ, payload, tag
      character(len=:), allocatable :: src, num
      character(len=8) :: buf
      integer :: i
      if (allocated(error)) return
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      src = '@0xd1e2f30415263748;'//achar(10)// &
            'using Json = import "/capnp/compat/json.capnp";'//achar(10)
      if (named) then
         src = src//'struct '//typ//' $Json.discriminator(name = "kind") {'//achar(10)
      else
         src = src//'struct '//typ//' {'//achar(10)
      end if
      src = src//'  union {'//achar(10)//'    none @0 :Void;'//achar(10)
      do i = 0, n - 1
         write (buf, '(i0)') i
         num = trim(buf)
         src = src//'    g'//num//' :group $Json.flatten() {'//achar(10)
      end do
      src = src//'      v @1 :UInt32;'//achar(10)
      do i = 1, n
         src = src//'    }'//achar(10)
      end do
      src = src//'  }'//achar(10)//'}'//achar(10)
      call cmp_json(error, src, typ, payload, tag)
   end subroutine flat_union_case

   !> A chain of flattened struct fields does not consume container depth.
   subroutine run_json_fs70(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=:), allocatable :: src, num, nxt
      character(len=8) :: buf
      integer :: i
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      src = '@0xe2f3041526374859;'//achar(10)// &
            'using Json = import "/capnp/compat/json.capnp";'//achar(10)
      do i = 0, 68
         write (buf, '(i0)') i
         num = trim(buf)
         write (buf, '(i0)') i + 1
         nxt = trim(buf)
         src = src//'struct FS'//num//' { s @0 :FS'//nxt//' $Json.flatten(); }'//achar(10)
      end do
      src = src//'struct FS69 { v @0 :UInt32; }'//achar(10)
      call cmp_json(error, src, 'FS0', '{"v": 7}', 'fs70')
   end subroutine run_json_fs70

   subroutine cmp_json(error, src, typ, payload, tag)
      type(error_type), allocatable, intent(inout) :: error
      character(len=*), intent(in) :: src, typ, payload, tag
      integer :: err
      if (allocated(error)) return
      if (.not. sh('mkdir -p '//tmp)) then
         call test_failed(error, 'mkdir')
         return
      end if
      call write_text(tmp//'/'//tag//'.capnp', src, err)
      call write_text(tmp//'/'//tag//'.json', payload, err)
      if (err /= 0) then
         call test_failed(error, tag//' write')
         return
      end if
      if (.not. sh(capnpf_setup() &
         //' && INC=$(dirname $(dirname $(command -v capnp)))/include' &
         //' && capnp convert -I"$INC" json:text --short '//tmp//'/'//tag//'.capnp '//typ &
         //' < '//tmp//'/'//tag//'.json > '//tmp//'/'//tag//'.ref 2>/dev/null' &
         //'; echo $? > '//tmp//'/'//tag//'.ref.rc' &
         //' && "$CAP" convert -I"$INC" json:text --short '//tmp//'/'//tag//'.capnp '//typ &
         //' < '//tmp//'/'//tag//'.json > '//tmp//'/'//tag//'.ours 2>/dev/null' &
         //'; echo $? > '//tmp//'/'//tag//'.ours.rc' &
         //' && cmp -s '//tmp//'/'//tag//'.ref '//tmp//'/'//tag//'.ours' &
         //' && cmp -s '//tmp//'/'//tag//'.ref.rc '//tmp//'/'//tag//'.ours.rc' &
         //' && grep -qx 0 '//tmp//'/'//tag//'.ours.rc')) then
         call test_failed(error, tag//' does not match capnp')
      end if
   end subroutine cmp_json

   !> A NUL ends one JSON message. Garbage up to the next object is kept
   !> out of that message, and a leading NUL is not a message boundary.
   subroutine run_json_nul_stream(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=:), allocatable :: a, b, gap
      a = '{"big":"1"}'
      b = '{"big":"2"}'
      call expect_two(a//achar(0)//'garbage'//b, a, b)
      call expect_two(a//' '//achar(0)//'  xx'//b, a, b)
      call expect_two(a//achar(0)//'gar[bage'//b, a, b)
      call expect_two(a//achar(0)//achar(0)//'junk'//achar(0)//b, a, b)
      gap = a//'garbage'//achar(0)//b
      call expect_fail_first(gap)
      call expect_fail_first(achar(0)//b)
      call expect_one(a//achar(0))
   contains
      subroutine expect_two(payload, first, second)
         character(len=*), intent(in) :: payload, first, second
         integer :: b1, t1, b2, t2, err
         if (allocated(error)) return
         call capnp_json_span(payload, 1, b1, t1, err)
         call check_(error, err == CAPNP_OK .and. b1 > 0, 'nul-stream: first span')
         if (allocated(error)) return
         call check_(error, payload(b1:t1) == first, 'nul-stream: first text')
         call capnp_json_span(payload, t1 + 1, b2, t2, err)
         call check_(error, err == CAPNP_OK .and. b2 > 0, 'nul-stream: second span')
         if (allocated(error)) return
         call check_(error, payload(b2:t2) == second, 'nul-stream: second text')
      end subroutine expect_two

      subroutine expect_fail_first(payload)
         character(len=*), intent(in) :: payload
         integer :: b1, t1, err
         if (allocated(error)) return
         call capnp_json_span(payload, 1, b1, t1, err)
         call check_(error, err /= CAPNP_OK .or. b1 == 0, 'nul-stream: leading nul rejected')
      end subroutine expect_fail_first

      subroutine expect_one(payload)
         character(len=*), intent(in) :: payload
         integer :: b1, t1, b2, t2, err
         if (allocated(error)) return
         call capnp_json_span(payload, 1, b1, t1, err)
         call check_(error, err == CAPNP_OK .and. b1 > 0, 'nul-stream: trailing nul')
         if (allocated(error)) return
         call capnp_json_span(payload, t1 + 1, b2, t2, err)
         call check_(error, err == CAPNP_OK .and. b2 == 0, 'nul-stream: nothing after trailing nul')
      end subroutine expect_one
   end subroutine run_json_nul_stream

end module test_limits
