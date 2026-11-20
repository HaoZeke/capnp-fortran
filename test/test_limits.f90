!> Depth and stream regressions that must fail on the parent of their fix.
module test_limits
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed, &
        skip_test
   use capnp
   use capnp_dynamic
   use capnp_text
   use capnp_json
   implicit none
   private
   public :: collect_limits

   character(len=*), parameter :: tmp = '/tmp/capnpf-limits'

contains

   subroutine collect_limits(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [ &
         new_unittest("json-audit-depth", run_json_audit_depth), &
         new_unittest("group-encode-depth", run_group_encode_depth), &
         new_unittest("json-flat-depth", run_json_flat_depth) &
         ]
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

end module test_limits
