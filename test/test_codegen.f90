!> Generator regressions: multi-file generics, suffixed names, annotation
!> stems, and checked-in modules that drift from the plugin.
module test_codegen
   use capnp
   use testdrive, only: unittest_type, error_type, check, test_failed, skip_test
   implicit none
   private
   public :: collect_codegen

contains

   subroutine collect_codegen(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      allocate (testsuite(15))
      testsuite(1)%name = "codegen-multifile-generics"
      testsuite(1)%test => run_multifile
      testsuite(2)%name = "codegen-suffix-clamp"
      testsuite(2)%test => run_suffix
      testsuite(3)%name = "codegen-annotation-stem"
      testsuite(3)%test => run_annot
      testsuite(4)%name = "codegen-dynamic-fresh"
      testsuite(4)%test => run_fresh
      testsuite(5)%name = "codegen-named-plugin"
      testsuite(5)%test => run_named_plugin
      testsuite(6)%name = "codegen-helper-starts"
      testsuite(6)%test => run_helper_starts
      testsuite(7)%name = "codegen-long-annotation-text"
      testsuite(7)%test => run_longann
      testsuite(8)%name = "codegen-wire-count"
      testsuite(8)%test => run_wire_count
      testsuite(9)%name = "codegen-void-const"
      testsuite(9)%test => run_voidconst
      testsuite(10)%name = "codegen-implicit-golden"
      testsuite(10)%test => run_implicit_golden
      testsuite(11)%name = "codegen-implicit-roundtrip"
      testsuite(11)%test => run_implicit_roundtrip
      testsuite(12)%name = "codegen-box-plain"
      testsuite(12)%test => run_box_plain
      testsuite(13)%name = "codegen-box-default"
      testsuite(13)%test => run_box_default
      testsuite(14)%name = "codegen-box-const"
      testsuite(14)%test => run_box_const
      testsuite(15)%name = "codegen-box-any-tag"
      testsuite(15)%test => run_box_any_tag
   end subroutine collect_codegen

   subroutine run_multifile(error)
      type(error_type), allocatable, intent(out) :: error
      call run_mode(error, 'multifile', 'codegen: two generic files')
   end subroutine run_multifile

   subroutine run_suffix(error)
      type(error_type), allocatable, intent(out) :: error
      call run_mode(error, 'suffix', 'codegen: suffixed names fit')
   end subroutine run_suffix

   subroutine run_annot(error)
      type(error_type), allocatable, intent(out) :: error
      call run_mode(error, 'annot', 'codegen: annotation stem')
   end subroutine run_annot

   subroutine run_longann(error)
      type(error_type), allocatable, intent(out) :: error
      call run_mode(error, 'longann', 'codegen: long Text annotations fit 132 columns')
   end subroutine run_longann

   subroutine run_fresh(error)
      type(error_type), allocatable, intent(out) :: error
      call run_mode(error, 'fresh', 'codegen: dynamic module is fresh')
   end subroutine run_fresh

   !> A plugin named by the caller is the one that runs, even when another
   !> profile's plugin is earlier in the build tree.
   subroutine run_named_plugin(error)
      type(error_type), allocatable, intent(out) :: error
      integer :: stat, cs
      stat = 0
      cs = 0
      call execute_command_line( &
         'mkdir -p /tmp/capnp-fortran-stale-plugin && ' // &
         'printf "%s\n" "#!/bin/sh" "exit 9" > /tmp/capnp-fortran-stale-plugin/capnpc-fortran && ' // &
         'chmod +x /tmp/capnp-fortran-stale-plugin/capnpc-fortran', &
         exitstat=stat, cmdstat=cs)
      if (cs /= 0 .or. stat /= 0) then
         call test_failed(error, 'codegen: could not plant a stale plugin')
         return
      end if
      stat = 0
      cs = 0
      call execute_command_line( &
         'CAPNP_FORTRAN_BIN=/tmp/capnp-fortran-stale-plugin/capnpc-fortran ' // &
         'CAPNP_FORTRAN_MODDIR=/tmp/capnp-fortran-stale-plugin ' // &
         'bash test/codegen_check.sh fresh', &
         exitstat=stat, cmdstat=cs)
      if (cs /= 0) then
         call test_failed(error, 'codegen: helper could not start')
         return
      end if
      call check(error, stat /= 0, 'codegen: named stale plugin is rejected')
   end subroutine run_named_plugin

   !> A helper that cannot be started is a failure. Skipping it would hide
   !> a broken generator check.
   subroutine run_helper_starts(error)
      type(error_type), allocatable, intent(out) :: error
      type(error_type), allocatable :: inner
      integer :: code
      call run_mode(inner, 'fresh', 'codegen helper could not start', &
                    shell='/no/such/capnp-fortran-shell')
      code = 0
      if (allocated(inner)) then
         code = inner%stat
         ! The finalizer aborts on any status other than success, including
         ! a failure we have already inspected.
         inner%stat = 0
      end if
      call check(error, code == 1, 'codegen: missing helper is a failure')
   end subroutine run_helper_starts

   subroutine run_mode(error, mode, what, shell)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), intent(in) :: mode, what
      character(len=*), intent(in), optional :: shell
      character(len=:), allocatable :: bin, moddir, sh, command
      integer :: stat, cs
      logical :: ok
      sh = 'bash'
      if (present(shell)) sh = shell
      call plugin_paths(bin, moddir, ok)
      if (.not. ok) then
         call test_failed(error, 'codegen: this test binary has no plugin path')
         return
      end if
      command = 'CAPNP_FORTRAN_BIN="' // bin // '" CAPNP_FORTRAN_MODDIR="' // &
                moddir // '" "' // sh // '" test/codegen_check.sh ' // mode
      if (present(shell)) command = command // ' >/dev/null 2>&1'
      stat = 0
      cs = 0
      call execute_command_line(command, exitstat=stat, cmdstat=cs)
      if (cs /= 0) then
         call test_failed(error, what)
         return
      end if
      call check(error, stat == 0, what)
   end subroutine run_mode

   !> Plugin beside this tester, and the module directory of the build
   !> that produced it. Both are required; the first match in the build
   !> tree is not.
   subroutine plugin_paths(bin, moddir, ok)
      character(len=:), allocatable, intent(out) :: bin, moddir
      logical, intent(out) :: ok
      character(len=4096) :: exe
      integer :: n, stat, i, slashes
      ok = .false.
      call get_command_argument(0, exe, length=n, status=stat)
      if (stat /= 0 .or. n < 1) return
      slashes = 0
      do i = n, 1, -1
         if (exe(i:i) /= '/') cycle
         slashes = slashes + 1
         if (slashes == 2) then
            bin = exe(1:i-1) // '/app/capnpc-fortran'
            exit
         end if
      end do
      if (.not. allocated(bin)) return
      call read_module_dir(moddir, ok)
   end subroutine plugin_paths

   subroutine read_module_dir(moddir, ok)
      character(len=:), allocatable, intent(out) :: moddir
      logical, intent(out) :: ok
      integer :: u, ios, a, b
      character(len=512) :: line
      logical :: take_next
      ok = .false.
      take_next = .false.
      open (newunit=u, file='build/compile_commands.json', status='old', &
            action='read', iostat=ios)
      if (ios /= 0) then
         call newest_module_dir(moddir, ok)
         return
      end if
      do
         read (u, '(A)', iostat=ios) line
         if (ios /= 0) exit
         if (take_next) then
            a = index(line, '"')
            b = index(line(a+1:), '"')
            if (a > 0 .and. b > 0) then
               if (.not. allocated(moddir)) then
                  moddir = line(a+1:a+b-1)
                  ok = len_trim(moddir) > 0
               else if (moddir /= line(a+1:a+b-1)) then
                  ok = .false.
                  exit
               end if
            end if
            take_next = .false.
         else if (index(line, '"-J"') > 0) then
            take_next = .true.
         end if
      end do
      close (u)
      ! A build with nothing to compile leaves no commands behind.
      if (.not. ok .and. .not. allocated(moddir)) call newest_module_dir(moddir, ok)
   end subroutine read_module_dir

   subroutine newest_module_dir(moddir, ok)
      character(len=:), allocatable, intent(out) :: moddir
      logical, intent(out) :: ok
      character(len=512) :: line
      integer :: u, ios, stat, cs
      ok = .false.
      stat = 0
      cs = 0
      call execute_command_line( &
         'find build -name capnp.mod -printf "%T@ %h\n" 2>/dev/null | ' // &
         'sort -nr | head -n 1 | cut -d" " -f2- > build/codegen-moddir.txt', &
         exitstat=stat, cmdstat=cs)
      if (cs /= 0 .or. stat /= 0) return
      open (newunit=u, file='build/codegen-moddir.txt', status='old', &
            action='read', iostat=ios)
      if (ios /= 0) return
      read (u, '(A)', iostat=ios) line
      close (u, status='delete')
      if (ios /= 0) return
      moddir = trim(line)
      ok = len(moddir) > 0
   end subroutine newest_module_dir

   !> A Void constant is a named integer parameter equal to 0.
   subroutine run_voidconst(error)
      type(error_type), allocatable, intent(out) :: error
      call run_mode(error, 'voidconst', 'codegen: void constant is a named integer')
   end subroutine run_voidconst

   subroutine run_implicit_golden(error)
      type(error_type), allocatable, intent(out) :: error
      call run_with_capnp(error, 'implicit-golden', 'codegen: implicit method golden')
   end subroutine run_implicit_golden

   subroutine run_implicit_roundtrip(error)
      type(error_type), allocatable, intent(out) :: error
      call run_with_capnp(error, 'implicit-roundtrip', 'codegen: implicit method round trip')
   end subroutine run_implicit_roundtrip

   subroutine run_box_plain(error)
      type(error_type), allocatable, intent(out) :: error
      call run_with_capnp(error, 'box-plain', 'codegen: box gains no implicit accessors')
   end subroutine run_box_plain

   subroutine run_box_default(error)
      type(error_type), allocatable, intent(out) :: error
      call run_with_capnp(error, 'box-default', 'codegen: box pointer default round trip')
   end subroutine run_box_default

   subroutine run_box_const(error)
      type(error_type), allocatable, intent(out) :: error
      call run_with_capnp(error, 'box-const', 'codegen: const default decodes')
   end subroutine run_box_const

   subroutine run_box_any_tag(error)
      type(error_type), allocatable, intent(out) :: error
      call run_with_capnp(error, 'box-any-tag', 'codegen: longer name leaves the tag default')
   end subroutine run_box_any_tag

   !> Same probe as test_gate2: exit 77 / a missing capnp skips the case.
   subroutine run_with_capnp(error, mode, what)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), intent(in) :: mode, what
      integer :: rc0, st0
      rc0 = 0
      st0 = 0
      call execute_command_line('command -v capnp > /dev/null 2>&1', &
                                exitstat=rc0, cmdstat=st0)
      if (st0 /= 0 .or. rc0 /= 0) then
         call skip_test(error, 'capnp is not on PATH')
         return
      end if
      call run_mode(error, mode, what)
   end subroutine run_with_capnp

   !> A 4096-node zero-sized table loads as in C++ 1.4.0, charged against
   !> the plugin's 2**30-word budget. Zero-sized requested files reach
   !> validation.
   subroutine run_wire_count(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root, nodes, files, elem
      integer(int8), allocatable :: bytes(:)
      character(len=:), allocatable :: bin, moddir, text
      integer :: err, stat, cs
      logical :: ok

      call plugin_paths(bin, moddir, ok)
      if (.not. ok) then
         call test_failed(error, 'codegen: this test binary has no plugin path')
         return
      end if

      call capnp_message_init_builder(msg, err)
      root = capnp_new_struct(msg, 0, 2, err)
      nodes = capnp_new_composite_list(msg, 4096_int64, 0, 0, err)
      call capnp_setp(root, 0, nodes, err)
      files = capnp_new_composite_list(msg, 1_int64, 1, 1, err)
      elem = capnp_list_get_struct(files, 0, err)
      call capnp_set_i64(elem, 0_int64, 1_int64, err)
      call capnp_set_text(elem, 0, 'x.capnp', err)
      call capnp_setp(root, 1, files, err)
      call capnp_set_root(msg, root, err)
      call capnp_serialize_bytes(msg, bytes, err)
      call capnp_write_file('/tmp/capnp-fortran-amp-nodes.bin', bytes, err)
      call capnp_message_free(msg)
      if (err /= CAPNP_OK) then
         call test_failed(error, 'codegen: could not write the node request')
         return
      end if
      call execute_command_line( &
         '"'//bin//'" /tmp/capnp-fortran-amp-nodes.bin ' // &
         '> /tmp/capnp-fortran-amp-nodes.out 2> /tmp/capnp-fortran-amp-nodes.err', &
         exitstat=stat, cmdstat=cs)
      text = slurp('/tmp/capnp-fortran-amp-nodes.out')
      call check(error, cs == 0 .and. index(text, ', err=5') == 0 .and. &
                 index(text, 'emit failed for x.capnp') > 0, &
                 'codegen: 4096 zero-size nodes load')
      if (allocated(error)) return

      call capnp_message_init_builder(msg, err)
      root = capnp_new_struct(msg, 0, 2, err)
      files = capnp_new_list(msg, CAPNP_SZ_VOID, 4096_int64, err)
      call capnp_setp(root, 1, files, err)
      call capnp_set_root(msg, root, err)
      call capnp_serialize_bytes(msg, bytes, err)
      call capnp_write_file('/tmp/capnp-fortran-amp-files.bin', bytes, err)
      call capnp_message_free(msg)
      if (err /= CAPNP_OK) then
         call test_failed(error, 'codegen: could not write the file request')
         return
      end if
      call execute_command_line( &
         '"'//bin//'" /tmp/capnp-fortran-amp-files.bin ' // &
         '> /tmp/capnp-fortran-amp-files.out 2> /tmp/capnp-fortran-amp-files.err', &
         exitstat=stat, cmdstat=cs)
      text = slurp('/tmp/capnp-fortran-amp-files.out')
      call check(error, cs == 0 .and. stat /= 0 .and. &
                 index(text, 'requestedFiles exceeds') == 0, &
                 'codegen: requested files reach list validation')
   end subroutine run_wire_count

   function slurp(path) result(s)
      character(len=*), intent(in) :: path
      character(len=:), allocatable :: s
      character(len=512) :: line
      integer :: u, ios
      s = ''
      open (newunit=u, file=path, status='old', action='read', iostat=ios)
      if (ios /= 0) return
      do
         read (u, '(A)', iostat=ios) line
         if (ios /= 0) exit
         s = s//trim(line)
      end do
      close (u)
   end function slurp

end module test_codegen
