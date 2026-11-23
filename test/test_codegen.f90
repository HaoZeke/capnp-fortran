!> Generator regressions: multi-file generics, suffixed names, annotation
!> stems, and checked-in modules that drift from the plugin.
module test_codegen
   use testdrive, only: unittest_type, error_type, check, test_failed
   implicit none
   private
   public :: collect_codegen

contains

   subroutine collect_codegen(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      allocate (testsuite(7))
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

end module test_codegen
