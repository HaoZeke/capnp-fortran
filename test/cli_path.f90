!> Locate capnpf and capnpc-fortran for the profile that is running.
!> The test runner publishes absolute paths. A shell that does not see
!> them uses the executable next to this process, which is the profile
!> that built the test, including a release or sanitized build.
module cli_path
   use iso_c_binding, only: c_char, c_int, c_ptr, c_size_t, c_null_char, c_associated
   use testdrive, only: error_type, test_failed, skip_test
   implicit none
   private
   public :: capnpf_setup, capnpc_setup, publish_cli_paths, have_capnp, schema_skip

   interface
      function c_setenv(name, value, overwrite) bind(c, name='setenv') result(rc)
         import c_char, c_int
         character(kind=c_char), intent(in) :: name(*), value(*)
         integer(c_int), value :: overwrite
         integer(c_int) :: rc
      end function c_setenv
      function c_getcwd(buf, buflen) bind(c, name='getcwd') result(p)
         import c_char, c_ptr, c_size_t
         character(kind=c_char), intent(out) :: buf(*)
         integer(c_size_t), value :: buflen
         type(c_ptr) :: p
      end function c_getcwd
   end interface

contains

   function capnpf_setup() result(s)
      character(len=:), allocatable :: s
      s = tool_setup('CAPNPF', 'capnpf')
   end function capnpf_setup

   function capnpc_setup() result(s)
      character(len=:), allocatable :: s
      s = tool_setup('CAPNPC', 'capnpc-fortran')
   end function capnpc_setup

   !> Shell fragment. Sets CAP to an absolute executable path.
   !> The published variable wins. Otherwise CAP is the sibling of this
   !> process under app/, so the lookup does not read fpm's build log and
   !> does not switch to another profile.
   function tool_setup(env_name, target) result(s)
      character(len=*), intent(in) :: env_name, target
      character(len=:), allocatable :: s
      s = 'if [ -n "${'//env_name//'-}" ] && [ -x "$'//env_name//'" ]; then ' &
          //'CAP=$'//env_name//'; else CAP='//shell_quote(active_exe(target))//'; fi; ' &
          //'test -n "$CAP" && test -x "$CAP"'
   end function tool_setup

   function active_exe(target) result(path)
      character(len=*), intent(in) :: target
      character(len=:), allocatable :: path, dir
      dir = app_dir(this_exe())
      if (len(dir) == 0) then
         path = ''
      else
         path = absolute(dir//'/'//target)
      end if
   end function active_exe

   function shell_quote(path) result(q)
      character(len=*), intent(in) :: path
      character(len=:), allocatable :: q
      integer :: i
      q = "'"
      do i = 1, len(path)
         if (path(i:i) == "'") then
            q = q//"'\''"
         else
            q = q//path(i:i)
         end if
      end do
      q = q//"'"
   end function shell_quote

   !> Publish this profile's executables for child shells.
   subroutine publish_cli_paths()
      character(len=:), allocatable :: exe, dir
      exe = this_exe()
      dir = app_dir(exe)
      if (len(dir) == 0) return
      if (is_file(dir//'/capnpf')) call put_env('CAPNPF', absolute(dir//'/capnpf'))
      if (is_file(dir//'/capnpc-fortran')) &
         call put_env('CAPNPC', absolute(dir//'/capnpc-fortran'))
   end subroutine publish_cli_paths

   function this_exe() result(exe)
      character(len=:), allocatable :: exe
      character(len=4096) :: buf
      integer :: n, stat
      call get_command_argument(0, buf, length=n, status=stat)
      if (stat /= 0 .or. n <= 0) then
         exe = ''
      else
         exe = buf(1:n)
      end if
   end function this_exe

   function app_dir(exe) result(dir)
      character(len=*), intent(in) :: exe
      character(len=:), allocatable :: dir, parent
      parent = dirname(dirname(exe))
      if (len(parent) == 0) then
         dir = ''
      else
         dir = parent//'/app'
      end if
   end function app_dir

   function dirname(path) result(dir)
      character(len=*), intent(in) :: path
      character(len=:), allocatable :: dir
      integer :: i
      i = index(path, '/', back=.true.)
      if (i <= 1) then
         dir = ''
      else
         dir = path(1:i - 1)
      end if
   end function dirname

   function absolute(path) result(out)
      character(len=*), intent(in) :: path
      character(len=:), allocatable :: out, cwd
      if (len(path) > 0) then
         if (path(1:1) == '/') then
            out = path
            return
         end if
      end if
      cwd = get_cwd()
      if (len(cwd) == 0) then
         out = path
      else
         out = cwd//'/'//path
      end if
   end function absolute

   function get_cwd() result(cwd)
      character(len=:), allocatable :: cwd
      character(kind=c_char) :: buf(4096)
      integer :: i
      if (.not. c_associated(c_getcwd(buf, 4096_c_size_t))) then
         cwd = ''
         return
      end if
      cwd = ''
      do i = 1, size(buf)
         if (buf(i) == c_null_char) exit
         cwd = cwd//buf(i)
      end do
   end function get_cwd

   subroutine put_env(name, value)
      character(len=*), intent(in) :: name, value
      character(kind=c_char) :: n(len(name) + 1), v(len(value) + 1)
      integer :: i
      do i = 1, len(name)
         n(i) = name(i:i)
      end do
      n(len(name) + 1) = c_null_char
      do i = 1, len(value)
         v(i) = value(i:i)
      end do
      v(len(value) + 1) = c_null_char
      if (c_setenv(n, v, 1_c_int) /= 0) return
   end subroutine put_env

   function is_file(path) result(yes)
      character(len=*), intent(in) :: path
      logical :: yes
      inquire (file=path, exist=yes)
   end function is_file

   !> True when capnp and the three dev schemas are installed.
   !> CAPNP_FORTRAN_ROOT, when set, is the repo root. With it unset the
   !> script is test/dev_schemas.sh in the working directory.
   function have_capnp() result(y)
      logical :: y
      character(len=:), allocatable :: root, script
      integer :: n, st, cs
      call get_environment_variable('CAPNP_FORTRAN_ROOT', length=n)
      if (n > 0) then
         allocate (character(len=n) :: root)
         call get_environment_variable('CAPNP_FORTRAN_ROOT', root)
         root = trim(root)
      else
         root = ''
      end if
      if (len(root) > 0) then
         script = root//'/test/dev_schemas.sh'
      else
         script = 'test/dev_schemas.sh'
      end if
      st = 0
      cs = 0
      call execute_command_line('bash '//shell_quote(script), exitstat=st, cmdstat=cs, &
         wait=.true.)
      y = cs == 0 .and. st == 0
   end function have_capnp

   subroutine schema_skip(error)
      type(error_type), allocatable, intent(inout) :: error
      integer :: n
      call get_environment_variable('CI', length=n)
      if (n > 0) then
         call test_failed(error, 'capnp standard schemas are not available')
      else
         call skip_test(error, 'capnp standard schemas are not available')
      end if
   end subroutine schema_skip

end module cli_path
