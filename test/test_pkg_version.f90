!> Fail when `capnpf --version` disagrees with the fpm.toml package version.
program test_pkg_version
   use cli_path, only: capnpf_setup, publish_cli_paths
   implicit none
   integer :: st, cs
   character(len=:), allocatable :: cmd
   call publish_cli_paths()
   cmd = 'root=${CAPNP_FORTRAN_ROOT:-.}; ' &
      //'pkg=$(sed -n ''s/^version *= *"\([^"]*\)".*/\1/p'' ' &
      //'"$root/fpm.toml" | head -n 1); ' &
      //'test -n "$pkg" && '//capnpf_setup()//' && ' &
      //'out=$("$CAP" --version) && ' &
      //'test "$out" = "Cap''n Proto (capnpf) version $pkg"'
   st = 0
   cs = 0
   call execute_command_line(cmd, exitstat=st, cmdstat=cs, wait=.true.)
   if (cs /= 0 .or. st /= 0) error stop 1
end program test_pkg_version
