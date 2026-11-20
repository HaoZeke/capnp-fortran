program test_gate2
   use capnp
   implicit none
   call case_0()
   call case_1()
contains
   subroutine gate(cmd)
      character(len=*), intent(in) :: cmd
      integer :: u, rc, st
      open (newunit=u, file='/tmp/g2-cmd.sh', status='replace')
      write (u, '(a)') trim(cmd)
      close (u)
      call execute_command_line('bash /tmp/g2-cmd.sh', exitstat=rc, cmdstat=st)
      if (st /= 0 .or. rc /= 0) error stop 1
   end subroutine gate

   subroutine case_0()
      call gate( &
         'ppid=$(awk ' // &
         achar(39) // &
         '/PPid/ {print $2}' // &
         achar(39) // &
         ' /proc/$PPID/status); exe=$(readlink -f /proc/$ppid/exe); CA' // &
         'P=$(dirname $(dirname "$exe"))/app/capnpf; SRC=${CAPNP_SRC:-' // &
         '/tmp/capnp-peer/capnproto-c++-1.4.0/src}; printf ' // &
         achar(39) // &
         '(big = 010)\n' // &
         achar(39) // &
         ' > /tmp/g2-oct.in; "$CAP" convert text:text schema/codec.cap' // &
         'np Sample < /tmp/g2-oct.in > /tmp/g2-oct.o; echo $? > /tmp/g' // &
         '2-oct.e1; capnp convert text:text schema/codec.capnp Sample ' // &
         '< /tmp/g2-oct.in > /tmp/g2-oct.r; echo $? > /tmp/g2-oct.e2; ' // &
         'cmp -s /tmp/g2-oct.o /tmp/g2-oct.r && cmp -s /tmp/g2-oct.e1 ' // &
         '/tmp/g2-oct.e2')
   end subroutine case_0

   subroutine case_1()
      call gate( &
         'ppid=$(awk ' // &
         achar(39) // &
         '/PPid/ {print $2}' // &
         achar(39) // &
         ' /proc/$PPID/status); exe=$(readlink -f /proc/$ppid/exe); CA' // &
         'P=$(dirname $(dirname "$exe"))/app/capnpf; SRC=${CAPNP_SRC:-' // &
         '/tmp/capnp-peer/capnproto-c++-1.4.0/src}; python3 -c "open(' // &
         achar(39) // &
         '/tmp/g2-nl.in' // &
         achar(39) // &
         ',' // &
         achar(39) // &
         'w' // &
         achar(39) // &
         ').write(' // &
         achar(39) // &
         '(title = \"a' // &
         achar(39) // &
         '+chr(10)+' // &
         achar(39) // &
         'b\")\n' // &
         achar(39) // &
         ')"; "$CAP" convert text:text schema/codec.capnp Sample < /tm' // &
         'p/g2-nl.in > /tmp/g2-nl.o; test $? -ne 0')
   end subroutine case_1
end program test_gate2
