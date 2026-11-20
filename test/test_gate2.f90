program test_gate2
   use capnp
   implicit none
   call case_0()
   call case_1()
   call case_2()
   call case_3()
   call case_4()
   call case_5()
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

   subroutine case_2()
      call gate( &
         'ppid=$(awk ' // &
         achar(39) // &
         '/PPid/ {print $2}' // &
         achar(39) // &
         ' /proc/$PPID/status); exe=$(readlink -f /proc/$ppid/exe); CA' // &
         'P=$(dirname $(dirname "$exe"))/app/capnpf; SRC=${CAPNP_SRC:-' // &
         '/tmp/capnp-peer/capnproto-c++-1.4.0/src}; ' // &
         achar(10) // &
         'cat > /tmp/g2-dv.capnp <<' // &
         achar(39) // &
         'EOF' // &
         achar(39) // &
         achar(10) // &
         '@0xd1e2f3a4b5c6d7e8;' // &
         achar(10) // &
         'struct Inner { n @0 :Int32 = 7; }' // &
         achar(10) // &
         'struct U {' // &
         achar(10) // &
         '  u :union {' // &
         achar(10) // &
         '    c @0 :Void;' // &
         achar(10) // &
         '    a @1 :Text = "ua";' // &
         achar(10) // &
         '    b @2 :Data = 0x"cafe";' // &
         achar(10) // &
         '    l @3 :List(Int32) = [4, 5];' // &
         achar(10) // &
         '    s @4 :Inner = (n = 9);' // &
         achar(10) // &
         '  }' // &
         achar(10) // &
         '}' // &
         achar(10) // &
         'EOF' // &
         achar(10) // &
         'for d in 1 2 3 4; do' // &
         achar(10) // &
         'python3 -c "import sys; sys.stdout.buffer.write(bytes([0,0,0,' // &
         '0,3,0,0,0,0,0,0,0,1,0,1,0,$d]) + bytes(15))" > /tmp/g2-dv.in' // &
         achar(10) // &
         '"$CAP" convert binary:text /tmp/g2-dv.capnp U < /tmp/g2-dv.in' // &
         ' > /tmp/g2-dv.o; echo $? >> /tmp/g2-dv.o' // &
         achar(10) // &
         'capnp convert binary:text /tmp/g2-dv.capnp U < /tmp/g2-dv.in ' // &
         '> /tmp/g2-dv.r; echo $? >> /tmp/g2-dv.r' // &
         achar(10) // &
         'cmp -s /tmp/g2-dv.o /tmp/g2-dv.r || exit 1' // &
         achar(10) // &
         'done')
   end subroutine case_2

   subroutine case_3()
      call gate( &
         'ppid=$(awk ' // &
         achar(39) // &
         '/PPid/ {print $2}' // &
         achar(39) // &
         ' /proc/$PPID/status); exe=$(readlink -f /proc/$ppid/exe); CA' // &
         'P=$(dirname $(dirname "$exe"))/app/capnpf; SRC=${CAPNP_SRC:-' // &
         '/tmp/capnp-peer/capnproto-c++-1.4.0/src}; ' // &
         achar(10) // &
         'printf ' // &
         achar(39) // &
         '(textBox = (value = "hi", label = "l"), listBox = (value = [' // &
         '"a", "b"]), nest = (inner = (value = "n"), boxes = [(value =' // &
         ' "m")]))\n' // &
         achar(39) // &
         ' > /tmp/g2-bt.in' // &
         achar(10) // &
         'for t in text binary; do' // &
         achar(10) // &
         '  "$CAP" convert text:$t schema/box.capnp BoxUse < /tmp/g2-b' // &
         't.in > /tmp/g2-bt.o; echo $? >> /tmp/g2-bt.o' // &
         achar(10) // &
         '  capnp convert text:$t schema/box.capnp BoxUse < /tmp/g2-bt' // &
         '.in > /tmp/g2-bt.r; echo $? >> /tmp/g2-bt.r' // &
         achar(10) // &
         '  cmp -s /tmp/g2-bt.o /tmp/g2-bt.r || exit 1' // &
         achar(10) // &
         'done' // &
         achar(10) // &
         'capnp convert text:binary schema/box.capnp BoxUse < /tmp/g2-' // &
         'bt.in > /tmp/g2-bt.bin' // &
         achar(10) // &
         '"$CAP" convert binary:text schema/box.capnp BoxUse < /tmp/g2' // &
         '-bt.bin > /tmp/g2-bt.o; echo $? >> /tmp/g2-bt.o' // &
         achar(10) // &
         'capnp convert binary:text schema/box.capnp BoxUse < /tmp/g2-' // &
         'bt.bin > /tmp/g2-bt.r; echo $? >> /tmp/g2-bt.r' // &
         achar(10) // &
         'cmp -s /tmp/g2-bt.o /tmp/g2-bt.r')
   end subroutine case_3

   subroutine case_4()
      call gate( &
         'ppid=$(awk ' // &
         achar(39) // &
         '/PPid/ {print $2}' // &
         achar(39) // &
         ' /proc/$PPID/status); exe=$(readlink -f /proc/$ppid/exe); CA' // &
         'P=$(dirname $(dirname "$exe"))/app/capnpf; SRC=${CAPNP_SRC:-' // &
         '/tmp/capnp-peer/capnproto-c++-1.4.0/src}; printf ' // &
         achar(39) // &
         '{"big": 1.6}\n' // &
         achar(39) // &
         ' > /tmp/g2-fr.in; "$CAP" convert json:json schema/codec.capn' // &
         'p Sample < /tmp/g2-fr.in > /tmp/g2-fr.o; test $? -ne 0')
   end subroutine case_4

   subroutine case_5()
      call gate( &
         'ppid=$(awk ' // &
         achar(39) // &
         '/PPid/ {print $2}' // &
         achar(39) // &
         ' /proc/$PPID/status); exe=$(readlink -f /proc/$ppid/exe); CA' // &
         'P=$(dirname $(dirname "$exe"))/app/capnpf; SRC=${CAPNP_SRC:-' // &
         '/tmp/capnp-peer/capnproto-c++-1.4.0/src}; printf ' // &
         achar(39) // &
         '{"b64": null}\n' // &
         achar(39) // &
         ' > /tmp/g2-nd.in; "$CAP" convert json:json schema/codec.capn' // &
         'p Sample < /tmp/g2-nd.in > /tmp/g2-nd.o; test $? -eq 0')
   end subroutine case_5
end program test_gate2
