program test_gate2
   use capnp
   implicit none
   call case_0()
   call case_1()
   call case_2()
   call case_3()
   call case_4()
   call case_5()
   call case_6()
   call case_7()
   call case_8()
   call case_9()
   call case_10()
   call case_11()
   call case_12()
   call case_13()
   call case_14()
   call case_15()
   call case_16()
   call case_17()
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

   subroutine case_6()
      call gate( &
         'ppid=$(awk ' // &
         achar(39) // &
         '/PPid/ {print $2}' // &
         achar(39) // &
         ' /proc/$PPID/status); exe=$(readlink -f /proc/$ppid/exe); CA' // &
         'P=$(dirname $(dirname "$exe"))/app/capnpf; SRC=${CAPNP_SRC:-' // &
         '/tmp/capnp-peer/capnproto-c++-1.4.0/src}; printf ' // &
         achar(39) // &
         '{"hue": "azure"}\n' // &
         achar(39) // &
         ' > /tmp/g2-az.in; "$CAP" convert json:json schema/codec.capn' // &
         'p Sample < /tmp/g2-az.in > /tmp/g2-az.o; echo $? > /tmp/g2-a' // &
         'z.e1; capnp convert json:json schema/codec.capnp Sample < /t' // &
         'mp/g2-az.in > /tmp/g2-az.r; echo $? > /tmp/g2-az.e2; cmp -s ' // &
         '/tmp/g2-az.o /tmp/g2-az.r && cmp -s /tmp/g2-az.e1 /tmp/g2-az' // &
         '.e2')
   end subroutine case_6

   subroutine case_7()
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
         '{"textBox": {"value": "hi"}, "listBox": {"value": ["a", "b"]' // &
         '}, "nest": {"inner": {"value": "n"}, "boxes": [{"value": "m"' // &
         '}]}}\n' // &
         achar(39) // &
         ' > /tmp/g2-bj.in' // &
         achar(10) // &
         'for t in text binary; do' // &
         achar(10) // &
         '  "$CAP" convert json:$t schema/box.capnp BoxUse < /tmp/g2-b' // &
         'j.in > /tmp/g2-bj.o; echo $? >> /tmp/g2-bj.o' // &
         achar(10) // &
         '  capnp convert json:$t schema/box.capnp BoxUse < /tmp/g2-bj' // &
         '.in > /tmp/g2-bj.r; echo $? >> /tmp/g2-bj.r' // &
         achar(10) // &
         '  cmp -s /tmp/g2-bj.o /tmp/g2-bj.r || exit 1' // &
         achar(10) // &
         'done')
   end subroutine case_7

   subroutine case_8()
      call gate( &
         'ppid=$(awk ' // &
         achar(39) // &
         '/PPid/ {print $2}' // &
         achar(39) // &
         ' /proc/$PPID/status); exe=$(readlink -f /proc/$ppid/exe); CA' // &
         'P=$(dirname $(dirname "$exe"))/app/capnpf; SRC=${CAPNP_SRC:-' // &
         '/tmp/capnp-peer/capnproto-c++-1.4.0/src}; python3 -c ' // &
         achar(39) // &
         'print(chr(123)+chr(34)+"title"+chr(34)+": "+chr(34)+("ab"*20' // &
         '0)+chr(34)+chr(125))' // &
         achar(39) // &
         ' > /tmp/g2-qs.in; "$CAP" convert json:json schema/codec.capn' // &
         'p Sample < /tmp/g2-qs.in > /tmp/g2-qs.o; echo $? > /tmp/g2-q' // &
         's.e1; capnp convert json:json schema/codec.capnp Sample < /t' // &
         'mp/g2-qs.in > /tmp/g2-qs.r; echo $? > /tmp/g2-qs.e2; cmp -s ' // &
         '/tmp/g2-qs.o /tmp/g2-qs.r && cmp -s /tmp/g2-qs.e1 /tmp/g2-qs' // &
         '.e2')
   end subroutine case_8

   subroutine case_9()
      call gate( &
         'ppid=$(awk ' // &
         achar(39) // &
         '/PPid/ {print $2}' // &
         achar(39) // &
         ' /proc/$PPID/status); exe=$(readlink -f /proc/$ppid/exe); CA' // &
         'P=$(dirname $(dirname "$exe"))/app/capnpf; SRC=${CAPNP_SRC:-' // &
         '/tmp/capnp-peer/capnproto-c++-1.4.0/src}; printf ' // &
         achar(39) // &
         '{"b64": "Y Q = ="}\n' // &
         achar(39) // &
         ' > /tmp/g2-b64.in; "$CAP" convert json:json schema/codec.cap' // &
         'np Sample < /tmp/g2-b64.in > /tmp/g2-b64.o; echo $? > /tmp/g' // &
         '2-b64.e1; capnp convert json:json schema/codec.capnp Sample ' // &
         '< /tmp/g2-b64.in > /tmp/g2-b64.r; echo $? > /tmp/g2-b64.e2; ' // &
         'cmp -s /tmp/g2-b64.o /tmp/g2-b64.r && cmp -s /tmp/g2-b64.e1 ' // &
         '/tmp/g2-b64.e2')
   end subroutine case_9

   subroutine case_10()
      call gate( &
         'ppid=$(awk ' // &
         achar(39) // &
         '/PPid/ {print $2}' // &
         achar(39) // &
         ' /proc/$PPID/status); exe=$(readlink -f /proc/$ppid/exe); CA' // &
         'P=$(dirname $(dirname "$exe"))/app/capnpf; SRC=${CAPNP_SRC:-' // &
         '/tmp/capnp-peer/capnproto-c++-1.4.0/src}; printf ' // &
         achar(39) // &
         '{"label": "a"}\n' // &
         achar(39) // &
         ' > /tmp/g2-job.in; "$CAP" convert json:json schema/codec.cap' // &
         'np Sample < /tmp/g2-job.in > /tmp/g2-job.o; echo $? > /tmp/g' // &
         '2-job.e1; capnp convert json:json schema/codec.capnp Sample ' // &
         '< /tmp/g2-job.in > /tmp/g2-job.r; echo $? > /tmp/g2-job.e2; ' // &
         'cmp -s /tmp/g2-job.o /tmp/g2-job.r && cmp -s /tmp/g2-job.e1 ' // &
         '/tmp/g2-job.e2')
   end subroutine case_10

   subroutine case_11()
      call gate( &
         'ppid=$(awk ' // &
         achar(39) // &
         '/PPid/ {print $2}' // &
         achar(39) // &
         ' /proc/$PPID/status); exe=$(readlink -f /proc/$ppid/exe); CA' // &
         'P=$(dirname $(dirname "$exe"))/app/capnpf; SRC=${CAPNP_SRC:-' // &
         '/tmp/capnp-peer/capnproto-c++-1.4.0/src}; "$CAP" --not-a-fla' // &
         'g >/tmp/g2-err.o 2>/tmp/g2-err.e; echo $? > /tmp/g2-err.e1; ' // &
         'test ! -s /tmp/g2-err.o && test -s /tmp/g2-err.e && test "$(' // &
         'cat /tmp/g2-err.e1)" != 0')
   end subroutine case_11

   subroutine case_12()
      call gate( &
         'ppid=$(awk ' // &
         achar(39) // &
         '/PPid/ {print $2}' // &
         achar(39) // &
         ' /proc/$PPID/status); exe=$(readlink -f /proc/$ppid/exe); CA' // &
         'P=$(dirname $(dirname "$exe"))/app/capnpf; SRC=${CAPNP_SRC:-' // &
         '/tmp/capnp-peer/capnproto-c++-1.4.0/src}; printf ' // &
         achar(39) // &
         '{"big": 1}\n{"big": 2}\n' // &
         achar(39) // &
         ' > /tmp/g2-two.in; "$CAP" convert json:json schema/codec.cap' // &
         'np Sample < /tmp/g2-two.in > /tmp/g2-two.o; echo $? > /tmp/g' // &
         '2-two.e1; capnp convert json:json schema/codec.capnp Sample ' // &
         '< /tmp/g2-two.in > /tmp/g2-two.r; echo $? > /tmp/g2-two.e2; ' // &
         'cmp -s /tmp/g2-two.o /tmp/g2-two.r && cmp -s /tmp/g2-two.e1 ' // &
         '/tmp/g2-two.e2 && printf ' // &
         achar(39) // &
         '\020\015P\005\007\000\001\020x\000^' // &
         achar(39) // &
         ' > /tmp/g2-pk.in && { capnp convert packed:packed schema/code' // &
         'c.capnp Sample < /tmp/g2-pk.in > /dev/null 2>&1; test $? -ne ' // &
         '0; } && { "$CAP" convert packed:packed schema/codec.capnp Sam' // &
         'ple < /tmp/g2-pk.in > /dev/null 2>&1; test $? -ne 0; }')
   end subroutine case_12

   subroutine case_13()
      call gate( &
         'ppid=$(awk ' // &
         achar(39) // &
         '/PPid/ {print $2}' // &
         achar(39) // &
         ' /proc/$PPID/status); exe=$(readlink -f /proc/$ppid/exe); CA' // &
         'P=$(dirname $(dirname "$exe"))/app/capnpf; SRC=${CAPNP_SRC:-' // &
         '/tmp/capnp-peer/capnproto-c++-1.4.0/src}; printf ' // &
         achar(39) // &
         '(x = 1)\n' // &
         achar(39) // &
         ' | capnp convert text:binary schema/common.capnp Vec3 > /tmp' // &
         '/g2-fp.in; "$CAP" convert binary:flat-packed schema/common.c' // &
         'apnp Vec3 < /tmp/g2-fp.in > /tmp/g2-fp.o; echo $? > /tmp/g2-' // &
         'fp.e1; capnp convert binary:flat-packed schema/common.capnp ' // &
         'Vec3 < /tmp/g2-fp.in > /tmp/g2-fp.r; echo $? > /tmp/g2-fp.e2' // &
         '; cmp -s /tmp/g2-fp.o /tmp/g2-fp.r && cmp -s /tmp/g2-fp.e1 /' // &
         'tmp/g2-fp.e2')
   end subroutine case_13

   subroutine case_14()
      call gate( &
         'ppid=$(awk ' // &
         achar(39) // &
         '/PPid/ {print $2}' // &
         achar(39) // &
         ' /proc/$PPID/status); exe=$(readlink -f /proc/$ppid/exe); CA' // &
         'P=$(dirname $(dirname "$exe"))/app/capnpf; SRC=${CAPNP_SRC:-' // &
         '/tmp/capnp-peer/capnproto-c++-1.4.0/src}; ' // &
         achar(10) // &
         'for v in ' // &
         achar(39) // &
         '{"big": 3.0}' // &
         achar(39) // &
         ' ' // &
         achar(39) // &
         '{"big": 3.5}' // &
         achar(39) // &
         ' ' // &
         achar(39) // &
         '{"big": -9223372036854775808.0}' // &
         achar(39) // &
         ' ' // &
         achar(39) // &
         '{"ubig": 1e3}' // &
         achar(39) // &
         ' ' // &
         achar(39) // &
         '{"ubig": -0.0}' // &
         achar(39) // &
         ' ' // &
         achar(39) // &
         '{"ubig": 0.5}' // &
         achar(39) // &
         ' ' // &
         achar(39) // &
         '{"wide": -0.0}' // &
         achar(39) // &
         ' ' // &
         achar(39) // &
         '{"tiny": -0.0}' // &
         achar(39) // &
         '; do' // &
         achar(10) // &
         '  printf ' // &
         achar(39) // &
         '%s\n' // &
         achar(39) // &
         ' "$v" > /tmp/g2-rc.in' // &
         achar(10) // &
         '  "$CAP" convert json:json schema/codec.capnp Sample < /tmp/' // &
         'g2-rc.in > /tmp/g2-rc.o 2> /dev/null; r1=$?' // &
         achar(10) // &
         '  capnp convert json:json schema/codec.capnp Sample < /tmp/g' // &
         '2-rc.in > /tmp/g2-rc.r 2> /dev/null; r2=$?' // &
         achar(10) // &
         '  test $r1 = $r2 || exit 1' // &
         achar(10) // &
         '  test $r2 != 0 || cmp -s /tmp/g2-rc.o /tmp/g2-rc.r || exit ' // &
         '1' // &
         achar(10) // &
         'done')
   end subroutine case_14

   subroutine case_15()
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
         ' > /tmp/g2-mat.in; "$CAP" convert json:json schema/codec.cap' // &
         'np Sample < /tmp/g2-mat.in > /tmp/g2-mat.o; echo $? > /tmp/g' // &
         '2-mat.e1; capnp convert json:json schema/codec.capnp Sample ' // &
         '< /tmp/g2-mat.in > /tmp/g2-mat.r; echo $? > /tmp/g2-mat.e2; ' // &
         'cmp -s /tmp/g2-mat.o /tmp/g2-mat.r && cmp -s /tmp/g2-mat.e1 ' // &
         '/tmp/g2-mat.e2')
   end subroutine case_15

   subroutine case_16()
      call gate( &
         'ppid=$(awk ' // &
         achar(39) // &
         '/PPid/ {print $2}' // &
         achar(39) // &
         ' /proc/$PPID/status); exe=$(readlink -f /proc/$ppid/exe); CA' // &
         'P=$(dirname $(dirname "$exe"))/app/capnpf; SRC=${CAPNP_SRC:-' // &
         '/tmp/capnp-peer/capnproto-c++-1.4.0/src}; printf ' // &
         achar(39) // &
         '(title = "abcdefghij")\n' // &
         achar(39) // &
         ' > /tmp/g2-tg.in; "$CAP" convert text:text schema/codec.capn' // &
         'p Sample < /tmp/g2-tg.in > /tmp/g2-tg.o; echo $? > /tmp/g2-t' // &
         'g.e1; capnp convert text:text schema/codec.capnp Sample < /t' // &
         'mp/g2-tg.in > /tmp/g2-tg.r; echo $? > /tmp/g2-tg.e2; cmp -s ' // &
         '/tmp/g2-tg.o /tmp/g2-tg.r && cmp -s /tmp/g2-tg.e1 /tmp/g2-tg' // &
         '.e2')
   end subroutine case_16

   subroutine case_17()
      call gate( &
         'ppid=$(awk ' // &
         achar(39) // &
         '/PPid/ {print $2}' // &
         achar(39) // &
         ' /proc/$PPID/status); exe=$(readlink -f /proc/$ppid/exe); CA' // &
         'P=$(dirname $(dirname "$exe"))/app/capnpf; SRC=${CAPNP_SRC:-' // &
         '/tmp/capnp-peer/capnproto-c++-1.4.0/src}; printf ' // &
         achar(39) // &
         '{"title": "abcdefghij"}\n' // &
         achar(39) // &
         ' > /tmp/g2-jg.in; "$CAP" convert json:json schema/codec.capn' // &
         'p Sample < /tmp/g2-jg.in > /tmp/g2-jg.o; echo $? > /tmp/g2-j' // &
         'g.e1; capnp convert json:json schema/codec.capnp Sample < /t' // &
         'mp/g2-jg.in > /tmp/g2-jg.r; echo $? > /tmp/g2-jg.e2; cmp -s ' // &
         '/tmp/g2-jg.o /tmp/g2-jg.r && cmp -s /tmp/g2-jg.e1 /tmp/g2-jg' // &
         '.e2')
   end subroutine case_17
end program test_gate2
