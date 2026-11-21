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
   call case_18()
   call case_19()
   call case_20()
   call case_21()
   call case_22()
   call case_24()
   call case_25()
   call case_26()
   call case_27()
   call case_28()
   call case_29()
   call case_30()
   call case_31()
   call case_wide()
   call case_33()
   call case_34()
   call case_35()
   call case_36()
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

   subroutine case_18()
      call gate( &
         'ppid=$(awk ' // &
         achar(39) // &
         '/PPid/ {print $2}' // &
         achar(39) // &
         ' /proc/$PPID/status); exe=$(readlink -f /proc/$ppid/exe); CA' // &
         'P=$(dirname $(dirname "$exe"))/app/capnpf; SRC=${CAPNP_SRC:-' // &
         '/tmp/capnp-peer/capnproto-c++-1.4.0/src}; ' // &
         achar(10) // &
         'd=/tmp/g2-plug-$$; mkdir -p "$d"; cp test/fixtures/box.cgr.b' // &
         'in "$d/req.bin"' // &
         achar(10) // &
         'P=$(dirname "$CAP")/capnpc-fortran' // &
         achar(10) // &
         '(cd "$d" && "$P" req.bin) > /dev/null 2>&1 && test -s "$d/sc' // &
         'hema/box_capnp.f90" || { rm -rf "$d"; exit 1; }' // &
         achar(10) // &
         'rm -f "$d/schema/box_capnp.f90"' // &
         achar(10) // &
         'long="req.bin$(printf ' // &
         achar(39) // &
         '%4200s' // &
         achar(39) // &
         ' ' // &
         achar(39) // &
         achar(39) // &
         ')x"' // &
         achar(10) // &
         '(cd "$d" && "$P" "$long") > /dev/null 2>&1; r=$?' // &
         achar(10) // &
         'test $r -ne 0 && test ! -e "$d/schema/box_capnp.f90"; r=$?' // &
         achar(10) // &
         'rm -rf "$d"; exit $r')
   end subroutine case_18

   subroutine case_19()
      call gate( &
         'ppid=$(awk ' // &
         achar(39) // &
         '/PPid/ {print $2}' // &
         achar(39) // &
         ' /proc/$PPID/status); exe=$(readlink -f /proc/$ppid/exe); CA' // &
         'P=$(dirname $(dirname "$exe"))/app/capnpf; SRC=${CAPNP_SRC:-' // &
         '/tmp/capnp-peer/capnproto-c++-1.4.0/src}; ' // &
         achar(10) // &
         'for p in Vec3 Vec3. Status ' // &
         achar(39) // &
         achar(39) // &
         ' . x; do' // &
         achar(10) // &
         '  "$CAP" eval schema/common.capnp "$p" > /dev/null 2>&1; r1=' // &
         '$?' // &
         achar(10) // &
         '  capnp eval schema/common.capnp "$p" > /dev/null 2>&1; r2=$' // &
         '?' // &
         achar(10) // &
         '  test $r1 -lt 128 || exit 1' // &
         achar(10) // &
         '  test $((r1 != 0)) -eq $((r2 != 0)) || exit 1' // &
         achar(10) // &
         'done')
   end subroutine case_19

   subroutine case_20()
      call gate( &
         'ppid=$(awk ' // &
         achar(39) // &
         '/PPid/ {print $2}' // &
         achar(39) // &
         ' /proc/$PPID/status); exe=$(readlink -f /proc/$ppid/exe); CA' // &
         'P=$(dirname $(dirname "$exe"))/app/capnpf; SRC=${CAPNP_SRC:-' // &
         '/tmp/capnp-peer/capnproto-c++-1.4.0/src}; printf ' // &
         achar(39) // &
         'abc' // &
         achar(39) // &
         ' > /tmp/g2-ph.in; "$CAP" convert packed:binary schema/common' // &
         '.capnp Vec3 < /tmp/g2-ph.in > /tmp/g2-ph.o; echo $? > /tmp/g' // &
         '2-ph.e1; capnp convert packed:binary schema/common.capnp Vec' // &
         '3 < /tmp/g2-ph.in > /tmp/g2-ph.r; echo $? > /tmp/g2-ph.e2; c' // &
         'mp -s /tmp/g2-ph.e1 /tmp/g2-ph.e2 && test "$(cat /tmp/g2-ph.' // &
         'e1)" != 0')
   end subroutine case_20

   subroutine case_21()
      call gate( &
         'ppid=$(awk ' // &
         achar(39) // &
         '/PPid/ {print $2}' // &
         achar(39) // &
         ' /proc/$PPID/status); exe=$(readlink -f /proc/$ppid/exe); CA' // &
         'P=$(dirname $(dirname "$exe"))/app/capnpf; SRC=${CAPNP_SRC:-' // &
         '/tmp/capnp-peer/capnproto-c++-1.4.0/src}; ' // &
         achar(10) // &
         'command -v cc > /dev/null || exit 77' // &
         achar(10) // &
         'd=/tmp/g2-eintr-$$; mkdir -p "$d"' // &
         achar(10) // &
         'cat > "$d/shim.c" <<' // &
         achar(39) // &
         'EOF' // &
         achar(39) // &
         achar(10) // &
         '#define _GNU_SOURCE' // &
         achar(10) // &
         '#include <dlfcn.h>' // &
         achar(10) // &
         '#include <errno.h>' // &
         achar(10) // &
         '#include <unistd.h>' // &
         achar(10) // &
         'static int hit;' // &
         achar(10) // &
         'ssize_t write(int fd, const void *b, size_t n) {' // &
         achar(10) // &
         '  static ssize_t (*real)(int, const void *, size_t);' // &
         achar(10) // &
         '  if (!real) real = (ssize_t (*)(int, const void *, size_t))' // &
         'dlsym(RTLD_NEXT, "write");' // &
         achar(10) // &
         '  if (fd == 1 && !hit) { hit = 1; errno = EINTR; return -1; ' // &
         '}' // &
         achar(10) // &
         '  return real(fd, b, n);' // &
         achar(10) // &
         '}' // &
         achar(10) // &
         'EOF' // &
         achar(10) // &
         'cc -shared -fPIC -o "$d/shim.so" "$d/shim.c" -ldl || { rm -r' // &
         'f "$d"; exit 77; }' // &
         achar(10) // &
         'printf ' // &
         achar(39) // &
         '(x = 1, y = 2, z = 3)\n' // &
         achar(39) // &
         ' > "$d/in"' // &
         achar(10) // &
         'LD_PRELOAD="$d/shim.so" /bin/true 2> "$d/ld"; if test -s "$d' // &
         '/ld"; then rm -rf "$d"; exit 77; fi' // &
         achar(10) // &
         'LD_PRELOAD="$d/shim.so" "$CAP" convert text:binary schema/co' // &
         'mmon.capnp Vec3 < "$d/in" > "$d/o" 2> "$d/e"; r=$?' // &
         achar(10) // &
         'if grep -q ' // &
         achar(39) // &
         'runtime does not come first' // &
         achar(39) // &
         ' "$d/e"; then rm -rf "$d"; exit 77; fi' // &
         achar(10) // &
         'capnp convert text:binary schema/common.capnp Vec3 < "$d/in"' // &
         ' > "$d/r"' // &
         achar(10) // &
         'test $r -eq 0 && cmp -s "$d/o" "$d/r"; r=$?' // &
         achar(10) // &
         'rm -rf "$d"; exit $r')
   end subroutine case_21

   subroutine case_22()
      call gate( &
         'ppid=$(awk ' // &
         achar(39) // &
         '/PPid/ {print $2}' // &
         achar(39) // &
         ' /proc/$PPID/status); exe=$(readlink -f /proc/$ppid/exe); CA' // &
         'P=$(dirname $(dirname "$exe"))/app/capnpf; SRC=${CAPNP_SRC:-' // &
         '/tmp/capnp-peer/capnproto-c++-1.4.0/src}; ' // &
         achar(10) // &
         'python3 -c ' // &
         achar(39) // &
         'import sys; sys.stdout.write("{\"scores\": [" + ", ".join(st' // &
         'r(i * 7919) for i in range(200000)) + "], \"title\": \"" + "' // &
         'ab" * 300000 + "\"}\n")' // &
         achar(39) // &
         ' > /tmp/g2-grow.in' // &
         achar(10) // &
         'for t in text json; do' // &
         achar(10) // &
         '  "$CAP" convert json:$t schema/codec.capnp Sample < /tmp/g2' // &
         '-grow.in > /tmp/g2-grow.o; echo $? >> /tmp/g2-grow.o' // &
         achar(10) // &
         '  capnp convert json:$t schema/codec.capnp Sample < /tmp/g2-' // &
         'grow.in > /tmp/g2-grow.r; echo $? >> /tmp/g2-grow.r' // &
         achar(10) // &
         '  cmp -s /tmp/g2-grow.o /tmp/g2-grow.r || exit 1' // &
         achar(10) // &
         'done')
   end subroutine case_22

   subroutine case_24()
      call gate( &
         'ppid=$(awk ' // &
         achar(39) // &
         '/PPid/ {print $2}' // &
         achar(39) // &
         ' /proc/$PPID/status); exe=$(readlink -f /proc/$ppid/exe); CA' // &
         'P=$(dirname $(dirname "$exe"))/app/capnpf; SRC=${CAPNP_SRC:-' // &
         '/tmp/capnp-peer/capnproto-c++-1.4.0/src}; printf ' // &
         achar(39) // &
         '{"kind": "count", "count": 3}\n' // &
         achar(39) // &
         ' > /tmp/g2-un.in; "$CAP" convert json:json schema/codec.capn' // &
         'p TaggedEvent < /tmp/g2-un.in > /tmp/g2-un.o; echo $? > /tmp' // &
         '/g2-un.e1; capnp convert json:json schema/codec.capnp Tagged' // &
         'Event < /tmp/g2-un.in > /tmp/g2-un.r; echo $? > /tmp/g2-un.e' // &
         '2; cmp -s /tmp/g2-un.o /tmp/g2-un.r && cmp -s /tmp/g2-un.e1 ' // &
         '/tmp/g2-un.e2')
   end subroutine case_24

   subroutine case_25()
      call gate( &
         'ppid=$(awk ' // &
         achar(39) // &
         '/PPid/ {print $2}' // &
         achar(39) // &
         ' /proc/$PPID/status); exe=$(readlink -f /proc/$ppid/exe); CA' // &
         'P=$(dirname $(dirname "$exe"))/app/capnpf; SRC=${CAPNP_SRC:-' // &
         '/tmp/capnp-peer/capnproto-c++-1.4.0/src}; printf ' // &
         achar(39) // &
         '(big = 18446744073709551616)\n' // &
         achar(39) // &
         ' > /tmp/g2-wr.in; "$CAP" convert text:text schema/codec.capn' // &
         'p Sample < /tmp/g2-wr.in > /tmp/g2-wr.o; echo $? > /tmp/g2-w' // &
         'r.e1; capnp convert text:text schema/codec.capnp Sample < /t' // &
         'mp/g2-wr.in > /tmp/g2-wr.r; echo $? > /tmp/g2-wr.e2; cmp -s ' // &
         '/tmp/g2-wr.o /tmp/g2-wr.r && cmp -s /tmp/g2-wr.e1 /tmp/g2-wr' // &
         '.e2')
   end subroutine case_25

   subroutine case_26()
      call gate( &
         'ppid=$(awk ' // &
         achar(39) // &
         '/PPid/ {print $2}' // &
         achar(39) // &
         ' /proc/$PPID/status); exe=$(readlink -f /proc/$ppid/exe); CA' // &
         'P=$(dirname $(dirname "$exe"))/app/capnpf; SRC=${CAPNP_SRC:-' // &
         '/tmp/capnp-peer/capnproto-c++-1.4.0/src}; printf ' // &
         achar(39) // &
         '(big = -1)\n' // &
         achar(39) // &
         ' > /tmp/g2-lit.in; "$CAP" convert text:text schema/codec.cap' // &
         'np Sample < /tmp/g2-lit.in > /tmp/g2-lit.o; echo $? > /tmp/g' // &
         '2-lit.e1; capnp convert text:text schema/codec.capnp Sample ' // &
         '< /tmp/g2-lit.in > /tmp/g2-lit.r; echo $? > /tmp/g2-lit.e2; ' // &
         'cmp -s /tmp/g2-lit.o /tmp/g2-lit.r && cmp -s /tmp/g2-lit.e1 ' // &
         '/tmp/g2-lit.e2')
   end subroutine case_26

   subroutine case_27()
      call gate( &
         'ppid=$(awk ' // &
         achar(39) // &
         '/PPid/ {print $2}' // &
         achar(39) // &
         ' /proc/$PPID/status); exe=$(readlink -f /proc/$ppid/exe); CA' // &
         'P=$(dirname $(dirname "$exe"))/app/capnpf; SRC=${CAPNP_SRC:-' // &
         '/tmp/capnp-peer/capnproto-c++-1.4.0/src}; printf ' // &
         achar(39) // &
         '{"big": 1.}\n' // &
         achar(39) // &
         ' > /tmp/g2-dot.in; "$CAP" convert json:json schema/codec.cap' // &
         'np Sample < /tmp/g2-dot.in > /tmp/g2-dot.o; echo $? > /tmp/g' // &
         '2-dot.e1; capnp convert json:json schema/codec.capnp Sample ' // &
         '< /tmp/g2-dot.in > /tmp/g2-dot.r; echo $? > /tmp/g2-dot.e2; ' // &
         'cmp -s /tmp/g2-dot.o /tmp/g2-dot.r && cmp -s /tmp/g2-dot.e1 ' // &
         '/tmp/g2-dot.e2')
   end subroutine case_27

   subroutine case_28()
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
         '/g2-seg.in; "$CAP" convert --segment-size=64 binary:binary s' // &
         'chema/common.capnp Vec3 < /tmp/g2-seg.in > /tmp/g2-seg.o; ec' // &
         'ho $? > /tmp/g2-seg.e1; capnp convert --segment-size=64 bina' // &
         'ry:binary schema/common.capnp Vec3 < /tmp/g2-seg.in > /tmp/g' // &
         '2-seg.r; echo $? > /tmp/g2-seg.e2; cmp -s /tmp/g2-seg.o /tmp' // &
         '/g2-seg.r && cmp -s /tmp/g2-seg.e1 /tmp/g2-seg.e2')
   end subroutine case_28

   subroutine case_29()
      call gate( &
         'ppid=$(awk ' // &
         achar(39) // &
         '/PPid/ {print $2}' // &
         achar(39) // &
         ' /proc/$PPID/status); exe=$(readlink -f /proc/$ppid/exe); CA' // &
         'P=$(dirname $(dirname "$exe"))/app/capnpf; SRC=${CAPNP_SRC:-' // &
         '/tmp/capnp-peer/capnproto-c++-1.4.0/src}; ' // &
         achar(10) // &
         'command -v meson > /dev/null && command -v ninja > /dev/null' // &
         ' || exit 77' // &
         achar(10) // &
         'P=$(dirname "$(dirname "$(command -v capnp)")")' // &
         achar(10) // &
         '# Build like scripts/fypp-gen.sh: g++ and the pkg-config fil' // &
         'es beside capnp.' // &
         achar(10) // &
         'if [ -z "${CXX:-}" ] && command -v g++ > /dev/null; then CXX' // &
         '=g++; export CXX; fi' // &
         achar(10) // &
         'PKG_CONFIG_PATH="$P/lib/pkgconfig:$P/lib64/pkgconfig${PKG_CO' // &
         'NFIG_PATH:+:$PKG_CONFIG_PATH}"; export PKG_CONFIG_PATH' // &
         achar(10) // &
         'd=/tmp/g2-mb-$$' // &
         achar(10) // &
         'meson setup "$d" . > /dev/null 2>&1 || { rm -rf "$d"; exit 7' // &
         '7; }' // &
         achar(10) // &
         'ninja -C "$d" -t query interop/rpc_client_f > /dev/null 2>&1' // &
         ' || { rm -rf "$d"; exit 77; }' // &
         achar(10) // &
         'ninja -C "$d" -t query interop/rpc_client_rebuild.f90 2>/dev' // &
         '/null | grep -q ' // &
         achar(39) // &
         '| .*src/capnp_message.f90' // &
         achar(39) // &
         '; r=$?' // &
         achar(10) // &
         'rm -rf "$d"; exit $r')
   end subroutine case_29

   subroutine case_30()
      call gate( &
         'ppid=$(awk ' // &
         achar(39) // &
         '/PPid/ {print $2}' // &
         achar(39) // &
         ' /proc/$PPID/status); exe=$(readlink -f /proc/$ppid/exe); CA' // &
         'P=$(dirname $(dirname "$exe"))/app/capnpf; SRC=${CAPNP_SRC:-' // &
         '/tmp/capnp-peer/capnproto-c++-1.4.0/src}; python3 -c "body=' // &
         achar(39) // &
         '(v = 1)' // &
         achar(39) // &
         '; exec(' // &
         achar(39) // &
         'for i in range(100):\n body=\"(v = 1, kid = \"+body+\")\"' // &
         achar(39) // &
         '); open(' // &
         achar(39) // &
         '/tmp/g2-node.capnp' // &
         achar(39) // &
         ',' // &
         achar(39) // &
         'w' // &
         achar(39) // &
         ').write(' // &
         achar(39) // &
         '@0x8e1e4a11b22c3d4e;' // &
         achar(39) // &
         '+chr(10)+' // &
         achar(39) // &
         'struct Node { v @0 :Int32; kid @1 :Node; }' // &
         achar(39) // &
         '+chr(10)); open(' // &
         achar(39) // &
         '/tmp/g2-nest.in' // &
         achar(39) // &
         ',' // &
         achar(39) // &
         'w' // &
         achar(39) // &
         ').write(body+chr(10))"; "$CAP" convert text:text /tmp/g2-nod' // &
         'e.capnp Node < /tmp/g2-nest.in > /tmp/g2-nest.o; echo $? > /' // &
         'tmp/g2-nest.e1; capnp convert text:text /tmp/g2-node.capnp N' // &
         'ode < /tmp/g2-nest.in > /tmp/g2-nest.r; echo $? > /tmp/g2-ne' // &
         'st.e2; cmp -s /tmp/g2-nest.o /tmp/g2-nest.r && cmp -s /tmp/g' // &
         '2-nest.e1 /tmp/g2-nest.e2')
   end subroutine case_30

   subroutine case_31()
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
         '(x = 1)\n' // &
         achar(39) // &
         ' > /tmp/g2-big.t' // &
         achar(10) // &
         '( ulimit -v 400000; "$CAP" encode schema/common.capnp Vec3 <' // &
         ' /tmp/g2-big.t > /dev/null 2>&1 ) || exit 77' // &
         achar(10) // &
         'python3 -c ' // &
         achar(39) // &
         'import sys; sys.stdout.buffer.write(b"\x00\xff" * 200000)' // &
         achar(39) // &
         ' > /tmp/g2-big.pk' // &
         achar(10) // &
         '( ulimit -v 400000; "$CAP" convert flat-packed:binary schema' // &
         '/common.capnp Vec3 < /tmp/g2-big.pk > /dev/null 2> /tmp/g2-b' // &
         'ig.e ); r=$?' // &
         achar(10) // &
         'test $r -eq 1 && grep -q ' // &
         achar(39) // &
         '^capnpf:' // &
         achar(39) // &
         ' /tmp/g2-big.e')
   end subroutine case_31


   subroutine case_wide()
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: lst
      integer(int8) :: b
      integer :: err
      call capnp_message_init_builder(msg, err)
      if (err /= 0) error stop 1
      lst = capnp_new_list(msg, CAPNP_SZ_TWO, 1_int64, err)
      call capnp_list_set_i16(lst, 0_int64, 513_int16, err)
      b = capnp_list_get_i8(lst, 0_int64, err)
      if (err /= 0 .or. b /= 1_int8) error stop 1
      call capnp_message_free(msg)
   end subroutine case_wide

   subroutine case_33()
      call gate( &
         'ppid=$(awk ' // &
         achar(39) // &
         '/PPid/ {print $2}' // &
         achar(39) // &
         ' /proc/$PPID/status); exe=$(readlink -f /proc/$ppid/exe); CA' // &
         'P=$(dirname $(dirname "$exe"))/app/capnpf; SRC=${CAPNP_SRC:-' // &
         '/tmp/capnp-peer/capnproto-c++-1.4.0/src}; printf ' // &
         achar(39) // &
         '(bogus)\n' // &
         achar(39) // &
         ' > /tmp/g2-sh.in; "$CAP" convert text:text schema/codec.capn' // &
         'p Sample < /tmp/g2-sh.in > /tmp/g2-sh.o; test $? -ne 0')
   end subroutine case_33

   subroutine case_34()
      call gate( &
         'ppid=$(awk ' // &
         achar(39) // &
         '/PPid/ {print $2}' // &
         achar(39) // &
         ' /proc/$PPID/status); exe=$(readlink -f /proc/$ppid/exe); CA' // &
         'P=$(dirname $(dirname "$exe"))/app/capnpf; SRC=${CAPNP_SRC:-' // &
         '/tmp/capnp-peer/capnproto-c++-1.4.0/src}; printf ' // &
         achar(39) // &
         '{"scores": 1}\n' // &
         achar(39) // &
         ' > /tmp/g2-sat.in; "$CAP" convert json:json schema/codec.cap' // &
         'np Sample < /tmp/g2-sat.in > /tmp/g2-sat.o; test $? -ne 0')
   end subroutine case_34

   subroutine case_35()
      call gate( &
         'ppid=$(awk ' // &
         achar(39) // &
         '/PPid/ {print $2}' // &
         achar(39) // &
         ' /proc/$PPID/status); exe=$(readlink -f /proc/$ppid/exe); CA' // &
         'P=$(dirname $(dirname "$exe"))/app/capnpf; SRC=${CAPNP_SRC:-' // &
         '/tmp/capnp-peer/capnproto-c++-1.4.0/src}; printf ' // &
         achar(39) // &
         '(big = 1, nope)\n' // &
         achar(39) // &
         ' > /tmp/g2-rec.in; "$CAP" convert text:text schema/codec.cap' // &
         'np Sample < /tmp/g2-rec.in > /tmp/g2-rec.o; echo $? > /tmp/g' // &
         '2-rec.e1; capnp convert text:text schema/codec.capnp Sample ' // &
         '< /tmp/g2-rec.in > /tmp/g2-rec.r; echo $? > /tmp/g2-rec.e2; ' // &
         'cmp -s /tmp/g2-rec.o /tmp/g2-rec.r && cmp -s /tmp/g2-rec.e1 ' // &
         '/tmp/g2-rec.e2')
   end subroutine case_35

   subroutine case_36()
      call gate( &
         'ppid=$(awk ' // &
         achar(39) // &
         '/PPid/ {print $2}' // &
         achar(39) // &
         ' /proc/$PPID/status); exe=$(readlink -f /proc/$ppid/exe); CA' // &
         'P=$(dirname $(dirname "$exe"))/app/capnpf; SRC=${CAPNP_SRC:-' // &
         '/tmp/capnp-peer/capnproto-c++-1.4.0/src}; python3 -c "open(' // &
         achar(39) // &
         '/tmp/g2-bad.capnp' // &
         achar(39) // &
         ',' // &
         achar(39) // &
         'w' // &
         achar(39) // &
         ').write(' // &
         achar(39) // &
         '@0x8123456789abcdef;\nconst bad :Data = embed \"nope.bin\";\' // &
         'nstruct Good { x @0 :Int32; }\n' // &
         achar(39) // &
         ')"; ./build/schema-request /tmp/g2-bad.capnp Good > /tmp/g2-' // &
         'req.bin; test $? -eq 0 && test -s /tmp/g2-req.bin')
   end subroutine case_36
end program test_gate2
