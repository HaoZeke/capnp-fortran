!> Regressions for the text and JSON codec review.
module test_regress
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed, skip_test
   use, intrinsic :: iso_fortran_env, only: int64
   use capnp
   use capnp_dynamic
   use capnp_text
   use capnp_json
   implicit none
   private
   public :: collect_regress

   character(len=*), parameter :: tmp = '/tmp/capnpf-regress'

contains


   subroutine collect_regress(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [ &
         new_unittest("comment-eof", run_comment_eof), &
         new_unittest("list-linear", run_list_linear), &
         new_unittest("text-leniency", run_text_leniency), &
         new_unittest("unloaded-guard", run_unloaded) &
         ]
   end subroutine collect_regress

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

   function cap_setup() result(s)
      character(len=:), allocatable :: s
      s = 'set -- build/gfortran_*/app/capnpf && test $# -eq 1 && CAP=$1'
   end function cap_setup

   subroutine run_comment_eof(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root
      integer(int8), allocatable :: bytes(:)
      integer :: err, idx
      integer(int64) :: count
      call capnp_read_file('test/fixtures/kitchen.cgr.bin', bytes, err)
      if (err /= CAPNP_OK) then
         call skip_test(error, 'kitchen cgr missing')
         return
      end if
      call capnp_dyn_load(schema, bytes, err)
      call check_(error, err == CAPNP_OK, 'comment: schema')
      idx = capnp_dyn_find(schema, 'Sink', err)
      call check_(error, idx > 0, 'comment: Sink')
      call capnp_text_decode(schema, idx, '(count = 1) # c', msg, err)
      call check_(error, err == CAPNP_OK, 'comment: decode at eof')
      root = capnp_root(msg, err)
      count = capnp_dyn_get_int(schema, idx, root, 'count', err)
      call check_(error, err == CAPNP_OK .and. count == 1_int64, 'comment: count is 1')
      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end subroutine run_comment_eof

   subroutine run_list_linear(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root, lst
      character(len=:), allocatable :: text, src
      integer(int8), allocatable :: bytes(:)
      integer :: err, idx
      integer(int64) :: c1, c2, rate, n
      real :: elapsed
      if (.not. have_capnp()) then
         call skip_test(error, 'capnp tool not on PATH')
         return
      end if
      if (.not. sh('mkdir -p '//tmp)) then
         call test_failed(error, 'mkdir')
         return
      end if
      src = '@0x8f0c3d2b1a947e65;'//achar(10)// &
            'struct IntList { xs @0 :List(Int32); }'//achar(10)
      call spit(tmp//'/intlist.capnp', src, err)
      if (.not. sh('capnp compile -o- '//tmp//'/intlist.capnp > '//tmp//'/intlist.cgr')) then
         call test_failed(error, 'compile IntList')
         return
      end if
      call capnp_read_file(tmp//'/intlist.cgr', bytes, err)
      call capnp_dyn_load(schema, bytes, err)
      call check_(error, err == CAPNP_OK, 'list: schema')
      idx = capnp_dyn_find(schema, 'IntList', err)
      call check_(error, idx > 0, 'list: type')
      call capnp_message_init_builder(msg, err)
      root = capnp_dyn_new_root(schema, idx, msg, err)
      n = 200000_int64
      lst = capnp_dyn_init_list(schema, idx, root, 'xs', n, err)
      call check_(error, err == CAPNP_OK .and. capnp_list_len(lst) == n, 'list: allocated')
      call capnp_text_encode(schema, idx, root, text, err, pretty=.false., &
                             traversal_words=1_int64)
      call check_(error, err == CAPNP_ERR_TRAVERSAL, 'list: tiny traversal budget')
      call system_clock(c1, rate)
      call capnp_text_encode(schema, idx, root, text, err, pretty=.false.)
      call system_clock(c2)
      elapsed = real(c2 - c1)/real(rate)
      call check_(error, err == CAPNP_OK, 'list: encode')
      call check_(error, elapsed < 3.0, 'list: encode under 3s')
      call check_(error, index(text, '(xs = [0, 0, 0,') == 1, 'list: prefix')
      call check_(error, index(text, '1') == 0, 'list: only zeros')
      call spit(tmp//'/big.txt', text, err)
      call check_(error, sh('capnp encode '//tmp//'/intlist.capnp IntList < ' &
         //tmp//'/big.txt > '//tmp//'/big.bin'), 'list: capnp accepts the text')
      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end subroutine run_list_linear

   subroutine run_text_leniency(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root
      integer(int8), allocatable :: bytes(:)
      integer :: err, idx
      integer(int64) :: count
      real(real64) :: ratio
      call capnp_read_file('test/fixtures/kitchen.cgr.bin', bytes, err)
      if (err /= CAPNP_OK) then
         call skip_test(error, 'kitchen cgr missing')
         return
      end if
      call capnp_dyn_load(schema, bytes, err)
      idx = capnp_dyn_find(schema, 'Sink', err)
      call capnp_text_decode(schema, idx, '(count = +10)', msg, err)
      call check_(error, err /= CAPNP_OK, 'text: reject unary plus')
      call capnp_text_decode(schema, idx, '(ratio = .5)', msg, err)
      call check_(error, err /= CAPNP_OK, 'text: reject leading dot')
      call capnp_text_decode(schema, idx, '(state = 1)', msg, err)
      call check_(error, err /= CAPNP_OK, 'text: reject numeric enum')
      call capnp_text_decode(schema, idx, '(state = (1))', msg, err)
      call check_(error, err /= CAPNP_OK, 'text: reject parenthesised ordinal')
      call capnp_text_decode(schema, idx, '(count = 010)', msg, err)
      call check_(error, err == CAPNP_OK, 'text: octal still accepted')
      root = capnp_root(msg, err)
      count = capnp_dyn_get_int(schema, idx, root, 'count', err)
      call check_(error, err == CAPNP_OK .and. count == 8_int64, 'text: 010 is eight')
      call capnp_message_free(msg)
      call capnp_text_decode(schema, idx, '(ratio = 5., state = (busy))', msg, err)
      call check_(error, err == CAPNP_OK, 'text: trailing dot and (name)')
      root = capnp_root(msg, err)
      ratio = capnp_dyn_get_f64(schema, idx, root, 'ratio', err)
      call check_(error, err == CAPNP_OK .and. abs(ratio - 5.0_real64) < 1.0e-12_real64, &
                  'text: 5. is five')
      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end subroutine run_text_leniency

   subroutine run_unloaded(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: p
      character(len=:), allocatable :: text
      integer :: err
      p = capnp_ptr_t()
      call check_text_bounds(error)
      if (allocated(error)) return
      call capnp_text_encode(schema, 1, p, text, err)
      call check_(error, err == CAPNP_ERR_ARG, 'guard: text encode unloaded schema')
      call capnp_text_decode(schema, 1, '()', msg, err)
      call check_(error, err == CAPNP_ERR_ARG, 'guard: text decode unloaded schema')
      call capnp_json_encode(schema, 1, p, text, err)
      call check_(error, err == CAPNP_ERR_ARG, 'guard: json encode unloaded schema')
      call capnp_json_decode(schema, 1, '{}', msg, err)
      call check_(error, err == CAPNP_ERR_ARG, 'guard: json decode unloaded schema')
   end subroutine run_unloaded

   subroutine spit(path, s, err, err_ignored)
      character(len=*), intent(in) :: path, s
      integer, intent(out), optional :: err
      type(error_type), allocatable, intent(inout), optional :: err_ignored
      integer(int8), allocatable :: b(:)
      integer :: i, local
      allocate (b(0:len(s) - 1))
      do i = 1, len(s)
         b(i - 1) = i8_of(iachar(s(i:i)))
      end do
      call capnp_write_file(path, b, local)
      if (present(err)) err = local
      if (present(err_ignored)) then
         if (local /= CAPNP_OK) call test_failed(err_ignored, 'write '//path)
      end if
   end subroutine spit

   subroutine slurp(path, s, err)
      character(len=*), intent(in) :: path
      character(len=:), allocatable, intent(out) :: s
      integer, intent(out) :: err
      integer(int8), allocatable :: b(:)
      integer :: i, n, lb, u
      call capnp_read_file(path, b, err)
      if (err /= CAPNP_OK) return
      n = size(b)
      lb = lbound(b, 1)
      allocate (character(len=n) :: s)
      do i = 1, n
         u = int(b(lb + i - 1))
         if (u < 0) u = u + 256
         s(i:i) = achar(u)
      end do
   end subroutine slurp

   pure function i8_of(u) result(v)
      integer, intent(in) :: u
      integer(int8) :: v
      integer :: x
      x = iand(u, 255)
      if (x >= 128) then
         v = int(x - 256, int8)
      else
         v = int(x, int8)
      end if
   end function i8_of

   function clash_schema() result(s)
      character(len=:), allocatable :: s
      s = '@0xe31a90c4b75d28f6;'//achar(10)// &
          'using Json = import "/capnp/compat/json.capnp";'//achar(10)// &
          'struct NameClash {'//achar(10)// &
          '  a @0 :Int32 $Json.name("same");'//achar(10)// &
          '  b @1 :Int32 $Json.name("same");'//achar(10)// &
          '}'//achar(10)// &
          'struct FlatClash {'//achar(10)// &
          '  left :group $Json.flatten() { x @0 :Int32; }'//achar(10)// &
          '  right :group $Json.flatten() { x @1 :Int32; }'//achar(10)// &
          '}'//achar(10)// &
          'struct DiscClash {'//achar(10)// &
          '  a @0 :Int32;'//achar(10)// &
          '  u :union $Json.flatten() $Json.discriminator(name = "a") {'//achar(10)// &
          '    v @1 :Int32;'//achar(10)// &
          '    w @2 :Int32;'//achar(10)// &
          '  }'//achar(10)// &
          '}'//achar(10)// &
          'struct VoidVal $Json.discriminator(valueName = "payload") {'//achar(10)// &
          '  union {'//achar(10)// &
          '    empty @0 :Void;'//achar(10)// &
          '    n @1 :Int32;'//achar(10)// &
          '  }'//achar(10)// &
          '}'//achar(10)// &
          'struct MissDisc $Json.discriminator(name = "kind") {'//achar(10)// &
          '  seq @0 :Int32;'//achar(10)// &
          '  union {'//achar(10)// &
          '    idle @1 :Void;'//achar(10)// &
          '    count @2 :Int32;'//achar(10)// &
          '  }'//achar(10)// &
          '}'//achar(10)
   end function clash_schema

   !> Recompile the text encoder with bounds checks. The parent evaluates
   !> size() on an unallocated node array and aborts. The fix returns
   !> an argument error before that call.
   subroutine check_text_bounds(error)
      type(error_type), allocatable, intent(inout) :: error
      if (allocated(error)) return
      if (.not. sh('mkdir -p '//tmp//'/mods' &
         //' && mod=$(find build -name capnp_text.mod | xargs ls -t | head -1)' &
         //' && lib=$(find build -name libcapnp.a | xargs ls -t | head -1)' &
         //' && test -n "$mod" && test -n "$lib"' &
         //' && dir=$(dirname "$mod")' &
         //' && san=; if [ -n "${ASAN_OPTIONS:-}" ]; then san="-fsanitize=address,undefined"; fi' &
         //' && gfortran $san -fcheck=all -J '//tmp//'/mods -I "$dir" -c src/capnp_text.f90 -o ' &
         //tmp//'/text_check.o' &
         //' && cat > '//tmp//'/ubounds.f90 <<''EOF'' &&'//achar(10)// &
         'program u'//achar(10)// &
         '   use capnp_text, only: capnp_text_encode'//achar(10)// &
         '   use capnp_dynamic, only: capnp_dyn_schema_t'//achar(10)// &
         '   use capnp_message, only: capnp_ptr_t'//achar(10)// &
         '   use capnp_kinds, only: capnp_err_arg'//achar(10)// &
         '   implicit none'//achar(10)// &
         '   type(capnp_dyn_schema_t) :: schema'//achar(10)// &
         '   type(capnp_ptr_t) :: p'//achar(10)// &
         '   character(len=:), allocatable :: text'//achar(10)// &
         '   integer :: err'//achar(10)// &
         '   call capnp_text_encode(schema, 1, p, text, err)'//achar(10)// &
         '   if (err /= capnp_err_arg) stop 1'//achar(10)// &
         'end program'//achar(10)// &
         'EOF'//achar(10)// &
         'gfortran $san -fcheck=all -I '//tmp//'/mods -I "$dir" '//tmp//'/ubounds.f90 ' &
         //tmp//'/text_check.o "$lib" -o '//tmp//'/ubounds' &
         //' && ASAN_OPTIONS=detect_leaks=0 '//tmp//'/ubounds')) then
         call test_failed(error, 'guard: bounds-checked text encode')
      end if
   end subroutine check_text_bounds
end module test_regress
