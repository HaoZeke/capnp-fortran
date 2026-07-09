!> Canonicalization: our canonical bytes must equal `capnp convert
!> binary:canonical` output for the addressbook fixture, byte for byte,
!> and canonicalization must be idempotent.
module test_canonical
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed, skip_test
   use capnp
   implicit none

   private
   public :: collect_canonical

contains

   subroutine collect_canonical(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [new_unittest("canonical", run_canonical)]
   end subroutine collect_canonical

   subroutine check_(error, cond, name)
      type(error_type), allocatable, intent(inout) :: error
      logical, intent(in) :: cond
      character(len=*), intent(in) :: name
      if (allocated(error)) return
      call check(error, cond, name)
   end subroutine check_

   subroutine run_canonical(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg, msg2
      integer(int8), allocatable :: raw(:), ref(:), ours(:), again(:), framed(:)
      integer :: err
      integer(int64) :: i

      call capnp_read_file('test/fixtures/addressbook.bin', raw, err)
      if (err /= CAPNP_OK) then
         call skip_test(error, 'fixtures not found (run from project root)')
         return
         end if
      call capnp_read_file('test/fixtures/addressbook.canonical.bin', ref, err)
      call check_(error, err == CAPNP_OK, 'canon: reference fixture read')

      call capnp_deserialize_bytes(raw, msg, err)
      call capnp_canonicalize(msg, ours, err)
      call check_(error, err == CAPNP_OK, 'canon: canonicalize runs')
      call check_(error, size(ours) == size(ref), 'canon: size matches capnp tool')
      if (size(ours) == size(ref)) then
         call check_(error, all(ours == ref), 'canon: bytes match capnp tool')
         if (.not. all(ours == ref)) then
            do i = 0_int64, size(ours, kind=int64) - 1_int64
               if (ours(i) /= ref(i)) then
                  print '(a,i0,a,i4,a,i4)', '  first diff at byte ', i, ': ours ', &
                     ours(i), ' ref ', ref(i)
                  exit
               end if
            end do
         end if
      else
         print '(a,i0,a,i0)', '  sizes: ours ', size(ours), ' ref ', size(ref)
      end if

      ! Idempotence: canonical(canonical(m)) == canonical(m). Wrap the raw
      ! segment in a single-segment framing to re-read it.
      allocate (framed(0:size(ours) + 7))
      framed = 0_int8
      framed(4) = int(mod(size(ours)/8, 256), int8)
      framed(5) = int(mod(size(ours)/8/256, 256), int8)
      framed(8:8 + size(ours) - 1) = ours
      call capnp_deserialize_bytes(framed, msg2, err)
      call capnp_canonicalize(msg2, again, err)
      call check_(error, err == CAPNP_OK .and. size(again) == size(ours), 'canon: idempotent size')
      call check_(error, all(again == ours), 'canon: idempotent bytes')

      call capnp_message_free(msg)
      call capnp_message_free(msg2)
   end subroutine run_canonical

end module test_canonical
