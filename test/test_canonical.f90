!> Canonicalization: our canonical bytes must equal `capnp convert
!> binary:canonical` output for the addressbook fixture, byte for byte,
!> and canonicalization must be idempotent.
program test_canonical
   use capnp
   implicit none

   integer :: nfail = 0
   type(capnp_message_t), target :: msg, msg2
   integer(int8), allocatable :: raw(:), ref(:), ours(:), again(:), framed(:)
   integer :: err
   integer(int64) :: i

   call capnp_read_file('test/fixtures/addressbook.bin', raw, err)
   if (err /= CAPNP_OK) then
      print '(a)', 'SKIP: fixtures not found (run from project root)'
      stop 0
   end if
   call capnp_read_file('test/fixtures/addressbook.canonical.bin', ref, err)
   call check_(err == CAPNP_OK, 'canon: reference fixture read')

   call capnp_deserialize_bytes(raw, msg, err)
   call capnp_canonicalize(msg, ours, err)
   call check_(err == CAPNP_OK, 'canon: canonicalize runs')
   call check_(size(ours) == size(ref), 'canon: size matches capnp tool')
   if (size(ours) == size(ref)) then
      call check_(all(ours == ref), 'canon: bytes match capnp tool')
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
   call check_(err == CAPNP_OK .and. size(again) == size(ours), 'canon: idempotent size')
   call check_(all(again == ours), 'canon: idempotent bytes')

   call capnp_message_free(msg)
   call capnp_message_free(msg2)

   if (nfail > 0) then
      print '(a,i0,a)', 'FAILED: ', nfail, ' assertion(s)'
      error stop 1
   end if
   print '(a)', 'All canonical tests passed.'

contains

   subroutine check_(cond, name)
      logical, intent(in) :: cond
      character(len=*), intent(in) :: name
      if (.not. cond) then
         nfail = nfail + 1
         print '(a,a)', 'FAIL: ', name
      end if
   end subroutine check_

end program test_canonical
