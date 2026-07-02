!> Interop: decode messages produced by the reference `capnp encode` tool
!> (flat and packed fixtures checked in under test/fixtures) with this
!> runtime and verify every field of the sample AddressBook.
program test_interop
   use capnp
   use capnp_stream
   use addressbook_schema
   implicit none

   integer :: nfail = 0
   type(capnp_message_t), target :: msg
   integer(int8), allocatable :: bytes(:)
   integer :: err

   call capnp_read_file('test/fixtures/addressbook.bin', bytes, err)
   if (err /= CAPNP_OK) then
      print '(a)', 'SKIP: fixtures not found (run from project root)'
      stop 0
   end if
   call capnp_deserialize_bytes(bytes, msg, err)
   call check_(err == CAPNP_OK, 'flat: deserializes')
   call verify(msg, 'flat')
   call capnp_message_free(msg)

   call capnp_read_file('test/fixtures/addressbook.packed.bin', bytes, err)
   call check_(err == CAPNP_OK, 'packed: fixture read')
   call capnp_deserialize_packed_bytes(bytes, msg, err)
   call check_(err == CAPNP_OK, 'packed: deserializes')
   call verify(msg, 'packed')
   call capnp_message_free(msg)

   if (nfail > 0) then
      print '(a,i0,a)', 'FAILED: ', nfail, ' assertion(s)'
      error stop 1
   end if
   print '(a)', 'All interop tests passed.'

contains

   subroutine check_(cond, name)
      logical, intent(in) :: cond
      character(len=*), intent(in) :: name
      if (.not. cond) then
         nfail = nfail + 1
         print '(a,a)', 'FAIL: ', name
      end if
   end subroutine check_

   subroutine verify(rmsg, label)
      type(capnp_message_t), intent(inout), target :: rmsg
      character(len=*), intent(in) :: label
      type(addressbook_t) :: book
      type(person_t) :: pe
      type(phone_t) :: ph
      type(capnp_ptr_t) :: people, phones
      integer :: err
      character(len=:), allocatable :: s

      book = addressbook_root(rmsg, err)
      call check_(err == CAPNP_OK, label//': root')
      people = addressbook_people(book, err)
      call check_(capnp_list_len(people) == 2_int64, label//': two people')

      pe = addressbook_person(people, 0, err)
      call check_(person_get_id(pe) == 123_int64, label//': alice id')
      call person_get_name(pe, s, err)
      call check_(s == 'Alice', label//': alice name')
      call person_get_email(pe, s, err)
      call check_(s == 'alice@example.com', label//': alice email')
      phones = person_phones(pe, err)
      call check_(capnp_list_len(phones) == 1_int64, label//': alice one phone')
      ph = person_phone(phones, 0, err)
      call phone_get_number(ph, s, err)
      call check_(s == '555-1212', label//': alice number')
      call check_(phone_get_type(ph) == PHONE_TYPE_MOBILE, label//': alice type')
      call check_(person_employment_which(pe) == EMPLOYMENT_SCHOOL, label//': alice school')
      call person_get_employer(pe, s, err)
      call check_(s == 'MIT', label//': alice school name')

      pe = addressbook_person(people, 1, err)
      call check_(person_get_id(pe) == 456_int64, label//': bob id')
      call person_get_name(pe, s, err)
      call check_(s == 'Bob', label//': bob name')
      phones = person_phones(pe, err)
      call check_(capnp_list_len(phones) == 2_int64, label//': bob two phones')
      ph = person_phone(phones, 0, err)
      call phone_get_number(ph, s, err)
      call check_(s == '555-4567', label//': bob home number')
      call check_(phone_get_type(ph) == PHONE_TYPE_HOME, label//': bob home type')
      ph = person_phone(phones, 1, err)
      call check_(phone_get_type(ph) == PHONE_TYPE_WORK, label//': bob work type')
      call check_(person_employment_which(pe) == EMPLOYMENT_UNEMPLOYED, label//': bob unemployed')
   end subroutine verify

end program test_interop
