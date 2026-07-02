!> End-to-end AddressBook: build the classic two-person book, serialize
!> (flat and packed), reread, verify every field. Mirrors the capnp C++
!> sample data so the same bytes can later be fed to `capnp decode`.
program test_addressbook
   use capnp
   use capnp_stream
   use addressbook_schema
   implicit none

   integer :: nfail = 0
   integer(int8), allocatable :: flat(:), packed(:)

   call build_and_check()

   if (nfail > 0) then
      print '(a,i0,a)', 'FAILED: ', nfail, ' assertion(s)'
      error stop 1
   end if
   print '(a)', 'All addressbook tests passed.'

contains

   subroutine check_(cond, name)
      logical, intent(in) :: cond
      character(len=*), intent(in) :: name
      if (.not. cond) then
         nfail = nfail + 1
         print '(a,a)', 'FAIL: ', name
      end if
   end subroutine check_

   subroutine build_and_check()
      type(capnp_message_t), target :: msg, rmsg, pmsg
      type(addressbook_t) :: book
      type(person_t) :: alice, bob
      type(phone_t) :: ph
      type(capnp_ptr_t) :: people, phones
      integer :: err
      character(len=:), allocatable :: s

      call capnp_message_init_builder(msg, err)
      book = new_addressbook(msg, err)
      call check_(err == CAPNP_OK, 'book: new root')

      people = addressbook_init_people(book, 2_int64, err)
      call check_(err == CAPNP_OK, 'book: init people(2)')

      alice = addressbook_person(people, 0, err)
      call person_set_id(alice, 123_int64, err)
      call person_set_name(alice, 'Alice', err)
      call person_set_email(alice, 'alice@example.com', err)
      phones = person_init_phones(alice, 1_int64, err)
      ph = person_phone(phones, 0, err)
      call phone_set_number(ph, '555-1212', err)
      call phone_set_type(ph, PHONE_TYPE_MOBILE, err)
      call person_set_school(alice, 'MIT', err)
      call check_(err == CAPNP_OK, 'alice: built')

      bob = addressbook_person(people, 1, err)
      call person_set_id(bob, 456_int64, err)
      call person_set_name(bob, 'Bob', err)
      call person_set_email(bob, 'bob@example.com', err)
      phones = person_init_phones(bob, 2_int64, err)
      ph = person_phone(phones, 0, err)
      call phone_set_number(ph, '555-4567', err)
      call phone_set_type(ph, PHONE_TYPE_HOME, err)
      ph = person_phone(phones, 1, err)
      call phone_set_number(ph, '555-7654', err)
      call phone_set_type(ph, PHONE_TYPE_WORK, err)
      call person_set_unemployed(bob, err)
      call check_(err == CAPNP_OK, 'bob: built')

      call capnp_serialize_bytes(msg, flat, err)
      call check_(err == CAPNP_OK .and. size(flat) > 0, 'book: serialized')
      call capnp_serialize_packed_bytes(msg, packed, err)
      call check_(err == CAPNP_OK .and. size(packed) < size(flat), 'book: packed smaller')

      call capnp_deserialize_bytes(flat, rmsg, err)
      call verify_book(rmsg, 'flat')
      call capnp_deserialize_packed_bytes(packed, pmsg, err)
      call check_(err == CAPNP_OK, 'book: packed deserializes')
      call verify_book(pmsg, 'packed')

      call capnp_message_free(msg)
      call capnp_message_free(rmsg)
      call capnp_message_free(pmsg)
   end subroutine build_and_check

   subroutine verify_book(rmsg, label)
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
      call check_(s == '555-1212', label//': alice phone number')
      call check_(phone_get_type(ph) == PHONE_TYPE_MOBILE, label//': alice phone type')
      call check_(person_employment_which(pe) == EMPLOYMENT_SCHOOL, label//': alice school tag')
      call person_get_employer(pe, s, err)
      call check_(s == 'MIT', label//': alice school text')

      pe = addressbook_person(people, 1, err)
      call check_(person_get_id(pe) == 456_int64, label//': bob id')
      call person_get_name(pe, s, err)
      call check_(s == 'Bob', label//': bob name')
      phones = person_phones(pe, err)
      call check_(capnp_list_len(phones) == 2_int64, label//': bob two phones')
      ph = person_phone(phones, 1, err)
      call phone_get_number(ph, s, err)
      call check_(s == '555-7654', label//': bob work number')
      call check_(phone_get_type(ph) == PHONE_TYPE_WORK, label//': bob work type')
      call check_(person_employment_which(pe) == EMPLOYMENT_UNEMPLOYED, label//': bob unemployed')
   end subroutine verify_book

end program test_addressbook
