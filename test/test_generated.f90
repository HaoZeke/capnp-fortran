!> Exercise the capnpc-fortran OUTPUT: test/generated/addressbook_capnp.f90
!> is emitted by the plugin from the checked-in CodeGeneratorRequest fixture.
!> Decode the reference `capnp encode` bytes through the generated API, then
!> build the same book with it and re-verify.
program test_generated
   use capnp
   use capnp_stream
   use addressbook_capnp
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
   call check_(err == CAPNP_OK, 'gen: fixture deserializes')
   call verify(msg, 'fixture')
   call capnp_message_free(msg)

   call build_roundtrip()
   call union_reselect()

   if (nfail > 0) then
      print '(a,i0,a)', 'FAILED: ', nfail, ' assertion(s)'
      error stop 1
   end if
   print '(a)', 'All generated-code tests passed.'

contains

   subroutine check_(cond, name)
      logical, intent(in) :: cond
      character(len=*), intent(in) :: name
      if (.not. cond) then
         nfail = nfail + 1
         print '(a,a)', 'FAIL: ', name
      end if
   end subroutine check_

   subroutine build_roundtrip()
      type(capnp_message_t), target :: bmsg, rmsg
      type(address_book_t) :: book
      type(person_t) :: pe
      type(person_phone_number_t) :: ph
      type(capnp_ptr_t) :: people, phones
      integer(int8), allocatable :: b(:)
      integer :: err

      call capnp_message_init_builder(bmsg, err)
      book = address_book_new_root(bmsg, err)
      call check_(err == CAPNP_OK, 'gen: new root')
      people = address_book_people_init(book, 2_int64, err)

      pe%p = capnp_list_get_struct(people, 0, err)
      call person_id_set(pe, 123_int64, err)
      call person_name_set(pe, 'Alice', err)
      call person_email_set(pe, 'alice@example.com', err)
      phones = person_phones_init(pe, 1_int64, err)
      ph%p = capnp_list_get_struct(phones, 0, err)
      call person_phone_number_number_set(ph, '555-1212', err)
      call person_phone_number_type_set(ph, PERSON_PHONE_NUMBER_TYPE_MOBILE, err)
      call person_employment_school_set(pe, 'MIT', err)

      pe%p = capnp_list_get_struct(people, 1, err)
      call person_id_set(pe, 456_int64, err)
      call person_name_set(pe, 'Bob', err)
      call person_email_set(pe, 'bob@example.com', err)
      phones = person_phones_init(pe, 2_int64, err)
      ph%p = capnp_list_get_struct(phones, 0, err)
      call person_phone_number_number_set(ph, '555-4567', err)
      call person_phone_number_type_set(ph, PERSON_PHONE_NUMBER_TYPE_HOME, err)
      ph%p = capnp_list_get_struct(phones, 1, err)
      call person_phone_number_number_set(ph, '555-7654', err)
      call person_phone_number_type_set(ph, PERSON_PHONE_NUMBER_TYPE_WORK, err)
      call person_employment_unemployed_set(pe, err)
      call check_(err == CAPNP_OK, 'gen: built book')

      call capnp_serialize_bytes(bmsg, b, err)
      call capnp_deserialize_bytes(b, rmsg, err)
      call check_(err == CAPNP_OK, 'gen: round trip deserializes')
      call verify(rmsg, 'rebuilt')
      call capnp_message_free(bmsg)
      call capnp_message_free(rmsg)
   end subroutine build_roundtrip

   subroutine verify(rmsg, label)
      type(capnp_message_t), intent(inout), target :: rmsg
      character(len=*), intent(in) :: label
      type(address_book_t) :: book
      type(person_t) :: pe
      type(person_phone_number_t) :: ph
      type(capnp_ptr_t) :: people, phones
      integer :: err
      character(len=:), allocatable :: s

      book = address_book_read_root(rmsg, err)
      people = address_book_people_get(book, err)
      call check_(capnp_list_len(people) == 2_int64, label//': two people')

      pe%p = capnp_list_get_struct(people, 0, err)
      call check_(person_id_get(pe) == 123_int64, label//': alice id')
      call person_name_get(pe, s, err)
      call check_(s == 'Alice', label//': alice name')
      call person_email_get(pe, s, err)
      call check_(s == 'alice@example.com', label//': alice email')
      phones = person_phones_get(pe, err)
      call check_(capnp_list_len(phones) == 1_int64, label//': alice one phone')
      ph%p = capnp_list_get_struct(phones, 0, err)
      call person_phone_number_number_get(ph, s, err)
      call check_(s == '555-1212', label//': alice number')
      call check_(person_phone_number_type_get(ph) == PERSON_PHONE_NUMBER_TYPE_MOBILE, &
                  label//': alice type')
      call check_(person_employment_which(pe) == PERSON_EMPLOYMENT_SCHOOL_TAG, &
                  label//': alice school tag')
      call person_employment_school_get(pe, s, err)
      call check_(s == 'MIT', label//': alice school')

      pe%p = capnp_list_get_struct(people, 1, err)
      call check_(person_id_get(pe) == 456_int64, label//': bob id')
      phones = person_phones_get(pe, err)
      call check_(capnp_list_len(phones) == 2_int64, label//': bob two phones')
      ph%p = capnp_list_get_struct(phones, 1, err)
      call check_(person_phone_number_type_get(ph) == PERSON_PHONE_NUMBER_TYPE_WORK, &
                  label//': bob work type')
      call check_(person_employment_which(pe) == PERSON_EMPLOYMENT_UNEMPLOYED_TAG, &
                  label//': bob unemployed tag')
   end subroutine verify

   !> Re-selecting a union variant must flip the discriminant; the C++
   !> builder overwrite semantics.
   subroutine union_reselect()
      type(capnp_message_t), target :: bmsg
      type(address_book_t) :: book
      type(person_t) :: pe
      type(capnp_ptr_t) :: people
      character(len=:), allocatable :: s
      integer :: err

      call capnp_message_init_builder(bmsg, err)
      book = address_book_new_root(bmsg, err)
      people = address_book_people_init(book, 1_int64, err)
      pe%p = capnp_list_get_struct(people, 0, err)

      call check_(person_employment_which(pe) == PERSON_EMPLOYMENT_UNEMPLOYED_TAG, &
                  'union: fresh struct reads tag 0')
      call person_employment_school_set(pe, 'ETH', err)
      call check_(person_employment_which(pe) == PERSON_EMPLOYMENT_SCHOOL_TAG, &
                  'union: school selected')
      call person_employment_employer_set(pe, 'ACME', err)
      call check_(person_employment_which(pe) == PERSON_EMPLOYMENT_EMPLOYER_TAG, &
                  'union: employer reselected')
      call person_employment_employer_get(pe, s, err)
      call check_(err == CAPNP_OK .and. s == 'ACME', 'union: employer value')
      call person_employment_self_employed_set(pe, err)
      call check_(person_employment_which(pe) == PERSON_EMPLOYMENT_SELF_EMPLOYED_TAG, &
                  'union: void variant reselected')
      call capnp_message_free(bmsg)
   end subroutine union_reselect

end program test_generated
