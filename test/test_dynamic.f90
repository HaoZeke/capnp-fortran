!> Dynamic reflection: load the compiled addressbook schema at runtime
!> and read the reference `capnp encode` fixture purely by type and
!> field names -- no generated code involved.
program test_dynamic
   use capnp
   use capnp_dynamic
   use capnp_schema, only: TYPE_TEXT
   implicit none

   integer :: nfail = 0
   type(capnp_dyn_schema_t), target :: schema
   type(capnp_message_t), target :: msg, bmsg
   type(capnp_ptr_t) :: root, people, person, phones, phone, q
   integer(int8), allocatable :: bytes(:)
   character(len=:), allocatable :: s
   integer :: err, book_idx, person_idx, phone_idx

   call capnp_read_file('test/fixtures/addressbook.cgr.bin', bytes, err)
   if (err /= CAPNP_OK) then
      print '(a)', 'SKIP: fixtures not found (run from project root)'
      stop 0
   end if
   call capnp_dyn_load(schema, bytes, err)
   call check_(err == CAPNP_OK, 'dyn: schema loads')

   book_idx = capnp_dyn_find(schema, 'AddressBook', err)
   person_idx = capnp_dyn_find(schema, 'Person', err)
   phone_idx = capnp_dyn_find(schema, 'Person.PhoneNumber', err)
   call check_(book_idx > 0 .and. person_idx > 0 .and. phone_idx > 0, &
               'dyn: nodes found by name')
   call check_(capnp_dyn_find(schema, 'NoSuchType', err) == 0, 'dyn: absent type is 0')

   call check_(capnp_dyn_field_type(schema, person_idx, 'name', err) == TYPE_TEXT, &
               'dyn: field type text')

   call capnp_read_file('test/fixtures/addressbook.bin', bytes, err)
   call capnp_deserialize_bytes(bytes, msg, err)
   root = capnp_root(msg, err)
   call check_(err == CAPNP_OK, 'dyn: fixture message loads')

   people = capnp_dyn_getp(schema, book_idx, root, 'people', err)
   call check_(err == CAPNP_OK .and. capnp_list_len(people) == 2_int64, &
               'dyn: people list by name')

   person = capnp_list_get_struct(people, 0, err)
   call check_(capnp_dyn_get_int(schema, person_idx, person, 'id', err) == 123_int64, &
               'dyn: alice id by name')
   call capnp_dyn_get_text(schema, person_idx, person, 'name', s, err)
   call check_(s == 'Alice', 'dyn: alice name by name')
   call capnp_dyn_get_text(schema, person_idx, person, 'email', s, err)
   call check_(s == 'alice@example.com', 'dyn: alice email by name')

   phones = capnp_dyn_getp(schema, person_idx, person, 'phones', err)
   phone = capnp_list_get_struct(phones, 0, err)
   call capnp_dyn_get_text(schema, phone_idx, phone, 'number', s, err)
   call check_(s == '555-1212', 'dyn: phone number by name')
   call check_(capnp_dyn_get_int(schema, phone_idx, phone, 'type', err) == 0_int64, &
               'dyn: phone type enum by name')

   ! Unknown field errors cleanly.
   q = capnp_dyn_getp(schema, person_idx, person, 'nonexistent', err)
   call check_(err /= CAPNP_OK, 'dyn: unknown field errors')

   ! Dynamic write: build a person by name and read it back.
   call capnp_message_init_builder(bmsg, err)
   person = capnp_new_struct(bmsg, 1, 4, err)
   call capnp_set_root(bmsg, person, err)
   call capnp_dyn_set_int(schema, person_idx, person, 'id', 777_int64, err)
   call check_(err == CAPNP_OK, 'dyn: set int by name')
   call capnp_dyn_set_text(schema, person_idx, person, 'name', 'Dyn', err)
   call check_(err == CAPNP_OK, 'dyn: set text by name')
   call check_(capnp_dyn_get_int(schema, person_idx, person, 'id', err) == 777_int64, &
               'dyn: write/read int round trip')
   call capnp_dyn_get_text(schema, person_idx, person, 'name', s, err)
   call check_(s == 'Dyn', 'dyn: write/read text round trip')

   call capnp_message_free(msg)
   call capnp_message_free(bmsg)
   call capnp_dyn_free(schema)

   if (nfail > 0) then
      print '(a,i0,a)', 'FAILED: ', nfail, ' assertion(s)'
      error stop 1
   end if
   print '(a)', 'All dynamic reflection tests passed.'

contains

   subroutine check_(cond, name)
      logical, intent(in) :: cond
      character(len=*), intent(in) :: name
      if (.not. cond) then
         nfail = nfail + 1
         print '(a,a)', 'FAIL: ', name
      end if
   end subroutine check_

end program test_dynamic
