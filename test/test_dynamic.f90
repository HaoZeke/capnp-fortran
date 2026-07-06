!> Dynamic reflection: load compiled schemas at runtime and read/write
!> fields by name -- no generated accessors on the read path for
!> addressbook; kitchen is used only to seed bool/f64 wire values that
!> capnp_dyn_get_bool / capnp_dyn_get_f64 then re-read by name.
program test_dynamic
   use capnp
   use capnp_dynamic
   use capnp_schema, only: TYPE_TEXT, NODE_STRUCT, node_which, &
                           node_struct_discriminant_count, &
                           node_struct_discriminant_offset, &
                           node_struct_data_words, node_struct_pointer_count, &
                           node_struct_is_group, node_display_name, &
                           node_struct_fields
   use kitchen_capnp, only: sink_t, sink_new_root, sink_flag_set, sink_ratio_set
   use addressbook_capnp, only: person_t, person_employment_which, &
                                PERSON_EMPLOYMENT_SCHOOL_TAG
   use capnp_union, only: capnp_which
   implicit none

   integer :: nfail = 0
   type(capnp_dyn_schema_t), target :: schema, kschema
   type(capnp_message_t), target :: msg, bmsg, kmsg
   type(capnp_ptr_t) :: root, people, person, phones, phone, q, fl
   type(sink_t) :: sink
   type(person_t) :: pe
   integer(int8), allocatable :: bytes(:)
   character(len=:), allocatable :: s, dn
   integer :: err, book_idx, person_idx, phone_idx, sink_idx, tag, want
   integer :: disc_count, nw, dw, pw, i, named_person
   integer(int64) :: disc_off
   logical :: flag
   real(real64) :: ratio

   call capnp_read_file('test/fixtures/addressbook.cgr.bin', bytes, err)
   if (err /= CAPNP_OK) then
      print '(a)', 'SKIP: fixtures not found (run from project root)'
      stop 0
   end if
   call capnp_dyn_load(schema, bytes, err)
   call check_(err == CAPNP_OK, 'dyn: schema loads')

   book_idx = capnp_dyn_find(schema, 'AddressBook', err)
   named_person = capnp_dyn_find(schema, 'Person', err)
   phone_idx = capnp_dyn_find(schema, 'Person.PhoneNumber', err)
   call check_(book_idx > 0 .and. named_person > 0 .and. phone_idx > 0, &
               'dyn: nodes found by name')
   call check_(capnp_dyn_find(schema, 'NoSuchType', err) == 0, 'dyn: absent type is 0')

   ! Resolve Person to the non-group struct that has the employment union and
   ! the field set the accessors use (name lookup can hit a nested/group node
   ! with a similar leaf name if present).
   person_idx = 0
   do i = 1, size(schema%nodes)
      if (node_which(schema%nodes(i)) /= NODE_STRUCT) cycle
      if (node_struct_is_group(schema%nodes(i))) cycle
      call node_display_name(schema%nodes(i), dn, err)
      if (err /= CAPNP_OK) cycle
      if (index(dn, 'Person') == 0) cycle
      if (index(dn, 'PhoneNumber') > 0) cycle
      dw = node_struct_data_words(schema%nodes(i))
      pw = node_struct_pointer_count(schema%nodes(i))
      disc_count = node_struct_discriminant_count(schema%nodes(i))
      fl = node_struct_fields(schema%nodes(i), err)
      if (err /= CAPNP_OK) cycle
      if (dw == 1 .and. pw == 4 .and. disc_count > 0 .and. &
          capnp_list_len(fl) >= 4_int64) then
         person_idx = i
         exit
      end if
   end do
   if (person_idx == 0) person_idx = named_person
   call check_(person_idx > 0, 'dyn: Person schema node')

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

   ! Union which on fixture Alice (school in the classic addressbook sample).
   pe%p = person
   want = person_employment_which(pe)
   call check_(want == PERSON_EMPLOYMENT_SCHOOL_TAG, &
               'fixture: alice employment is school (generated which)')
   disc_count = node_struct_discriminant_count(schema%nodes(person_idx))
   disc_off = node_struct_discriminant_offset(schema%nodes(person_idx))
   call node_display_name(schema%nodes(person_idx), dn, err)
   print '(a,a,a,i0,a,i0,a,i0,a,i0,a,i0,a,i0)', &
      'Person node: name=', trim(dn), &
      ' handle_dwords=', schema%nodes(person_idx)%dwords, &
      ' handle_pwords=', schema%nodes(person_idx)%pwords, &
      ' field_dwords=', node_struct_data_words(schema%nodes(person_idx)), &
      ' field_pwords=', node_struct_pointer_count(schema%nodes(person_idx)), &
      ' disc_count=', disc_count, ' disc_off=', int(disc_off)
   tag = capnp_dyn_which(schema, person_idx, person, err)
   call check_(err == CAPNP_OK .and. tag == want, &
               'dyn: which matches generated on fixture alice')
   call check_(capnp_which(person, 2) == want, 'dyn: capnp_which(disc=2) agrees')
   phones = capnp_dyn_getp(schema, person_idx, person, 'phones', err)
   phone = capnp_list_get_struct(phones, 0, err)
   call capnp_dyn_get_text(schema, phone_idx, phone, 'number', s, err)
   call check_(s == '555-1212', 'dyn: phone number by name')
   call check_(capnp_dyn_get_int(schema, phone_idx, phone, 'type', err) == 0_int64, &
               'dyn: phone type enum by name')

   q = capnp_dyn_getp(schema, person_idx, person, 'nonexistent', err)
   call check_(err /= CAPNP_OK, 'dyn: unknown field errors')

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

   ! Kitchen CGR + generated seeders: prove dyn bool/f64 read the same wire.
   call capnp_read_file('test/fixtures/kitchen.cgr.bin', bytes, err)
   call check_(err == CAPNP_OK, 'dyn: kitchen cgr reads')
   call capnp_dyn_load(kschema, bytes, err)
   call check_(err == CAPNP_OK, 'dyn: kitchen schema loads')
   sink_idx = capnp_dyn_find(kschema, 'Sink', err)
   call check_(sink_idx > 0, 'dyn: Sink node found')
   call capnp_message_init_builder(kmsg, err)
   sink = sink_new_root(kmsg, err)
   call sink_flag_set(sink, .false., err)
   call sink_ratio_set(sink, 3.5_real64, err)
   call check_(err == CAPNP_OK, 'dyn: kitchen seed ok')
   flag = capnp_dyn_get_bool(kschema, sink_idx, sink%p, 'flag', err)
   call check_(err == CAPNP_OK .and. .not. flag, 'dyn: get_bool flag')
   ratio = capnp_dyn_get_f64(kschema, sink_idx, sink%p, 'ratio', err)
   call check_(err == CAPNP_OK .and. abs(ratio - 3.5_real64) < 1.0e-15_real64, &
               'dyn: get_f64 ratio')

   call capnp_message_free(msg)
   call capnp_message_free(bmsg)
   call capnp_message_free(kmsg)
   call capnp_dyn_free(schema)
   call capnp_dyn_free(kschema)

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
