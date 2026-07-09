!> Dynamic reflection: load compiled schemas at runtime and read/write
!> fields by name -- no generated accessors on the read path for
!> addressbook; kitchen is used only to seed bool/f64 wire values that
!> capnp_dyn_get_bool / capnp_dyn_get_f64 then re-read by name.
module test_dynamic
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed, skip_test
   use capnp
   use capnp_dynamic
   use capnp_schema, only: TYPE_TEXT
   use kitchen_capnp, only: sink_t, sink_new_root, sink_flag_set, sink_ratio_set
   use addressbook_capnp, only: person_t, person_employment_which, &
                                PERSON_EMPLOYMENT_SCHOOL_TAG
   use dual_capnp, only: dual_t, dual_new_root, dual_primary_void_a_set, &
                         dual_primary_text_a_set, dual_secondary_void_b_set, &
                         dual_secondary_int_b_set, dual_primary_which, &
                         dual_secondary_which, DUAL_PRIMARY_VOID_A_TAG, &
                         DUAL_PRIMARY_TEXT_A_TAG, DUAL_SECONDARY_VOID_B_TAG, &
                         DUAL_SECONDARY_INT_B_TAG
   use capnp_union, only: capnp_which
   implicit none
   private
   public :: collect_dynamic

contains

   subroutine collect_dynamic(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [new_unittest("dynamic", run_dynamic)]
   end subroutine collect_dynamic

   subroutine check_(error, cond, name)
      type(error_type), allocatable, intent(inout) :: error
      logical, intent(in) :: cond
      character(len=*), intent(in) :: name
      if (allocated(error)) return
      call check(error, cond, name)
   end subroutine check_

   subroutine run_dynamic(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema, kschema, dschema
      type(capnp_message_t), target :: msg, bmsg, kmsg, dmsg
      type(capnp_ptr_t) :: root, people, person, phones, phone, q
      type(sink_t) :: sink
      type(person_t) :: pe
      type(dual_t) :: dual
      integer(int8), allocatable :: bytes(:)
      character(len=:), allocatable :: s
      integer :: err, book_idx, person_idx, phone_idx, sink_idx, dual_idx, tag, want
      integer :: tag_pri, tag_sec
      logical :: flag
      real(real64) :: ratio

      call capnp_read_file('test/fixtures/addressbook.cgr.bin', bytes, err)
      if (err /= CAPNP_OK) then
         call skip_test(error, 'fixtures not found (run from project root)')
         return
      end if
      call capnp_dyn_load(schema, bytes, err)
      call check_(error, err == CAPNP_OK, 'dyn: schema loads')

      book_idx = capnp_dyn_find(schema, 'AddressBook', err)
      person_idx = capnp_dyn_find(schema, 'Person', err)
      phone_idx = capnp_dyn_find(schema, 'Person.PhoneNumber', err)
      call check_(error, book_idx > 0 .and. person_idx > 0 .and. phone_idx > 0, &
                  'dyn: nodes found by name')
      call check_(error, capnp_dyn_find(schema, 'NoSuchType', err) == 0, 'dyn: absent type is 0')

      call check_(error, capnp_dyn_field_type(schema, person_idx, 'name', err) == TYPE_TEXT, &
                  'dyn: field type text')

      call capnp_read_file('test/fixtures/addressbook.bin', bytes, err)
      call capnp_deserialize_bytes(bytes, msg, err)
      root = capnp_root(msg, err)
      call check_(error, err == CAPNP_OK, 'dyn: fixture message loads')

      people = capnp_dyn_getp(schema, book_idx, root, 'people', err)
      call check_(error, err == CAPNP_OK .and. capnp_list_len(people) == 2_int64, &
                  'dyn: people list by name')

      person = capnp_list_get_struct(people, 0, err)
      call check_(error, capnp_dyn_get_int(schema, person_idx, person, 'id', err) == 123_int64, &
                  'dyn: alice id by name')
      call capnp_dyn_get_text(schema, person_idx, person, 'name', s, err)
      call check_(error, s == 'Alice', 'dyn: alice name by name')
      call capnp_dyn_get_text(schema, person_idx, person, 'email', s, err)
      call check_(error, s == 'alice@example.com', 'dyn: alice email by name')

      ! Union which: Person reports disc_count=0 on the parent node; the
      ! employment group carries the discriminant. dyn_which must follow groups.
      pe%p = person
      want = person_employment_which(pe)
      call check_(error, want == PERSON_EMPLOYMENT_SCHOOL_TAG, &
                  'fixture: alice employment is school (generated which)')
      tag = capnp_dyn_which(schema, person_idx, person, err)
      call check_(error, err == CAPNP_OK .and. tag == want, &
                  'dyn: which matches generated on fixture alice')
      tag = capnp_dyn_which(schema, person_idx, person, err, group='employment')
      call check_(error, err == CAPNP_OK .and. tag == want, &
                  'dyn: which group=employment matches')
      call check_(error, capnp_which(person, 2) == want, 'dyn: capnp_which(disc=2) agrees')

      phones = capnp_dyn_getp(schema, person_idx, person, 'phones', err)
      phone = capnp_list_get_struct(phones, 0, err)
      call capnp_dyn_get_text(schema, phone_idx, phone, 'number', s, err)
      call check_(error, s == '555-1212', 'dyn: phone number by name')
      call check_(error, capnp_dyn_get_int(schema, phone_idx, phone, 'type', err) == 0_int64, &
                  'dyn: phone type enum by name')

      q = capnp_dyn_getp(schema, person_idx, person, 'nonexistent', err)
      call check_(error, err /= CAPNP_OK, 'dyn: unknown field errors')

      call capnp_message_init_builder(bmsg, err)
      person = capnp_new_struct(bmsg, 1, 4, err)
      call capnp_set_root(bmsg, person, err)
      call capnp_dyn_set_int(schema, person_idx, person, 'id', 777_int64, err)
      call check_(error, err == CAPNP_OK, 'dyn: set int by name')
      call capnp_dyn_set_text(schema, person_idx, person, 'name', 'Dyn', err)
      call check_(error, err == CAPNP_OK, 'dyn: set text by name')
      call check_(error, capnp_dyn_get_int(schema, person_idx, person, 'id', err) == 777_int64, &
                  'dyn: write/read int round trip')
      call capnp_dyn_get_text(schema, person_idx, person, 'name', s, err)
      call check_(error, s == 'Dyn', 'dyn: write/read text round trip')

      call capnp_read_file('test/fixtures/kitchen.cgr.bin', bytes, err)
      call check_(error, err == CAPNP_OK, 'dyn: kitchen cgr reads')
      call capnp_dyn_load(kschema, bytes, err)
      call check_(error, err == CAPNP_OK, 'dyn: kitchen schema loads')
      sink_idx = capnp_dyn_find(kschema, 'Sink', err)
      call check_(error, sink_idx > 0, 'dyn: Sink node found')
      call capnp_message_init_builder(kmsg, err)
      sink = sink_new_root(kmsg, err)
      call sink_flag_set(sink, .false., err)
      call sink_ratio_set(sink, 3.5_real64, err)
      call check_(error, err == CAPNP_OK, 'dyn: kitchen seed ok')
      flag = capnp_dyn_get_bool(kschema, sink_idx, sink%p, 'flag', err)
      call check_(error, err == CAPNP_OK .and. .not. flag, 'dyn: get_bool flag')
      ! Default-true seed path: flip back to true and re-read.
      call sink_flag_set(sink, .true., err)
      flag = capnp_dyn_get_bool(kschema, sink_idx, sink%p, 'flag', err)
      call check_(error, err == CAPNP_OK .and. flag, 'dyn: get_bool true')
      ratio = capnp_dyn_get_f64(kschema, sink_idx, sink%p, 'ratio', err)
      call check_(error, err == CAPNP_OK .and. abs(ratio - 3.5_real64) < 1.0e-15_real64, &
                  'dyn: get_f64 ratio')

      ! Multi-union Dual: set *divergent* tags (primary voidA=0, secondary
      ! intB=1) so a first-union-only dyn_which cannot satisfy both named
      ! asserts. Ambiguous which without group= must error.
      call capnp_read_file('test/fixtures/dual.cgr.bin', bytes, err)
      call check_(error, err == CAPNP_OK, 'dyn: dual cgr reads')
      call capnp_dyn_load(dschema, bytes, err)
      call check_(error, err == CAPNP_OK, 'dyn: dual schema loads')
      dual_idx = capnp_dyn_find(dschema, 'Dual', err)
      call check_(error, dual_idx > 0, 'dyn: Dual node found')
      call capnp_message_init_builder(dmsg, err)
      dual = dual_new_root(dmsg, err)
      call dual_primary_void_a_set(dual, err)
      call dual_secondary_int_b_set(dual, 17_int32, err)
      call check_(error, err == CAPNP_OK, 'dyn: dual unions set')
      tag_pri = dual_primary_which(dual)
      tag_sec = dual_secondary_which(dual)
      call check_(error, tag_pri == DUAL_PRIMARY_VOID_A_TAG, 'dyn: generated primary is voidA/0')
      call check_(error, tag_sec == DUAL_SECONDARY_INT_B_TAG, 'dyn: generated secondary is intB/1')
      call check_(error, tag_pri /= tag_sec, 'dyn: dual tags are divergent (not theater)')
      tag = capnp_dyn_which(dschema, dual_idx, dual%p, err)
      call check_(error, err == CAPNP_ERR_ARG, 'dyn: ambiguous multi-union without group')
      tag = capnp_dyn_which(dschema, dual_idx, dual%p, err, group='primary')
      call check_(error, err == CAPNP_OK .and. tag == tag_pri .and. tag == 0, &
                  'dyn: primary group which is 0 (voidA)')
      tag = capnp_dyn_which(dschema, dual_idx, dual%p, err, group='secondary')
      call check_(error, err == CAPNP_OK .and. tag == tag_sec .and. tag == 1, &
                  'dyn: secondary group which is 1 (intB)')
      ! Flip primary to textA (1) and secondary to voidB (0); re-assert swap.
      call dual_primary_text_a_set(dual, 'alpha', err)
      call dual_secondary_void_b_set(dual, err)
      call check_(error, err == CAPNP_OK, 'dyn: dual unions flipped')
      tag_pri = dual_primary_which(dual)
      tag_sec = dual_secondary_which(dual)
      call check_(error, tag_pri == DUAL_PRIMARY_TEXT_A_TAG .and. tag_sec == DUAL_SECONDARY_VOID_B_TAG, &
                  'dyn: flipped generated tags 1 and 0')
      call check_(error, tag_pri /= tag_sec, 'dyn: flipped tags still divergent')
      tag = capnp_dyn_which(dschema, dual_idx, dual%p, err, group='primary')
      call check_(error, err == CAPNP_OK .and. tag == 1, 'dyn: primary group which is 1 (textA)')
      tag = capnp_dyn_which(dschema, dual_idx, dual%p, err, group='secondary')
      call check_(error, err == CAPNP_OK .and. tag == 0, 'dyn: secondary group which is 0 (voidB)')

      call capnp_message_free(msg)
      call capnp_message_free(bmsg)
      call capnp_message_free(kmsg)
      call capnp_message_free(dmsg)
      call capnp_dyn_free(schema)
      call capnp_dyn_free(kschema)
      call capnp_dyn_free(dschema)
   end subroutine run_dynamic

end module test_dynamic
