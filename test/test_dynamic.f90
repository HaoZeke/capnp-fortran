!> Dynamic reflection: load compiled schemas at runtime and read/write
!> fields by name -- no generated accessors on the read path for
!> addressbook; kitchen is used only to seed bool/f64 wire values that
!> capnp_dyn_get_bool / capnp_dyn_get_f64 then re-read by name.
module test_dynamic
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed, skip_test
   use capnp
   use capnp_dynamic
   use capnp_schema, only: TYPE_TEXT, type_which, value_which, value_text
   use kitchen_capnp, only: sink_t, sink_new_root, SCHEMA__A826_NOTE_ID
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
      testsuite = [new_unittest("dynamic", run_dynamic), &
                   new_unittest("dynamic-default-isolation", run_default_isolation)]
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
      type(capnp_dyn_schema_t), target :: schema, kschema, dschema, aschema, bschema
      type(capnp_dyn_brand_t) :: brand
      type(capnp_message_t), target :: msg, bmsg, kmsg, dmsg, amsg
      type(capnp_ptr_t) :: root, people, person, phones, phone, q
      type(capnp_dyn_annotation_t), allocatable :: annotations(:)
      type(sink_t) :: sink
      type(person_t) :: pe
      type(dual_t) :: dual
      integer(int8), allocatable :: bytes(:), data(:)
      character(len=:), allocatable :: s
      integer :: err, book_idx, person_idx, phone_idx, sink_idx, vec_idx
      integer :: dual_idx, dynamic_idx, choice_idx, service_idx, box_idx
      integer :: tag, want
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
      vec_idx = capnp_dyn_find(kschema, 'Vec3', err)
      call check_(error, sink_idx > 0 .and. vec_idx > 0, 'dyn: Sink and Vec3 nodes found')
      call capnp_dyn_node_annotations(kschema, sink_idx, annotations, err)
      call check_(error, err == CAPNP_OK .and. size(annotations) == 1 .and. &
                  annotations(1)%id == SCHEMA__A826_NOTE_ID .and. &
                  value_which(annotations(1)%value) == TYPE_TEXT, &
                  'dyn: node annotation metadata')
      call value_text(annotations(1)%value, s, err)
      call check_(error, err == CAPNP_OK .and. s == 'sink type', 'dyn: node annotation value')
      call capnp_dyn_field_annotations(kschema, sink_idx, 'flag', annotations, err)
      call check_(error, err == CAPNP_OK .and. size(annotations) == 1 .and. &
                  annotations(1)%id == SCHEMA__A826_NOTE_ID .and. &
                  value_which(annotations(1)%value) == TYPE_TEXT, &
                  'dyn: field annotation metadata')
      call value_text(annotations(1)%value, s, err)
      call check_(error, err == CAPNP_OK .and. s == 'flag field', 'dyn: field annotation value')
      call capnp_message_init_builder(kmsg, err)
      sink = sink_new_root(kmsg, err)
      q = capnp_dyn_getp(kschema, sink_idx, sink%p, 'origin', err)
      call check_(error, err == CAPNP_OK .and. q%kind == CAPNP_PK_STRUCT .and. &
                  abs(capnp_dyn_get_f64(kschema, vec_idx, q, 'x', err) - 1.5_real64) < 1.0e-15_real64, &
                  'dyn: struct pointer default')
      q = capnp_dyn_getp(kschema, sink_idx, sink%p, 'scores', err)
      call check_(error, err == CAPNP_OK .and. q%kind == CAPNP_PK_LIST .and. &
                  capnp_list_len(q) == 3_int64 .and. capnp_list_get_i64(q, 2_int64, err) == 4_int64, &
                  'dyn: list pointer default')
      q = capnp_new_struct(kmsg, 0, 0, err)
      call check_(error, err == CAPNP_OK .and. &
                  capnp_dyn_get_int(kschema, sink_idx, q, 'count', err) == -7_int64, &
                  'dyn: scalar schema evolution default')
      call capnp_dyn_get_text(kschema, sink_idx, q, 'label', s, err)
      call check_(error, err == CAPNP_OK .and. s == 'unnamed', 'dyn: text pointer default')
      call capnp_dyn_get_data(kschema, sink_idx, q, 'payload', data, err)
      call check_(error, err == CAPNP_OK .and. size(data) == 4 .and. &
                  data(lbound(data, 1)) == -34_int8 .and. data(ubound(data, 1)) == -17_int8, &
                  'dyn: data pointer default')
      q = capnp_dyn_getp(kschema, sink_idx, q, 'origin', err)
      call check_(error, err == CAPNP_OK .and. q%kind == CAPNP_PK_STRUCT .and. &
                  abs(capnp_dyn_get_f64(kschema, vec_idx, q, 'z', err) + 3.5_real64) < 1.0e-15_real64, &
                  'dyn: pointer schema evolution default')
      q = capnp_dyn_getp(kschema, sink_idx, sink%p, 'count', err)
      call check_(error, err == CAPNP_ERR_KIND, 'dyn: getp rejects scalar field')
      call capnp_dyn_set_bool(kschema, sink_idx, sink%p, 'flag', .false., err)
      call capnp_dyn_set_f64(kschema, sink_idx, sink%p, 'ratio', 3.5_real64, err)
      data = [-34_int8, -83_int8, -66_int8]
      call capnp_dyn_set_data(kschema, sink_idx, sink%p, 'payload', data, err)
      call check_(error, err == CAPNP_OK, 'dyn: kitchen values set')
      flag = capnp_dyn_get_bool(kschema, sink_idx, sink%p, 'flag', err)
      call check_(error, err == CAPNP_OK .and. .not. flag, 'dyn: get_bool flag')
      ! Default-true path: flip back to true and re-read.
      call capnp_dyn_set_bool(kschema, sink_idx, sink%p, 'flag', .true., err)
      flag = capnp_dyn_get_bool(kschema, sink_idx, sink%p, 'flag', err)
      call check_(error, err == CAPNP_OK .and. flag, 'dyn: get_bool true')
      ratio = capnp_dyn_get_f64(kschema, sink_idx, sink%p, 'ratio', err)
      call check_(error, err == CAPNP_OK .and. abs(ratio - 3.5_real64) < 1.0e-15_real64, &
                  'dyn: get_f64 ratio')
      call capnp_dyn_get_data(kschema, sink_idx, sink%p, 'payload', data, err)
      call check_(error, err == CAPNP_OK .and. size(data) == 3 .and. &
                  data(lbound(data, 1)) == -34_int8 .and. data(ubound(data, 1)) == -66_int8, &
                  'dyn: get_data payload')
      q = capnp_dyn_init_struct(kschema, sink_idx, sink%p, 'origin', err)
      call capnp_dyn_set_f64(kschema, vec_idx, q, 'x', 9.5_real64, err)
      call check_(error, err == CAPNP_OK .and. &
                  abs(capnp_dyn_get_f64(kschema, vec_idx, q, 'x', err) - 9.5_real64) < 1.0e-15_real64, &
                  'dyn: init_struct and write nested value')
      q = capnp_dyn_init_list(kschema, sink_idx, sink%p, 'tags', 2_int64, err)
      call capnp_list_set_text(q, 0, 'left', err)
      call capnp_list_set_text(q, 1, 'right', err)
      call capnp_dyn_setp(kschema, sink_idx, sink%p, 'tags', q, err)
      q = capnp_dyn_getp(kschema, sink_idx, sink%p, 'tags', err)
      call check_(error, err == CAPNP_OK .and. capnp_list_len(q) == 2_int64, &
                  'dyn: init_list and setp')

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

      call capnp_read_file('test/fixtures/dynamic.cgr.bin', bytes, err)
      call check_(error, err == CAPNP_OK, 'dyn: dynamic cgr reads')
      call capnp_dyn_load(aschema, bytes, err)
      dynamic_idx = capnp_dyn_find(aschema, 'Dynamic', err)
      choice_idx = capnp_dyn_find(aschema, 'Dynamic.choice', err)
      service_idx = capnp_dyn_find(aschema, 'AnnotatedService', err)
      call check_(error, err == CAPNP_OK .and. dynamic_idx > 0 .and. choice_idx > 0 .and. service_idx > 0, &
                  'dyn: Dynamic, Choice, and AnnotatedService nodes found')
      call capnp_dyn_method_annotations(aschema, service_idx, 'ping', annotations, err)
      call check_(error, err == CAPNP_OK .and. size(annotations) == 1 .and. &
                  value_which(annotations(1)%value) == TYPE_TEXT, 'dyn: method annotation metadata')
      call value_text(annotations(1)%value, s, err)
      call check_(error, err == CAPNP_OK .and. s == 'ping method', 'dyn: method annotation value')
      call capnp_message_init_builder(amsg, err)
      q = capnp_dyn_new_root(aschema, dynamic_idx, amsg, err)
      call capnp_dyn_set_capability(aschema, dynamic_idx, q, 'capability', 4294967296_int64, err)
      call check_(error, err == CAPNP_ERR_ARG, 'dyn: capability index range check')
      call capnp_dyn_set_capability(aschema, dynamic_idx, q, 'capability', 41_int64, err)
      call check_(error, err == CAPNP_OK .and. &
                  capnp_dyn_get_capability(aschema, dynamic_idx, q, 'capability', err) == 41_int64, &
                  'dyn: capability index round trip')
      people = capnp_new_struct(amsg, 0, 0, err)
      call capnp_dyn_setp(aschema, dynamic_idx, q, 'anyStruct', people, err)
      people = capnp_dyn_get_any_struct(aschema, dynamic_idx, q, 'anyStruct', err)
      call check_(error, err == CAPNP_OK .and. people%kind == CAPNP_PK_STRUCT, 'dyn: AnyStruct')
      root = capnp_dyn_getp(aschema, dynamic_idx, q, 'choice', err)
      call capnp_dyn_setp(aschema, choice_idx, root, 'items', people, err)
      call check_(error, err == CAPNP_ERR_KIND, 'dyn: setp rejects wrong list kind')
      people = capnp_new_list(amsg, CAPNP_SZ_FOUR, 1_int64, err)
      call capnp_dyn_setp(aschema, dynamic_idx, q, 'anyList', people, err)
      people = capnp_dyn_get_any_list(aschema, dynamic_idx, q, 'anyList', err)
      call check_(error, err == CAPNP_OK .and. people%kind == CAPNP_PK_LIST, 'dyn: AnyList')
      people = capnp_dyn_getp(aschema, dynamic_idx, q, 'choice', err)
      call capnp_dyn_set_int(aschema, choice_idx, people, 'number', 73_int64, err)
      call check_(error, err == CAPNP_OK, 'dyn: integer union setter runs')
      tag = capnp_dyn_which(aschema, choice_idx, people, err)
      call check_(error, err == CAPNP_OK .and. tag == 0, 'dyn: integer setter selects union')
      call capnp_dyn_select(aschema, choice_idx, people, 'label', err)
      call capnp_dyn_set_text(aschema, choice_idx, people, 'label', 'selected', err)
      call capnp_dyn_get_text(aschema, choice_idx, people, 'label', s, err)
      call check_(error, err == CAPNP_OK .and. s == 'selected' .and. &
                  capnp_dyn_which(aschema, choice_idx, people, err) == 1, 'dyn: select and text union')
      tag = capnp_dyn_get_int(aschema, choice_idx, people, 'number', err)
      call check_(error, err == CAPNP_OK .and. capnp_dyn_which(aschema, choice_idx, people, err) == 1, &
                  'dyn: integer read does not select union')
      q = capnp_dyn_init_list(aschema, choice_idx, people, 'items', 2_int64, err)
      call check_(error, err == CAPNP_OK .and. capnp_list_len(q) == 2_int64 .and. &
                  capnp_dyn_which(aschema, choice_idx, people, err) == 2, &
                  'dyn: init list selects union')
      call capnp_dyn_set_int(aschema, choice_idx, people, 'number', 74_int64, err)
      call check_(error, err == CAPNP_OK .and. capnp_dyn_which(aschema, choice_idx, people, err) == 0, &
                  'dyn: integer write selects union')

      call capnp_read_file('test/fixtures/box.cgr.bin', bytes, err)
      call check_(error, err == CAPNP_OK, 'dyn: box cgr reads')
      call capnp_dyn_load(bschema, bytes, err)
      box_idx = capnp_dyn_find(bschema, 'BoxUse', err)
      q = capnp_dyn_field_schema(bschema, box_idx, 'textBox', err)
      call capnp_dyn_load_brand(q, brand, err)
      call check_(error, err == CAPNP_OK .and. size(brand%scopes) > 0 .and. &
                  size(brand%scopes(1)%bindings) > 0 .and. &
                  type_which(brand%scopes(1)%bindings(1)%type) == TYPE_TEXT, &
                  'dyn: arbitrary text brand binding')

      call capnp_message_free(msg)
      call capnp_message_free(bmsg)
      call capnp_message_free(kmsg)
      call capnp_message_free(dmsg)
      call capnp_message_free(amsg)
      call capnp_dyn_free(schema)
      call capnp_dyn_free(kschema)
      call capnp_dyn_free(dschema)
      call capnp_dyn_free(aschema)
      call capnp_dyn_free(bschema)
   end subroutine run_dynamic

   !> Writing through a pointer default must not change the schema object.
   subroutine run_default_isolation(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: a, b
      type(sink_t) :: sink_a, sink_b
      type(capnp_ptr_t) :: origin
      integer(int8), allocatable :: bytes(:)
      integer :: err, sink_idx, vec_idx
      real(real64) :: x

      call capnp_read_file('test/fixtures/kitchen.cgr.bin', bytes, err)
      if (err /= CAPNP_OK) then
         call skip_test(error, 'fixtures not found (run from project root)')
         return
      end if
      call capnp_dyn_load(schema, bytes, err)
      call check_(error, err == CAPNP_OK, 'default: kitchen schema loads')
      sink_idx = capnp_dyn_find(schema, 'Sink', err)
      vec_idx = capnp_dyn_find(schema, 'Vec3', err)
      call check_(error, sink_idx > 0 .and. vec_idx > 0, 'default: nodes found')

      call capnp_message_init_builder(a, err)
      sink_a = sink_new_root(a, err)
      origin = capnp_dyn_getp(schema, sink_idx, sink_a%p, 'origin', err)
      call check_(error, err == CAPNP_OK .and. origin%kind == CAPNP_PK_STRUCT, &
                  'default: origin handle')
      call capnp_dyn_set_f64(schema, vec_idx, origin, 'x', 42.0_real64, err)
      call check_(error, err == CAPNP_OK, 'default: write through origin')
      x = capnp_dyn_get_f64(schema, vec_idx, origin, 'x', err)
      call check_(error, err == CAPNP_OK .and. abs(x - 42.0_real64) < 1.0e-15_real64, &
                  'default: write stays on the first struct')

      call capnp_message_init_builder(b, err)
      sink_b = sink_new_root(b, err)
      origin = capnp_dyn_getp(schema, sink_idx, sink_b%p, 'origin', err)
      x = capnp_dyn_get_f64(schema, vec_idx, origin, 'x', err)
      call check_(error, err == CAPNP_OK .and. abs(x - 1.5_real64) < 1.0e-15_real64, &
                  'default: schema origin x stays 1.5')

      call capnp_message_free(a)
      call capnp_message_free(b)
      call capnp_dyn_free(schema)
   end subroutine run_default_isolation

end module test_dynamic
