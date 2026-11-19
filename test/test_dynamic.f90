!> Dynamic reflection: load compiled schemas at runtime and read/write
!> fields by name -- no generated accessors on the read path for
!> addressbook; kitchen is used only to seed bool/f64 wire values that
!> capnp_dyn_get_bool / capnp_dyn_get_f64 then re-read by name.
module test_dynamic
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed, skip_test
   use capnp
   use capnp_dynamic
   use capnp_schema, only: TYPE_TEXT, TYPE_DATA, TYPE_LIST, TYPE_VOID, &
                                TYPE_STRUCT, TYPE_ANY_POINTER, type_which, &
                                type_list_element, type_type_id, type_anyptr_which, &
                                type_param_scope_id, ANYPTR_PARAMETER, &
                                value_which, value_text, node_id, &
                                node_struct_discriminant_offset
   use kitchen_capnp, only: sink_t, sink_new_root, SCHEMA__KITCHEN_CAPNP_NOTE_ANNOTATION_ID
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
                   new_unittest("dynamic-default-isolation", run_default_isolation), &
                   new_unittest("dynamic-malformed-text", run_malformed_text), &
                   new_unittest("dynamic-unloaded-schema", run_unloaded_schema), &
                   new_unittest("dynamic-init-null", run_init_null), &
                   new_unittest("dynamic-assign-types", run_assign_types), &
                   new_unittest("dynamic-group-union", run_group_union), &
                   new_unittest("dynamic-union-clear", run_union_clear), &
                   new_unittest("dynamic-scalars", run_scalars), &
                   new_unittest("dynamic-two-hop-brand", run_two_hop_brand), &
                   new_unittest("dynamic-reader-default", run_reader_default), &
                   new_unittest("dynamic-unset-which", run_unset_which), &
                   new_unittest("dynamic-unbound-anypointer", run_unbound_anypointer), &
                   new_unittest("dynamic-default-arena", run_default_arena), &
                   new_unittest("dynamic-schema-inout", run_schema_inout)]
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
      integer :: scalar_idx, item_idx, tw
      integer(int64) :: bound
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
                  annotations(1)%id == SCHEMA__KITCHEN_CAPNP_NOTE_ANNOTATION_ID .and. &
                  value_which(annotations(1)%value) == TYPE_TEXT, &
                  'dyn: node annotation metadata')
      call value_text(annotations(1)%value, s, err)
      call check_(error, err == CAPNP_OK .and. s == 'sink type', 'dyn: node annotation value')
      call capnp_dyn_field_annotations(kschema, sink_idx, 'flag', annotations, err)
      call check_(error, err == CAPNP_OK .and. size(annotations) == 1 .and. &
                  annotations(1)%id == SCHEMA__KITCHEN_CAPNP_NOTE_ANNOTATION_ID .and. &
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
      tag = capnp_dyn_which(aschema, 0, q, err)
      call check_(error, err == CAPNP_ERR_ARG .and. tag == -1, 'dyn: which rejects index 0')
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
      call capnp_dyn_setp(aschema, dynamic_idx, q, 'anyStruct', people, err)
      call check_(error, err == CAPNP_ERR_KIND, 'dyn: AnyStruct rejects a list')
      root = capnp_new_struct(amsg, 0, 0, err)
      call capnp_dyn_setp(aschema, dynamic_idx, q, 'anyList', root, err)
      call check_(error, err == CAPNP_ERR_KIND, 'dyn: AnyList rejects a struct')
      call capnp_dyn_setp(aschema, dynamic_idx, q, 'capability', root, err)
      call check_(error, err == CAPNP_ERR_KIND, 'dyn: Capability rejects a struct')
      root = capnp_dyn_get_any_struct(aschema, dynamic_idx, q, 'anyList', err)
      call check_(error, err == CAPNP_ERR_KIND, 'dyn: get_any_struct rejects AnyList')
      root = capnp_dyn_get_any_list(aschema, dynamic_idx, q, 'anyStruct', err)
      call check_(error, err == CAPNP_ERR_KIND, 'dyn: get_any_list rejects AnyStruct')
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
      call check_(error, err == CAPNP_ERR_KIND, 'dyn: inactive union read is rejected')
      tag = capnp_dyn_which(aschema, choice_idx, people, err)
      call check_(error, err == CAPNP_OK .and. tag == 1, &
                  'dyn: inactive read leaves the union on label')
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
      call check_(error, brand%scopes(1)%bindings(1)%resolved_which == TYPE_TEXT .and. &
                  brand%scopes(1)%bindings(1)%resolved_id == 0_int64, &
                  'dyn: resolved text brand')
      box_idx = capnp_dyn_find(bschema, 'QUse', err)
      q = capnp_dyn_field_schema(bschema, box_idx, 'q', err)
      call capnp_dyn_load_brand(q, brand, err)
      call check_(error, err == CAPNP_OK .and. size(brand%scopes) == 1 .and. &
                  size(brand%scopes(1)%bindings) == 5, 'dyn: five brand bindings')
      call check_(error, type_which(brand%scopes(1)%bindings(5)%type) == TYPE_DATA .and. &
                  brand%scopes(1)%bindings(5)%resolved_which == TYPE_DATA, &
                  'dyn: fifth binding resolves to data')
      box_idx = capnp_dyn_find(bschema, 'BoxUse', err)
      q = capnp_dyn_field_schema(bschema, box_idx, 'listBox', err)
      call capnp_dyn_load_brand(q, brand, err)
      call check_(error, err == CAPNP_OK .and. &
                  type_which(brand%scopes(1)%bindings(1)%type) == TYPE_LIST .and. &
                  brand%scopes(1)%bindings(1)%resolved_which == TYPE_LIST, &
                  'dyn: list brand resolves beside the raw type')
      scalar_idx = capnp_dyn_find(aschema, 'Scalars', err)
      q = capnp_dyn_field_schema(aschema, scalar_idx, 'note', err)
      call capnp_dyn_load_brand(q, brand, err)
      call check_(error, err == CAPNP_ERR_KIND, 'dyn: text type has no brand')
      box_idx = capnp_dyn_find(bschema, 'Box', err)
      q = capnp_dyn_field_schema(bschema, capnp_dyn_find(bschema, 'BoxUse', err), 'textBox', err)
      call capnp_dyn_load_brand(q, brand, err)
      call check_(error, capnp_dyn_field_type(bschema, box_idx, 'value', err, brand=brand) == &
                  TYPE_TEXT, 'dyn: field type follows the text brand')
      item_idx = capnp_dyn_find(bschema, 'Item', err)
      q = capnp_dyn_field_schema(bschema, capnp_dyn_find(bschema, 'BagUse', err), 'bag', err)
      call capnp_dyn_load_brand(q, brand, err)
      tw = capnp_dyn_field_type(bschema, box_idx, 'value', err, brand=brand, resolved_id=bound)
      call check_(error, err == CAPNP_OK .and. tw == TYPE_LIST .and. item_idx > 0 .and. &
                  bound == node_id(bschema%nodes(item_idx)), &
                  'dyn: list element brand is a struct')

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

   !> Text whose last byte is not NUL is malformed, not a truncated string.
   subroutine run_malformed_text(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: person, raw
      integer(int8), allocatable :: bytes(:)
      character(len=:), allocatable :: s
      integer :: err, person_idx

      call capnp_message_init_builder(msg, err)
      person = capnp_new_struct(msg, 0, 1, err)
      raw = capnp_new_list(msg, CAPNP_SZ_BYTE, 2_int64, err)
      raw%msg%segs(raw%seg)%bytes(raw%off) = int(iachar('a'), int8)
      raw%msg%segs(raw%seg)%bytes(raw%off + 1_int64) = int(iachar('b'), int8)
      call capnp_setp(person, 0, raw, err)
      call capnp_get_text(person, 0, s, err)
      call check_(error, err == CAPNP_ERR_KIND, 'text: missing NUL is rejected')

      raw = capnp_new_list(msg, CAPNP_SZ_BYTE, 1_int64, err)
      raw%msg%segs(raw%seg)%bytes(raw%off) = int(iachar('Z'), int8)
      call capnp_setp(person, 0, raw, err)
      call capnp_get_text(person, 0, s, err)
      call check_(error, err == CAPNP_ERR_KIND, 'text: lone non-NUL byte is rejected')

      call capnp_read_file('test/fixtures/addressbook.cgr.bin', bytes, err)
      if (err /= CAPNP_OK) then
         call capnp_message_free(msg)
         call skip_test(error, 'fixtures not found (run from project root)')
         return
      end if
      call capnp_dyn_load(schema, bytes, err)
      person_idx = capnp_dyn_find(schema, 'Person', err)
      person = capnp_new_struct(msg, 1, 4, err)
      raw = capnp_new_list(msg, CAPNP_SZ_BYTE, 2_int64, err)
      raw%msg%segs(raw%seg)%bytes(raw%off) = int(iachar('a'), int8)
      raw%msg%segs(raw%seg)%bytes(raw%off + 1_int64) = int(iachar('b'), int8)
      call capnp_dyn_setp(schema, person_idx, person, 'name', raw, err)
      call check_(error, err == CAPNP_OK, 'text: byte list can be stored')
      call capnp_dyn_get_text(schema, person_idx, person, 'name', s, err)
      call check_(error, err == CAPNP_ERR_KIND, 'text: dynamic read rejects missing NUL')

      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end subroutine run_malformed_text

   !> An unloaded schema is rejected before any node is indexed.
   subroutine run_unloaded_schema(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t) :: schema
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: p
      type(capnp_dyn_annotation_t), allocatable :: ann(:)
      integer :: err, tag

      call capnp_message_init_builder(msg, err)
      p = capnp_dyn_new_root(schema, 1, msg, err)
      call check_(error, err == CAPNP_ERR_ARG, 'unloaded: new_root')
      p = capnp_dyn_new_root(schema, 0, msg, err)
      call check_(error, err == CAPNP_ERR_ARG, 'unloaded: new_root index 0')
      call capnp_dyn_node_annotations(schema, 1, ann, err)
      call check_(error, err == CAPNP_ERR_ARG .and. size(ann) == 0, &
                  'unloaded: node annotations')
      call capnp_dyn_enumerant_annotations(schema, 1, 'mobile', ann, err)
      call check_(error, err == CAPNP_ERR_ARG, 'unloaded: enumerant annotations')
      call capnp_dyn_method_annotations(schema, 1, 'ping', ann, err)
      call check_(error, err == CAPNP_ERR_ARG, 'unloaded: method annotations')
      tag = capnp_dyn_which(schema, 1, p, err)
      call check_(error, err == CAPNP_ERR_ARG .and. tag == -1, 'unloaded: which')
      call capnp_message_free(msg)
   end subroutine run_unloaded_schema

   !> init on a pointer with no message reports an error.
   subroutine run_init_null(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_ptr_t) :: p, q
      integer(int8), allocatable :: bytes(:)
      integer :: err, sink_idx

      call capnp_read_file('test/fixtures/kitchen.cgr.bin', bytes, err)
      if (err /= CAPNP_OK) then
         call skip_test(error, 'fixtures not found (run from project root)')
         return
      end if
      call capnp_dyn_load(schema, bytes, err)
      sink_idx = capnp_dyn_find(schema, 'Sink', err)
      call check_(error, err == CAPNP_OK .and. sink_idx > 0, 'init-null: sink node')
      p = capnp_ptr_t()
      q = capnp_dyn_init_struct(schema, sink_idx, p, 'origin', err)
      call check_(error, err == CAPNP_ERR_ARG .and. q%kind == CAPNP_PK_NULL, &
                  'init-null: struct')
      q = capnp_dyn_init_list(schema, sink_idx, p, 'tags', 1_int64, err)
      call check_(error, err == CAPNP_ERR_ARG .and. q%kind == CAPNP_PK_NULL, &
                  'init-null: pointer list')
      q = capnp_dyn_init_list(schema, sink_idx, p, 'scores', 2_int64, err)
      call check_(error, err == CAPNP_ERR_ARG .and. q%kind == CAPNP_PK_NULL, &
                  'init-null: primitive list')
      call capnp_dyn_free(schema)
   end subroutine run_init_null

   !> setp rejects a struct of the wrong schema and a list of the wrong
   !> element type, and accepts a matching struct and list.
   subroutine run_assign_types(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: sink, other, wrong, right, forged, wide, narrow
      type(capnp_ptr_t) :: spots, bad_spots
      integer(int8), allocatable :: bytes(:)
      integer :: err, sink_idx

      call capnp_read_file('test/fixtures/kitchen.cgr.bin', bytes, err)
      if (err /= CAPNP_OK) then
         call skip_test(error, 'fixtures not found (run from project root)')
         return
      end if
      call capnp_dyn_load(schema, bytes, err)
      sink_idx = capnp_dyn_find(schema, 'Sink', err)
      call capnp_message_init_builder(msg, err)
      sink = capnp_dyn_new_root(schema, sink_idx, msg, err)
      call check_(error, err == CAPNP_OK, 'assign: sink root')

      wrong = capnp_new_struct(msg, 1, 0, err)
      call capnp_dyn_setp(schema, sink_idx, sink, 'origin', wrong, err)
      call check_(error, err == CAPNP_ERR_KIND, 'assign: struct shape')

      right = capnp_new_struct(msg, 3, 0, err)
      call capnp_dyn_setp(schema, sink_idx, sink, 'origin', right, err)
      call check_(error, err == CAPNP_OK, 'assign: matching struct shape')

      forged = capnp_new_struct(msg, 3, 0, err)
      forged%schema_id = 99_int64
      call capnp_dyn_setp(schema, sink_idx, sink, 'origin', forged, err)
      call check_(error, err == CAPNP_ERR_KIND, 'assign: struct schema id')

      other = capnp_dyn_new_root(schema, sink_idx, msg, err)
      right = capnp_dyn_init_struct(schema, sink_idx, other, 'origin', err)
      call capnp_dyn_setp(schema, sink_idx, sink, 'origin', right, err)
      call check_(error, err == CAPNP_OK .and. right%schema_id /= 0_int64, &
                  'assign: stamped struct schema id')

      wide = capnp_new_list(msg, CAPNP_SZ_EIGHT, 1_int64, err)
      call capnp_dyn_setp(schema, sink_idx, sink, 'scores', wide, err)
      call check_(error, err == CAPNP_OK, 'assign: int64 list')
      narrow = capnp_new_list(msg, CAPNP_SZ_FOUR, 1_int64, err)
      call capnp_dyn_setp(schema, sink_idx, sink, 'scores', narrow, err)
      call check_(error, err == CAPNP_ERR_KIND, 'assign: list element width')

      spots = capnp_new_composite_list(msg, 1_int64, 3, 0, err)
      call capnp_dyn_setp(schema, sink_idx, sink, 'spots', spots, err)
      call check_(error, err == CAPNP_OK, 'assign: composite element layout')
      bad_spots = capnp_new_composite_list(msg, 1_int64, 1, 0, err)
      call capnp_dyn_setp(schema, sink_idx, sink, 'spots', bad_spots, err)
      call check_(error, err == CAPNP_ERR_KIND, 'assign: composite element mismatch')

      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end subroutine run_assign_types

   !> Selecting a group arm sets the parent discriminant.
   subroutine run_group_union(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: shape
      integer(int8), allocatable :: bytes(:)
      integer :: err, shape_idx, tag

      call capnp_read_file('test/fixtures/dynamic.cgr.bin', bytes, err)
      if (err /= CAPNP_OK) then
         call skip_test(error, 'fixtures not found (run from project root)')
         return
      end if
      call capnp_dyn_load(schema, bytes, err)
      shape_idx = capnp_dyn_find(schema, 'Shape', err)
      call check_(error, err == CAPNP_OK .and. shape_idx > 0, 'group: shape node')
      call capnp_message_init_builder(msg, err)
      shape = capnp_dyn_new_root(schema, shape_idx, msg, err)
      call check_(error, err == CAPNP_OK, 'group: root')
      tag = capnp_dyn_which(schema, shape_idx, shape, err)
      call check_(error, err == CAPNP_OK .and. tag == 0, 'group: default arm is circle')
      call capnp_dyn_select(schema, shape_idx, shape, 'square', err)
      call check_(error, err == CAPNP_OK, 'group: select square')
      tag = capnp_dyn_which(schema, shape_idx, shape, err)
      call check_(error, err == CAPNP_OK .and. tag == 1, 'group: parent discriminant is square')
      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end subroutine run_group_union

   !> Every scalar width, void, an unknown enumerant, and embedded NULs.
   subroutine run_scalars(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root
      integer(int8), allocatable :: bytes(:), blob(:), got(:)
      character(len=:), allocatable :: text, back
      integer :: err, idx
      integer(int64) :: n

      call capnp_read_file('test/fixtures/dynamic.cgr.bin', bytes, err)
      if (err /= CAPNP_OK) then
         call skip_test(error, 'fixtures not found (run from project root)')
         return
      end if
      call capnp_dyn_load(schema, bytes, err)
      idx = capnp_dyn_find(schema, 'Scalars', err)
      call check_(error, err == CAPNP_OK .and. idx > 0, 'scalars: node')
      call check_(error, capnp_dyn_field_type(schema, idx, 'v', err) == TYPE_VOID, &
                  'scalars: void type')
      call capnp_message_init_builder(msg, err)
      root = capnp_dyn_new_root(schema, idx, msg, err)
      call check_(error, err == CAPNP_OK, 'scalars: root')
      n = capnp_dyn_get_int(schema, idx, root, 'v', err)
      call check_(error, err == CAPNP_ERR_KIND .and. n == 0_int64, 'scalars: void is not an int')

      call capnp_dyn_set_bool(schema, idx, root, 'b', .true., err)
      call check_(error, err == CAPNP_OK .and. &
                  capnp_dyn_get_bool(schema, idx, root, 'b', err), 'scalars: bool')
      call capnp_dyn_set_int(schema, idx, root, 'i8', -5_int64, err)
      call check_(error, capnp_dyn_get_int(schema, idx, root, 'i8', err) == -5_int64, &
                  'scalars: int8')
      call capnp_dyn_set_int(schema, idx, root, 'i16', -300_int64, err)
      call check_(error, capnp_dyn_get_int(schema, idx, root, 'i16', err) == -300_int64, &
                  'scalars: int16')
      call capnp_dyn_set_int(schema, idx, root, 'i32', -70000_int64, err)
      call check_(error, capnp_dyn_get_int(schema, idx, root, 'i32', err) == -70000_int64, &
                  'scalars: int32')
      call capnp_dyn_set_int(schema, idx, root, 'i64', -5000000000_int64, err)
      call check_(error, capnp_dyn_get_int(schema, idx, root, 'i64', err) == -5000000000_int64, &
                  'scalars: int64')
      call capnp_dyn_set_int(schema, idx, root, 'u8', 200_int64, err)
      call check_(error, capnp_dyn_get_int(schema, idx, root, 'u8', err) == 200_int64, &
                  'scalars: uint8')
      call capnp_dyn_set_int(schema, idx, root, 'u16', 40000_int64, err)
      call check_(error, capnp_dyn_get_int(schema, idx, root, 'u16', err) == 40000_int64, &
                  'scalars: uint16')
      call capnp_dyn_set_int(schema, idx, root, 'u32', 3000000000_int64, err)
      call check_(error, capnp_dyn_get_int(schema, idx, root, 'u32', err) == 3000000000_int64, &
                  'scalars: uint32')
      call capnp_dyn_set_int(schema, idx, root, 'u64', 2_int64**62, err)
      call check_(error, capnp_dyn_get_int(schema, idx, root, 'u64', err) == 2_int64**62, &
                  'scalars: uint64')
      call capnp_dyn_set_f64(schema, idx, root, 'f32', 1.5_real64, err)
      call check_(error, capnp_dyn_get_f64(schema, idx, root, 'f32', err) == 1.5_real64, &
                  'scalars: float32')
      call capnp_dyn_set_f64(schema, idx, root, 'f64', -2.5_real64, err)
      call check_(error, capnp_dyn_get_f64(schema, idx, root, 'f64', err) == -2.5_real64, &
                  'scalars: float64')
      call capnp_dyn_set_int(schema, idx, root, 'hue', 9_int64, err)
      call check_(error, err == CAPNP_OK .and. &
                  capnp_dyn_get_int(schema, idx, root, 'hue', err) == 9_int64, &
                  'scalars: unknown enum')

      text = 'a'//achar(0)//'b'
      call capnp_dyn_set_text(schema, idx, root, 'note', text, err)
      call capnp_dyn_get_text(schema, idx, root, 'note', back, err)
      call check_(error, err == CAPNP_OK .and. len(back) == 3 .and. back(2:2) == achar(0), &
                  'scalars: text keeps an embedded NUL')
      allocate (blob(0:2))
      blob = [int(iachar('a'), int8), 0_int8, int(iachar('b'), int8)]
      call capnp_dyn_set_data(schema, idx, root, 'blob', blob, err)
      call capnp_dyn_get_data(schema, idx, root, 'blob', got, err)
      call check_(error, err == CAPNP_OK .and. allocated(got) .and. all(got == blob), &
                  'scalars: data keeps an embedded NUL')

      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end subroutine run_scalars

   !> Switching a union clears the previous arm, and select rejects a field
   !> that is not a union member.
   subroutine run_union_clear(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: choice, shape
      integer(int8), allocatable :: bytes(:)
      character(len=:), allocatable :: s
      integer :: err, choice_idx, scalar_idx, shape_idx, circle_idx, tag
      real(real64) :: radius

      call capnp_read_file('test/fixtures/dynamic.cgr.bin', bytes, err)
      if (err /= CAPNP_OK) then
         call skip_test(error, 'fixtures not found (run from project root)')
         return
      end if
      call capnp_dyn_load(schema, bytes, err)
      choice_idx = capnp_dyn_find(schema, 'Dynamic.choice', err)
      scalar_idx = capnp_dyn_find(schema, 'Scalars', err)
      shape_idx = capnp_dyn_find(schema, 'Shape', err)
      circle_idx = capnp_dyn_find(schema, 'Shape.circle', err)
      call check_(error, choice_idx > 0 .and. scalar_idx > 0 .and. shape_idx > 0 .and. &
                  circle_idx > 0, 'union: nodes found')

      call capnp_message_init_builder(msg, err)
      choice = capnp_dyn_new_root(schema, choice_idx, msg, err)
      call capnp_dyn_set_text(schema, choice_idx, choice, 'label', 'hello', err)
      call capnp_dyn_set_int(schema, choice_idx, choice, 'number', 4_int64, err)
      call capnp_dyn_select(schema, choice_idx, choice, 'label', err)
      call capnp_dyn_get_text(schema, choice_idx, choice, 'label', s, err)
      call check_(error, err == CAPNP_OK .and. s == '', 'union: old text arm is cleared')

      choice = capnp_dyn_new_root(schema, scalar_idx, msg, err)
      call capnp_dyn_select(schema, scalar_idx, choice, 'note', err)
      call check_(error, err == CAPNP_ERR_KIND, 'union: select rejects a non-union field')

      shape = capnp_dyn_new_root(schema, shape_idx, msg, err)
      call capnp_dyn_set_f64(schema, circle_idx, shape, 'radius', 3.5_real64, err)
      call capnp_dyn_select(schema, shape_idx, shape, 'square', err)
      tag = capnp_dyn_which(schema, shape_idx, shape, err)
      radius = capnp_dyn_get_f64(schema, circle_idx, shape, 'radius', err)
      call check_(error, err == CAPNP_OK .and. tag == 1 .and. radius == 0.0_real64, &
                  'union: group arm is cleared')

      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end subroutine run_union_clear

   !> A reader of a pointer default must not alias the schema message.
   subroutine run_reader_default(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: built, reader
      type(sink_t) :: sink
      type(capnp_ptr_t) :: root, origin, again
      integer(int8), allocatable :: bytes(:)
      integer :: err, sink_idx, vec_idx
      real(real64) :: x

      call capnp_read_file('test/fixtures/kitchen.cgr.bin', bytes, err)
      if (err /= CAPNP_OK) then
         call skip_test(error, 'fixtures not found (run from project root)')
         return
      end if
      call capnp_dyn_load(schema, bytes, err)
      sink_idx = capnp_dyn_find(schema, 'Sink', err)
      vec_idx = capnp_dyn_find(schema, 'Vec3', err)
      call check_(error, sink_idx > 0 .and. vec_idx > 0, 'reader-default: nodes')

      call capnp_message_init_builder(built, err)
      sink = sink_new_root(built, err)
      call capnp_serialize_bytes(built, bytes, err)
      call capnp_deserialize_bytes(bytes, reader, err)
      root = capnp_root(reader, err)
      origin = capnp_dyn_getp(schema, sink_idx, root, 'origin', err)
      call check_(error, err == CAPNP_OK .and. origin%kind == CAPNP_PK_STRUCT, &
                  'reader-default: view')
      call check_(error, .not. associated(origin%msg, schema%cgr), &
                  'reader-default: view is not the schema')
      x = capnp_dyn_get_f64(schema, vec_idx, origin, 'x', err)
      call check_(error, err == CAPNP_OK .and. abs(x - 1.5_real64) < 1.0e-15_real64, &
                  'reader-default: reads 1.5')
      call capnp_dyn_set_f64(schema, vec_idx, origin, 'x', 99.0_real64, err)
      call check_(error, err /= CAPNP_OK, 'reader-default: write is refused')
      again = capnp_dyn_getp(schema, sink_idx, root, 'origin', err)
      x = capnp_dyn_get_f64(schema, vec_idx, again, 'x', err)
      call check_(error, err == CAPNP_OK .and. abs(x - 1.5_real64) < 1.0e-15_real64, &
                  'reader-default: schema default stays 1.5')

      call capnp_message_free(built)
      call capnp_message_free(reader)
      call capnp_dyn_free(schema)
   end subroutine run_reader_default

   !> No active union arm is -1, for both a struct discriminant and a brand.
   subroutine run_unset_which(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg
      type(capnp_dyn_brand_t) :: brand
      type(capnp_ptr_t) :: shape, typed, raw, scopes, scope, binds, binding, bad
      integer(int8), allocatable :: bytes(:)
      integer(int64) :: off
      integer :: err, shape_idx, tag

      call capnp_read_file('test/fixtures/dynamic.cgr.bin', bytes, err)
      if (err /= CAPNP_OK) then
         call skip_test(error, 'fixtures not found (run from project root)')
         return
      end if
      call capnp_dyn_load(schema, bytes, err)
      shape_idx = capnp_dyn_find(schema, 'Shape', err)
      call check_(error, shape_idx > 0, 'unset: shape node')
      call capnp_message_init_builder(msg, err)
      shape = capnp_dyn_new_root(schema, shape_idx, msg, err)
      off = node_struct_discriminant_offset(schema%nodes(shape_idx))
      call capnp_set_u16(shape, off*2_int64, 18_int32, err)
      tag = capnp_dyn_which(schema, shape_idx, shape, err)
      call check_(error, err == CAPNP_OK .and. tag == -1, 'unset: discriminant 18 is no arm')

      typed = capnp_new_struct(msg, 3, 1, err)
      call capnp_set_u16(typed, 0_int64, 16_int32, err)
      raw = capnp_new_struct(msg, 0, 1, err)
      scopes = capnp_new_composite_list(msg, 1_int64, 2, 1, err)
      call capnp_setp(raw, 0, scopes, err)
      scope = capnp_list_get_struct(scopes, 0, err)
      call capnp_set_i64(scope, 0_int64, 42_int64, err)
      call capnp_set_u16(scope, 8_int64, 0_int32, err)
      binds = capnp_new_composite_list(msg, 1_int64, 1, 1, err)
      call capnp_setp(scope, 0, binds, err)
      binding = capnp_list_get_struct(binds, 0, err)
      call capnp_set_u16(binding, 0_int64, 1_int32, err)
      bad = capnp_new_struct(msg, 3, 1, err)
      call capnp_set_u16(bad, 0_int64, 18_int32, err)
      call capnp_set_u16(bad, 8_int64, 99_int32, err)
      call capnp_setp(binding, 0, bad, err)
      call capnp_setp(typed, 0, raw, err)
      call capnp_dyn_load_brand(typed, brand, err)
      call check_(error, err == CAPNP_OK .and. size(brand%scopes) == 1 .and. &
                  size(brand%scopes(1)%bindings) == 1 .and. &
                  brand%scopes(1)%bindings(1)%resolved_which == -1, &
                  'unset: resolved_which is not anyPointer')

      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end subroutine run_unset_which

   !> Box(parameter of Nest) plus Nest(Item). Cap'n Proto 1.4.0 resolves
   !> that walk to Item.
   subroutine run_two_hop_brand(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_dyn_brand_t) :: inner_brand, bag_brand, brand
      type(capnp_ptr_t) :: inner_t, bag_t, item_type
      integer(int8), allocatable :: bytes(:)
      integer(int64) :: nest_scope, bound
      integer :: err, box_idx, nest_idx, item_idx, bag_idx, tw
      ! Ids reported by the C++ 1.4.0 schema loader for schema/box.capnp.
      integer(int64), parameter :: cpp_item_id = -6139276977445145440_int64
      integer(int64), parameter :: cpp_nest_id = -5680941978801891149_int64

      call capnp_read_file('test/fixtures/box.cgr.bin', bytes, err)
      if (err /= CAPNP_OK) then
         call skip_test(error, 'fixtures not found (run from project root)')
         return
      end if
      call capnp_dyn_load(schema, bytes, err)
      box_idx = capnp_dyn_find(schema, 'Box', err)
      nest_idx = capnp_dyn_find(schema, 'Nest', err)
      item_idx = capnp_dyn_find(schema, 'Item', err)
      bag_idx = capnp_dyn_find(schema, 'BagUse', err)
      call check_(error, err == CAPNP_OK .and. box_idx > 0 .and. nest_idx > 0 .and. &
                  item_idx > 0 .and. bag_idx > 0 .and. &
                  node_id(schema%nodes(item_idx)) == cpp_item_id .and. &
                  node_id(schema%nodes(nest_idx)) == cpp_nest_id, &
                  'twohop: schema ids match the C++ loader')

      ! Nest.inner is Box, bound to Nest's own parameter.
      inner_t = capnp_dyn_field_schema(schema, nest_idx, 'inner', err)
      call capnp_dyn_load_brand(inner_t, inner_brand, err)
      call check_(error, err == CAPNP_OK .and. allocated(inner_brand%scopes) .and. &
                  size(inner_brand%scopes) == 1 .and. &
                  allocated(inner_brand%scopes(1)%bindings) .and. &
                  size(inner_brand%scopes(1)%bindings) == 1, &
                  'twohop: inner brand has one binding')
      if (allocated(error)) then
         call capnp_dyn_free(schema)
         return
      end if
      nest_scope = type_param_scope_id(inner_brand%scopes(1)%bindings(1)%type)
      call check_(error, type_which(inner_brand%scopes(1)%bindings(1)%type) == &
                  TYPE_ANY_POINTER .and. &
                  type_anyptr_which(inner_brand%scopes(1)%bindings(1)%type) == &
                  ANYPTR_PARAMETER .and. nest_scope == cpp_nest_id, &
                  'twohop: box binding is a parameter of Nest')

      ! BagUse.bag is Box(List(Item)); the list element is the Item type.
      bag_t = capnp_dyn_field_schema(schema, bag_idx, 'bag', err)
      call capnp_dyn_load_brand(bag_t, bag_brand, err)
      call check_(error, err == CAPNP_OK .and. allocated(bag_brand%scopes) .and. &
                  size(bag_brand%scopes) == 1 .and. &
                  allocated(bag_brand%scopes(1)%bindings) .and. &
                  type_which(bag_brand%scopes(1)%bindings(1)%type) == TYPE_LIST, &
                  'twohop: bag binding is a list')
      if (allocated(error)) then
         call capnp_dyn_free(schema)
         return
      end if
      item_type = type_list_element(bag_brand%scopes(1)%bindings(1)%type, err)
      call check_(error, err == CAPNP_OK .and. type_which(item_type) == TYPE_STRUCT .and. &
                  type_type_id(item_type) == cpp_item_id, &
                  'twohop: list element is Item')

      allocate (brand%scopes(2))
      brand%scopes(1) = inner_brand%scopes(1)
      brand%scopes(2)%id = nest_scope
      brand%scopes(2)%inherited = .false.
      allocate (brand%scopes(2)%bindings(1))
      brand%scopes(2)%bindings(1)%unbound = .false.
      brand%scopes(2)%bindings(1)%type = item_type
      tw = capnp_dyn_field_type(schema, box_idx, 'value', err, brand=brand, &
                                resolved_id=bound)
      call check_(error, err == CAPNP_OK .and. tw == TYPE_STRUCT .and. &
                  bound == cpp_item_id, &
                  'twohop: value resolves to Item')

      call capnp_dyn_free(schema)
   end subroutine run_two_hop_brand

   !> An unbound Box(T) field is AnyPointer, as in the C++ schema loader.
   subroutine run_unbound_anypointer(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_dyn_brand_t) :: empty, brand
      type(capnp_ptr_t) :: raw
      integer(int8), allocatable :: bytes(:)
      integer(int64) :: scope_id
      integer :: err, box_idx, tw

      call capnp_read_file('test/fixtures/box.cgr.bin', bytes, err)
      if (err /= CAPNP_OK) then
         call skip_test(error, 'fixtures not found (run from project root)')
         return
      end if
      call capnp_dyn_load(schema, bytes, err)
      box_idx = capnp_dyn_find(schema, 'Box', err)
      raw = capnp_dyn_field_schema(schema, box_idx, 'value', err)
      scope_id = type_param_scope_id(raw)
      call check_(error, err == CAPNP_OK .and. box_idx > 0 .and. &
                  scope_id == node_id(schema%nodes(box_idx)) .and. scope_id /= 0_int64, &
                  'unbound: parameter scope is the Box node')

      tw = capnp_dyn_field_type(schema, box_idx, 'value', err, brand=empty)
      call check_(error, err == CAPNP_OK .and. tw == TYPE_ANY_POINTER, &
                  'unbound: empty brand is AnyPointer')

      allocate (brand%scopes(1))
      brand%scopes(1)%id = scope_id
      brand%scopes(1)%inherited = .false.
      allocate (brand%scopes(1)%bindings(1))
      brand%scopes(1)%bindings(1)%unbound = .true.
      tw = capnp_dyn_field_type(schema, box_idx, 'value', err, brand=brand)
      call check_(error, err == CAPNP_OK .and. tw == TYPE_ANY_POINTER, &
                  'unbound: unbound binding is AnyPointer')

      deallocate (brand%scopes(1)%bindings)
      allocate (brand%scopes(1)%bindings(0))
      tw = capnp_dyn_field_type(schema, box_idx, 'value', err, brand=brand)
      call check_(error, err == CAPNP_OK .and. tw == TYPE_ANY_POINTER, &
                  'unbound: index past the bindings is AnyPointer')

      brand%scopes(1)%id = 0_int64
      deallocate (brand%scopes(1)%bindings)
      allocate (brand%scopes(1)%bindings(1))
      brand%scopes(1)%bindings(1)%unbound = .true.
      tw = capnp_dyn_field_type(schema, box_idx, 'value', err, brand=brand)
      call check_(error, err == CAPNP_OK .and. tw == TYPE_ANY_POINTER, &
                  'unbound: missing scope is AnyPointer')

      call capnp_dyn_free(schema)
   end subroutine run_unbound_anypointer


   !> A second read of a defaulted pointer must not grow the reader arena.
   subroutine run_default_arena(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: built, reader
      type(sink_t) :: sink
      type(capnp_ptr_t) :: root, origin
      integer(int8), allocatable :: bytes(:)
      integer(int64) :: len1
      integer :: err, sink_idx, vec_idx, n1, i
      real(real64) :: x

      call capnp_read_file('test/fixtures/kitchen.cgr.bin', bytes, err)
      if (err /= CAPNP_OK) then
         call skip_test(error, 'fixtures not found (run from project root)')
         return
      end if
      call capnp_dyn_load(schema, bytes, err)
      sink_idx = capnp_dyn_find(schema, 'Sink', err)
      vec_idx = capnp_dyn_find(schema, 'Vec3', err)
      call capnp_message_init_builder(built, err)
      sink = sink_new_root(built, err)
      call capnp_serialize_bytes(built, bytes, err)
      call capnp_deserialize_bytes(bytes, reader, err)
      root = capnp_root(reader, err)
      origin = capnp_dyn_getp(schema, sink_idx, root, 'origin', err)
      call check_(error, err == CAPNP_OK .and. schema%ro_ready .and. schema%ro_arena%nsegs >= 1, &
                  'arena: first read copies once')
      n1 = schema%ro_arena%nsegs
      len1 = schema%ro_arena%segs(1)%len
      do i = 1, 32
         origin = capnp_dyn_getp(schema, sink_idx, root, 'origin', err)
         if (err /= CAPNP_OK) exit
      end do
      call check_(error, err == CAPNP_OK .and. schema%ro_arena%nsegs == n1 .and. &
                  schema%ro_arena%segs(1)%len == len1 .and. len1 > 8_int64, &
                  'arena: later reads do not allocate')
      x = capnp_dyn_get_f64(schema, vec_idx, origin, 'x', err)
      call check_(error, err == CAPNP_OK .and. abs(x - 1.5_real64) < 1.0e-15_real64, &
                  'arena: cached default still reads 1.5')

      call capnp_message_free(built)
      call capnp_message_free(reader)
      call capnp_dyn_free(schema)
   end subroutine run_default_arena

   !> The inout schema on reader lookups is stated where callers look.
   subroutine run_schema_inout(error)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), parameter :: sentence = &
         'Reader pointer lookups store one cached copy of each defaulted field on the schema'
      call check_(error, file_has('src/capnp_dynamic.f90', sentence), &
                  'inout: source states the cache')
      call check_(error, file_has('CHANGELOG.md', sentence) .or. &
                  file_has('docs/newsfragments/+dyn-schema-cache.fixed.md', sentence), &
                  'inout: changelog states the cache')
      call check_(error, file_has('docs/reference.md', sentence), &
                  'inout: reference states the cache')
   end subroutine run_schema_inout

   function file_has(path, needle) result(found)
      character(len=*), intent(in) :: path, needle
      logical :: found
      integer :: u, ios
      character(len=512) :: line
      found = .false.
      open (newunit=u, file=path, status='old', action='read', iostat=ios)
      if (ios /= 0) return
      do
         read (u, '(A)', iostat=ios) line
         if (ios /= 0) exit
         if (index(line, needle) > 0) then
            found = .true.
            exit
         end if
      end do
      close (u)
   end function file_has

end module test_dynamic
