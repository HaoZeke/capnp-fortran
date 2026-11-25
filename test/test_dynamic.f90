!> Dynamic reflection: load compiled schemas at runtime and read/write
!> fields by name -- no generated accessors on the read path for
!> addressbook; kitchen is used only to seed bool/f64 wire values that
!> capnp_dyn_get_bool / capnp_dyn_get_f64 then re-read by name.
module test_dynamic
   use testdrive, only: unittest_type, error_type, check, test_failed, skip_test
   use capnp
   use capnp_dynamic
   use capnp_schema, only: TYPE_TEXT, TYPE_DATA, TYPE_LIST, TYPE_VOID, TYPE_INT64, &
                                TYPE_INT32, TYPE_STRUCT, TYPE_ANY_POINTER, NODE_STRUCT, &
                                FIELD_SLOT, type_which, &
                                type_list_element, type_type_id, type_anyptr_which, &
                                type_param_scope_id, ANYPTR_PARAMETER, &
                                value_which, value_text, node_id, &
                                node_struct_discriminant_offset
   use kitchen_capnp, only: sink_t, sink_new_root, SCHEMA__KITCHEN_CAPNP_NOTE_ANNOTATION_ID
   use addressbook_capnp, only: person_t, person_employment_which, &
                                person_employment_employer_set, &
                                PERSON_EMPLOYMENT_SCHOOL_TAG
   use dual_capnp, only: dual_t, dual_new_root, dual_primary_void_a_set, &
                         dual_primary_text_a_set, dual_secondary_void_b_set, &
                         dual_secondary_int_b_set, dual_primary_which, &
                         dual_secondary_which, DUAL_PRIMARY_VOID_A_TAG, &
                         DUAL_PRIMARY_TEXT_A_TAG, DUAL_SECONDARY_VOID_B_TAG, &
                         DUAL_SECONDARY_INT_B_TAG
   use dynamic_capnp, only: dynamic_t, dynamic_new_root, dynamic_choice_number_set, &
                            dynamic_choice_label_set
   use capnp_union, only: capnp_which
   implicit none
   private
   public :: collect_dynamic

contains

   subroutine collect_dynamic(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      allocate (testsuite(37))
      testsuite(1)%name = "dynamic"
      testsuite(1)%test => run_dynamic
      testsuite(2)%name = "dynamic-default-isolation"
      testsuite(2)%test => run_default_isolation
      testsuite(3)%name = "dynamic-malformed-text"
      testsuite(3)%test => run_malformed_text
      testsuite(4)%name = "dynamic-unloaded-schema"
      testsuite(4)%test => run_unloaded_schema
      testsuite(5)%name = "dynamic-init-null"
      testsuite(5)%test => run_init_null
      testsuite(6)%name = "dynamic-assign-types"
      testsuite(6)%test => run_assign_types
      testsuite(7)%name = "dynamic-group-union"
      testsuite(7)%test => run_group_union
      testsuite(8)%name = "dynamic-union-clear"
      testsuite(8)%test => run_union_clear
      testsuite(9)%name = "dynamic-scalars"
      testsuite(9)%test => run_scalars
      testsuite(10)%name = "dynamic-two-hop-brand"
      testsuite(10)%test => run_two_hop_brand
      testsuite(11)%name = "dynamic-reader-default"
      testsuite(11)%test => run_reader_default
      testsuite(12)%name = "dynamic-unset-which"
      testsuite(12)%test => run_unset_which
      testsuite(13)%name = "dynamic-unbound-anypointer"
      testsuite(13)%test => run_unbound_anypointer
      testsuite(14)%name = "dynamic-default-arena"
      testsuite(14)%test => run_default_arena
      testsuite(15)%name = "dynamic-schema-inout"
      testsuite(15)%test => run_schema_inout
      testsuite(16)%name = "dynamic-null-reader"
      testsuite(16)%test => run_null_reader
      testsuite(17)%name = "dynamic-nested-default-view"
      testsuite(17)%test => run_nested_default_view
      testsuite(18)%name = "dynamic-union-arm-defaults"
      testsuite(18)%test => run_union_arm_defaults
      testsuite(19)%name = "dynamic-setp-text-nul"
      testsuite(19)%test => run_setp_text_nul
      testsuite(20)%name = "dynamic-node-bomb"
      testsuite(20)%test => run_node_bomb
      testsuite(21)%name = "dynamic-load-cleans-failure"
      testsuite(21)%test => run_load_cleanup
      testsuite(22)%name = "dynamic-wire-count"
      testsuite(22)%test => run_wire_count
      testsuite(23)%name = "dynamic-int-range"
      testsuite(23)%test => run_int_range
      testsuite(24)%name = "dynamic-unknown-list"
      testsuite(24)%test => run_unknown_list
      testsuite(25)%name = "dynamic-brand-depth"
      testsuite(25)%test => run_brand_depth
      testsuite(26)%name = "dynamic-union-disc"
      testsuite(26)%test => run_union_disc
      testsuite(27)%name = "dynamic-bad-handle"
      testsuite(27)%test => run_bad_handle
      testsuite(28)%name = "dynamic-deep-groups"
      testsuite(28)%test => run_deep_groups
      testsuite(29)%name = "dynamic-nested-union-clear"
      testsuite(29)%test => run_nested_union_clear
      testsuite(30)%name = "dynamic-null-struct-pointers"
      testsuite(30)%test => run_null_struct_pointers
      testsuite(31)%name = "dynamic-int-to-float"
      testsuite(31)%test => run_int_to_float
      testsuite(32)%name = "dynamic-union-pointer-defaults"
      testsuite(32)%test => run_union_pointer_defaults
      testsuite(33)%name = "dynamic-init-list-rollback"
      testsuite(33)%test => run_init_list_rollback
      testsuite(34)%name = "dynamic-union-rejected-set"
      testsuite(34)%test => run_union_rejected_set
      testsuite(35)%name = "dynamic-union-set-keeps-arm"
      testsuite(35)%test => run_union_set_keeps_arm
      testsuite(36)%name = "dynamic-f64-to-int"
      testsuite(36)%test => run_f64_to_int
      testsuite(37)%name = "dynamic-text-coerce"
      testsuite(37)%test => run_text_coerce
   end subroutine collect_dynamic

   subroutine check_(error, cond, name)
      type(error_type), allocatable, intent(inout) :: error
      logical, intent(in) :: cond
      character(len=*), intent(in) :: name
      if (allocated(error)) return
      call check(error, cond, name)
   end subroutine check_

   subroutine run_load_cleanup(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t) :: schema
      integer(int8) :: bytes(0:7)
      integer :: err
      bytes = 0_int8
      bytes(4) = 1_int8
      call capnp_dyn_load(schema, bytes, err)
      call check_(error, err == CAPNP_ERR_FRAMING .and. .not. allocated(schema%cgr%segs) .and. &
                  .not. allocated(schema%nodes), 'dynamic load: failed request is released')
      call capnp_dyn_free(schema)
   end subroutine run_load_cleanup

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
      ! C++ set stores any pointer in an AnyPointer field.
      call capnp_dyn_setp(aschema, dynamic_idx, q, 'anyStruct', people, err)
      call check_(error, err == CAPNP_OK, 'dyn: AnyStruct takes a list')
      root = capnp_new_struct(amsg, 0, 0, err)
      call capnp_dyn_setp(aschema, dynamic_idx, q, 'anyList', root, err)
      call check_(error, err == CAPNP_OK, 'dyn: AnyList takes a struct')
      call capnp_dyn_setp(aschema, dynamic_idx, q, 'capability', root, err)
      call check_(error, err == CAPNP_OK, 'dyn: Capability takes a struct')
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
      call check_(error, err == CAPNP_ERR_KIND, 'text: dynamic store rejects missing NUL')
      call capnp_setp(person, 0, raw, err)
      call check_(error, err == CAPNP_OK, 'text: raw store into the name slot')
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
      call capnp_dyn_select(schema, choice_idx, choice, 'number', err)
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

   !> A reader handle with no message still receives the default, and the
   !> reader branch does not dereference that message to ask if it is a builder.
   subroutine run_null_reader(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_ptr_t) :: bare, origin
      integer(int8), allocatable :: bytes(:)
      integer :: err, sink_idx, vec_idx
      real(real64) :: x
      character(len=*), parameter :: bad = &
         'associated(p%msg) .or. .not. p%msg%is_builder'

      call capnp_read_file('test/fixtures/kitchen.cgr.bin', bytes, err)
      if (err /= CAPNP_OK) then
         call skip_test(error, 'fixtures not found (run from project root)')
         return
      end if
      call capnp_dyn_load(schema, bytes, err)
      sink_idx = capnp_dyn_find(schema, 'Sink', err)
      vec_idx = capnp_dyn_find(schema, 'Vec3', err)
      bare = capnp_ptr_t()
      bare%kind = CAPNP_PK_STRUCT
      bare%pwords = 1
      origin = capnp_dyn_getp(schema, sink_idx, bare, 'origin', err)
      call check_(error, err == CAPNP_OK .and. origin%kind == CAPNP_PK_STRUCT .and. origin%frozen, &
                  'null reader: default view')
      x = capnp_dyn_get_f64(schema, vec_idx, origin, 'x', err)
      call check_(error, err == CAPNP_OK .and. abs(x - 1.5_real64) < 1.0e-15_real64, &
                  'null reader: reads 1.5')
      call check_(error, .not. file_has('src/capnp_dynamic.f90', bad), &
                  'null reader: builder flag is not read through or')
      call capnp_dyn_free(schema)
   end subroutine run_null_reader

   !> Compile a small schema with the capnp tool so a test can use shapes
   !> the checked-in fixtures do not have.
   subroutine load_defaults_schema(error, schema, ok)
      type(error_type), allocatable, intent(inout) :: error
      type(capnp_dyn_schema_t), intent(inout) :: schema
      logical, intent(out) :: ok
      character(len=*), parameter :: dir = 'build/dyn_inline'
      integer(int8), allocatable :: bytes(:)
      integer :: u, ios, stat, cs, err
      ok = .false.
      call execute_command_line('mkdir -p '//dir, exitstat=stat, cmdstat=cs)
      open (newunit=u, file=dir//'/defaults.capnp', status='replace', action='write', iostat=ios)
      if (ios /= 0 .or. cs /= 0) then
         call skip_test(error, 'cannot write '//dir)
         return
      end if
      write (u, '(a)') '@0xd1f0c3a5b7e90a11;'
      write (u, '(a)') 'struct Leaf { v @0 :UInt32; }'
      write (u, '(a)') 'struct Mid { leaf @0 :Leaf = (v = 7); }'
      write (u, '(a)') 'struct Outer { mid @0 :Mid = (); }'
      write (u, '(a)') 'struct Pick { union { a @0 :Leaf = (v = 1); b @1 :Leaf = (v = 2); } }'
      close (u)
      call execute_command_line('capnp compile -o- '//dir//'/defaults.capnp > '// &
                                dir//'/defaults.cgr', exitstat=stat, cmdstat=cs)
      if (cs /= 0 .or. stat /= 0) then
         call skip_test(error, 'capnp compile is not available')
         return
      end if
      call capnp_read_file(dir//'/defaults.cgr', bytes, err)
      if (err /= CAPNP_OK) return
      call capnp_dyn_load(schema, bytes, err)
      ok = err == CAPNP_OK
   end subroutine load_defaults_schema

   !> A defaulted pointer inside a frozen default view reads its own
   !> default, as a reader does.
   subroutine run_nested_default_view(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: built, reader
      type(capnp_ptr_t) :: root, mid, leaf
      integer(int8), allocatable :: bytes(:)
      integer :: err, outer_idx, mid_idx, leaf_idx
      logical :: ok
      call load_defaults_schema(error, schema, ok)
      if (allocated(error)) return
      call check_(error, ok, 'nested view: schema loads')
      if (allocated(error)) return
      outer_idx = capnp_dyn_find(schema, 'Outer', err)
      mid_idx = capnp_dyn_find(schema, 'Mid', err)
      leaf_idx = capnp_dyn_find(schema, 'Leaf', err)
      call capnp_message_init_builder(built, err)
      root = capnp_dyn_new_root(schema, outer_idx, built, err)
      call capnp_serialize_bytes(built, bytes, err)
      call capnp_deserialize_bytes(bytes, reader, err)
      root = capnp_root(reader, err)
      mid = capnp_dyn_getp(schema, outer_idx, root, 'mid', err)
      call check_(error, err == CAPNP_OK .and. mid%frozen, 'nested view: mid is a frozen view')
      leaf = capnp_dyn_getp(schema, mid_idx, mid, 'leaf', err)
      call check_(error, err == CAPNP_OK .and. leaf%kind == CAPNP_PK_STRUCT .and. leaf%frozen, &
                  'nested view: leaf default reads from the view')
      call check_(error, capnp_dyn_get_int(schema, leaf_idx, leaf, 'v', err) == 7_int64 .and. &
                  err == CAPNP_OK, 'nested view: leaf default is 7')
      call capnp_message_free(built)
      call capnp_message_free(reader)
      call capnp_dyn_free(schema)
   end subroutine run_nested_default_view

   !> Two union arms share one pointer slot but not one default.
   subroutine run_union_arm_defaults(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: built_a, built_b, reader_a, reader_b
      type(capnp_ptr_t) :: root, leaf
      integer(int8), allocatable :: bytes(:)
      integer :: err, pick_idx, leaf_idx
      logical :: ok
      call load_defaults_schema(error, schema, ok)
      if (allocated(error)) return
      call check_(error, ok, 'union defaults: schema loads')
      if (allocated(error)) return
      pick_idx = capnp_dyn_find(schema, 'Pick', err)
      leaf_idx = capnp_dyn_find(schema, 'Leaf', err)
      call capnp_message_init_builder(built_a, err)
      root = capnp_dyn_new_root(schema, pick_idx, built_a, err)
      call capnp_serialize_bytes(built_a, bytes, err)
      call capnp_deserialize_bytes(bytes, reader_a, err)
      root = capnp_root(reader_a, err)
      leaf = capnp_dyn_getp(schema, pick_idx, root, 'a', err)
      call check_(error, err == CAPNP_OK .and. &
                  capnp_dyn_get_int(schema, leaf_idx, leaf, 'v', err) == 1_int64, &
                  'union defaults: arm a reads 1')
      call capnp_message_init_builder(built_b, err)
      root = capnp_dyn_new_root(schema, pick_idx, built_b, err)
      call capnp_dyn_select(schema, pick_idx, root, 'b', err)
      call check_(error, err == CAPNP_OK, 'union defaults: select b')
      call capnp_serialize_bytes(built_b, bytes, err)
      call capnp_deserialize_bytes(bytes, reader_b, err)
      root = capnp_root(reader_b, err)
      leaf = capnp_dyn_getp(schema, pick_idx, root, 'b', err)
      call check_(error, err == CAPNP_OK .and. &
                  capnp_dyn_get_int(schema, leaf_idx, leaf, 'v', err) == 2_int64, &
                  'union defaults: arm b reads its own default 2')
      call capnp_message_free(built_a)
      call capnp_message_free(built_b)
      call capnp_message_free(reader_a)
      call capnp_message_free(reader_b)
      call capnp_dyn_free(schema)
   end subroutine run_union_arm_defaults

   !> A Text value needs its trailing NUL; Data takes any bytes.
   subroutine run_setp_text_nul(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root, raw
      integer(int8), allocatable :: bytes(:)
      character(len=:), allocatable :: s
      integer :: err, person_idx
      call capnp_read_file('test/fixtures/addressbook.cgr.bin', bytes, err)
      if (err /= CAPNP_OK) then
         call skip_test(error, 'fixtures not found (run from project root)')
         return
      end if
      call capnp_dyn_load(schema, bytes, err)
      person_idx = capnp_dyn_find(schema, 'Person', err)
      call capnp_message_init_builder(msg, err)
      root = capnp_dyn_new_root(schema, person_idx, msg, err)
      raw = capnp_new_list(msg, CAPNP_SZ_BYTE, 3_int64, err)
      call capnp_list_set_i8(raw, 0_int64, 97_int8, err)
      call capnp_list_set_i8(raw, 1_int64, 98_int8, err)
      call capnp_list_set_i8(raw, 2_int64, 99_int8, err)
      call capnp_dyn_setp(schema, person_idx, root, 'name', raw, err)
      call check_(error, err == CAPNP_ERR_KIND, 'text nul: unterminated Text is rejected')
      call capnp_dyn_get_text(schema, person_idx, root, 'name', s, err)
      call check_(error, err == CAPNP_OK .and. s == '', 'text nul: field left unset')
      raw = capnp_new_list(msg, CAPNP_SZ_BYTE, 0_int64, err)
      call capnp_dyn_setp(schema, person_idx, root, 'name', raw, err)
      call check_(error, err == CAPNP_ERR_KIND, 'text nul: empty byte list is rejected')
      raw = capnp_new_list(msg, CAPNP_SZ_BYTE, 3_int64, err)
      call capnp_list_set_i8(raw, 0_int64, 104_int8, err)
      call capnp_list_set_i8(raw, 1_int64, 105_int8, err)
      call capnp_dyn_setp(schema, person_idx, root, 'name', raw, err)
      call capnp_dyn_get_text(schema, person_idx, root, 'name', s, err)
      call check_(error, err == CAPNP_OK .and. s == 'hi', 'text nul: terminated Text is kept')
      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end subroutine run_setp_text_nul
   !> Zero-sized lists load like C++ 1.4.0, one traversal word per element
   !> against the budget the reader configures. Past it the read fails.
   subroutine run_wire_count(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t) :: schema
      type(capnp_dyn_brand_t) :: brand
      type(capnp_message_t), target :: msg, rd
      type(capnp_ptr_t) :: root, nodes, typed, raw, scopes
      integer(int8), allocatable :: bytes(:)
      integer :: err

      call capnp_message_init_builder(msg, err)
      root = capnp_new_struct(msg, 0, 4, err)
      call capnp_set_root(msg, root, err)
      nodes = capnp_new_composite_list(msg, 4096_int64, 0, 0, err)
      call capnp_setp(root, 0, nodes, err)
      call capnp_serialize_bytes(msg, bytes, err)
      call check_(error, err == CAPNP_OK, 'wire-count: framed')
      call capnp_dyn_load(schema, bytes, err)
      call check_(error, err == CAPNP_OK .and. schema%loaded, &
                  'wire-count: 4096 zero-size nodes load')
      if (schema%loaded) call check_(error, size(schema%nodes) == 4096, &
                                     'wire-count: all 4096 nodes are kept')
      call capnp_dyn_free(schema)
      call capnp_dyn_load(schema, bytes, err, traversal_words=4096_int64)
      call check_(error, err /= CAPNP_OK .and. .not. schema%loaded, &
                  'wire-count: 4096 nodes exceed a 4096-word budget')
      call capnp_dyn_free(schema)
      call capnp_message_free(msg)

      call capnp_message_init_builder(msg, err)
      typed = capnp_new_struct(msg, 3, 1, err)
      call capnp_set_u16(typed, 0_int64, 16_int32, err)
      raw = capnp_new_struct(msg, 0, 1, err)
      scopes = capnp_new_list(msg, CAPNP_SZ_VOID, 4096_int64, err)
      call capnp_setp(raw, 0, scopes, err)
      call capnp_setp(typed, 0, raw, err)
      call capnp_dyn_load_brand(typed, brand, err)
      call check_(error, err == CAPNP_OK, 'wire-count: 4096 void brand scopes load')
      if (err == CAPNP_OK) call check_(error, size(brand%scopes) == 4096, &
                                       'wire-count: all 4096 scopes are kept')
      call capnp_set_root(msg, typed, err)
      call capnp_serialize_bytes(msg, bytes, err)
      call capnp_message_free(msg)

      call capnp_deserialize_bytes(bytes, rd, err)
      typed = capnp_root(rd, err)
      call capnp_dyn_load_brand(typed, brand, err)
      call check_(error, err == CAPNP_OK, 'wire-count: 4096 read brand scopes load')
      if (err == CAPNP_OK) call check_(error, size(brand%scopes) == 4096, &
                                       'wire-count: all 4096 read scopes are kept')
      call capnp_message_free(rd)

      call capnp_deserialize_bytes(bytes, rd, err, traversal_words=4096_int64)
      typed = capnp_root(rd, err)
      call capnp_dyn_load_brand(typed, brand, err)
      call check_(error, err /= CAPNP_OK, &
                  'wire-count: brand scopes past the reader budget')
      call capnp_message_free(rd)
   end subroutine run_wire_count

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

   !> Main's 10000 zero-sized nodes under a smaller configured budget.
   !> The traversal charge refuses them before any host array is allocated.
   subroutine run_node_bomb(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg
      type(capnp_dyn_schema_t) :: schema
      type(capnp_ptr_t) :: root, lst
      integer(int8), allocatable :: bytes(:)
      integer :: err
      call capnp_message_init_builder(msg, err)
      root = capnp_new_struct(msg, 0, 1, err)
      lst = capnp_new_composite_list(msg, 10000_int64, 0, 0, err)
      call capnp_setp(root, 0, lst, err)
      call capnp_set_root(msg, root, err)
      call capnp_serialize_bytes(msg, bytes, err)
      call capnp_message_free(msg)
      call capnp_dyn_load(schema, bytes, err, traversal_words=8192_int64)
      call check_(error, err /= CAPNP_OK .and. .not. allocated(schema%nodes), &
                  'nodes: zero-size list past the budget is not allocated')
      call capnp_dyn_free(schema)
   end subroutine run_node_bomb

   !> Values that do not fit the field width are rejected and leave the
   !> previous bits alone. A rejected union member still becomes the
   !> active arm, as in C++.
   subroutine run_int_range(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root, choice
      integer(int8), allocatable :: bytes(:)
      character(len=:), allocatable :: s
      integer :: err, werr, idx, cidx, tag

      call capnp_read_file('test/fixtures/dynamic.cgr.bin', bytes, err)
      if (err /= CAPNP_OK) then
         call skip_test(error, 'fixtures not found (run from project root)')
         return
      end if
      call capnp_dyn_load(schema, bytes, err)
      idx = capnp_dyn_find(schema, 'Scalars', err)
      call capnp_message_init_builder(msg, err)
      root = capnp_dyn_new_root(schema, idx, msg, err)
      call capnp_dyn_set_int(schema, idx, root, 'i8', 3_int64, err)
      call capnp_dyn_set_int(schema, idx, root, 'i8', 128_int64, err)
      call check_(error, err == CAPNP_ERR_ARG .and. &
                  capnp_dyn_get_int(schema, idx, root, 'i8', err) == 3_int64, &
                  'range: int8 overflow keeps 3')
      call capnp_dyn_set_int(schema, idx, root, 'i8', -129_int64, err)
      call check_(error, err == CAPNP_ERR_ARG, 'range: int8 underflow')
      call capnp_dyn_set_int(schema, idx, root, 'i8', -128_int64, err)
      call check_(error, err == CAPNP_OK .and. &
                  capnp_dyn_get_int(schema, idx, root, 'i8', err) == -128_int64, &
                  'range: int8 minimum')
      call capnp_dyn_set_int(schema, idx, root, 'i8', 127_int64, err)
      call check_(error, err == CAPNP_OK .and. &
                  capnp_dyn_get_int(schema, idx, root, 'i8', err) == 127_int64, &
                  'range: int8 maximum')
      call capnp_dyn_set_int(schema, idx, root, 'u8', -1_int64, err)
      call check_(error, err == CAPNP_ERR_ARG, 'range: uint8 rejects a negative')
      call capnp_dyn_set_int(schema, idx, root, 'u8', 256_int64, err)
      call check_(error, err == CAPNP_ERR_ARG, 'range: uint8 overflow')
      call capnp_dyn_set_int(schema, idx, root, 'i16', 32768_int64, err)
      call check_(error, err == CAPNP_ERR_ARG, 'range: int16 overflow')
      call capnp_dyn_set_int(schema, idx, root, 'u16', -1_int64, err)
      call check_(error, err == CAPNP_ERR_ARG, 'range: uint16 rejects a negative')
      call capnp_dyn_set_int(schema, idx, root, 'i32', 2147483648_int64, err)
      call check_(error, err == CAPNP_ERR_ARG, 'range: int32 overflow')
      call capnp_dyn_set_int(schema, idx, root, 'u32', -1_int64, err)
      call check_(error, err == CAPNP_ERR_ARG, 'range: uint32 rejects a negative')
      call capnp_dyn_set_int(schema, idx, root, 'u32', 4294967296_int64, err)
      call check_(error, err == CAPNP_ERR_ARG, 'range: uint32 overflow')
      call capnp_dyn_set_int(schema, idx, root, 'u64', -1_int64, err)
      call check_(error, err == CAPNP_ERR_ARG, 'range: uint64 rejects a negative')
      call capnp_dyn_set_int(schema, idx, root, 'hue', -1_int64, err)
      call check_(error, err == CAPNP_ERR_ARG, 'range: enum rejects a negative')
      call capnp_dyn_set_int(schema, idx, root, 'hue', 65536_int64, err)
      call check_(error, err == CAPNP_ERR_ARG, 'range: enum above uint16')
      call capnp_dyn_set_int(schema, idx, root, 'hue', 9_int64, err)
      call check_(error, err == CAPNP_OK .and. &
                  capnp_dyn_get_int(schema, idx, root, 'hue', err) == 9_int64, &
                  'range: unknown enumerant still fits')
      cidx = capnp_dyn_find(schema, 'Dynamic.choice', err)
      choice = capnp_dyn_new_root(schema, cidx, msg, err)
      call capnp_dyn_set_text(schema, cidx, choice, 'label', 'keep', err)
      call capnp_dyn_set_int(schema, cidx, choice, 'number', 2147483648_int64, err)
      call check_(error, err == CAPNP_ERR_ARG, 'range: union int32 overflow')
      ! C++ set() runs setInUnion before as<int32_t>() throws.
      tag = capnp_dyn_which(schema, cidx, choice, err)
      call check_(error, err == CAPNP_OK .and. tag == 0 .and. &
                  capnp_dyn_get_int(schema, cidx, choice, 'number', werr) == 0_int64, &
                  'range: rejected union write selects the arm without storing')
      call capnp_dyn_set_int(schema, cidx, choice, 'missing', 1_int64, err)
      call check_(error, err /= CAPNP_OK, 'range: unknown member rejected')
      tag = capnp_dyn_which(schema, cidx, choice, err)
      call check_(error, err == CAPNP_OK .and. tag == 0, &
                  'range: unknown member leaves the arm alone')
      call capnp_dyn_set_int(schema, cidx, choice, 'label', 5_int64, err)
      call check_(error, err == CAPNP_ERR_KIND, 'range: union type mismatch')
      tag = capnp_dyn_which(schema, cidx, choice, err)
      call capnp_dyn_get_text(schema, cidx, choice, 'label', s, err)
      call check_(error, err == CAPNP_OK .and. tag == 1 .and. s == 'keep', &
                  'range: rejected writes leave the arm data alone')
      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end subroutine run_int_range

   !> An unknown dynamic element tag uses a void list, as it does in C++.
   subroutine run_unknown_list(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: sink, elem, made, alien, field_t
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
      made = capnp_dyn_init_list(schema, sink_idx, sink, 'scores', 2_int64, err)
      call check_(error, err == CAPNP_OK .and. made%esize == CAPNP_SZ_EIGHT, &
                  'unknown-list: int64 list still inits')
      field_t = capnp_dyn_field_schema(schema, sink_idx, 'scores', err)
      elem = type_list_element(field_t, err)
      call check_(error, err == CAPNP_OK .and. type_which(elem) == TYPE_INT64, &
                  'unknown-list: element starts as int64')
      call capnp_set_u16(elem, 0_int64, 99_int32, err)
      made = capnp_dyn_init_list(schema, sink_idx, sink, 'scores', 1_int64, err)
      call check_(error, err == CAPNP_OK .and. made%esize == CAPNP_SZ_VOID, &
                  'unknown-list: init makes a void list')
      alien = capnp_new_list(msg, CAPNP_SZ_PTR, 1_int64, err)
      call capnp_dyn_setp(schema, sink_idx, sink, 'scores', alien, err)
      call check_(error, err == CAPNP_ERR_KIND, 'unknown-list: pointer list is not a match')
      alien = capnp_new_list(msg, CAPNP_SZ_VOID, 1_int64, err)
      call capnp_dyn_setp(schema, sink_idx, sink, 'scores', alien, err)
      call check_(error, err == CAPNP_OK, 'unknown-list: void list is a match')
      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end subroutine run_unknown_list

   !> A parameter binding that points at itself stops with a depth error.
   subroutine run_brand_depth(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_dyn_brand_t) :: brand
      type(capnp_ptr_t) :: raw
      integer(int8), allocatable :: bytes(:)
      integer(int64) :: scope_id, bound
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
      call check_(error, err == CAPNP_OK .and. box_idx > 0 .and. scope_id /= 0_int64, &
                  'depth: box parameter')
      allocate (brand%scopes(1))
      brand%scopes(1)%id = scope_id
      brand%scopes(1)%inherited = .false.
      allocate (brand%scopes(1)%bindings(1))
      brand%scopes(1)%bindings(1)%unbound = .false.
      brand%scopes(1)%bindings(1)%type = raw
      tw = capnp_dyn_field_type(schema, box_idx, 'value', err, brand=brand, &
                                resolved_id=bound)
      call check_(error, err == CAPNP_ERR_DEPTH .and. tw == -1 .and. bound == 0_int64, &
                  'depth: cyclic parameter is an error')
      call check_(error, brand_chain_type(schema, box_idx, brand, scope_id, 12) == TYPE_LIST, &
                  'depth: twelve nested lists resolve')
      call check_(error, brand_chain_type(schema, box_idx, brand, scope_id, 300) == TYPE_LIST, &
                  'depth: three hundred nested lists resolve')
      call check_(error, brand_chain_type(schema, box_idx, brand, scope_id, 4100) == -1, &
                  'depth: a chain past the limit is an error')
      call capnp_dyn_free(schema)
   end subroutine run_brand_depth

   !> Bind the parameter of Box to List(...List(Text)) nested n deep and
   !> return the resolved type, or -1 with the error left in the check.
   function brand_chain_type(schema, box_idx, brand, scope_id, n) result(tw)
      type(capnp_dyn_schema_t), intent(in) :: schema
      integer, intent(in) :: box_idx, n
      type(capnp_dyn_brand_t), intent(inout) :: brand
      integer(int64), intent(in) :: scope_id
      integer :: tw
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: inner, outer
      integer(int64) :: bound
      integer :: err, i

      call capnp_message_init_builder(msg, err)
      inner = capnp_new_struct(msg, 3, 1, err)
      call capnp_set_u16(inner, 0_int64, int(TYPE_TEXT, int32), err)
      do i = 1, n
         outer = capnp_new_struct(msg, 3, 1, err)
         call capnp_set_u16(outer, 0_int64, int(TYPE_LIST, int32), err)
         call capnp_setp(outer, 0, inner, err)
         inner = outer
      end do
      brand%scopes(1)%id = scope_id
      brand%scopes(1)%bindings(1)%type = inner
      tw = capnp_dyn_field_type(schema, box_idx, 'value', err, brand=brand, &
                                resolved_id=bound)
      if (err /= CAPNP_OK) tw = -1
      call capnp_message_free(msg)
   end function brand_chain_type

   !> A failed discriminant write is the error the caller sees, and the
   !> value is not stored afterwards.
   subroutine run_union_disc(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg
      type(person_t) :: h
      type(capnp_ptr_t) :: q
      integer :: err

      call capnp_message_init_builder(msg, err)
      h%p = capnp_new_struct(msg, 0, 4, err)
      call check_(error, err == CAPNP_OK, 'union-disc: short struct')
      call person_employment_employer_set(h, 'Acme', err)
      call check_(error, err == CAPNP_ERR_BOUNDS, 'union-disc: discriminant error stands')
      q = capnp_getp(h%p, 3, err)
      call check_(error, err == CAPNP_OK .and. q%kind == CAPNP_PK_NULL, &
                  'union-disc: text was not written')
      call capnp_message_free(msg)
   end subroutine run_union_disc

   !> A list, or a struct that claims data it does not have, is not a
   !> successful default. A null handle still reads the default.
   subroutine run_bad_handle(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root, lst, bare, plist, q
      integer(int8), allocatable :: bytes(:), payload(:)
      integer(int64) :: n
      integer :: err, idx, tag
      logical :: flag
      real(real64) :: x
      character(len=:), allocatable :: s

      call capnp_read_file('test/fixtures/dynamic.cgr.bin', bytes, err)
      if (err /= CAPNP_OK) then
         call skip_test(error, 'fixtures not found (run from project root)')
         return
      end if
      call capnp_dyn_load(schema, bytes, err)
      idx = capnp_dyn_find(schema, 'Scalars', err)
      call capnp_message_init_builder(msg, err)
      lst = capnp_new_list(msg, CAPNP_SZ_EIGHT, 1_int64, err)
      n = capnp_dyn_get_int(schema, idx, lst, 'i8', err)
      call check_(error, err == CAPNP_ERR_KIND .and. n == 0_int64, &
                  'bad-handle: int on a list')
      flag = capnp_dyn_get_bool(schema, idx, lst, 'b', err)
      call check_(error, err == CAPNP_ERR_KIND .and. .not. flag, &
                  'bad-handle: bool on a list')
      x = capnp_dyn_get_f64(schema, idx, lst, 'f64', err)
      call check_(error, err == CAPNP_ERR_KIND .and. x == 0.0_real64, &
                  'bad-handle: float on a list')
      plist = capnp_new_list(msg, CAPNP_SZ_PTR, 2_int64, err)
      call capnp_dyn_get_text(schema, idx, plist, 'note', s, err)
      call check_(error, err == CAPNP_ERR_KIND, 'bad-handle: text on a pointer list')
      call capnp_dyn_get_data(schema, idx, plist, 'blob', payload, err)
      call check_(error, err == CAPNP_ERR_KIND, 'bad-handle: data on a pointer list')
      q = capnp_dyn_getp(schema, idx, plist, 'note', err)
      call check_(error, err == CAPNP_ERR_KIND .and. q%kind == CAPNP_PK_NULL, &
                  'bad-handle: pointer on a pointer list')
      idx = capnp_dyn_find(schema, 'Dynamic', err)
      q = capnp_dyn_getp(schema, idx, plist, 'choice', err)
      call check_(error, err == CAPNP_ERR_KIND .and. q%kind == CAPNP_PK_NULL, &
                  'bad-handle: group on a pointer list')
      idx = capnp_dyn_find(schema, 'Scalars', err)
      bare = capnp_ptr_t()
      bare%kind = CAPNP_PK_STRUCT
      bare%dwords = 1
      n = capnp_dyn_get_int(schema, idx, bare, 'i8', err)
      call check_(error, err == CAPNP_ERR_KIND .and. n == 0_int64, &
                  'bad-handle: int with no message')
      bare = capnp_ptr_t()
      n = capnp_dyn_get_int(schema, idx, bare, 'i8', err)
      call check_(error, err == CAPNP_OK .and. n == 0_int64, &
                  'bad-handle: null handle reads the default')
      root = capnp_dyn_new_root(schema, idx, msg, err)
      root%seg = 9
      n = capnp_dyn_get_int(schema, idx, root, 'i8', err)
      call check_(error, err == CAPNP_ERR_BOUNDS .and. n == 0_int64, &
                  'bad-handle: segment is outside the message')
      idx = capnp_dyn_find(schema, 'Shape', err)
      tag = capnp_dyn_which(schema, idx, lst, err)
      call check_(error, err == CAPNP_ERR_KIND .and. tag == -1, &
                  'bad-handle: which on a list')
      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end subroutine run_bad_handle

   !> Selecting another union arm clears an arm made of groups nested ten
   !> deep. dynamic.c++ clears a group arm without a nesting cap.
   subroutine run_deep_groups(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root
      integer(int8), allocatable :: bytes(:)
      character(len=:), allocatable :: path
      character(len=3) :: num
      integer :: err, idx, last, i

      call capnp_read_file('test/fixtures/dynamic.cgr.bin', bytes, err)
      if (err /= CAPNP_OK) then
         call skip_test(error, 'fixtures not found (run from project root)')
         return
      end if
      call capnp_dyn_load(schema, bytes, err)
      idx = capnp_dyn_find(schema, 'DeepGroups', err)
      path = 'DeepGroups'
      do i = 1, 10
         write (num, '(i0)') i
         path = path//'.g'//trim(num)
      end do
      last = capnp_dyn_find(schema, path, err)
      call check_(error, idx > 0 .and. last > 0 .and. err == CAPNP_OK, &
                  'deep-groups: nodes found')
      call capnp_message_init_builder(msg, err)
      root = capnp_dyn_new_root(schema, idx, msg, err)
      call capnp_dyn_select(schema, idx, root, 'g1', err)
      call check_(error, err == CAPNP_OK, 'deep-groups: select the group arm')
      call capnp_dyn_set_int(schema, last, root, 'v10', 77_int64, err)
      call check_(error, err == CAPNP_OK .and. &
                  capnp_dyn_get_int(schema, last, root, 'v10', err) == 77_int64, &
                  'deep-groups: write the innermost field')
      call capnp_dyn_select(schema, idx, root, 'a', err)
      call capnp_dyn_set_int(schema, idx, root, 'a', 5_int64, err)
      call check_(error, err == CAPNP_OK, 'deep-groups: another arm replaces the groups')
      call check_(error, capnp_dyn_which(schema, idx, root, err) == 0, &
                  'deep-groups: the new arm is selected')
      call check_(error, capnp_dyn_get_int(schema, last, root, 'v10', err) == 0_int64, &
                  'deep-groups: the innermost field is cleared')
      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end subroutine run_deep_groups

   !> Clearing a union group arm resets any inner union discriminant as well
   !> as the slots, and a cyclic group schema terminates with CAPNP_ERR_DEPTH.
   subroutine run_nested_union_clear(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema, cschema
      type(capnp_message_t), target :: msg, cgr, cmsg
      type(capnp_ptr_t) :: root, croot, nodes, n0, n1, fl0, fl1, f, sroot
      integer(int8), allocatable :: bytes(:)
      character(len=:), allocatable :: s
      integer(int64) :: v
      integer :: err, idx, gidx, tag

      call capnp_read_file('test/fixtures/dynamic.cgr.bin', bytes, err)
      if (err /= CAPNP_OK) then
         call skip_test(error, 'fixtures not found (run from project root)')
         return
      end if
      call capnp_dyn_load(schema, bytes, err)
      idx = capnp_dyn_find(schema, 'NestedUnion', err)
      gidx = capnp_dyn_find(schema, 'NestedUnion.g', err)
      call check_(error, err == CAPNP_OK .and. idx > 0 .and. gidx > 0, &
                  'nested-union: nodes found')
      call capnp_message_init_builder(msg, err)
      root = capnp_dyn_new_root(schema, idx, msg, err)
      call capnp_dyn_select(schema, idx, root, 'g', err)
      call capnp_dyn_set_text(schema, gidx, root, 'y', 'hello', err)
      tag = capnp_dyn_which(schema, gidx, root, err)
      call check_(error, err == CAPNP_OK .and. tag == 1, &
                  'nested-union: inner y arm active')
      call capnp_dyn_select(schema, idx, root, 'a', err)
      call capnp_dyn_set_int(schema, idx, root, 'a', 5_int64, err)
      call check_(error, err == CAPNP_OK, 'nested-union: switch to outer a')
      call capnp_dyn_select(schema, idx, root, 'g', err)
      tag = capnp_dyn_which(schema, gidx, root, err)
      call check_(error, err == CAPNP_OK .and. tag == 0, &
                  'nested-union: inner discriminant reset to 0')
      v = capnp_dyn_get_int(schema, gidx, root, 'x', err)
      call check_(error, err == CAPNP_OK .and. v == 0_int64, &
                  'nested-union: inner x arm readable as 0')
      call capnp_dyn_select(schema, gidx, root, 'y', err)
      call capnp_dyn_get_text(schema, gidx, root, 'y', s, err)
      call check_(error, err == CAPNP_OK .and. s == '', &
                  'nested-union: inner text slot cleared')
      call capnp_message_free(msg)
      call capnp_dyn_free(schema)

      ! Build a two-node CGR where a union group arm references itself.
      call capnp_message_init_builder(cgr, err)
      croot = capnp_new_struct(cgr, 0, 4, err)
      call capnp_set_root(cgr, croot, err)
      nodes = capnp_new_composite_list(cgr, 2_int64, 6, 6, err)
      call capnp_setp(croot, 0, nodes, err)

      n0 = capnp_list_get_struct(nodes, 0, err)
      call capnp_set_i64(n0, 0_int64, 100_int64, err)
      call capnp_set_text(n0, 0, 'Cyclic', err)
      call capnp_set_u16(n0, 12_int64, 1_int32, err)
      call capnp_set_u16(n0, 14_int64, 1_int32, err)
      call capnp_set_u16(n0, 24_int64, 0_int32, err)
      call capnp_set_u16(n0, 30_int64, 2_int32, err)
      call capnp_set_u32(n0, 32_int64, 0_int64, err)
      fl0 = capnp_new_composite_list(cgr, 2_int64, 3, 4, err)
      call capnp_setp(n0, 3, fl0, err)
      f = capnp_list_get_struct(fl0, 0, err)
      call capnp_set_text(f, 0, 'a', err)
      call capnp_set_u16(f, 2_int64, 0_int32, err, default=65535_int32)
      call capnp_set_u16(f, 8_int64, 0_int32, err)
      f = capnp_list_get_struct(fl0, 1, err)
      call capnp_set_text(f, 0, 'g', err)
      call capnp_set_u16(f, 2_int64, 1_int32, err, default=65535_int32)
      call capnp_set_u16(f, 8_int64, 1_int32, err)
      call capnp_set_i64(f, 16_int64, 200_int64, err)

      n1 = capnp_list_get_struct(nodes, 1, err)
      call capnp_set_i64(n1, 0_int64, 200_int64, err)
      call capnp_set_text(n1, 0, 'Cyclic.g', err)
      call capnp_set_u16(n1, 12_int64, 1_int32, err)
      call capnp_set_u16(n1, 14_int64, 1_int32, err)
      call capnp_set_u16(n1, 24_int64, 0_int32, err)
      call capnp_set_bool(n1, 224_int64, .true., err)
      fl1 = capnp_new_composite_list(cgr, 1_int64, 3, 4, err)
      call capnp_setp(n1, 3, fl1, err)
      f = capnp_list_get_struct(fl1, 0, err)
      call capnp_set_text(f, 0, 'loop', err)
      call capnp_set_u16(f, 2_int64, 65535_int32, err, default=65535_int32)
      call capnp_set_u16(f, 8_int64, 1_int32, err)
      call capnp_set_i64(f, 16_int64, 200_int64, err)

      call capnp_serialize_bytes(cgr, bytes, err)
      call capnp_dyn_load(cschema, bytes, err)
      idx = capnp_dyn_find(cschema, 'Cyclic', err)
      call check_(error, err == CAPNP_OK .and. idx > 0, 'nested-union: cyclic schema loaded')
      call capnp_message_init_builder(cmsg, err)
      sroot = capnp_dyn_new_root(cschema, idx, cmsg, err)
      call capnp_dyn_select(cschema, idx, sroot, 'g', err)
      call check_(error, err == CAPNP_OK, 'nested-union: select cyclic group arm')
      call capnp_dyn_select(cschema, idx, sroot, 'a', err)
      call check_(error, err == CAPNP_ERR_DEPTH, &
                  'nested-union: cyclic group terminates with CAPNP_ERR_DEPTH')
      call capnp_message_free(cmsg)
      call capnp_dyn_free(cschema)
      call capnp_message_free(cgr)
   end subroutine run_nested_union_clear

   !> Reading pointer fields through a null struct handle that is attached to
   !> a message returns the field defaults, matching C++ StructReader.
   subroutine run_null_struct_pointers(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: bmsg, rmsg
      type(capnp_ptr_t) :: broot, bnull, rnull, origin, scores, tags, q
      integer(int8), allocatable :: bytes(:), raw(:), blob(:)
      character(len=:), allocatable :: s
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
      call check_(error, err == CAPNP_OK .and. sink_idx > 0 .and. vec_idx > 0, &
                  'null-ptr: nodes found')

      ! Empty reader message: root handle is CAPNP_PK_NULL with msg associated.
      call capnp_message_init_builder(bmsg, err)
      call capnp_serialize_bytes(bmsg, raw, err)
      call capnp_deserialize_bytes(raw, rmsg, err)
      rnull = capnp_root(rmsg, err)
      call check_(error, err == CAPNP_OK .and. rnull%kind == CAPNP_PK_NULL .and. &
                  associated(rnull%msg), 'null-ptr: associated reader null')

      q = capnp_getp(rnull, 0, err)
      call check_(error, err == CAPNP_OK .and. q%kind == CAPNP_PK_NULL, &
                  'null-ptr: capnp_getp on reader null returns null')
      call capnp_dyn_get_text(schema, sink_idx, rnull, 'label', s, err)
      call check_(error, err == CAPNP_OK .and. s == 'unnamed', &
                  'null-ptr: reader text default')
      call capnp_dyn_get_data(schema, sink_idx, rnull, 'payload', blob, err)
      call check_(error, err == CAPNP_OK .and. size(blob) == 4 .and. &
                  iand(int(blob(lbound(blob, 1))), 255) == int(z'de') .and. &
                  iand(int(blob(ubound(blob, 1))), 255) == int(z'ef'), &
                  'null-ptr: reader data default')
      origin = capnp_dyn_getp(schema, sink_idx, rnull, 'origin', err)
      call check_(error, err == CAPNP_OK .and. origin%kind == CAPNP_PK_STRUCT, &
                  'null-ptr: reader struct default')
      x = capnp_dyn_get_f64(schema, vec_idx, origin, 'x', err)
      call check_(error, err == CAPNP_OK .and. abs(x - 1.5_real64) < 1.0e-15_real64, &
                  'null-ptr: reader nested struct default x')
      scores = capnp_dyn_getp(schema, sink_idx, rnull, 'scores', err)
      call check_(error, err == CAPNP_OK .and. scores%kind == CAPNP_PK_LIST .and. &
                  capnp_list_len(scores) == 3_int64, 'null-ptr: reader list default')
      tags = capnp_dyn_getp(schema, sink_idx, rnull, 'tags', err)
      call check_(error, err == CAPNP_OK .and. tags%kind == CAPNP_PK_NULL, &
                  'null-ptr: reader unset list without default')

      ! Associated builder null handle (unset pointer slot on a builder struct).
      broot = capnp_new_struct(bmsg, 0, 1, err)
      bnull = capnp_getp(broot, 0, err)
      call check_(error, err == CAPNP_OK .and. bnull%kind == CAPNP_PK_NULL .and. &
                  associated(bnull%msg), 'null-ptr: associated builder null')
      call capnp_dyn_get_text(schema, sink_idx, bnull, 'label', s, err)
      call check_(error, err == CAPNP_OK .and. s == 'unnamed', &
                  'null-ptr: builder text default')
      origin = capnp_dyn_getp(schema, sink_idx, bnull, 'origin', err)
      call check_(error, err == CAPNP_OK .and. origin%kind == CAPNP_PK_STRUCT .and. &
                  origin%frozen, 'null-ptr: builder null returns frozen default')
      call capnp_dyn_set_f64(schema, vec_idx, origin, 'x', 9.0_real64, err)
      call check_(error, err /= CAPNP_OK, 'null-ptr: frozen default rejects write')

      call capnp_message_free(rmsg)
      call capnp_message_free(bmsg)
      call capnp_dyn_free(schema)
   end subroutine run_null_struct_pointers

   !> Integer writes on Float32 and Float64 fields convert to float,
   !> matching DynamicStruct::Builder::set in dynamic.c++.
   subroutine run_int_to_float(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema, kschema
      type(capnp_message_t), target :: msg, kmsg
      type(capnp_ptr_t) :: root, sink
      integer(int8), allocatable :: bytes(:)
      integer :: err, idx, sidx
      real(real64) :: x

      call capnp_read_file('test/fixtures/dynamic.cgr.bin', bytes, err)
      if (err /= CAPNP_OK) then
         call skip_test(error, 'fixtures not found (run from project root)')
         return
      end if
      call capnp_dyn_load(schema, bytes, err)
      idx = capnp_dyn_find(schema, 'Scalars', err)
      call capnp_message_init_builder(msg, err)
      root = capnp_dyn_new_root(schema, idx, msg, err)

      call capnp_dyn_set_int(schema, idx, root, 'f32', -42_int64, err)
      x = capnp_dyn_get_f64(schema, idx, root, 'f32', err)
      call check_(error, err == CAPNP_OK .and. abs(x + 42.0_real64) < 1.0e-6_real64, &
                  'int-to-float: negative int to f32')

      call capnp_dyn_set_int(schema, idx, root, 'f64', 1000000_int64, err)
      x = capnp_dyn_get_f64(schema, idx, root, 'f64', err)
      call check_(error, err == CAPNP_OK .and. abs(x - 1000000.0_real64) < 1.0e-12_real64, &
                  'int-to-float: positive int to f64')

      call capnp_read_file('test/fixtures/kitchen.cgr.bin', bytes, err)
      call capnp_dyn_load(kschema, bytes, err)
      sidx = capnp_dyn_find(kschema, 'Sink', err)
      call capnp_message_init_builder(kmsg, err)
      sink = capnp_dyn_new_root(kschema, sidx, kmsg, err)
      call capnp_dyn_set_int(kschema, sidx, sink, 'ratio', 7_int64, err)
      x = capnp_dyn_get_f64(kschema, sidx, sink, 'ratio', err)
      call check_(error, err == CAPNP_OK .and. abs(x - 7.0_real64) < 1.0e-12_real64, &
                  'int-to-float: defaulted f64 set to 7')
      call capnp_dyn_set_int(kschema, sidx, sink, 'ratio', 0_int64, err)
      x = capnp_dyn_get_f64(kschema, sidx, sink, 'ratio', err)
      call check_(error, err == CAPNP_OK .and. abs(x) < 1.0e-12_real64, &
                  'int-to-float: defaulted f64 set to 0')

      call capnp_message_free(kmsg)
      call capnp_dyn_free(kschema)
      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end subroutine run_int_to_float

   !> Union pointer arms share a slot offset; reader default caching must
   !> distinguish arms rather than returning the first arm's default.
   subroutine run_union_pointer_defaults(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: cgr_msg, b1, b2, r1, r2
      type(capnp_ptr_t) :: root, nodes, node, fields, f0, f1
      type(capnp_ptr_t) :: t0, t1, elem, dv0, dv1, def0, def1
      type(capnp_ptr_t) :: s1, s2, rr1, rr2, got0, got1
      integer(int8), allocatable :: cgr_bytes(:), msg1(:), msg2(:)
      integer :: err, idx
      integer(int32) :: v0, v1

      call capnp_message_init_builder(cgr_msg, err)
      root = capnp_new_struct(cgr_msg, 0, 4, err)
      nodes = capnp_new_composite_list(cgr_msg, 1_int64, 6, 6, err)
      node = capnp_list_get_struct(nodes, 0, err)
      call capnp_set_i64(node, 0_int64, 1001_int64, err)
      call capnp_set_text(node, 0, 'UnionDef', err)
      call capnp_set_u16(node, 12_int64, int(NODE_STRUCT, int32), err)
      call capnp_set_u16(node, 14_int64, 1_int32, err)
      call capnp_set_u16(node, 24_int64, 1_int32, err)
      call capnp_set_u16(node, 30_int64, 2_int32, err)
      call capnp_set_u32(node, 32_int64, 0_int64, err)

      fields = capnp_new_composite_list(cgr_msg, 2_int64, 3, 4, err)
      f0 = capnp_list_get_struct(fields, 0, err)
      call capnp_set_text(f0, 0, 'first', err)
      call capnp_set_u16(f0, 0_int64, 0_int32, err)
      call capnp_set_u16(f0, 2_int64, 0_int32, err, default=65535_int32)
      call capnp_set_u32(f0, 4_int64, 0_int64, err)
      call capnp_set_u16(f0, 8_int64, int(FIELD_SLOT, int32), err)
      t0 = capnp_new_struct(cgr_msg, 3, 1, err)
      call capnp_set_u16(t0, 0_int64, int(TYPE_LIST, int32), err)
      elem = capnp_new_struct(cgr_msg, 3, 1, err)
      call capnp_set_u16(elem, 0_int64, int(TYPE_INT32, int32), err)
      call capnp_setp(t0, 0, elem, err)
      call capnp_setp(f0, 2, t0, err)
      dv0 = capnp_new_struct(cgr_msg, 2, 1, err)
      call capnp_set_u16(dv0, 0_int64, int(TYPE_LIST, int32), err)
      def0 = capnp_new_list(cgr_msg, CAPNP_SZ_FOUR, 1_int64, err)
      call capnp_list_set_i32(def0, 0_int64, 111_int32, err)
      call capnp_setp(dv0, 0, def0, err)
      call capnp_setp(f0, 3, dv0, err)

      f1 = capnp_list_get_struct(fields, 1, err)
      call capnp_set_text(f1, 0, 'second', err)
      call capnp_set_u16(f1, 0_int64, 1_int32, err)
      call capnp_set_u16(f1, 2_int64, 1_int32, err, default=65535_int32)
      call capnp_set_u32(f1, 4_int64, 0_int64, err)
      call capnp_set_u16(f1, 8_int64, int(FIELD_SLOT, int32), err)
      t1 = capnp_new_struct(cgr_msg, 3, 1, err)
      call capnp_set_u16(t1, 0_int64, int(TYPE_LIST, int32), err)
      call capnp_setp(t1, 0, elem, err)
      call capnp_setp(f1, 2, t1, err)
      dv1 = capnp_new_struct(cgr_msg, 2, 1, err)
      call capnp_set_u16(dv1, 0_int64, int(TYPE_LIST, int32), err)
      def1 = capnp_new_list(cgr_msg, CAPNP_SZ_FOUR, 1_int64, err)
      call capnp_list_set_i32(def1, 0_int64, 222_int32, err)
      call capnp_setp(dv1, 0, def1, err)
      call capnp_setp(f1, 3, dv1, err)

      call capnp_setp(node, 3, fields, err)
      call capnp_setp(root, 0, nodes, err)
      call capnp_set_root(cgr_msg, root, err)
      call capnp_serialize_bytes(cgr_msg, cgr_bytes, err)
      call check_(error, err == CAPNP_OK, 'union-ptr-def: serialize cgr')

      call capnp_dyn_load(schema, cgr_bytes, err)
      idx = capnp_dyn_find(schema, 'UnionDef', err)
      call check_(error, err == CAPNP_OK .and. idx > 0, 'union-ptr-def: find node')

      call capnp_message_init_builder(b1, err)
      s1 = capnp_dyn_new_root(schema, idx, b1, err)
      call capnp_dyn_select(schema, idx, s1, 'first', err)
      call capnp_serialize_bytes(b1, msg1, err)
      call capnp_deserialize_bytes(msg1, r1, err)
      rr1 = capnp_root(r1, err)

      call capnp_message_init_builder(b2, err)
      s2 = capnp_dyn_new_root(schema, idx, b2, err)
      call capnp_dyn_select(schema, idx, s2, 'second', err)
      call capnp_serialize_bytes(b2, msg2, err)
      call capnp_deserialize_bytes(msg2, r2, err)
      rr2 = capnp_root(r2, err)

      got0 = capnp_dyn_getp(schema, idx, rr1, 'first', err)
      if (err == CAPNP_OK) v0 = capnp_list_get_i32(got0, 0_int64, err)
      call check_(error, err == CAPNP_OK .and. v0 == 111_int32, &
                  'union-ptr-def: first arm default')

      got1 = capnp_dyn_getp(schema, idx, rr2, 'second', err)
      if (err == CAPNP_OK) v1 = capnp_list_get_i32(got1, 0_int64, err)
      call check_(error, err == CAPNP_OK .and. v1 == 222_int32, &
                  'union-ptr-def: second arm default')

      call capnp_message_free(cgr_msg)
      call capnp_message_free(b1)
      call capnp_message_free(b2)
      call capnp_message_free(r1)
      call capnp_message_free(r2)
      call capnp_dyn_free(schema)
   end subroutine run_union_pointer_defaults

   !> capnp_dyn_init_list validates the element struct node id and list
   !> allocation before switching the union discriminant.
   subroutine run_init_list_rollback(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: cgr_msg, bmsg
      type(capnp_ptr_t) :: root, nodes, node, fields, f0, f1, f2
      type(capnp_ptr_t) :: t0, t1, t2, elem1, elem2, s, made
      character(len=:), allocatable :: txt
      integer(int8), allocatable :: cgr_bytes(:)
      integer :: err, idx, tag

      call capnp_message_init_builder(cgr_msg, err)
      root = capnp_new_struct(cgr_msg, 0, 4, err)
      nodes = capnp_new_composite_list(cgr_msg, 1_int64, 6, 6, err)
      node = capnp_list_get_struct(nodes, 0, err)
      call capnp_set_i64(node, 0_int64, 1002_int64, err)
      call capnp_set_text(node, 0, 'InitListUnion', err)
      call capnp_set_u16(node, 12_int64, int(NODE_STRUCT, int32), err)
      call capnp_set_u16(node, 14_int64, 1_int32, err)
      call capnp_set_u16(node, 24_int64, 1_int32, err)
      call capnp_set_u16(node, 30_int64, 3_int32, err)
      call capnp_set_u32(node, 32_int64, 0_int64, err)

      fields = capnp_new_composite_list(cgr_msg, 3_int64, 3, 4, err)
      f0 = capnp_list_get_struct(fields, 0, err)
      call capnp_set_text(f0, 0, 'label', err)
      call capnp_set_u16(f0, 0_int64, 0_int32, err)
      call capnp_set_u16(f0, 2_int64, 0_int32, err, default=65535_int32)
      call capnp_set_u32(f0, 4_int64, 0_int64, err)
      call capnp_set_u16(f0, 8_int64, int(FIELD_SLOT, int32), err)
      t0 = capnp_new_struct(cgr_msg, 3, 1, err)
      call capnp_set_u16(t0, 0_int64, int(TYPE_TEXT, int32), err)
      call capnp_setp(f0, 2, t0, err)

      f1 = capnp_list_get_struct(fields, 1, err)
      call capnp_set_text(f1, 0, 'missing', err)
      call capnp_set_u16(f1, 0_int64, 1_int32, err)
      call capnp_set_u16(f1, 2_int64, 1_int32, err, default=65535_int32)
      call capnp_set_u32(f1, 4_int64, 0_int64, err)
      call capnp_set_u16(f1, 8_int64, int(FIELD_SLOT, int32), err)
      t1 = capnp_new_struct(cgr_msg, 3, 1, err)
      call capnp_set_u16(t1, 0_int64, int(TYPE_LIST, int32), err)
      elem1 = capnp_new_struct(cgr_msg, 3, 1, err)
      call capnp_set_u16(elem1, 0_int64, int(TYPE_STRUCT, int32), err)
      call capnp_set_i64(elem1, 8_int64, 9999_int64, err)
      call capnp_setp(t1, 0, elem1, err)
      call capnp_setp(f1, 2, t1, err)

      f2 = capnp_list_get_struct(fields, 2, err)
      call capnp_set_text(f2, 0, 'nums', err)
      call capnp_set_u16(f2, 0_int64, 2_int32, err)
      call capnp_set_u16(f2, 2_int64, 2_int32, err, default=65535_int32)
      call capnp_set_u32(f2, 4_int64, 0_int64, err)
      call capnp_set_u16(f2, 8_int64, int(FIELD_SLOT, int32), err)
      t2 = capnp_new_struct(cgr_msg, 3, 1, err)
      call capnp_set_u16(t2, 0_int64, int(TYPE_LIST, int32), err)
      elem2 = capnp_new_struct(cgr_msg, 3, 1, err)
      call capnp_set_u16(elem2, 0_int64, int(TYPE_INT32, int32), err)
      call capnp_setp(t2, 0, elem2, err)
      call capnp_setp(f2, 2, t2, err)

      call capnp_setp(node, 3, fields, err)
      call capnp_setp(root, 0, nodes, err)
      call capnp_set_root(cgr_msg, root, err)
      call capnp_serialize_bytes(cgr_msg, cgr_bytes, err)
      call check_(error, err == CAPNP_OK, 'init-list-rollback: serialize cgr')

      call capnp_dyn_load(schema, cgr_bytes, err)
      idx = capnp_dyn_find(schema, 'InitListUnion', err)
      call check_(error, err == CAPNP_OK .and. idx > 0, 'init-list-rollback: find node')

      call capnp_message_init_builder(bmsg, err)
      s = capnp_dyn_new_root(schema, idx, bmsg, err)
      call capnp_dyn_set_text(schema, idx, s, 'label', 'keep', err)
      call check_(error, err == CAPNP_OK, 'init-list-rollback: seed label')

      made = capnp_dyn_init_list(schema, idx, s, 'missing', 2_int64, err)
      call check_(error, err == CAPNP_ERR_ARG .and. made%kind == CAPNP_PK_NULL, &
                  'init-list-rollback: missing struct node rejected')
      tag = capnp_dyn_which(schema, idx, s, err)
      call capnp_dyn_get_text(schema, idx, s, 'label', txt, err)
      call check_(error, err == CAPNP_OK .and. tag == 0 .and. txt == 'keep', &
                  'init-list-rollback: missing node leaves prior arm intact')

      made = capnp_dyn_init_list(schema, idx, s, 'nums', 2000000000_int64, err)
      call check_(error, err == CAPNP_ERR_ARG .and. made%kind == CAPNP_PK_NULL, &
                  'init-list-rollback: oversize list rejected')
      tag = capnp_dyn_which(schema, idx, s, err)
      call capnp_dyn_get_text(schema, idx, s, 'label', txt, err)
      call check_(error, err == CAPNP_OK .and. tag == 0 .and. txt == 'keep', &
                  'init-list-rollback: failed alloc leaves prior arm intact')

      call capnp_message_free(cgr_msg)
      call capnp_message_free(bmsg)
      call capnp_dyn_free(schema)
   end subroutine run_init_list_rollback

   !> Every typed setter moves the union arm before it rejects a value,
   !> as dynamic.c++ Builder::set does. The arm data is not cleared.
   subroutine run_union_rejected_set(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: choice, raw
      integer(int8), allocatable :: bytes(:)
      character(len=:), allocatable :: s
      integer :: err, werr, cidx

      call capnp_read_file('test/fixtures/dynamic.cgr.bin', bytes, err)
      if (err /= CAPNP_OK) then
         call skip_test(error, 'fixtures not found (run from project root)')
         return
      end if
      call capnp_dyn_load(schema, bytes, err)
      cidx = capnp_dyn_find(schema, 'Dynamic.choice', err)
      call capnp_message_init_builder(msg, err)
      choice = capnp_dyn_new_root(schema, cidx, msg, err)
      call capnp_dyn_set_text(schema, cidx, choice, 'label', 'keep', err)
      call check_(error, err == CAPNP_OK, 'rejected-set: seed label')

      call capnp_dyn_set_text(schema, cidx, choice, 'number', 'x', err)
      call check_(error, err == CAPNP_ERR_KIND .and. &
                  capnp_dyn_which(schema, cidx, choice, werr) == 0, &
                  'rejected-set: text into number')
      call capnp_dyn_set_data(schema, cidx, choice, 'label', [1_int8, 2_int8], err)
      call check_(error, err == CAPNP_ERR_KIND .and. &
                  capnp_dyn_which(schema, cidx, choice, werr) == 1, &
                  'rejected-set: data into label')
      call capnp_dyn_set_bool(schema, cidx, choice, 'number', .true., err)
      call check_(error, err == CAPNP_ERR_KIND .and. &
                  capnp_dyn_which(schema, cidx, choice, werr) == 0, &
                  'rejected-set: bool into number')
      call capnp_dyn_set_f64(schema, cidx, choice, 'label', 1.5_real64, err)
      call check_(error, err == CAPNP_ERR_KIND .and. &
                  capnp_dyn_which(schema, cidx, choice, werr) == 1, &
                  'rejected-set: float into label')
      raw = capnp_new_list(msg, CAPNP_SZ_BYTE, 2_int64, err)
      call capnp_dyn_setp(schema, cidx, choice, 'items', raw, err)
      call check_(error, err == CAPNP_ERR_KIND .and. &
                  capnp_dyn_which(schema, cidx, choice, werr) == 2, &
                  'rejected-set: byte list into items')
      call capnp_dyn_set_int(schema, cidx, choice, 'label', 5_int64, err)
      call check_(error, err == CAPNP_ERR_KIND .and. &
                  capnp_dyn_which(schema, cidx, choice, werr) == 1, &
                  'rejected-set: int into label')
      call capnp_dyn_get_text(schema, cidx, choice, 'label', s, err)
      call check_(error, err == CAPNP_OK .and. s == 'keep', &
                  'rejected-set: label data survives every rejected write')
      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end subroutine run_union_rejected_set

   !> A successful union write only moves the discriminant, as dynamic.c++
   !> setInUnion and the generated setters do. The previous arm's data and
   !> pointer slots keep their bits.
   subroutine run_union_set_keeps_arm(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg, gmsg
      type(capnp_ptr_t) :: choice, root, made
      type(dynamic_t) :: gen
      integer(int8), allocatable :: bytes(:)
      character(len=:), allocatable :: s
      character(len=3) :: num
      integer :: err, cidx, idx, gidx, last, i

      call capnp_read_file('test/fixtures/dynamic.cgr.bin', bytes, err)
      if (err /= CAPNP_OK) then
         call skip_test(error, 'fixtures not found (run from project root)')
         return
      end if
      call capnp_dyn_load(schema, bytes, err)
      cidx = capnp_dyn_find(schema, 'Dynamic.choice', err)
      call capnp_message_init_builder(msg, err)

      choice = capnp_dyn_new_root(schema, cidx, msg, err)
      call capnp_dyn_set_int(schema, cidx, choice, 'number', 7_int64, err)
      call capnp_dyn_set_text(schema, cidx, choice, 'label', 'x', err)
      call check_(error, err == CAPNP_OK .and. capnp_which(choice, 2) == 1 .and. &
                  capnp_get_i32(choice, 0_int64) == 7_int32, &
                  'keeps-arm: number bits survive a label write')
      made = capnp_dyn_init_list(schema, cidx, choice, 'items', 2_int64, err)
      call check_(error, err == CAPNP_OK .and. capnp_which(choice, 2) == 2 .and. &
                  capnp_get_i32(choice, 0_int64) == 7_int32, &
                  'keeps-arm: number bits survive init of items')

      choice = capnp_dyn_new_root(schema, cidx, msg, err)
      call capnp_dyn_set_text(schema, cidx, choice, 'label', 'keep', err)
      call capnp_dyn_set_int(schema, cidx, choice, 'number', 7_int64, err)
      call capnp_get_text(choice, 3, s, err)
      call check_(error, err == CAPNP_OK .and. capnp_which(choice, 2) == 0 .and. &
                  s == 'keep', 'keeps-arm: label pointer survives a number write')

      call capnp_message_init_builder(gmsg, err)
      gen = dynamic_new_root(gmsg, err)
      call dynamic_choice_label_set(gen, 'keep', err)
      call dynamic_choice_number_set(gen, 7_int32, err)
      call capnp_get_text(gen%p, 3, s, err)
      call check_(error, err == CAPNP_OK .and. s == 'keep', &
                  'keeps-arm: generated setters agree')
      call capnp_message_free(gmsg)

      idx = capnp_dyn_find(schema, 'NestedUnion', err)
      gidx = capnp_dyn_find(schema, 'NestedUnion.g', err)
      root = capnp_dyn_new_root(schema, idx, msg, err)
      call capnp_dyn_select(schema, idx, root, 'g', err)
      call capnp_dyn_set_text(schema, gidx, root, 'y', 'hello', err)
      call capnp_dyn_set_int(schema, idx, root, 'a', 5_int64, err)
      call capnp_dyn_get_text(schema, gidx, root, 'y', s, err)
      call check_(error, err == CAPNP_OK .and. capnp_dyn_which(schema, idx, root, err) == 0 .and. &
                  capnp_dyn_which(schema, gidx, root, err) == 1 .and. s == 'hello', &
                  'keeps-arm: inner union survives an outer write')

      idx = capnp_dyn_find(schema, 'DeepGroups', err)
      s = 'DeepGroups'
      do i = 1, 10
         write (num, '(i0)') i
         s = s//'.g'//trim(num)
      end do
      last = capnp_dyn_find(schema, s, err)
      root = capnp_dyn_new_root(schema, idx, msg, err)
      call capnp_dyn_select(schema, idx, root, 'g1', err)
      call capnp_dyn_set_int(schema, last, root, 'v10', 77_int64, err)
      call capnp_dyn_set_int(schema, idx, root, 'a', 5_int64, err)
      call check_(error, capnp_dyn_get_int(schema, last, root, 'v10', err) == 77_int64, &
                  'keeps-arm: deep group bits survive an outer write')

      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end subroutine run_union_set_keeps_arm

   !> A double written to an integer field converts as dynamic.c++
   !> checkRoundTripFromFloat does. Enum and Bool still reject it.
   subroutine run_f64_to_int(error)
      use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_positive_inf
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root, choice
      integer(int8), allocatable :: bytes(:)
      character(len=:), allocatable :: s
      real(real64), parameter :: two63 = 9223372036854775808.0_real64
      real(real64) :: nan, inf
      integer :: err, idx, cidx

      call capnp_read_file('test/fixtures/dynamic.cgr.bin', bytes, err)
      if (err /= CAPNP_OK) then
         call skip_test(error, 'fixtures not found (run from project root)')
         return
      end if
      nan = ieee_value(nan, ieee_quiet_nan)
      inf = ieee_value(inf, ieee_positive_inf)
      call capnp_dyn_load(schema, bytes, err)
      idx = capnp_dyn_find(schema, 'Scalars', err)
      cidx = capnp_dyn_find(schema, 'Dynamic.choice', err)
      call capnp_message_init_builder(msg, err)
      root = capnp_dyn_new_root(schema, idx, msg, err)

      call expect('i8', 2.0_real64, CAPNP_OK, 2_int64, 'f64-int: 2.0 stores 2')
      call expect('i8', 1.5_real64, CAPNP_ERR_ARG, 2_int64, 'f64-int: 1.5 rejected')
      call expect('i8', -1.5_real64, CAPNP_ERR_ARG, 2_int64, 'f64-int: -1.5 rejected')
      call expect('i8', 127.0_real64, CAPNP_OK, 127_int64, 'f64-int: int8 max')
      call expect('i8', 128.0_real64, CAPNP_ERR_ARG, 127_int64, 'f64-int: int8 max + 1')
      call expect('i8', -128.0_real64, CAPNP_OK, -128_int64, 'f64-int: int8 min')
      call expect('i8', -129.0_real64, CAPNP_ERR_ARG, -128_int64, 'f64-int: int8 min - 1')
      call expect('u8', 255.0_real64, CAPNP_OK, 255_int64, 'f64-int: uint8 max')
      call expect('u8', 256.0_real64, CAPNP_ERR_ARG, 255_int64, 'f64-int: uint8 max + 1')
      call expect('u8', -0.0_real64, CAPNP_OK, 0_int64, 'f64-int: -0.0 stores 0')
      call expect('u16', -1.0_real64, CAPNP_ERR_ARG, 0_int64, 'f64-int: unsigned rejects -1')
      call expect('i16', -32768.0_real64, CAPNP_OK, -32768_int64, 'f64-int: int16 min')
      call expect('u32', 4294967295.0_real64, CAPNP_OK, 4294967295_int64, 'f64-int: uint32 max')
      call expect('u32', 4294967296.0_real64, CAPNP_ERR_ARG, 4294967295_int64, &
                  'f64-int: uint32 max + 1')
      call expect('i32', -2147483648.0_real64, CAPNP_OK, -2147483648_int64, 'f64-int: int32 min')
      call expect('i32', 1.0e-300_real64, CAPNP_ERR_ARG, -2147483648_int64, &
                  'f64-int: tiny fraction rejected')
      call expect('i32', nan, CAPNP_ERR_ARG, -2147483648_int64, 'f64-int: NaN rejected')
      call expect('i32', inf, CAPNP_ERR_ARG, -2147483648_int64, 'f64-int: infinity rejected')
      call expect('i64', -two63, CAPNP_OK, -huge(0_int64) - 1_int64, 'f64-int: int64 min')
      call expect('i64', two63, CAPNP_ERR_ARG, -huge(0_int64) - 1_int64, &
                  'f64-int: 2**63 rejected for int64')
      call expect('i64', two63 - 1024.0_real64, CAPNP_OK, 9223372036854774784_int64, &
                  'f64-int: largest int64 double')
      call expect('u64', two63, CAPNP_OK, -huge(0_int64) - 1_int64, &
                  'f64-int: 2**63 fits uint64')
      call expect('u64', 2.0_real64*two63 - 2048.0_real64, CAPNP_OK, -2048_int64, &
                  'f64-int: largest uint64 double')
      call expect('u64', 2.0_real64*two63, CAPNP_ERR_ARG, -2048_int64, &
                  'f64-int: 2**64 rejected for uint64')
      call capnp_dyn_set_int(schema, idx, root, 'hue', 1_int64, err)
      call expect('hue', 2.0_real64, CAPNP_ERR_KIND, 1_int64, 'f64-int: enum rejects a double')
      call capnp_dyn_set_f64(schema, idx, root, 'b', 1.0_real64, err)
      call check_(error, err == CAPNP_ERR_KIND, 'f64-int: bool rejects a double')

      choice = capnp_dyn_new_root(schema, cidx, msg, err)
      call capnp_dyn_set_text(schema, cidx, choice, 'label', 'keep', err)
      call capnp_dyn_set_f64(schema, cidx, choice, 'number', 1.5_real64, err)
      call capnp_get_text(choice, 3, s, err)
      call check_(error, capnp_which(choice, 2) == 0 .and. s == 'keep' .and. &
                  capnp_get_i32(choice, 0_int64) == 0_int32, &
                  'f64-int: rejected union write selects the arm only')
      call capnp_dyn_set_f64(schema, cidx, choice, 'number', 2.0_real64, err)
      call check_(error, err == CAPNP_OK .and. &
                  capnp_dyn_get_int(schema, cidx, choice, 'number', err) == 2_int64, &
                  'f64-int: union member stores 2')

      call capnp_message_free(msg)
      call capnp_dyn_free(schema)

   contains

      subroutine expect(fname, v, code, stored, label)
         character(len=*), intent(in) :: fname, label
         real(real64), intent(in) :: v
         integer, intent(in) :: code
         integer(int64), intent(in) :: stored
         integer :: werr
         call capnp_dyn_set_f64(schema, idx, root, fname, v, werr)
         call check_(error, werr == code .and. &
                     capnp_dyn_get_int(schema, idx, root, fname, err) == stored, label)
      end subroutine expect
   end subroutine run_f64_to_int

   !> Text written to Enum, Data and AnyPointer fields, and any pointer
   !> kind written to a constrained AnyPointer, as dynamic.c++ set allows.
   subroutine run_text_coerce(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root, dyn, q, lst, st
      integer(int8), allocatable :: bytes(:), got(:)
      character(len=:), allocatable :: s
      integer :: err, idx, didx

      call capnp_read_file('test/fixtures/dynamic.cgr.bin', bytes, err)
      if (err /= CAPNP_OK) then
         call skip_test(error, 'fixtures not found (run from project root)')
         return
      end if
      call capnp_dyn_load(schema, bytes, err)
      idx = capnp_dyn_find(schema, 'Scalars', err)
      didx = capnp_dyn_find(schema, 'Dynamic', err)
      call capnp_message_init_builder(msg, err)
      root = capnp_dyn_new_root(schema, idx, msg, err)

      call capnp_dyn_set_text(schema, idx, root, 'hue', 'green', err)
      call check_(error, err == CAPNP_OK .and. &
                  capnp_dyn_get_int(schema, idx, root, 'hue', err) == 1_int64, &
                  'text-coerce: enumerant name stores its ordinal')
      call capnp_dyn_set_text(schema, idx, root, 'hue', 'purple', err)
      call check_(error, err == CAPNP_ERR_ARG .and. &
                  capnp_dyn_get_int(schema, idx, root, 'hue', err) == 1_int64, &
                  'text-coerce: unknown enumerant rejected')
      call capnp_dyn_set_text(schema, idx, root, 'hue', 'blue ', err)
      call check_(error, err == CAPNP_ERR_ARG, 'text-coerce: enumerant name matches exactly')

      call capnp_dyn_set_text(schema, idx, root, 'blob', 'ab', err)
      call capnp_dyn_get_data(schema, idx, root, 'blob', got, err)
      call check_(error, err == CAPNP_OK .and. size(got) == 2, 'text-coerce: data has no NUL')
      if (allocated(error)) return
      call check_(error, all(got == [int(iachar('a'), int8), int(iachar('b'), int8)]), &
                  'text-coerce: data bytes')
      call capnp_dyn_set_text(schema, idx, root, 'blob', '', err)
      q = capnp_getp(root, 1, err)
      call check_(error, err == CAPNP_OK .and. q%kind == CAPNP_PK_LIST .and. q%nelem == 0_int64, &
                  'text-coerce: empty text stores empty data')

      dyn = capnp_dyn_new_root(schema, didx, msg, err)
      call capnp_dyn_set_text(schema, didx, dyn, 'anyStruct', 'hi', err)
      call capnp_get_text(dyn, 1, s, err)
      call check_(error, err == CAPNP_OK .and. s == 'hi', 'text-coerce: AnyStruct takes text')
      call capnp_dyn_set_text(schema, didx, dyn, 'capability', 'hi', err)
      call capnp_get_text(dyn, 0, s, err)
      call check_(error, err == CAPNP_OK .and. s == 'hi', 'text-coerce: Capability takes text')

      st = capnp_new_struct(msg, 1, 0, err)
      lst = capnp_new_list(msg, CAPNP_SZ_FOUR, 3_int64, err)
      call capnp_dyn_setp(schema, didx, dyn, 'anyList', st, err)
      q = capnp_getp(dyn, 2, err)
      call check_(error, err == CAPNP_OK .and. q%kind == CAPNP_PK_STRUCT, &
                  'text-coerce: AnyList takes a struct')
      call capnp_dyn_setp(schema, didx, dyn, 'anyStruct', lst, err)
      q = capnp_getp(dyn, 1, err)
      call check_(error, err == CAPNP_OK .and. q%kind == CAPNP_PK_LIST, &
                  'text-coerce: AnyStruct takes a list')
      call capnp_dyn_setp(schema, didx, dyn, 'capability', st, err)
      q = capnp_getp(dyn, 0, err)
      call check_(error, err == CAPNP_OK .and. q%kind == CAPNP_PK_STRUCT, &
                  'text-coerce: Capability takes a struct')

      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end subroutine run_text_coerce

end module test_dynamic
