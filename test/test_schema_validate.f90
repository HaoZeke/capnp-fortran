!> Negative loads built by mutating a valid CodeGeneratorRequest.
module test_schema_validate
   use testdrive, only: unittest_type, error_type, check
   use, intrinsic :: iso_fortran_env, only: int8, int64
   use capnp
   use capnp_schema
   use capnp_dynamic
   implicit none
   private
   public :: collect_schema_validate

contains

   subroutine collect_schema_validate(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      allocate (testsuite(4))
      testsuite(1)%name = "offsets"
      testsuite(1)%test => run_offsets
      testsuite(2)%name = "members"
      testsuite(2)%test => run_members
      testsuite(3)%name = "kinds"
      testsuite(3)%test => run_kinds
      testsuite(4)%name = "fixtures"
      testsuite(4)%test => run_fixtures
   end subroutine collect_schema_validate

   subroutine check_(error, cond, name)
      type(error_type), allocatable, intent(inout) :: error
      logical, intent(in) :: cond
      character(len=*), intent(in) :: name
      if (allocated(error)) return
      call check(error, cond, name)
   end subroutine check_

   subroutine load_fix(schema, path, err)
      type(capnp_dyn_schema_t), intent(inout) :: schema
      character(len=*), intent(in) :: path
      integer, intent(out) :: err
      integer(int8), allocatable :: bytes(:)
      call capnp_dyn_free(schema)
      call capnp_read_file(path, bytes, err)
      if (err /= CAPNP_OK) return
      call capnp_dyn_load(schema, bytes, err)
   end subroutine load_fix

   subroutine reload(schema, err)
      type(capnp_dyn_schema_t), intent(inout) :: schema
      integer, intent(out) :: err
      integer(int8), allocatable :: bytes(:)
      call capnp_serialize_bytes(schema%cgr, bytes, err)
      if (err /= CAPNP_OK) return
      call capnp_dyn_free(schema)
      call capnp_dyn_load(schema, bytes, err)
   end subroutine reload

   subroutine field_by_name(node, name, field, found, err)
      type(capnp_ptr_t), intent(in) :: node
      character(len=*), intent(in) :: name
      type(capnp_ptr_t), intent(out) :: field
      logical, intent(out) :: found
      integer, intent(out) :: err
      type(capnp_ptr_t) :: list, f
      character(len=:), allocatable :: got
      integer :: i, n
      found = .false.
      field = capnp_ptr_t()
      list = node_struct_fields(node, err)
      if (err /= CAPNP_OK) return
      if (list%kind /= CAPNP_PK_LIST) return
      n = int(capnp_list_len(list))
      do i = 0, n - 1
         f = capnp_list_get_struct(list, i, err)
         if (err /= CAPNP_OK) return
         call field_name(f, got, err)
         if (err /= CAPNP_OK) return
         if (len(got) == len(name) .and. got == name) then
            field = f
            found = .true.
            return
         end if
      end do
   end subroutine field_by_name

   subroutine rename_field(field, newname, err)
      type(capnp_ptr_t), intent(in) :: field
      character(len=*), intent(in) :: newname
      integer, intent(out) :: err
      type(capnp_ptr_t) :: q
      integer :: i
      q = capnp_getp(field, 0, err)
      if (err /= CAPNP_OK) return
      if (q%kind /= CAPNP_PK_LIST .or. q%nelem /= len(newname) + 1) then
         err = CAPNP_ERR_ARG
         return
      end if
      do i = 1, len(newname)
         q%msg%segs(q%seg)%bytes(q%off + i - 1) = int(iachar(newname(i:i)), int8)
      end do
   end subroutine rename_field

   subroutine expect_arg(error, schema, label)
      type(error_type), allocatable, intent(inout) :: error
      type(capnp_dyn_schema_t), intent(inout) :: schema
      character(len=*), intent(in) :: label
      integer :: err
      if (allocated(error)) return
      call reload(schema, err)
      call check_(error, err == CAPNP_ERR_ARG .and. .not. schema%loaded, label)
   end subroutine expect_arg

   subroutine expect_ok(error, schema, label)
      type(error_type), allocatable, intent(inout) :: error
      type(capnp_dyn_schema_t), intent(inout) :: schema
      character(len=*), intent(in) :: label
      integer :: err
      if (allocated(error)) return
      call reload(schema, err)
      call check_(error, err == CAPNP_OK .and. schema%loaded, label)
   end subroutine expect_ok

   subroutine run_offsets(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_ptr_t) :: node, field
      logical :: found
      integer :: err, idx, dw
      call load_fix(schema, 'test/fixtures/kitchen.cgr.bin', err)
      call check_(error, err == CAPNP_OK, 'offsets: kitchen loads')
      if (allocated(error)) return
      call expect_ok(error, schema, 'kitchen-roundtrip')
      call capnp_dyn_free(schema)

      call load_fix(schema, 'test/fixtures/kitchen.cgr.bin', err)
      idx = capnp_dyn_find(schema, 'Sink', err)
      call check_(error, err == CAPNP_OK .and. idx > 0, 'offsets: Sink')
      if (allocated(error)) return
      node = schema%nodes(idx)
      call capnp_set_u16(node, 14_int64, 0_int32, err)
      call check_(error, err == CAPNP_OK, 'offsets: zero data words')
      call expect_arg(error, schema, 'data-words')
      call capnp_dyn_free(schema)

      call load_fix(schema, 'test/fixtures/kitchen.cgr.bin', err)
      idx = capnp_dyn_find(schema, 'Sink', err)
      node = schema%nodes(idx)
      call capnp_set_u16(node, 24_int64, 0_int32, err)
      call expect_arg(error, schema, 'pointer-count')
      call capnp_dyn_free(schema)

      call load_fix(schema, 'test/fixtures/kitchen.cgr.bin', err)
      idx = capnp_dyn_find(schema, 'Sink', err)
      node = schema%nodes(idx)
      dw = node_struct_data_words(node)
      call field_by_name(node, 'flag', field, found, err)
      call check_(error, found .and. err == CAPNP_OK, 'offsets: flag')
      if (allocated(error)) return
      call capnp_set_u32(field, 4_int64, int(dw, int64)*64_int64, err)
      call expect_arg(error, schema, 'bool-offset')
      call capnp_dyn_free(schema)

      call load_fix(schema, 'test/fixtures/addressbook.cgr.bin', err)
      idx = capnp_dyn_find(schema, 'Person.employment', err)
      call check_(error, idx > 0, 'offsets: employment')
      if (allocated(error)) return
      node = schema%nodes(idx)
      call capnp_set_u32(node, 32_int64, 100_int64, err)
      call expect_arg(error, schema, 'disc-offset')
      call capnp_dyn_free(schema)

      call load_fix(schema, 'test/fixtures/addressbook.cgr.bin', err)
      idx = capnp_dyn_find(schema, 'Person.employment', err)
      node = schema%nodes(idx)
      call capnp_set_u16(node, 30_int64, 1_int32, err)
      call expect_arg(error, schema, 'disc-count-one')
      call capnp_dyn_free(schema)

      call load_fix(schema, 'test/fixtures/addressbook.cgr.bin', err)
      idx = capnp_dyn_find(schema, 'Person', err)
      node = schema%nodes(idx)
      call capnp_set_u16(node, 30_int64, 5_int32, err)
      call expect_arg(error, schema, 'disc-count-mismatch')
      call capnp_dyn_free(schema)

      call load_fix(schema, 'test/fixtures/dynamic.cgr.bin', err)
      idx = capnp_dyn_find(schema, 'Scalars', err)
      call check_(error, idx > 0, 'offsets: Scalars')
      if (allocated(error)) return
      node = schema%nodes(idx)
      call field_by_name(node, 'v', field, found, err)
      call check_(error, found, 'offsets: void field')
      if (allocated(error)) return
      call capnp_set_u32(field, 4_int64, 4000000000_int64, err)
      call expect_ok(error, schema, 'void-offset')
      call capnp_dyn_free(schema)
   end subroutine run_offsets


   subroutine put_named(item, name, ord, err)
      type(capnp_ptr_t), intent(in) :: item
      character(len=*), intent(in) :: name
      integer(int32), intent(in) :: ord
      integer, intent(inout) :: err
      if (err /= CAPNP_OK) return
      call capnp_set_text(item, 0, name, err)
      if (err /= CAPNP_OK) return
      call capnp_set_u16(item, 0_int64, ord, err)
   end subroutine put_named

   subroutine load_blank_names(error, schema)
      type(error_type), allocatable, intent(inout) :: error
      type(capnp_dyn_schema_t), intent(inout) :: schema
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root, nodes, node, members, item
      integer(int8), allocatable :: bytes(:)
      integer :: err
      if (allocated(error)) return
      call capnp_message_init_builder(msg, err)
      if (err == CAPNP_OK) root = capnp_new_struct(msg, 0, 4, err)
      if (err == CAPNP_OK) call capnp_set_root(msg, root, err)
      if (err == CAPNP_OK) nodes = capnp_new_composite_list(msg, 3_int64, 6, 6, err)
      if (err == CAPNP_OK) call capnp_setp(root, 0, nodes, err)

      if (err == CAPNP_OK) node = capnp_list_get_struct(nodes, 0, err)
      if (err == CAPNP_OK) call capnp_set_i64(node, 0_int64, 11_int64, err)
      if (err == CAPNP_OK) call capnp_set_u16(node, 12_int64, 1_int32, err)
      if (err == CAPNP_OK) members = capnp_new_composite_list(msg, 2_int64, 3, 4, err)
      if (err == CAPNP_OK) call capnp_setp(node, 3, members, err)
      if (err == CAPNP_OK) item = capnp_list_get_struct(members, 0, err)
      if (err == CAPNP_OK) call put_named(item, 'a', 0_int32, err)
      if (err == CAPNP_OK) item = capnp_list_get_struct(members, 1, err)
      if (err == CAPNP_OK) call put_named(item, 'a ', 1_int32, err)

      if (err == CAPNP_OK) node = capnp_list_get_struct(nodes, 1, err)
      if (err == CAPNP_OK) call capnp_set_i64(node, 0_int64, 12_int64, err)
      if (err == CAPNP_OK) call capnp_set_u16(node, 12_int64, 2_int32, err)
      if (err == CAPNP_OK) members = capnp_new_composite_list(msg, 2_int64, 1, 2, err)
      if (err == CAPNP_OK) call capnp_setp(node, 3, members, err)
      if (err == CAPNP_OK) item = capnp_list_get_struct(members, 0, err)
      if (err == CAPNP_OK) call put_named(item, 'a', 0_int32, err)
      if (err == CAPNP_OK) item = capnp_list_get_struct(members, 1, err)
      if (err == CAPNP_OK) call put_named(item, 'a ', 1_int32, err)

      if (err == CAPNP_OK) node = capnp_list_get_struct(nodes, 2, err)
      if (err == CAPNP_OK) call capnp_set_i64(node, 0_int64, 13_int64, err)
      if (err == CAPNP_OK) call capnp_set_u16(node, 12_int64, 3_int32, err)
      if (err == CAPNP_OK) members = capnp_new_composite_list(msg, 2_int64, 3, 4, err)
      if (err == CAPNP_OK) call capnp_setp(node, 3, members, err)
      if (err == CAPNP_OK) item = capnp_list_get_struct(members, 0, err)
      if (err == CAPNP_OK) call put_named(item, 'a', 0_int32, err)
      if (err == CAPNP_OK) item = capnp_list_get_struct(members, 1, err)
      if (err == CAPNP_OK) call put_named(item, 'a ', 1_int32, err)

      if (err == CAPNP_OK) call capnp_serialize_bytes(msg, bytes, err)
      call capnp_message_free(msg)
      call check_(error, err == CAPNP_OK, 'blank: build')
      if (allocated(error)) return
      call capnp_dyn_free(schema)
      call capnp_dyn_load(schema, bytes, err)
      call check_(error, err == CAPNP_OK .and. schema%loaded, 'blank: load')
      if (allocated(error)) return
      call expect_ok(error, schema, 'blank-names')
      call capnp_dyn_free(schema)
   end subroutine load_blank_names

   subroutine run_members(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_ptr_t) :: node, field, items, item
      logical :: found
      integer :: err, idx, which
      call load_fix(schema, 'test/fixtures/dynamic.cgr.bin', err)
      idx = capnp_dyn_find(schema, 'Scalars', err)
      call check_(error, idx > 0, 'members: Scalars')
      if (allocated(error)) return
      node = schema%nodes(idx)
      call field_by_name(node, 'u8', field, found, err)
      call check_(error, found, 'members: u8')
      if (allocated(error)) return
      call rename_field(field, 'i8', err)
      call check_(error, err == CAPNP_OK, 'members: rename u8')
      call expect_arg(error, schema, 'dup-name')
      call capnp_dyn_free(schema)

      call load_fix(schema, 'test/fixtures/kitchen.cgr.bin', err)
      idx = capnp_dyn_find(schema, 'Sink', err)
      node = schema%nodes(idx)
      call field_by_name(node, 'count', field, found, err)
      call check_(error, found, 'members: count')
      if (allocated(error)) return
      call capnp_set_u16(field, 0_int64, 0_int32, err)
      call expect_arg(error, schema, 'code-order')
      call capnp_dyn_free(schema)

      call load_fix(schema, 'test/fixtures/kitchen.cgr.bin', err)
      idx = capnp_dyn_find(schema, 'Sink', err)
      node = schema%nodes(idx)
      call field_by_name(node, 'count', field, found, err)
      which = int(capnp_get_u16(field, 10_int64))
      call check_(error, which == 1, 'members: explicit ordinal')
      if (allocated(error)) return
      call capnp_set_u16(field, 12_int64, 0_int32, err)
      call expect_arg(error, schema, 'ordinal')
      call capnp_dyn_free(schema)

      call load_fix(schema, 'test/fixtures/dynamic.cgr.bin', err)
      idx = capnp_dyn_find(schema, 'Hue', err)
      call check_(error, idx > 0, 'members: Hue')
      if (allocated(error)) return
      node = schema%nodes(idx)
      items = node_enumerants(node, err)
      item = capnp_list_get_struct(items, 1, err)
      call capnp_set_u16(item, 0_int64, 0_int32, err)
      call expect_arg(error, schema, 'enum-order')
      call capnp_dyn_free(schema)

      call load_fix(schema, 'test/fixtures/dynamic.cgr.bin', err)
      idx = capnp_dyn_find(schema, 'AnnotatedService', err)
      call check_(error, idx > 0, 'members: AnnotatedService')
      if (allocated(error)) return
      node = schema%nodes(idx)
      items = node_interface_methods(node, err)
      item = capnp_list_get_struct(items, 0, err)
      call capnp_set_u16(item, 0_int64, 5_int32, err)
      call expect_arg(error, schema, 'method-order')
      call capnp_dyn_free(schema)

      call load_blank_names(error, schema)
   end subroutine run_members

   subroutine run_kinds(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_ptr_t) :: node, field, typ, val, brand, scopes, scope
      type(capnp_ptr_t) :: binds, binding, btype, items, item
      logical :: found
      integer :: err, idx, hue, n
      integer(int64) :: hue_id
      call load_fix(schema, 'test/fixtures/kitchen.cgr.bin', err)
      idx = capnp_dyn_find(schema, 'answer', err)
      call check_(error, idx > 0, 'kinds: answer')
      if (allocated(error)) return
      node = schema%nodes(idx)
      val = node_const_value(node, err)
      call capnp_set_u16(val, 0_int64, TYPE_BOOL, err)
      call expect_arg(error, schema, 'const-value')
      call capnp_dyn_free(schema)

      call load_fix(schema, 'test/fixtures/dynamic.cgr.bin', err)
      hue = capnp_dyn_find(schema, 'Hue', err)
      call check_(error, hue > 0, 'kinds: Hue id')
      if (allocated(error)) return
      hue_id = node_id(schema%nodes(hue))
      idx = capnp_dyn_find(schema, 'methodNote', err)
      call check_(error, idx > 0, 'kinds: methodNote')
      if (allocated(error)) return
      node = schema%nodes(idx)
      typ = capnp_getp(node, 3, err)
      call capnp_set_u16(typ, 0_int64, TYPE_STRUCT, err)
      call capnp_set_i64(typ, 8_int64, hue_id, err)
      call expect_arg(error, schema, 'annotation-type')
      call capnp_dyn_free(schema)

      call load_fix(schema, 'test/fixtures/box.cgr.bin', err)
      idx = capnp_dyn_find(schema, 'Box', err)
      call check_(error, idx > 0 .and. node_is_generic(schema%nodes(idx)), 'kinds: Box generic')
      if (allocated(error)) return
      call capnp_set_bool(schema%nodes(idx), 288_int64, .false., err)
      call expect_arg(error, schema, 'generic-flag')
      call capnp_dyn_free(schema)

      call load_fix(schema, 'test/fixtures/box.cgr.bin', err)
      idx = capnp_dyn_find(schema, 'BoxUse', err)
      node = schema%nodes(idx)
      call field_by_name(node, 'textBox', field, found, err)
      call check_(error, found, 'kinds: textBox')
      if (allocated(error)) return
      typ = field_slot_type(field, err)
      brand = type_brand(typ, err)
      scopes = brand_scopes(brand, err)
      n = int(capnp_list_len(scopes))
      call check_(error, n > 0, 'kinds: brand scopes')
      if (allocated(error)) return
      scope = capnp_list_get_struct(scopes, 0, err)
      binds = scope_bindings(scope, err)
      binding = capnp_list_get_struct(binds, 0, err)
      btype = binding_type(binding, err)
      call capnp_set_u16(btype, 0_int64, TYPE_INT32, err)
      call expect_arg(error, schema, 'brand-pointer')
      call capnp_dyn_free(schema)

      call load_fix(schema, 'test/fixtures/dynamic.cgr.bin', err)
      idx = capnp_dyn_find(schema, 'Shape.circle', err)
      call check_(error, idx > 0 .and. node_struct_is_group(schema%nodes(idx)), &
                  'kinds: circle group')
      if (allocated(error)) return
      call capnp_set_i64(schema%nodes(idx), 16_int64, 0_int64, err)
      call expect_arg(error, schema, 'group-scope')
      call capnp_dyn_free(schema)

      call load_fix(schema, 'test/fixtures/dynamic.cgr.bin', err)
      hue = capnp_dyn_find(schema, 'Hue', err)
      hue_id = node_id(schema%nodes(hue))
      idx = capnp_dyn_find(schema, 'Shape', err)
      node = schema%nodes(idx)
      call field_by_name(node, 'circle', field, found, err)
      call check_(error, found .and. field_which(field) == FIELD_GROUP, 'kinds: circle field')
      if (allocated(error)) return
      call capnp_set_i64(field, 16_int64, hue_id, err)
      call expect_arg(error, schema, 'group-kind')
      call capnp_dyn_free(schema)

      call load_fix(schema, 'test/fixtures/dynamic.cgr.bin', err)
      hue = capnp_dyn_find(schema, 'Hue', err)
      hue_id = node_id(schema%nodes(hue))
      idx = capnp_dyn_find(schema, 'AnnotatedService', err)
      items = node_interface_methods(schema%nodes(idx), err)
      item = capnp_list_get_struct(items, 0, err)
      call capnp_set_i64(item, 8_int64, hue_id, err)
      call expect_arg(error, schema, 'method-param')
      call capnp_dyn_free(schema)

      call load_fix(schema, 'test/fixtures/dynamic.cgr.bin', err)
      idx = capnp_dyn_find(schema, 'Shape.circle', err)
      call capnp_set_u16(schema%nodes(idx), 14_int64, 40_int32, err)
      call expect_ok(error, schema, 'group-larger')
      call capnp_dyn_free(schema)

      call load_fix(schema, 'test/fixtures/dynamic.cgr.bin', err)
      idx = capnp_dyn_find(schema, 'Scalars', err)
      node = schema%nodes(idx)
      call field_by_name(node, 'note', field, found, err)
      typ = field_slot_type(field, err)
      val = field_slot_default(field, err)
      call capnp_set_u16(typ, 0_int64, TYPE_STRUCT, err)
      call capnp_set_i64(typ, 8_int64, 42_int64, err)
      call capnp_set_u16(val, 0_int64, TYPE_STRUCT, err)
      call expect_ok(error, schema, 'missing-type')
      call capnp_dyn_free(schema)

      call load_two_kinds(error, schema)
   end subroutine run_kinds

   subroutine load_two_kinds(error, schema)
      type(error_type), allocatable, intent(inout) :: error
      type(capnp_dyn_schema_t), intent(inout) :: schema
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root, nodes, node
      integer(int8), allocatable :: bytes(:)
      integer :: err
      if (allocated(error)) return
      call capnp_message_init_builder(msg, err)
      if (err == CAPNP_OK) root = capnp_new_struct(msg, 0, 4, err)
      if (err == CAPNP_OK) call capnp_set_root(msg, root, err)
      if (err == CAPNP_OK) nodes = capnp_new_composite_list(msg, 2_int64, 6, 6, err)
      if (err == CAPNP_OK) call capnp_setp(root, 0, nodes, err)
      if (err == CAPNP_OK) node = capnp_list_get_struct(nodes, 0, err)
      if (err == CAPNP_OK) call capnp_set_i64(node, 0_int64, 11_int64, err)
      if (err == CAPNP_OK) call capnp_set_u16(node, 12_int64, int(NODE_STRUCT, int32), err)
      if (err == CAPNP_OK) node = capnp_list_get_struct(nodes, 1, err)
      if (err == CAPNP_OK) call capnp_set_i64(node, 0_int64, 11_int64, err)
      if (err == CAPNP_OK) call capnp_set_u16(node, 12_int64, int(NODE_ENUM, int32), err)
      if (err == CAPNP_OK) call capnp_serialize_bytes(msg, bytes, err)
      call capnp_message_free(msg)
      call check_(error, err == CAPNP_OK, 'two-kinds: build')
      if (allocated(error)) return
      call capnp_dyn_free(schema)
      call capnp_dyn_load(schema, bytes, err)
      call check_(error, err == CAPNP_ERR_ARG .and. .not. schema%loaded, 'two-kinds')
      call capnp_dyn_free(schema)
   end subroutine load_two_kinds

   subroutine run_fixtures(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_dyn_schema_t) :: schema
      character(len=48) :: files(13)
      integer :: i, err
      files(1) = 'test/fixtures/adder.cgr.bin'
      files(2) = 'test/fixtures/addressbook.cgr.bin'
      files(3) = 'test/fixtures/box.cgr.bin'
      files(4) = 'test/fixtures/dual.cgr.bin'
      files(5) = 'test/fixtures/dynamic.cgr.bin'
      files(6) = 'test/fixtures/holder.cgr.bin'
      files(7) = 'test/fixtures/kitchen.cgr.bin'
      files(8) = 'test/fixtures/null_structlist.cgr.bin'
      files(9) = 'test/fixtures/rpc.cgr.bin'
      files(10) = 'test/fixtures/rpc-threeparty.cgr.bin'
      files(11) = 'test/fixtures/schema.cgr.bin'
      files(12) = 'test/fixtures/stream.cgr.bin'
      files(13) = 'test/fixtures/streamer.cgr.bin'
      do i = 1, size(files)
         call load_fix(schema, trim(files(i)), err)
         call check_(error, err == CAPNP_OK .and. schema%loaded, 'fixture '//trim(files(i)))
         call capnp_dyn_free(schema)
         if (allocated(error)) return
      end do
   end subroutine run_fixtures

end module test_schema_validate
