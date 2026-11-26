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
      allocate (testsuite(2))
      testsuite(1)%name = "offsets"
      testsuite(1)%test => run_offsets
      testsuite(2)%name = "members"
      testsuite(2)%test => run_members
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

   subroutine reload(schema, label, err)
      type(capnp_dyn_schema_t), intent(inout) :: schema
      character(len=*), intent(in) :: label
      integer, intent(out) :: err
      integer(int8), allocatable :: bytes(:)
      character(len=1024) :: dump
      integer :: n, u, ios, st
      call capnp_serialize_bytes(schema%cgr, bytes, err)
      if (err /= CAPNP_OK) return
      call get_environment_variable('CAPNP_VALIDATE_DUMP', dump, length=n, status=st)
      if (st == 0 .and. n > 0) then
         open (newunit=u, file=trim(dump)//'/'//trim(label)//'.bin', &
               access='stream', form='unformatted', status='replace', iostat=ios)
         if (ios == 0) then
            write (u) bytes
            close (u)
         end if
      end if
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
         if (got == name) then
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
      call reload(schema, label, err)
      call check_(error, err == CAPNP_ERR_ARG .and. .not. schema%loaded, label)
   end subroutine expect_arg

   subroutine expect_ok(error, schema, label)
      type(error_type), allocatable, intent(inout) :: error
      type(capnp_dyn_schema_t), intent(inout) :: schema
      character(len=*), intent(in) :: label
      integer :: err
      if (allocated(error)) return
      call reload(schema, label, err)
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
   end subroutine run_members

end module test_schema_validate
