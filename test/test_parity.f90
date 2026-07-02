!> Parity semantics: deep copy across messages, cross-message setp, list
!> upgrade/downgrade views, u64 surface, List(Text).
program test_parity
   use capnp
   implicit none

   integer :: nfail = 0

   call t_deep_copy()
   call t_cross_message_setp()
   call t_list_upgrade_views()
   call t_list_downgrade_views()
   call t_u64()
   call t_list_of_text()

   if (nfail > 0) then
      print '(a,i0,a)', 'FAILED: ', nfail, ' assertion(s)'
      error stop 1
   end if
   print '(a)', 'All parity tests passed.'

contains

   subroutine check_(cond, name)
      logical, intent(in) :: cond
      character(len=*), intent(in) :: name
      if (.not. cond) then
         nfail = nfail + 1
         print '(a,a)', 'FAIL: ', name
      end if
   end subroutine check_

   !> Build a small tree in one message, deep-copy into another, serialize
   !> both, verify the copy field-by-field after a round trip.
   subroutine t_deep_copy()
      type(capnp_message_t), target :: a, b, r
      type(capnp_ptr_t) :: root, kid, lst, croot, q
      integer(int8), allocatable :: bytes(:)
      character(len=:), allocatable :: s
      integer :: err, i

      call capnp_message_init_builder(a, err)
      root = capnp_new_struct(a, 1, 3, err)
      call capnp_set_i64(root, 0_int64, 41_int64, err)
      call capnp_set_text(root, 0, 'copy me', err)
      kid = capnp_new_struct(a, 1, 0, err)
      call capnp_set_i32(kid, 0_int64, 7_int32, err)
      call capnp_setp(root, 1, kid, err)
      lst = capnp_new_list(a, CAPNP_SZ_FOUR, 3_int64, err)
      do i = 0, 2
         call capnp_list_set_i32(lst, int(i, int64), int(i*11, int32), err)
      end do
      call capnp_setp(root, 2, lst, err)
      call capnp_set_root(a, root, err)
      call check_(err == CAPNP_OK, 'copy: source built')

      call capnp_message_init_builder(b, err)
      croot = capnp_copy(b, root, err)
      call check_(err == CAPNP_OK, 'copy: deep copy ok')
      call capnp_set_root(b, croot, err)

      call capnp_serialize_bytes(b, bytes, err)
      call capnp_deserialize_bytes(bytes, r, err)
      q = capnp_root(r, err)
      call check_(capnp_get_i64(q, 0_int64) == 41_int64, 'copy: data word')
      call capnp_get_text(q, 0, s, err)
      call check_(s == 'copy me', 'copy: text')
      q = capnp_getp(q, 1, err)
      call check_(capnp_get_i32(q, 0_int64) == 7_int32, 'copy: nested struct')
      q = capnp_root(r, err)
      q = capnp_getp(q, 2, err)
      call check_(capnp_list_len(q) == 3_int64, 'copy: list len')
      call check_(capnp_list_get_i32(q, 2_int64, err) == 22_int32, 'copy: list elem')
      call capnp_message_free(a)
      call capnp_message_free(b)
      call capnp_message_free(r)
   end subroutine t_deep_copy

   !> setp with an object from another message must clone, not error.
   subroutine t_cross_message_setp()
      type(capnp_message_t), target :: a, b, r
      type(capnp_ptr_t) :: aroot, broot, q
      integer(int8), allocatable :: bytes(:)
      character(len=:), allocatable :: s
      integer :: err

      call capnp_message_init_builder(a, err)
      aroot = capnp_new_struct(a, 0, 1, err)
      call capnp_set_text(aroot, 0, 'foreign', err)
      call capnp_set_root(a, aroot, err)

      call capnp_message_init_builder(b, err)
      broot = capnp_new_struct(b, 0, 1, err)
      call capnp_set_root(b, broot, err)
      call capnp_setp(broot, 0, aroot, err)
      call check_(err == CAPNP_OK, 'xmsg: setp clones')

      call capnp_serialize_bytes(b, bytes, err)
      call capnp_deserialize_bytes(bytes, r, err)
      q = capnp_root(r, err)
      q = capnp_getp(q, 0, err)
      call capnp_get_text(q, 0, s, err)
      call check_(s == 'foreign', 'xmsg: cloned text intact')
      call capnp_message_free(a)
      call capnp_message_free(b)
      call capnp_message_free(r)
   end subroutine t_cross_message_setp

   !> Old writer encoded List(UInt32); new reader wants List(Struct) where
   !> the value is field @0. Also List(Text) elements as one-pointer structs.
   subroutine t_list_upgrade_views()
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: lst, el
      character(len=:), allocatable :: s
      integer :: err

      call capnp_message_init_builder(msg, err)
      lst = capnp_new_list(msg, CAPNP_SZ_FOUR, 2_int64, err)
      call capnp_list_set_i32(lst, 0_int64, 100_int32, err)
      call capnp_list_set_i32(lst, 1_int64, 200_int32, err)

      el = capnp_list_get_struct(lst, 1, err)
      call check_(err == CAPNP_OK .and. el%kind == CAPNP_PK_STRUCT, 'upgrade: view exists')
      call check_(capnp_get_i32(el, 0_int64) == 200_int32, 'upgrade: field @0 reads value')
      ! Beyond the element's bits reads default, never a neighbour.
      call check_(capnp_get_i64(el, 0_int64, default=5_int64) == 5_int64, &
                  'upgrade: oversize read yields default')

      lst = capnp_new_list(msg, CAPNP_SZ_PTR, 1_int64, err)
      call capnp_list_set_text(lst, 0, 'elem', err)
      el = capnp_list_get_struct(lst, 0, err)
      call check_(err == CAPNP_OK .and. el%pwords == 1, 'upgrade: ptr-list view')
      call capnp_get_text(el, 0, s, err)
      call check_(s == 'elem', 'upgrade: ptr view field @0')
      call capnp_message_free(msg)
   end subroutine t_list_upgrade_views

   !> Old writer encoded List(Struct); new reader wants primitives (field
   !> @0) or the first pointer.
   subroutine t_list_downgrade_views()
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: lst, el, q
      character(len=:), allocatable :: s
      integer :: err, i

      call capnp_message_init_builder(msg, err)
      lst = capnp_new_composite_list(msg, 2_int64, 1, 1, err)
      do i = 0, 1
         el = capnp_list_get_struct(lst, i, err)
         call capnp_set_i32(el, 0_int64, int(1000 + i, int32), err)
         call capnp_set_text(el, 0, 'hello', err)
      end do

      call check_(capnp_list_get_i32(lst, 1_int64, err) == 1001_int32, &
                  'downgrade: composite as List(u32)')
      q = capnp_getp(lst, 0, err)
      call check_(err == CAPNP_OK .and. q%kind == CAPNP_PK_LIST, 'downgrade: first pointer')
      call capnp_get_text(lst, 0, s, err)
      call check_(s == 'hello', 'downgrade: composite as List(Text)')
      call capnp_message_free(msg)
   end subroutine t_list_downgrade_views

   subroutine t_u64()
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root
      integer :: err
      call capnp_message_init_builder(msg, err)
      root = capnp_new_struct(msg, 1, 0, err)
      ! 0xDEADBEEFCAFEBABE as a two's-complement int64 bit pattern.
      call capnp_set_u64(root, 0_int64, -2401053089206453570_int64, err)
      call check_(err == CAPNP_OK, 'u64: set')
      call check_(capnp_get_u64(root, 0_int64) == -2401053089206453570_int64, 'u64: get')
      call capnp_message_free(msg)
   end subroutine t_u64

   subroutine t_list_of_text()
      type(capnp_message_t), target :: msg, r
      type(capnp_ptr_t) :: root, lst
      integer(int8), allocatable :: bytes(:)
      character(len=:), allocatable :: s
      integer :: err

      call capnp_message_init_builder(msg, err)
      root = capnp_new_struct(msg, 0, 1, err)
      lst = capnp_new_list(msg, CAPNP_SZ_PTR, 3_int64, err)
      call capnp_list_set_text(lst, 0, 'alpha', err)
      call capnp_list_set_text(lst, 1, 'beta', err)
      call capnp_list_set_text(lst, 2, 'gamma', err)
      call capnp_setp(root, 0, lst, err)
      call capnp_set_root(msg, root, err)
      call capnp_serialize_bytes(msg, bytes, err)
      call capnp_deserialize_bytes(bytes, r, err)
      root = capnp_root(r, err)
      lst = capnp_getp(root, 0, err)
      call check_(capnp_list_len(lst) == 3_int64, 'ltext: len')
      call capnp_list_get_text(lst, 1, s, err)
      call check_(s == 'beta', 'ltext: element')
      call capnp_list_get_text(lst, 2, s, err)
      call check_(s == 'gamma', 'ltext: last element')
      call capnp_message_free(msg)
      call capnp_message_free(r)
   end subroutine t_list_of_text

end program test_parity
