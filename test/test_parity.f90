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
   call t_bulk_accessors()
   call t_incremental_unpack()
   call t_zero_copy_view()
   call t_disown_adopt()
   call t_incremental_pack()
   call t_views_and_lengths()
   call t_stream_unit()
   call t_packed_stream_unit()
   call t_total_size()

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

   subroutine t_bulk_accessors()
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: lst
      integer(int32), allocatable :: got(:)
      integer(int32) :: put(0:3)
      integer :: err
      call capnp_message_init_builder(msg, err)
      lst = capnp_new_list(msg, CAPNP_SZ_FOUR, 4_int64, err)
      put = [11_int32, 22_int32, 33_int32, 44_int32]
      call capnp_list_set_all_i32(lst, put, err)
      call check_(err == CAPNP_OK, 'bulk: set_all')
      call capnp_list_get_all_i32(lst, got, err)
      call check_(err == CAPNP_OK .and. size(got) == 4, 'bulk: get_all size')
      call check_(all(got == put), 'bulk: values round trip')
      ! Size mismatch is an argument error.
      call capnp_list_set_all_i32(lst, put(0:2), err)
      call check_(err == CAPNP_ERR_ARG, 'bulk: size mismatch rejected')
      call capnp_message_free(msg)
   end subroutine t_bulk_accessors

   !> Disown detaches a subtree (slot zeroed, storage intact); re-linking it
   !> elsewhere in the same message is adoption without copy.
   subroutine t_disown_adopt()
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root, kid, orphan, back
      integer :: err
      call capnp_message_init_builder(msg, err)
      root = capnp_new_struct(msg, 0, 2, err)
      kid = capnp_new_struct(msg, 1, 0, err)
      call capnp_set_i64(kid, 0_int64, 555_int64, err)
      call capnp_setp(root, 0, kid, err)
      call capnp_set_root(msg, root, err)

      orphan = capnp_disown(root, 0, err)
      call check_(err == CAPNP_OK .and. orphan%kind == CAPNP_PK_STRUCT, 'orphan: disowned')
      back = capnp_getp(root, 0, err)
      call check_(err == CAPNP_OK .and. back%kind == CAPNP_PK_NULL, 'orphan: slot cleared')
      call capnp_setp(root, 1, orphan, err)
      call check_(err == CAPNP_OK, 'orphan: adopted in new slot')
      back = capnp_getp(root, 1, err)
      call check_(capnp_get_i64(back, 0_int64) == 555_int64, 'orphan: contents intact')
      call capnp_message_free(msg)
   end subroutine t_disown_adopt

   !> A view reader aliases the caller's buffer: mutating the buffer after
   !> capnp_deserialize_view must show through the reader (proof of no copy).
   subroutine t_zero_copy_view()
      type(capnp_message_t), target :: msg, vr
      type(capnp_ptr_t) :: root, r
      integer(int8), allocatable, target :: bytes(:)
      integer :: err
      call capnp_message_init_builder(msg, err)
      root = capnp_new_struct(msg, 1, 0, err)
      call capnp_set_i32(root, 0_int64, 100_int32, err)
      call capnp_set_root(msg, root, err)
      call capnp_serialize_bytes(msg, bytes, err)
      call capnp_deserialize_view(bytes, vr, err)
      call check_(err == CAPNP_OK, 'view: deserializes')
      r = capnp_root(vr, err)
      call check_(capnp_get_i32(r, 0_int64) == 100_int32, 'view: reads value')
      ! Data word starts at byte 16 (8 header + 8 root pointer).
      bytes(16) = 101_int8
      call check_(capnp_get_i32(r, 0_int64) == 101_int32, 'view: aliases caller buffer')
      call capnp_message_free(vr) ! must not free the caller's buffer
      call check_(allocated(bytes) .and. bytes(16) == 101_int8, 'view: free leaves buffer')
      call capnp_message_free(msg)
   end subroutine t_zero_copy_view

   !> The incremental unpacker must reproduce whole-buffer capnp_unpack for
   !> every chunk split position of the spec vector plus escape runs.
   subroutine t_incremental_unpack()
      type(capnp_unpacker_t) :: u
      integer(int8), allocatable :: whole(:), inc(:), packed(:)
      integer(int8) :: src(0:31)
      integer(int64) :: outn, cut
      integer :: err, i
      ! Mixed content: spec-vector words, a zero run, a raw run.
      src = 0_int8
      src(0) = 8_int8
      src(4) = 3_int8
      src(6) = 2_int8
      do i = 16, 31
         src(i) = int(mod(i*7, 127) + 1, int8) ! fully nonzero -> raw run
      end do
      call capnp_pack(src, packed, err)
      call check_(err == CAPNP_OK, 'inc: packed source')
      call capnp_unpack(packed, whole, err)
      call check_(err == CAPNP_OK .and. size(whole) == 32 .and. &
                  all(whole == src), 'inc: whole-buffer reference')
      do cut = 1_int64, size(packed, kind=int64) - 1_int64
         u = capnp_unpacker_t()
         if (allocated(inc)) deallocate (inc)
         outn = 0_int64
         call capnp_unpack_push(u, packed(0:cut - 1), inc, outn, err)
         call check_(err == CAPNP_OK, 'inc: first chunk ok')
         call capnp_unpack_push(u, packed(cut:), inc, outn, err)
         call check_(err == CAPNP_OK, 'inc: second chunk ok')
         if (outn /= 32_int64 .or. .not. all(inc(0:outn - 1) == src)) then
            call check_(.false., 'inc: split reproduces source')
            print '(a,i0)', '  failing split at byte ', cut
            return
         end if
      end do
      call check_(.true., 'inc: all splits reproduce source')
   end subroutine t_incremental_unpack

   !> The incremental packer must equal whole-buffer capnp_pack for every
   !> split position over mixed content (zero runs, literal runs, sparse).
   subroutine t_incremental_pack()
      type(capnp_packer_t) :: pk
      integer(int8) :: src(0:47)
      integer(int8), allocatable :: whole(:), inc(:)
      integer(int64) :: outn, cut
      integer :: err, i
      src = 0_int8
      src(0) = 8_int8
      src(4) = 3_int8 ! sparse word
      do i = 16, 31
         src(i) = int(mod(i*5, 126) + 1, int8) ! two fully nonzero words
      end do
      ! words 4-5 stay zero (zero run at the tail)
      call capnp_pack(src, whole, err)
      call check_(err == CAPNP_OK, 'ipack: whole reference')
      do cut = 1_int64, 47_int64
         pk = capnp_packer_t()
         if (allocated(inc)) deallocate (inc)
         outn = 0_int64
         call capnp_pack_push(pk, src(0:cut - 1), inc, outn, err)
         call capnp_pack_push(pk, src(cut:), inc, outn, err)
         call capnp_pack_finish(pk, inc, outn, err)
         if (err /= CAPNP_OK .or. outn /= size(whole, kind=int64)) then
            call check_(.false., 'ipack: split size equals whole')
            print '(a,i0)', '  failing split at ', cut
            return
         end if
         if (.not. all(inc(0:outn - 1) == whole)) then
            call check_(.false., 'ipack: split bytes equal whole')
            print '(a,i0)', '  failing split at ', cut
            return
         end if
      end do
      call check_(.true., 'ipack: all splits byte-identical')
   end subroutine t_incremental_pack

   subroutine t_views_and_lengths()
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root
      integer(int8), pointer :: view(:)
      integer(int8) :: blob(0:3)
      integer :: err
      call capnp_message_init_builder(msg, err)
      root = capnp_new_struct(msg, 0, 2, err)
      call capnp_set_text(root, 0, 'hello', err)
      blob = [1_int8, 2_int8, 3_int8, 4_int8]
      call capnp_set_data(root, 1, blob, err)
      call check_(capnp_text_len(root, 0, err) == 5_int64, 'view: text_len')
      call capnp_get_data_view(root, 1, view, err)
      call check_(err == CAPNP_OK .and. associated(view), 'view: data view exists')
      call check_(size(view) == 4 .and. all(view == blob), 'view: data view bytes')
      call capnp_message_free(msg)
   end subroutine t_views_and_lengths

   !> Two messages written back-to-back on one stream unit read in sequence
   !> without over-consuming.
   subroutine t_stream_unit()
      type(capnp_message_t), target :: a, b, r1, r2
      type(capnp_ptr_t) :: root, q
      integer(int8), allocatable :: ba(:), bb(:)
      integer :: err, unit, ios
      character(len=*), parameter :: path = 'build/two_messages.bin'
      call capnp_message_init_builder(a, err)
      root = capnp_new_struct(a, 1, 0, err)
      call capnp_set_i64(root, 0_int64, 111_int64, err)
      call capnp_set_root(a, root, err)
      call capnp_serialize_bytes(a, ba, err)
      call capnp_message_init_builder(b, err)
      root = capnp_new_struct(b, 1, 0, err)
      call capnp_set_i64(root, 0_int64, 222_int64, err)
      call capnp_set_root(b, root, err)
      call capnp_serialize_bytes(b, bb, err)
      open (newunit=unit, file=path, access='stream', form='unformatted', &
            status='replace', action='readwrite', iostat=ios)
      call check_(ios == 0, 'stream: scratch file opens')
      write (unit) ba, bb
      rewind (unit)
      call capnp_read_message_unit(unit, r1, err)
      call check_(err == CAPNP_OK, 'stream: first message reads')
      q = capnp_root(r1, err)
      call check_(capnp_get_i64(q, 0_int64) == 111_int64, 'stream: first value')
      call capnp_read_message_unit(unit, r2, err)
      call check_(err == CAPNP_OK, 'stream: second message reads')
      q = capnp_root(r2, err)
      call check_(capnp_get_i64(q, 0_int64) == 222_int64, 'stream: second value')
      close (unit, status='delete')
      call capnp_message_free(a)
      call capnp_message_free(b)
      call capnp_message_free(r1)
      call capnp_message_free(r2)
   end subroutine t_stream_unit

   !> Two packed messages back-to-back on one stream unit; the packed
   !> reader must stop at each message's exact last byte. The second
   !> message carries text so the packed stream mixes zero runs, partial
   !> tags, and literal runs across the boundary.
   subroutine t_packed_stream_unit()
      type(capnp_message_t), target :: a, b, r1, r2
      type(capnp_ptr_t) :: root, q
      integer(int8), allocatable :: pa(:), pb(:)
      character(len=:), allocatable :: s
      integer :: err, unit, ios
      character(len=*), parameter :: path = 'build/two_messages.packed.bin'
      call capnp_message_init_builder(a, err)
      root = capnp_new_struct(a, 1, 0, err)
      call capnp_set_i64(root, 0_int64, 333_int64, err)
      call capnp_set_root(a, root, err)
      call capnp_serialize_packed_bytes(a, pa, err)
      call capnp_message_init_builder(b, err)
      root = capnp_new_struct(b, 1, 1, err)
      call capnp_set_i64(root, 0_int64, 444_int64, err)
      call capnp_set_text(root, 0, 'packed across the stream', err)
      call capnp_set_root(b, root, err)
      call capnp_serialize_packed_bytes(b, pb, err)
      call check_(err == CAPNP_OK, 'packed stream: built')
      open (newunit=unit, file=path, access='stream', form='unformatted', &
            status='replace', action='readwrite', iostat=ios)
      call check_(ios == 0, 'packed stream: scratch file opens')
      write (unit) pa, pb
      rewind (unit)
      call capnp_read_message_packed_unit(unit, r1, err)
      call check_(err == CAPNP_OK, 'packed stream: first message reads')
      q = capnp_root(r1, err)
      call check_(capnp_get_i64(q, 0_int64) == 333_int64, 'packed stream: first value')
      call capnp_read_message_packed_unit(unit, r2, err)
      call check_(err == CAPNP_OK, 'packed stream: second message reads')
      q = capnp_root(r2, err)
      call check_(capnp_get_i64(q, 0_int64) == 444_int64, 'packed stream: second value')
      call capnp_get_text(q, 0, s, err)
      call check_(err == CAPNP_OK .and. s == 'packed across the stream', &
                  'packed stream: second text')
      close (unit, status='delete')
      call capnp_message_free(a)
      call capnp_message_free(b)
      call capnp_message_free(r1)
      call capnp_message_free(r2)
   end subroutine t_packed_stream_unit

   !> totalSize accounting: root (1+3) + 'copy me' text (1 word incl NUL)
   !> + kid struct (1+0) + List(Int32) x3 (2 words) = 8 words.
   subroutine t_total_size()
      type(capnp_message_t), target :: a
      type(capnp_ptr_t) :: root, kid, lst
      integer(int64) :: words
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
      call check_(err == CAPNP_OK, 'total size: built')

      words = capnp_total_size(root, err)
      call check_(err == CAPNP_OK .and. words == 8_int64, 'total size: 8 words')
      words = capnp_total_size(kid, err)
      call check_(words == 1_int64, 'total size: leaf struct')
      words = capnp_total_size(lst, err)
      call check_(words == 2_int64, 'total size: primitive list')
      call capnp_message_free(a)
   end subroutine t_total_size

end program test_parity
