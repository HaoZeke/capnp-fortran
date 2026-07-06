!> Unit tests: endian codecs, pointer words, builder/reader round trips,
!> golden wire bytes, far pointers, packed codec vectors from the spec.
program check
   use capnp
   implicit none

   integer :: nfail = 0

   call t_endian()
   call t_pointer_words()
   call t_golden_single_struct()
   call t_struct_fields_roundtrip()
   call t_text_data()
   call t_lists()
   call t_composite_list()
   call t_nested_struct()
   call t_far_pointer()
   call t_packed_spec_vectors()
   call t_packed_roundtrip()
   call t_framing_errors()
   call t_traversal_limit()
   call t_depth_limit()

   if (nfail > 0) then
      print '(a,i0,a)', 'FAILED: ', nfail, ' assertion(s)'
      error stop 1
   end if
   print '(a)', 'All tests passed.'

contains

   subroutine check_(cond, name)
      logical, intent(in) :: cond
      character(len=*), intent(in) :: name
      if (.not. cond) then
         nfail = nfail + 1
         print '(a,a)', 'FAIL: ', name
      end if
   end subroutine check_

   subroutine t_endian()
      integer(int8) :: buf(0:15)
      buf = 0_int8
      call cp_put_i32(buf, 0_int64, int(z'12345678', int32))
      call check_(buf(0) == int(z'78', int8) .and. buf(1) == int(z'56', int8) .and. &
                  buf(2) == int(z'34', int8) .and. buf(3) == int(z'12', int8), 'endian: i32 LE bytes')
      call check_(cp_get_i32(buf, 0_int64) == int(z'12345678', int32), 'endian: i32 round trip')
      call cp_put_i64(buf, 8_int64, -1_int64)
      call check_(cp_get_i64(buf, 8_int64) == -1_int64, 'endian: i64 all-ones')
      call cp_put_i16(buf, 4_int64, -2_int16)
      call check_(cp_get_i16(buf, 4_int64) == -2_int16, 'endian: i16 negative')
      call cp_put_f64(buf, 8_int64, 1.5_real64)
      call check_(cp_get_f64(buf, 8_int64) == 1.5_real64, 'endian: f64 round trip')
   end subroutine t_endian

   subroutine t_pointer_words()
      integer(int64) :: w
      w = wp_make_struct(0_int32, 1_int32, 0_int32)
      call check_(w == int(z'0000000100000000', int64), 'ptr: struct C=1 D=0 golden')
      call check_(wp_kind(w) == CAPNP_WK_STRUCT .and. wp_offset(w) == 0 .and. &
                  wp_struct_dwords(w) == 1 .and. wp_struct_pwords(w) == 0, 'ptr: struct decode')
      w = wp_make_struct(-2_int32, 3_int32, 4_int32)
      call check_(wp_offset(w) == -2 .and. wp_struct_dwords(w) == 3 .and. &
                  wp_struct_pwords(w) == 4, 'ptr: negative offset')
      w = wp_make_list(5_int32, CAPNP_SZ_FOUR, 7_int64)
      call check_(wp_kind(w) == CAPNP_WK_LIST .and. wp_offset(w) == 5 .and. &
                  wp_list_esize(w) == CAPNP_SZ_FOUR .and. wp_list_count(w) == 7_int64, &
                  'ptr: list decode')
      w = wp_make_far(.false., 3_int64, 1_int64)
      call check_(wp_kind(w) == CAPNP_WK_FAR .and. .not. wp_far_two(w) .and. &
                  wp_far_off(w) == 3_int64 .and. wp_far_seg(w) == 1_int64, 'ptr: far decode')
      w = wp_make_far(.true., 0_int64, 2_int64)
      call check_(wp_far_two(w), 'ptr: double-far flag')
      w = wp_make_cap(9_int64)
      call check_(wp_kind(w) == CAPNP_WK_CAP .and. wp_cap_index(w) == 9_int64, 'ptr: cap decode')
   end subroutine t_pointer_words

   !> Golden bytes: root struct with one data word holding u32 0x12345678.
   subroutine t_golden_single_struct()
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root
      integer(int8), allocatable :: bytes(:)
      integer(int8) :: expect(0:23)
      integer :: err, i
      call capnp_message_init_builder(msg, err)
      call check_(err == CAPNP_OK, 'golden: init')
      root = capnp_new_struct(msg, 1, 0, err)
      call capnp_set_u32(root, 0_int64, int(z'12345678', int64), err)
      call check_(err == CAPNP_OK, 'golden: set field')
      call capnp_set_root(msg, root, err)
      call capnp_serialize_bytes(msg, bytes, err)
      call check_(err == CAPNP_OK .and. size(bytes) == 24, 'golden: 24 bytes total')
      expect = 0_int8
      expect(4) = 2_int8                 ! segment size: 2 words
      expect(12) = 1_int8                ! root ptr: dwords=1 at byte 12
      expect(16) = int(z'78', int8)
      expect(17) = int(z'56', int8)
      expect(18) = int(z'34', int8)
      expect(19) = int(z'12', int8)
      do i = 0, 23
         if (bytes(i) /= expect(i)) then
            call check_(.false., 'golden: byte mismatch')
            print '(a,i0,a,i0,a,i0)', '  byte ', i, ': got ', bytes(i), ' want ', expect(i)
            exit
         end if
      end do
      call capnp_message_free(msg)
   end subroutine t_golden_single_struct

   subroutine t_struct_fields_roundtrip()
      type(capnp_message_t), target :: msg, rmsg
      type(capnp_ptr_t) :: root, r
      integer(int8), allocatable :: bytes(:)
      integer :: err
      call capnp_message_init_builder(msg, err)
      root = capnp_new_struct(msg, 4, 0, err)
      call capnp_set_i8(root, 0_int64, -5_int8, err)
      call capnp_set_i16(root, 2_int64, -300_int16, err)
      call capnp_set_i32(root, 4_int64, 123456_int32, err, default=7_int32)
      call capnp_set_i64(root, 8_int64, -9876543210_int64, err)
      call capnp_set_u8(root, 16_int64, 250_int16, err)
      call capnp_set_u16(root, 18_int64, 65500_int32, err)
      call capnp_set_u32(root, 20_int64, 4000000000_int64, err)
      call capnp_set_f32(root, 24_int64, 3.25_real32, err)
      call capnp_set_bool(root, 224_int64, .true., err)
      call capnp_set_bool(root, 225_int64, .true., err, default=.true.)
      call capnp_set_root(msg, root, err)
      call capnp_serialize_bytes(msg, bytes, err)
      call capnp_deserialize_bytes(bytes, rmsg, err)
      r = capnp_root(rmsg, err)
      call check_(err == CAPNP_OK .and. r%kind == CAPNP_PK_STRUCT, 'fields: root resolves')
      call check_(capnp_get_i8(r, 0_int64) == -5_int8, 'fields: i8')
      call check_(capnp_get_i16(r, 2_int64) == -300_int16, 'fields: i16')
      call check_(capnp_get_i32(r, 4_int64, default=7_int32) == 123456_int32, 'fields: i32+default')
      call check_(capnp_get_i64(r, 8_int64) == -9876543210_int64, 'fields: i64')
      call check_(capnp_get_u8(r, 16_int64) == 250_int16, 'fields: u8')
      call check_(capnp_get_u16(r, 18_int64) == 65500_int32, 'fields: u16')
      call check_(capnp_get_u32(r, 20_int64) == 4000000000_int64, 'fields: u32')
      call check_(capnp_get_f32(r, 24_int64) == 3.25_real32, 'fields: f32')
      call check_(capnp_get_bool(r, 224_int64), 'fields: bool set')
      call check_(capnp_get_bool(r, 225_int64, default=.true.), 'fields: bool default XOR')
      ! Reads past the data section return defaults (older-schema semantics).
      call check_(capnp_get_i32(r, 4096_int64, default=42_int32) == 42_int32, 'fields: OOB read -> default')
      call capnp_message_free(msg)
      call capnp_message_free(rmsg)
   end subroutine t_struct_fields_roundtrip

   subroutine t_text_data()
      type(capnp_message_t), target :: msg, rmsg
      type(capnp_ptr_t) :: root, r, tl
      integer(int8), allocatable :: bytes(:), blob(:), rblob(:)
      character(len=:), allocatable :: s
      integer :: err
      call capnp_message_init_builder(msg, err)
      root = capnp_new_struct(msg, 0, 2, err)
      call capnp_set_text(root, 0, 'Hello, Cap''n Proto!', err)
      call check_(err == CAPNP_OK, 'text: set')
      allocate (blob(0:4))
      blob = [0_int8, 1_int8, 2_int8, 3_int8, 4_int8]
      blob(0) = -34_int8 ! 0xde
      call capnp_set_data(root, 1, blob, err)
      call capnp_set_root(msg, root, err)
      call capnp_serialize_bytes(msg, bytes, err)
      call capnp_deserialize_bytes(bytes, rmsg, err)
      r = capnp_root(rmsg, err)
      call capnp_get_text(r, 0, s, err)
      call check_(err == CAPNP_OK .and. s == 'Hello, Cap''n Proto!', 'text: round trip')
      ! Text wire shape: NUL included in element count.
      tl = capnp_getp(r, 0, err)
      call check_(tl%kind == CAPNP_PK_LIST .and. tl%esize == CAPNP_SZ_BYTE .and. &
                  tl%nelem == 20_int64, 'text: count includes NUL')
      call capnp_get_data(r, 1, rblob, err)
      call check_(err == CAPNP_OK .and. size(rblob) == 5 .and. all(rblob == blob), 'data: round trip')
      ! Absent pointer field reads as empty text, no error.
      call capnp_get_text(r, 5, s, err)
      call check_(err == CAPNP_OK .and. len(s) == 0, 'text: absent -> empty')
      call capnp_message_free(msg)
      call capnp_message_free(rmsg)
   end subroutine t_text_data

   subroutine t_lists()
      type(capnp_message_t), target :: msg, rmsg
      type(capnp_ptr_t) :: root, lst, r, rl
      integer(int8), allocatable :: bytes(:)
      integer :: err, i
      integer(int32) :: v
      call capnp_message_init_builder(msg, err)
      root = capnp_new_struct(msg, 0, 2, err)
      lst = capnp_new_list(msg, CAPNP_SZ_FOUR, 5_int64, err)
      do i = 0, 4
         call capnp_list_set_i32(lst, int(i, int64), int(10*i + 1, int32), err)
      end do
      call capnp_setp(root, 0, lst, err)
      lst = capnp_new_list(msg, CAPNP_SZ_BIT, 10_int64, err)
      call capnp_list_set_bool(lst, 3_int64, .true., err)
      call capnp_list_set_bool(lst, 9_int64, .true., err)
      call capnp_setp(root, 1, lst, err)
      call capnp_set_root(msg, root, err)
      call capnp_serialize_bytes(msg, bytes, err)
      call capnp_deserialize_bytes(bytes, rmsg, err)
      r = capnp_root(rmsg, err)
      rl = capnp_getp(r, 0, err)
      call check_(rl%kind == CAPNP_PK_LIST .and. capnp_list_len(rl) == 5_int64, 'list: i32 len')
      v = capnp_list_get_i32(rl, 3_int64, err)
      call check_(err == CAPNP_OK .and. v == 31_int32, 'list: i32 element')
      rl = capnp_getp(r, 1, err)
      call check_(capnp_list_len(rl) == 10_int64, 'list: bit len')
      call check_(capnp_list_get_bool(rl, 3_int64, err) .and. .not. capnp_list_get_bool(rl, 4_int64, err), &
                  'list: bit elements')
      call check_(capnp_list_get_bool(rl, 9_int64, err), 'list: last bit')
      call capnp_message_free(msg)
      call capnp_message_free(rmsg)
   end subroutine t_lists

   subroutine t_composite_list()
      type(capnp_message_t), target :: msg, rmsg
      type(capnp_ptr_t) :: root, lst, el, r, rl
      integer(int8), allocatable :: bytes(:)
      character(len=:), allocatable :: s
      integer :: err, i
      call capnp_message_init_builder(msg, err)
      root = capnp_new_struct(msg, 0, 1, err)
      lst = capnp_new_composite_list(msg, 3_int64, 1, 1, err)
      call check_(err == CAPNP_OK .and. lst%nelem == 3_int64, 'clist: new')
      do i = 0, 2
         el = capnp_list_get_struct(lst, i, err)
         call capnp_set_i32(el, 0_int64, int(100 + i, int32), err)
         call capnp_set_text(el, 0, 'element', err)
      end do
      call capnp_setp(root, 0, lst, err)
      call capnp_set_root(msg, root, err)
      call capnp_serialize_bytes(msg, bytes, err)
      call capnp_deserialize_bytes(bytes, rmsg, err)
      r = capnp_root(rmsg, err)
      rl = capnp_getp(r, 0, err)
      call check_(err == CAPNP_OK .and. rl%esize == CAPNP_SZ_COMPOSITE .and. &
                  capnp_list_len(rl) == 3_int64, 'clist: read len')
      el = capnp_list_get_struct(rl, 2, err)
      call check_(capnp_get_i32(el, 0_int64) == 102_int32, 'clist: element field')
      call capnp_get_text(el, 0, s, err)
      call check_(s == 'element', 'clist: element text')
      call capnp_message_free(msg)
      call capnp_message_free(rmsg)
   end subroutine t_composite_list

   subroutine t_nested_struct()
      type(capnp_message_t), target :: msg, rmsg
      type(capnp_ptr_t) :: root, kid, r, rk
      integer(int8), allocatable :: bytes(:)
      integer :: err
      call capnp_message_init_builder(msg, err)
      root = capnp_new_struct(msg, 1, 1, err)
      kid = capnp_new_struct(msg, 1, 0, err)
      call capnp_set_i64(kid, 0_int64, 777_int64, err)
      call capnp_setp(root, 0, kid, err)
      call capnp_set_i32(root, 0_int64, 1_int32, err)
      call capnp_set_root(msg, root, err)
      call capnp_serialize_bytes(msg, bytes, err)
      call capnp_deserialize_bytes(bytes, rmsg, err)
      r = capnp_root(rmsg, err)
      rk = capnp_getp(r, 0, err)
      call check_(err == CAPNP_OK .and. rk%kind == CAPNP_PK_STRUCT, 'nested: resolves')
      call check_(capnp_get_i64(rk, 0_int64) == 777_int64, 'nested: field')
      ! Out-of-range pointer index resolves null without error.
      rk = capnp_getp(r, 3, err)
      call check_(err == CAPNP_OK .and. rk%kind == CAPNP_PK_NULL, 'nested: OOB ptr -> null')
      call capnp_message_free(msg)
      call capnp_message_free(rmsg)
   end subroutine t_nested_struct

   !> A tiny first segment forces the second object into a new segment, so
   !> the root's pointer field must become a far pointer.
   subroutine t_far_pointer()
      type(capnp_message_t), target :: msg, rmsg
      type(capnp_ptr_t) :: root, kid, r, rk
      integer(int8), allocatable :: bytes(:)
      integer :: err
      call capnp_message_init_builder(msg, err, first_words=3_int64)
      root = capnp_new_struct(msg, 1, 1, err)   ! fills the first segment
      kid = capnp_new_struct(msg, 2, 0, err)    ! must spill to segment 2
      call check_(msg%nsegs >= 2, 'far: second segment created')
      call check_(root%seg /= kid%seg, 'far: objects in different segments')
      call capnp_set_i64(kid, 0_int64, 111_int64, err)
      call capnp_set_i64(kid, 8_int64, 222_int64, err)
      call capnp_setp(root, 0, kid, err)
      call check_(err == CAPNP_OK, 'far: setp cross-segment')
      call capnp_set_root(msg, root, err)
      call capnp_serialize_bytes(msg, bytes, err)
      call capnp_deserialize_bytes(bytes, rmsg, err)
      r = capnp_root(rmsg, err)
      rk = capnp_getp(r, 0, err)
      call check_(err == CAPNP_OK .and. rk%kind == CAPNP_PK_STRUCT, 'far: resolves through pad')
      call check_(capnp_get_i64(rk, 0_int64) == 111_int64 .and. &
                  capnp_get_i64(rk, 8_int64) == 222_int64, 'far: fields intact')
      call capnp_message_free(msg)
      call capnp_message_free(rmsg)
   end subroutine t_far_pointer

   !> Vectors from https://capnproto.org/encoding.html#packing
   subroutine t_packed_spec_vectors()
      integer(int8) :: unpacked(0:15)
      integer(int8), allocatable :: packed(:), back(:)
      integer(int8) :: expect(0:7)
      integer :: err, vals(16), i, ev(8)
      vals = [int(z'08'), 0, 0, 0, int(z'03'), 0, int(z'02'), 0, &
              int(z'19'), 0, 0, 0, int(z'aa'), int(z'01'), 0, 0]
      do i = 1, 16
         unpacked(i - 1) = int(merge(vals(i) - 256, vals(i), vals(i) > 127), int8)
      end do
      call capnp_pack(unpacked, packed, err)
      ev = [int(z'51'), int(z'08'), int(z'03'), int(z'02'), int(z'31'), int(z'19'), &
            int(z'aa'), int(z'01')]
      do i = 1, 8
         expect(i - 1) = int(merge(ev(i) - 256, ev(i), ev(i) > 127), int8)
      end do
      call check_(err == CAPNP_OK .and. size(packed) == 8, 'packed: spec vector size')
      call check_(all(packed == expect), 'packed: spec vector bytes')
      call capnp_unpack(packed, back, err)
      call check_(err == CAPNP_OK .and. size(back) == 16 .and. all(back == unpacked), &
                  'packed: spec vector inverts')
      ! Two all-zero words -> 00 01.
      unpacked = 0_int8
      call capnp_pack(unpacked, packed, err)
      call check_(size(packed) == 2 .and. packed(0) == 0_int8 .and. packed(1) == 1_int8, &
                  'packed: zero-run escape')
      call capnp_unpack(packed, back, err)
      call check_(size(back) == 16 .and. all(back == 0_int8), 'packed: zero-run inverts')
      ! Two fully nonzero words -> ff <8> 01 <8>.
      unpacked = 1_int8
      call capnp_pack(unpacked, packed, err)
      call check_(size(packed) == 18 .and. packed(0) == -1_int8 .and. packed(9) == 1_int8, &
                  'packed: literal-run escape')
      call capnp_unpack(packed, back, err)
      call check_(size(back) == 16 .and. all(back == 1_int8), 'packed: literal-run inverts')
   end subroutine t_packed_spec_vectors

   subroutine t_packed_roundtrip()
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root
      integer(int8), allocatable :: bytes(:), packed(:), back(:)
      integer :: err, i
      call capnp_message_init_builder(msg, err)
      root = capnp_new_struct(msg, 8, 1, err)
      do i = 0, 7
         call capnp_set_i64(root, int(8*i, int64), int(i, int64)*int(z'0101010101', int64), err)
      end do
      call capnp_set_text(root, 0, 'pack me', err)
      call capnp_set_root(msg, root, err)
      call capnp_serialize_bytes(msg, bytes, err)
      call capnp_pack(bytes, packed, err)
      call check_(err == CAPNP_OK, 'packed: pack message')
      call capnp_unpack(packed, back, err)
      call check_(err == CAPNP_OK .and. size(back) == size(bytes) .and. all(back == bytes), &
                  'packed: message round trip')
      call capnp_message_free(msg)
   end subroutine t_packed_roundtrip

   subroutine t_framing_errors()
      type(capnp_message_t), target :: rmsg
      integer(int8) :: junk(0:6)
      integer(int8) :: hdr(0:15)
      integer :: err
      junk = 0_int8
      call capnp_deserialize_bytes(junk, rmsg, err)
      call check_(err == CAPNP_ERR_FRAMING, 'framing: short buffer rejected')
      hdr = 0_int8
      hdr(4) = 100_int8 ! claims a 100-word segment that is not there
      call capnp_deserialize_bytes(hdr, rmsg, err)
      call check_(err == CAPNP_ERR_FRAMING, 'framing: truncated segment rejected')
   end subroutine t_framing_errors

   subroutine t_traversal_limit()
      type(capnp_message_t), target :: msg, rmsg
      type(capnp_ptr_t) :: root, r
      integer(int8), allocatable :: bytes(:)
      integer :: err
      call capnp_message_init_builder(msg, err)
      root = capnp_new_struct(msg, 64, 0, err)
      call capnp_set_root(msg, root, err)
      call capnp_serialize_bytes(msg, bytes, err)
      call capnp_deserialize_bytes(bytes, rmsg, err, traversal_words=4_int64)
      r = capnp_root(rmsg, err)
      call check_(err == CAPNP_ERR_TRAVERSAL, 'guards: traversal limit enforced')
      call capnp_message_free(msg)
      call capnp_message_free(rmsg)
   end subroutine t_traversal_limit

   !> Pointer resolution charges nesting depth; a zero depth_limit still
   !> allows the root (depth 0) but fails the first capnp_getp (depth 1).
   subroutine t_depth_limit()
      type(capnp_message_t), target :: msg, rmsg
      type(capnp_ptr_t) :: root, kid, q
      integer(int8), allocatable :: bytes(:)
      integer :: err
      call capnp_message_init_builder(msg, err)
      root = capnp_new_struct(msg, 0, 1, err)
      kid = capnp_new_struct(msg, 1, 0, err)
      call capnp_set_i32(kid, 0_int64, 7_int32, err)
      call capnp_setp(root, 0, kid, err)
      call capnp_set_root(msg, root, err)
      call capnp_serialize_bytes(msg, bytes, err)
      call capnp_deserialize_bytes(bytes, rmsg, err, depth_limit=0)
      root = capnp_root(rmsg, err)
      call check_(err == CAPNP_OK .and. root%kind == CAPNP_PK_STRUCT, &
                  'guards: root at depth 0 allowed')
      q = capnp_getp(root, 0, err)
      call check_(err == CAPNP_ERR_DEPTH, 'guards: depth limit on getp')
      call capnp_message_free(msg)
      call capnp_message_free(rmsg)
   end subroutine t_depth_limit

end program check
