!> Wire / codec unit tests using fortran-lang test-drive.
module test_wire
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use capnp
   implicit none
   private

   public :: collect_wire

contains

   subroutine collect_wire(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [ &
           new_unittest("endian", test_endian), &
           new_unittest("pointer-words", test_pointer_words), &
           new_unittest("golden-single-struct", test_golden_single_struct), &
           new_unittest("struct-fields-roundtrip", test_struct_fields_roundtrip), &
           new_unittest("text-data", test_text_data), &
           new_unittest("lists", test_lists), &
           new_unittest("composite-list", test_composite_list), &
           new_unittest("nested-struct", test_nested_struct), &
           new_unittest("far-pointer", test_far_pointer), &
           new_unittest("double-far-e2e", test_double_far_e2e), &
           new_unittest("wire-capability", test_wire_capability), &
           new_unittest("packed-spec-vectors", test_packed_spec_vectors), &
           new_unittest("packed-roundtrip", test_packed_roundtrip), &
           new_unittest("packed-incremental", test_packed_incremental), &
           new_unittest("framing-errors", test_framing_errors), &
           new_unittest("traversal-limit", test_traversal_limit), &
           new_unittest("depth-limit", test_depth_limit), &
           new_unittest("depth-limit-nested", test_depth_limit_nested)]
   end subroutine collect_wire

   subroutine test_endian(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int8) :: buf(0:15)
      buf = 0_int8
      call cp_put_i32(buf, 0_int64, int(z'12345678', int32))
      call check(error, buf(0) == int(z'78', int8) .and. buf(1) == int(z'56', int8) .and. &
                  buf(2) == int(z'34', int8) .and. buf(3) == int(z'12', int8), 'endian: i32 LE bytes')
      if (allocated(error)) return
      call check(error, cp_get_i32(buf, 0_int64) == int(z'12345678', int32), 'endian: i32 round trip')
      if (allocated(error)) return
      call cp_put_i64(buf, 8_int64, -1_int64)
      call check(error, cp_get_i64(buf, 8_int64) == -1_int64, 'endian: i64 all-ones')
      if (allocated(error)) return
      call cp_put_i16(buf, 4_int64, -2_int16)
      call check(error, cp_get_i16(buf, 4_int64) == -2_int16, 'endian: i16 negative')
      if (allocated(error)) return
      call cp_put_f64(buf, 8_int64, 1.5_real64)
      call check(error, cp_get_f64(buf, 8_int64) == 1.5_real64, 'endian: f64 round trip')
      if (allocated(error)) return
   end subroutine test_endian
   subroutine test_pointer_words(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64) :: w
      w = wp_make_struct(0_int32, 1_int32, 0_int32)
      call check(error, w == int(z'0000000100000000', int64), 'ptr: struct C=1 D=0 golden')
      if (allocated(error)) return
      call check(error, wp_kind(w) == CAPNP_WK_STRUCT .and. wp_offset(w) == 0 .and. &
                  wp_struct_dwords(w) == 1 .and. wp_struct_pwords(w) == 0, 'ptr: struct decode')
      if (allocated(error)) return
      w = wp_make_struct(-2_int32, 3_int32, 4_int32)
      call check(error, wp_offset(w) == -2 .and. wp_struct_dwords(w) == 3 .and. &
                  wp_struct_pwords(w) == 4, 'ptr: negative offset')
      if (allocated(error)) return
      w = wp_make_list(5_int32, CAPNP_SZ_FOUR, 7_int64)
      call check(error, wp_kind(w) == CAPNP_WK_LIST .and. wp_offset(w) == 5 .and. &
                  wp_list_esize(w) == CAPNP_SZ_FOUR .and. wp_list_count(w) == 7_int64, &
                  'ptr: list decode')
      if (allocated(error)) return
      w = wp_make_far(.false., 3_int64, 1_int64)
      call check(error, wp_kind(w) == CAPNP_WK_FAR .and. .not. wp_far_two(w) .and. &
                  wp_far_off(w) == 3_int64 .and. wp_far_seg(w) == 1_int64, 'ptr: far decode')
      if (allocated(error)) return
      w = wp_make_far(.true., 0_int64, 2_int64)
      call check(error, wp_far_two(w), 'ptr: double-far flag')
      if (allocated(error)) return
      w = wp_make_cap(9_int64)
      call check(error, wp_kind(w) == CAPNP_WK_CAP .and. wp_cap_index(w) == 9_int64, 'ptr: cap decode')
      if (allocated(error)) return
   end subroutine test_pointer_words
   !> Golden bytes: root struct with one data word holding u32 0x12345678.
   subroutine test_golden_single_struct(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root
      integer(int8), allocatable :: bytes(:)
      integer(int8) :: expect(0:23)
      integer :: err, i
      call capnp_message_init_builder(msg, err)
      call check(error, err == CAPNP_OK, 'golden: init')
      if (allocated(error)) return
      root = capnp_new_struct(msg, 1, 0, err)
      call capnp_set_u32(root, 0_int64, int(z'12345678', int64), err)
      call check(error, err == CAPNP_OK, 'golden: set field')
      if (allocated(error)) return
      call capnp_set_root(msg, root, err)
      call capnp_serialize_bytes(msg, bytes, err)
      call check(error, err == CAPNP_OK .and. size(bytes) == 24, 'golden: 24 bytes total')
      if (allocated(error)) return
      expect = 0_int8
      expect(4) = 2_int8                 ! segment size: 2 words
      expect(12) = 1_int8                ! root ptr: dwords=1 at byte 12
      expect(16) = int(z'78', int8)
      expect(17) = int(z'56', int8)
      expect(18) = int(z'34', int8)
      expect(19) = int(z'12', int8)
      do i = 0, 23
         if (bytes(i) /= expect(i)) then
            call check(error, .false., 'golden: byte mismatch')
            if (allocated(error)) return
            print '(a,i0,a,i0,a,i0)', '  byte ', i, ': got ', bytes(i), ' want ', expect(i)
            exit
         end if
      end do
      call capnp_message_free(msg)
   end subroutine test_golden_single_struct
   subroutine test_struct_fields_roundtrip(error)
      type(error_type), allocatable, intent(out) :: error
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
      call check(error, err == CAPNP_OK .and. r%kind == CAPNP_PK_STRUCT, 'fields: root resolves')
      if (allocated(error)) return
      call check(error, capnp_get_i8(r, 0_int64) == -5_int8, 'fields: i8')
      if (allocated(error)) return
      call check(error, capnp_get_i16(r, 2_int64) == -300_int16, 'fields: i16')
      if (allocated(error)) return
      call check(error, capnp_get_i32(r, 4_int64, default=7_int32) == 123456_int32, 'fields: i32+default')
      if (allocated(error)) return
      call check(error, capnp_get_i64(r, 8_int64) == -9876543210_int64, 'fields: i64')
      if (allocated(error)) return
      call check(error, capnp_get_u8(r, 16_int64) == 250_int16, 'fields: u8')
      if (allocated(error)) return
      call check(error, capnp_get_u16(r, 18_int64) == 65500_int32, 'fields: u16')
      if (allocated(error)) return
      call check(error, capnp_get_u32(r, 20_int64) == 4000000000_int64, 'fields: u32')
      if (allocated(error)) return
      call check(error, capnp_get_f32(r, 24_int64) == 3.25_real32, 'fields: f32')
      if (allocated(error)) return
      call check(error, capnp_get_bool(r, 224_int64), 'fields: bool set')
      if (allocated(error)) return
      call check(error, capnp_get_bool(r, 225_int64, default=.true.), 'fields: bool default XOR')
      if (allocated(error)) return
      ! Reads past the data section return defaults (older-schema semantics).
      call check(error, capnp_get_i32(r, 4096_int64, default=42_int32) == 42_int32, 'fields: OOB read -> default')
      if (allocated(error)) return
      call capnp_message_free(msg)
      call capnp_message_free(rmsg)
   end subroutine test_struct_fields_roundtrip
   subroutine test_text_data(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg, rmsg
      type(capnp_ptr_t) :: root, r, tl
      integer(int8), allocatable :: bytes(:), blob(:), rblob(:)
      character(len=:), allocatable :: s
      integer :: err
      call capnp_message_init_builder(msg, err)
      root = capnp_new_struct(msg, 0, 2, err)
      call capnp_set_text(root, 0, 'Hello, Cap''n Proto!', err)
      call check(error, err == CAPNP_OK, 'text: set')
      if (allocated(error)) return
      allocate (blob(0:4))
      blob = [0_int8, 1_int8, 2_int8, 3_int8, 4_int8]
      blob(0) = -34_int8 ! 0xde
      call capnp_set_data(root, 1, blob, err)
      call capnp_set_root(msg, root, err)
      call capnp_serialize_bytes(msg, bytes, err)
      call capnp_deserialize_bytes(bytes, rmsg, err)
      r = capnp_root(rmsg, err)
      call capnp_get_text(r, 0, s, err)
      call check(error, err == CAPNP_OK .and. s == 'Hello, Cap''n Proto!', 'text: round trip')
      if (allocated(error)) return
      ! Text wire shape: NUL included in element count.
      tl = capnp_getp(r, 0, err)
      call check(error, tl%kind == CAPNP_PK_LIST .and. tl%esize == CAPNP_SZ_BYTE .and. &
                  tl%nelem == 20_int64, 'text: count includes NUL')
      if (allocated(error)) return
      call capnp_get_data(r, 1, rblob, err)
      call check(error, err == CAPNP_OK .and. size(rblob) == 5 .and. all(rblob == blob), 'data: round trip')
      if (allocated(error)) return
      ! Absent pointer field reads as empty text, no error.
      call capnp_get_text(r, 5, s, err)
      call check(error, err == CAPNP_OK .and. len(s) == 0, 'text: absent -> empty')
      if (allocated(error)) return
      call capnp_message_free(msg)
      call capnp_message_free(rmsg)
   end subroutine test_text_data
   subroutine test_lists(error)
      type(error_type), allocatable, intent(out) :: error
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
      call check(error, rl%kind == CAPNP_PK_LIST .and. capnp_list_len(rl) == 5_int64, 'list: i32 len')
      if (allocated(error)) return
      v = capnp_list_get_i32(rl, 3_int64, err)
      call check(error, err == CAPNP_OK .and. v == 31_int32, 'list: i32 element')
      if (allocated(error)) return
      rl = capnp_getp(r, 1, err)
      call check(error, capnp_list_len(rl) == 10_int64, 'list: bit len')
      if (allocated(error)) return
      call check(error, capnp_list_get_bool(rl, 3_int64, err) .and. .not. capnp_list_get_bool(rl, 4_int64, err), &
                  'list: bit elements')
      if (allocated(error)) return
      call check(error, capnp_list_get_bool(rl, 9_int64, err), 'list: last bit')
      if (allocated(error)) return
      call capnp_message_free(msg)
      call capnp_message_free(rmsg)
   end subroutine test_lists
   subroutine test_composite_list(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg, rmsg
      type(capnp_ptr_t) :: root, lst, el, r, rl
      integer(int8), allocatable :: bytes(:)
      character(len=:), allocatable :: s
      integer :: err, i
      call capnp_message_init_builder(msg, err)
      root = capnp_new_struct(msg, 0, 1, err)
      lst = capnp_new_composite_list(msg, 3_int64, 1, 1, err)
      call check(error, err == CAPNP_OK .and. lst%nelem == 3_int64, 'clist: new')
      if (allocated(error)) return
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
      call check(error, err == CAPNP_OK .and. rl%esize == CAPNP_SZ_COMPOSITE .and. &
                  capnp_list_len(rl) == 3_int64, 'clist: read len')
      if (allocated(error)) return
      el = capnp_list_get_struct(rl, 2, err)
      call check(error, capnp_get_i32(el, 0_int64) == 102_int32, 'clist: element field')
      if (allocated(error)) return
      call capnp_get_text(el, 0, s, err)
      call check(error, s == 'element', 'clist: element text')
      if (allocated(error)) return
      call capnp_message_free(msg)
      call capnp_message_free(rmsg)
   end subroutine test_composite_list
   subroutine test_nested_struct(error)
      type(error_type), allocatable, intent(out) :: error
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
      call check(error, err == CAPNP_OK .and. rk%kind == CAPNP_PK_STRUCT, 'nested: resolves')
      if (allocated(error)) return
      call check(error, capnp_get_i64(rk, 0_int64) == 777_int64, 'nested: field')
      if (allocated(error)) return
      ! Out-of-range pointer index resolves null without error.
      rk = capnp_getp(r, 3, err)
      call check(error, err == CAPNP_OK .and. rk%kind == CAPNP_PK_NULL, 'nested: OOB ptr -> null')
      if (allocated(error)) return
      call capnp_message_free(msg)
      call capnp_message_free(rmsg)
   end subroutine test_nested_struct
   !> A tiny first segment forces the second object into a new segment, so
   !> the root's pointer field must become a far pointer.
   subroutine test_far_pointer(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg, rmsg
      type(capnp_ptr_t) :: root, kid, r, rk
      integer(int8), allocatable :: bytes(:)
      integer :: err
      call capnp_message_init_builder(msg, err, first_words=3_int64)
      root = capnp_new_struct(msg, 1, 1, err)   ! fills the first segment
      kid = capnp_new_struct(msg, 2, 0, err)    ! must spill to segment 2
      call check(error, msg%nsegs >= 2, 'far: second segment created')
      if (allocated(error)) return
      call check(error, root%seg /= kid%seg, 'far: objects in different segments')
      if (allocated(error)) return
      call capnp_set_i64(kid, 0_int64, 111_int64, err)
      call capnp_set_i64(kid, 8_int64, 222_int64, err)
      call capnp_setp(root, 0, kid, err)
      call check(error, err == CAPNP_OK, 'far: setp cross-segment')
      if (allocated(error)) return
      call capnp_set_root(msg, root, err)
      call capnp_serialize_bytes(msg, bytes, err)
      call capnp_deserialize_bytes(bytes, rmsg, err)
      r = capnp_root(rmsg, err)
      rk = capnp_getp(r, 0, err)
      call check(error, err == CAPNP_OK .and. rk%kind == CAPNP_PK_STRUCT, 'far: resolves through pad')
      if (allocated(error)) return
      call check(error, capnp_get_i64(rk, 0_int64) == 111_int64 .and. &
                  capnp_get_i64(rk, 8_int64) == 222_int64, 'far: fields intact')
      if (allocated(error)) return
      call capnp_message_free(msg)
      call capnp_message_free(rmsg)
   end subroutine test_far_pointer
   !> Multi-segment double-far: pointer on seg A, landing pad (single far +
   !> tag) on seg B, object on seg C. Serialize, deserialize, resolve through
   !> resolve_double_far and read the object's data word.
   subroutine test_double_far_e2e(error)
      use capnp_endian, only: cp_put_i64
      use capnp_arena, only: capnp_arena_alloc_in
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg, rmsg
      type(capnp_ptr_t) :: root, obj, junk, r, rk
      integer(int8), allocatable :: bytes(:)
      integer :: err, i
      integer(int64) :: pad_off, slot, far_w, tag_w
      call capnp_message_init_builder(msg, err, first_words=2_int64)
      ! Root on seg 1: empty data, one pointer slot.
      root = capnp_new_struct(msg, 0, 1, err)
      call capnp_set_root(msg, root, err)
      ! Force at least three segments: fill remaining capacity, then allocate.
      do i = 1, 8
         junk = capnp_new_struct(msg, 4, 0, err)
         if (msg%nsegs >= 3) exit
      end do
      call check(error, msg%nsegs >= 3, 'dfar: three segments available')
      if (allocated(error)) return
      ! Object on the last segment: one data word.
      obj = capnp_new_struct(msg, 1, 0, err)
      call check(error, obj%seg == msg%nsegs, 'dfar: object on last segment')
      if (allocated(error)) return
      call capnp_set_i64(obj, 0_int64, 4242_int64, err)
      ! Landing pad (2 words) on segment 2, not the object's segment.
      call check(error, obj%seg /= 2, 'dfar: pad and object on different segs')
      if (allocated(error)) return
      call capnp_arena_alloc_in(msg, 2, 2_int64, pad_off, err)
      call check(error, err == CAPNP_OK, 'dfar: pad allocated on seg 2')
      if (allocated(error)) return
      ! Pad word 0: single far to object. Word 1: struct tag, offset 0.
      far_w = wp_make_far(.false., obj%off/8_int64, int(obj%seg - 1, int64))
      tag_w = wp_make_struct(0_int32, 1_int32, 0_int32)
      call cp_put_i64(msg%segs(2)%bytes, pad_off, far_w)
      call cp_put_i64(msg%segs(2)%bytes, pad_off + 8_int64, tag_w)
      ! Root pointer slot: double-far to the pad (segment id 1 = seg 2).
      slot = root%off + int(root%dwords, int64)*8_int64
      call cp_put_i64(msg%segs(root%seg)%bytes, slot, &
                      wp_make_far(.true., pad_off/8_int64, 1_int64))
      call capnp_serialize_bytes(msg, bytes, err)
      call check(error, err == CAPNP_OK, 'dfar: serializes')
      if (allocated(error)) return
      call capnp_deserialize_bytes(bytes, rmsg, err)
      call check(error, err == CAPNP_OK, 'dfar: deserializes')
      if (allocated(error)) return
      r = capnp_root(rmsg, err)
      rk = capnp_getp(r, 0, err)
      call check(error, err == CAPNP_OK .and. rk%kind == CAPNP_PK_STRUCT, &
                  'dfar: resolves through double-far')
      if (allocated(error)) return
      call check(error, capnp_get_i64(rk, 0_int64) == 4242_int64, 'dfar: data intact')
      if (allocated(error)) return
      call capnp_message_free(msg)
      call capnp_message_free(rmsg)
   end subroutine test_double_far_e2e
   !> Pure-wire capability pointer: set a CAP pointer by index, serialize,
   !> deserialize, and read the same index back (no RPC connection).
   subroutine test_wire_capability(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg, rmsg
      type(capnp_ptr_t) :: root, cap, r, q
      integer(int8), allocatable :: bytes(:)
      integer :: err
      call capnp_message_init_builder(msg, err)
      root = capnp_new_struct(msg, 0, 1, err)
      cap%kind = CAPNP_PK_CAP
      cap%capidx = 7_int64
      cap%msg => msg
      call capnp_setp(root, 0, cap, err)
      call check(error, err == CAPNP_OK, 'capwire: setp cap')
      if (allocated(error)) return
      call capnp_set_root(msg, root, err)
      call capnp_serialize_bytes(msg, bytes, err)
      call check(error, err == CAPNP_OK, 'capwire: serializes')
      if (allocated(error)) return
      call capnp_deserialize_bytes(bytes, rmsg, err)
      r = capnp_root(rmsg, err)
      q = capnp_getp(r, 0, err)
      call check(error, err == CAPNP_OK .and. q%kind == CAPNP_PK_CAP, 'capwire: kind CAP')
      if (allocated(error)) return
      call check(error, q%capidx == 7_int64, 'capwire: index 7 round-trips')
      if (allocated(error)) return
      call capnp_message_free(msg)
      call capnp_message_free(rmsg)
   end subroutine test_wire_capability
   !> Vectors from https://capnproto.org/encoding.html#packing
   subroutine test_packed_spec_vectors(error)
      type(error_type), allocatable, intent(out) :: error
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
      call check(error, err == CAPNP_OK .and. size(packed) == 8, 'packed: spec vector size')
      if (allocated(error)) return
      call check(error, all(packed == expect), 'packed: spec vector bytes')
      if (allocated(error)) return
      call capnp_unpack(packed, back, err)
      call check(error, err == CAPNP_OK .and. size(back) == 16 .and. all(back == unpacked), &
                  'packed: spec vector inverts')
      if (allocated(error)) return
      ! Two all-zero words -> 00 01.
      unpacked = 0_int8
      call capnp_pack(unpacked, packed, err)
      call check(error, size(packed) == 2 .and. packed(0) == 0_int8 .and. packed(1) == 1_int8, &
                  'packed: zero-run escape')
      if (allocated(error)) return
      call capnp_unpack(packed, back, err)
      call check(error, size(back) == 16 .and. all(back == 0_int8), 'packed: zero-run inverts')
      if (allocated(error)) return
      ! Two fully nonzero words -> ff <8> 01 <8>.
      unpacked = 1_int8
      call capnp_pack(unpacked, packed, err)
      call check(error, size(packed) == 18 .and. packed(0) == -1_int8 .and. packed(9) == 1_int8, &
                  'packed: literal-run escape')
      if (allocated(error)) return
      call capnp_unpack(packed, back, err)
      call check(error, size(back) == 16 .and. all(back == 1_int8), 'packed: literal-run inverts')
      if (allocated(error)) return
   end subroutine test_packed_spec_vectors
   subroutine test_packed_roundtrip(error)
      type(error_type), allocatable, intent(out) :: error
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
      call check(error, err == CAPNP_OK, 'packed: pack message')
      if (allocated(error)) return
      call capnp_unpack(packed, back, err)
      call check(error, err == CAPNP_OK .and. size(back) == size(bytes) .and. all(back == bytes), &
                  'packed: message round trip')
      if (allocated(error)) return
      call capnp_message_free(msg)
   end subroutine test_packed_roundtrip
   !> Incremental packer/unpacker must match whole-buffer pack across chunk
   !> sizes, and reject unfinished partial words on finish.
   subroutine test_packed_incremental(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int8) :: plain(0:39)
      integer(int8), allocatable :: whole(:), incr(:), out(:), back(:)
      type(capnp_packer_t) :: pk
      type(capnp_unpacker_t) :: un
      integer(int64) :: outn, i, n, chunk
      integer :: err, csz
      ! Mix zero runs, sparse tags, and full-literal words.
      plain = 0_int8
      plain(0) = 7_int8
      plain(8:15) = 1_int8
      plain(16) = int(z'aa', int8)
      plain(24:31) = 0_int8
      plain(32) = 3_int8
      plain(39) = 9_int8
      call capnp_pack(plain, whole, err)
      call check(error, err == CAPNP_OK, 'packed-incr: whole pack ok')
      if (allocated(error)) return

      ! Pack in 1-byte chunks (stress partial-word carry).
      pk = capnp_packer_t()
      if (allocated(incr)) deallocate (incr)
      outn = 0_int64
      n = size(plain, kind=int64)
      do i = 0_int64, n - 1_int64
         call capnp_pack_push(pk, plain(i:i), incr, outn, err)
         if (err /= CAPNP_OK) exit
      end do
      call check(error, err == CAPNP_OK, 'packed-incr: 1-byte push ok')
      if (allocated(error)) return
      call capnp_pack_finish(pk, incr, outn, err)
      call check(error, err == CAPNP_OK, 'packed-incr: finish after 1-byte chunks')
      if (allocated(error)) return
      call check(error, outn == size(whole, kind=int64) .and. all(incr(0:outn - 1) == whole), &
                  'packed-incr: 1-byte chunks match whole pack')
      if (allocated(error)) return

      ! Pack in 3-byte chunks (cross word boundaries).
      pk = capnp_packer_t()
      if (allocated(incr)) deallocate (incr)
      outn = 0_int64
      i = 0_int64
      do while (i < n)
         chunk = min(3_int64, n - i)
         call capnp_pack_push(pk, plain(i:i + chunk - 1), incr, outn, err)
         if (err /= CAPNP_OK) exit
         i = i + chunk
      end do
      call check(error, err == CAPNP_OK, 'packed-incr: 3-byte push ok')
      if (allocated(error)) return
      call capnp_pack_finish(pk, incr, outn, err)
      call check(error, err == CAPNP_OK .and. outn == size(whole, kind=int64) .and. &
                  all(incr(0:outn - 1) == whole), &
                  'packed-incr: 3-byte chunks match whole pack')
      if (allocated(error)) return

      ! Unpack the packed stream in 2-byte chunks.
      un = capnp_unpacker_t()
      if (allocated(back)) deallocate (back)
      outn = 0_int64
      n = size(whole, kind=int64)
      i = 0_int64
      do while (i < n)
         chunk = min(2_int64, n - i)
         call capnp_unpack_push(un, whole(i:i + chunk - 1), back, outn, err)
         if (err /= CAPNP_OK) exit
         i = i + chunk
      end do
      call check(error, err == CAPNP_OK, 'packed-incr: unpack_push ok')
      if (allocated(error)) return
      call check(error, outn == size(plain, kind=int64) .and. all(back(0:outn - 1) == plain), &
                  'packed-incr: chunked unpack recovers plain')
      if (allocated(error)) return

      ! Whole pack rejects non-word-aligned length.
      call capnp_pack(plain(0:4), out, err)
      call check(error, err == CAPNP_ERR_ARG, 'packed-incr: non-aligned pack rejected')
      if (allocated(error)) return

      ! Finish with a hanging partial word is an error.
      pk = capnp_packer_t()
      if (allocated(incr)) deallocate (incr)
      outn = 0_int64
      call capnp_pack_push(pk, plain(0:2), incr, outn, err)
      call check(error, err == CAPNP_OK, 'packed-incr: partial push ok')
      if (allocated(error)) return
      call capnp_pack_finish(pk, incr, outn, err)
      call check(error, err == CAPNP_ERR_ARG, 'packed-incr: finish partial word rejected')
      if (allocated(error)) return
   end subroutine test_packed_incremental
   subroutine test_framing_errors(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: rmsg
      integer(int8) :: junk(0:6)
      integer(int8) :: hdr(0:15)
      integer :: err
      junk = 0_int8
      call capnp_deserialize_bytes(junk, rmsg, err)
      call check(error, err == CAPNP_ERR_FRAMING, 'framing: short buffer rejected')
      if (allocated(error)) return
      hdr = 0_int8
      hdr(4) = 100_int8 ! claims a 100-word segment that is not there
      call capnp_deserialize_bytes(hdr, rmsg, err)
      call check(error, err == CAPNP_ERR_FRAMING, 'framing: truncated segment rejected')
      if (allocated(error)) return
   end subroutine test_framing_errors
   subroutine test_traversal_limit(error)
      type(error_type), allocatable, intent(out) :: error
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
      call check(error, err == CAPNP_ERR_TRAVERSAL, 'guards: traversal limit enforced')
      if (allocated(error)) return
      call capnp_message_free(msg)
      call capnp_message_free(rmsg)
   end subroutine test_traversal_limit
   !> Pointer resolution charges nesting depth; a zero depth_limit still
   !> allows the root (depth 0) but fails the first capnp_getp (depth 1).
   subroutine test_depth_limit(error)
      type(error_type), allocatable, intent(out) :: error
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
      call check(error, err == CAPNP_OK .and. root%kind == CAPNP_PK_STRUCT, &
                  'guards: root at depth 0 allowed')
      if (allocated(error)) return
      q = capnp_getp(root, 0, err)
      call check(error, err == CAPNP_ERR_DEPTH, 'guards: depth limit on getp')
      if (allocated(error)) return
      call capnp_message_free(msg)
      call capnp_message_free(rmsg)
   end subroutine test_depth_limit
   !> Nested walks accumulate depth on the handle: with depth_limit=1 a
   !> two-hop chain (root -> a -> b) allows the first getp and fails the second.
   subroutine test_depth_limit_nested(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg, rmsg
      type(capnp_ptr_t) :: root, a, b, q
      integer(int8), allocatable :: bytes(:)
      integer :: err
      call capnp_message_init_builder(msg, err)
      root = capnp_new_struct(msg, 0, 1, err)
      a = capnp_new_struct(msg, 0, 1, err)
      b = capnp_new_struct(msg, 1, 0, err)
      call capnp_set_i32(b, 0_int64, 99_int32, err)
      call capnp_setp(a, 0, b, err)
      call capnp_setp(root, 0, a, err)
      call capnp_set_root(msg, root, err)
      call capnp_serialize_bytes(msg, bytes, err)
      call capnp_deserialize_bytes(bytes, rmsg, err, depth_limit=1)
      root = capnp_root(rmsg, err)
      call check(error, err == CAPNP_OK, 'guards: nested root ok')
      if (allocated(error)) return
      a = capnp_getp(root, 0, err)
      call check(error, err == CAPNP_OK .and. a%kind == CAPNP_PK_STRUCT, &
                  'guards: first hop within limit')
      if (allocated(error)) return
      call check(error, a%depth == 1, 'guards: first hop depth is 1')
      if (allocated(error)) return
      q = capnp_getp(a, 0, err)
      call check(error, err == CAPNP_ERR_DEPTH, 'guards: second hop exceeds limit')
      if (allocated(error)) return
      call capnp_message_free(msg)
      call capnp_message_free(rmsg)
   end subroutine test_depth_limit_nested

end module test_wire
