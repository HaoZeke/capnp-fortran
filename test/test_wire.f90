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
           new_unittest("empty-struct-offset-minus-one", test_empty_struct_offset), &
           new_unittest("golden-single-struct", test_golden_single_struct), &
           new_unittest("struct-fields-roundtrip", test_struct_fields_roundtrip), &
           new_unittest("text-data", test_text_data), &
           new_unittest("overwrite-text-zero", test_overwrite_text_zero), &
           new_unittest("lists", test_lists), &
           new_unittest("list-count-limit", test_list_count_limit), &
           new_unittest("void-list-struct", test_void_list_struct), &
           new_unittest("void-list-amplify", test_void_list_amplify), &
           new_unittest("empty-list-amplify", test_empty_list_amplify), &
           new_unittest("composite-list", test_composite_list), &
           new_unittest("composite-count-unsigned", test_composite_count_unsigned), &
           new_unittest("nested-struct", test_nested_struct), &
           new_unittest("far-pointer", test_far_pointer), &
           new_unittest("far-chain-rejected", test_far_chain_rejected), &
           new_unittest("double-far-e2e", test_double_far_e2e), &
           new_unittest("far-segment-overflow-zero", test_far_segment_overflow_zero), &
           new_unittest("far-landing-pad-zero", test_far_landing_pad_zero), &
           new_unittest("cross-message-copy-clears-slot", test_cross_message_copy_clears_slot), &
           new_unittest("overwrite-zeroing", test_overwrite_zeroing), &
           new_unittest("wire-capability", test_wire_capability), &
           new_unittest("cap-reserved-bits", test_cap_reserved_bits), &
           new_unittest("packed-spec-vectors", test_packed_spec_vectors), &
           new_unittest("packed-roundtrip", test_packed_roundtrip), &
           new_unittest("packed-incremental", test_packed_incremental), &
           new_unittest("packed-unpack-finish", test_packed_unpack_finish), &
           new_unittest("packed-size-limit", test_packed_size_limit), &
           new_unittest("packed-segment-boundary", test_packed_segment_boundary), &
           new_unittest("packed-header-boundary", test_packed_header_boundary), &
           new_unittest("framing-errors", test_framing_errors), &
           new_unittest("framing-many-segments", test_framing_many_segments), &
           new_unittest("framing-file-limits", test_framing_file_limits), &
           new_unittest("read-empty-file", test_read_empty_file), &
           new_unittest("stream-reader-options", test_stream_reader_options), &
           new_unittest("amplified-lists", test_amplified_lists), &
           new_unittest("zero-size-target-clamp", test_zero_size_target_clamp), &
           new_unittest("far-pointer-parity", test_far_pointer_parity), &
           new_unittest("traversal-limit", test_traversal_limit), &
           new_unittest("traversal-root-words", test_traversal_root_words), &
           new_unittest("traversal-single-far", test_traversal_single_far), &
           new_unittest("traversal-double-far", test_traversal_double_far), &
           new_unittest("depth-limit", test_depth_limit), &
           new_unittest("depth-limit-nested", test_depth_limit_nested), &
           new_unittest("nest-limit-64", test_nest_limit_64), &
           new_unittest("far-depth-one", test_far_depth_one)]
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
      w = wp_make_struct(-1_int32, 0_int32, 0_int32)
      call check(error, wp_offset(w) == -1 .and. wp_struct_dwords(w) == 0 .and. &
                  wp_struct_pwords(w) == 0, 'ptr: empty struct B=-1')
      if (allocated(error)) return
   end subroutine test_pointer_words

   subroutine test_empty_struct_offset(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg, rmsg
      type(capnp_ptr_t) :: root, r
      integer(int8), allocatable :: bytes(:)
      integer :: err
      call capnp_message_init_builder(msg, err)
      root = capnp_new_struct(msg, 0, 0, err)
      call capnp_set_root(msg, root, err)
      call capnp_serialize_bytes(msg, bytes, err)
      call check(error, err == CAPNP_OK .and. size(bytes) >= 16, 'empty: framed')
      if (allocated(error)) return
      call check(error, bytes(8) == int(z'fc', int8) .and. bytes(9) == -1_int8 .and. &
                  bytes(10) == -1_int8 .and. bytes(11) == -1_int8, 'empty: 0xFFFFFFFC')
      if (allocated(error)) return
      call capnp_deserialize_bytes(bytes, rmsg, err)
      r = capnp_root(rmsg, err)
      call check(error, err == CAPNP_OK .and. r%kind == CAPNP_PK_STRUCT .and. &
                  r%dwords == 0 .and. r%pwords == 0, 'empty: read as struct not null')
      if (allocated(error)) return
      call capnp_message_free(msg)
      call capnp_message_free(rmsg)
   end subroutine test_empty_struct_offset
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
   !> Overwriting Text must erase the previous bytes. Compared with the
   !> flat array from C++ 1.4.0 MallocMessageBuilder for Holder.text.
   subroutine test_overwrite_text_zero(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root
      integer(int8), allocatable :: bytes(:)
      integer(int8) :: expect(0:79)
      character(len=*), parameter :: hex = &
         '00000000090000000000000000000400' // &
         '00000000000000000000000000000000' // &
         '00000000000000000d0000001a000000' // &
         '00000000000000000000000000000000' // &
         '00000000000000006f6b000000000000'
      integer :: err, i
      do i = 0, 79
         expect(i) = int(hexdig(hex(2*i + 1:2*i + 1))*16 + &
                         hexdig(hex(2*i + 2:2*i + 2)), int8)
      end do
      call capnp_message_init_builder(msg, err)
      root = capnp_new_struct(msg, 0, 4, err)
      call capnp_set_root(msg, root, err)
      call capnp_set_text(root, 3, 'SECRET_PASSWORD_12345', err)
      call check(error, err == CAPNP_OK, 'overwrite: first text')
      if (allocated(error)) return
      call capnp_set_text(root, 3, 'ok', err)
      call check(error, err == CAPNP_OK, 'overwrite: second text')
      if (allocated(error)) return
      call capnp_serialize_bytes(msg, bytes, err)
      call check(error, err == CAPNP_OK .and. size(bytes) == 80, 'overwrite: 80 bytes')
      if (allocated(error)) return
      call check(error, all(bytes == expect), 'overwrite: matches C++ 1.4.0 bytes')
      if (allocated(error)) return
      call capnp_message_free(msg)
   contains
      pure function hexdig(ch) result(n)
         character(len=1), intent(in) :: ch
         integer :: n
         select case (ch)
         case ('0':'9')
            n = iachar(ch) - iachar('0')
         case ('a':'f')
            n = iachar(ch) - iachar('a') + 10
         case default
            n = 0
         end select
      end function hexdig
   end subroutine test_overwrite_text_zero
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
   !> 2^29 elements is past LIST_ELEMENT_COUNT_BITS (common.h). C++ throws
   !> in initListPointer; 2^29-1 of List(Void) still allocates.
   subroutine test_list_count_limit(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: lst
      integer :: err
      call capnp_message_init_builder(msg, err)
      call check(error, err == CAPNP_OK, 'list-count: builder')
      if (allocated(error)) return
      lst = capnp_new_list(msg, CAPNP_SZ_VOID, 536870912_int64, err)
      call check(error, err == CAPNP_ERR_ARG, 'list-count: 2^29 void rejected')
      if (allocated(error)) return
      lst = capnp_new_composite_list(msg, 536870912_int64, 1, 0, err)
      call check(error, err == CAPNP_ERR_ARG, 'list-count: 2^29 composite rejected')
      if (allocated(error)) return
      lst = capnp_new_composite_list(msg, 536870912_int64, 0, 0, err)
      call check(error, err == CAPNP_ERR_ARG, 'list-count: 2^29 empty composite rejected')
      if (allocated(error)) return
      lst = capnp_new_list(msg, CAPNP_SZ_VOID, 536870911_int64, err)
      call check(error, err == CAPNP_OK .and. capnp_list_len(lst) == 536870911_int64, &
                  'list-count: 2^29-1 void accepted')
      if (allocated(error)) return
      call capnp_message_free(msg)
   end subroutine test_list_count_limit
   !> List(Void) upgrades to a struct list of zero-size elements. Field
   !> reads are defaults. layout.c++ readListPointer / getStructElement.
   subroutine test_void_list_struct(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root, lst, el
      integer(int8), allocatable :: buf(:)
      integer :: err
      allocate (buf(0:23))
      buf = 0_int8
      call cp_put_i32(buf, 0_int64, 0_int32)
      call cp_put_i32(buf, 4_int64, 2_int32)
      call cp_put_i64(buf, 8_int64, wp_make_struct(0_int32, 0_int32, 1_int32))
      call cp_put_i64(buf, 16_int64, wp_make_list(0_int32, CAPNP_SZ_VOID, 3_int64))
      call capnp_deserialize_bytes(buf, msg, err)
      call check(error, err == CAPNP_OK, 'void-struct: framing')
      if (allocated(error)) return
      root = capnp_root(msg, err)
      lst = capnp_getp(root, 0, err)
      call check(error, err == CAPNP_OK .and. lst%kind == CAPNP_PK_LIST .and. &
                  capnp_list_len(lst) == 3_int64, 'void-struct: list')
      if (allocated(error)) return
      el = capnp_list_get_struct(lst, 0, err)
      call check(error, err == CAPNP_OK .and. el%kind == CAPNP_PK_STRUCT, 'void-struct: element')
      if (allocated(error)) return
      call check(error, capnp_get_i64(el, 0_int64) == 0_int64, 'void-struct: default field')
      if (allocated(error)) return
      el = capnp_list_get_struct(lst, 2, err)
      call check(error, err == CAPNP_OK, 'void-struct: last element')
      if (allocated(error)) return
      call capnp_message_free(msg)
   end subroutine test_void_list_struct
   !> List(Void) of 10^7 elements has no payload bytes but costs one word
   !> each (layout.c++ amplifiedRead). The default limit is 2^23 words.
   subroutine test_void_list_amplify(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root, lst
      integer(int8), allocatable :: buf(:)
      integer :: err
      allocate (buf(0:23))
      buf = 0_int8
      call cp_put_i32(buf, 0_int64, 0_int32)
      call cp_put_i32(buf, 4_int64, 2_int32)
      call cp_put_i64(buf, 8_int64, wp_make_struct(0_int32, 0_int32, 1_int32))
      call cp_put_i64(buf, 16_int64, wp_make_list(0_int32, CAPNP_SZ_VOID, 10000000_int64))
      call capnp_deserialize_bytes(buf, msg, err)
      call check(error, err == CAPNP_OK, 'void-amplify: framing')
      if (allocated(error)) return
      root = capnp_root(msg, err)
      call check(error, err == CAPNP_OK .and. root%kind == CAPNP_PK_STRUCT, 'void-amplify: root')
      if (allocated(error)) return
      lst = capnp_getp(root, 0, err)
      call check(error, err == CAPNP_ERR_TRAVERSAL, 'void-amplify: traversal')
      if (allocated(error)) return
      call check(error, lst%kind == CAPNP_PK_NULL, 'void-amplify: no list handle')
      if (allocated(error)) return
      call capnp_message_free(msg)
   end subroutine test_void_list_amplify
   !> Inline-composite list of 10^7 zero-word structs. The tag carries the
   !> count; the body is one word. layout.c++:2323 charges one word each.
   subroutine test_empty_list_amplify(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root, lst
      integer(int8), allocatable :: buf(:)
      integer(int64) :: tag
      integer :: err
      allocate (buf(0:31))
      buf = 0_int8
      call cp_put_i32(buf, 0_int64, 0_int32)
      call cp_put_i32(buf, 4_int64, 3_int32)
      call cp_put_i64(buf, 8_int64, wp_make_struct(0_int32, 0_int32, 1_int32))
      call cp_put_i64(buf, 16_int64, wp_make_list(0_int32, CAPNP_SZ_COMPOSITE, 0_int64))
      tag = shiftl(10000000_int64, 2)
      call cp_put_i64(buf, 24_int64, tag)
      call capnp_deserialize_bytes(buf, msg, err)
      call check(error, err == CAPNP_OK, 'empty-amplify: framing')
      if (allocated(error)) return
      root = capnp_root(msg, err)
      call check(error, err == CAPNP_OK .and. root%kind == CAPNP_PK_STRUCT, 'empty-amplify: root')
      if (allocated(error)) return
      lst = capnp_getp(root, 0, err)
      call check(error, err == CAPNP_ERR_TRAVERSAL, 'empty-amplify: traversal')
      if (allocated(error)) return
      call check(error, lst%kind == CAPNP_PK_NULL, 'empty-amplify: no list handle')
      if (allocated(error)) return
      call capnp_message_free(msg)
   end subroutine test_empty_list_amplify
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
   !> Tag word 0x0000000180000004 has bit 31 set and a 29-bit count of 1.
   !> layout.c++ inlineCompositeListElementCount returns 1, then [0] is 42.
   subroutine test_composite_count_unsigned(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root, lst, el
      integer(int8), allocatable :: buf(:)
      integer(int64) :: tag
      integer :: err
      allocate (buf(0:39))
      buf = 0_int8
      call cp_put_i32(buf, 0_int64, 0_int32)
      call cp_put_i32(buf, 4_int64, 4_int32)
      call cp_put_i64(buf, 8_int64, wp_make_struct(0_int32, 0_int32, 1_int32))
      call cp_put_i64(buf, 16_int64, wp_make_list(0_int32, CAPNP_SZ_COMPOSITE, 1_int64))
      tag = int(z'0000000180000004', int64)
      call cp_put_i64(buf, 24_int64, tag)
      call cp_put_i64(buf, 32_int64, 42_int64)
      call capnp_deserialize_bytes(buf, msg, err)
      call check(error, err == CAPNP_OK, 'composite-unsigned: framing')
      if (allocated(error)) return
      root = capnp_root(msg, err)
      lst = capnp_getp(root, 0, err)
      call check(error, err == CAPNP_OK .and. capnp_list_len(lst) == 1_int64, &
                  'composite-unsigned: length is 1')
      if (allocated(error)) return
      el = capnp_list_get_struct(lst, 0, err)
      call check(error, err == CAPNP_OK, 'composite-unsigned: element')
      if (allocated(error)) return
      call check(error, capnp_get_i64(el, 0_int64) == 42_int64, 'composite-unsigned: value')
      if (allocated(error)) return
      call capnp_message_free(msg)
   end subroutine test_composite_count_unsigned
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

   subroutine test_far_segment_overflow_zero(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root
      integer :: err

      call capnp_message_init_builder(msg, err)
      root = capnp_new_struct(msg, 2, 0, err)
      call capnp_set_root(msg, root, err)
      ! 2^31 - 1 overflows int32 addition `int(seg) + 1` before bounds checks.
      call cp_put_i64(msg%segs(1)%bytes, 0_int64, &
                      wp_make_far(.false., 0_int64, 2147483647_int64))
      call capnp_set_root(msg, capnp_ptr_t(), err)
      call check(error, err == CAPNP_OK .and. &
                 cp_get_i64(msg%segs(1)%bytes, 0_int64) == 0_int64, &
                 'far-segment: max int32 id is ignored while zeroing')
      if (allocated(error)) return

      call cp_put_i64(msg%segs(1)%bytes, 0_int64, &
                      wp_make_far(.false., 0_int64, 2147483648_int64))
      call capnp_set_root(msg, capnp_ptr_t(), err)
      call check(error, err == CAPNP_OK .and. &
                 cp_get_i64(msg%segs(1)%bytes, 0_int64) == 0_int64, &
                 'far-segment: bit-31 id is ignored while zeroing')
      if (allocated(error)) return

      ! Double-far with out-of-range pad word 0 still zeroes its 16-byte pad.
      call cp_put_i64(msg%segs(1)%bytes, 8_int64, &
                      wp_make_far(.false., 0_int64, 2147483647_int64))
      call cp_put_i64(msg%segs(1)%bytes, 16_int64, &
                      wp_make_struct(0_int32, 1_int32, 0_int32))
      call cp_put_i64(msg%segs(1)%bytes, 0_int64, &
                      wp_make_far(.true., 1_int64, 0_int64))
      call capnp_set_root(msg, capnp_ptr_t(), err)
      call check(error, err == CAPNP_OK .and. &
                 cp_get_i64(msg%segs(1)%bytes, 0_int64) == 0_int64 .and. &
                 cp_get_i64(msg%segs(1)%bytes, 8_int64) == 0_int64 .and. &
                 cp_get_i64(msg%segs(1)%bytes, 16_int64) == 0_int64, &
                 'far-segment: double-far out-of-range id zeroes landing pad')
      call capnp_message_free(msg)
   end subroutine test_far_segment_overflow_zero

   subroutine test_far_landing_pad_zero(error)
      use capnp_arena, only: capnp_arena_alloc_in
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root, cur, nxt
      integer :: err, i
      integer(int64) :: pad_off

      ! A chained landing pad is followed like C++ zeroObject: the body and
      ! every pad word in the chain are cleared.
      call capnp_message_init_builder(msg, err)
      root = capnp_new_struct(msg, 5, 0, err)
      call capnp_set_root(msg, root, err)
      call cp_put_i64(msg%segs(1)%bytes, 8_int64, wp_make_far(.false., 2_int64, 0_int64))
      call cp_put_i64(msg%segs(1)%bytes, 16_int64, wp_make_struct(0_int32, 1_int32, 0_int32))
      call cp_put_i64(msg%segs(1)%bytes, 24_int64, 99_int64)
      call cp_put_i64(msg%segs(1)%bytes, 0_int64, wp_make_far(.false., 1_int64, 0_int64))
      call capnp_set_root(msg, capnp_ptr_t(), err)
      call check(error, err == CAPNP_OK .and. &
                 cp_get_i64(msg%segs(1)%bytes, 0_int64) == 0_int64 .and. &
                 cp_get_i64(msg%segs(1)%bytes, 8_int64) == 0_int64 .and. &
                 cp_get_i64(msg%segs(1)%bytes, 16_int64) == 0_int64 .and. &
                 cp_get_i64(msg%segs(1)%bytes, 24_int64) == 0_int64, &
                 'far-pad-zero: far-to-far chain is cleared')
      call capnp_message_free(msg)
      if (allocated(error)) return

      ! Every hop of a 65-pad chain reaches the body.
      call capnp_message_init_builder(msg, err)
      root = capnp_new_struct(msg, 70, 0, err)
      call capnp_set_root(msg, root, err)
      do i = 1, 65
         call cp_put_i64(msg%segs(1)%bytes, int(i, int64)*8_int64, &
                         wp_make_far(.false., int(i + 1, int64), 0_int64))
      end do
      call cp_put_i64(msg%segs(1)%bytes, 66_int64*8_int64, &
                      wp_make_struct(0_int32, 1_int32, 0_int32))
      call cp_put_i64(msg%segs(1)%bytes, 67_int64*8_int64, 99_int64)
      call cp_put_i64(msg%segs(1)%bytes, 0_int64, wp_make_far(.false., 1_int64, 0_int64))
      call capnp_set_root(msg, capnp_ptr_t(), err)
      call check(error, err == CAPNP_OK .and. &
                 cp_get_i64(msg%segs(1)%bytes, 66_int64*8_int64) == 0_int64 .and. &
                 cp_get_i64(msg%segs(1)%bytes, 67_int64*8_int64) == 0_int64, &
                 'far-pad-zero: every far hop reaches and zeroes the body')
      call capnp_message_free(msg)
      if (allocated(error)) return

      ! A self-looping pad and a two-pad cycle both terminate.
      call capnp_message_init_builder(msg, err)
      root = capnp_new_struct(msg, 2, 0, err)
      call capnp_set_root(msg, root, err)
      call cp_put_i64(msg%segs(1)%bytes, 8_int64, wp_make_far(.false., 1_int64, 0_int64))
      call cp_put_i64(msg%segs(1)%bytes, 0_int64, wp_make_far(.false., 1_int64, 0_int64))
      call capnp_set_root(msg, capnp_ptr_t(), err)
      call check(error, err == CAPNP_OK .and. &
                 cp_get_i64(msg%segs(1)%bytes, 0_int64) == 0_int64 .and. &
                 cp_get_i64(msg%segs(1)%bytes, 8_int64) == 0_int64, &
                 'far-pad-zero: self-looping far pad terminates cleanly')
      call capnp_message_free(msg)
      if (allocated(error)) return

      call capnp_message_init_builder(msg, err)
      root = capnp_new_struct(msg, 3, 0, err)
      call capnp_set_root(msg, root, err)
      call cp_put_i64(msg%segs(1)%bytes, 8_int64, wp_make_far(.false., 2_int64, 0_int64))
      call cp_put_i64(msg%segs(1)%bytes, 16_int64, wp_make_far(.false., 1_int64, 0_int64))
      call cp_put_i64(msg%segs(1)%bytes, 0_int64, wp_make_far(.false., 1_int64, 0_int64))
      call capnp_set_root(msg, capnp_ptr_t(), err)
      call check(error, err == CAPNP_OK .and. &
                 cp_get_i64(msg%segs(1)%bytes, 0_int64) == 0_int64 .and. &
                 cp_get_i64(msg%segs(1)%bytes, 8_int64) == 0_int64 .and. &
                 cp_get_i64(msg%segs(1)%bytes, 16_int64) == 0_int64, &
                 'far-pad-zero: two-pad far cycle terminates cleanly')
      call capnp_message_free(msg)
      if (allocated(error)) return

      ! Pad word 0 of a double-far may carry the double-far bit, as in C++.
      call capnp_message_init_builder(msg, err)
      root = capnp_new_struct(msg, 5, 0, err)
      call capnp_set_root(msg, root, err)
      call cp_put_i64(msg%segs(1)%bytes, 8_int64, wp_make_far(.true., 3_int64, 0_int64))
      call cp_put_i64(msg%segs(1)%bytes, 16_int64, wp_make_struct(0_int32, 1_int32, 0_int32))
      call cp_put_i64(msg%segs(1)%bytes, 24_int64, 55_int64)
      call cp_put_i64(msg%segs(1)%bytes, 0_int64, wp_make_far(.true., 1_int64, 0_int64))
      call capnp_set_root(msg, capnp_ptr_t(), err)
      call check(error, err == CAPNP_OK .and. &
                 cp_get_i64(msg%segs(1)%bytes, 0_int64) == 0_int64 .and. &
                 cp_get_i64(msg%segs(1)%bytes, 8_int64) == 0_int64 .and. &
                 cp_get_i64(msg%segs(1)%bytes, 16_int64) == 0_int64 .and. &
                 cp_get_i64(msg%segs(1)%bytes, 24_int64) == 0_int64, &
                 'far-pad-zero: double-far pad word 0 ignores the double-far bit')
      call capnp_message_free(msg)
      if (allocated(error)) return

      ! Valid single-far hop at depth = CAPNP_DEFAULT_DEPTH_LIMIT - 1 preserves depth.
      call capnp_message_init_builder(msg, err)
      root = capnp_new_struct(msg, 0, 1, err)
      call capnp_set_root(msg, root, err)
      cur = root
      do i = 2, CAPNP_DEFAULT_DEPTH_LIMIT - 1
         nxt = capnp_new_struct(msg, 0, 1, err)
         call capnp_setp(cur, 0, nxt, err)
         cur = nxt
      end do
      nxt = capnp_new_struct(msg, 1, 0, err)
      call capnp_set_i64(nxt, 0_int64, 77_int64, err)
      ! Place a valid single-far landing pad pointing to the 64th struct.
      call capnp_arena_alloc_in(msg, nxt%seg, 1_int64, pad_off, err)
      call cp_put_i64(msg%segs(nxt%seg)%bytes, pad_off, &
                      wp_make_struct(int((nxt%off - (pad_off + 8_int64))/8_int64, int32), &
                                     1_int32, 0_int32))
      call cp_put_i64(msg%segs(cur%seg)%bytes, cur%off, &
                      wp_make_far(.false., pad_off/8_int64, int(nxt%seg - 1, int64)))
      call capnp_set_root(msg, capnp_ptr_t(), err)
      call check(error, err == CAPNP_OK .and. &
                 cp_get_i64(msg%segs(nxt%seg)%bytes, nxt%off) == 0_int64 .and. &
                 cp_get_i64(msg%segs(nxt%seg)%bytes, pad_off) == 0_int64, &
                 'far-pad-zero: valid single-far at max depth zeroes target')
      call capnp_message_free(msg)
   end subroutine test_far_landing_pad_zero

   subroutine test_cross_message_copy_clears_slot(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: dst, src, rsrc
      type(capnp_ptr_t) :: old_root, sroot, schild, rroot
      integer(int8), allocatable :: bytes(:)
      integer :: err

      call capnp_message_init_builder(dst, err)
      old_root = capnp_new_struct(dst, 1, 0, err)
      call capnp_set_i64(old_root, 0_int64, 42_int64, err)
      call capnp_set_root(dst, old_root, err)

      call capnp_message_init_builder(src, err)
      sroot = capnp_new_struct(src, 0, 1, err)
      schild = capnp_new_struct(src, 1, 0, err)
      call capnp_set_i64(schild, 0_int64, 99_int64, err)
      call capnp_setp(sroot, 0, schild, err)
      call capnp_set_root(src, sroot, err)
      call capnp_serialize_bytes(src, bytes, err)
      call capnp_deserialize_bytes(bytes, rsrc, err, depth_limit=1)
      rroot = capnp_root(rsrc, err)
      call check(error, err == CAPNP_OK, 'cross-copy-clear: foreign root resolves at limit 1')
      if (allocated(error)) then
         call capnp_message_free(dst)
         call capnp_message_free(src)
         call capnp_message_free(rsrc)
         return
      end if

      call capnp_set_root(dst, rroot, err)
      call check(error, err == CAPNP_ERR_DEPTH .and. &
                 cp_get_i64(dst%segs(old_root%seg)%bytes, old_root%off) == 0_int64 .and. &
                 cp_get_i64(dst%segs(1)%bytes, 0_int64) == 0_int64, &
                 'cross-copy-clear: failed cross-message copy leaves slot zeroed')
      call capnp_message_free(dst)
      call capnp_message_free(src)
      call capnp_message_free(rsrc)
   end subroutine test_cross_message_copy_clears_slot

   subroutine test_overwrite_zeroing(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root, child
      integer(int64) :: slot, far_w, pad_off
      integer :: err, pad_seg

      ! Recursive zeroing: overwriting root clears child body.
      call capnp_message_init_builder(msg, err)
      root = capnp_new_struct(msg, 1, 1, err)
      child = capnp_new_struct(msg, 1, 0, err)
      call capnp_set_i64(child, 0_int64, 123_int64, err)
      call capnp_setp(root, 0, child, err)
      call capnp_set_root(msg, root, err)
      call capnp_set_root(msg, capnp_ptr_t(), err)
      call check(error, err == CAPNP_OK .and. &
                 cp_get_i64(msg%segs(child%seg)%bytes, child%off) == 0_int64, &
                 'overwrite: recursive target is zeroed')
      if (allocated(error)) then
         call capnp_message_free(msg)
         return
      end if

      ! Same-segment self-alias guard: writing the same target retains it.
      root = capnp_new_struct(msg, 1, 1, err)
      child = capnp_new_struct(msg, 1, 0, err)
      call capnp_set_i64(child, 0_int64, 456_int64, err)
      call capnp_setp(root, 0, child, err)
      call capnp_setp(root, 0, child, err)
      call check(error, err == CAPNP_OK .and. &
                 capnp_get_i64(child, 0_int64) == 456_int64, &
                 'overwrite: self-alias retains the target')
      call capnp_message_free(msg)
      if (allocated(error)) return

      ! Cross-segment far overwrite: zeroes both remote body and landing pad.
      call capnp_message_init_builder(msg, err, first_words=2_int64)
      root = capnp_new_struct(msg, 0, 1, err)
      call capnp_set_root(msg, root, err)
      child = capnp_new_struct(msg, 2, 0, err)
      call check(error, child%seg /= root%seg, 'overwrite: child on second segment')
      if (allocated(error)) then
         call capnp_message_free(msg)
         return
      end if
      call capnp_set_i64(child, 0_int64, 789_int64, err)
      call capnp_setp(root, 0, child, err)
      slot = root%off + int(root%dwords, int64)*8_int64
      far_w = cp_get_i64(msg%segs(root%seg)%bytes, slot)
      call check(error, wp_kind(far_w) == CAPNP_WK_FAR .and. .not. wp_far_two(far_w), &
                 'overwrite: cross-segment slot encodes single far pointer')
      if (allocated(error)) then
         call capnp_message_free(msg)
         return
      end if
      pad_seg = int(wp_far_seg(far_w)) + 1
      pad_off = wp_far_off(far_w)*8_int64

      ! Cross-segment far self-alias guard retains remote target.
      call capnp_setp(root, 0, child, err)
      call check(error, err == CAPNP_OK .and. &
                 capnp_get_i64(child, 0_int64) == 789_int64, &
                 'overwrite: far self-alias retains the target')
      if (allocated(error)) then
         call capnp_message_free(msg)
         return
      end if

      ! Overwriting far slot with null zeroes landing pad and remote body.
      far_w = cp_get_i64(msg%segs(root%seg)%bytes, slot)
      pad_seg = int(wp_far_seg(far_w)) + 1
      pad_off = wp_far_off(far_w)*8_int64
      call capnp_setp(root, 0, capnp_ptr_t(), err)
      call check(error, err == CAPNP_OK .and. &
                 cp_get_i64(msg%segs(child%seg)%bytes, child%off) == 0_int64 .and. &
                 cp_get_i64(msg%segs(pad_seg)%bytes, pad_off) == 0_int64, &
                 'overwrite: far overwrite zeroes target and landing pad')
      call capnp_message_free(msg)
   end subroutine test_overwrite_zeroing
   !> A single far whose landing pad is another far is not a struct.
   !> C++ 1.4.0 layout.c++ readStructPointer rejects kind FAR there.
   subroutine test_far_chain_rejected(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root
      integer(int8), allocatable :: buf(:)
      integer(int64) :: hdr
      integer :: err
      hdr = 16_int64
      allocate (buf(0:hdr + 32_int64 - 1))
      buf = 0_int8
      call cp_put_i32(buf, 0_int64, 2_int32)
      call cp_put_i32(buf, 4_int64, 1_int32)
      call cp_put_i32(buf, 8_int64, 1_int32)
      call cp_put_i32(buf, 12_int64, 2_int32)
      call cp_put_i64(buf, hdr, wp_make_far(.false., 0_int64, 1_int64))
      call cp_put_i64(buf, hdr + 8_int64, wp_make_far(.false., 0_int64, 2_int64))
      call cp_put_i64(buf, hdr + 16_int64, wp_make_struct(0_int32, 1_int32, 0_int32))
      call cp_put_i64(buf, hdr + 24_int64, 99_int64)
      call capnp_deserialize_bytes(buf, msg, err)
      call check(error, err == CAPNP_OK, 'far-chain: framing')
      if (allocated(error)) return
      root = capnp_root(msg, err)
      call check(error, err == CAPNP_ERR_KIND, 'far-chain: landing pad far is rejected')
      if (allocated(error)) return
      call check(error, root%kind /= CAPNP_PK_STRUCT, 'far-chain: not read as struct')
      if (allocated(error)) return
      call capnp_message_free(msg)
   end subroutine test_far_chain_rejected
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
   !> Low word 0x7 is kind OTHER with reserved bits. isCapability() is false
   !> (layout.c++). A clean capability word still resolves.
   subroutine test_cap_reserved_bits(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root
      integer(int8), allocatable :: buf(:)
      integer :: err
      allocate (buf(0:15))
      buf = 0_int8
      call cp_put_i32(buf, 0_int64, 0_int32)
      call cp_put_i32(buf, 4_int64, 1_int32)
      call cp_put_i64(buf, 8_int64, 7_int64)
      call capnp_deserialize_bytes(buf, msg, err)
      call check(error, err == CAPNP_OK, 'cap-reserved: framing')
      if (allocated(error)) return
      root = capnp_root(msg, err)
      call check(error, err == CAPNP_ERR_KIND, 'cap-reserved: word 7 rejected')
      if (allocated(error)) return
      call check(error, root%kind /= CAPNP_PK_CAP, 'cap-reserved: not a capability')
      if (allocated(error)) return
      call capnp_message_free(msg)
      call cp_put_i64(buf, 8_int64, wp_make_cap(4_int64))
      call capnp_deserialize_bytes(buf, msg, err)
      root = capnp_root(msg, err)
      call check(error, err == CAPNP_OK .and. root%kind == CAPNP_PK_CAP .and. &
                  root%capidx == 4_int64, 'cap-reserved: clean cap still resolves')
      if (allocated(error)) return
      call capnp_message_free(msg)
   end subroutine test_cap_reserved_bits
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
      ! C++ 0xFF extra: word with one trailing zero stays uncompressed.
      unpacked = 0_int8
      unpacked(0:7) = int(z'61', int8)
      unpacked(8:14) = int(z'62', int8)
      unpacked(15) = 0_int8
      call capnp_pack(unpacked, packed, err)
      call check(error, err == CAPNP_OK .and. size(packed) == 18, 'packed: one-zero size')
      if (allocated(error)) return
      call check(error, packed(0) == -1_int8 .and. packed(9) == 1_int8, 'packed: one-zero N=1')
      if (allocated(error)) return
      call check(error, all(packed(1:8) == int(z'61', int8)), 'packed: one-zero word0')
      if (allocated(error)) return
      call check(error, all(packed(10:16) == int(z'62', int8)) .and. packed(17) == 0_int8, &
                  'packed: one-zero word1 raw')
      if (allocated(error)) return
      call capnp_unpack(packed, back, err)
      call check(error, err == CAPNP_OK .and. size(back) == 16 .and. all(back == unpacked), &
                  'packed: one-zero inverts')
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

   subroutine test_packed_unpack_finish(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_unpacker_t) :: unpacker
      integer(int8) :: truncated(0:2)
      integer(int8), allocatable :: out(:)
      integer(int64) :: outn
      integer :: err
      truncated = [-1_int8, 1_int8, 2_int8]
      outn = 0_int64
      call capnp_unpack_push(unpacker, truncated, out, outn, err)
      call check(error, err == CAPNP_OK .and. outn == 0_int64, 'packed-finish: partial push')
      if (allocated(error)) return
      call capnp_unpack_finish(unpacker, err)
      call check(error, err == CAPNP_ERR_PACKED, 'packed-finish: truncated input rejected')
   end subroutine test_packed_unpack_finish

   subroutine test_packed_size_limit(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg
      integer(int8), allocatable :: packed(:), flat(:)
      integer(int8) :: small_bomb(0:3), exact16(0:3), over17(0:3)
      integer :: err, i
      allocate (packed(0:402))
      packed = 0_int8
      packed(0:2) = [48_int8, -1_int8, -57_int8]
      do i = 3, 402, 2
         packed(i:i + 1) = [0_int8, -1_int8]
      end do
      call capnp_deserialize_packed_bytes(packed, msg, err, traversal_words=16_int64)
      call check(error, err == CAPNP_ERR_TRAVERSAL, 'packed-limit: expansion rejected before allocation')
      if (allocated(error)) return
      call capnp_message_free(msg)

      small_bomb = [16_int8, 100_int8, 0_int8, 99_int8]
      call capnp_deserialize_packed_bytes(small_bomb, msg, err, traversal_words=16_int64)
      call check(error, err == CAPNP_ERR_TRAVERSAL, 'packed-limit: 100-word payload rejected at limit 16')
      if (allocated(error)) return
      call capnp_message_free(msg)

      exact16 = [16_int8, 16_int8, 0_int8, 15_int8]
      call capnp_deserialize_packed_bytes(exact16, msg, err, traversal_words=16_int64)
      call check(error, err == CAPNP_OK, 'packed-limit: exact limit accepted')
      if (allocated(error)) return
      call capnp_message_free(msg)

      over17 = [16_int8, 17_int8, 0_int8, 16_int8]
      call capnp_deserialize_packed_bytes(over17, msg, err, traversal_words=16_int64)
      call check(error, err == CAPNP_ERR_TRAVERSAL, 'packed-limit: limit+1 rejected')
      if (allocated(error)) return
      call capnp_message_free(msg)

      call capnp_deserialize_packed_bytes(exact16, msg, err, traversal_words=huge(0_int64))
      call check(error, err == CAPNP_OK, 'packed-limit: huge traversal limit accepted')
      if (allocated(error)) return
      call capnp_message_free(msg)

      call capnp_unpack(exact16, flat, err, max_words=huge(0_int64))
      call check(error, err == CAPNP_OK .and. size(flat) == 136, 'packed-limit: huge unpack max_words accepted')
   end subroutine test_packed_size_limit

   !> A run that starts in the first word or the segment table must end
   !> there: C++ PackedInputStream bounds each read by its own end, so a
   !> run that leaks into the next region is rejected even when the total
   !> length would still add up.
   subroutine test_packed_header_boundary(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg
      integer(int8) :: zero_into_body(0:4), raw_into_body(0:27), raw_into_table(0:28)
      integer(int8) :: cases(0:28, 3)
      integer :: lens(3)
      integer :: err, unit, ios, k
      character(len=*), parameter :: path = 'packed-header-boundary.bin'
      zero_into_body = [17_int8, 1_int8, 1_int8, 0_int8, 1_int8]
      raw_into_body = [17_int8, 1_int8, 1_int8, -1_int8, 1_int8, 0_int8, 0_int8, 0_int8, 0_int8, 0_int8, 0_int8, 0_int8, &
                       1_int8, 1_int8, 2_int8, 3_int8, 4_int8, 5_int8, 6_int8, 7_int8, 8_int8, 0_int8, 0_int8, 0_int8, &
                       0_int8, 0_int8, 0_int8, 0_int8]
      raw_into_table = [-1_int8, 1_int8, 0_int8, 0_int8, 0_int8, 1_int8, 0_int8, 0_int8, 0_int8, 1_int8, &
                        1_int8, 0_int8, 0_int8, 0_int8, 0_int8, 0_int8, 0_int8, 0_int8, 0_int8, 1_int8, &
                        0_int8, 0_int8, 0_int8, 0_int8, 0_int8, 0_int8, 0_int8, 0_int8, 0_int8]
      cases = 0_int8
      cases(0:4, 1) = zero_into_body
      cases(0:27, 2) = raw_into_body
      cases(0:28, 3) = raw_into_table
      lens = [5, 23, 20]
      do k = 1, 3
         call capnp_deserialize_packed_bytes(cases(0:lens(k) - 1, k), msg, err)
         call check(error, err == CAPNP_ERR_PACKED, 'packed-header: bytes run crosses a region')
         if (allocated(error)) return
         call capnp_message_free(msg)

         open (newunit=unit, file=path, access='stream', form='unformatted', &
               status='replace', action='readwrite', iostat=ios)
         if (ios /= 0) then
            call check(error, .false., 'packed-header: fixture open')
            return
         end if
         write (unit) cases(0:lens(k) - 1, k)
         rewind (unit)
         call capnp_read_message_packed_unit(unit, msg, err)
         close (unit, status='delete')
         call check(error, err == CAPNP_ERR_PACKED, 'packed-header: unit run crosses a region')
         if (allocated(error)) return
         call capnp_message_free(msg)
      end do
   end subroutine test_packed_header_boundary

   subroutine test_packed_segment_boundary(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg
      integer(int8) :: packed(0:3), raw_overrun(0:27)
      integer :: err, unit, ios
      character(len=*), parameter :: path = 'packed-segment-boundary.bin'
      packed = [16_int8, 1_int8, 0_int8, 5_int8]
      open (newunit=unit, file=path, access='stream', form='unformatted', &
            status='replace', action='readwrite', iostat=ios)
      if (ios /= 0) then
         call check(error, .false., 'packed-boundary: fixture open')
         return
      end if
      write (unit) packed
      rewind (unit)
      call capnp_read_message_packed_unit(unit, msg, err)
      close (unit, status='delete')
      call check(error, err == CAPNP_ERR_PACKED, 'packed-boundary: unit zero run exceeds segment')
      if (allocated(error)) return
      call capnp_message_free(msg)

      call capnp_deserialize_packed_bytes(packed, msg, err)
      call check(error, err == CAPNP_ERR_PACKED, 'packed-boundary: bytes zero run exceeds segment')
      if (allocated(error)) return
      call capnp_message_free(msg)

      raw_overrun = [16_int8, 1_int8, -1_int8, 1_int8, 2_int8, 3_int8, 4_int8, 5_int8, 6_int8, 7_int8, 8_int8, 2_int8, &
                     1_int8, 1_int8, 1_int8, 1_int8, 1_int8, 1_int8, 1_int8, 1_int8, &
                     2_int8, 2_int8, 2_int8, 2_int8, 2_int8, 2_int8, 2_int8, 2_int8]
      call capnp_deserialize_packed_bytes(raw_overrun, msg, err)
      call check(error, err == CAPNP_ERR_PACKED, 'packed-boundary: bytes raw run exceeds segment')
      call capnp_message_free(msg)
   end subroutine test_packed_segment_boundary

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

   subroutine test_framing_many_segments(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg
      integer(int8), allocatable :: frame(:)
      integer :: err, i
      allocate (frame(0:6159))
      frame = 0_int8
      frame(1) = 2_int8
      do i = 1, 513
         frame(4*i) = 1_int8
      end do
      call capnp_deserialize_bytes(frame, msg, err)
      call check(error, err == CAPNP_OK .and. msg%nsegs == 513, 'framing: 513 segments accepted')
      call capnp_message_free(msg)
   end subroutine test_framing_many_segments

   !> The file reader applies the stream reader limits (C++
   !> InputStreamMessageReader): `firstWord[0] < 511` so 511 segments pass
   !> and 512 do not, and the total size stays within the traversal limit.
   !> Both checks run before any body is read. Flat arrays have no such cap.
   subroutine test_framing_file_limits(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg
      integer(int8), allocatable :: frame(:)
      integer(int8) :: packed511(0:4), packed512(0:4)
      integer :: err, i, unit
      character(len=*), parameter :: path = 'framing-file-limits.bin'
      allocate (frame(0:6159))
      frame = 0_int8
      frame(1) = 2_int8
      do i = 1, 513
         frame(4*i) = 1_int8
      end do
      call capnp_write_file(path, frame, err)
      call capnp_read_message(path, msg, err)
      call check(error, err == CAPNP_ERR_FRAMING, 'file-limits: 513 segments rejected')
      if (allocated(error)) return
      call capnp_message_free(msg)
      deallocate (frame)

      call zero_sized_segments(frame, 511)
      call capnp_deserialize_bytes(frame, msg, err)
      call check(error, err == CAPNP_OK .and. msg%nsegs == 511, &
                 'file-limits: flat 511 segments accepted')
      if (allocated(error)) return
      call capnp_message_free(msg)
      call capnp_write_file(path, frame, err)
      call capnp_read_message(path, msg, err)
      call check(error, err == CAPNP_OK .and. msg%nsegs == 511, &
                 'file-limits: 511 segments accepted')
      if (allocated(error)) return
      call capnp_message_free(msg)
      open (newunit=unit, file=path, access='stream', form='unformatted', status='old')
      call capnp_read_message_unit(unit, msg, err)
      close (unit)
      call check(error, err == CAPNP_OK .and. msg%nsegs == 511, &
                 'file-limits: unit 511 segments accepted')
      if (allocated(error)) return
      call capnp_message_free(msg)

      call zero_sized_segments(frame, 512)
      call capnp_deserialize_bytes(frame, msg, err)
      call check(error, err == CAPNP_OK .and. msg%nsegs == 512, &
                 'file-limits: flat 512 segments accepted')
      if (allocated(error)) return
      call capnp_message_free(msg)
      call capnp_write_file(path, frame, err)
      call capnp_read_message(path, msg, err)
      call check(error, err == CAPNP_ERR_FRAMING, 'file-limits: 512 segments rejected')
      if (allocated(error)) return
      call capnp_message_free(msg)
      open (newunit=unit, file=path, access='stream', form='unformatted', status='old')
      call capnp_read_message_unit(unit, msg, err)
      close (unit)
      call check(error, err == CAPNP_ERR_FRAMING, 'file-limits: unit 512 segments rejected')
      if (allocated(error)) return
      ! 511 zero-size segments pack as one literal word plus 255 zero words.
      packed511 = [3_int8, -2_int8, 1_int8, 0_int8, -2_int8]
      call capnp_deserialize_packed_bytes(packed511, msg, err)
      call check(error, err == CAPNP_OK .and. msg%nsegs == 511, &
                 'file-limits: packed 511 segments accepted')
      if (allocated(error)) return
      call capnp_message_free(msg)
      call capnp_write_file(path, packed511, err)
      open (newunit=unit, file=path, access='stream', form='unformatted', status='old')
      call capnp_read_message_packed_unit(unit, msg, err)
      close (unit)
      call check(error, err == CAPNP_OK .and. msg%nsegs == 511, &
                 'file-limits: packed unit 511 segments accepted')
      if (allocated(error)) return
      call capnp_message_free(msg)
      ! 512 zero-size segments: literal word plus 256 zero words.
      packed512 = [3_int8, -1_int8, 1_int8, 0_int8, -1_int8]
      call capnp_deserialize_packed_bytes(packed512, msg, err)
      call check(error, err == CAPNP_ERR_FRAMING, 'file-limits: packed 512 segments rejected')
      if (allocated(error)) return
      call capnp_message_free(msg)
      call capnp_write_file(path, packed512, err)
      open (newunit=unit, file=path, access='stream', form='unformatted', status='old')
      call capnp_read_message_packed_unit(unit, msg, err)
      close (unit)
      call check(error, err == CAPNP_ERR_FRAMING, 'file-limits: packed unit 512 segments rejected')
      if (allocated(error)) return
      call capnp_message_free(msg)

      deallocate (frame)
      allocate (frame(0:7))
      frame = 0_int8
      frame(4) = 100_int8
      call capnp_write_file(path, frame, err)
      call capnp_read_message(path, msg, err, traversal_words=10_int64)
      call check(error, err == CAPNP_ERR_TRAVERSAL, 'file-limits: size above traversal limit')
      call capnp_message_free(msg)
      open (newunit=i, file=path, status='old')
      close (i, status='delete')
   end subroutine test_framing_file_limits

   !> Header for `nsegs` zero-sized segments and no body.
   subroutine zero_sized_segments(frame, nsegs)
      integer(int8), allocatable, intent(out) :: frame(:)
      integer, intent(in) :: nsegs
      integer(int64) :: nbytes
      nbytes = (int(1 + nsegs, int64)*4_int64 + 7_int64)/8_int64*8_int64
      if (allocated(frame)) deallocate (frame)
      allocate (frame(0:nbytes - 1))
      frame = 0_int8
      call cp_put_i32(frame, 0_int64, int(nsegs - 1, int32))
   end subroutine zero_sized_segments

   subroutine test_read_empty_file(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int8), allocatable :: bytes(:)
      type(capnp_message_t), target :: msg
      integer :: err, efile, unit, ios
      logical :: canonical
      character(len=*), parameter :: path = 'empty-file.bin'
      open (newunit=unit, file=path, access='stream', form='unformatted', &
            status='replace', action='write', iostat=ios)
      if (ios /= 0) then
         call check(error, .false., 'empty-file: fixture open')
         return
      end if
      close (unit)
      call capnp_read_file(path, bytes, efile)
      call capnp_read_message(path, msg, err)
      call check(error, err == CAPNP_ERR_FRAMING, 'empty-file: stream reader rejects empty input')
      call capnp_message_free(msg)
      if (allocated(error)) return
      open (newunit=unit, file=path, status='old', iostat=ios)
      if (ios == 0) close (unit, status='delete')
      call check(error, efile == CAPNP_OK .and. size(bytes) == 0, 'empty-file: zero bytes returned')
      if (allocated(error)) return
      call capnp_deserialize_bytes(bytes, msg, err)
      call check(error, err == CAPNP_OK .and. msg%nsegs == 0, &
                 'empty-file: flat empty bytes are a message')
      if (allocated(error)) return
      canonical = capnp_is_canonical(msg, err)
      call check(error, err == CAPNP_OK .and. .not. canonical, &
                 'empty-file: flat empty message is non-canonical')
      call capnp_message_free(msg)
      if (allocated(error)) return
      call capnp_deserialize_view(bytes, msg, err)
      call check(error, err == CAPNP_OK .and. msg%nsegs == 0, &
                 'empty-file: flat empty view is a message')
      call capnp_message_free(msg)
   end subroutine test_read_empty_file

   subroutine test_stream_reader_options(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg, rmsg
      type(capnp_ptr_t) :: root, child, q
      integer(int8), allocatable :: bytes(:), packed(:)
      integer :: err, unit, ios
      character(len=*), parameter :: plain_path = 'stream-options.bin'
      character(len=*), parameter :: packed_path = 'stream-options-packed.bin'
      call capnp_message_init_builder(msg, err)
      root = capnp_new_struct(msg, 0, 1, err)
      child = capnp_new_struct(msg, 1, 0, err)
      call capnp_setp(root, 0, child, err)
      call capnp_set_root(msg, root, err)
      call capnp_serialize_bytes(msg, bytes, err)
      call capnp_serialize_packed_bytes(msg, packed, err)
      call capnp_write_file(plain_path, bytes, err)
      call capnp_write_file(packed_path, packed, err)

      call capnp_read_message(plain_path, rmsg, err, traversal_words=0_int64)
      call check(error, err == CAPNP_ERR_TRAVERSAL, 'stream-options: file traversal limit')
      if (allocated(error)) return
      call capnp_message_free(rmsg)

      call capnp_read_message_packed(packed_path, rmsg, err, traversal_words=0_int64)
      call check(error, err == CAPNP_ERR_TRAVERSAL, 'stream-options: packed file limit')
      if (allocated(error)) return
      call capnp_message_free(rmsg)

      call capnp_read_message_packed(packed_path, rmsg, err, depth_limit=0)
      call check(error, err == CAPNP_OK, 'stream-options: packed file read depth_limit=0')
      if (allocated(error)) return
      q = capnp_root(rmsg, err)
      call check(error, err == CAPNP_ERR_DEPTH, 'stream-options: packed file root depth limit')
      if (allocated(error)) return
      call capnp_message_free(rmsg)

      open (newunit=unit, file=plain_path, access='stream', form='unformatted', &
            status='old', action='read', iostat=ios)
      call capnp_read_message_unit(unit, rmsg, err, traversal_words=0_int64)
      close (unit)
      call check(error, err == CAPNP_ERR_TRAVERSAL, 'stream-options: unit traversal limit')
      if (allocated(error)) return
      call capnp_message_free(rmsg)

      open (newunit=unit, file=packed_path, access='stream', form='unformatted', &
            status='old', action='read', iostat=ios)
      call capnp_read_message_packed_unit(unit, rmsg, err, traversal_words=0_int64)
      close (unit, status='delete')
      open (newunit=unit, file=plain_path, status='old', iostat=ios)
      if (ios == 0) close (unit, status='delete')
      call check(error, err == CAPNP_ERR_TRAVERSAL, 'stream-options: packed unit limit')
      call capnp_message_free(msg)
      call capnp_message_free(rmsg)
   end subroutine test_stream_reader_options

   !> Lists of zero-sized elements cost nothing on the wire but are billed
   !> one word per element (C++ `amplifiedRead`), and a composite tag's
   !> element count is 29 bits wide.
   subroutine test_amplified_lists(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64), parameter :: one = 1_int64
      integer(int64) :: tag_count
      integer :: err

      ! Composite list of 5000 zero-sized structs, then a void list of 5000.
      call probe(ishft(5000_int64, 2), 0_int64, 7, 0_int64, 100000_int64, err)
      call check(error, err == CAPNP_OK, 'amplified: zero-size composite fits a large budget')
      if (allocated(error)) return
      call probe(ishft(5000_int64, 2), 0_int64, 7, 0_int64, 1000_int64, err)
      call check(error, err == CAPNP_ERR_TRAVERSAL, 'amplified: zero-size composite is billed')
      if (allocated(error)) return
      call probe(0_int64, 5000_int64, 0, 0_int64, 100000_int64, err)
      call check(error, err == CAPNP_OK, 'amplified: void list fits a large budget')
      if (allocated(error)) return
      call probe(0_int64, 5000_int64, 0, 0_int64, 1000_int64, err)
      call check(error, err == CAPNP_ERR_TRAVERSAL, 'amplified: void list is billed')
      if (allocated(error)) return

      ! Tag count 2**30 - 1 has bit 29 set; the 29-bit count is 2**29 - 1,
      ! which cannot fit one content word of one-word elements.
      tag_count = ishft(ishft(one, 30) - one, 2) + ishft(one, 32)
      call probe(tag_count, 0_int64, 7, 1_int64, 100000_int64, err)
      call check(error, err == CAPNP_ERR_BOUNDS, 'amplified: composite count wider than content')
   contains
      subroutine probe(tag_low, count, esize, content_words, limit, rc)
         integer(int64), intent(in) :: tag_low, count, content_words, limit
         integer, intent(in) :: esize
         integer, intent(out) :: rc
         type(capnp_message_t), target :: m
         type(capnp_ptr_t) :: r, c
         integer(int8) :: frame(0:63)
         integer(int64) :: words
         frame = 0_int8
         words = 3_int64 + content_words
         if (esize /= 7) words = 2_int64
         call cp_put_i32(frame, 4_int64, int(words, int32))
         call cp_put_i64(frame, 8_int64, ishft(1_int64, 48))
         if (esize == 7) then
            call cp_put_i64(frame, 16_int64, 1_int64 + ishft(7_int64, 32) + ishft(content_words, 35))
            call cp_put_i64(frame, 24_int64, tag_low)
         else
            call cp_put_i64(frame, 16_int64, 1_int64 + ishft(int(esize, int64), 32) + ishft(count, 35))
         end if
         call capnp_deserialize_bytes(frame(0:8 + words*8 - 1), m, rc, traversal_words=limit)
         if (rc /= CAPNP_OK) return
         r = capnp_root(m, rc)
         if (rc /= CAPNP_OK) return
         c = capnp_getp(r, 0, rc)
         call capnp_message_free(m)
      end subroutine probe
   end subroutine test_amplified_lists

   !> A pointer whose offset leaves the segment targets the segment's end
   !> (C++ `SegmentReader::checkOffset`), so an object of size zero reads
   !> fine there and anything larger is out of bounds.
   subroutine test_zero_size_target_clamp(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: m
      type(capnp_ptr_t) :: r, c
      integer(int8) :: frame(0:31)
      integer :: err
      frame = 0_int8
      call cp_put_i32(frame, 4_int64, 3_int32)
      call cp_put_i64(frame, 8_int64, ishft(2_int64, 48))
      call cp_put_i64(frame, 16_int64, wp_make_struct(500_int32, 0_int32, 0_int32))
      call capnp_deserialize_bytes(frame, m, err)
      r = capnp_root(m, err)
      c = capnp_getp(r, 0, err)
      call check(error, err == CAPNP_OK .and. c%kind == CAPNP_PK_STRUCT, 'clamp: empty struct far away')
      if (allocated(error)) return
      c = capnp_getp(r, 1, err)
      call check(error, err == CAPNP_OK, 'clamp: null second pointer')
      if (allocated(error)) return
      call capnp_message_free(m)

      call cp_put_i64(frame, 16_int64, wp_make_list(500_int32, 2_int32, 0_int64))
      call capnp_deserialize_bytes(frame, m, err)
      r = capnp_root(m, err)
      c = capnp_getp(r, 0, err)
      call check(error, err == CAPNP_OK .and. c%kind == CAPNP_PK_LIST .and. c%nelem == 0_int64, 'clamp: empty list far away')
      if (allocated(error)) return
      call capnp_message_free(m)

      call cp_put_i64(frame, 16_int64, wp_make_list(500_int32, 2_int32, 1_int64))
      call capnp_deserialize_bytes(frame, m, err)
      r = capnp_root(m, err)
      c = capnp_getp(r, 0, err)
      call check(error, err == CAPNP_ERR_BOUNDS, 'clamp: one byte far away is out of bounds')
      call capnp_message_free(m)
   end subroutine test_zero_size_target_clamp

   !> Far-pointer resolution matches C++ `WireHelpers::followFars`:
   !> zero landing pad is an empty struct, far hops do not consume depth,
   !> chained single-far pointers fail, double-far out-of-segment targets
   !> clamp for zero-sized objects, and `capnp_root` bills the root word.
   subroutine test_far_pointer_parity(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: m
      type(capnp_ptr_t) :: r, c
      integer(int8) :: frame(0:47), small(0:15)
      integer :: err

      ! Single-far to zero landing pad decodes as empty struct (0, 0), not null.
      frame = 0_int8
      call cp_put_i32(frame, 4_int64, 3_int32)
      call cp_put_i64(frame, 8_int64, wp_make_struct(0_int32, 0_int32, 1_int32))
      call cp_put_i64(frame, 16_int64, wp_make_far(.false., 2_int64, 0_int64))
      call cp_put_i64(frame, 24_int64, 0_int64)
      call capnp_deserialize_bytes(frame(0:31), m, err)
      r = capnp_root(m, err)
      c = capnp_getp(r, 0, err)
      call check(error, err == CAPNP_OK .and. c%kind == CAPNP_PK_STRUCT .and. &
                 c%dwords == 0 .and. c%pwords == 0, 'far-parity: zero pad is empty struct')
      call capnp_message_free(m)
      if (allocated(error)) return

      ! A far landing pad does not add a level, but the child object does.
      frame = 0_int8
      call cp_put_i32(frame, 4_int64, 4_int32)
      call cp_put_i64(frame, 8_int64, wp_make_struct(0_int32, 0_int32, 1_int32))
      call cp_put_i64(frame, 16_int64, wp_make_far(.false., 2_int64, 0_int64))
      call cp_put_i64(frame, 24_int64, wp_make_struct(0_int32, 1_int32, 0_int32))
      call cp_put_i64(frame, 32_int64, 7_int64)
      call capnp_deserialize_bytes(frame(0:39), m, err, depth_limit=1)
      r = capnp_root(m, err)
      c = capnp_getp(r, 0, err)
      call check(error, err == CAPNP_ERR_DEPTH, 'far-parity: child consumes depth after far')
      call capnp_message_free(m)
      if (allocated(error)) return
      call capnp_deserialize_bytes(frame(0:39), m, err, depth_limit=2)
      r = capnp_root(m, err)
      c = capnp_getp(r, 0, err)
      call check(error, err == CAPNP_OK .and. c%kind == CAPNP_PK_STRUCT .and. &
                 capnp_get_i64(c, 0_int64) == 7_int64, 'far-parity: far hop does not charge depth')
      call capnp_message_free(m)
      if (allocated(error)) return

      ! Chained single-far pointer (far -> far -> struct) is rejected.
      frame = 0_int8
      call cp_put_i32(frame, 4_int64, 5_int32)
      call cp_put_i64(frame, 8_int64, wp_make_struct(0_int32, 0_int32, 1_int32))
      call cp_put_i64(frame, 16_int64, wp_make_far(.false., 2_int64, 0_int64))
      call cp_put_i64(frame, 24_int64, wp_make_far(.false., 3_int64, 0_int64))
      call cp_put_i64(frame, 32_int64, wp_make_struct(0_int32, 1_int32, 0_int32))
      call cp_put_i64(frame, 40_int64, 9_int64)
      call capnp_deserialize_bytes(frame, m, err)
      r = capnp_root(m, err)
      c = capnp_getp(r, 0, err)
      call check(error, err == CAPNP_ERR_KIND, 'far-parity: chained single-far rejected')
      call capnp_message_free(m)
      if (allocated(error)) return

      ! Double-far with out-of-segment offset clamps to segment end for size 0.
      frame = 0_int8
      call cp_put_i32(frame, 4_int64, 4_int32)
      call cp_put_i64(frame, 8_int64, wp_make_struct(0_int32, 0_int32, 1_int32))
      call cp_put_i64(frame, 16_int64, wp_make_far(.true., 2_int64, 0_int64))
      call cp_put_i64(frame, 24_int64, wp_make_far(.false., 500_int64, 0_int64))
      call cp_put_i64(frame, 32_int64, wp_make_struct(0_int32, 0_int32, 0_int32))
      call capnp_deserialize_bytes(frame(0:39), m, err)
      r = capnp_root(m, err)
      c = capnp_getp(r, 0, err)
      call check(error, err == CAPNP_OK .and. c%kind == CAPNP_PK_STRUCT, &
                 'far-parity: double-far clamps empty target')
      call capnp_message_free(m)
      if (allocated(error)) return

      ! Double-far pad word 0 matches C++ followFars: double-far bit is ignored
      ! and segment bounds are checked before kind.
      frame = 0_int8
      call cp_put_i32(frame, 4_int64, 5_int32)
      call cp_put_i64(frame, 8_int64, wp_make_struct(0_int32, 0_int32, 1_int32))
      call cp_put_i64(frame, 16_int64, wp_make_far(.true., 2_int64, 0_int64))
      call cp_put_i64(frame, 24_int64, wp_make_far(.true., 4_int64, 0_int64))
      call cp_put_i64(frame, 32_int64, wp_make_struct(0_int32, 1_int32, 0_int32))
      call cp_put_i64(frame, 40_int64, 4660_int64)
      call capnp_deserialize_bytes(frame, m, err)
      r = capnp_root(m, err)
      c = capnp_getp(r, 0, err)
      call check(error, err == CAPNP_OK .and. c%kind == CAPNP_PK_STRUCT .and. &
                 capnp_get_i64(c, 0_int64) == 4660_int64, &
                 'far-parity: double-far pad ignores double-far bit')
      call capnp_message_free(m)
      if (allocated(error)) return

      frame = 0_int8
      call cp_put_i32(frame, 4_int64, 4_int32)
      call cp_put_i64(frame, 8_int64, wp_make_struct(0_int32, 0_int32, 1_int32))
      call cp_put_i64(frame, 16_int64, wp_make_far(.true., 2_int64, 0_int64))
      call cp_put_i64(frame, 24_int64, wp_make_struct(0_int32, 5_int32, 0_int32))
      call cp_put_i64(frame, 32_int64, wp_make_struct(0_int32, 0_int32, 0_int32))
      call capnp_deserialize_bytes(frame(0:39), m, err)
      r = capnp_root(m, err)
      c = capnp_getp(r, 0, err)
      call check(error, err == CAPNP_ERR_SEGMENT, &
                 'far-parity: double-far pad checks segment before kind')
      call capnp_message_free(m)
      if (allocated(error)) return

      ! Root pointer word itself is billed to traversal_words.
      small = 0_int8
      call cp_put_i32(small, 4_int64, 1_int32)
      call capnp_deserialize_bytes(small, m, err, traversal_words=0_int64)
      r = capnp_root(m, err)
      call check(error, err == CAPNP_ERR_TRAVERSAL, 'far-parity: root pointer billed')
      call capnp_message_free(m)
   end subroutine test_far_pointer_parity

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
   !> An 8-word struct costs 2 root words plus 8 body words. Limit 9 fails
   !> and limit 10 reads it (message.c++:100, layout.c++:2707).
   subroutine test_traversal_root_words(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root
      integer(int8), allocatable :: buf(:)
      integer :: err
      allocate (buf(0:79))
      buf = 0_int8
      call cp_put_i32(buf, 0_int64, 0_int32)
      call cp_put_i32(buf, 4_int64, 9_int32)
      call cp_put_i64(buf, 8_int64, wp_make_struct(0_int32, 8_int32, 0_int32))
      call capnp_deserialize_bytes(buf, msg, err, traversal_words=9_int64)
      call check(error, err == CAPNP_OK, 'trav-root: framing')
      if (allocated(error)) return
      root = capnp_root(msg, err)
      call check(error, err == CAPNP_ERR_TRAVERSAL, 'trav-root: limit 9 rejects')
      if (allocated(error)) return
      call capnp_message_free(msg)
      call capnp_deserialize_bytes(buf, msg, err, traversal_words=10_int64)
      root = capnp_root(msg, err)
      call check(error, err == CAPNP_OK .and. root%kind == CAPNP_PK_STRUCT, &
                  'trav-root: limit 10 reads the struct')
      if (allocated(error)) return
      call capnp_message_free(msg)
   end subroutine test_traversal_root_words
   !> Single-far 1-word struct costs 2 root + 1 pad + 1 body. Limit 3 fails,
   !> limit 4 yields 55 (layout.c++:586).
   subroutine test_traversal_single_far(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root
      integer(int8), allocatable :: buf(:)
      integer :: err
      allocate (buf(0:39))
      buf = 0_int8
      call cp_put_i32(buf, 0_int64, 1_int32)
      call cp_put_i32(buf, 4_int64, 1_int32)
      call cp_put_i32(buf, 8_int64, 2_int32)
      call cp_put_i64(buf, 16_int64, wp_make_far(.false., 0_int64, 1_int64))
      call cp_put_i64(buf, 24_int64, wp_make_struct(0_int32, 1_int32, 0_int32))
      call cp_put_i64(buf, 32_int64, 55_int64)
      call capnp_deserialize_bytes(buf, msg, err, traversal_words=3_int64)
      root = capnp_root(msg, err)
      call check(error, err == CAPNP_ERR_TRAVERSAL, 'trav-sfar: limit 3 rejects')
      if (allocated(error)) return
      call capnp_message_free(msg)
      call capnp_deserialize_bytes(buf, msg, err, traversal_words=4_int64)
      root = capnp_root(msg, err)
      call check(error, err == CAPNP_OK .and. capnp_get_i64(root, 0_int64) == 55_int64, &
                  'trav-sfar: limit 4 reads 55')
      if (allocated(error)) return
      call capnp_message_free(msg)
   end subroutine test_traversal_single_far
   !> Double-far 1-word struct costs 2 root + 2 pad + 1 body. Limit 4 fails,
   !> limit 5 yields 123 (layout.c++:586).
   subroutine test_traversal_double_far(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root
      integer(int8), allocatable :: buf(:)
      integer :: err
      allocate (buf(0:47))
      buf = 0_int8
      call cp_put_i32(buf, 0_int64, 2_int32)
      call cp_put_i32(buf, 4_int64, 1_int32)
      call cp_put_i32(buf, 8_int64, 2_int32)
      call cp_put_i32(buf, 12_int64, 1_int32)
      call cp_put_i64(buf, 16_int64, wp_make_far(.true., 0_int64, 1_int64))
      call cp_put_i64(buf, 24_int64, wp_make_far(.false., 0_int64, 2_int64))
      call cp_put_i64(buf, 32_int64, wp_make_struct(0_int32, 1_int32, 0_int32))
      call cp_put_i64(buf, 40_int64, 123_int64)
      call capnp_deserialize_bytes(buf, msg, err, traversal_words=4_int64)
      root = capnp_root(msg, err)
      call check(error, err == CAPNP_ERR_TRAVERSAL, 'trav-dfar: limit 4 rejects')
      if (allocated(error)) return
      call capnp_message_free(msg)
      call capnp_deserialize_bytes(buf, msg, err, traversal_words=5_int64)
      root = capnp_root(msg, err)
      call check(error, err == CAPNP_OK .and. capnp_get_i64(root, 0_int64) == 123_int64, &
                  'trav-dfar: limit 5 reads 123')
      if (allocated(error)) return
      call capnp_message_free(msg)
   end subroutine test_traversal_double_far
   !> nestingLimit must be greater than zero to read the root
   !> (layout.c++:2186). Limit 1 allows the root and rejects the first getp.
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
      call capnp_deserialize_bytes(bytes, rmsg, err, depth_limit=1)
      root = capnp_root(rmsg, err)
      call check(error, err == CAPNP_OK .and. root%kind == CAPNP_PK_STRUCT, &
                  'guards: root within limit 1')
      if (allocated(error)) return
      q = capnp_getp(root, 0, err)
      call check(error, err == CAPNP_ERR_DEPTH, 'guards: depth limit on getp')
      if (allocated(error)) return
      call capnp_message_free(msg)
      call capnp_message_free(rmsg)
   end subroutine test_depth_limit
   !> With depth_limit=2 the first nested struct is readable and the next
   !> getp hits the limit. The root itself consumes one level.
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
      call capnp_deserialize_bytes(bytes, rmsg, err, depth_limit=2)
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
   !> Default nesting limit is 64. A chain of 64 structs is readable; the
   !> 65th struct is rejected (layout.c++:2186).
   subroutine test_nest_limit_64(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg, rmsg
      type(capnp_ptr_t) :: node, prev, p
      integer(int8), allocatable :: bytes(:)
      integer :: err, i, n, last, hops
      do n = 64, 65
         call capnp_message_init_builder(msg, err)
         prev = capnp_ptr_t()
         do i = n, 1, -1
            node = capnp_new_struct(msg, 1, 1, err)
            call capnp_set_i32(node, 0_int64, int(i, int32), err)
            if (i /= n) call capnp_setp(node, 0, prev, err)
            prev = node
         end do
         call capnp_set_root(msg, prev, err)
         call capnp_serialize_bytes(msg, bytes, err)
         call capnp_deserialize_bytes(bytes, rmsg, err, depth_limit=64)
         p = capnp_root(rmsg, err)
         last = 0
         hops = 0
         do
            if (err /= CAPNP_OK .or. p%kind /= CAPNP_PK_STRUCT) exit
            last = int(capnp_get_i32(p, 0_int64))
            hops = hops + 1
            p = capnp_getp(p, 0, err)
         end do
         if (n == 64) then
            call check(error, err == CAPNP_OK .and. last == 64 .and. hops == 64, &
                        'nest-64: chain of 64 is readable')
         else
            call check(error, err == CAPNP_ERR_DEPTH .and. last == 64, &
                        'nest-65: 65th struct is past the limit')
         end if
         if (allocated(error)) return
         call capnp_message_free(msg)
         call capnp_message_free(rmsg)
      end do
   end subroutine test_nest_limit_64
   !> A far landing pad does not consume a nesting level. With depth_limit=1
   !> the far root is depth 0 and its data is readable (layout.c++ followFars).
   subroutine test_far_depth_one(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg
      type(capnp_ptr_t) :: root
      integer(int8), allocatable :: buf(:)
      integer :: err
      allocate (buf(0:39))
      buf = 0_int8
      call cp_put_i32(buf, 0_int64, 1_int32)
      call cp_put_i32(buf, 4_int64, 1_int32)
      call cp_put_i32(buf, 8_int64, 2_int32)
      call cp_put_i64(buf, 16_int64, wp_make_far(.false., 0_int64, 1_int64))
      call cp_put_i64(buf, 24_int64, wp_make_struct(0_int32, 1_int32, 0_int32))
      call cp_put_i64(buf, 32_int64, 77_int64)
      call capnp_deserialize_bytes(buf, msg, err, depth_limit=0)
      root = capnp_root(msg, err)
      call check(error, err == CAPNP_ERR_DEPTH, 'far-depth: limit 0 rejects the root')
      if (allocated(error)) return
      call capnp_message_free(msg)
      call capnp_deserialize_bytes(buf, msg, err, depth_limit=1)
      root = capnp_root(msg, err)
      call check(error, err == CAPNP_OK .and. root%depth == 0 .and. &
                  capnp_get_i64(root, 0_int64) == 77_int64, &
                  'far-depth: limit 1 reads val 77 at depth 0')
      if (allocated(error)) return
      call capnp_message_free(msg)
   end subroutine test_far_depth_one

end module test_wire
