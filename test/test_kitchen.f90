!> Kitchen-sink generated-code test: cross-file imports, scalar and
!> pointer-field defaults (data, struct, list), enums from another module,
!> List(Text)/List(Data)/List(List)/List(struct), anyPointer.
program test_kitchen
   use capnp
   use common_capnp
   use kitchen_capnp
   implicit none

   integer :: nfail = 0

   call t_consts()
   call t_defaults_on_fresh_struct()
   call t_values_roundtrip()

   if (nfail > 0) then
      print '(a,i0,a)', 'FAILED: ', nfail, ' assertion(s)'
      error stop 1
   end if
   print '(a)', 'All kitchen tests passed.'

contains

   subroutine check_(cond, name)
      logical, intent(in) :: cond
      character(len=*), intent(in) :: name
      if (.not. cond) then
         nfail = nfail + 1
         print '(a,a)', 'FAIL: ', name
      end if
   end subroutine check_

   subroutine t_consts()
      type(capnp_ptr_t) :: l
      type(vec3_t) :: v
      integer :: err
      call check_(ANSWER == 42_int64, 'const: answer')
      call check_(abs(TAU - 6.283185307179586_real64) < 1.0e-15_real64, 'const: tau')
      call check_(GREETING == 'hey there', 'const: greeting')
      call check_(STATUS_BUSY_E == 1, 'const: imported enum')
      call check_(size(MAGIC) == 4 .and. MAGIC(0) == -54_int8 .and. &
                  MAGIC(3) == 13_int8, 'const: data bytes')
      l = primes(err)
      call check_(err == CAPNP_OK .and. capnp_list_len(l) == 4_int64, 'const: list len')
      call check_(capnp_list_get_i32(l, 0_int64, err) == 2_int32 .and. &
                  capnp_list_get_i32(l, 3_int64, err) == 7_int32, 'const: list values')
      v = home(err)
      call check_(err == CAPNP_OK, 'const: struct resolves')
      call check_(vec3_x_get(v) == -1.0_real64 .and. vec3_y_get(v) == 0.5_real64 &
                  .and. vec3_z_get(v) == 9.25_real64, 'const: struct values')
   end subroutine t_consts

   !> A fresh struct must read every default: scalars, text, data blob,
   !> struct blob (imported type), list blob.
   subroutine t_defaults_on_fresh_struct()
      type(capnp_message_t), target :: msg
      type(sink_t) :: s
      type(vec3_t) :: v
      type(capnp_ptr_t) :: l
      character(len=:), allocatable :: str
      integer(int8), allocatable :: b(:)
      integer :: err

      call capnp_message_init_builder(msg, err)
      s = sink_new_root(msg, err)
      call check_(err == CAPNP_OK, 'defaults: new root')

      call check_(sink_flag_get(s), 'defaults: bool true')
      call check_(sink_count_get(s) == -7_int32, 'defaults: int -7')
      call check_(sink_ratio_get(s) == 2.5_real64, 'defaults: float 2.5')
      call sink_label_get(s, str, err)
      call check_(str == 'unnamed', 'defaults: text')
      call sink_payload_get(s, b, err)
      call check_(size(b) == 4 .and. b(lbound(b, 1)) == -34_int8, 'defaults: data blob')
      call check_(sink_state_get(s) == STATUS_BUSY_E, 'defaults: imported enum field')

      v = sink_origin_get(s, err)
      call check_(err == CAPNP_OK, 'defaults: struct blob resolves')
      call check_(vec3_x_get(v) == 1.5_real64 .and. vec3_y_get(v) == 2.5_real64 &
                  .and. vec3_z_get(v) == -3.5_real64, 'defaults: struct blob values')

      ! scores default [3, 1, 4]
      block
         type(capnp_message_t), target :: rmsg
         type(sink_t) :: rs
         integer(int8), allocatable :: bytes(:)
         ! Read side: a reader message with a null field yields the blob view.
         call capnp_serialize_bytes(msg, bytes, err)
         call capnp_deserialize_bytes(bytes, rmsg, err)
         rs = sink_read_root(rmsg, err)
         l = sink_scores_get(rs, err)
         call check_(err == CAPNP_OK .and. capnp_list_len(l) == 3_int64, 'defaults: list blob len')
         call check_(capnp_list_get_i64(l, 0_int64, err) == 3_int64 .and. &
                     capnp_list_get_i64(l, 2_int64, err) == 4_int64, 'defaults: list blob values')
         call capnp_message_free(rmsg)
      end block
      call capnp_message_free(msg)
   end subroutine t_defaults_on_fresh_struct

   subroutine t_values_roundtrip()
      type(capnp_message_t), target :: msg, rmsg
      type(sink_t) :: s
      type(vec3_t) :: v
      type(capnp_ptr_t) :: l, row, any
      integer(int8), allocatable :: bytes(:), b(:)
      character(len=:), allocatable :: str
      integer :: err, i

      call capnp_message_init_builder(msg, err)
      s = sink_new_root(msg, err)
      call sink_flag_set(s, .false., err)
      call sink_count_set(s, 123456_int32, err)
      call sink_ratio_set(s, 0.25_real64, err)
      call sink_label_set(s, 'named after all', err)
      call sink_state_set(s, STATUS_DOWN_E, err)

      v = sink_origin_init(s, err)
      call vec3_x_set(v, 9.0_real64, err)
      call vec3_y_set(v, 8.0_real64, err)
      call vec3_z_set(v, 7.0_real64, err)

      l = sink_tags_init(s, 2_int64, err)
      call capnp_list_set_text(l, 0, 'red', err)
      call capnp_list_set_text(l, 1, 'blue', err)

      l = sink_grid_init(s, 2_int64, err)
      do i = 0, 1
         row = capnp_new_list(msg, CAPNP_SZ_FOUR, 2_int64, err)
         call capnp_list_set_i32(row, 0_int64, int(10*i + 1, int32), err)
         call capnp_list_set_i32(row, 1_int64, int(10*i + 2, int32), err)
         call capnp_setp(l, i, row, err)
      end do

      l = sink_spots_init(s, 1_int64, err)
      v%p = capnp_list_get_struct(l, 0, err)
      call vec3_x_set(v, 0.5_real64, err)

      any = capnp_new_list(msg, CAPNP_SZ_BYTE, 3_int64, err)
      call capnp_list_set_i8(any, 0_int64, 1_int8, err)
      call sink_stuff_set(s, any, err)
      call check_(err == CAPNP_OK, 'values: built')

      call capnp_serialize_bytes(msg, bytes, err)
      call capnp_deserialize_bytes(bytes, rmsg, err)
      s = sink_read_root(rmsg, err)

      call check_(.not. sink_flag_get(s), 'values: bool')
      call check_(sink_count_get(s) == 123456_int32, 'values: int')
      call check_(sink_ratio_get(s) == 0.25_real64, 'values: float')
      call sink_label_get(s, str, err)
      call check_(str == 'named after all', 'values: text')
      call check_(sink_state_get(s) == STATUS_DOWN_E, 'values: enum')
      v = sink_origin_get(s, err)
      call check_(vec3_x_get(v) == 9.0_real64 .and. vec3_z_get(v) == 7.0_real64, &
                  'values: nested struct')
      call sink_tags_get_elem(s, 1, str, err)
      call check_(str == 'blue', 'values: List(Text) elem')
      l = sink_grid_get(s, err)
      row = capnp_getp(l, 1, err)
      call check_(capnp_list_get_i32(row, 1_int64, err) == 12_int32, 'values: List(List)')
      l = sink_spots_get(s, err)
      v%p = capnp_list_get_struct(l, 0, err)
      call check_(vec3_x_get(v) == 0.5_real64, 'values: List(struct)')
      any = sink_stuff_get(s, err)
      call check_(any%kind == CAPNP_PK_LIST .and. capnp_list_len(any) == 3_int64, &
                  'values: anyPointer')
      call capnp_message_free(msg)
      call capnp_message_free(rmsg)
   end subroutine t_values_roundtrip

end program test_kitchen
