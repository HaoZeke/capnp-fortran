!> Minimal C ABI over the Fortran runtime, enough to build, serialize, and
!> read back an addressbook-style message from C. Exists to drive the
!> golden-master interop tests (interop/golden_master.c) that compare our
!> wire output against opensourcerouting/c-capnproto byte for byte.
!>
!> Handles are small integers into module-level pools:
!>   - a message pool (max 16 concurrent capnp_message_t targets), and
!>   - a per-message object pool of resolved capnp_ptr_t handles.
!> Object ids index the per-message object pool (1-based). Every entry point
!> is bind(c); errors surface as capnp error codes (0 == CAPNP_OK).
!>
!> The message targets are module-SAVE so the capnp_message_t pointers that
!> capnp_ptr_t carries stay valid across calls (same target contract the
!> runtime documents for callers holding handles).
module capnp_cabi
   use iso_c_binding
   use capnp
   implicit none
   private

   integer, parameter :: MAXMSG = 16
   integer, parameter :: MAXOBJ = 1024

   type(capnp_message_t), target, save :: g_pool(MAXMSG)
   logical, save :: g_used(MAXMSG) = .false.
   type(capnp_ptr_t), save :: g_obj(MAXMSG, MAXOBJ)
   integer, save :: g_nobj(MAXMSG) = 0

contains

   ! ------------------------------------------------------------------
   ! Internal helpers (not part of the C ABI).
   ! ------------------------------------------------------------------

   logical function msg_ok(h)
      integer, intent(in) :: h
      msg_ok = (h >= 1 .and. h <= MAXMSG)
      if (msg_ok) msg_ok = g_used(h)
   end function msg_ok

   logical function obj_ok(h, id)
      integer, intent(in) :: h, id
      obj_ok = msg_ok(h)
      if (obj_ok) obj_ok = (id >= 1 .and. id <= g_nobj(h))
   end function obj_ok

   !> Store a resolved handle in message h's object pool, return its id
   !> (or -1 if the pool is full).
   integer function push_obj(h, p)
      integer, intent(in) :: h
      type(capnp_ptr_t), intent(in) :: p
      if (g_nobj(h) >= MAXOBJ) then
         push_obj = -1
         return
      end if
      g_nobj(h) = g_nobj(h) + 1
      push_obj = g_nobj(h)
      g_obj(h, push_obj) = p
   end function push_obj

   !> Copy a NUL-terminated C string into a Fortran string.
   function c_to_f(cstr) result(s)
      character(kind=c_char), intent(in) :: cstr(*)
      character(len=:), allocatable :: s
      integer :: n, i
      n = 0
      do
         if (cstr(n + 1) == c_null_char) exit
         n = n + 1
      end do
      allocate (character(len=n) :: s)
      do i = 1, n
         s(i:i) = char(iachar(cstr(i)))
      end do
   end function c_to_f

   !> Find a free message slot; -1 if none.
   integer function alloc_slot()
      integer :: i
      alloc_slot = -1
      do i = 1, MAXMSG
         if (.not. g_used(i)) then
            alloc_slot = i
            return
         end if
      end do
   end function alloc_slot

   ! ------------------------------------------------------------------
   ! Builder lifecycle
   ! ------------------------------------------------------------------

   integer(c_int) function cabi_builder_new() bind(c, name='cabi_builder_new')
      integer :: h, err
      cabi_builder_new = -1
      h = alloc_slot()
      if (h < 0) return
      call capnp_message_init_builder(g_pool(h), err)
      if (err /= CAPNP_OK) return
      g_used(h) = .true.
      g_nobj(h) = 0
      cabi_builder_new = h
   end function cabi_builder_new

   subroutine cabi_builder_free(h) bind(c, name='cabi_builder_free')
      integer(c_int), value :: h
      if (h >= 1 .and. h <= MAXMSG) then
         if (g_used(h)) call capnp_message_free(g_pool(h))
         g_used(h) = .false.
         g_nobj(h) = 0
      end if
   end subroutine cabi_builder_free

   ! ------------------------------------------------------------------
   ! Object allocation
   ! ------------------------------------------------------------------

   integer(c_int) function cabi_new_struct(h, dwords, pwords) &
      bind(c, name='cabi_new_struct')
      integer(c_int), value :: h, dwords, pwords
      type(capnp_ptr_t) :: p
      integer :: err
      cabi_new_struct = -1
      if (.not. msg_ok(int(h))) return
      p = capnp_new_struct(g_pool(h), int(dwords), int(pwords), err)
      if (err /= CAPNP_OK) return
      cabi_new_struct = push_obj(int(h), p)
   end function cabi_new_struct

   integer(c_int) function cabi_new_composite_list(h, count, dwords, pwords) &
      bind(c, name='cabi_new_composite_list')
      integer(c_int), value :: h, count, dwords, pwords
      type(capnp_ptr_t) :: p
      integer :: err
      cabi_new_composite_list = -1
      if (.not. msg_ok(int(h))) return
      p = capnp_new_composite_list(g_pool(h), int(count, int64), &
                                   int(dwords), int(pwords), err)
      if (err /= CAPNP_OK) return
      cabi_new_composite_list = push_obj(int(h), p)
   end function cabi_new_composite_list

   integer(c_int) function cabi_list_get_struct(h, list_id, i) &
      bind(c, name='cabi_list_get_struct')
      integer(c_int), value :: h, list_id, i
      type(capnp_ptr_t) :: q
      integer :: err
      cabi_list_get_struct = -1
      if (.not. obj_ok(int(h), int(list_id))) return
      q = capnp_list_get_struct(g_obj(h, list_id), int(i), err)
      if (err /= CAPNP_OK) return
      cabi_list_get_struct = push_obj(int(h), q)
   end function cabi_list_get_struct

   ! ------------------------------------------------------------------
   ! Pointer wiring
   ! ------------------------------------------------------------------

   integer(c_int) function cabi_set_root(h, obj_id) bind(c, name='cabi_set_root')
      integer(c_int), value :: h, obj_id
      integer :: err
      cabi_set_root = CAPNP_ERR_ARG
      if (.not. obj_ok(int(h), int(obj_id))) return
      call capnp_set_root(g_pool(h), g_obj(h, obj_id), err)
      cabi_set_root = err
   end function cabi_set_root

   integer(c_int) function cabi_setp(h, obj_id, slot, child_id) &
      bind(c, name='cabi_setp')
      integer(c_int), value :: h, obj_id, slot, child_id
      integer :: err
      cabi_setp = CAPNP_ERR_ARG
      if (.not. obj_ok(int(h), int(obj_id))) return
      if (.not. obj_ok(int(h), int(child_id))) return
      call capnp_setp(g_obj(h, obj_id), int(slot), g_obj(h, child_id), err)
      cabi_setp = err
   end function cabi_setp

   ! ------------------------------------------------------------------
   ! Primitive setters. Offsets are byte offsets in the data section
   ! (bit offsets for bool), matching capn_write32/capn_write16/... .
   ! The wire bytes are identical whether written signed or unsigned;
   ! the shim writes raw kinds so it can mirror c-capnproto exactly.
   ! ------------------------------------------------------------------

   integer(c_int) function cabi_set_u32(h, obj_id, byte_off, value) &
      bind(c, name='cabi_set_u32')
      integer(c_int), value :: h, obj_id, byte_off
      integer(c_int32_t), value :: value
      integer :: err
      cabi_set_u32 = CAPNP_ERR_ARG
      if (.not. obj_ok(int(h), int(obj_id))) return
      call capnp_set_i32(g_obj(h, obj_id), int(byte_off, int64), int(value, int32), err)
      cabi_set_u32 = err
   end function cabi_set_u32

   integer(c_int) function cabi_set_u16(h, obj_id, byte_off, value) &
      bind(c, name='cabi_set_u16')
      integer(c_int), value :: h, obj_id, byte_off
      integer(c_int32_t), value :: value
      integer(int64) :: u
      integer :: err
      cabi_set_u16 = CAPNP_ERR_ARG
      if (.not. obj_ok(int(h), int(obj_id))) return
      u = iand(int(value, int64), 65535_int64)
      if (u > 32767_int64) u = u - 65536_int64
      call capnp_set_i16(g_obj(h, obj_id), int(byte_off, int64), int(u, int16), err)
      cabi_set_u16 = err
   end function cabi_set_u16

   integer(c_int) function cabi_set_i64(h, obj_id, byte_off, value) &
      bind(c, name='cabi_set_i64')
      integer(c_int), value :: h, obj_id, byte_off
      integer(c_int64_t), value :: value
      integer :: err
      cabi_set_i64 = CAPNP_ERR_ARG
      if (.not. obj_ok(int(h), int(obj_id))) return
      call capnp_set_i64(g_obj(h, obj_id), int(byte_off, int64), int(value, int64), err)
      cabi_set_i64 = err
   end function cabi_set_i64

   integer(c_int) function cabi_set_f64(h, obj_id, byte_off, value) &
      bind(c, name='cabi_set_f64')
      integer(c_int), value :: h, obj_id, byte_off
      real(c_double), value :: value
      integer :: err
      cabi_set_f64 = CAPNP_ERR_ARG
      if (.not. obj_ok(int(h), int(obj_id))) return
      call capnp_set_f64(g_obj(h, obj_id), int(byte_off, int64), real(value, real64), err)
      cabi_set_f64 = err
   end function cabi_set_f64

   integer(c_int) function cabi_set_bool(h, obj_id, bit_off, value) &
      bind(c, name='cabi_set_bool')
      integer(c_int), value :: h, obj_id, bit_off, value
      integer :: err
      cabi_set_bool = CAPNP_ERR_ARG
      if (.not. obj_ok(int(h), int(obj_id))) return
      call capnp_set_bool(g_obj(h, obj_id), int(bit_off, int64), value /= 0, err)
      cabi_set_bool = err
   end function cabi_set_bool

   integer(c_int) function cabi_set_text(h, obj_id, slot, str) &
      bind(c, name='cabi_set_text')
      integer(c_int), value :: h, obj_id, slot
      character(kind=c_char), intent(in) :: str(*)
      integer :: err
      cabi_set_text = CAPNP_ERR_ARG
      if (.not. obj_ok(int(h), int(obj_id))) return
      call capnp_set_text(g_obj(h, obj_id), int(slot), c_to_f(str), err)
      cabi_set_text = err
   end function cabi_set_text

   ! ------------------------------------------------------------------
   ! Serialization
   ! ------------------------------------------------------------------

   !> Frame message h into the caller buffer (cap bytes). written receives the
   !> full framed length even when it exceeds cap (so callers can resize).
   integer(c_int) function cabi_serialize(h, buf, cap, written) &
      bind(c, name='cabi_serialize')
      integer(c_int), value :: h
      type(c_ptr), value :: buf
      integer(c_int64_t), value :: cap
      integer(c_int64_t), intent(out) :: written
      integer(int8), allocatable :: b(:)
      integer(int8), pointer :: fb(:)
      integer(int64) :: total
      integer :: err
      written = 0_c_int64_t
      cabi_serialize = CAPNP_ERR_ARG
      if (.not. msg_ok(int(h))) return
      call capnp_serialize_bytes(g_pool(h), b, err)
      if (err /= CAPNP_OK) then
         cabi_serialize = err
         return
      end if
      total = size(b, kind=int64)
      written = int(total, c_int64_t)
      if (total > cap) then
         cabi_serialize = CAPNP_ERR_ARG
         return
      end if
      if (total > 0_int64) then
         call c_f_pointer(buf, fb, [int(total)])
         fb(1:total) = b(0:total - 1)
      end if
      cabi_serialize = CAPNP_OK
   end function cabi_serialize

   !> Parse framed bytes into a fresh reader message; returns a handle or -1.
   integer(c_int) function cabi_deserialize(buf, length) &
      bind(c, name='cabi_deserialize')
      type(c_ptr), value :: buf
      integer(c_int64_t), value :: length
      integer(int8), pointer :: fb(:)
      integer(int8), allocatable :: b(:)
      integer :: h, err
      cabi_deserialize = -1
      if (length < 1_c_int64_t) return
      h = alloc_slot()
      if (h < 0) return
      call c_f_pointer(buf, fb, [int(length)])
      allocate (b(0:length - 1))
      b = fb(1:length)
      call capnp_deserialize_bytes(b, g_pool(h), err)
      if (err /= CAPNP_OK) return
      g_used(h) = .true.
      g_nobj(h) = 0
      cabi_deserialize = h
   end function cabi_deserialize

   integer(c_int) function cabi_root(h) bind(c, name='cabi_root')
      integer(c_int), value :: h
      type(capnp_ptr_t) :: p
      integer :: err
      cabi_root = -1
      if (.not. msg_ok(int(h))) return
      p = capnp_root(g_pool(h), err)
      if (err /= CAPNP_OK) return
      cabi_root = push_obj(int(h), p)
   end function cabi_root

   ! ------------------------------------------------------------------
   ! Getters
   ! ------------------------------------------------------------------

   integer(c_int32_t) function cabi_get_u32(h, obj_id, byte_off) &
      bind(c, name='cabi_get_u32')
      integer(c_int), value :: h, obj_id, byte_off
      cabi_get_u32 = 0_c_int32_t
      if (.not. obj_ok(int(h), int(obj_id))) return
      cabi_get_u32 = int(capnp_get_i32(g_obj(h, obj_id), int(byte_off, int64)), c_int32_t)
   end function cabi_get_u32

   integer(c_int32_t) function cabi_get_u16(h, obj_id, byte_off) &
      bind(c, name='cabi_get_u16')
      integer(c_int), value :: h, obj_id, byte_off
      cabi_get_u16 = 0_c_int32_t
      if (.not. obj_ok(int(h), int(obj_id))) return
      cabi_get_u16 = int(iand(int(capnp_get_i16(g_obj(h, obj_id), int(byte_off, int64)), int64), &
                              65535_int64), c_int32_t)
   end function cabi_get_u16

   integer(c_int64_t) function cabi_get_i64(h, obj_id, byte_off) &
      bind(c, name='cabi_get_i64')
      integer(c_int), value :: h, obj_id, byte_off
      cabi_get_i64 = 0_c_int64_t
      if (.not. obj_ok(int(h), int(obj_id))) return
      cabi_get_i64 = int(capnp_get_i64(g_obj(h, obj_id), int(byte_off, int64)), c_int64_t)
   end function cabi_get_i64

   real(c_double) function cabi_get_f64(h, obj_id, byte_off) &
      bind(c, name='cabi_get_f64')
      integer(c_int), value :: h, obj_id, byte_off
      cabi_get_f64 = 0.0_c_double
      if (.not. obj_ok(int(h), int(obj_id))) return
      cabi_get_f64 = real(capnp_get_f64(g_obj(h, obj_id), int(byte_off, int64)), c_double)
   end function cabi_get_f64

   integer(c_int) function cabi_get_bool(h, obj_id, bit_off) &
      bind(c, name='cabi_get_bool')
      integer(c_int), value :: h, obj_id, bit_off
      cabi_get_bool = 0
      if (.not. obj_ok(int(h), int(obj_id))) return
      if (capnp_get_bool(g_obj(h, obj_id), int(bit_off, int64))) cabi_get_bool = 1
   end function cabi_get_bool

   integer(c_int) function cabi_getp(h, obj_id, slot) bind(c, name='cabi_getp')
      integer(c_int), value :: h, obj_id, slot
      type(capnp_ptr_t) :: q
      integer :: err
      cabi_getp = -1
      if (.not. obj_ok(int(h), int(obj_id))) return
      q = capnp_getp(g_obj(h, obj_id), int(slot), err)
      if (err /= CAPNP_OK) return
      cabi_getp = push_obj(int(h), q)
   end function cabi_getp

   !> Read text at pointer slot into the caller buffer (cap bytes, no NUL is
   !> appended). written receives the full text length in bytes.
   integer(c_int) function cabi_get_text(h, obj_id, slot, buf, cap, written) &
      bind(c, name='cabi_get_text')
      integer(c_int), value :: h, obj_id, slot
      type(c_ptr), value :: buf
      integer(c_int64_t), value :: cap
      integer(c_int64_t), intent(out) :: written
      character(len=:), allocatable :: s
      integer(int8), pointer :: fb(:)
      integer(int64) :: n, m, i
      integer :: err
      written = 0_c_int64_t
      cabi_get_text = CAPNP_ERR_ARG
      if (.not. obj_ok(int(h), int(obj_id))) return
      call capnp_get_text(g_obj(h, obj_id), int(slot), s, err)
      if (err /= CAPNP_OK) then
         cabi_get_text = err
         return
      end if
      n = len(s, kind=int64)
      written = int(n, c_int64_t)
      m = min(n, cap)
      if (m > 0_int64) then
         call c_f_pointer(buf, fb, [int(m)])
         do i = 1_int64, m
            fb(i) = cp_i8b(int(iachar(s(i:i)), int32))
         end do
      end if
      cabi_get_text = CAPNP_OK
   end function cabi_get_text

   integer(c_int64_t) function cabi_list_len(h, list_id) &
      bind(c, name='cabi_list_len')
      integer(c_int), value :: h, list_id
      cabi_list_len = 0_c_int64_t
      if (.not. obj_ok(int(h), int(list_id))) return
      cabi_list_len = int(capnp_list_len(g_obj(h, list_id)), c_int64_t)
   end function cabi_list_len

end module capnp_cabi
