!> Message reader/builder core: pointer resolution (far chains included),
!> struct/list object handles, primitive field access with default XOR,
!> text/data helpers.
!>
!> Generated from capnp_message.fypp; edit the template, then `pixi run gen`.
!>
!> Handles carry a pointer to their message, so messages must be declared
!> with the `target` attribute by callers (same contract as c-capnproto's
!> `struct capn*` inside `capn_ptr`).
module capnp_message
   use capnp_kinds
   use capnp_endian
   use capnp_pointer
   use capnp_arena, only: capnp_message_t, capnp_segment_t, &
                          capnp_arena_alloc, capnp_arena_alloc_in
   implicit none
   private

   public :: capnp_ptr_t
   public :: capnp_root, capnp_set_root, capnp_getp, capnp_setp
   public :: capnp_new_struct, capnp_new_list, capnp_new_composite_list
   public :: capnp_list_len, capnp_list_getp, capnp_list_get_struct
   public :: capnp_get_text, capnp_set_text, capnp_get_data, capnp_set_data
   public :: capnp_get_bool, capnp_set_bool
   public :: capnp_list_get_bool, capnp_list_set_bool
   public :: capnp_copy, capnp_disown
   public :: capnp_get_u64, capnp_set_u64
   public :: capnp_list_get_text, capnp_list_set_text
   public :: capnp_list_get_all_bool, capnp_list_set_all_bool
   public :: capnp_get_data_view, capnp_get_text_view, capnp_text_len
   public :: capnp_total_size
   public :: capnp_ptr_kind, capnp_ptr_is_null, capnp_ptr_list_len
   public :: capnp_get_i8, capnp_set_i8
   public :: capnp_list_get_i8, capnp_list_set_i8
   public :: capnp_list_get_all_i8, capnp_list_set_all_i8
   public :: capnp_get_i16, capnp_set_i16
   public :: capnp_list_get_i16, capnp_list_set_i16
   public :: capnp_list_get_all_i16, capnp_list_set_all_i16
   public :: capnp_get_i32, capnp_set_i32
   public :: capnp_list_get_i32, capnp_list_set_i32
   public :: capnp_list_get_all_i32, capnp_list_set_all_i32
   public :: capnp_get_i64, capnp_set_i64
   public :: capnp_list_get_i64, capnp_list_set_i64
   public :: capnp_list_get_all_i64, capnp_list_set_all_i64
   public :: capnp_get_u8, capnp_set_u8
   public :: capnp_get_u16, capnp_set_u16
   public :: capnp_get_u32, capnp_set_u32
   public :: capnp_get_f32, capnp_set_f32
   public :: capnp_list_get_f32, capnp_list_set_f32
   public :: capnp_get_f64, capnp_set_f64
   public :: capnp_list_get_f64, capnp_list_set_f64

   !> Resolved object handle; value type, cheap to copy. For composite lists
   !> off points past the tag word and dwords/pwords hold the element sizes.
   !> dbits >= 0 marks a synthetic struct view over a primitive list element
   !> (schema-evolution upgrade); reads beyond dbits yield defaults and
   !> writes beyond it are bounds errors.
   !> depth is the nesting level at which this handle was resolved (root = 0);
   !> subsequent getp/list pointer walks use depth+1 against msg%depth_limit.
   type :: capnp_ptr_t
      integer :: kind = CAPNP_PK_NULL
      integer :: seg = 0
      integer(int64) :: off = 0_int64
      integer :: dwords = 0
      integer :: pwords = 0
      integer :: esize = -1
      integer(int64) :: nelem = 0_int64
      integer(int64) :: capidx = 0_int64
      !> Schema node id when the dynamic API knows the struct type. 0 means
      !> the handle was not stamped, so callers cannot check identity.
      integer(int64) :: schema_id = 0_int64
      integer(int64) :: dbits = -1_int64
      integer :: depth = 0
      !> Reader views of schema defaults. Writers refuse these so a
      !> default shared with the schema cannot be mutated.
      logical :: frozen = .false.
      type(capnp_message_t), pointer :: msg => null()
   end type capnp_ptr_t

contains

   ! ------------------------------------------------------------------
   ! Pointer resolution
   ! ------------------------------------------------------------------

   !> Resolve the pointer word stored at (seg, byte_off) into an object
   !> handle, following a single or double far pointer as in C++
   !> `WireHelpers::followFars`. Reader guard: traversal budget is charged
   !> per landing pad and per resolved object.
   subroutine resolve_at(msg, seg, byte_off, depth, p, err)
      type(capnp_message_t), intent(inout), target :: msg
      integer, intent(in) :: seg
      integer(int64), intent(in) :: byte_off
      integer, intent(in) :: depth
      type(capnp_ptr_t), intent(out) :: p
      integer, intent(out) :: err
      integer(int64) :: w, w0, base, need_bytes, tseg64, pad_off, pad_bytes
      integer :: k, tseg, oseg

      err = CAPNP_OK
      p = capnp_ptr_t()
      p%msg => msg

      if (seg < 1 .or. seg > msg%nsegs) then
         err = CAPNP_ERR_SEGMENT
         return
      end if
      if (byte_off < 0_int64 .or. byte_off + 8_int64 > msg%segs(seg)%len) then
         err = CAPNP_ERR_BOUNDS
         return
      end if

      w = cp_get_i64(msg%segs(seg)%bytes, byte_off)
      ! A null pointer returns before the nesting check (layout.c++:2174).
      if (w == 0_int64) return
      ! readStructPointer requires nestingLimit > 0, then stores limit-1.
      ! followFars does not consume a level (layout.c++:2186, 2215).
      if (depth >= msg%depth_limit) then
         err = CAPNP_ERR_DEPTH
         return
      end if

      tseg = seg
      if (wp_kind(w) == CAPNP_WK_FAR) then
         tseg64 = wp_far_seg(w)
         if (tseg64 < 0_int64 .or. tseg64 >= int(msg%nsegs, int64)) then
            err = CAPNP_ERR_SEGMENT
            return
         end if
         tseg = int(tseg64) + 1
         if (msg%segs(tseg)%len == 0_int64) then
            err = CAPNP_ERR_SEGMENT
            return
         end if
         pad_off = wp_far_off(w)*8_int64
         pad_bytes = merge(16_int64, 8_int64, wp_far_two(w))
         if (pad_off < 0_int64 .or. pad_off + pad_bytes > msg%segs(tseg)%len) then
            err = CAPNP_ERR_BOUNDS
            return
         end if
         if (.not. charge(msg, pad_bytes, err)) return
         if (.not. wp_far_two(w)) then
            ! Single far: landing pad replaces the pointer without re-checking null.
            w = cp_get_i64(msg%segs(tseg)%bytes, pad_off)
            base = clamp_target(msg, tseg, pad_off + 8_int64 + int(wp_offset(w), int64)*8_int64)
         else
            ! Double far: pad word 0 locates the object, word 1 describes it.
            w0 = cp_get_i64(msg%segs(tseg)%bytes, pad_off)
            w = cp_get_i64(msg%segs(tseg)%bytes, pad_off + 8_int64)
            tseg64 = wp_far_seg(w0)
            if (tseg64 < 0_int64 .or. tseg64 >= int(msg%nsegs, int64)) then
               err = CAPNP_ERR_SEGMENT
               return
            end if
            oseg = int(tseg64) + 1
            if (msg%segs(oseg)%len == 0_int64) then
               err = CAPNP_ERR_SEGMENT
               return
            end if
            if (wp_kind(w0) /= CAPNP_WK_FAR) then
               err = CAPNP_ERR_KIND
               return
            end if
            tseg = oseg
            base = clamp_target(msg, tseg, wp_far_off(w0)*8_int64)
         end if
      else
         base = clamp_target(msg, tseg, byte_off + 8_int64 + int(wp_offset(w), int64)*8_int64)
      end if

      k = wp_kind(w)
      select case (k)
      case (CAPNP_WK_STRUCT)
         need_bytes = int(wp_struct_dwords(w) + wp_struct_pwords(w), int64)*8_int64
         if (base + need_bytes > msg%segs(tseg)%len) then
            err = CAPNP_ERR_BOUNDS
            return
         end if
         if (.not. charge(msg, need_bytes, err)) return
         p%kind = CAPNP_PK_STRUCT
         p%seg = tseg
         p%off = base
         p%dwords = wp_struct_dwords(w)
         p%pwords = wp_struct_pwords(w)
         p%depth = depth

      case (CAPNP_WK_LIST)
         call fill_list(msg, tseg, base, w, p, err)
         if (err == CAPNP_OK) p%depth = depth

      case (CAPNP_WK_FAR)
         err = CAPNP_ERR_KIND

      case (CAPNP_WK_CAP)
         ! `WirePointer::isCapability` requires the whole low word to be
         ! OTHER. Bits 2 through 31 identify reserved pointer types.
         if (ibits(w, 2, 30) /= 0_int64) then
            err = CAPNP_ERR_KIND
            return
         end if
         p%kind = CAPNP_PK_CAP
         p%capidx = wp_cap_index(w)
         p%depth = depth
      end select
   end subroutine resolve_at

   !> Populate a list handle from a list pointer word whose content starts at
   !> base (byte offset in seg). Composite lists consume their tag word here.
   subroutine fill_list(msg, seg, base, w, p, err)
      type(capnp_message_t), intent(inout), target :: msg
      integer, intent(in) :: seg
      integer(int64), intent(in) :: base
      integer(int64), intent(in) :: w
      type(capnp_ptr_t), intent(out) :: p
      integer, intent(out) :: err
      integer(int64) :: cnt, content_bytes, tag
      integer :: es, sb

      err = CAPNP_OK
      p = capnp_ptr_t()
      p%msg => msg
      es = wp_list_esize(w)
      cnt = wp_list_count(w)

      if (es == CAPNP_SZ_COMPOSITE) then
         content_bytes = (cnt + 1_int64)*8_int64 ! tag + content words
         if (base < 0_int64 .or. base + content_bytes > msg%segs(seg)%len) then
            err = CAPNP_ERR_BOUNDS
            return
         end if
         if (.not. charge(msg, content_bytes, err)) return
         tag = cp_get_i64(msg%segs(seg)%bytes, base)
         if (wp_kind(tag) /= CAPNP_WK_STRUCT) then
            err = CAPNP_ERR_KIND
            return
         end if
         p%kind = CAPNP_PK_LIST
         p%seg = seg
         p%off = base + 8_int64
         p%esize = es
         ! layout.c++ inlineCompositeListElementCount: unsigned 29-bit
         ! count in the tag offset field. Bit 31 is outside that mask.
         p%nelem = ibits(tag, 2, 29)
         p%dwords = wp_struct_dwords(tag)
         p%pwords = wp_struct_pwords(tag)
         ! Element count must fit the declared content words.
         if (p%nelem*int(p%dwords + p%pwords, int64) > cnt) then
            err = CAPNP_ERR_BOUNDS
            p%kind = CAPNP_PK_NULL
            return
         end if
         ! layout.c++:2323: a zero-word element still costs one word.
         if (p%dwords + p%pwords == 0 .and. p%nelem > 0_int64) then
            if (.not. charge(msg, p%nelem*8_int64, err)) p%kind = CAPNP_PK_NULL
         end if
      else
         sb = capnp_list_step_bits(es)
         content_bytes = (int(sb, int64)*cnt + 7_int64)/8_int64
         if (base < 0_int64 .or. base + content_bytes > msg%segs(seg)%len) then
            err = CAPNP_ERR_BOUNDS
            return
         end if
         if (.not. charge(msg, word_round(content_bytes), err)) return
         p%kind = CAPNP_PK_LIST
         p%seg = seg
         p%off = base
         p%esize = es
         p%nelem = cnt
         ! layout.c++:2396: List(Void) costs one word per element.
         if (es == CAPNP_SZ_VOID .and. cnt > 0_int64) then
            if (.not. charge(msg, cnt*8_int64, err)) p%kind = CAPNP_PK_NULL
         end if
      end if
   end subroutine fill_list

   !> A target outside its segment becomes the segment's end, so only an
   !> object of size zero can survive the bounds check that follows (C++
   !> `SegmentReader::checkOffset`).
   pure function clamp_target(msg, seg, base) result(r)
      type(capnp_message_t), intent(in) :: msg
      integer, intent(in) :: seg
      integer(int64), intent(in) :: base
      integer(int64) :: r
      if (base < 0_int64 .or. base > msg%segs(seg)%len) then
         r = msg%segs(seg)%len
      else
         r = base
      end if
   end function clamp_target

   !> Charge the reader traversal budget; builders are exempt.
   function charge(msg, nbytes, err) result(ok)
      type(capnp_message_t), intent(inout) :: msg
      integer(int64), intent(in) :: nbytes
      integer, intent(out) :: err
      logical :: ok
      err = CAPNP_OK
      ok = .true.
      if (msg%is_builder) return
      msg%traversal_words = msg%traversal_words - (nbytes + 7_int64)/8_int64
      if (msg%traversal_words < 0_int64) then
         err = CAPNP_ERR_TRAVERSAL
         ok = .false.
      end if
   end function charge

   pure function word_round(nbytes) result(r)
      integer(int64), intent(in) :: nbytes
      integer(int64) :: r
      r = ((nbytes + 7_int64)/8_int64)*8_int64
   end function word_round

   !> Accessible data-section size in bits: the synthetic-view limit when
   !> set, the full section otherwise.
   pure function data_bits(p) result(b)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64) :: b
      if (p%dbits >= 0_int64) then
         b = p%dbits
      else
         b = int(p%dwords, int64)*64_int64
      end if
   end function data_bits

   ! ------------------------------------------------------------------
   ! Roots and pointer fields
   ! ------------------------------------------------------------------

   !> Resolve the root object (segment 1, word 0).
   function capnp_root(msg, err) result(p)
      type(capnp_message_t), intent(inout), target :: msg
      integer, intent(out) :: err
      type(capnp_ptr_t) :: p
      err = CAPNP_OK
      p = capnp_ptr_t()
      p%msg => msg
      ! An out-of-range root is not charged (arena.h:430 short-circuits).
      if (msg%nsegs < 1 .or. msg%segs(1)%len < 8_int64) then
         err = CAPNP_ERR_BOUNDS
         return
      end if
      ! The root pointer word is charged twice: MessageReader::getRootInternal
      ! (message.c++:100) and PointerReader::getRoot (layout.c++:2707).
      if (.not. charge(msg, 8_int64, err)) return
      if (.not. charge(msg, 8_int64, err)) return
      call resolve_at(msg, 1, 0_int64, 0, p, err)
   end function capnp_root

   !> Point the root at an object built in this message.
   subroutine capnp_set_root(msg, q, err)
      type(capnp_message_t), intent(inout), target :: msg
      type(capnp_ptr_t), intent(in) :: q
      integer, intent(out) :: err
      call write_ptr_slot(msg, 1, 0_int64, q, err)
   end subroutine capnp_set_root

   !> Read pointer field i of a struct (or element i of a pointer list).
   !> Out-of-range struct pointer fields resolve to null, per spec.
   function capnp_getp(p, i, err) result(q)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(in) :: i
      integer, intent(out) :: err
      type(capnp_ptr_t) :: q
      integer(int64) :: slot
      err = CAPNP_OK
      q = capnp_ptr_t()
      if (.not. associated(p%msg)) return
      q%msg => p%msg
      select case (p%kind)
      case (CAPNP_PK_STRUCT)
         if (i < 0 .or. i >= p%pwords) return
         slot = p%off + int(p%dwords, int64)*8_int64 + int(i, int64)*8_int64
      case (CAPNP_PK_LIST)
         if (i < 0 .or. int(i, int64) >= p%nelem) then
            err = CAPNP_ERR_BOUNDS
            return
         end if
         select case (p%esize)
         case (CAPNP_SZ_PTR)
            slot = p%off + int(i, int64)*8_int64
         case (CAPNP_SZ_COMPOSITE)
            ! Downgrade view: first pointer of the element's pointer section.
            if (p%pwords < 1) return
            slot = p%off + int(i, int64)*int(p%dwords + p%pwords, int64)*8_int64 &
                   + int(p%dwords, int64)*8_int64
         case default
            err = CAPNP_ERR_KIND
            return
         end select
      case default
         err = CAPNP_ERR_KIND
         return
      end select
      call resolve_at(p%msg, p%seg, slot, p%depth + 1, q, err)
      if (p%frozen) q%frozen = .true.
   end function capnp_getp

   !> Struct element i of a struct list. Composite lists give the real
   !> element; primitive and pointer lists give the schema-evolution upgrade
   !> view (the element is field @0 of a synthetic struct, per the spec's
   !> list-upgrade rules). Bit lists cannot upgrade.
   function capnp_list_get_struct(p, i, err) result(q)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(in) :: i
      integer, intent(out) :: err
      type(capnp_ptr_t) :: q
      integer :: sb, dstep
      err = CAPNP_OK
      q = capnp_ptr_t()
      if (.not. associated(p%msg)) return
      q%msg => p%msg
      if (p%kind /= CAPNP_PK_LIST) then
         err = CAPNP_ERR_KIND
         return
      end if
      if (i < 0 .or. int(i, int64) >= p%nelem) then
         err = CAPNP_ERR_BOUNDS
         return
      end if
      ! layout.c++ ListReader::getStructElement requires nestingLimit > 0
      ! and passes nestingLimit - 1 to the element reader. Builders carry no
      ! nesting limit in C++, so a builder element keeps the list's depth.
      dstep = 1
      if (p%msg%is_builder) dstep = 0
      if (.not. p%msg%is_builder .and. p%depth + 1 >= p%msg%depth_limit) then
         err = CAPNP_ERR_DEPTH
         return
      end if
      select case (p%esize)
      case (CAPNP_SZ_COMPOSITE)
         q%kind = CAPNP_PK_STRUCT
         q%seg = p%seg
         q%off = p%off + int(i, int64)*int(p%dwords + p%pwords, int64)*8_int64
         q%dwords = p%dwords
         q%pwords = p%pwords
         q%depth = p%depth + dstep
      case (CAPNP_SZ_BYTE, CAPNP_SZ_TWO, CAPNP_SZ_FOUR, CAPNP_SZ_EIGHT)
         sb = capnp_list_step_bits(p%esize)
         q%kind = CAPNP_PK_STRUCT
         q%seg = p%seg
         q%off = p%off + int(i, int64)*int(sb/8, int64)
         q%dwords = 1
         q%pwords = 0
         q%dbits = int(sb, int64)
         q%depth = p%depth + dstep
      case (CAPNP_SZ_PTR)
         q%kind = CAPNP_PK_STRUCT
         q%seg = p%seg
         q%off = p%off + int(i, int64)*8_int64
         q%dwords = 0
         q%pwords = 1
         q%dbits = 0_int64
         q%depth = p%depth + dstep
      case (CAPNP_SZ_VOID)
         ! layout.c++ readListPointer (the non-composite arm) treats VOID as
         ! a struct list with dataSize 0 and pointerCount 0.
         ! getStructElement then returns that zero-size struct.
         q%kind = CAPNP_PK_STRUCT
         q%seg = p%seg
         q%off = p%off
         q%dwords = 0
         q%pwords = 0
         q%dbits = 0_int64
         q%depth = p%depth + dstep
      case default
         err = CAPNP_ERR_KIND
      end select
   end function capnp_list_get_struct

   !> Element i of a pointer list (alias of capnp_getp for lists).
   function capnp_list_getp(p, i, err) result(q)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(in) :: i
      integer, intent(out) :: err
      type(capnp_ptr_t) :: q
      q = capnp_getp(p, i, err)
   end function capnp_list_getp

   pure function capnp_list_len(p) result(n)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64) :: n
      n = 0_int64
      if (p%kind == CAPNP_PK_LIST) n = p%nelem
   end function capnp_list_len

   ! ------------------------------------------------------------------
   ! Builder: object allocation and pointer writing
   ! ------------------------------------------------------------------

   function capnp_new_struct(msg, dwords, pwords, err) result(p)
      type(capnp_message_t), intent(inout), target :: msg
      integer, intent(in) :: dwords, pwords
      integer, intent(out) :: err
      type(capnp_ptr_t) :: p
      integer :: seg
      integer(int64) :: off
      p = capnp_ptr_t()
      if (dwords < 0 .or. pwords < 0 .or. dwords > 65535 .or. pwords > 65535) then
         err = CAPNP_ERR_ARG
         return
      end if
      call capnp_arena_alloc(msg, int(dwords + pwords, int64), seg, off, err)
      if (err /= CAPNP_OK) return
      p%msg => msg
      p%kind = CAPNP_PK_STRUCT
      p%seg = seg
      p%off = off
      p%dwords = dwords
      p%pwords = pwords
   end function capnp_new_struct

   !> New list of a primitive/pointer element size (not composite).
   !> List(Text)/List(Data)/List(AnyPointer) use esize=CAPNP_SZ_PTR (wire C=6),
   !> matching Cap'n Proto C++ and encoding.html. Struct lists use
   !> capnp_new_composite_list (wire C=7).
   function capnp_new_list(msg, esize, count, err) result(p)
      type(capnp_message_t), intent(inout), target :: msg
      integer, intent(in) :: esize
      integer(int64), intent(in) :: count
      integer, intent(out) :: err
      type(capnp_ptr_t) :: p
      integer :: seg, sb
      integer(int64) :: off, nwords
      p = capnp_ptr_t()
      sb = capnp_list_step_bits(esize)
      ! layout.c++ initListPointer rejects elementCount >= 1<<29.
      if (sb < 0 .or. count < 0_int64 .or. count >= 536870912_int64) then
         err = CAPNP_ERR_ARG
         return
      end if
      nwords = (int(sb, int64)*count + 63_int64)/64_int64
      call capnp_arena_alloc(msg, nwords, seg, off, err)
      if (err /= CAPNP_OK) return
      p%msg => msg
      p%kind = CAPNP_PK_LIST
      p%seg = seg
      p%off = off
      p%esize = esize
      p%nelem = count
   end function capnp_new_list

   !> New list of structs (always encoded composite, tag word included).
   function capnp_new_composite_list(msg, count, dwords, pwords, err) result(p)
      type(capnp_message_t), intent(inout), target :: msg
      integer(int64), intent(in) :: count
      integer, intent(in) :: dwords, pwords
      integer, intent(out) :: err
      type(capnp_ptr_t) :: p
      integer :: seg
      integer(int64) :: off, nwords
      p = capnp_ptr_t()
      ! layout.c++ initStructListPointer uses the same 29-bit element cap.
      if (count < 0_int64 .or. count >= 536870912_int64 .or. dwords < 0 .or. pwords < 0) then
         err = CAPNP_ERR_ARG
         return
      end if
      nwords = 1_int64 + count*int(dwords + pwords, int64)
      call capnp_arena_alloc(msg, nwords, seg, off, err)
      if (err /= CAPNP_OK) return
      call cp_put_i64(msg%segs(seg)%bytes, off, &
                      wp_make_struct(int(count, int32), int(dwords, int32), int(pwords, int32)))
      p%msg => msg
      p%kind = CAPNP_PK_LIST
      p%seg = seg
      p%off = off + 8_int64
      p%esize = CAPNP_SZ_COMPOSITE
      p%nelem = count
      p%dwords = dwords
      p%pwords = pwords
   end function capnp_new_composite_list

   !> Write pointer field i of struct p (or element i of a pointer list) to
   !> reference q. Same-segment references encode relative struct/list
   !> pointers; cross-segment references get a far pointer via a landing pad
   !> in q's segment.
   recursive subroutine capnp_setp(p, i, q, err)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(in) :: i
      type(capnp_ptr_t), intent(in) :: q
      integer, intent(out) :: err
      integer(int64) :: slot
      err = CAPNP_OK
      if (p%frozen) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (.not. associated(p%msg)) then
         err = CAPNP_ERR_ARG
         return
      end if
      select case (p%kind)
      case (CAPNP_PK_STRUCT)
         if (i < 0 .or. i >= p%pwords) then
            err = CAPNP_ERR_BOUNDS
            return
         end if
         slot = p%off + int(p%dwords, int64)*8_int64 + int(i, int64)*8_int64
      case (CAPNP_PK_LIST)
         if (p%esize /= CAPNP_SZ_PTR) then
            err = CAPNP_ERR_KIND
            return
         end if
         if (i < 0 .or. int(i, int64) >= p%nelem) then
            err = CAPNP_ERR_BOUNDS
            return
         end if
         slot = p%off + int(i, int64)*8_int64
      case default
         err = CAPNP_ERR_KIND
         return
      end select
      call write_ptr_slot(p%msg, p%seg, slot, q, err)
   end subroutine capnp_setp

   !> First word of the object as referenced by a pointer: composite lists
   !> are referenced through their tag word.
   pure function object_start_word(q) result(w)
      type(capnp_ptr_t), intent(in) :: q
      integer(int64) :: w
      w = q%off/8_int64
      if (q%kind == CAPNP_PK_LIST .and. q%esize == CAPNP_SZ_COMPOSITE) w = w - 1_int64
   end function object_start_word

   !> Pointer word describing q as seen from the word after loc_word.
   pure function local_ptr_word(q, loc_word) result(w)
      type(capnp_ptr_t), intent(in) :: q
      integer(int64), intent(in) :: loc_word
      integer(int64) :: w
      integer(int32) :: rel
      rel = int(object_start_word(q) - (loc_word + 1_int64), int32)
      select case (q%kind)
      case (CAPNP_PK_STRUCT)
         if (q%dwords == 0 .and. q%pwords == 0) then
            ! encoding.html: empty struct is A=0 B=-1, not offset 0 (null).
            w = wp_make_struct(-1_int32, 0_int32, 0_int32)
         else
            w = wp_make_struct(rel, int(q%dwords, int32), int(q%pwords, int32))
         end if
      case (CAPNP_PK_LIST)
         if (q%esize == CAPNP_SZ_COMPOSITE) then
            w = wp_make_list(rel, q%esize, q%nelem*int(q%dwords + q%pwords, int64))
         else
            w = wp_make_list(rel, q%esize, q%nelem)
         end if
      case default
         w = 0_int64
      end select
   end function local_ptr_word

   !> True when [base, base+nbytes) lies inside segment seg.
   pure function span_ok(msg, seg, base, nbytes) result(ok)
      type(capnp_message_t), intent(in) :: msg
      integer, intent(in) :: seg
      integer(int64), intent(in) :: base, nbytes
      logical :: ok
      ok = .false.
      if (seg < 1 .or. seg > msg%nsegs) return
      if (base < 0_int64 .or. nbytes < 0_int64) return
      if (base > msg%segs(seg)%len) return
      if (nbytes > msg%segs(seg)%len - base) return
      ok = .true.
   end function span_ok

   subroutine zero_bytes(msg, seg, base, nbytes)
      type(capnp_message_t), intent(inout) :: msg
      integer, intent(in) :: seg
      integer(int64), intent(in) :: base, nbytes
      if (nbytes <= 0_int64) return
      if (.not. span_ok(msg, seg, base, nbytes)) return
      msg%segs(seg)%bytes(base:base + nbytes - 1) = 0_int8
   end subroutine zero_bytes

   !> Bytes occupied by a list body. Composite includes the tag word.
   pure function list_payload_bytes(w) result(n)
      integer(int64), intent(in) :: w
      integer(int64) :: n
      integer :: es, sb
      integer(int64) :: cnt
      es = wp_list_esize(w)
      cnt = wp_list_count(w)
      select case (es)
      case (CAPNP_SZ_VOID)
         n = 0_int64
      case (CAPNP_SZ_COMPOSITE)
         n = (cnt + 1_int64)*8_int64
      case (CAPNP_SZ_PTR)
         n = cnt*8_int64
      case default
         sb = capnp_list_step_bits(es)
         if (sb < 0) then
            n = -1_int64
         else
            n = ((int(sb, int64)*cnt + 63_int64)/64_int64)*8_int64
         end if
      end select
   end function list_payload_bytes

   !> Zero the object `tag` describes at `base`. The tag's offset is ignored.
   !> layout.c++ zeroObject(segment, capTable, tag, ptr), lines 669-737.
   recursive subroutine zero_at(msg, seg, base, tag, depth)
      type(capnp_message_t), intent(inout) :: msg
      integer, intent(in) :: seg, depth
      integer(int64), intent(in) :: base, tag
      integer :: es, dwords, pwords, j
      integer(int64) :: cnt, nbytes, child, stride, nelem, i64, ctag, pos
      if (depth >= CAPNP_DEFAULT_DEPTH_LIMIT) return
      select case (wp_kind(tag))
      case (CAPNP_WK_STRUCT)
         dwords = wp_struct_dwords(tag)
         pwords = wp_struct_pwords(tag)
         nbytes = int(dwords + pwords, int64)*8_int64
         if (.not. span_ok(msg, seg, base, nbytes)) return
         do j = 0, pwords - 1
            child = base + int(dwords, int64)*8_int64 + int(j, int64)*8_int64
            call zero_pointer_word(msg, seg, child, &
                                    cp_get_i64(msg%segs(seg)%bytes, child), depth + 1)
         end do
         call zero_bytes(msg, seg, base, nbytes)
      case (CAPNP_WK_LIST)
         es = wp_list_esize(tag)
         cnt = wp_list_count(tag)
         if (es == CAPNP_SZ_VOID) return
         if (es == CAPNP_SZ_PTR) then
            nbytes = cnt*8_int64
            if (.not. span_ok(msg, seg, base, nbytes)) return
            do i64 = 0_int64, cnt - 1_int64
               child = base + i64*8_int64
               call zero_pointer_word(msg, seg, child, &
                                       cp_get_i64(msg%segs(seg)%bytes, child), depth + 1)
            end do
            call zero_bytes(msg, seg, base, nbytes)
         else if (es == CAPNP_SZ_COMPOSITE) then
            nbytes = (cnt + 1_int64)*8_int64
            if (.not. span_ok(msg, seg, base, nbytes)) return
            ctag = cp_get_i64(msg%segs(seg)%bytes, base)
            if (wp_kind(ctag) == CAPNP_WK_STRUCT) then
               dwords = wp_struct_dwords(ctag)
               pwords = wp_struct_pwords(ctag)
               stride = int(dwords + pwords, int64)
               nelem = ibits(ctag, 2, 29)
               if (pwords > 0 .and. stride > 0_int64) then
                  if (nelem > cnt/stride) nelem = cnt/stride
                  do i64 = 0_int64, nelem - 1_int64
                     pos = base + 8_int64 + i64*stride*8_int64 + int(dwords, int64)*8_int64
                     do j = 0, pwords - 1
                        child = pos + int(j, int64)*8_int64
                        call zero_pointer_word(msg, seg, child, &
                           cp_get_i64(msg%segs(seg)%bytes, child), depth + 1)
                     end do
                  end do
               end if
            end if
            call zero_bytes(msg, seg, base, nbytes)
         else
            nbytes = list_payload_bytes(tag)
            call zero_bytes(msg, seg, base, nbytes)
         end if
      end select
   end subroutine zero_at

   !> Zero the object a pointer word addresses, including a far landing pad.
   !> layout.c++ zeroObject(segment, capTable, ref), lines 625-667.
   recursive subroutine zero_pointer_word(msg, seg, slot, w, depth)
      type(capnp_message_t), intent(inout) :: msg
      integer, intent(in) :: seg, depth
      integer(int64), intent(in) :: slot, w
      integer :: tseg, oseg
      integer(int64) :: base, padoff, pad, cw, w0, tag, tseg64, oseg64
      if (depth >= CAPNP_DEFAULT_DEPTH_LIMIT .or. w == 0_int64) return
      select case (wp_kind(w))
      case (CAPNP_WK_STRUCT, CAPNP_WK_LIST)
         base = slot + 8_int64 + int(wp_offset(w), int64)*8_int64
         call zero_at(msg, seg, base, w, depth)
      case (CAPNP_WK_FAR)
         ! zeroObject follows a landing pad that is itself a far pointer and
         ! a far hop costs no nesting level. Each chained pad is cleared once
         ! read, so a cyclic chain ends at a word that is already zero.
         cw = w
         do
            tseg64 = wp_far_seg(cw)
            if (tseg64 < 0_int64 .or. tseg64 >= int(msg%nsegs, int64)) return
            tseg = int(tseg64) + 1
            padoff = wp_far_off(cw)*8_int64
            if (wp_far_two(cw)) then
               if (.not. span_ok(msg, tseg, padoff, 16_int64)) return
               w0 = cp_get_i64(msg%segs(tseg)%bytes, padoff)
               tag = cp_get_i64(msg%segs(tseg)%bytes, padoff + 8_int64)
               if (wp_kind(w0) == CAPNP_WK_FAR) then
                  oseg64 = wp_far_seg(w0)
                  if (oseg64 >= 0_int64 .and. oseg64 < int(msg%nsegs, int64)) then
                     oseg = int(oseg64) + 1
                     call zero_at(msg, oseg, wp_far_off(w0)*8_int64, tag, depth)
                  end if
               end if
               call zero_bytes(msg, tseg, padoff, 16_int64)
               return
            end if
            if (.not. span_ok(msg, tseg, padoff, 8_int64)) return
            pad = cp_get_i64(msg%segs(tseg)%bytes, padoff)
            if (wp_kind(pad) == CAPNP_WK_FAR) then
               call zero_bytes(msg, tseg, padoff, 8_int64)
               cw = pad
               cycle
            end if
            if (pad /= 0_int64 .and. &
                (wp_kind(pad) == CAPNP_WK_STRUCT .or. wp_kind(pad) == CAPNP_WK_LIST)) then
               base = padoff + 8_int64 + int(wp_offset(pad), int64)*8_int64
               call zero_at(msg, tseg, base, pad, depth)
            end if
            call zero_bytes(msg, tseg, padoff, 8_int64)
            return
         end do
      end select
   end subroutine zero_pointer_word

   !> Byte range of the object a pointer word addresses (far pads excluded).
   subroutine locate_body(msg, seg, slot, w, oseg, obase, onbytes, ok)
      type(capnp_message_t), intent(in) :: msg
      integer, intent(in) :: seg
      integer(int64), intent(in) :: slot, w
      integer, intent(out) :: oseg
      integer(int64), intent(out) :: obase, onbytes
      logical, intent(out) :: ok
      integer :: tseg
      integer(int64) :: padoff, w0, tag, tseg64, oseg64
      ok = .false.
      oseg = 0
      obase = 0_int64
      onbytes = 0_int64
      if (w == 0_int64) return
      select case (wp_kind(w))
      case (CAPNP_WK_STRUCT)
         oseg = seg
         obase = slot + 8_int64 + int(wp_offset(w), int64)*8_int64
         onbytes = int(wp_struct_dwords(w) + wp_struct_pwords(w), int64)*8_int64
         ok = .true.
      case (CAPNP_WK_LIST)
         oseg = seg
         obase = slot + 8_int64 + int(wp_offset(w), int64)*8_int64
         onbytes = list_payload_bytes(w)
         ok = onbytes >= 0_int64
      case (CAPNP_WK_FAR)
         tseg64 = wp_far_seg(w)
         if (tseg64 < 0_int64 .or. tseg64 >= int(msg%nsegs, int64)) return
         tseg = int(tseg64) + 1
         padoff = wp_far_off(w)*8_int64
         if (.not. wp_far_two(w)) then
            if (.not. span_ok(msg, tseg, padoff, 8_int64)) return
            tag = cp_get_i64(msg%segs(tseg)%bytes, padoff)
            if (tag == 0_int64 .or. wp_kind(tag) == CAPNP_WK_FAR) return
            oseg = tseg
            obase = padoff + 8_int64 + int(wp_offset(tag), int64)*8_int64
            if (wp_kind(tag) == CAPNP_WK_STRUCT) then
               onbytes = int(wp_struct_dwords(tag) + wp_struct_pwords(tag), int64)*8_int64
            else if (wp_kind(tag) == CAPNP_WK_LIST) then
               onbytes = list_payload_bytes(tag)
            else
               return
            end if
            ok = onbytes >= 0_int64
         else
            if (.not. span_ok(msg, tseg, padoff, 16_int64)) return
            w0 = cp_get_i64(msg%segs(tseg)%bytes, padoff)
            tag = cp_get_i64(msg%segs(tseg)%bytes, padoff + 8_int64)
            if (wp_kind(w0) /= CAPNP_WK_FAR) return
            oseg64 = wp_far_seg(w0)
            if (oseg64 < 0_int64 .or. oseg64 >= int(msg%nsegs, int64)) return
            oseg = int(oseg64) + 1
            obase = wp_far_off(w0)*8_int64
            if (wp_kind(tag) == CAPNP_WK_STRUCT) then
               onbytes = int(wp_struct_dwords(tag) + wp_struct_pwords(tag), int64)*8_int64
            else if (wp_kind(tag) == CAPNP_WK_LIST) then
               onbytes = list_payload_bytes(tag)
            else
               return
            end if
            ok = onbytes >= 0_int64
         end if
      end select
   end subroutine locate_body

   !> Byte range of an object handle already in hand.
   subroutine locate_handle(q, oseg, obase, onbytes, ok)
      type(capnp_ptr_t), intent(in) :: q
      integer, intent(out) :: oseg
      integer(int64), intent(out) :: obase, onbytes
      logical, intent(out) :: ok
      integer :: sb
      ok = .false.
      oseg = q%seg
      obase = q%off
      onbytes = 0_int64
      select case (q%kind)
      case (CAPNP_PK_STRUCT)
         onbytes = int(q%dwords + q%pwords, int64)*8_int64
         ok = .true.
      case (CAPNP_PK_LIST)
         if (q%esize == CAPNP_SZ_COMPOSITE) then
            obase = q%off - 8_int64
            onbytes = 8_int64 + q%nelem*int(q%dwords + q%pwords, int64)*8_int64
         else if (q%esize == CAPNP_SZ_PTR) then
            onbytes = q%nelem*8_int64
         else if (q%esize == CAPNP_SZ_VOID) then
            onbytes = 0_int64
         else
            sb = capnp_list_step_bits(q%esize)
            if (sb < 0) return
            onbytes = ((int(sb, int64)*q%nelem + 63_int64)/64_int64)*8_int64
         end if
         ok = .true.
      end select
   end subroutine locate_handle

   !> Zero the object currently addressed by a slot, unless `q` is that
   !> object (or lives inside it). layout.c++ allocate() line 485.
   subroutine zero_slot_target(msg, seg, slot, q)
      type(capnp_message_t), intent(inout), target :: msg
      integer, intent(in) :: seg
      integer(int64), intent(in) :: slot
      type(capnp_ptr_t), intent(in) :: q
      integer :: oseg, qseg
      integer(int64) :: w, obase, onbytes, qbase, qnbytes
      logical :: ok, qok
      if (.not. span_ok(msg, seg, slot, 8_int64)) return
      w = cp_get_i64(msg%segs(seg)%bytes, slot)
      if (w == 0_int64) return
      call locate_body(msg, seg, slot, w, oseg, obase, onbytes, ok)
      if (ok .and. associated(q%msg, msg)) then
         call locate_handle(q, qseg, qbase, qnbytes, qok)
         if (qok .and. qseg == oseg .and. onbytes > 0_int64 .and. qnbytes > 0_int64) then
            if (obase < qbase + qnbytes .and. qbase < obase + onbytes) return
         end if
      end if
      call zero_pointer_word(msg, seg, slot, w, 0)
   end subroutine zero_slot_target

   recursive subroutine write_ptr_slot(msg, sseg, slot, q, err)
      type(capnp_message_t), intent(inout), target :: msg
      integer, intent(in) :: sseg
      integer(int64), intent(in) :: slot
      type(capnp_ptr_t), intent(in) :: q
      integer, intent(out) :: err
      integer(int64) :: pad_off
      err = CAPNP_OK
      if (.not. msg%is_builder) then
         err = CAPNP_ERR_ARG
         return
      end if
      ! Drop the previous object before the slot word changes, matching
      ! layout.c++ allocate() which calls zeroObject on a live pointer.
      call zero_slot_target(msg, sseg, slot, q)
      if (q%kind == CAPNP_PK_NULL .or. .not. associated(q%msg)) then
         call cp_put_i64(msg%segs(sseg)%bytes, slot, 0_int64)
         return
      end if
      if (.not. associated(q%msg, msg)) then
         ! Cross-message reference: deep-copy the object into this message
         ! (capnp C++ set() semantics), then link locally.
         call cp_put_i64(msg%segs(sseg)%bytes, slot, 0_int64)
         block
            type(capnp_ptr_t) :: c
            c = capnp_copy(msg, q, err)
            if (err /= CAPNP_OK) return
            call write_ptr_slot(msg, sseg, slot, c, err)
         end block
         return
      end if
      if (q%kind == CAPNP_PK_CAP) then
         call cp_put_i64(msg%segs(sseg)%bytes, slot, wp_make_cap(q%capidx))
         return
      end if
      if (q%seg == sseg) then
         call cp_put_i64(msg%segs(sseg)%bytes, slot, local_ptr_word(q, slot/8_int64))
      else
         ! Landing pad in q's segment holds the local pointer; the slot gets
         ! a single far pointer to the pad. Wire segment ids are 0-based.
         call capnp_arena_alloc_in(msg, q%seg, 1_int64, pad_off, err)
         if (err /= CAPNP_OK) return
         call cp_put_i64(msg%segs(q%seg)%bytes, pad_off, local_ptr_word(q, pad_off/8_int64))
         call cp_put_i64(msg%segs(sseg)%bytes, slot, &
                         wp_make_far(.false., pad_off/8_int64, int(q%seg - 1, int64)))
      end if
   end subroutine write_ptr_slot

   !> Byte address of element i viewed as a want_bits scalar. A
   !> matching-width primitive list addresses the element directly; a
   !> composite list downgrades to field @0 of each element (list-upgrade
   !> rules); anything else is a kind error.
   function list_elem_base(p, i, want_bits, err) result(base)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: i
      integer, intent(in) :: want_bits
      integer, intent(out) :: err
      integer(int64) :: base
      err = CAPNP_OK
      base = 0_int64
      if (p%kind /= CAPNP_PK_LIST .or. .not. associated(p%msg)) then
         err = CAPNP_ERR_KIND
         return
      end if
      if (i < 0_int64 .or. i >= p%nelem) then
         err = CAPNP_ERR_BOUNDS
         return
      end if
      if (capnp_list_step_bits(p%esize) == want_bits .and. p%esize /= CAPNP_SZ_PTR) then
         base = p%off + i*int(want_bits/8, int64)
      else if (p%esize == CAPNP_SZ_COMPOSITE .and. p%dwords >= 1) then
         base = p%off + i*int(p%dwords + p%pwords, int64)*8_int64
      else
         err = CAPNP_ERR_KIND
      end if
   end function list_elem_base

   ! ------------------------------------------------------------------
   ! Primitive struct fields. Reads past the data section return the
   ! default (spec: short structs from older schema versions). Writes past
   ! the data section are errors. Defaults XOR onto the wire.
   ! ------------------------------------------------------------------

   function capnp_get_i8 (p, off, default) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: off
      integer(int8), intent(in), optional :: default
      integer(int8) :: v, d
      d = 0_int8
      if (present(default)) d = default
      v = d
      if (p%kind /= CAPNP_PK_STRUCT .or. .not. associated(p%msg)) return
      if (off < 0_int64 .or. (off + 1_int64)*8_int64 > data_bits(p)) return
      v = ieor(cp_get_i8 (p%msg%segs(p%seg)%bytes, p%off + off), d)
   end function capnp_get_i8

   subroutine capnp_set_i8 (p, off, v, err, default)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: off
      integer(int8), intent(in) :: v
      integer, intent(out) :: err
      integer(int8), intent(in), optional :: default
      integer(int8) :: d
      err = CAPNP_OK
      if (p%frozen) then
         err = CAPNP_ERR_ARG
         return
      end if
      d = 0_int8
      if (present(default)) d = default
      if (p%kind /= CAPNP_PK_STRUCT .or. .not. associated(p%msg)) then
         err = CAPNP_ERR_KIND
         return
      end if
      if (off < 0_int64 .or. (off + 1_int64)*8_int64 > data_bits(p)) then
         err = CAPNP_ERR_BOUNDS
         return
      end if
      call cp_put_i8 (p%msg%segs(p%seg)%bytes, p%off + off, ieor(v, d))
   end subroutine capnp_set_i8

   function capnp_list_get_i8 (p, i, err) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: i
      integer, intent(out) :: err
      integer(int8) :: v
      integer(int64) :: base
      err = CAPNP_OK
      v = 0_int8
      base = list_elem_base(p, i, 8, err)
      if (err /= CAPNP_OK) return
      v = cp_get_i8 (p%msg%segs(p%seg)%bytes, base)
   end function capnp_list_get_i8

   subroutine capnp_list_set_i8 (p, i, v, err)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: i
      integer(int8), intent(in) :: v
      integer, intent(out) :: err
      integer(int64) :: base
      err = CAPNP_OK
      if (p%frozen) then
         err = CAPNP_ERR_ARG
         return
      end if
      base = list_elem_base(p, i, 8, err)
      if (err /= CAPNP_OK) return
      call cp_put_i8 (p%msg%segs(p%seg)%bytes, base, v)
   end subroutine capnp_list_set_i8

   !> Whole-list read (capn_getv parity). arr is allocated to the length.
   subroutine capnp_list_get_all_i8 (p, arr, err)
      type(capnp_ptr_t), intent(in) :: p
      integer(int8), allocatable, intent(out) :: arr(:)
      integer, intent(out) :: err
      integer(int64) :: i
      err = CAPNP_OK
      allocate (arr(0:max(capnp_list_len(p), 1_int64) - 1))
      if (capnp_list_len(p) == 0_int64) then
         deallocate (arr)
         allocate (arr(0))
         return
      end if
      do i = 0_int64, capnp_list_len(p) - 1_int64
         arr(i) = capnp_list_get_i8 (p, i, err)
         if (err /= CAPNP_OK) return
      end do
   end subroutine capnp_list_get_all_i8

   !> Whole-list write (capn_setv parity). size(arr) must equal the length.
   subroutine capnp_list_set_all_i8 (p, arr, err)
      type(capnp_ptr_t), intent(in) :: p
      integer(int8), intent(in) :: arr(0:)
      integer, intent(out) :: err
      integer(int64) :: i
      err = CAPNP_OK
      if (size(arr, kind=int64) /= capnp_list_len(p)) then
         err = CAPNP_ERR_ARG
         return
      end if
      do i = 0_int64, capnp_list_len(p) - 1_int64
         call capnp_list_set_i8 (p, i, arr(i), err)
         if (err /= CAPNP_OK) return
      end do
   end subroutine capnp_list_set_all_i8

   function capnp_get_i16 (p, off, default) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: off
      integer(int16), intent(in), optional :: default
      integer(int16) :: v, d
      d = 0_int16
      if (present(default)) d = default
      v = d
      if (p%kind /= CAPNP_PK_STRUCT .or. .not. associated(p%msg)) return
      if (off < 0_int64 .or. (off + 2_int64)*8_int64 > data_bits(p)) return
      v = ieor(cp_get_i16 (p%msg%segs(p%seg)%bytes, p%off + off), d)
   end function capnp_get_i16

   subroutine capnp_set_i16 (p, off, v, err, default)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: off
      integer(int16), intent(in) :: v
      integer, intent(out) :: err
      integer(int16), intent(in), optional :: default
      integer(int16) :: d
      err = CAPNP_OK
      if (p%frozen) then
         err = CAPNP_ERR_ARG
         return
      end if
      d = 0_int16
      if (present(default)) d = default
      if (p%kind /= CAPNP_PK_STRUCT .or. .not. associated(p%msg)) then
         err = CAPNP_ERR_KIND
         return
      end if
      if (off < 0_int64 .or. (off + 2_int64)*8_int64 > data_bits(p)) then
         err = CAPNP_ERR_BOUNDS
         return
      end if
      call cp_put_i16 (p%msg%segs(p%seg)%bytes, p%off + off, ieor(v, d))
   end subroutine capnp_set_i16

   function capnp_list_get_i16 (p, i, err) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: i
      integer, intent(out) :: err
      integer(int16) :: v
      integer(int64) :: base
      err = CAPNP_OK
      v = 0_int16
      base = list_elem_base(p, i, 16, err)
      if (err /= CAPNP_OK) return
      v = cp_get_i16 (p%msg%segs(p%seg)%bytes, base)
   end function capnp_list_get_i16

   subroutine capnp_list_set_i16 (p, i, v, err)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: i
      integer(int16), intent(in) :: v
      integer, intent(out) :: err
      integer(int64) :: base
      err = CAPNP_OK
      if (p%frozen) then
         err = CAPNP_ERR_ARG
         return
      end if
      base = list_elem_base(p, i, 16, err)
      if (err /= CAPNP_OK) return
      call cp_put_i16 (p%msg%segs(p%seg)%bytes, base, v)
   end subroutine capnp_list_set_i16

   !> Whole-list read (capn_getv parity). arr is allocated to the length.
   subroutine capnp_list_get_all_i16 (p, arr, err)
      type(capnp_ptr_t), intent(in) :: p
      integer(int16), allocatable, intent(out) :: arr(:)
      integer, intent(out) :: err
      integer(int64) :: i
      err = CAPNP_OK
      allocate (arr(0:max(capnp_list_len(p), 1_int64) - 1))
      if (capnp_list_len(p) == 0_int64) then
         deallocate (arr)
         allocate (arr(0))
         return
      end if
      do i = 0_int64, capnp_list_len(p) - 1_int64
         arr(i) = capnp_list_get_i16 (p, i, err)
         if (err /= CAPNP_OK) return
      end do
   end subroutine capnp_list_get_all_i16

   !> Whole-list write (capn_setv parity). size(arr) must equal the length.
   subroutine capnp_list_set_all_i16 (p, arr, err)
      type(capnp_ptr_t), intent(in) :: p
      integer(int16), intent(in) :: arr(0:)
      integer, intent(out) :: err
      integer(int64) :: i
      err = CAPNP_OK
      if (size(arr, kind=int64) /= capnp_list_len(p)) then
         err = CAPNP_ERR_ARG
         return
      end if
      do i = 0_int64, capnp_list_len(p) - 1_int64
         call capnp_list_set_i16 (p, i, arr(i), err)
         if (err /= CAPNP_OK) return
      end do
   end subroutine capnp_list_set_all_i16

   function capnp_get_i32 (p, off, default) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: off
      integer(int32), intent(in), optional :: default
      integer(int32) :: v, d
      d = 0_int32
      if (present(default)) d = default
      v = d
      if (p%kind /= CAPNP_PK_STRUCT .or. .not. associated(p%msg)) return
      if (off < 0_int64 .or. (off + 4_int64)*8_int64 > data_bits(p)) return
      v = ieor(cp_get_i32 (p%msg%segs(p%seg)%bytes, p%off + off), d)
   end function capnp_get_i32

   subroutine capnp_set_i32 (p, off, v, err, default)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: off
      integer(int32), intent(in) :: v
      integer, intent(out) :: err
      integer(int32), intent(in), optional :: default
      integer(int32) :: d
      err = CAPNP_OK
      if (p%frozen) then
         err = CAPNP_ERR_ARG
         return
      end if
      d = 0_int32
      if (present(default)) d = default
      if (p%kind /= CAPNP_PK_STRUCT .or. .not. associated(p%msg)) then
         err = CAPNP_ERR_KIND
         return
      end if
      if (off < 0_int64 .or. (off + 4_int64)*8_int64 > data_bits(p)) then
         err = CAPNP_ERR_BOUNDS
         return
      end if
      call cp_put_i32 (p%msg%segs(p%seg)%bytes, p%off + off, ieor(v, d))
   end subroutine capnp_set_i32

   function capnp_list_get_i32 (p, i, err) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: i
      integer, intent(out) :: err
      integer(int32) :: v
      integer(int64) :: base
      err = CAPNP_OK
      v = 0_int32
      base = list_elem_base(p, i, 32, err)
      if (err /= CAPNP_OK) return
      v = cp_get_i32 (p%msg%segs(p%seg)%bytes, base)
   end function capnp_list_get_i32

   subroutine capnp_list_set_i32 (p, i, v, err)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: i
      integer(int32), intent(in) :: v
      integer, intent(out) :: err
      integer(int64) :: base
      err = CAPNP_OK
      if (p%frozen) then
         err = CAPNP_ERR_ARG
         return
      end if
      base = list_elem_base(p, i, 32, err)
      if (err /= CAPNP_OK) return
      call cp_put_i32 (p%msg%segs(p%seg)%bytes, base, v)
   end subroutine capnp_list_set_i32

   !> Whole-list read (capn_getv parity). arr is allocated to the length.
   subroutine capnp_list_get_all_i32 (p, arr, err)
      type(capnp_ptr_t), intent(in) :: p
      integer(int32), allocatable, intent(out) :: arr(:)
      integer, intent(out) :: err
      integer(int64) :: i
      err = CAPNP_OK
      allocate (arr(0:max(capnp_list_len(p), 1_int64) - 1))
      if (capnp_list_len(p) == 0_int64) then
         deallocate (arr)
         allocate (arr(0))
         return
      end if
      do i = 0_int64, capnp_list_len(p) - 1_int64
         arr(i) = capnp_list_get_i32 (p, i, err)
         if (err /= CAPNP_OK) return
      end do
   end subroutine capnp_list_get_all_i32

   !> Whole-list write (capn_setv parity). size(arr) must equal the length.
   subroutine capnp_list_set_all_i32 (p, arr, err)
      type(capnp_ptr_t), intent(in) :: p
      integer(int32), intent(in) :: arr(0:)
      integer, intent(out) :: err
      integer(int64) :: i
      err = CAPNP_OK
      if (size(arr, kind=int64) /= capnp_list_len(p)) then
         err = CAPNP_ERR_ARG
         return
      end if
      do i = 0_int64, capnp_list_len(p) - 1_int64
         call capnp_list_set_i32 (p, i, arr(i), err)
         if (err /= CAPNP_OK) return
      end do
   end subroutine capnp_list_set_all_i32

   function capnp_get_i64 (p, off, default) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: off
      integer(int64), intent(in), optional :: default
      integer(int64) :: v, d
      d = 0_int64
      if (present(default)) d = default
      v = d
      if (p%kind /= CAPNP_PK_STRUCT .or. .not. associated(p%msg)) return
      if (off < 0_int64 .or. (off + 8_int64)*8_int64 > data_bits(p)) return
      v = ieor(cp_get_i64 (p%msg%segs(p%seg)%bytes, p%off + off), d)
   end function capnp_get_i64

   subroutine capnp_set_i64 (p, off, v, err, default)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: off
      integer(int64), intent(in) :: v
      integer, intent(out) :: err
      integer(int64), intent(in), optional :: default
      integer(int64) :: d
      err = CAPNP_OK
      if (p%frozen) then
         err = CAPNP_ERR_ARG
         return
      end if
      d = 0_int64
      if (present(default)) d = default
      if (p%kind /= CAPNP_PK_STRUCT .or. .not. associated(p%msg)) then
         err = CAPNP_ERR_KIND
         return
      end if
      if (off < 0_int64 .or. (off + 8_int64)*8_int64 > data_bits(p)) then
         err = CAPNP_ERR_BOUNDS
         return
      end if
      call cp_put_i64 (p%msg%segs(p%seg)%bytes, p%off + off, ieor(v, d))
   end subroutine capnp_set_i64

   function capnp_list_get_i64 (p, i, err) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: i
      integer, intent(out) :: err
      integer(int64) :: v
      integer(int64) :: base
      err = CAPNP_OK
      v = 0_int64
      base = list_elem_base(p, i, 64, err)
      if (err /= CAPNP_OK) return
      v = cp_get_i64 (p%msg%segs(p%seg)%bytes, base)
   end function capnp_list_get_i64

   subroutine capnp_list_set_i64 (p, i, v, err)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: i
      integer(int64), intent(in) :: v
      integer, intent(out) :: err
      integer(int64) :: base
      err = CAPNP_OK
      if (p%frozen) then
         err = CAPNP_ERR_ARG
         return
      end if
      base = list_elem_base(p, i, 64, err)
      if (err /= CAPNP_OK) return
      call cp_put_i64 (p%msg%segs(p%seg)%bytes, base, v)
   end subroutine capnp_list_set_i64

   !> Whole-list read (capn_getv parity). arr is allocated to the length.
   subroutine capnp_list_get_all_i64 (p, arr, err)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), allocatable, intent(out) :: arr(:)
      integer, intent(out) :: err
      integer(int64) :: i
      err = CAPNP_OK
      allocate (arr(0:max(capnp_list_len(p), 1_int64) - 1))
      if (capnp_list_len(p) == 0_int64) then
         deallocate (arr)
         allocate (arr(0))
         return
      end if
      do i = 0_int64, capnp_list_len(p) - 1_int64
         arr(i) = capnp_list_get_i64 (p, i, err)
         if (err /= CAPNP_OK) return
      end do
   end subroutine capnp_list_get_all_i64

   !> Whole-list write (capn_setv parity). size(arr) must equal the length.
   subroutine capnp_list_set_all_i64 (p, arr, err)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: arr(0:)
      integer, intent(out) :: err
      integer(int64) :: i
      err = CAPNP_OK
      if (size(arr, kind=int64) /= capnp_list_len(p)) then
         err = CAPNP_ERR_ARG
         return
      end if
      do i = 0_int64, capnp_list_len(p) - 1_int64
         call capnp_list_set_i64 (p, i, arr(i), err)
         if (err /= CAPNP_OK) return
      end do
   end subroutine capnp_list_set_all_i64

   ! Unsigned views: same wire bytes, widened into the next signed kind so
   ! the full unsigned range is representable.
   function capnp_get_u8 (p, off, default) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: off
      integer(int16), intent(in), optional :: default
      integer(int16) :: v, d
      d = 0_int16
      if (present(default)) d = default
      v = d
      if (p%kind /= CAPNP_PK_STRUCT .or. .not. associated(p%msg)) return
      if (off < 0_int64 .or. (off + 1_int64)*8_int64 > data_bits(p)) return
      v = ieor(iand(int(cp_get_i8 (p%msg%segs(p%seg)%bytes, p%off + off), int16), &
                    255_int16), d)
   end function capnp_get_u8

   subroutine capnp_set_u8 (p, off, v, err, default)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: off
      integer(int16), intent(in) :: v
      integer, intent(out) :: err
      integer(int16), intent(in), optional :: default
      integer(int16) :: d, x
      integer(int64) :: u
      err = CAPNP_OK
      if (p%frozen) then
         err = CAPNP_ERR_ARG
         return
      end if
      d = 0_int16
      if (present(default)) d = default
      if (p%kind /= CAPNP_PK_STRUCT .or. .not. associated(p%msg)) then
         err = CAPNP_ERR_KIND
         return
      end if
      if (off < 0_int64 .or. (off + 1_int64)*8_int64 > data_bits(p)) then
         err = CAPNP_ERR_BOUNDS
         return
      end if
      x = iand(ieor(v, d), 255_int16)
      u = int(x, int64)
      if (u > 127_int64) u = u - 256_int64
      call cp_put_i8 (p%msg%segs(p%seg)%bytes, p%off + off, int(u, int8))
   end subroutine capnp_set_u8

   function capnp_get_u16 (p, off, default) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: off
      integer(int32), intent(in), optional :: default
      integer(int32) :: v, d
      d = 0_int32
      if (present(default)) d = default
      v = d
      if (p%kind /= CAPNP_PK_STRUCT .or. .not. associated(p%msg)) return
      if (off < 0_int64 .or. (off + 2_int64)*8_int64 > data_bits(p)) return
      v = ieor(iand(int(cp_get_i16 (p%msg%segs(p%seg)%bytes, p%off + off), int32), &
                    65535_int32), d)
   end function capnp_get_u16

   subroutine capnp_set_u16 (p, off, v, err, default)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: off
      integer(int32), intent(in) :: v
      integer, intent(out) :: err
      integer(int32), intent(in), optional :: default
      integer(int32) :: d, x
      integer(int64) :: u
      err = CAPNP_OK
      if (p%frozen) then
         err = CAPNP_ERR_ARG
         return
      end if
      d = 0_int32
      if (present(default)) d = default
      if (p%kind /= CAPNP_PK_STRUCT .or. .not. associated(p%msg)) then
         err = CAPNP_ERR_KIND
         return
      end if
      if (off < 0_int64 .or. (off + 2_int64)*8_int64 > data_bits(p)) then
         err = CAPNP_ERR_BOUNDS
         return
      end if
      x = iand(ieor(v, d), 65535_int32)
      u = int(x, int64)
      if (u > 32767_int64) u = u - 65536_int64
      call cp_put_i16 (p%msg%segs(p%seg)%bytes, p%off + off, int(u, int16))
   end subroutine capnp_set_u16

   function capnp_get_u32 (p, off, default) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: off
      integer(int64), intent(in), optional :: default
      integer(int64) :: v, d
      d = 0_int64
      if (present(default)) d = default
      v = d
      if (p%kind /= CAPNP_PK_STRUCT .or. .not. associated(p%msg)) return
      if (off < 0_int64 .or. (off + 4_int64)*8_int64 > data_bits(p)) return
      v = ieor(iand(int(cp_get_i32 (p%msg%segs(p%seg)%bytes, p%off + off), int64), &
                    4294967295_int64), d)
   end function capnp_get_u32

   subroutine capnp_set_u32 (p, off, v, err, default)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: off
      integer(int64), intent(in) :: v
      integer, intent(out) :: err
      integer(int64), intent(in), optional :: default
      integer(int64) :: d, x
      integer(int64) :: u
      err = CAPNP_OK
      if (p%frozen) then
         err = CAPNP_ERR_ARG
         return
      end if
      d = 0_int64
      if (present(default)) d = default
      if (p%kind /= CAPNP_PK_STRUCT .or. .not. associated(p%msg)) then
         err = CAPNP_ERR_KIND
         return
      end if
      if (off < 0_int64 .or. (off + 4_int64)*8_int64 > data_bits(p)) then
         err = CAPNP_ERR_BOUNDS
         return
      end if
      x = iand(ieor(v, d), 4294967295_int64)
      u = int(x, int64)
      if (u > 2147483647_int64) u = u - 4294967296_int64
      call cp_put_i32 (p%msg%segs(p%seg)%bytes, p%off + off, int(u, int32))
   end subroutine capnp_set_u32

   ! Floats: default XOR applies to the IEEE bit pattern.
   function capnp_get_f32 (p, off, default) result(x)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: off
      real(real32), intent(in), optional :: default
      real(real32) :: x
      integer(int32) :: d
      d = 0_int32
      if (present(default)) d = cp_f32_bits(default)
      x = cp_bits_f32 (d)
      if (p%kind /= CAPNP_PK_STRUCT .or. .not. associated(p%msg)) return
      if (off < 0_int64 .or. (off + 4_int64)*8_int64 > data_bits(p)) return
      x = cp_bits_f32 (ieor(cp_get_i32 (p%msg%segs(p%seg)%bytes, p%off + off), d))
   end function capnp_get_f32

   subroutine capnp_set_f32 (p, off, x, err, default)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: off
      real(real32), intent(in) :: x
      integer, intent(out) :: err
      real(real32), intent(in), optional :: default
      integer(int32) :: d
      err = CAPNP_OK
      if (p%frozen) then
         err = CAPNP_ERR_ARG
         return
      end if
      d = 0_int32
      if (present(default)) d = cp_f32_bits(default)
      if (p%kind /= CAPNP_PK_STRUCT .or. .not. associated(p%msg)) then
         err = CAPNP_ERR_KIND
         return
      end if
      if (off < 0_int64 .or. (off + 4_int64)*8_int64 > data_bits(p)) then
         err = CAPNP_ERR_BOUNDS
         return
      end if
      call cp_put_i32 (p%msg%segs(p%seg)%bytes, p%off + off, ieor(cp_f32_bits(x), d))
   end subroutine capnp_set_f32

   function capnp_list_get_f32 (p, i, err) result(x)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: i
      integer, intent(out) :: err
      real(real32) :: x
      x = cp_bits_f32 (capnp_list_get_i32 (p, i, err))
   end function capnp_list_get_f32

   subroutine capnp_list_set_f32 (p, i, x, err)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: i
      real(real32), intent(in) :: x
      integer, intent(out) :: err
      call capnp_list_set_i32 (p, i, cp_f32_bits(x), err)
   end subroutine capnp_list_set_f32

   function capnp_get_f64 (p, off, default) result(x)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: off
      real(real64), intent(in), optional :: default
      real(real64) :: x
      integer(int64) :: d
      d = 0_int64
      if (present(default)) d = cp_f64_bits(default)
      x = cp_bits_f64 (d)
      if (p%kind /= CAPNP_PK_STRUCT .or. .not. associated(p%msg)) return
      if (off < 0_int64 .or. (off + 8_int64)*8_int64 > data_bits(p)) return
      x = cp_bits_f64 (ieor(cp_get_i64 (p%msg%segs(p%seg)%bytes, p%off + off), d))
   end function capnp_get_f64

   subroutine capnp_set_f64 (p, off, x, err, default)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: off
      real(real64), intent(in) :: x
      integer, intent(out) :: err
      real(real64), intent(in), optional :: default
      integer(int64) :: d
      err = CAPNP_OK
      if (p%frozen) then
         err = CAPNP_ERR_ARG
         return
      end if
      d = 0_int64
      if (present(default)) d = cp_f64_bits(default)
      if (p%kind /= CAPNP_PK_STRUCT .or. .not. associated(p%msg)) then
         err = CAPNP_ERR_KIND
         return
      end if
      if (off < 0_int64 .or. (off + 8_int64)*8_int64 > data_bits(p)) then
         err = CAPNP_ERR_BOUNDS
         return
      end if
      call cp_put_i64 (p%msg%segs(p%seg)%bytes, p%off + off, ieor(cp_f64_bits(x), d))
   end subroutine capnp_set_f64

   function capnp_list_get_f64 (p, i, err) result(x)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: i
      integer, intent(out) :: err
      real(real64) :: x
      x = cp_bits_f64 (capnp_list_get_i64 (p, i, err))
   end function capnp_list_get_f64

   subroutine capnp_list_set_f64 (p, i, x, err)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: i
      real(real64), intent(in) :: x
      integer, intent(out) :: err
      call capnp_list_set_i64 (p, i, cp_f64_bits(x), err)
   end subroutine capnp_list_set_f64

   ! ------------------------------------------------------------------
   ! Bools: struct field offsets count in bits; bit lists likewise.
   ! ------------------------------------------------------------------

   function capnp_get_bool(p, bit_off, default) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: bit_off
      logical, intent(in), optional :: default
      logical :: v, d
      integer(int64) :: byte
      integer :: bit
      d = .false.
      if (present(default)) d = default
      v = d
      if (p%kind /= CAPNP_PK_STRUCT .or. .not. associated(p%msg)) return
      if (bit_off < 0_int64 .or. bit_off >= data_bits(p)) return
      byte = p%off + bit_off/8_int64
      bit = int(mod(bit_off, 8_int64))
      v = btest(p%msg%segs(p%seg)%bytes(byte), bit) .neqv. d
   end function capnp_get_bool

   subroutine capnp_set_bool(p, bit_off, v, err, default)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: bit_off
      logical, intent(in) :: v
      integer, intent(out) :: err
      logical, intent(in), optional :: default
      logical :: d, w
      integer(int64) :: byte
      integer :: bit
      err = CAPNP_OK
      if (p%frozen) then
         err = CAPNP_ERR_ARG
         return
      end if
      d = .false.
      if (present(default)) d = default
      if (p%kind /= CAPNP_PK_STRUCT .or. .not. associated(p%msg)) then
         err = CAPNP_ERR_KIND
         return
      end if
      if (bit_off < 0_int64 .or. bit_off >= data_bits(p)) then
         err = CAPNP_ERR_BOUNDS
         return
      end if
      byte = p%off + bit_off/8_int64
      bit = int(mod(bit_off, 8_int64))
      w = v .neqv. d
      if (w) then
         p%msg%segs(p%seg)%bytes(byte) = ibset(p%msg%segs(p%seg)%bytes(byte), bit)
      else
         p%msg%segs(p%seg)%bytes(byte) = ibclr(p%msg%segs(p%seg)%bytes(byte), bit)
      end if
   end subroutine capnp_set_bool

   function capnp_list_get_bool(p, i, err) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: i
      integer, intent(out) :: err
      logical :: v
      integer(int64) :: byte
      err = CAPNP_OK
      v = .false.
      if (p%kind /= CAPNP_PK_LIST .or. p%esize /= CAPNP_SZ_BIT .or. &
          .not. associated(p%msg)) then
         err = CAPNP_ERR_KIND
         return
      end if
      if (i < 0_int64 .or. i >= p%nelem) then
         err = CAPNP_ERR_BOUNDS
         return
      end if
      byte = p%off + i/8_int64
      v = btest(p%msg%segs(p%seg)%bytes(byte), int(mod(i, 8_int64)))
   end function capnp_list_get_bool

   subroutine capnp_list_set_bool(p, i, v, err)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: i
      logical, intent(in) :: v
      integer, intent(out) :: err
      integer(int64) :: byte
      err = CAPNP_OK
      if (p%frozen) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (p%kind /= CAPNP_PK_LIST .or. p%esize /= CAPNP_SZ_BIT .or. &
          .not. associated(p%msg)) then
         err = CAPNP_ERR_KIND
         return
      end if
      if (i < 0_int64 .or. i >= p%nelem) then
         err = CAPNP_ERR_BOUNDS
         return
      end if
      byte = p%off + i/8_int64
      if (v) then
         p%msg%segs(p%seg)%bytes(byte) = ibset(p%msg%segs(p%seg)%bytes(byte), int(mod(i, 8_int64)))
      else
         p%msg%segs(p%seg)%bytes(byte) = ibclr(p%msg%segs(p%seg)%bytes(byte), int(mod(i, 8_int64)))
      end if
   end subroutine capnp_list_set_bool

   ! ------------------------------------------------------------------
   ! Text and Data. Text is a byte list whose count includes a trailing
   ! NUL; Data is a bare byte list.
   ! ------------------------------------------------------------------

   !> Text bytes include a trailing NUL. A missing NUL is malformed.
   function text_has_nul(q) result(ok)
      type(capnp_ptr_t), intent(in) :: q
      logical :: ok
      integer(int64) :: last
      ok = .false.
      if (.not. associated(q%msg)) return
      if (q%nelem < 1_int64) return
      last = q%off + q%nelem - 1_int64
      ok = cp_u8(q%msg%segs(q%seg)%bytes(last)) == 0
   end function text_has_nul

   !> Text at pointer field i of p. Missing/null yields an empty string with
   !> err = CAPNP_OK, matching default-text-is-empty semantics.
   !> A byte list whose last byte is not NUL is malformed text.
   subroutine capnp_get_text(p, i, str, err)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(in) :: i
      character(len=:), allocatable, intent(out) :: str
      integer, intent(out) :: err
      type(capnp_ptr_t) :: q
      integer(int64) :: n, j
      str = ''
      q = capnp_getp(p, i, err)
      if (err /= CAPNP_OK .or. q%kind == CAPNP_PK_NULL) return
      if (q%kind /= CAPNP_PK_LIST .or. q%esize /= CAPNP_SZ_BYTE .or. q%nelem < 1_int64) then
         err = CAPNP_ERR_KIND
         return
      end if
      if (.not. text_has_nul(q)) then
         err = CAPNP_ERR_KIND
         return
      end if
      n = q%nelem - 1_int64 ! trailing NUL
      if (n <= 0_int64) return
      deallocate (str)
      allocate (character(len=n) :: str)
      do j = 0_int64, n - 1_int64
         str(j + 1:j + 1) = char(cp_u8(q%msg%segs(q%seg)%bytes(q%off + j)))
      end do
   end subroutine capnp_get_text

   subroutine capnp_set_text(p, i, str, err)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(in) :: i
      character(len=*), intent(in) :: str
      integer, intent(out) :: err
      type(capnp_ptr_t) :: q
      integer(int64) :: j
      if (p%frozen .or. .not. associated(p%msg)) then
         err = CAPNP_ERR_ARG
         return
      end if
      q = capnp_new_list(p%msg, CAPNP_SZ_BYTE, len(str, kind=int64) + 1_int64, err)
      if (err /= CAPNP_OK) return
      do j = 1_int64, len(str, kind=int64)
         q%msg%segs(q%seg)%bytes(q%off + j - 1) = cp_i8b(int(iachar(str(j:j)), int32))
      end do
      ! Trailing NUL is already zero from the arena.
      call capnp_setp(p, i, q, err)
   end subroutine capnp_set_text

   subroutine capnp_get_data(p, i, bytes, err)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(in) :: i
      integer(int8), allocatable, intent(out) :: bytes(:)
      integer, intent(out) :: err
      type(capnp_ptr_t) :: q
      allocate (bytes(0))
      q = capnp_getp(p, i, err)
      if (err /= CAPNP_OK .or. q%kind == CAPNP_PK_NULL) return
      if (q%kind /= CAPNP_PK_LIST .or. q%esize /= CAPNP_SZ_BYTE) then
         err = CAPNP_ERR_KIND
         return
      end if
      if (q%nelem > 0_int64) then
         deallocate (bytes)
         allocate (bytes(0:q%nelem - 1))
         bytes = q%msg%segs(q%seg)%bytes(q%off:q%off + q%nelem - 1)
      end if
   end subroutine capnp_get_data

   subroutine capnp_set_data(p, i, bytes, err)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(in) :: i
      integer(int8), intent(in) :: bytes(:)
      integer, intent(out) :: err
      type(capnp_ptr_t) :: q
      if (p%frozen .or. .not. associated(p%msg)) then
         err = CAPNP_ERR_ARG
         return
      end if
      q = capnp_new_list(p%msg, CAPNP_SZ_BYTE, size(bytes, kind=int64), err)
      if (err /= CAPNP_OK) return
      if (size(bytes) > 0) then
         q%msg%segs(q%seg)%bytes(q%off:q%off + size(bytes, kind=int64) - 1) = bytes
      end if
      call capnp_setp(p, i, q, err)
   end subroutine capnp_set_data

   !> Text element i of a List(Text).
   !> Normative encoding (capnproto.org/encoding.html): List(Text) is a pointer
   !> list (element size C=6); each element points at Text (byte list + NUL).
   !> Also accepts the composite upgrade view (C=7, 0 data words, 1 pointer)
   !> used when a pointer list is read as a struct list.
   subroutine capnp_list_get_text(p, i, str, err)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(in) :: i
      character(len=:), allocatable, intent(out) :: str
      integer, intent(out) :: err
      str = ''
      err = CAPNP_OK
      if (p%kind /= CAPNP_PK_LIST) then
         err = CAPNP_ERR_KIND
         return
      end if
      if (p%esize == CAPNP_SZ_PTR) then
         ! C=6: element is a Text pointer; capnp_get_text on the list uses
         ! getp(list,i) then expects a byte list (see capnp_get_text).
         call capnp_get_text(p, i, str, err)
         return
      end if
      if (p%esize == CAPNP_SZ_COMPOSITE) then
         ! C=7 upgrade: element is a struct whose first pointer is Text
         ! (layout.h ListReader::getPointerElement keeps nestingLimit).
         call capnp_get_text(p, i, str, err)
         return
      end if
      err = CAPNP_ERR_KIND
   end subroutine capnp_list_get_text

   subroutine capnp_list_set_text(p, i, str, err)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(in) :: i
      character(len=*), intent(in) :: str
      integer, intent(out) :: err
      type(capnp_ptr_t) :: el
      if (p%frozen) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (p%kind /= CAPNP_PK_LIST) then
         err = CAPNP_ERR_KIND
         return
      end if
      if (p%esize == CAPNP_SZ_PTR) then
         call capnp_set_text(p, i, str, err)
         return
      end if
      if (p%esize == CAPNP_SZ_COMPOSITE) then
         el = capnp_list_get_struct(p, i, err)
         if (err /= CAPNP_OK) return
         call capnp_set_text(el, 0, str, err)
         return
      end if
      err = CAPNP_ERR_KIND
   end subroutine capnp_list_set_text

   ! ------------------------------------------------------------------
   ! u64: identical wire bytes to i64, kept for capnp-c surface parity.
   ! The int64 container carries the two's-complement bit pattern.
   ! ------------------------------------------------------------------

   function capnp_get_u64(p, off, default) result(v)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: off
      integer(int64), intent(in), optional :: default
      integer(int64) :: v
      v = capnp_get_i64(p, off, default)
   end function capnp_get_u64

   subroutine capnp_set_u64(p, off, v, err, default)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64), intent(in) :: off
      integer(int64), intent(in) :: v
      integer, intent(out) :: err
      integer(int64), intent(in), optional :: default
      call capnp_set_i64(p, off, v, err, default)
   end subroutine capnp_set_u64

   subroutine capnp_list_get_all_bool(p, arr, err)
      type(capnp_ptr_t), intent(in) :: p
      logical, allocatable, intent(out) :: arr(:)
      integer, intent(out) :: err
      integer(int64) :: i, n
      err = CAPNP_OK
      n = capnp_list_len(p)
      allocate (arr(0:max(n, 1_int64) - 1))
      if (n == 0_int64) then
         deallocate (arr)
         allocate (arr(0))
         return
      end if
      do i = 0_int64, n - 1_int64
         arr(i) = capnp_list_get_bool(p, i, err)
         if (err /= CAPNP_OK) return
      end do
   end subroutine capnp_list_get_all_bool

   subroutine capnp_list_set_all_bool(p, arr, err)
      type(capnp_ptr_t), intent(in) :: p
      logical, intent(in) :: arr(0:)
      integer, intent(out) :: err
      integer(int64) :: i
      err = CAPNP_OK
      if (size(arr, kind=int64) /= capnp_list_len(p)) then
         err = CAPNP_ERR_ARG
         return
      end if
      do i = 0_int64, capnp_list_len(p) - 1_int64
         call capnp_list_set_bool(p, i, arr(i), err)
         if (err /= CAPNP_OK) return
      end do
   end subroutine capnp_list_set_all_bool

   !> Zero-copy view of a Text or Data field's bytes (capn_text/capn_data
   !> parity: pointer + length instead of a copy). For Text the trailing NUL
   !> is excluded. The view aliases message storage: valid only while the
   !> message lives and its segment is not grown by further allocation.
   subroutine capnp_get_data_view(p, i, view, err)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(in) :: i
      integer(int8), pointer, intent(out) :: view(:)
      integer, intent(out) :: err
      type(capnp_ptr_t) :: q
      integer(int64) :: n
      integer(int8), pointer :: bytes(:)
      view => null()
      q = capnp_getp(p, i, err)
      if (err /= CAPNP_OK .or. q%kind == CAPNP_PK_NULL) return
      if (q%kind /= CAPNP_PK_LIST .or. q%esize /= CAPNP_SZ_BYTE) then
         err = CAPNP_ERR_KIND
         return
      end if
      n = q%nelem
      bytes => q%msg%segs(q%seg)%bytes
      if (n > 0_int64) view(0:n - 1) => bytes(q%off:q%off + n - 1)
   end subroutine capnp_get_data_view

   !> Zero-copy view of a Text field's characters, trailing NUL excluded.
   !> Same aliasing contract as capnp_get_data_view.
   subroutine capnp_get_text_view(p, i, view, err)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(in) :: i
      integer(int8), pointer, intent(out) :: view(:)
      integer, intent(out) :: err
      type(capnp_ptr_t) :: q
      integer(int64) :: n
      integer(int8), pointer :: bytes(:)
      view => null()
      q = capnp_getp(p, i, err)
      if (err /= CAPNP_OK .or. q%kind == CAPNP_PK_NULL) return
      if (q%kind /= CAPNP_PK_LIST .or. q%esize /= CAPNP_SZ_BYTE .or. q%nelem < 1_int64) then
         err = CAPNP_ERR_KIND
         return
      end if
      if (.not. text_has_nul(q)) then
         err = CAPNP_ERR_KIND
         return
      end if
      n = max(0_int64, q%nelem - 1_int64)
      bytes => q%msg%segs(q%seg)%bytes
      if (n > 0_int64) view(0:n - 1) => bytes(q%off:q%off + n - 1)
   end subroutine capnp_get_text_view

   !> Length of a Text field in characters (NUL excluded), without copying.
   function capnp_text_len(p, i, err) result(n)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(in) :: i
      integer, intent(out) :: err
      integer(int64) :: n
      type(capnp_ptr_t) :: q
      n = 0_int64
      q = capnp_getp(p, i, err)
      if (err /= CAPNP_OK .or. q%kind == CAPNP_PK_NULL) return
      if (q%kind /= CAPNP_PK_LIST .or. q%esize /= CAPNP_SZ_BYTE .or. q%nelem < 1_int64) then
         err = CAPNP_ERR_KIND
         return
      end if
      if (.not. text_has_nul(q)) then
         err = CAPNP_ERR_KIND
         return
      end if
      n = max(0_int64, q%nelem - 1_int64)
   end function capnp_text_len

   !> Detach pointer field i from struct p, returning the object (orphan
   !> disown): the slot is zeroed, the object's storage stays put and can be
   !> re-linked with capnp_setp within the same message.
   function capnp_disown(p, i, err) result(q)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(in) :: i
      integer, intent(out) :: err
      type(capnp_ptr_t) :: q
      integer(int64) :: slot
      q = capnp_getp(p, i, err)
      if (err /= CAPNP_OK .or. q%kind == CAPNP_PK_NULL) return
      if (p%frozen .or. .not. p%msg%is_builder) then
         err = CAPNP_ERR_ARG
         q = capnp_ptr_t()
         return
      end if
      select case (p%kind)
      case (CAPNP_PK_STRUCT)
         slot = p%off + int(p%dwords, int64)*8_int64 + int(i, int64)*8_int64
      case (CAPNP_PK_LIST)
         slot = p%off + int(i, int64)*8_int64
      case default
         err = CAPNP_ERR_KIND
         return
      end select
      call cp_put_i64(p%msg%segs(p%seg)%bytes, slot, 0_int64)
   end function capnp_disown

   !> Inquiry getters: the handle's components are conventionally read
   !> directly (generated code does), but these keep user code free of
   !> component access where opacity is preferred.
   pure function capnp_ptr_kind(p) result(k)
      type(capnp_ptr_t), intent(in) :: p
      integer :: k
      k = p%kind
   end function capnp_ptr_kind

   pure function capnp_ptr_is_null(p) result(is_null)
      type(capnp_ptr_t), intent(in) :: p
      logical :: is_null
      is_null = p%kind == CAPNP_PK_NULL
   end function capnp_ptr_is_null

   !> Element count of a list handle, 0 for anything else; mirrors
   !> capnp_list_len but never errors.
   pure function capnp_ptr_list_len(p) result(n)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64) :: n
      n = 0_int64
      if (p%kind == CAPNP_PK_LIST) n = p%nelem
   end function capnp_ptr_list_len

   !> Words consumed by an object and everything it references: data and
   !> pointer sections, list bodies, composite tag words (C++ totalSize()).
   !> Far-pointer landing pads are not counted; the total is the size a
   !> deep copy of the object would occupy.
   function capnp_total_size(p, err) result(words)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(out) :: err
      integer(int64) :: words
      words = size_depth(p, 0, err)
   end function capnp_total_size

   recursive function size_depth(p, depth, err) result(words)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(in) :: depth
      integer, intent(out) :: err
      integer(int64) :: words
      type(capnp_ptr_t) :: r
      integer(int64) :: i
      err = CAPNP_OK
      words = 0_int64
      if (p%kind == CAPNP_PK_NULL .or. p%kind == CAPNP_PK_CAP) return
      if (associated(p%msg)) then
         if (depth > p%msg%depth_limit) then
            err = CAPNP_ERR_DEPTH
            return
         end if
      end if
      select case (p%kind)
      case (CAPNP_PK_STRUCT)
         words = int(p%dwords, int64) + int(p%pwords, int64) + &
                 size_children(p, depth, err)
      case (CAPNP_PK_LIST)
         select case (p%esize)
         case (CAPNP_SZ_COMPOSITE)
            words = 1_int64 + p%nelem*(int(p%dwords, int64) + int(p%pwords, int64))
            if (p%pwords > 0) then
               r = capnp_ptr_t()
               r%msg => p%msg
               r%kind = CAPNP_PK_STRUCT
               r%seg = p%seg
               r%dwords = p%dwords
               r%pwords = p%pwords
               r%depth = p%depth
               do i = 0_int64, p%nelem - 1_int64
                  r%off = p%off + i*int(p%dwords + p%pwords, int64)*8_int64
                  words = words + size_children(r, depth, err)
                  if (err /= CAPNP_OK) return
               end do
            end if
         case (CAPNP_SZ_PTR)
            words = p%nelem
            do i = 0_int64, p%nelem - 1_int64
               r = capnp_getp(p, int(i), err)
               if (err /= CAPNP_OK) return
               words = words + size_depth(r, depth + 1, err)
               if (err /= CAPNP_OK) return
            end do
         case default
            words = (p%nelem*int(capnp_list_step_bits(p%esize), int64) + &
                     63_int64)/64_int64
         end select
      end select
   end function size_depth

   !> Sum of size_depth over a struct's pointer slots.
   recursive function size_children(p, depth, err) result(words)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(in) :: depth
      integer, intent(out) :: err
      integer(int64) :: words
      type(capnp_ptr_t) :: r
      integer :: j
      err = CAPNP_OK
      words = 0_int64
      do j = 0, p%pwords - 1
         r = capnp_getp(p, j, err)
         if (err /= CAPNP_OK) return
         words = words + size_depth(r, depth + 1, err)
         if (err /= CAPNP_OK) return
      end do
   end function size_children

   ! ------------------------------------------------------------------
   ! Deep copy: clone an object (from any message) into a builder message.
   ! This is what capnp C++ set() does for cross-message pointers and the
   ! basis of orphan adoption.
   ! ------------------------------------------------------------------

   recursive function capnp_copy(msg, src, err) result(q)
      type(capnp_message_t), intent(inout), target :: msg
      type(capnp_ptr_t), intent(in) :: src
      integer, intent(out) :: err
      type(capnp_ptr_t) :: q
      q = copy_depth(msg, src, 0, err)
   end function capnp_copy

   recursive function copy_depth(msg, src, depth, err) result(q)
      type(capnp_message_t), intent(inout), target :: msg
      type(capnp_ptr_t), intent(in) :: src
      integer, intent(in) :: depth
      integer, intent(out) :: err
      type(capnp_ptr_t) :: q
      type(capnp_ptr_t) :: r, c
      integer(int64) :: i, nb
      err = CAPNP_OK
      q = capnp_ptr_t()
      if (depth > msg%depth_limit) then
         err = CAPNP_ERR_DEPTH
         return
      end if
      select case (src%kind)
      case (CAPNP_PK_NULL)
         return
      case (CAPNP_PK_CAP)
         q%msg => msg
         q%kind = CAPNP_PK_CAP
         q%capidx = src%capidx
      case (CAPNP_PK_STRUCT)
         q = capnp_new_struct(msg, src%dwords, src%pwords, err)
         if (err /= CAPNP_OK) return
         call copy_struct_body(q, src, depth, err)
      case (CAPNP_PK_LIST)
         select case (src%esize)
         case (CAPNP_SZ_COMPOSITE)
            q = capnp_new_composite_list(msg, src%nelem, src%dwords, src%pwords, err)
            if (err /= CAPNP_OK) return
            r = capnp_ptr_t()
            r%msg => src%msg
            r%kind = CAPNP_PK_STRUCT
            r%seg = src%seg
            r%dwords = src%dwords
            r%pwords = src%pwords
            r%depth = src%depth
            do i = 0_int64, src%nelem - 1_int64
               c = capnp_list_get_struct(q, int(i), err)
               if (err /= CAPNP_OK) return
               r%off = src%off + i*int(src%dwords + src%pwords, int64)*8_int64
               call copy_struct_body(c, r, depth + 1, err)
               if (err /= CAPNP_OK) return
            end do
         case (CAPNP_SZ_PTR)
            q = capnp_new_list(msg, CAPNP_SZ_PTR, src%nelem, err)
            if (err /= CAPNP_OK) return
            do i = 0_int64, src%nelem - 1_int64
               r = capnp_getp(src, int(i), err)
               if (err /= CAPNP_OK) return
               c = copy_depth(msg, r, depth + 1, err)
               if (err /= CAPNP_OK) return
               call capnp_setp(q, int(i), c, err)
               if (err /= CAPNP_OK) return
            end do
         case default
            q = capnp_new_list(msg, src%esize, src%nelem, err)
            if (err /= CAPNP_OK) return
            nb = (int(capnp_list_step_bits(src%esize), int64)*src%nelem + 7_int64)/8_int64
            if (nb > 0_int64) then
               q%msg%segs(q%seg)%bytes(q%off:q%off + nb - 1) = &
                  src%msg%segs(src%seg)%bytes(src%off:src%off + nb - 1)
            end if
         end select
      end select
   end function copy_depth

   !> Copy the contents of struct src into the (already allocated) struct
   !> dst: overlapping data bytes verbatim, pointer fields recursively.
   recursive subroutine copy_struct_body(dst, src, depth, err)
      type(capnp_ptr_t), intent(in) :: dst, src
      integer, intent(in) :: depth
      integer, intent(out) :: err
      type(capnp_ptr_t) :: r, c
      integer(int64) :: nb
      integer :: i, np
      err = CAPNP_OK
      nb = min(int(dst%dwords, int64)*8_int64, (data_bits(src) + 7_int64)/8_int64)
      if (nb > 0_int64) then
         dst%msg%segs(dst%seg)%bytes(dst%off:dst%off + nb - 1) = &
            src%msg%segs(src%seg)%bytes(src%off:src%off + nb - 1)
      end if
      np = min(dst%pwords, src%pwords)
      do i = 0, np - 1
         r = capnp_getp(src, i, err)
         if (err /= CAPNP_OK) return
         if (r%kind == CAPNP_PK_NULL) cycle
         c = copy_depth(dst%msg, r, depth + 1, err)
         if (err /= CAPNP_OK) return
         call capnp_setp(dst, i, c, err)
         if (err /= CAPNP_OK) return
      end do
   end subroutine copy_struct_body

end module capnp_message
