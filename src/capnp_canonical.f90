!> Canonical form, per https://capnproto.org/encoding.html#canonicalization:
!> a single segment with no far pointers, objects laid out in preorder
!> (pointers within a struct in slot order), struct data and pointer
!> sections truncated of trailing zero words / null pointers, and composite
!> list elements all truncated to the same minimal size.
module capnp_canonical
   use capnp_kinds
   use capnp_endian
   use capnp_pointer
   use capnp_arena
   use capnp_message
   use capnp_serialize, only: capnp_serialize_bytes
   implicit none
   private

   public :: capnp_canonicalize, capnp_is_canonical

contains

   !> Canonical bytes of a message's root object: one raw segment starting
   !> with the root pointer, no segment table (`capnp convert
   !> binary:canonical` output format).
   subroutine capnp_canonicalize(msg, bytes, err)
      type(capnp_message_t), intent(inout), target :: msg
      integer(int8), allocatable, intent(out) :: bytes(:)
      integer, intent(out) :: err
      type(capnp_message_t), target :: canon
      type(capnp_ptr_t) :: root, croot
      integer(int64) :: need, n, remaining, after
      integer :: stat

      root = capnp_root(msg, err)
      if (err /= CAPNP_OK) return

      ! Measure with the live traversal budget so amplified DAGs abort immediately.
      remaining = msg%traversal_words
      need = 1_int64 + measure(root, 0, err) ! + root pointer word
      if (err /= CAPNP_OK) return
      after = msg%traversal_words
      msg%traversal_words = remaining

      call capnp_message_init_builder(canon, err, first_words=need)
      if (err /= CAPNP_OK) then
         call capnp_message_free(canon)
         msg%traversal_words = after
         return
      end if
      croot = canon_copy(canon, root, 0, err)
      if (err /= CAPNP_OK) then
         call capnp_message_free(canon)
         msg%traversal_words = after
         return
      end if
      call capnp_set_root(canon, croot, err)
      if (err /= CAPNP_OK) then
         call capnp_message_free(canon)
         msg%traversal_words = after
         return
      end if
      if (canon%nsegs /= 1) then
         call capnp_message_free(canon)
         msg%traversal_words = after
         err = CAPNP_ERR_BOUNDS ! measurement bug; never canonical
         return
      end if
      n = canon%segs(1)%len
      allocate (bytes(0:n - 1), stat=stat)
      if (stat /= 0) then
         call capnp_message_free(canon)
         msg%traversal_words = after
         err = CAPNP_ERR_ALLOC
         return
      end if
      bytes = canon%segs(1)%bytes(0:n - 1)
      call capnp_message_free(canon)
      msg%traversal_words = after
   end subroutine capnp_canonicalize

   !> Whether the message is already in canonical form. A lazy walk in
   !> the shape of C++ `MessageReader::isCanonical`: the first mismatch
   !> answers false, so only the part of the message that was visited is
   !> billed to the traversal budget, and only a malformed or over-limit
   !> prefix is an error. Capabilities and far pointers are not canonical.
   logical function capnp_is_canonical(msg, err) result(is_canonical)
      type(capnp_message_t), intent(inout), target :: msg
      integer, intent(out) :: err
      integer(int64) :: heads(1), nwords
      logical :: root_ok
      is_canonical = .false.
      err = CAPNP_OK
      if (msg%nsegs < 1) return
      if (msg%segs(1)%len == 0_int64) return
      if (msg%nsegs >= 2) then
         if (msg%segs(2)%len > 0_int64) return
      end if
      nwords = msg%segs(1)%len/CAPNP_WORD_BYTES
      if (.not. bill(msg, 1_int64, err)) return
      heads(1) = 1_int64
      root_ok = walk_ptr(msg, nwords, 0_int64, heads, 1, msg%depth_limit, err)
      if (err /= CAPNP_OK) return
      is_canonical = root_ok .and. heads(1) == nwords
   end function capnp_is_canonical

   !> Bill `words` against a reader's traversal budget (builders are exempt).
   logical function bill(msg, words, err)
      type(capnp_message_t), intent(inout) :: msg
      integer(int64), intent(in) :: words
      integer, intent(out) :: err
      err = CAPNP_OK
      bill = .true.
      if (msg%is_builder) return
      msg%traversal_words = msg%traversal_words - words
      if (msg%traversal_words < 0_int64) then
         err = CAPNP_ERR_TRAVERSAL
         bill = .false.
      end if
   end function bill

   !> Bounds-check an object of `size` words at word `tgt`, then bill it.
   logical function claim(msg, nwords, tgt, size, err)
      type(capnp_message_t), intent(inout) :: msg
      integer(int64), intent(in) :: nwords, tgt, size
      integer, intent(out) :: err
      err = CAPNP_OK
      claim = .false.
      if (tgt + size > nwords) then
         err = CAPNP_ERR_BOUNDS
         return
      end if
      claim = bill(msg, size, err)
   end function claim

   !> Word index a pointer in word `slot` targets; an offset outside the
   !> segment resolves to its end, as `SegmentReader::checkOffset` does.
   pure function target_word(w, slot, nwords) result(tgt)
      integer(int64), intent(in) :: w, slot, nwords
      integer(int64) :: tgt
      tgt = slot + 1_int64 + int(wp_offset(w), int64)
      if (tgt < 0_int64 .or. tgt > nwords) tgt = nwords
   end function target_word

   !> `PointerReader::isCanonical` for the pointer in word `slot` of
   !> segment 1. `heads(ih)` is the next word the preorder layout expects.
   !> `level` is the remaining nesting limit; on an error `err` is set.
   recursive logical function walk_ptr(msg, nwords, slot, heads, ih, level, err) result(ok)
      type(capnp_message_t), intent(inout), target :: msg
      integer(int64), intent(in) :: nwords, slot
      integer(int64), intent(inout) :: heads(:)
      integer, intent(in) :: ih, level
      integer, intent(out) :: err
      integer(int64) :: w
      ok = .false.
      err = CAPNP_OK
      w = cp_get_i64(msg%segs(1)%bytes, slot*CAPNP_WORD_BYTES)
      if (w == 0_int64) then
         ok = .true.
         return
      end if
      select case (wp_kind(w))
      case (CAPNP_WK_STRUCT)
         ok = walk_struct_ptr(msg, nwords, slot, w, heads, ih, level, err)
      case (CAPNP_WK_LIST)
         ok = walk_list_ptr(msg, nwords, slot, w, heads, ih, level, err)
      case default
         ok = .false.
      end select
   end function walk_ptr

   recursive logical function walk_struct_ptr(msg, nwords, slot, w, heads, ih, level, err) result(ok)
      type(capnp_message_t), intent(inout), target :: msg
      integer(int64), intent(in) :: nwords, slot, w
      integer(int64), intent(inout) :: heads(:)
      integer, intent(in) :: ih, level
      integer, intent(out) :: err
      integer(int64) :: tgt
      integer :: dw, pw
      logical :: data_trunc, ptr_trunc
      ok = .false.
      err = CAPNP_OK
      if (level <= 0) then
         err = CAPNP_ERR_DEPTH
         return
      end if
      tgt = target_word(w, slot, nwords)
      dw = wp_struct_dwords(w)
      pw = wp_struct_pwords(w)
      if (.not. claim(msg, nwords, tgt, int(dw + pw, int64), err)) return
      if (dw == 0 .and. pw == 0) then
         ok = tgt == slot
         return
      end if
      ok = walk_struct_body(msg, nwords, tgt, dw, pw, heads, ih, ih, level - 1, &
                            data_trunc, ptr_trunc, err)
      ok = ok .and. data_trunc .and. ptr_trunc
   end function walk_struct_ptr

   !> `StructReader::isCanonical`: the struct at word `loc` must sit at
   !> `heads(ih)`; its pointer fields are walked against `heads(ip)`.
   recursive logical function walk_struct_body(msg, nwords, loc, dw, pw, heads, ih, ip, level, &
                                               data_trunc, ptr_trunc, err) result(ok)
      type(capnp_message_t), intent(inout), target :: msg
      integer(int64), intent(in) :: nwords, loc
      integer, intent(in) :: dw, pw, ih, ip, level
      integer(int64), intent(inout) :: heads(:)
      logical, intent(out) :: data_trunc, ptr_trunc
      integer, intent(out) :: err
      integer :: k
      ok = .false.
      err = CAPNP_OK
      data_trunc = .false.
      ptr_trunc = .false.
      if (loc /= heads(ih)) return
      if (dw == 0) then
         data_trunc = .true.
      else
         data_trunc = cp_get_i64(msg%segs(1)%bytes, (loc + dw - 1)*CAPNP_WORD_BYTES) /= 0_int64
      end if
      if (pw == 0) then
         ptr_trunc = .true.
      else
         ptr_trunc = pointer_is_set(msg, loc + dw + pw - 1, err)
         if (err /= CAPNP_OK) return
      end if
      heads(ih) = heads(ih) + dw + pw
      do k = 0, pw - 1
         if (.not. walk_ptr(msg, nwords, loc + dw + k, heads, ip, level, err)) return
      end do
      ok = .true.
   end function walk_struct_body

   !> `!PointerReader::isNull()` for the pointer in word `slot`. C++ asks
   !> the pointer for its type, which follows a far pointer to its landing
   !> pad and rejects an unknown kind, so those errors surface here even
   !> though a far pointer is never canonical.
   function pointer_is_set(msg, slot, err) result(is_set)
      type(capnp_message_t), intent(inout), target :: msg
      integer(int64), intent(in) :: slot
      integer, intent(out) :: err
      logical :: is_set
      integer(int64) :: w, pad, tag, seg_words
      integer :: tseg, oseg
      logical :: two
      err = CAPNP_OK
      w = cp_get_i64(msg%segs(1)%bytes, slot*CAPNP_WORD_BYTES)
      is_set = w /= 0_int64
      if (.not. is_set) return
      select case (wp_kind(w))
      case (CAPNP_WK_CAP)
         if (iand(w, 4294967295_int64) /= int(CAPNP_WK_CAP, int64)) err = CAPNP_ERR_KIND
      case (CAPNP_WK_FAR)
         two = wp_far_two(w)
         if (.not. segment_present(msg, wp_far_seg(w), tseg)) then
            err = CAPNP_ERR_SEGMENT
            return
         end if
         seg_words = msg%segs(tseg)%len/CAPNP_WORD_BYTES
         pad = wp_far_off(w)
         if (pad + 1_int64 + merge(1_int64, 0_int64, two) > seg_words) then
            err = CAPNP_ERR_BOUNDS
            return
         end if
         if (.not. bill(msg, 1_int64 + merge(1_int64, 0_int64, two), err)) return
         tag = cp_get_i64(msg%segs(tseg)%bytes, pad*CAPNP_WORD_BYTES)
         if (two) then
            if (.not. segment_present(msg, wp_far_seg(tag), oseg)) then
               err = CAPNP_ERR_SEGMENT
               return
            end if
            if (wp_kind(tag) /= CAPNP_WK_FAR) then
               err = CAPNP_ERR_KIND
               return
            end if
            tag = cp_get_i64(msg%segs(tseg)%bytes, (pad + 1_int64)*CAPNP_WORD_BYTES)
         end if
         if (wp_kind(tag) == CAPNP_WK_FAR) then
            err = CAPNP_ERR_KIND
         else if (wp_kind(tag) == CAPNP_WK_CAP .and. &
                  iand(tag, 4294967295_int64) /= int(CAPNP_WK_CAP, int64)) then
            err = CAPNP_ERR_KIND
         end if
      end select
   end function pointer_is_set

   !> Segment `id` (0-based) as a 1-based index; a missing or empty segment
   !> is absent, as in `ReaderArena::tryGetSegment`.
   logical function segment_present(msg, id, idx)
      type(capnp_message_t), intent(in) :: msg
      integer(int64), intent(in) :: id
      integer, intent(out) :: idx
      idx = 0
      segment_present = .false.
      if (id < 0_int64 .or. id >= int(msg%nsegs, int64)) return
      idx = int(id) + 1
      segment_present = msg%segs(idx)%len > 0_int64
   end function segment_present

   recursive logical function walk_list_ptr(msg, nwords, slot, w, heads, ih, level, err) result(ok)
      type(capnp_message_t), intent(inout), target :: msg
      integer(int64), intent(in) :: nwords, slot, w
      integer(int64), intent(inout) :: heads(:)
      integer, intent(in) :: ih, level
      integer, intent(out) :: err
      integer(int64) :: tgt, tag, count, total, per, bits, bitsize, end_word, pos, i
      integer :: es, dw, pw, lvl, sub
      logical :: data_trunc, ptr_trunc, list_data, list_ptr
      integer(int64) :: pheads(2)
      ok = .false.
      err = CAPNP_OK
      if (level <= 0) then
         err = CAPNP_ERR_DEPTH
         return
      end if
      lvl = level - 1
      tgt = target_word(w, slot, nwords)
      es = wp_list_esize(w)
      select case (es)
      case (CAPNP_SZ_COMPOSITE)
         total = wp_list_count(w)
         if (.not. claim(msg, nwords, tgt, total + 1_int64, err)) return
         tag = cp_get_i64(msg%segs(1)%bytes, tgt*CAPNP_WORD_BYTES)
         if (wp_kind(tag) /= CAPNP_WK_STRUCT) then
            err = CAPNP_ERR_KIND
            return
         end if
         count = ibits(tag, 2, 29)
         dw = wp_struct_dwords(tag)
         pw = wp_struct_pwords(tag)
         per = int(dw + pw, int64)
         if (count*per > total) then
            err = CAPNP_ERR_BOUNDS
            return
         end if
         if (per == 0_int64) then
            if (.not. bill(msg, count, err)) return
         end if
         heads(ih) = heads(ih) + 1_int64
         if (tgt + 1_int64 /= heads(ih)) return
         if (count*per /= total) return
         if (per == 0_int64) then
            ok = .true.
            return
         end if
         end_word = heads(ih) + total
         pheads(1) = heads(ih)
         pheads(2) = end_word
         list_data = .false.
         list_ptr = .false.
         do i = 0_int64, count - 1_int64
            if (lvl <= 0) then
               err = CAPNP_ERR_DEPTH
               return
            end if
            if (.not. walk_struct_body(msg, nwords, tgt + 1_int64 + i*per, dw, pw, pheads, 1, 2, &
                                       lvl - 1, data_trunc, ptr_trunc, err)) return
            list_data = list_data .or. data_trunc
            list_ptr = list_ptr .or. ptr_trunc
         end do
         heads(ih) = pheads(2)
         ok = list_data .and. list_ptr
      case (CAPNP_SZ_PTR)
         count = wp_list_count(w)
         if (.not. claim(msg, nwords, tgt, count, err)) return
         if (tgt /= heads(ih)) return
         heads(ih) = heads(ih) + count
         do i = 0_int64, count - 1_int64
            if (.not. walk_ptr(msg, nwords, tgt + i, heads, ih, lvl, err)) return
         end do
         ok = .true.
      case default
         count = wp_list_count(w)
         bits = int(capnp_list_step_bits(es), int64)
         bitsize = count*bits
         end_word = (bitsize + 63_int64)/64_int64
         if (.not. claim(msg, nwords, tgt, end_word, err)) return
         if (es == CAPNP_SZ_VOID) then
            if (.not. bill(msg, count, err)) return
         end if
         if (tgt /= heads(ih)) return
         pos = tgt*CAPNP_WORD_BYTES + bitsize/8_int64
         sub = int(mod(bitsize, 8_int64))
         if (sub > 0) then
            if (iand(int(msg%segs(1)%bytes(pos), int32), not(ishft(1_int32, sub) - 1_int32)) /= 0_int32) return
            pos = pos + 1_int64
         end if
         do while (pos /= (tgt + end_word)*CAPNP_WORD_BYTES)
            if (msg%segs(1)%bytes(pos) /= 0_int8) return
            pos = pos + 1_int64
         end do
         heads(ih) = tgt + end_word
         ok = .true.
      end select
   end function walk_list_ptr

   ! --- truncation measures ---------------------------------------------

   !> Trailing zero words dropped from a struct's data section.
   function trimmed_dwords(p) result(nd)
      type(capnp_ptr_t), intent(in) :: p
      integer :: nd
      integer(int64) :: w
      nd = p%dwords
      do while (nd > 0)
         w = cp_get_i64(p%msg%segs(p%seg)%bytes, p%off + int(nd - 1, int64)*8_int64)
         if (w /= 0_int64) return
         nd = nd - 1
      end do
   end function trimmed_dwords

   !> Trailing null pointers dropped from a struct's pointer section.
   function trimmed_pwords(p, err) result(np)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(out) :: err
      integer :: np
      err = CAPNP_OK
      np = p%pwords
      do while (np > 0)
         if (cp_get_i64(p%msg%segs(p%seg)%bytes, p%off + &
                        int(p%dwords + np - 1, int64)*8_int64) /= 0_int64) return
         np = np - 1
      end do
   end function trimmed_pwords

   !> Uniform trimmed element sizes for a composite list: the maximum over
   !> all elements (canonical lists share one tag).
   subroutine composite_trim(p, nd, np, err)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(out) :: nd, np
      integer, intent(out) :: err
      type(capnp_ptr_t) :: el
      integer(int64) :: i
      err = CAPNP_OK
      nd = 0
      np = 0
      do i = 0_int64, p%nelem - 1_int64
         el = capnp_list_get_struct(p, int(i), err)
         if (err /= CAPNP_OK) return
         nd = max(nd, trimmed_dwords(el))
         np = max(np, trimmed_pwords(el, err))
         if (err /= CAPNP_OK) return
      end do
   end subroutine composite_trim

   ! --- measuring pass ----------------------------------------------------

   !> Canonical word count of an object (its own storage plus everything it
   !> references, tags and landing-free).
   recursive function measure(p, depth, err) result(words)
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(in) :: depth
      integer, intent(out) :: err
      integer(int64) :: words
      type(capnp_ptr_t) :: q
      integer(int64) :: i
      integer :: nd, np, k
      err = CAPNP_OK
      words = 0_int64
      select case (p%kind)
      case (CAPNP_PK_NULL)
         return
      case (CAPNP_PK_CAP)
         err = CAPNP_ERR_KIND
         return
      end select
      if (depth >= p%msg%depth_limit) then
         err = CAPNP_ERR_DEPTH
         return
      end if
      select case (p%kind)
      case (CAPNP_PK_STRUCT)
         nd = trimmed_dwords(p)
         np = trimmed_pwords(p, err)
         if (err /= CAPNP_OK) return
         words = int(nd + np, int64)
         do k = 0, np - 1
            q = capnp_getp(p, k, err)
            if (err /= CAPNP_OK) return
            words = words + measure(q, depth + 1, err)
            if (err /= CAPNP_OK) return
         end do
      case (CAPNP_PK_LIST)
         select case (p%esize)
         case (CAPNP_SZ_COMPOSITE)
            if (p%nelem > 0_int64 .and. depth + 1 >= p%msg%depth_limit) then
               err = CAPNP_ERR_DEPTH
               return
            end if
            call composite_trim(p, nd, np, err)
            if (err /= CAPNP_OK) return
            words = 1_int64 + p%nelem*int(nd + np, int64) ! tag + elements
            do i = 0_int64, p%nelem - 1_int64
               q = capnp_list_get_struct(p, int(i), err)
               if (err /= CAPNP_OK) return
               ! layout.c++ setListPointer(canonical=true) copies element
               ! pointers using the list's nestingLimit, after getStructElement
               ! already checked nestingLimit > 0.
               q%depth = p%depth
               do k = 0, min(np, q%pwords) - 1
                  block
                     type(capnp_ptr_t) :: r
                     r = capnp_getp(q, k, err)
                     if (err /= CAPNP_OK) return
                     words = words + measure(r, depth + 1, err)
                     if (err /= CAPNP_OK) return
                  end block
               end do
            end do
         case (CAPNP_SZ_PTR)
            words = p%nelem
            do i = 0_int64, p%nelem - 1_int64
               q = capnp_getp(p, int(i), err)
               if (err /= CAPNP_OK) return
               words = words + measure(q, depth + 1, err)
               if (err /= CAPNP_OK) return
            end do
         case default
            words = (int(capnp_list_step_bits(p%esize), int64)*p%nelem + 63_int64)/64_int64
         end select
      end select
   end function measure

   ! --- canonical copy ------------------------------------------------------

   !> Copy p into the canonical arena with truncation; allocation order is
   !> preorder because children are allocated in slot order right after
   !> their parent.
   recursive function canon_copy(canon, p, depth, err) result(q)
      type(capnp_message_t), intent(inout), target :: canon
      type(capnp_ptr_t), intent(in) :: p
      integer, intent(in) :: depth
      integer, intent(out) :: err
      type(capnp_ptr_t) :: q
      type(capnp_ptr_t) :: r, c, del, sel
      integer(int64) :: i, nb
      integer :: nd, np, k
      err = CAPNP_OK
      q = capnp_ptr_t()
      select case (p%kind)
      case (CAPNP_PK_NULL)
         return
      case (CAPNP_PK_CAP)
         ! Capabilities have no canonical form in a data message.
         err = CAPNP_ERR_KIND
         return
      end select
      if (depth >= p%msg%depth_limit) then
         err = CAPNP_ERR_DEPTH
         return
      end if
      select case (p%kind)
      case (CAPNP_PK_STRUCT)
         nd = trimmed_dwords(p)
         np = trimmed_pwords(p, err)
         if (err /= CAPNP_OK) return
         q = capnp_new_struct(canon, nd, np, err)
         if (err /= CAPNP_OK) return
         nb = min(int(nd, int64)*8_int64, (data_bits_of(p) + 7_int64)/8_int64)
         if (nb > 0_int64) q%msg%segs(q%seg)%bytes(q%off:q%off + nb - 1) = &
            p%msg%segs(p%seg)%bytes(p%off:p%off + nb - 1)
         do k = 0, np - 1
            r = capnp_getp(p, k, err)
            if (err /= CAPNP_OK) return
            if (r%kind == CAPNP_PK_NULL) cycle
            c = canon_copy(canon, r, depth + 1, err)
            if (err /= CAPNP_OK) return
            call capnp_setp(q, k, c, err)
            if (err /= CAPNP_OK) return
         end do
      case (CAPNP_PK_LIST)
         select case (p%esize)
         case (CAPNP_SZ_COMPOSITE)
            if (p%nelem > 0_int64 .and. depth + 1 >= p%msg%depth_limit) then
               err = CAPNP_ERR_DEPTH
               return
            end if
            call composite_trim(p, nd, np, err)
            if (err /= CAPNP_OK) return
            q = capnp_new_composite_list(canon, p%nelem, nd, np, err)
            if (err /= CAPNP_OK) return
            do i = 0_int64, p%nelem - 1_int64
               sel = capnp_list_get_struct(p, int(i), err)
               if (err /= CAPNP_OK) return
               sel%depth = p%depth
               del = capnp_list_get_struct(q, int(i), err)
               if (err /= CAPNP_OK) return
               nb = min(int(nd, int64)*8_int64, int(sel%dwords, int64)*8_int64)
               if (nb > 0_int64) del%msg%segs(del%seg)%bytes(del%off:del%off + nb - 1) = &
                  sel%msg%segs(sel%seg)%bytes(sel%off:sel%off + nb - 1)
               do k = 0, min(np, sel%pwords) - 1
                  r = capnp_getp(sel, k, err)
                  if (err /= CAPNP_OK) return
                  if (r%kind == CAPNP_PK_NULL) cycle
                  c = canon_copy(canon, r, depth + 1, err)
                  if (err /= CAPNP_OK) return
                  call capnp_setp(del, k, c, err)
                  if (err /= CAPNP_OK) return
               end do
            end do
         case (CAPNP_SZ_PTR)
            q = capnp_new_list(canon, CAPNP_SZ_PTR, p%nelem, err)
            if (err /= CAPNP_OK) return
            do i = 0_int64, p%nelem - 1_int64
               r = capnp_getp(p, int(i), err)
               if (err /= CAPNP_OK) return
               if (r%kind == CAPNP_PK_NULL) cycle
               c = canon_copy(canon, r, depth + 1, err)
               if (err /= CAPNP_OK) return
               call capnp_setp(q, int(i), c, err)
               if (err /= CAPNP_OK) return
            end do
         case default
            q = capnp_new_list(canon, p%esize, p%nelem, err)
            if (err /= CAPNP_OK) return
            nb = (int(capnp_list_step_bits(p%esize), int64)*p%nelem + 7_int64)/8_int64
            if (nb > 0_int64) q%msg%segs(q%seg)%bytes(q%off:q%off + nb - 1) = &
               p%msg%segs(p%seg)%bytes(p%off:p%off + nb - 1)
            if (p%esize == CAPNP_SZ_BIT .and. mod(p%nelem, 8_int64) /= 0_int64) then
               q%msg%segs(q%seg)%bytes(q%off + nb - 1_int64) = int(iand( &
                  int(q%msg%segs(q%seg)%bytes(q%off + nb - 1_int64), int32), &
                  ishft(1_int32, int(mod(p%nelem, 8_int64))) - 1_int32), int8)
            end if
         end select
      end select
   end function canon_copy

   pure function data_bits_of(p) result(b)
      type(capnp_ptr_t), intent(in) :: p
      integer(int64) :: b
      if (p%dbits >= 0_int64) then
         b = p%dbits
      else
         b = int(p%dwords, int64)*64_int64
      end if
   end function data_bits_of

end module capnp_canonical
