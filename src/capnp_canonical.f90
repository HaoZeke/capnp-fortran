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

   public :: capnp_canonicalize

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
      integer(int64) :: need, n

      root = capnp_root(msg, err)
      if (err /= CAPNP_OK) return

      ! Measure first so the arena never needs a second segment.
      need = 1_int64 + measure(root, 0, err) ! + root pointer word
      if (err /= CAPNP_OK) return

      call capnp_message_init_builder(canon, err, first_words=need)
      if (err /= CAPNP_OK) return
      croot = canon_copy(canon, root, 0, err)
      if (err /= CAPNP_OK) return
      call capnp_set_root(canon, croot, err)
      if (err /= CAPNP_OK) return
      if (canon%nsegs /= 1) then
         err = CAPNP_ERR_BOUNDS ! measurement bug; never canonical
         return
      end if
      n = canon%segs(1)%len
      allocate (bytes(0:n - 1))
      bytes = canon%segs(1)%bytes(0:n - 1)
      call capnp_message_free(canon)
   end subroutine capnp_canonicalize

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
      type(capnp_ptr_t) :: q
      err = CAPNP_OK
      np = p%pwords
      do while (np > 0)
         q = capnp_getp(p, np - 1, err)
         if (err /= CAPNP_OK) return
         if (q%kind /= CAPNP_PK_NULL) return
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
      if (depth > 64) then
         err = CAPNP_ERR_DEPTH
         return
      end if
      select case (p%kind)
      case (CAPNP_PK_NULL, CAPNP_PK_CAP)
         return
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
            call composite_trim(p, nd, np, err)
            if (err /= CAPNP_OK) return
            words = 1_int64 + p%nelem*int(nd + np, int64) ! tag + elements
            do i = 0_int64, p%nelem - 1_int64
               q = capnp_list_get_struct(p, int(i), err)
               if (err /= CAPNP_OK) return
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
      if (depth > 64) then
         err = CAPNP_ERR_DEPTH
         return
      end if
      select case (p%kind)
      case (CAPNP_PK_NULL)
         return
      case (CAPNP_PK_CAP)
         ! Capabilities have no canonical form in a data message.
         err = CAPNP_ERR_KIND
         return
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
            call composite_trim(p, nd, np, err)
            if (err /= CAPNP_OK) return
            q = capnp_new_composite_list(canon, p%nelem, nd, np, err)
            if (err /= CAPNP_OK) return
            do i = 0_int64, p%nelem - 1_int64
               sel = capnp_list_get_struct(p, int(i), err)
               if (err /= CAPNP_OK) return
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
