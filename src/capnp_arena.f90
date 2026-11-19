!> Segments and the message arena. A message owns a set of flat byte
!> segments; builders allocate zeroed word runs from them, readers either
!> copy received bytes or view them in place (zero-copy, capn_init_mem
!> parity). Segment storage is a pointer array so a view can alias caller
!> memory; `owned` records who deallocates.
module capnp_arena
   use capnp_kinds, only: int8, int64, CAPNP_OK, CAPNP_ERR_ALLOC, CAPNP_ERR_ARG, &
                          CAPNP_WORD_BYTES, CAPNP_DEFAULT_TRAVERSAL_WORDS, &
                          CAPNP_DEFAULT_DEPTH_LIMIT, CAPNP_ALLOC_FIXED_SIZE, &
                          CAPNP_ALLOC_GROW_HEURISTICALLY, &
                          CAPNP_SUGGESTED_FIRST_SEGMENT_WORDS, &
                          capnp_builder_options_t
   implicit none
   private

   public :: capnp_segment_t, capnp_message_t
   public :: capnp_message_init_builder, capnp_message_free
   public :: capnp_arena_alloc, capnp_arena_alloc_in
   public :: capnp_segment_view
   public :: capnp_builder_options_t
   public :: capnp_grown_segment_bytes

   integer(int64), parameter :: DEFAULT_FIRST_WORDS = CAPNP_SUGGESTED_FIRST_SEGMENT_WORDS
   integer(int64), parameter :: MAX_SEGMENT_WORDS = 536870912_int64

   type :: capnp_segment_t
      !> Storage, 0-based. Capacity is size(bytes); len is the used prefix.
      !> owned segments are deallocated by capnp_message_free; views alias
      !> caller memory, which must outlive the message.
      integer(int8), pointer :: bytes(:) => null()
      integer(int64) :: len = 0_int64
      logical :: owned = .false.
   end type capnp_segment_t

   type :: capnp_message_t
      type(capnp_segment_t), allocatable :: segs(:)
      integer :: nsegs = 0
      logical :: is_builder = .false.
      !> Reader guards; builders leave them untouched.
      integer(int64) :: traversal_words = CAPNP_DEFAULT_TRAVERSAL_WORDS
      integer :: depth_limit = CAPNP_DEFAULT_DEPTH_LIMIT
      !> How later segments are sized. The first segment stays first_words.
      integer :: alloc_strategy = CAPNP_ALLOC_GROW_HEURISTICALLY
      !> Preferred later-segment size in words. Zero means "exactly the request"
      !> when the strategy is fixed size. Heuristic growth does not read this.
      integer(int64) :: segment_words = CAPNP_SUGGESTED_FIRST_SEGMENT_WORDS
   end type capnp_message_t

contains

   !> Start a builder message. Word 0 of segment 1 is reserved and zeroed for
   !> the root pointer. first_words bounds the first segment's capacity; a
   !> small value forces multi-segment messages (and thus far pointers) early.
   !> options selects the same first-segment size and the allocation strategy.
   !> An explicit first_words overrides the size in options. The default is
   !> still 1024 words and heuristic doubling of later segments.
   subroutine capnp_message_init_builder(msg, err, first_words, options)
      type(capnp_message_t), intent(inout) :: msg
      integer, intent(out) :: err
      integer(int64), intent(in), optional :: first_words
      type(capnp_builder_options_t), intent(in), optional :: options
      integer(int64) :: cap, preferred
      integer :: strategy
      err = CAPNP_OK
      call capnp_message_free(msg)
      preferred = DEFAULT_FIRST_WORDS
      strategy = CAPNP_ALLOC_GROW_HEURISTICALLY
      if (present(options)) then
         if (options%first_segment_words < 0_int64 .or. &
             (options%allocation_strategy /= CAPNP_ALLOC_FIXED_SIZE .and. &
              options%allocation_strategy /= CAPNP_ALLOC_GROW_HEURISTICALLY)) then
            err = CAPNP_ERR_ARG
            return
         end if
         ! An explicit first_words replaces the size, so only the size that
         ! is actually used has to sit inside the segment cap.
         if (.not. present(first_words) .and. &
             options%first_segment_words > MAX_SEGMENT_WORDS) then
            err = CAPNP_ERR_ARG
            return
         end if
         preferred = options%first_segment_words
         strategy = options%allocation_strategy
      end if
      ! The historical keyword still means "at least one word".
      if (present(first_words)) then
         if (first_words > MAX_SEGMENT_WORDS) then
            err = CAPNP_ERR_ARG
            return
         end if
         preferred = max(0_int64, first_words)
      end if
      cap = preferred
      if (cap < 1_int64) cap = 1_int64
      if (cap > MAX_SEGMENT_WORDS) then
         err = CAPNP_ERR_ARG
         return
      end if
      msg%is_builder = .true.
      msg%alloc_strategy = strategy
      msg%segment_words = preferred
      allocate (msg%segs(4))
      msg%nsegs = 1
      call seg_reserve(msg%segs(1), cap*CAPNP_WORD_BYTES, err)
      if (err /= CAPNP_OK) then
         call capnp_message_free(msg)
         return
      end if
      msg%segs(1)%len = CAPNP_WORD_BYTES
      msg%segs(1)%bytes(0:CAPNP_WORD_BYTES - 1) = 0_int8
   end subroutine capnp_message_init_builder

   !> Alias a caller buffer slice as a read-only segment (zero-copy). The
   !> buffer must stay allocated, unmoved, and targeted for the message's
   !> lifetime.
   subroutine capnp_segment_view(seg, buf, nbytes)
      type(capnp_segment_t), intent(inout) :: seg
      integer(int8), intent(in), pointer :: buf(:)
      integer(int64), intent(in) :: nbytes
      seg%bytes(0:) => buf
      seg%len = nbytes
      seg%owned = .false.
   end subroutine capnp_segment_view

   subroutine capnp_message_free(msg)
      type(capnp_message_t), intent(inout) :: msg
      integer :: i
      if (allocated(msg%segs)) then
         do i = 1, size(msg%segs)
            if (msg%segs(i)%owned) then
               if (associated(msg%segs(i)%bytes)) deallocate (msg%segs(i)%bytes)
            end if
            msg%segs(i)%bytes => null()
         end do
         deallocate (msg%segs)
      end if
      msg%nsegs = 0
      msg%is_builder = .false.
      msg%traversal_words = CAPNP_DEFAULT_TRAVERSAL_WORDS
      msg%depth_limit = CAPNP_DEFAULT_DEPTH_LIMIT
      msg%alloc_strategy = CAPNP_ALLOC_GROW_HEURISTICALLY
      msg%segment_words = DEFAULT_FIRST_WORDS
   end subroutine capnp_message_free

   !> Allocate nwords zeroed words. Tries the last segment; if the run does
   !> not fit its remaining capacity, appends a fresh segment. Returns the
   !> segment index and byte offset of the run.
   subroutine capnp_arena_alloc(msg, nwords, seg_idx, byte_off, err)
      type(capnp_message_t), intent(inout) :: msg
      integer(int64), intent(in) :: nwords
      integer, intent(out) :: seg_idx
      integer(int64), intent(out) :: byte_off
      integer, intent(out) :: err
      integer(int64) :: need, cap
      err = CAPNP_OK
      seg_idx = 0
      byte_off = 0_int64
      if (.not. msg%is_builder .or. nwords < 0_int64 .or. nwords > MAX_SEGMENT_WORDS) then
         err = CAPNP_ERR_ARG
         return
      end if
      need = nwords*CAPNP_WORD_BYTES
      associate (last => msg%segs(msg%nsegs))
         if (last%len + need <= size(last%bytes, kind=int64)) then
            byte_off = last%len
            last%bytes(last%len:last%len + need - 1) = 0_int8
            last%len = last%len + need
            seg_idx = msg%nsegs
            return
         end if
      end associate
      cap = next_segment_bytes(msg, need)
      call msg_append_segment(msg, cap, err)
      if (err /= CAPNP_OK) return
      seg_idx = msg%nsegs
      byte_off = 0_int64
      msg%segs(seg_idx)%bytes(0:need - 1) = 0_int8
      msg%segs(seg_idx)%len = need
   end subroutine capnp_arena_alloc

   !> Allocate nwords zeroed words inside a specific segment, growing it when
   !> needed. Used for far-pointer landing pads, which must live in the
   !> segment of the object they describe.
   subroutine capnp_arena_alloc_in(msg, seg_idx, nwords, byte_off, err)
      type(capnp_message_t), intent(inout) :: msg
      integer, intent(in) :: seg_idx
      integer(int64), intent(in) :: nwords
      integer(int64), intent(out) :: byte_off
      integer, intent(out) :: err
      integer(int64) :: need
      err = CAPNP_OK
      byte_off = 0_int64
      if (.not. msg%is_builder .or. seg_idx < 1 .or. seg_idx > msg%nsegs) then
         err = CAPNP_ERR_ARG
         return
      end if
      need = nwords*CAPNP_WORD_BYTES
      associate (seg => msg%segs(seg_idx))
         if (seg%len + need > size(seg%bytes, kind=int64)) then
            if (msg%alloc_strategy == CAPNP_ALLOC_FIXED_SIZE) then
               call seg_reserve(seg, seg%len + need, err)
            else
               call seg_reserve(seg, capnp_grown_segment_bytes( &
                  size(seg%bytes, kind=int64), seg%len + need), err)
            end if
            if (err /= CAPNP_OK) return
         end if
         byte_off = seg%len
         seg%bytes(seg%len:seg%len + need - 1) = 0_int8
         seg%len = seg%len + need
      end associate
   end subroutine capnp_arena_alloc_in

   subroutine msg_append_segment(msg, cap_bytes, err)
      type(capnp_message_t), intent(inout) :: msg
      integer(int64), intent(in) :: cap_bytes
      integer, intent(out) :: err
      type(capnp_segment_t), allocatable :: tmp(:)
      integer :: i
      err = CAPNP_OK
      if (msg%nsegs == size(msg%segs)) then
         allocate (tmp(2*size(msg%segs)))
         do i = 1, msg%nsegs
            tmp(i)%bytes => msg%segs(i)%bytes
            tmp(i)%len = msg%segs(i)%len
            tmp(i)%owned = msg%segs(i)%owned
            msg%segs(i)%bytes => null()
         end do
         call move_alloc(tmp, msg%segs)
      end if
      msg%nsegs = msg%nsegs + 1
      call seg_reserve(msg%segs(msg%nsegs), cap_bytes, err)
      if (err /= CAPNP_OK) msg%nsegs = msg%nsegs - 1
   end subroutine msg_append_segment

   !> Bytes for a segment that does not fit in the previous one.
   !> Fixed size repeats the preferred first-segment size (C++ 1.4.0),
   !> or the request when that preferred size is zero or too small.
   !> Heuristic growth keeps the historical default: at least twice the
   !> previous capacity, and at least the request, and never past the
   !> segment cap. The doubling is done in capnp_grown_segment_bytes so a
   !> large previous segment cannot overflow the signed size.
   pure function next_segment_bytes(msg, need) result(cap)
      type(capnp_message_t), intent(in) :: msg
      integer(int64), intent(in) :: need
      integer(int64) :: cap, preferred
      if (msg%alloc_strategy == CAPNP_ALLOC_FIXED_SIZE) then
         preferred = msg%segment_words
         if (preferred < 0_int64) preferred = 0_int64
         if (preferred > MAX_SEGMENT_WORDS) preferred = MAX_SEGMENT_WORDS
         cap = max(need, preferred*CAPNP_WORD_BYTES)
         if (cap > MAX_SEGMENT_WORDS*CAPNP_WORD_BYTES) cap = MAX_SEGMENT_WORDS*CAPNP_WORD_BYTES
      else
         cap = capnp_grown_segment_bytes( &
            size(msg%segs(msg%nsegs)%bytes, kind=int64), need)
      end if
   end function next_segment_bytes

   !> Next heuristic segment size in bytes. prev_bytes is the previous
   !> capacity and need is the request. The result is at least need when
   !> need fits in a segment, and it never exceeds the segment cap. The
   !> doubling is skipped when prev_bytes is already more than half the
   !> cap, which is the overflow case for a signed 64-bit size.
   pure function capnp_grown_segment_bytes(prev_bytes, need) result(cap)
      integer(int64), intent(in) :: prev_bytes, need
      integer(int64) :: cap, doubled, max_bytes, prev
      max_bytes = MAX_SEGMENT_WORDS*CAPNP_WORD_BYTES
      prev = prev_bytes
      if (prev < 0_int64) prev = 0_int64
      if (prev > max_bytes/2_int64) then
         doubled = max_bytes
      else
         doubled = prev*2_int64
      end if
      if (need > doubled) then
         cap = need
      else
         cap = doubled
      end if
      if (cap > max_bytes) cap = max_bytes
   end function capnp_grown_segment_bytes

   !> Grow (or create) a segment's storage to at least cap_bytes, preserving
   !> contents. Indices stay valid: offsets are array indices, not addresses.
   !> Growing a view copies it into owned storage first. A failed allocate
   !> leaves the previous storage in place and returns CAPNP_ERR_ALLOC.
   subroutine seg_reserve(seg, cap_bytes, err)
      type(capnp_segment_t), intent(inout) :: seg
      integer(int64), intent(in) :: cap_bytes
      integer, intent(out) :: err
      integer(int8), pointer :: tmp(:)
      integer :: stat
      err = CAPNP_OK
      if (cap_bytes < 1_int64) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (.not. associated(seg%bytes)) then
         allocate (seg%bytes(0:cap_bytes - 1), stat=stat)
         if (stat /= 0) then
            err = CAPNP_ERR_ALLOC
            return
         end if
         seg%owned = .true.
         return
      end if
      if (size(seg%bytes, kind=int64) >= cap_bytes) return
      allocate (tmp(0:cap_bytes - 1), stat=stat)
      if (stat /= 0) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      if (seg%len > 0_int64) tmp(0:seg%len - 1) = seg%bytes(0:seg%len - 1)
      if (seg%owned) deallocate (seg%bytes)
      seg%bytes => tmp
      seg%owned = .true.
   end subroutine seg_reserve

end module capnp_arena
