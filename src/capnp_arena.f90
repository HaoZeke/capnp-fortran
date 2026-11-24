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
   public :: capnp_arena_take_pad
   public :: capnp_segment_view
   public :: capnp_builder_options_t
   public :: capnp_grown_segment_bytes
   public :: capnp_message_nbytes, capnp_host_count_ok

   integer(int64), parameter :: DEFAULT_FIRST_WORDS = CAPNP_SUGGESTED_FIRST_SEGMENT_WORDS
   integer(int64), parameter :: MAX_SEGMENT_WORDS = 536870912_int64

   type :: capnp_segment_t
      !> Storage, 0-based. Capacity is size(bytes); len is the used prefix.
      !> owned segments are deallocated by capnp_message_free; views alias
      !> caller memory, which must outlive the message.
      integer(int8), pointer :: bytes(:) => null()
      integer(int64) :: len = 0_int64
      logical :: owned = .false.
      !> Word 0 of a segment opened for an object that did not fit in the
      !> previous one. The far pointer's landing pad is that word, and the
      !> object starts at word 1, matching capnp 1.4.0.
      logical :: far_pad = .false.
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
      !> Next heuristic segment size in words. MallocMessageBuilder starts
      !> this at the first segment's size and adds each later segment's
      !> allocated size, so the series is 1024, 1024, 2048, 4096, ...
      integer(int64) :: next_words = CAPNP_SUGGESTED_FIRST_SEGMENT_WORDS
      !> Landing pads reserved immediately before an object, for a far
      !> pointer that has not been written yet. Word indexes, 0-based.
      integer, allocatable :: pad_seg(:)
      integer(int64), allocatable :: pad_word(:)
      integer :: npad = 0
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
      ! Builders match MallocMessageBuilder: no nesting cap. Readers keep
      ! the default of 64 unless the caller passes a reader option.
      msg%depth_limit = huge(0)
      msg%alloc_strategy = strategy
      msg%segment_words = preferred
      msg%next_words = cap
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
            if (msg%segs(i)%owned .and. associated(msg%segs(i)%bytes)) &
               deallocate (msg%segs(i)%bytes)
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
      msg%next_words = DEFAULT_FIRST_WORDS
      if (allocated(msg%pad_seg)) deallocate (msg%pad_seg)
      if (allocated(msg%pad_word)) deallocate (msg%pad_word)
      msg%npad = 0
   end subroutine capnp_message_free

   !> Allocate nwords zeroed words. Without from_seg, tries the last
   !> segment and otherwise appends one. With from_seg, follows
   !> MallocMessageBuilder: the pointer's own segment first (a near
   !> pointer), then the latest segment with a landing-pad word in front
   !> of the object, then a new segment whose first word is that pad.
   !> Returns the segment index and byte offset of the object.
   subroutine capnp_arena_alloc(msg, nwords, seg_idx, byte_off, err, from_seg)
      type(capnp_message_t), intent(inout) :: msg
      integer(int64), intent(in) :: nwords
      integer, intent(out) :: seg_idx
      integer(int64), intent(out) :: byte_off
      integer, intent(out) :: err
      integer, intent(in), optional :: from_seg
      integer(int64) :: need, cap
      logical :: force_new
      err = CAPNP_OK
      seg_idx = 0
      byte_off = 0_int64
      force_new = .false.
      if (.not. msg%is_builder .or. nwords < 0_int64 .or. nwords > MAX_SEGMENT_WORDS) then
         err = CAPNP_ERR_ARG
         return
      end if
      need = nwords*CAPNP_WORD_BYTES
      if (present(from_seg)) then
         if (from_seg >= 1 .and. from_seg <= msg%nsegs) then
            if (seg_try(msg%segs(from_seg), nwords, byte_off)) then
               seg_idx = from_seg
               return
            end if
         end if
         ! The object did not fit beside the pointer. The next try is one
         ! extra word so the landing pad can sit immediately in front.
         if (nwords < MAX_SEGMENT_WORDS .and. msg%nsegs >= 1) then
            if (seg_try(msg%segs(msg%nsegs), nwords + 1_int64, byte_off)) then
               seg_idx = msg%nsegs
               call note_pad(msg, seg_idx, byte_off/CAPNP_WORD_BYTES, err)
               if (err /= CAPNP_OK) return
               byte_off = byte_off + CAPNP_WORD_BYTES
               return
            end if
         end if
         force_new = .true.
      end if
      if (.not. force_new) then
         associate (last => msg%segs(msg%nsegs))
            if (last%len + need <= size(last%bytes, kind=int64)) then
               byte_off = last%len
               if (need > 0_int64) last%bytes(last%len:last%len + need - 1) = 0_int8
               last%len = last%len + need
               seg_idx = msg%nsegs
               return
            end if
         end associate
      end if
      ! The new segment leads with the far-pointer landing pad, then the
      ! object. The segment is sized for both, as capnp 1.4.0 does.
      if (nwords < MAX_SEGMENT_WORDS) then
         cap = next_segment_bytes(msg, need + CAPNP_WORD_BYTES)
      else
         cap = next_segment_bytes(msg, need)
      end if
      call msg_append_segment(msg, cap, err)
      if (err /= CAPNP_OK) return
      if (msg%alloc_strategy /= CAPNP_ALLOC_FIXED_SIZE) then
         call bump_next_words(msg, cap/CAPNP_WORD_BYTES)
      end if
      seg_idx = msg%nsegs
      if (nwords < MAX_SEGMENT_WORDS .and. &
          size(msg%segs(seg_idx)%bytes, kind=int64) >= need + CAPNP_WORD_BYTES) then
         byte_off = CAPNP_WORD_BYTES
         msg%segs(seg_idx)%bytes(0:need + CAPNP_WORD_BYTES - 1) = 0_int8
         msg%segs(seg_idx)%len = need + CAPNP_WORD_BYTES
         msg%segs(seg_idx)%far_pad = .true.
      else
         byte_off = 0_int64
         msg%segs(seg_idx)%bytes(0:need - 1) = 0_int8
         msg%segs(seg_idx)%len = need
      end if
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
      integer(int64) :: need, used
      err = CAPNP_OK
      byte_off = 0_int64
      if (.not. msg%is_builder .or. seg_idx < 1 .or. seg_idx > msg%nsegs) then
         err = CAPNP_ERR_ARG
         return
      end if
      ! message.c++ refuses a segment above MAX_SEGMENT_WORDS before it
      ! allocates. A negative count is not a size.
      if (nwords < 0_int64) then
         err = CAPNP_ERR_ARG
         return
      end if
      if (nwords > MAX_SEGMENT_WORDS) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      associate (seg => msg%segs(seg_idx))
         used = (max(seg%len, 0_int64) + CAPNP_WORD_BYTES - 1_int64)/CAPNP_WORD_BYTES
         if (used > MAX_SEGMENT_WORDS - nwords) then
            err = CAPNP_ERR_ALLOC
            return
         end if
         need = nwords*CAPNP_WORD_BYTES
         if (seg%len + need > size(seg%bytes, kind=int64)) then
            if (msg%alloc_strategy == CAPNP_ALLOC_FIXED_SIZE) then
               call seg_reserve(seg, seg%len + need, err)
            else
               call seg_reserve(seg, capnp_grown_segment_bytes( &
                  size(seg%bytes, kind=int64), seg%len + need), err)
            end if
            if (err /= CAPNP_OK) return
         end if
         if (need > 0_int64 .and. seg%len + need > size(seg%bytes, kind=int64)) then
            err = CAPNP_ERR_ALLOC
            return
         end if
         byte_off = seg%len
         if (need > 0_int64) then
            seg%bytes(seg%len:seg%len + need - 1) = 0_int8
            seg%len = seg%len + need
         end if
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
            tmp(i)%far_pad = msg%segs(i)%far_pad
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
         cap = heuristic_segment_bytes(msg%next_words, need)
      end if
   end function next_segment_bytes

   !> Bytes for the next heuristic segment. preferred_words is the running
   !> nextSize; need is the request in bytes. The allocated size is the
   !> larger of the two, and it never passes the segment cap.
   pure function heuristic_segment_bytes(preferred_words, need) result(cap)
      integer(int64), intent(in) :: preferred_words, need
      integer(int64) :: cap, words, req
      words = preferred_words
      if (words < 0_int64) words = 0_int64
      if (words > MAX_SEGMENT_WORDS) words = MAX_SEGMENT_WORDS
      if (need > 0_int64) then
         req = (need + CAPNP_WORD_BYTES - 1_int64)/CAPNP_WORD_BYTES
      else
         req = 0_int64
      end if
      if (req > words) words = req
      if (words > MAX_SEGMENT_WORDS) words = MAX_SEGMENT_WORDS
      if (words < 1_int64) words = 1_int64
      cap = words*CAPNP_WORD_BYTES
   end function heuristic_segment_bytes

   !> nextSize += allocated, saturating at the segment cap. This is the
   !> step MallocMessageBuilder takes after every segment except the first.
   subroutine bump_next_words(msg, allocated_words)
      type(capnp_message_t), intent(inout) :: msg
      integer(int64), intent(in) :: allocated_words
      integer(int64) :: add
      add = allocated_words
      if (add < 0_int64) add = 0_int64
      if (msg%next_words < 0_int64) msg%next_words = 0_int64
      if (add >= MAX_SEGMENT_WORDS .or. msg%next_words > MAX_SEGMENT_WORDS - add) then
         msg%next_words = MAX_SEGMENT_WORDS
      else
         msg%next_words = msg%next_words + add
      end if
   end subroutine bump_next_words

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

   !> Take nwords from the unused tail of seg. A zero-word request
   !> succeeds without moving the tail, including when the segment is full.
   function seg_try(seg, nwords, byte_off) result(ok)
      type(capnp_segment_t), intent(inout) :: seg
      integer(int64), intent(in) :: nwords
      integer(int64), intent(out) :: byte_off
      logical :: ok
      integer(int64) :: need, cap
      ok = .false.
      byte_off = 0_int64
      if (.not. associated(seg%bytes) .or. nwords < 0_int64) return
      need = nwords*CAPNP_WORD_BYTES
      cap = size(seg%bytes, kind=int64)
      if (seg%len < 0_int64 .or. seg%len > cap) return
      if (need > cap - seg%len) return
      byte_off = seg%len
      if (need > 0_int64) seg%bytes(seg%len:seg%len + need - 1) = 0_int8
      seg%len = seg%len + need
      ok = .true.
   end function seg_try

   subroutine note_pad(msg, seg, word, err)
      type(capnp_message_t), intent(inout) :: msg
      integer, intent(in) :: seg
      integer(int64), intent(in) :: word
      integer, intent(out) :: err
      integer, allocatable :: snew(:)
      integer(int64), allocatable :: wnew(:)
      integer :: n, stat
      err = CAPNP_OK
      if (.not. allocated(msg%pad_seg)) then
         allocate (msg%pad_seg(8), msg%pad_word(8), stat=stat)
         if (stat /= 0) then
            err = CAPNP_ERR_ALLOC
            return
         end if
         msg%npad = 0
      else if (msg%npad >= size(msg%pad_seg)) then
         n = size(msg%pad_seg)*2
         allocate (snew(n), wnew(n), stat=stat)
         if (stat /= 0) then
            err = CAPNP_ERR_ALLOC
            return
         end if
         snew(1:msg%npad) = msg%pad_seg(1:msg%npad)
         wnew(1:msg%npad) = msg%pad_word(1:msg%npad)
         call move_alloc(snew, msg%pad_seg)
         call move_alloc(wnew, msg%pad_word)
      end if
      msg%npad = msg%npad + 1
      msg%pad_seg(msg%npad) = seg
      msg%pad_word(msg%npad) = word
   end subroutine note_pad

   !> Claim a landing pad recorded in front of an object. found is false
   !> when no such pad is waiting.
   function capnp_arena_take_pad(msg, seg, word, pad_off) result(found)
      type(capnp_message_t), intent(inout) :: msg
      integer, intent(in) :: seg
      integer(int64), intent(in) :: word
      integer(int64), intent(out) :: pad_off
      logical :: found
      integer :: i
      found = .false.
      pad_off = 0_int64
      if (word < 0_int64 .or. msg%npad < 1) return
      do i = 1, msg%npad
         if (msg%pad_seg(i) == seg .and. msg%pad_word(i) == word) then
            pad_off = word*CAPNP_WORD_BYTES
            msg%pad_seg(i) = msg%pad_seg(msg%npad)
            msg%pad_word(i) = msg%pad_word(msg%npad)
            msg%npad = msg%npad - 1
            found = .true.
            return
         end if
      end do
   end function capnp_arena_take_pad

   !> Sum of segment lengths. Saturates at huge(int64) if the sum overflows.
   pure function capnp_message_nbytes(msg) result(n)
      type(capnp_message_t), intent(in) :: msg
      integer(int64) :: n
      integer :: i
      n = 0_int64
      if (.not. allocated(msg%segs)) return
      do i = 1, msg%nsegs
         if (msg%segs(i)%len <= 0_int64) cycle
         if (n > huge(n) - msg%segs(i)%len) then
            n = huge(n)
            return
         end if
         n = n + msg%segs(i)%len
      end do
   end function capnp_message_nbytes

   !> True when n is a count that can be stored. A real element is at least
   !> one byte, so a count past the message size is zero-size amplification.
   pure function capnp_host_count_ok(n, nbytes) result(ok)
      integer(int64), intent(in) :: n, nbytes
      logical :: ok
      ok = n >= 0_int64 .and. nbytes >= 0_int64 .and. n <= nbytes
   end function capnp_host_count_ok

end module capnp_arena
