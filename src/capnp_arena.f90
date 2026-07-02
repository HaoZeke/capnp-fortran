!> Segments and the message arena. A message owns a set of flat byte
!> segments; builders allocate zeroed word runs from them, readers either
!> copy received bytes or view them in place (zero-copy, capn_init_mem
!> parity). Segment storage is a pointer array so a view can alias caller
!> memory; `owned` records who deallocates.
module capnp_arena
   use capnp_kinds, only: int8, int64, CAPNP_OK, CAPNP_ERR_ALLOC, CAPNP_ERR_ARG, &
                          CAPNP_WORD_BYTES, CAPNP_DEFAULT_TRAVERSAL_WORDS, &
                          CAPNP_DEFAULT_DEPTH_LIMIT
   implicit none
   private

   public :: capnp_segment_t, capnp_message_t
   public :: capnp_message_init_builder, capnp_message_free
   public :: capnp_arena_alloc, capnp_arena_alloc_in
   public :: capnp_segment_view

   integer(int64), parameter :: DEFAULT_FIRST_WORDS = 1024_int64
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
   end type capnp_message_t

contains

   !> Start a builder message. Word 0 of segment 1 is reserved and zeroed for
   !> the root pointer. first_words bounds the first segment's capacity; a
   !> small value forces multi-segment messages (and thus far pointers) early.
   subroutine capnp_message_init_builder(msg, err, first_words)
      type(capnp_message_t), intent(inout) :: msg
      integer, intent(out) :: err
      integer(int64), intent(in), optional :: first_words
      integer(int64) :: cap
      err = CAPNP_OK
      call capnp_message_free(msg)
      cap = DEFAULT_FIRST_WORDS
      if (present(first_words)) cap = max(1_int64, first_words)
      msg%is_builder = .true.
      allocate (msg%segs(4))
      msg%nsegs = 1
      call seg_reserve(msg%segs(1), cap*CAPNP_WORD_BYTES)
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
      ! Fresh segment: at least double the previous capacity, at least need.
      cap = max(need, 2_int64*size(msg%segs(msg%nsegs)%bytes, kind=int64))
      call msg_append_segment(msg, cap)
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
            call seg_reserve(seg, max(2_int64*size(seg%bytes, kind=int64), seg%len + need))
         end if
         byte_off = seg%len
         seg%bytes(seg%len:seg%len + need - 1) = 0_int8
         seg%len = seg%len + need
      end associate
   end subroutine capnp_arena_alloc_in

   subroutine msg_append_segment(msg, cap_bytes)
      type(capnp_message_t), intent(inout) :: msg
      integer(int64), intent(in) :: cap_bytes
      type(capnp_segment_t), allocatable :: tmp(:)
      integer :: i
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
      call seg_reserve(msg%segs(msg%nsegs), cap_bytes)
   end subroutine msg_append_segment

   !> Grow (or create) a segment's storage to at least cap_bytes, preserving
   !> contents. Indices stay valid: offsets are array indices, not addresses.
   !> Growing a view copies it into owned storage first.
   subroutine seg_reserve(seg, cap_bytes)
      type(capnp_segment_t), intent(inout) :: seg
      integer(int64), intent(in) :: cap_bytes
      integer(int8), pointer :: tmp(:)
      if (.not. associated(seg%bytes)) then
         allocate (seg%bytes(0:cap_bytes - 1))
         seg%owned = .true.
         return
      end if
      if (size(seg%bytes, kind=int64) >= cap_bytes) return
      allocate (tmp(0:cap_bytes - 1))
      if (seg%len > 0_int64) tmp(0:seg%len - 1) = seg%bytes(0:seg%len - 1)
      if (seg%owned) deallocate (seg%bytes)
      seg%bytes => tmp
      seg%owned = .true.
   end subroutine seg_reserve

end module capnp_arena
