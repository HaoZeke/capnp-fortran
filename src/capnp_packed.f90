!> Packed encoding, per https://capnproto.org/encoding.html#packing
!>
!> Each word gets a tag byte whose bit i marks byte i as nonzero; only the
!> nonzero bytes follow. Two escapes:
!>   tag 0x00: one count byte follows, the number of additional zero words.
!>   tag 0xff: the eight literal bytes follow, then one count byte, the
!>             number of following words copied verbatim.
module capnp_packed
   use capnp_kinds
   use capnp_endian, only: cp_u8, cp_i8b
   implicit none
   private

   public :: capnp_pack, capnp_unpack
   public :: capnp_unpacker_t, capnp_unpack_push, capnp_unpack_finish
   public :: capnp_packer_t, capnp_pack_push, capnp_pack_finish

   !> Incremental unpacker (capn-stream parity): feed arbitrary chunk
   !> boundaries with capnp_unpack_push; decoded words accumulate on the
   !> caller's output buffer.
   integer, parameter :: US_TAG = 0        !< expecting a tag byte
   integer, parameter :: US_PAYLOAD = 1    !< collecting flagged bytes of one word
   integer, parameter :: US_ZERO_COUNT = 2 !< expecting the zero-run count byte
   integer, parameter :: US_RAW_COUNT = 3  !< expecting the raw-run count byte
   integer, parameter :: US_RAW = 4        !< copying raw words verbatim

   type :: capnp_unpacker_t
      integer :: state = US_TAG
      integer :: tag = 0
      integer :: bit = 0                 ! next tag bit to inspect
      integer(int8) :: word(0:7) = 0_int8
      integer(int64) :: raw_left = 0_int64 ! raw bytes still owed
   end type capnp_unpacker_t

   !> Incremental packer: feed word-aligned chunks (splits anywhere) with
   !> capnp_pack_push, terminate with capnp_pack_finish. Output is
   !> byte-identical to whole-buffer capnp_pack.
   integer, parameter :: PM_IDLE = 0
   integer, parameter :: PM_ZERO_RUN = 1
   integer, parameter :: PM_LIT_RUN = 2

   type :: capnp_packer_t
      integer :: mode = PM_IDLE
      integer :: run = 0                     ! words in the current run
      integer(int8) :: lit(0:2039) = 0_int8  ! buffered literal-run words (255 max)
      integer(int8) :: partial(0:7) = 0_int8 ! carry of a split input word
      integer :: npart = 0
   end type capnp_packer_t

contains

   !> Pack a word-aligned buffer. After an 0xff tag the C++ encoder copies
   !> following words that have fewer than two zero bytes (capped at 255).
   !> One trailing null is not a pack win, so it stays in the literal run.
   subroutine capnp_pack(inb, outb, err)
      integer(int8), intent(in) :: inb(0:)
      integer(int8), allocatable, intent(out) :: outb(:)
      integer, intent(out) :: err
      integer(int8), allocatable :: buf(:)
      integer(int64) :: nwords, w, run, opos, j
      integer :: tag, nz, k, stat
      err = CAPNP_OK
      if (mod(size(inb, kind=int64), 8_int64) /= 0_int64) then
         err = CAPNP_ERR_ARG
         return
      end if
      nwords = size(inb, kind=int64)/8_int64
      ! Worst case: tag + 8 bytes per word, plus one count byte per 0xff run.
      allocate (buf(0:10_int64*max(nwords, 1_int64) - 1), stat=stat)
      if (stat /= 0) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      opos = 0_int64
      w = 0_int64
      do while (w < nwords)
         tag = 0
         nz = 0
         do k = 0, 7
            if (inb(w*8 + k) /= 0_int8) then
               tag = ibset(tag, k)
               nz = nz + 1
            end if
         end do
         buf(opos) = cp_i8b(tag)
         opos = opos + 1
         do k = 0, 7
            if (btest(tag, k)) then
               buf(opos) = inb(w*8 + k)
               opos = opos + 1
            end if
         end do
         w = w + 1
         if (tag == 0) then
            run = 0_int64
            do while (w + run < nwords .and. run < 255_int64)
               if (any(inb((w + run)*8:(w + run)*8 + 7) /= 0_int8)) exit
               run = run + 1
            end do
            buf(opos) = cp_i8b(int(run, int32))
            opos = opos + 1
            w = w + run
         else if (nz == 8) then
            run = 0_int64
            do while (w + run < nwords .and. run < 255_int64)
               if (word_zero_count(inb((w + run)*8:(w + run)*8 + 7)) >= 2) exit
               run = run + 1
            end do
            buf(opos) = cp_i8b(int(run, int32))
            opos = opos + 1
            do j = 0_int64, run*8_int64 - 1_int64
               buf(opos + j) = inb(w*8 + j)
            end do
            opos = opos + run*8_int64
            w = w + run
         end if
      end do
      allocate (outb(0:max(opos, 1_int64) - 1), stat=stat)
      if (stat /= 0) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      if (opos > 0_int64) outb(0:opos - 1) = buf(0:opos - 1)
      if (opos == 0_int64) then
         deallocate (outb)
         allocate (outb(0), stat=stat)
         if (stat /= 0) err = CAPNP_ERR_ALLOC
      end if
   end subroutine capnp_pack

   !> Unpack into a word-aligned buffer.
   subroutine capnp_unpack(inb, outb, err, max_words)
      integer(int8), intent(in) :: inb(0:)
      integer(int8), allocatable, intent(out) :: outb(:)
      integer, intent(out) :: err
      integer(int64), intent(in), optional :: max_words
      integer(int8), allocatable :: buf(:)
      integer(int64) :: ipos, opos, cap, n, j, limit
      integer :: tag, k, cnt, stat
      err = CAPNP_OK
      n = size(inb, kind=int64)
      limit = huge(0_int64)
      if (present(max_words)) then
         if (max_words < 0_int64) then
            err = CAPNP_ERR_ARG
            return
         end if
         if (max_words > ishft(huge(0_int64), -3)) then
            limit = huge(0_int64)
         else
            limit = max_words*CAPNP_WORD_BYTES
         end if
      end if
      cap = min(max(8_int64*n, 64_int64), max(limit, 1_int64))
      allocate (buf(0:cap - 1), stat=stat)
      if (stat /= 0) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      buf = 0_int8
      ipos = 0_int64
      opos = 0_int64
      do while (ipos < n)
         tag = cp_u8(inb(ipos))
         ipos = ipos + 1
         if (8_int64 > limit - opos) then
            err = CAPNP_ERR_TRAVERSAL
            return
         end if
         call ensure(buf, cap, opos + 8_int64, err)
         if (err /= CAPNP_OK) return
         do k = 0, 7
            if (btest(tag, k)) then
               if (ipos >= n) then
                  err = CAPNP_ERR_PACKED
                  return
               end if
               buf(opos + k) = inb(ipos)
               ipos = ipos + 1
            else
               buf(opos + k) = 0_int8
            end if
         end do
         opos = opos + 8
         if (tag == 0) then
            if (ipos >= n) then
               err = CAPNP_ERR_PACKED
               return
            end if
            cnt = cp_u8(inb(ipos))
            ipos = ipos + 1
            if (int(cnt, int64)*8_int64 > limit - opos) then
               err = CAPNP_ERR_TRAVERSAL
               return
            end if
            call ensure(buf, cap, opos + int(cnt, int64)*8_int64, err)
            if (err /= CAPNP_OK) return
            buf(opos:opos + int(cnt, int64)*8_int64 - 1) = 0_int8
            opos = opos + int(cnt, int64)*8_int64
         else if (tag == 255) then
            if (ipos >= n) then
               err = CAPNP_ERR_PACKED
               return
            end if
            cnt = cp_u8(inb(ipos))
            ipos = ipos + 1
            if (ipos + int(cnt, int64)*8_int64 > n) then
               err = CAPNP_ERR_PACKED
               return
            end if
            if (int(cnt, int64)*8_int64 > limit - opos) then
               err = CAPNP_ERR_TRAVERSAL
               return
            end if
            call ensure(buf, cap, opos + int(cnt, int64)*8_int64, err)
            if (err /= CAPNP_OK) return
            do j = 0_int64, int(cnt, int64)*8_int64 - 1_int64
               buf(opos + j) = inb(ipos + j)
            end do
            ipos = ipos + int(cnt, int64)*8_int64
            opos = opos + int(cnt, int64)*8_int64
         end if
      end do
      allocate (outb(0:max(opos, 1_int64) - 1), stat=stat)
      if (stat /= 0) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      if (opos > 0_int64) outb(0:opos - 1) = buf(0:opos - 1)
      if (opos == 0_int64) then
         deallocate (outb)
         allocate (outb(0), stat=stat)
         if (stat /= 0) err = CAPNP_ERR_ALLOC
      end if
   end subroutine capnp_unpack

   !> Feed one word-aligned-overall chunk into the incremental packer.
   !> Packed bytes append to out at outn (grown/updated). Chunks may split
   !> anywhere; call capnp_pack_finish to flush pending runs.
   subroutine capnp_pack_push(pk, chunk, out, outn, err)
      type(capnp_packer_t), intent(inout) :: pk
      integer(int8), intent(in) :: chunk(0:)
      integer(int8), allocatable, intent(inout) :: out(:)
      integer(int64), intent(inout) :: outn
      integer, intent(out) :: err
      integer(int8) :: word(0:7)
      integer(int64) :: ipos, n, cap
      integer :: take
      err = CAPNP_OK
      n = size(chunk, kind=int64)
      call out_init(out, outn, cap, n, err)
      if (err /= CAPNP_OK) return
      ipos = 0_int64
      do while (ipos < n)
         take = int(min(8_int64 - int(pk%npart, int64), n - ipos))
         pk%partial(pk%npart:pk%npart + take - 1) = chunk(ipos:ipos + take - 1)
         pk%npart = pk%npart + take
         ipos = ipos + take
         if (pk%npart < 8) return ! word continues in the next chunk
         word = pk%partial
         pk%npart = 0
         call pack_word(pk, word, out, outn, cap, err)
         if (err /= CAPNP_OK) return
      end do
   end subroutine capnp_pack_push

   !> Flush pending run state. The total input fed must have been a whole
   !> number of words.
   subroutine capnp_pack_finish(pk, out, outn, err)
      type(capnp_packer_t), intent(inout) :: pk
      integer(int8), allocatable, intent(inout) :: out(:)
      integer(int64), intent(inout) :: outn
      integer, intent(out) :: err
      integer(int64) :: cap
      err = CAPNP_OK
      call out_init(out, outn, cap, 1_int64, err)
      if (err /= CAPNP_OK) return
      if (pk%npart /= 0) then
         err = CAPNP_ERR_ARG
         return
      end if
      call flush_runs(pk, out, outn, cap, err)
   end subroutine capnp_pack_finish

   subroutine pack_word(pk, word, out, outn, cap, err)
      type(capnp_packer_t), intent(inout) :: pk
      integer(int8), intent(in) :: word(0:7)
      integer(int8), allocatable, intent(inout) :: out(:)
      integer(int64), intent(inout) :: outn, cap
      integer, intent(out) :: err
      integer :: tag, nz, k
      err = CAPNP_OK
      tag = 0
      nz = 0
      do k = 0, 7
         if (word(k) /= 0_int8) then
            tag = ibset(tag, k)
            nz = nz + 1
         end if
      end do
      if (tag == 0) then
         if (pk%mode == PM_LIT_RUN) then
            call flush_runs(pk, out, outn, cap, err)
            if (err /= CAPNP_OK) return
         end if
         if (pk%mode == PM_ZERO_RUN .and. pk%run == 256) then
            call flush_runs(pk, out, outn, cap, err)
            if (err /= CAPNP_OK) return
         end if
         if (pk%mode == PM_IDLE) then
            pk%mode = PM_ZERO_RUN
            pk%run = 1
         else
            pk%run = pk%run + 1
         end if
      else if (nz == 8) then
         if (pk%mode == PM_ZERO_RUN) then
            call flush_runs(pk, out, outn, cap, err)
            if (err /= CAPNP_OK) return
         end if
         if (pk%mode == PM_LIT_RUN .and. pk%run == 255) then
            call flush_runs(pk, out, outn, cap, err)
            if (err /= CAPNP_OK) return
         end if
         if (pk%mode == PM_IDLE) then
            call ensure(out, cap, outn + 9_int64, err)
            if (err /= CAPNP_OK) return
            out(outn) = -1_int8 ! 0xff
            out(outn + 1:outn + 8) = word
            outn = outn + 9
            pk%mode = PM_LIT_RUN
            pk%run = 0
         else
            pk%lit(pk%run*8:pk%run*8 + 7) = word
            pk%run = pk%run + 1
         end if
      else if (pk%mode == PM_LIT_RUN .and. (8 - nz) < 2 .and. pk%run < 255) then
         ! One zero byte: C++ keeps this word in the uncompressed span.
         pk%lit(pk%run*8:pk%run*8 + 7) = word
         pk%run = pk%run + 1
      else
         call flush_runs(pk, out, outn, cap, err)
         if (err /= CAPNP_OK) return
         call ensure(out, cap, outn + 9_int64, err)
         if (err /= CAPNP_OK) return
         out(outn) = cp_i8b(tag)
         outn = outn + 1
         do k = 0, 7
            if (btest(tag, k)) then
               out(outn) = word(k)
               outn = outn + 1
            end if
         end do
      end if
   end subroutine pack_word

   subroutine flush_runs(pk, out, outn, cap, err)
      type(capnp_packer_t), intent(inout) :: pk
      integer(int8), allocatable, intent(inout) :: out(:)
      integer(int64), intent(inout) :: outn, cap
      integer, intent(out) :: err
      err = CAPNP_OK
      select case (pk%mode)
      case (PM_ZERO_RUN)
         call ensure(out, cap, outn + 2_int64, err)
         if (err /= CAPNP_OK) return
         out(outn) = 0_int8
         out(outn + 1) = cp_i8b(pk%run - 1)
         outn = outn + 2
      case (PM_LIT_RUN)
         call ensure(out, cap, outn + 1_int64 + int(pk%run, int64)*8_int64, err)
         if (err /= CAPNP_OK) return
         out(outn) = cp_i8b(pk%run)
         outn = outn + 1
         if (pk%run > 0) then
            out(outn:outn + int(pk%run, int64)*8_int64 - 1) = pk%lit(0:pk%run*8 - 1)
            outn = outn + int(pk%run, int64)*8_int64
         end if
      end select
      pk%mode = PM_IDLE
      pk%run = 0
   end subroutine flush_runs

   subroutine out_init(out, outn, cap, hint, err)
      integer(int8), allocatable, intent(inout) :: out(:)
      integer(int64), intent(inout) :: outn
      integer(int64), intent(out) :: cap
      integer(int64), intent(in) :: hint
      integer, intent(out) :: err
      integer :: stat
      err = CAPNP_OK
      cap = 0_int64
      if (.not. allocated(out)) then
         allocate (out(0:max(2_int64*hint, 64_int64) - 1), stat=stat)
         if (stat /= 0) then
            err = CAPNP_ERR_ALLOC
            return
         end if
         out = 0_int8
         outn = 0_int64
      end if
      cap = size(out, kind=int64)
   end subroutine out_init

   !> Feed one chunk into the incremental unpacker. Decoded bytes append to
   !> out at position outn (both grown/updated); chunks may split anywhere,
   !> including inside a word's payload or before a count byte.
   subroutine capnp_unpack_push(u, chunk, out, outn, err)
      type(capnp_unpacker_t), intent(inout) :: u
      integer(int8), intent(in) :: chunk(0:)
      integer(int8), allocatable, intent(inout) :: out(:)
      integer(int64), intent(inout) :: outn
      integer, intent(out) :: err
      integer(int64) :: ipos, n, take, cap
      integer :: b, k, stat
      err = CAPNP_OK
      n = size(chunk, kind=int64)
      if (.not. allocated(out)) then
         allocate (out(0:max(8_int64*n, 64_int64) - 1), stat=stat)
         if (stat /= 0) then
            err = CAPNP_ERR_ALLOC
            return
         end if
         out = 0_int8
         outn = 0_int64
      end if
      cap = size(out, kind=int64)
      ipos = 0_int64
      do while (ipos < n)
         select case (u%state)
         case (US_TAG)
            u%tag = cp_u8(chunk(ipos))
            ipos = ipos + 1
            u%word = 0_int8
            u%bit = 0
            u%state = US_PAYLOAD
         case (US_PAYLOAD)
            do k = u%bit, 7
               if (btest(u%tag, k)) then
                  if (ipos >= n) then
                     u%bit = k
                     return ! word continues in the next chunk
                  end if
                  u%word(k) = chunk(ipos)
                  ipos = ipos + 1
               end if
            end do
            call ensure(out, cap, outn + 8_int64, err)
            if (err /= CAPNP_OK) return
            out(outn:outn + 7) = u%word
            outn = outn + 8
            if (u%tag == 0) then
               u%state = US_ZERO_COUNT
            else if (u%tag == 255) then
               u%state = US_RAW_COUNT
            else
               u%state = US_TAG
            end if
         case (US_ZERO_COUNT)
            b = cp_u8(chunk(ipos))
            ipos = ipos + 1
            call ensure(out, cap, outn + int(b, int64)*8_int64, err)
            if (err /= CAPNP_OK) return
            out(outn:outn + int(b, int64)*8_int64 - 1) = 0_int8
            outn = outn + int(b, int64)*8_int64
            u%state = US_TAG
         case (US_RAW_COUNT)
            b = cp_u8(chunk(ipos))
            ipos = ipos + 1
            u%raw_left = int(b, int64)*8_int64
            if (u%raw_left == 0_int64) then
               u%state = US_TAG
            else
               u%state = US_RAW
            end if
         case (US_RAW)
            take = min(u%raw_left, n - ipos)
            call ensure(out, cap, outn + take, err)
            if (err /= CAPNP_OK) return
            out(outn:outn + take - 1) = chunk(ipos:ipos + take - 1)
            outn = outn + take
            ipos = ipos + take
            u%raw_left = u%raw_left - take
            if (u%raw_left == 0_int64) u%state = US_TAG
         end select
      end do
   end subroutine capnp_unpack_push

   !> Reject a stream that ended inside a packed tag, word payload, or run.
   subroutine capnp_unpack_finish(u, err)
      type(capnp_unpacker_t), intent(in) :: u
      integer, intent(out) :: err
      err = CAPNP_OK
      if (u%state /= US_TAG) err = CAPNP_ERR_PACKED
   end subroutine capnp_unpack_finish

   pure integer function word_zero_count(word) result(z)
      integer(int8), intent(in) :: word(:)
      integer :: k
      z = 0
      do k = lbound(word, 1), ubound(word, 1)
         if (word(k) == 0_int8) z = z + 1
      end do
   end function word_zero_count

   !> Grow buf to hold need bytes. A host that cannot provide the memory
   !> is CAPNP_ERR_ALLOC, and buf and cap keep their old values.
   subroutine ensure(buf, cap, need, err)
      integer(int8), allocatable, intent(inout) :: buf(:)
      integer(int64), intent(inout) :: cap
      integer(int64), intent(in) :: need
      integer, intent(out) :: err
      integer(int8), allocatable :: tmp(:)
      integer(int64) :: ncap
      integer :: stat
      err = CAPNP_OK
      if (need <= cap) return
      ncap = cap
      do while (ncap < need)
         ncap = ncap*2_int64
      end do
      allocate (tmp(0:ncap - 1), stat=stat)
      if (stat /= 0) then
         err = CAPNP_ERR_ALLOC
         return
      end if
      tmp = 0_int8
      tmp(0:size(buf, kind=int64) - 1) = buf
      call move_alloc(tmp, buf)
      cap = ncap
   end subroutine ensure

end module capnp_packed
