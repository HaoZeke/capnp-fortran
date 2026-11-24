!> Canonicalization: our canonical bytes must equal `capnp convert
!> binary:canonical` output for the addressbook fixture, byte for byte,
!> and canonicalization must be idempotent.
module test_canonical
   use testdrive, only: unittest_type, error_type, check, test_failed, skip_test
   use capnp
   implicit none

   private
   public :: collect_canonical

contains

   subroutine collect_canonical(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      allocate (testsuite(7))
      testsuite(1)%name = "canonical"
      testsuite(1)%test => run_canonical
      testsuite(2)%name = "canonical-bool-padding"
      testsuite(2)%test => test_bool_padding
      testsuite(3)%name = "canonical-depth-limit"
      testsuite(3)%test => test_depth_limit
      testsuite(4)%name = "canonical-traversal-limit"
      testsuite(4)%test => test_traversal_limit
      testsuite(5)%name = "canonical-is-canonical-cases"
      testsuite(5)%test => test_is_canonical_cases
      testsuite(6)%name = "canonical-depth-boundary"
      testsuite(6)%test => test_depth_boundary
      testsuite(7)%name = "canonical-far-pointers"
      testsuite(7)%test => test_far_pointers
   end subroutine collect_canonical

   subroutine check_(error, cond, name)
      type(error_type), allocatable, intent(inout) :: error
      logical, intent(in) :: cond
      character(len=*), intent(in) :: name
      if (allocated(error)) return
      call check(error, cond, name)
   end subroutine check_

   function sp(off, dw, pw) result(w)
      integer, intent(in) :: off, dw, pw
      integer(int64) :: w
      w = wp_make_struct(int(off, int32), int(dw, int32), int(pw, int32))
   end function sp

   function lp(off, esize, count) result(w)
      integer, intent(in) :: off, esize, count
      integer(int64) :: w
      w = wp_make_list(int(off, int32), int(esize, int32), int(count, int64))
   end function lp

   !> Frame `words` as segment 1, optionally followed by a second segment
   !> of `second` zero words (`second` < 0 for none), and read it.
   subroutine load_words(words, second, msg, err, limit, depth)
      integer(int64), intent(in) :: words(:)
      integer, intent(in) :: second
      type(capnp_message_t), intent(inout) :: msg
      integer, intent(out) :: err
      integer(int64), intent(in), optional :: limit
      integer, intent(in), optional :: depth
      integer(int8), allocatable :: frame(:)
      integer :: hdr, n, i
      n = size(words)
      hdr = 8
      if (second >= 0) hdr = 16
      allocate (frame(0:hdr + 8*n + 8*max(second, 0) - 1))
      frame = 0_int8
      if (second >= 0) then
         call cp_put_i32(frame, 0_int64, 1_int32)
         call cp_put_i32(frame, 4_int64, int(n, int32))
         call cp_put_i32(frame, 8_int64, int(second, int32))
      else
         call cp_put_i32(frame, 4_int64, int(n, int32))
      end if
      do i = 1, n
         call cp_put_i64(frame, int(hdr + 8*(i - 1), int64), words(i))
      end do
      call capnp_deserialize_bytes(frame, msg, err, limit, depth)
   end subroutine load_words

   !> `capnp_is_canonical` on a framed message must give `want` and `want_err`.
   subroutine expect_is(error, name, words, second, want, want_err)
      type(error_type), allocatable, intent(inout) :: error
      character(len=*), intent(in) :: name
      integer(int64), intent(in) :: words(:)
      integer, intent(in) :: second, want_err
      logical, intent(in) :: want
      type(capnp_message_t), target :: msg
      logical :: got
      integer :: err
      call load_words(words, second, msg, err)
      got = capnp_is_canonical(msg, err)
      call check_(error, (got .eqv. want) .and. err == want_err, name)
      call capnp_message_free(msg)
   end subroutine expect_is

   !> `capnp_canonicalize` of a framed message must give exactly `want`.
   subroutine expect_canon(error, name, words, want)
      type(error_type), allocatable, intent(inout) :: error
      character(len=*), intent(in) :: name
      integer(int64), intent(in) :: words(:), want(:)
      type(capnp_message_t), target :: msg
      integer(int8), allocatable :: canon(:)
      integer :: err, i
      logical :: same
      call load_words(words, -1, msg, err)
      call capnp_canonicalize(msg, canon, err)
      call capnp_message_free(msg)
      same = err == CAPNP_OK
      if (same) same = size(canon) == 8*size(want)
      if (same) then
         do i = 1, size(want)
            if (cp_get_i64(canon, int(8*(i - 1), int64)) /= want(i)) same = .false.
         end do
      end if
      call check_(error, same, name)
   end subroutine expect_canon

   !> Every verdict below is what C++ 1.4.0 `MessageReader::isCanonical`
   !> returns for the same bytes: false (not an error) for far and
   !> capability pointers, an error only for what it fails to read.
   subroutine test_is_canonical_cases(error)
      type(error_type), allocatable, intent(out) :: error
      integer(int64), parameter :: abc = 6513249_int64 ! "abc" then zero bytes
      integer(int64) :: dirty
      dirty = abc + 281474976710656_int64 ! a stray byte at the end of the word
      call expect_is(error, 'is-canonical: plain struct', [sp(0, 1, 0), 1_int64], -1, .true., CAPNP_OK)
      call expect_is(error, 'is-canonical: null root', [0_int64], -1, .true., CAPNP_OK)
      call expect_is(error, 'is-canonical: empty struct root', [sp(-1, 0, 0)], -1, .true., CAPNP_OK)
      call expect_is(error, 'is-canonical: trailing word', [sp(0, 1, 0), 1_int64, 0_int64], -1, .false., CAPNP_OK)
      call expect_is(error, 'is-canonical: empty second segment', [sp(0, 1, 0), 1_int64], 0, .true., CAPNP_OK)
      call expect_is(error, 'is-canonical: second segment', [sp(0, 1, 0), 1_int64], 1, .false., CAPNP_OK)
      call expect_is(error, 'is-canonical: far root', &
                     [wp_make_far(.false., 1_int64, 0_int64), sp(0, 1, 0), 1_int64], -1, .false., CAPNP_OK)
      call expect_is(error, 'is-canonical: capability field', [sp(0, 0, 1), wp_make_cap(0_int64)], -1, .false., CAPNP_OK)
      call expect_is(error, 'is-canonical: unknown pointer kind', [sp(0, 0, 1), 7_int64], -1, .false., CAPNP_ERR_KIND)
      call expect_is(error, 'is-canonical: far to missing segment', &
                     [sp(0, 0, 1), wp_make_far(.false., 0_int64, 5_int64)], -1, .false., CAPNP_ERR_SEGMENT)
      call expect_is(error, 'is-canonical: far pad out of bounds', &
                     [sp(0, 0, 1), wp_make_far(.false., 9_int64, 0_int64)], -1, .false., CAPNP_ERR_BOUNDS)
      call expect_is(error, 'is-canonical: far to own segment', &
                     [sp(0, 0, 1), wp_make_far(.false., 2_int64, 0_int64), sp(0, 1, 0), 1_int64], -1, .false., CAPNP_OK)
      call expect_is(error, 'is-canonical: empty list far offset', [sp(0, 0, 1), lp(500, 2, 0)], -1, .true., CAPNP_OK)
      call expect_canon(error, 'canonicalize: empty list far offset', [sp(0, 0, 1), lp(500, 2, 0)], &
                        [sp(0, 0, 1), lp(0, 2, 0)])
      call expect_is(error, 'is-canonical: list out of bounds', [sp(0, 0, 1), lp(500, 2, 1)], -1, .false., CAPNP_ERR_BOUNDS)
      call expect_is(error, 'is-canonical: empty struct far offset', [sp(0, 0, 1), sp(500, 0, 0)], -1, .false., CAPNP_OK)
      call expect_is(error, 'is-canonical: byte list', [sp(0, 0, 1), lp(0, 2, 3), abc], -1, .true., CAPNP_OK)
      call expect_is(error, 'is-canonical: dirty byte padding', [sp(0, 0, 1), lp(0, 2, 3), dirty], -1, .false., CAPNP_OK)
      call expect_is(error, 'is-canonical: bit list', [sp(0, 0, 1), lp(0, 1, 3), 7_int64], -1, .true., CAPNP_OK)
      call expect_is(error, 'is-canonical: dirty bit padding', [sp(0, 0, 1), lp(0, 1, 3), 15_int64], -1, .false., CAPNP_OK)
      call expect_is(error, 'is-canonical: pointers out of preorder', &
                     [sp(0, 0, 2), lp(1, 2, 1), lp(0, 2, 1), 97_int64, 98_int64], -1, .false., CAPNP_OK)
   end subroutine test_is_canonical_cases

   !> Nesting limits at their exact edge. C++ 1.4.0 accepts `k` nested
   !> structs at limit `k`; a composite list also costs its element
   !> (`ListReader::getStructElement`), but only `isCanonical` walks
   !> through that element, so `canonicalize` is one level cheaper there.
   subroutine test_depth_boundary(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg
      integer(int64), allocatable :: chain(:)
      integer(int8), allocatable :: canon(:)
      integer(int64) :: dag(63)
      integer :: k, err, e_is, e_canon, i
      logical :: is_c

      do k = 64, 65
         allocate (chain(k + 1))
         chain = 0_int64
         do i = 1, k
            chain(i) = sp(0, 0, 1)
         end do
         chain(k) = sp(0, 1, 0)
         chain(k + 1) = 1_int64
         call load_words(chain, -1, msg, err)
         is_c = capnp_is_canonical(msg, e_is)
         call capnp_message_free(msg)
         call load_words(chain, -1, msg, err)
         call capnp_canonicalize(msg, canon, e_canon)
         call capnp_message_free(msg)
         if (k == 64) then
            call check_(error, is_c .and. e_is == CAPNP_OK .and. e_canon == CAPNP_OK, 'depth: 64 structs fit limit 64')
         else
            call check_(error, e_is == CAPNP_ERR_DEPTH .and. e_canon == CAPNP_ERR_DEPTH, 'depth: 65 structs exceed limit 64')
         end if
         deallocate (chain)
      end do

      call depth_probe([sp(0, 0, 1), lp(0, 7, 1), sp(1, 0, 1), sp(0, 1, 0), 1_int64], 3, e_is, e_canon)
      call check_(error, e_is == CAPNP_ERR_DEPTH .and. e_canon == CAPNP_OK, 'depth: composite list at limit 3')
      call depth_probe([sp(0, 0, 1), lp(0, 7, 1), sp(1, 0, 1), sp(0, 1, 0), 1_int64], 4, e_is, e_canon)
      call check_(error, e_is == CAPNP_OK .and. e_canon == CAPNP_OK, 'depth: composite list at limit 4')
      call depth_probe([sp(0, 0, 1), lp(0, 7, 1), sp(1, 0, 1), sp(0, 1, 0), 1_int64], 2, e_is, e_canon)
      call check_(error, e_is == CAPNP_ERR_DEPTH .and. e_canon == CAPNP_ERR_DEPTH, 'depth: composite list at limit 2')
      call depth_probe([sp(0, 0, 1), lp(0, 6, 1), sp(0, 1, 0), 1_int64], 2, e_is, e_canon)
      call check_(error, e_is == CAPNP_ERR_DEPTH .and. e_canon == CAPNP_ERR_DEPTH, 'depth: pointer list at limit 2')
      call depth_probe([sp(0, 0, 1), lp(0, 6, 1), sp(0, 1, 0), 1_int64], 3, e_is, e_canon)
      call check_(error, e_is == CAPNP_OK .and. e_canon == CAPNP_OK, 'depth: pointer list at limit 3')

      ! A 30-level DAG expands to 2**30 logical nodes. C++ answers false
      ! from the first mismatch instead of walking all of it.
      dag = 0_int64
      dag(1) = sp(0, 0, 2)
      do i = 0, 29
         dag(2 + 2*i) = sp(1, 0, 2)
         dag(3 + 2*i) = sp(0, 0, 2)
      end do
      call load_words(dag, -1, msg, err, limit=1000_int64)
      is_c = capnp_is_canonical(msg, e_is)
      call check_(error, (.not. is_c) .and. e_is == CAPNP_OK, 'depth: DAG is not canonical, and not an error')
      call capnp_message_free(msg)
   end subroutine test_depth_boundary

   subroutine depth_probe(words, depth, e_is, e_canon)
      integer(int64), intent(in) :: words(:)
      integer, intent(in) :: depth
      integer, intent(out) :: e_is, e_canon
      type(capnp_message_t), target :: msg
      integer(int8), allocatable :: canon(:)
      logical :: is_c
      integer :: err
      call load_words(words, -1, msg, err, depth=depth)
      is_c = capnp_is_canonical(msg, e_is)
      call capnp_message_free(msg)
      call load_words(words, -1, msg, err, depth=depth)
      call capnp_canonicalize(msg, canon, e_canon)
      call capnp_message_free(msg)
   end subroutine depth_probe

   subroutine run_canonical(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg, msg2
      integer(int8), allocatable :: raw(:), ref(:), ours(:), again(:), framed(:)
      integer :: err
      integer(int64) :: i

      call capnp_read_file('test/fixtures/addressbook.bin', raw, err)
      if (err /= CAPNP_OK) then
         call skip_test(error, 'fixtures not found (run from project root)')
         return
         end if
      call capnp_read_file('test/fixtures/addressbook.canonical.bin', ref, err)
      call check_(error, err == CAPNP_OK, 'canon: reference fixture read')

      call capnp_deserialize_bytes(raw, msg, err)
      call capnp_canonicalize(msg, ours, err)
      call check_(error, err == CAPNP_OK, 'canon: canonicalize runs')
      call check_(error, size(ours) == size(ref), 'canon: size matches capnp tool')
      if (size(ours) == size(ref)) then
         call check_(error, all(ours == ref), 'canon: bytes match capnp tool')
         if (.not. all(ours == ref)) then
            do i = 0_int64, size(ours, kind=int64) - 1_int64
               if (ours(i) /= ref(i)) then
                  print '(a,i0,a,i4,a,i4)', '  first diff at byte ', i, ': ours ', &
                     ours(i), ' ref ', ref(i)
                  exit
               end if
            end do
         end if
      else
         print '(a,i0,a,i0)', '  sizes: ours ', size(ours), ' ref ', size(ref)
      end if

      ! Idempotence: canonical(canonical(m)) == canonical(m). Wrap the raw
      ! segment in a single-segment framing to re-read it.
      allocate (framed(0:size(ours) + 7))
      framed = 0_int8
      framed(4) = int(mod(size(ours)/8, 256), int8)
      framed(5) = int(mod(size(ours)/8/256, 256), int8)
      framed(8:8 + size(ours) - 1) = ours
      call capnp_deserialize_bytes(framed, msg2, err)
      call capnp_canonicalize(msg2, again, err)
      call check_(error, err == CAPNP_OK .and. size(again) == size(ours), 'canon: idempotent size')
      call check_(error, all(again == ours), 'canon: idempotent bytes')

      call capnp_message_free(msg)
      call capnp_message_free(msg2)
   end subroutine run_canonical

   subroutine test_bool_padding(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg, read_msg, canon_msg, edge_msg
      type(capnp_ptr_t) :: root, bits, read_root, read_bits
      integer(int8), allocatable :: framed(:), bytes(:), canon(:)
      integer(int8) :: zero_seg(0:7), cap_frame(0:15)
      integer :: err
      logical :: canonical
      call capnp_message_init_builder(msg, err)
      root = capnp_new_struct(msg, 0, 1, err)
      bits = capnp_new_list(msg, CAPNP_SZ_BIT, 3_int64, err)
      call capnp_list_set_bool(bits, 0_int64, .true., err)
      call capnp_list_set_bool(bits, 1_int64, .true., err)
      call capnp_list_set_bool(bits, 2_int64, .true., err)
      msg%segs(bits%seg)%bytes(bits%off) = -1_int8
      call capnp_setp(root, 0, bits, err)
      call capnp_set_root(msg, root, err)
      call capnp_serialize_bytes(msg, bytes, err)
      call capnp_deserialize_bytes(bytes, read_msg, err)
      canonical = capnp_is_canonical(read_msg, err)
      call check_(error, err == CAPNP_OK .and. .not. canonical, 'canon-bool: padding is non-canonical')
      if (allocated(error)) return
      call capnp_canonicalize(read_msg, canon, err)
      call check_(error, err == CAPNP_OK, 'canon-bool: canonicalize')
      if (allocated(error)) return
      allocate (framed(0:size(canon) + 7))
      framed = 0_int8
      framed(4) = int(size(canon)/8, int8)
      framed(8:) = canon
      call capnp_deserialize_bytes(framed, canon_msg, err)
      read_root = capnp_root(canon_msg, err)
      read_bits = capnp_getp(read_root, 0, err)
      call check_(error, err == CAPNP_OK .and. canon_msg%segs(read_bits%seg)%bytes(read_bits%off) == 7_int8, &
                  'canon-bool: trailing bits masked')
      if (allocated(error)) return
      canonical = capnp_is_canonical(canon_msg, err)
      call check_(error, err == CAPNP_OK .and. canonical, 'canon-bool: predicate accepts output')
      if (allocated(error)) return

      zero_seg = 0_int8
      call capnp_deserialize_bytes(zero_seg, edge_msg, err)
      canonical = capnp_is_canonical(edge_msg, err)
      call check_(error, err == CAPNP_OK .and. .not. canonical, 'canon-bool: zero-word segment is non-canonical')
      call capnp_message_free(edge_msg)
      if (allocated(error)) return

      cap_frame = 0_int8
      cap_frame(4) = 1_int8
      cap_frame(8) = 3_int8
      call capnp_deserialize_bytes(cap_frame, edge_msg, err)
      canonical = capnp_is_canonical(edge_msg, err)
      call check_(error, err == CAPNP_OK .and. .not. canonical, 'canon-bool: capability root is non-canonical')
      call capnp_message_free(edge_msg)

      call capnp_message_free(msg)
      call capnp_message_free(read_msg)
      call capnp_message_free(canon_msg)
   end subroutine test_bool_padding

   subroutine test_depth_limit(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg, read_msg
      type(capnp_ptr_t) :: node, tail
      integer(int8), allocatable :: bytes(:), canon(:)
      integer :: err, i
      call capnp_message_init_builder(msg, err)
      tail = capnp_new_struct(msg, 0, 0, err)
      do i = 2, 70
         node = capnp_new_struct(msg, 0, 1, err)
         call capnp_setp(node, 0, tail, err)
         tail = node
      end do
      call capnp_set_root(msg, tail, err)
      call capnp_serialize_bytes(msg, bytes, err)
      call capnp_deserialize_bytes(bytes, read_msg, err, depth_limit=100)
      call capnp_canonicalize(read_msg, canon, err)
      call check_(error, err == CAPNP_OK .and. size(canon) > 0, 'canon-depth: reader limit is honored')
      call capnp_message_free(read_msg)
      if (allocated(error)) return

      call capnp_deserialize_bytes(bytes, read_msg, err, depth_limit=64)
      call capnp_canonicalize(read_msg, canon, err)
      call check_(error, err == CAPNP_ERR_DEPTH, 'canon-depth: default limit rejects depth 70')
      call capnp_message_free(msg)
      call capnp_message_free(read_msg)
   end subroutine test_depth_limit

   subroutine test_traversal_limit(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg, read_msg
      type(capnp_ptr_t) :: root, child
      integer(int8), allocatable :: bytes(:), canon(:), dag(:)
      integer :: err, i, base
      call capnp_message_init_builder(msg, err)
      root = capnp_new_struct(msg, 0, 1, err)
      child = capnp_new_struct(msg, 12, 0, err)
      call capnp_set_i64(child, 88_int64, 1_int64, err)
      call capnp_setp(root, 0, child, err)
      call capnp_set_root(msg, root, err)
      call capnp_serialize_bytes(msg, bytes, err)
      call capnp_deserialize_bytes(bytes, read_msg, err, traversal_words=15_int64)
      call capnp_canonicalize(read_msg, canon, err)
      call check_(error, err == CAPNP_OK .and. size(canon) == 112, 'canon-traversal: each word is charged once')
      call capnp_message_free(read_msg)
      if (allocated(error)) return

      ! 30-level binary DAG bomb (2^30 logical nodes) must abort live during measure.
      allocate (dag(0:511))
      dag = 0_int8
      call cp_put_i32(dag, 4_int64, 63_int32)
      call cp_put_i64(dag, 8_int64, wp_make_struct(0_int32, 0_int32, 2_int32))
      do i = 0, 29
         base = 8 + (1 + i*2)*8
         call cp_put_i64(dag, int(base, int64), wp_make_struct(1_int32, 0_int32, 2_int32))
         call cp_put_i64(dag, int(base + 8, int64), wp_make_struct(0_int32, 0_int32, 2_int32))
      end do
      call capnp_deserialize_bytes(dag, read_msg, err, traversal_words=1000_int64)
      call capnp_canonicalize(read_msg, canon, err)
      call check_(error, err == CAPNP_ERR_TRAVERSAL, 'canon-traversal: amplified DAG rejected live')
      call capnp_message_free(msg)
      call capnp_message_free(read_msg)
   end subroutine test_traversal_limit

   subroutine test_far_pointers(error)
      type(error_type), allocatable, intent(out) :: error
      type(capnp_message_t), target :: msg, out_msg
      integer(int8) :: frame(0:79), sfar(0:31), framed(0:23)
      integer(int8), allocatable :: canon(:)
      integer :: err
      logical :: is_c

      ! Double-far pad at seg 0 word 2 pointing to shorter seg 2 (1 word):
      ! `pointer_is_set` must read pad+1 from the pad segment, not seg 2.
      frame = 0_int8
      call cp_put_i32(frame, 0_int64, 2_int32) ! 3 segments
      call cp_put_i32(frame, 4_int64, 4_int32) ! seg 0: 4 words
      call cp_put_i32(frame, 8_int64, 0_int32) ! seg 1: 0 words
      call cp_put_i32(frame, 12_int64, 1_int32) ! seg 2: 1 word
      call cp_put_i64(frame, 16_int64, sp(0, 0, 1))
      call cp_put_i64(frame, 24_int64, wp_make_far(.true., 2_int64, 0_int64))
      call cp_put_i64(frame, 32_int64, wp_make_far(.false., 0_int64, 2_int64))
      call cp_put_i64(frame, 40_int64, sp(0, 1, 0))
      call capnp_deserialize_bytes(frame(0:55), msg, err)
      call check_(error, err == CAPNP_OK, 'canon-far: deserialize short-target double-far')
      is_c = capnp_is_canonical(msg, err)
      call check_(error, err == CAPNP_OK .and. .not. is_c, &
                  'canon-far: double-far reads tag from pad segment')
      call capnp_message_free(msg)
      if (allocated(error)) return

      ! Same layout with seg 2 padded to 4 words: invalid OTHER tag on seg 2
      ! must not trigger CAPNP_ERR_KIND, while invalid tag on pad word 1 must.
      call cp_put_i32(frame, 12_int64, 4_int32) ! seg 2: 4 words
      call cp_put_i64(frame, 72_int64, 7_int64) ! word 3 of seg 2: invalid OTHER
      call capnp_deserialize_bytes(frame, msg, err)
      is_c = capnp_is_canonical(msg, err)
      call check_(error, err == CAPNP_OK .and. .not. is_c, &
                  'canon-far: target segment noise ignored')
      call capnp_message_free(msg)
      if (allocated(error)) return

      call cp_put_i64(frame, 40_int64, 7_int64) ! pad word 1: invalid OTHER
      call cp_put_i64(frame, 72_int64, 0_int64)
      call capnp_deserialize_bytes(frame, msg, err)
      is_c = capnp_is_canonical(msg, err)
      call check_(error, err == CAPNP_ERR_KIND, &
                  'canon-far: invalid double-far tag rejected')
      call capnp_message_free(msg)
      if (allocated(error)) return

      ! Single-far to zero landing pad in struct's last slot canonicalizes
      ! to an empty struct (offset -1), never a trailing null pointer.
      sfar = 0_int8
      call cp_put_i32(sfar, 4_int64, 3_int32)
      call cp_put_i64(sfar, 8_int64, sp(0, 0, 1))
      call cp_put_i64(sfar, 16_int64, wp_make_far(.false., 2_int64, 0_int64))
      call cp_put_i64(sfar, 24_int64, 0_int64)
      call capnp_deserialize_bytes(sfar, msg, err)
      call capnp_canonicalize(msg, canon, err)
      call check_(error, err == CAPNP_OK .and. size(canon) == 16, &
                  'canon-far: single-far zero pad size')
      if (.not. allocated(error)) then
         call check_(error, cp_get_i64(canon, 8_int64) == sp(-1, 0, 0), &
                     'canon-far: single-far zero pad encodes empty struct')
      end if
      call capnp_message_free(msg)
      if (allocated(error)) return

      framed = 0_int8
      call cp_put_i32(framed, 4_int64, 2_int32)
      framed(8:23) = canon
      call capnp_deserialize_bytes(framed, out_msg, err)
      is_c = capnp_is_canonical(out_msg, err)
      call check_(error, err == CAPNP_OK .and. is_c, &
                  'canon-far: single-far zero pad output is canonical')
      call capnp_message_free(out_msg)
   end subroutine test_far_pointers

end module test_canonical
