!> Copy, total size, canonical form, text, and JSON from many threads at
!> once. Each iteration owns its messages and schema, and every result
!> must equal the one computed serially first. Built without OpenMP the
!> loop runs on one thread and checks the same answers. Arguments pick
!> cases by name; the default runs all of them.
program reentrancy
   use capnp
   use capnp_dynamic
   use capnp_text
   use capnp_json
   implicit none
   integer, parameter :: ROUNDS = 600
   !> Person.id is UInt32, so a string that is not a number stores zero
   !> and the decode reports a recoverable error.
   character(len=*), parameter :: bad_json = '{"people":[{"id":"x","name":"n"}]}'
   integer(int8), allocatable :: schema_bytes(:), ab_bytes(:)
   integer(int8), allocatable :: ref_canon(:), ref_capcanon(:)
   character(len=:), allocatable :: ref_text, ref_json
   integer(int64) :: ref_size
   character(len=*), parameter :: names(8) = [character(len=9) :: &
      'canonical', 'caps', 'copy', 'text', 'text-kind', 'json', 'json-kind', &
      'json-soft']
   integer, allocatable :: pick(:)
   character(len=16) :: arg
   integer :: err, i, k, nbad

   call capnp_read_file('test/fixtures/addressbook.cgr.bin', schema_bytes, err)
   if (err /= CAPNP_OK) error stop 'reentrancy: addressbook.cgr.bin'
   call capnp_read_file('test/fixtures/addressbook.bin', ab_bytes, err)
   if (err /= CAPNP_OK) error stop 'reentrancy: addressbook.bin'
   call references(err)
   if (err /= CAPNP_OK) error stop 'reentrancy: serial references'
   allocate (pick(0))
   do i = 1, command_argument_count()
      call get_command_argument(i, arg)
      k = findloc(names, trim(arg), 1)
      if (k == 0) error stop 'reentrancy: unknown case'
      pick = [pick, k]
   end do
   if (size(pick) == 0) pick = [(k, k=1, size(names))]

   nbad = 0
   !$omp parallel do schedule(dynamic, 1) reduction(+:nbad)
   do i = 1, ROUNDS
      if (.not. one_round(pick(mod(i, size(pick)) + 1))) nbad = nbad + 1
   end do
   !$omp end parallel do
   if (nbad /= 0) then
      print '(a,i0,a,i0)', 'reentrancy: ', nbad, ' bad rounds of ', ROUNDS
      error stop 1
   end if
   print '(a,i0,a)', 'reentrancy: ', ROUNDS, ' rounds agree with the serial results'
   deallocate (schema_bytes, ab_bytes, ref_canon, ref_capcanon, ref_text, ref_json, pick)

contains

   subroutine references(err)
      integer, intent(out) :: err
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg, capmsg
      type(capnp_ptr_t) :: root
      integer :: idx
      call capnp_dyn_load(schema, schema_bytes, err)
      if (err /= CAPNP_OK) return
      idx = capnp_dyn_find(schema, 'AddressBook', err)
      if (err /= CAPNP_OK) return
      call capnp_deserialize_bytes(ab_bytes, msg, err)
      if (err /= CAPNP_OK) return
      root = capnp_root(msg, err)
      if (err /= CAPNP_OK) return
      ref_size = capnp_total_size(root, err)
      if (err /= CAPNP_OK) return
      call capnp_canonicalize(msg, ref_canon, err)
      if (err /= CAPNP_OK) return
      call cap_message(capmsg, err)
      if (err /= CAPNP_OK) return
      call capnp_canonicalize(capmsg, ref_capcanon, err, null_caps=.true.)
      call capnp_message_free(capmsg)
      if (err /= CAPNP_OK) return
      call kind_message(capmsg, root, err)
      if (err /= CAPNP_OK) return
      call capnp_text_encode(schema, idx, root, ref_text, err)
      if (err /= CAPNP_ERR_KIND) error stop 'reentrancy: text kind reference'
      call capnp_json_encode(schema, idx, root, ref_json, err)
      if (err /= CAPNP_ERR_KIND) error stop 'reentrancy: json kind reference'
      call capnp_message_free(capmsg)
      root = capnp_root(msg, err)
      if (err /= CAPNP_OK) return
      call capnp_text_encode(schema, idx, root, ref_text, err)
      if (err /= CAPNP_OK) return
      call capnp_json_encode(schema, idx, root, ref_json, err)
      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end subroutine references

   !> A struct whose second pointer is a capability.
   subroutine cap_message(msg, err)
      type(capnp_message_t), intent(inout), target :: msg
      integer, intent(out) :: err
      type(capnp_ptr_t) :: root, cap, child
      call capnp_message_init_builder(msg, err)
      if (err /= CAPNP_OK) return
      root = capnp_new_struct(msg, 1, 2, err)
      if (err /= CAPNP_OK) return
      call capnp_set_root(msg, root, err)
      if (err /= CAPNP_OK) return
      child = capnp_new_struct(msg, 1, 0, err)
      if (err /= CAPNP_OK) return
      call capnp_set_i64(child, 0_int64, 42_int64, err)
      if (err /= CAPNP_OK) return
      call capnp_setp(root, 0, child, err)
      if (err /= CAPNP_OK) return
      cap = capnp_ptr_t()
      cap%msg => msg
      cap%kind = CAPNP_PK_CAP
      call capnp_setp(root, 1, cap, err)
   end subroutine cap_message

   !> An AddressBook whose people pointer is a struct. Both encoders
   !> print what they can and report a pointer of the wrong kind.
   subroutine kind_message(msg, root, err)
      type(capnp_message_t), intent(inout), target :: msg
      type(capnp_ptr_t), intent(out) :: root
      integer, intent(out) :: err
      type(capnp_ptr_t) :: wrong
      call capnp_message_init_builder(msg, err)
      if (err /= CAPNP_OK) return
      root = capnp_new_struct(msg, 0, 1, err)
      if (err /= CAPNP_OK) return
      call capnp_set_root(msg, root, err)
      if (err /= CAPNP_OK) return
      wrong = capnp_new_struct(msg, 1, 0, err)
      if (err /= CAPNP_OK) return
      call capnp_setp(root, 0, wrong, err)
   end subroutine kind_message

   function one_round(c) result(ok)
      integer, intent(in) :: c
      logical :: ok
      type(capnp_dyn_schema_t), target :: schema
      type(capnp_message_t), target :: msg, other, capmsg
      type(capnp_ptr_t) :: root, copied
      integer(int8), allocatable :: canon(:)
      character(len=:), allocatable :: text
      logical :: saw, hard
      integer :: err, idx
      ok = .false.
      call capnp_dyn_load(schema, schema_bytes, err)
      if (err /= CAPNP_OK) return
      idx = capnp_dyn_find(schema, 'AddressBook', err)
      if (err /= CAPNP_OK) return
      call capnp_deserialize_bytes(ab_bytes, msg, err)
      if (err /= CAPNP_OK) return
      root = capnp_root(msg, err)
      if (err /= CAPNP_OK) return
      select case (c)
      case (1)
         call capnp_canonicalize(msg, canon, err)
         ok = err == CAPNP_OK .and. same_bytes(canon, ref_canon)
      case (2)
         call cap_message(capmsg, err)
         if (err == CAPNP_OK) then
            call capnp_canonicalize(capmsg, canon, err, null_caps=.true., saw_cap=saw)
            ok = err == CAPNP_OK .and. saw .and. same_bytes(canon, ref_capcanon)
         end if
         if (ok) then
            call capnp_canonicalize(capmsg, canon, err)
            ok = err == CAPNP_ERR_KIND
         end if
         call capnp_message_free(capmsg)
      case (3)
         call capnp_message_init_builder(other, err)
         if (err == CAPNP_OK) copied = capnp_copy(other, root, err)
         if (err == CAPNP_OK) call capnp_set_root(other, copied, err)
         if (err == CAPNP_OK) ok = capnp_total_size(copied, err) == ref_size
         if (ok) then
            call capnp_canonicalize(other, canon, err)
            ok = err == CAPNP_OK .and. same_bytes(canon, ref_canon)
         end if
         call capnp_message_free(other)
      case (4)
         call capnp_text_encode(schema, idx, root, text, err)
         ok = err == CAPNP_OK .and. text == ref_text
         if (ok) then
            call capnp_text_decode(schema, idx, ref_text, other, err, hard_failed=hard)
            ok = err == CAPNP_OK .and. .not. hard
         end if
         if (ok) then
            copied = capnp_root(other, err)
            if (err == CAPNP_OK) call capnp_text_encode(schema, idx, copied, text, err)
            ok = err == CAPNP_OK .and. text == ref_text
         end if
         call capnp_message_free(other)
      case (5)
         call kind_message(other, copied, err)
         if (err == CAPNP_OK) call capnp_text_encode(schema, idx, copied, text, err)
         ok = err == CAPNP_ERR_KIND
         call capnp_message_free(other)
      case (6)
         call capnp_json_encode(schema, idx, root, text, err)
         ok = err == CAPNP_OK .and. text == ref_json
         if (ok) then
            call capnp_json_decode(schema, idx, ref_json, other, err, number_failed=saw)
            ok = err == CAPNP_OK .and. .not. saw
         end if
         if (ok) then
            copied = capnp_root(other, err)
            if (err == CAPNP_OK) call capnp_json_encode(schema, idx, copied, text, err)
            ok = err == CAPNP_OK .and. text == ref_json
         end if
         call capnp_message_free(other)
      case (7)
         call kind_message(other, copied, err)
         if (err == CAPNP_OK) call capnp_json_encode(schema, idx, copied, text, err)
         ok = err == CAPNP_ERR_KIND
         call capnp_message_free(other)
      case default
         call capnp_json_decode(schema, idx, bad_json, other, err, number_failed=saw)
         ok = err == CAPNP_ERR_ARG .and. saw .and. capnp_json_number_failed()
         call capnp_message_free(other)
      end select
      call capnp_message_free(msg)
      call capnp_dyn_free(schema)
   end function one_round

   pure function same_bytes(a, b) result(y)
      integer(int8), intent(in) :: a(:), b(:)
      logical :: y
      y = size(a) == size(b)
      if (y) y = all(a == b)
   end function same_bytes

end program reentrancy
