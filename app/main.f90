!> capnpc-fortran: Cap'n Proto schema compiler plugin.
!>
!>   capnp compile -o /path/to/capnpc-fortran schema.capnp
!>
!> reads an unpacked CodeGeneratorRequest on stdin and writes one
!> <schema>_capnp.f90 module per requested file into the working directory.
!> A file argument replaces stdin for offline runs:
!>
!>   capnp compile -o- schema.capnp > req.bin && capnpc-fortran req.bin
program capnpc_fortran
   use capnp
   use capnpc_schema
   use capnpc_emit
   implicit none

   type(capnp_message_t), target :: msg
   type(capnp_ptr_t) :: root, files, rf
   integer(int8), allocatable :: bytes(:)
   character(len=:), allocatable :: fname
   character(len=4096) :: arg
   integer :: err, i, n

   if (command_argument_count() >= 1) then
      call get_command_argument(1, arg)
      call capnp_read_file(trim(arg), bytes, err)
   else
      call capnp_read_file('/dev/stdin', bytes, err)
   end if
   if (err /= CAPNP_OK) call die('cannot read CodeGeneratorRequest', err)

   ! Schema graphs run deep; give the reader generous guards.
   call capnp_deserialize_bytes(bytes, msg, err, &
                                traversal_words=1073741824_int64, depth_limit=256)
   if (err /= CAPNP_OK) call die('malformed CodeGeneratorRequest framing', err)

   root = capnp_root(msg, err)
   if (err /= CAPNP_OK) call die('cannot resolve request root', err)

   files = cgr_requested_files(root, err)
   if (err /= CAPNP_OK) call die('cannot read requestedFiles', err)
   n = int(capnp_list_len(files))
   if (n == 0) call die('no requested files', CAPNP_ERR_ARG)

   do i = 0, n - 1
      rf = capnp_list_get_struct(files, i, err)
      if (err /= CAPNP_OK) call die('cannot read requested file', err)
      call reqfile_filename(rf, fname, err)
      if (err /= CAPNP_OK) call die('cannot read filename', err)
      call emit_file(root, reqfile_id(rf), fname, err)
      if (err /= CAPNP_OK) call die('emit failed for '//fname, err)
   end do

contains

   subroutine die(what, code)
      character(len=*), intent(in) :: what
      integer, intent(in) :: code
      write (*, '(a,a,a,i0)') 'capnpc-fortran: ', what, ', err=', code
      error stop 1
   end subroutine die

end program capnpc_fortran
