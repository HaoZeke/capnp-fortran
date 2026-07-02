!> Hand-written model of what capnpc-fortran emits for the canonical
!> addressbook.capnp. Field offsets and section sizes are what `capnp
!> compile` assigns:
!>
!>   struct Person {            # 1 data word, 4 pointers
!>     id @0 :UInt32;           # data byte 0
!>     name @1 :Text;           # ptr 0
!>     email @2 :Text;          # ptr 1
!>     phones @3 :List(PhoneNumber);  # ptr 2
!>     employment :union {      # discriminant u16 at data byte 4
!>       unemployed @4 :Void;
!>       employer @5 :Text;     # ptr 3
!>       school @6 :Text;       # ptr 3
!>       selfEmployed @7 :Void;
!>     }
!>     struct PhoneNumber {     # 1 data word, 1 pointer
!>       number @0 :Text;       # ptr 0
!>       type @1 :Type;         # enum u16 at data byte 0
!>     }
!>   }
!>   struct AddressBook { people @0 :List(Person); }  # 0 data, 1 pointer
module addressbook_schema
   use capnp
   use capnp_union, only: capnp_which, capnp_set_which
   implicit none
   private

   public :: person_t, phone_t, addressbook_t
   public :: PHONE_TYPE_MOBILE, PHONE_TYPE_HOME, PHONE_TYPE_WORK
   public :: EMPLOYMENT_UNEMPLOYED, EMPLOYMENT_EMPLOYER, EMPLOYMENT_SCHOOL, &
             EMPLOYMENT_SELF_EMPLOYED
   public :: new_addressbook, addressbook_root
   public :: addressbook_init_people, addressbook_people, addressbook_person
   public :: person_get_id, person_set_id, person_get_name, person_set_name
   public :: person_get_email, person_set_email
   public :: person_init_phones, person_phones, person_phone
   public :: person_employment_which, person_set_employer, person_set_school, &
             person_set_unemployed, person_set_self_employed, person_get_employer
   public :: phone_get_number, phone_set_number, phone_get_type, phone_set_type

   integer, parameter :: PHONE_TYPE_MOBILE = 0
   integer, parameter :: PHONE_TYPE_HOME = 1
   integer, parameter :: PHONE_TYPE_WORK = 2

   integer, parameter :: EMPLOYMENT_UNEMPLOYED = 0
   integer, parameter :: EMPLOYMENT_EMPLOYER = 1
   integer, parameter :: EMPLOYMENT_SCHOOL = 2
   integer, parameter :: EMPLOYMENT_SELF_EMPLOYED = 3

   integer, parameter :: PERSON_DWORDS = 1, PERSON_PWORDS = 4
   integer, parameter :: PHONE_DWORDS = 1, PHONE_PWORDS = 1
   integer, parameter :: BOOK_DWORDS = 0, BOOK_PWORDS = 1
   integer, parameter :: PERSON_EMPLOYMENT_DISC16 = 2 ! u16 units into data

   type :: person_t
      type(capnp_ptr_t) :: p
   end type person_t

   type :: phone_t
      type(capnp_ptr_t) :: p
   end type phone_t

   type :: addressbook_t
      type(capnp_ptr_t) :: p
   end type addressbook_t

contains

   ! --- AddressBook ---------------------------------------------------

   function new_addressbook(msg, err) result(b)
      type(capnp_message_t), intent(inout), target :: msg
      integer, intent(out) :: err
      type(addressbook_t) :: b
      b%p = capnp_new_struct(msg, BOOK_DWORDS, BOOK_PWORDS, err)
      if (err == CAPNP_OK) call capnp_set_root(msg, b%p, err)
   end function new_addressbook

   function addressbook_root(msg, err) result(b)
      type(capnp_message_t), intent(inout), target :: msg
      integer, intent(out) :: err
      type(addressbook_t) :: b
      b%p = capnp_root(msg, err)
   end function addressbook_root

   function addressbook_init_people(b, n, err) result(people)
      type(addressbook_t), intent(in) :: b
      integer(int64), intent(in) :: n
      integer, intent(out) :: err
      type(capnp_ptr_t) :: people
      people = capnp_new_composite_list(b%p%msg, n, PERSON_DWORDS, PERSON_PWORDS, err)
      if (err == CAPNP_OK) call capnp_setp(b%p, 0, people, err)
   end function addressbook_init_people

   function addressbook_people(b, err) result(people)
      type(addressbook_t), intent(in) :: b
      integer, intent(out) :: err
      type(capnp_ptr_t) :: people
      people = capnp_getp(b%p, 0, err)
   end function addressbook_people

   function addressbook_person(people, i, err) result(pe)
      type(capnp_ptr_t), intent(in) :: people
      integer, intent(in) :: i
      integer, intent(out) :: err
      type(person_t) :: pe
      pe%p = capnp_list_get_struct(people, i, err)
   end function addressbook_person

   ! --- Person --------------------------------------------------------

   function person_get_id(pe) result(id)
      type(person_t), intent(in) :: pe
      integer(int64) :: id
      id = capnp_get_u32(pe%p, 0_int64)
   end function person_get_id

   subroutine person_set_id(pe, id, err)
      type(person_t), intent(in) :: pe
      integer(int64), intent(in) :: id
      integer, intent(out) :: err
      call capnp_set_u32(pe%p, 0_int64, id, err)
   end subroutine person_set_id

   subroutine person_get_name(pe, name, err)
      type(person_t), intent(in) :: pe
      character(len=:), allocatable, intent(out) :: name
      integer, intent(out) :: err
      call capnp_get_text(pe%p, 0, name, err)
   end subroutine person_get_name

   subroutine person_set_name(pe, name, err)
      type(person_t), intent(in) :: pe
      character(len=*), intent(in) :: name
      integer, intent(out) :: err
      call capnp_set_text(pe%p, 0, name, err)
   end subroutine person_set_name

   subroutine person_get_email(pe, email, err)
      type(person_t), intent(in) :: pe
      character(len=:), allocatable, intent(out) :: email
      integer, intent(out) :: err
      call capnp_get_text(pe%p, 1, email, err)
   end subroutine person_get_email

   subroutine person_set_email(pe, email, err)
      type(person_t), intent(in) :: pe
      character(len=*), intent(in) :: email
      integer, intent(out) :: err
      call capnp_set_text(pe%p, 1, email, err)
   end subroutine person_set_email

   function person_init_phones(pe, n, err) result(phones)
      type(person_t), intent(in) :: pe
      integer(int64), intent(in) :: n
      integer, intent(out) :: err
      type(capnp_ptr_t) :: phones
      phones = capnp_new_composite_list(pe%p%msg, n, PHONE_DWORDS, PHONE_PWORDS, err)
      if (err == CAPNP_OK) call capnp_setp(pe%p, 2, phones, err)
   end function person_init_phones

   function person_phones(pe, err) result(phones)
      type(person_t), intent(in) :: pe
      integer, intent(out) :: err
      type(capnp_ptr_t) :: phones
      phones = capnp_getp(pe%p, 2, err)
   end function person_phones

   function person_phone(phones, i, err) result(ph)
      type(capnp_ptr_t), intent(in) :: phones
      integer, intent(in) :: i
      integer, intent(out) :: err
      type(phone_t) :: ph
      ph%p = capnp_list_get_struct(phones, i, err)
   end function person_phone

   function person_employment_which(pe) result(w)
      type(person_t), intent(in) :: pe
      integer :: w
      w = capnp_which(pe%p, PERSON_EMPLOYMENT_DISC16)
   end function person_employment_which

   subroutine person_set_employer(pe, employer, err)
      type(person_t), intent(in) :: pe
      character(len=*), intent(in) :: employer
      integer, intent(out) :: err
      call capnp_set_which(pe%p, PERSON_EMPLOYMENT_DISC16, EMPLOYMENT_EMPLOYER, err)
      if (err == CAPNP_OK) call capnp_set_text(pe%p, 3, employer, err)
   end subroutine person_set_employer

   subroutine person_get_employer(pe, employer, err)
      type(person_t), intent(in) :: pe
      character(len=:), allocatable, intent(out) :: employer
      integer, intent(out) :: err
      call capnp_get_text(pe%p, 3, employer, err)
   end subroutine person_get_employer

   subroutine person_set_school(pe, school, err)
      type(person_t), intent(in) :: pe
      character(len=*), intent(in) :: school
      integer, intent(out) :: err
      call capnp_set_which(pe%p, PERSON_EMPLOYMENT_DISC16, EMPLOYMENT_SCHOOL, err)
      if (err == CAPNP_OK) call capnp_set_text(pe%p, 3, school, err)
   end subroutine person_set_school

   subroutine person_set_unemployed(pe, err)
      type(person_t), intent(in) :: pe
      integer, intent(out) :: err
      call capnp_set_which(pe%p, PERSON_EMPLOYMENT_DISC16, EMPLOYMENT_UNEMPLOYED, err)
   end subroutine person_set_unemployed

   subroutine person_set_self_employed(pe, err)
      type(person_t), intent(in) :: pe
      integer, intent(out) :: err
      call capnp_set_which(pe%p, PERSON_EMPLOYMENT_DISC16, EMPLOYMENT_SELF_EMPLOYED, err)
   end subroutine person_set_self_employed

   ! --- PhoneNumber ---------------------------------------------------

   subroutine phone_get_number(ph, number, err)
      type(phone_t), intent(in) :: ph
      character(len=:), allocatable, intent(out) :: number
      integer, intent(out) :: err
      call capnp_get_text(ph%p, 0, number, err)
   end subroutine phone_get_number

   subroutine phone_set_number(ph, number, err)
      type(phone_t), intent(in) :: ph
      character(len=*), intent(in) :: number
      integer, intent(out) :: err
      call capnp_set_text(ph%p, 0, number, err)
   end subroutine phone_set_number

   function phone_get_type(ph) result(t)
      type(phone_t), intent(in) :: ph
      integer :: t
      t = int(capnp_get_u16(ph%p, 0_int64))
   end function phone_get_type

   subroutine phone_set_type(ph, t, err)
      type(phone_t), intent(in) :: ph
      integer, intent(in) :: t
      integer, intent(out) :: err
      call capnp_set_u16(ph%p, 0_int64, int(t, int32), err)
   end subroutine phone_set_type

end module addressbook_schema
