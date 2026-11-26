!> Schema compiler parity with Cap'n Proto C++ 1.4.0. Each case shells out
!> to test/codegen_parity.sh, which fails on the base emitter.
module test_codegen_parity
   use testdrive, only: unittest_type, error_type, check
   implicit none

   private
   public :: collect_codegen_parity

contains

   subroutine collect_codegen_parity(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      allocate (testsuite(53))
      testsuite(1)%name = "g1_min_signed"
      testsuite(1)%test => test_g1
      testsuite(2)%name = "g2_text_escape"
      testsuite(2)%test => test_g2
      testsuite(3)%name = "g3_default_list_copy"
      testsuite(3)%test => test_g3
      testsuite(4)%name = "g4_get_elem_default"
      testsuite(4)%test => test_g4
      testsuite(5)%name = "g5_extends"
      testsuite(5)%test => test_g5
      testsuite(6)%name = "g6_generics"
      testsuite(6)%test => test_g6
      testsuite(7)%name = "g7_basename"
      testsuite(7)%test => test_g7
      testsuite(8)%name = "g8_server_ctx"
      testsuite(8)%test => test_g8
      testsuite(9)%name = "g9_surface"
      testsuite(9)%test => test_g9
      testsuite(10)%name = "g10_data_annotation"
      testsuite(10)%test => test_g10
      testsuite(11)%name = "g11_super_dispatch"
      testsuite(11)%test => test_g11
      testsuite(12)%name = "g12_import_shared_types"
      testsuite(12)%test => test_g12
      testsuite(13)%name = "g13_same_name_annotations"
      testsuite(13)%test => test_g13
      testsuite(14)%name = "g14_long_text_literals"
      testsuite(14)%test => test_g14
      testsuite(15)%name = "g15_generic_extends_base"
      testsuite(15)%test => test_g15
      testsuite(16)%name = "g16_import_line_wrap"
      testsuite(16)%test => test_g16
      testsuite(17)%name = "g17_long_ifc_and_float_lines"
      testsuite(17)%test => test_g17
      testsuite(18)%name = "g18_stream_name_fit"
      testsuite(18)%test => test_g18
      testsuite(19)%name = "g19_override_vs_own_method"
      testsuite(19)%test => test_g19
      testsuite(20)%name = "g20_override_chain_binding"
      testsuite(20)%test => test_g20
      testsuite(21)%name = "g21_same_named_imports"
      testsuite(21)%test => test_g21
      testsuite(22)%name = "g22_generic_in_two_files"
      testsuite(22)%test => test_g22
      testsuite(23)%name = "g23_rpc_names_vs_structs"
      testsuite(23)%test => test_g23
      testsuite(24)%name = "g24_annotation_name_clashes"
      testsuite(24)%test => test_g24
      testsuite(25)%name = "g25_import_alias_uses"
      testsuite(25)%test => test_g25
      testsuite(26)%name = "g26_capability_helper_names"
      testsuite(26)%test => test_g26
      testsuite(27)%name = "g27_diamond_imports"
      testsuite(27)%test => test_g27
      testsuite(28)%name = "g28_same_basename_modules"
      testsuite(28)%test => test_g28
      testsuite(29)%name = "g29_nested_brand_arguments"
      testsuite(29)%test => test_g29
      testsuite(30)%name = "g30_imported_name_clashes"
      testsuite(30)%test => test_g30
      testsuite(31)%name = "g31_renamed_imports_through_an_importer"
      testsuite(31)%test => test_g31
      testsuite(32)%name = "g32_rpc_symbols_meet_imports"
      testsuite(32)%test => test_g32
      testsuite(33)%name = "g33_transitive_reexports"
      testsuite(33)%test => test_g33
      testsuite(34)%name = "g34_private_names_stay_private"
      testsuite(34)%test => test_g34
      testsuite(35)%name = "g35_colliding_generic_arguments"
      testsuite(35)%test => test_g35
      testsuite(36)%name = "g36_nested_generic_arguments"
      testsuite(36)%test => test_g36
      testsuite(37)%name = "g37_module_names_per_file"
      testsuite(37)%test => test_g37
      testsuite(38)%name = "g38_import_for_binding_per_file"
      testsuite(38)%test => test_g38
      testsuite(39)%name = "g39_instance_names_per_file"
      testsuite(39)%test => test_g39
      testsuite(40)%name = "g40_annotation_ids_per_file"
      testsuite(40)%test => test_g40
      testsuite(41)%name = "g41_cap_descriptor_reexport"
      testsuite(41)%test => test_g41
      testsuite(42)%name = "g42_rpc_symbols_stay_private"
      testsuite(42)%test => test_g42
      testsuite(43)%name = "g43_annotation_values_per_file"
      testsuite(43)%test => test_g43
      testsuite(44)%name = "g44_any_type_struct_names"
      testsuite(44)%test => test_g44
      testsuite(45)%name = "g45_accessor_names_beside_consts"
      testsuite(45)%test => test_g45
      testsuite(46)%name = "g46_instance_names_beside_interfaces"
      testsuite(46)%test => test_g46
      testsuite(47)%name = "g47_main_instance_names_kept"
      testsuite(47)%test => test_g47
      testsuite(48)%name = "g48_struct_consts_beside_accessors"
      testsuite(48)%test => test_g48
      testsuite(49)%name = "g49_text_consts_keep_their_value"
      testsuite(49)%test => test_g49
      testsuite(50)%name = "g50_many_renamed_accessors"
      testsuite(50)%test => test_g50
      testsuite(51)%name = "g51_group_beside_instance_name"
      testsuite(51)%test => test_g51
      testsuite(52)%name = "g52_subtype_client_setter"
      testsuite(52)%test => test_g52
      testsuite(53)%name = "g53_group_select_zeroes_slots"
      testsuite(53)%test => test_g53
   end subroutine collect_codegen_parity


   subroutine run_gap(error, gap)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), intent(in) :: gap
      integer :: stat, cs
      call execute_command_line('bash test/codegen_parity.sh '//gap, &
         exitstat=stat, cmdstat=cs)
      call check(error, cs == 0 .and. stat == 0, gap)
   end subroutine run_gap

   subroutine test_g1(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g1')
   end subroutine test_g1

   subroutine test_g2(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g2')
   end subroutine test_g2

   subroutine test_g3(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g3')
   end subroutine test_g3

   subroutine test_g4(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g4')
   end subroutine test_g4

   subroutine test_g5(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g5')
   end subroutine test_g5

   subroutine test_g6(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g6')
   end subroutine test_g6

   subroutine test_g7(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g7')
   end subroutine test_g7

   subroutine test_g8(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g8')
   end subroutine test_g8

   subroutine test_g9(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g9')
   end subroutine test_g9

   subroutine test_g10(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g10')
   end subroutine test_g10

   subroutine test_g11(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g11')
   end subroutine test_g11

   subroutine test_g12(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g12')
   end subroutine test_g12

   subroutine test_g13(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g13')
   end subroutine test_g13

   subroutine test_g14(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g14')
   end subroutine test_g14

   subroutine test_g15(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g15')
   end subroutine test_g15

   subroutine test_g16(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g16')
   end subroutine test_g16

   subroutine test_g17(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g17')
   end subroutine test_g17

   subroutine test_g18(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g18')
   end subroutine test_g18

   subroutine test_g19(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g19')
   end subroutine test_g19

   subroutine test_g20(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g20')
   end subroutine test_g20

   subroutine test_g21(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g21')
   end subroutine test_g21

   subroutine test_g22(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g22')
   end subroutine test_g22

   subroutine test_g23(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g23')
   end subroutine test_g23

   subroutine test_g24(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g24')
   end subroutine test_g24

   subroutine test_g25(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g25')
   end subroutine test_g25

   subroutine test_g26(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g26')
   end subroutine test_g26

   subroutine test_g27(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g27')
   end subroutine test_g27

   subroutine test_g28(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g28')
   end subroutine test_g28

   subroutine test_g29(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g29')
   end subroutine test_g29

   subroutine test_g30(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g30')
   end subroutine test_g30

   subroutine test_g31(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g31')
   end subroutine test_g31

   subroutine test_g32(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g32')
   end subroutine test_g32

   subroutine test_g33(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g33')
   end subroutine test_g33

   subroutine test_g34(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g34')
   end subroutine test_g34

   subroutine test_g35(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g35')
   end subroutine test_g35

   subroutine test_g36(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g36')
   end subroutine test_g36

   subroutine test_g37(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g37')
   end subroutine test_g37

   subroutine test_g38(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g38')
   end subroutine test_g38

   subroutine test_g39(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g39')
   end subroutine test_g39

   subroutine test_g40(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g40')
   end subroutine test_g40

   subroutine test_g41(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g41')
   end subroutine test_g41

   subroutine test_g42(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g42')
   end subroutine test_g42

   subroutine test_g43(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g43')
   end subroutine test_g43

   subroutine test_g44(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g44')
   end subroutine test_g44

   subroutine test_g45(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g45')
   end subroutine test_g45

   subroutine test_g46(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g46')
   end subroutine test_g46

   subroutine test_g47(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g47')
   end subroutine test_g47

   subroutine test_g48(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g48')
   end subroutine test_g48

   subroutine test_g49(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g49')
   end subroutine test_g49

   subroutine test_g50(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g50')
   end subroutine test_g50

   subroutine test_g51(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g51')
   end subroutine test_g51

   subroutine test_g52(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g52')
   end subroutine test_g52

   subroutine test_g53(error)
      type(error_type), allocatable, intent(out) :: error
      call run_gap(error, 'g53')
   end subroutine test_g53

end module test_codegen_parity
