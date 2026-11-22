!> Schema compiler parity with Cap'n Proto C++ 1.4.0. Each case shells out
!> to test/codegen_parity.sh, which fails on the base emitter.
module test_codegen_parity
   use testdrive, only: new_unittest, unittest_type, error_type, check
   implicit none

   private
   public :: collect_codegen_parity

contains

   subroutine collect_codegen_parity(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      testsuite = [ &
         new_unittest("g1_min_signed", test_g1), &
         new_unittest("g2_text_escape", test_g2), &
         new_unittest("g3_default_list_copy", test_g3), &
         new_unittest("g4_get_elem_default", test_g4), &
         new_unittest("g5_extends", test_g5), &
         new_unittest("g6_generics", test_g6), &
         new_unittest("g7_basename", test_g7), &
         new_unittest("g8_server_ctx", test_g8), &
         new_unittest("g9_surface", test_g9), &
         new_unittest("g10_data_annotation", test_g10), &
         new_unittest("g11_super_dispatch", test_g11), &
         new_unittest("g12_import_shared_types", test_g12), &
         new_unittest("g13_same_name_annotations", test_g13), &
         new_unittest("g14_long_text_literals", test_g14), &
         new_unittest("g15_generic_extends_base", test_g15), &
         new_unittest("g16_import_line_wrap", test_g16), &
         new_unittest("g17_long_ifc_and_float_lines", test_g17), &
         new_unittest("g18_stream_name_fit", test_g18), &
         new_unittest("g19_override_vs_own_method", test_g19), &
         new_unittest("g20_override_chain_binding", test_g20), &
         new_unittest("g21_same_named_imports", test_g21), &
         new_unittest("g22_generic_in_two_files", test_g22), &
         new_unittest("g23_rpc_names_vs_structs", test_g23), &
         new_unittest("g24_annotation_name_clashes", test_g24), &
         new_unittest("g25_import_alias_uses", test_g25), &
         new_unittest("g26_capability_helper_names", test_g26), &
         new_unittest("g27_diamond_imports", test_g27), &
         new_unittest("g28_same_basename_modules", test_g28), &
         new_unittest("g29_nested_brand_arguments", test_g29), &
         new_unittest("g30_imported_name_clashes", test_g30), &
         new_unittest("g31_renamed_imports_through_an_importer", test_g31), &
         new_unittest("g32_rpc_symbols_meet_imports", test_g32), &
         new_unittest("g33_transitive_reexports", test_g33), &
         new_unittest("g34_private_names_stay_private", test_g34), &
         new_unittest("g35_colliding_generic_arguments", test_g35), &
         new_unittest("g36_nested_generic_arguments", test_g36), &
         new_unittest("g37_module_names_per_file", test_g37), &
         new_unittest("g38_import_for_binding_per_file", test_g38), &
         new_unittest("g39_instance_names_per_file", test_g39), &
         new_unittest("g40_annotation_ids_per_file", test_g40), &
         new_unittest("g41_cap_descriptor_reexport", test_g41), &
         new_unittest("g42_rpc_symbols_stay_private", test_g42), &
         new_unittest("g43_annotation_values_per_file", test_g43), &
         new_unittest("g44_any_type_struct_names", test_g44), &
         new_unittest("g45_accessor_names_beside_consts", test_g45), &
         new_unittest("g46_instance_names_beside_interfaces", test_g46), &
         new_unittest("g47_main_instance_names_kept", test_g47), &
         new_unittest("g48_struct_consts_beside_accessors", test_g48), &
         new_unittest("g49_text_consts_keep_their_value", test_g49), &
         new_unittest("g50_many_renamed_accessors", test_g50), &
         new_unittest("g51_group_beside_instance_name", test_g51) &
         ]
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

end module test_codegen_parity
