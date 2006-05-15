#ifndef GCC_CHAMELEON_PROTOS_H
#define GCC_CHAMELEON_PROTOS_H 1

extern void dop_readonly_data_section PARAMS ((void));
extern int dop_hard_regno_mode_ok PARAMS ((int, enum machine_mode));
extern int dop_modes_tieable_p PARAMS ((enum machine_mode, enum machine_mode));
extern int dop_class_max_nregs PARAMS ((int, enum machine_mode));
extern void dop_init_cumulative_args PARAMS ((CUMULATIVE_ARGS*, tree, rtx, 
                                              tree, int));
extern rtx dop_function_arg PARAMS ((CUMULATIVE_ARGS*, enum machine_mode,
                                     tree, int));
extern void dop_function_arg_advance PARAMS ((CUMULATIVE_ARGS*,
                                              enum machine_mode,
                                              tree, int));
extern int dop_function_arg_partial_nregs PARAMS ((CUMULATIVE_ARGS*,
                                                   enum machine_mode,
                                                   tree, int));
extern int dop_return_in_memory PARAMS ((tree));
extern int dop_regno_class PARAMS ((int));
extern int dop_legitimate_register PARAMS ((rtx reg, int strict));
extern int dop_legitimate_address PARAMS ((enum machine_mode, rtx, int));
extern void dop_print_reg_expire PARAMS ((FILE*, rtx x));
extern void assemble_name_und PARAMS ((FILE*, const char*));
extern void dop_print_operand PARAMS ((FILE*, rtx, int));
extern void dop_print_operand_address PARAMS ((FILE*, rtx));
extern void dop_print_register_expire PARAMS ((FILE*, rtx));
extern void dop_output_ascii PARAMS ((FILE*, const char*, int));

#ifdef REAL_VALUE_TYPE
extern void dop_output_float PARAMS ((FILE*, REAL_VALUE_TYPE));
extern void dop_output_double PARAMS ((FILE*, REAL_VALUE_TYPE));
#endif

extern int dop_const_ok_for_letter_p PARAMS ((unsigned int, int));
extern int dop_const_double_ok_for_letter_p PARAMS ((rtx, int));
extern int dop_extra_constraint PARAMS ((rtx, char));

#ifdef RTX_CODE
extern rtx dop_gen_compare_reg PARAMS ((RTX_CODE, rtx, rtx));
extern rtx dop_gen_setcc PARAMS ((RTX_CODE, rtx, rtx));
extern rtx dop_gen_load_multiple PARAMS ((int, int, rtx, int, int, int,
  int, int));
#endif

extern int dop_illegal_mvc_const PARAMS ((rtx, enum machine_mode));
extern int dop_legal_mvc_const PARAMS ((rtx, enum machine_mode));
extern int dop_not_illegal_const PARAMS ((rtx, enum machine_mode));
extern int any_operand PARAMS ((rtx, enum machine_mode));
extern int dop_symbol_operand PARAMS ((rtx, enum machine_mode));
extern int dop_call_valid_operand PARAMS ((rtx, enum machine_mode));
extern int dop_branch_valid_operand PARAMS ((rtx, enum machine_mode));
extern int dop_multi_mem_operand PARAMS ((rtx, enum machine_mode));
extern int load_multiple_operation PARAMS ((rtx, enum machine_mode));
extern int dop_reg_or_imm_operand PARAMS ((rtx, enum machine_mode));
extern int dop_reload_memory_operand PARAMS ((rtx, enum machine_mode));
extern int dop_float_operand PARAMS ((rtx, enum machine_mode));
extern int dop_float_op2 PARAMS ((rtx, enum machine_mode));
extern int dop_argp_operand PARAMS ((rtx, enum machine_mode));
extern int dop_call_saved_regs_size PARAMS ((void));
extern unsigned int dop_initial_elimination_offset PARAMS ((unsigned int,
                                                            unsigned int));
extern enum reg_class dop_preferred_reload_class PARAMS ((rtx, enum reg_class));
extern enum reg_class dop_limit_reload_class PARAMS ((enum machine_mode,
                                                      enum reg_class));
extern int dop_secondary_memory_needed PARAMS ((enum reg_class, enum reg_class,
                                                enum machine_mode));
extern int dop_secondary_memory_needed_mode PARAMS ((enum machine_mode));
extern int dop_cannot_change_mode_class PARAMS ((enum machine_mode from,
                                                 enum machine_mode to,
                                                 enum reg_class class));
extern void dop_move_double PARAMS ((rtx, rtx));

#endif
