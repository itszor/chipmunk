/* Definitions of target machine for GNU compiler for DOP
   Copyright (C) 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001
   Free Software Foundation, Inc.
   Contributed by Steve Chamberlain (sac@cygnus.com).
   Improved by Jim Wilson (wilson@cygnus.com).

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef GCC_DOP_H
#define GCC_DOP_H

/*** Controlling the compilation driver ***/

/* nothing defined */

/*** Run-time Target Specifications ***/

#define TARGET_CPU_CPP_BUILTINS() \
  do { \
    builtin_define("__CHIPMUNK__"); \
  } while (0)

extern int target_flags;

#define TARGET_DEFAULT 1

#define TARGET_SWITCHES \
  { \
    {"", TARGET_DEFAULT, ""} \
  } 

#define TARGET_VERSION \
  fprintf(stderr, " (DOP V0.1)");

/*** Defining data structures for per-function information ***/

/* nothing defined */

/*** Storage layout ***/

#define BITS_BIG_ENDIAN 0
#define BYTES_BIG_ENDIAN 0
#define WORDS_BIG_ENDIAN 0
#define FLOAT_WORDS_BIG_ENDIAN 0
#define FLOAT_BIG_ENDIAN 0

#define BITS_PER_UNIT 8
#define BITS_PER_WORD 32

#define UNITS_PER_WORD 4

#define POINTER_SIZE 32

/* promote integer modes with size < 4 to SImode */
#define PROMOTE_MODE(MODE, UNSIGNEDP, TYPE) \
  do { \
    (MODE) = GET_MODE_CLASS(MODE) == MODE_INT ? \
      ((GET_MODE_SIZE(MODE) < 4) ? SImode : (MODE)) : (MODE); \
  } while (0)

/* Alignment required for function parameters on the stack, in bits */
#define PARM_BOUNDARY 32

/* Minimum aligned boundary enforced by hardware for stack pointer */
#define STACK_BOUNDARY 32

/* alignment required for a function entry point, in bits. */
/* may be beneficial to change for cacheing model */
/* (or not... you can't do this with chasm) */
/* this is 8 not 32 to suppress alignment output in text section */
#define FUNCTION_BOUNDARY 8

/* biggest alignment needed by any data type */
#define BIGGEST_ALIGNMENT 32

/* Alignment for a variable in the static store */
/* BASIC_ALIGN is alignment object would normally have, which should be ok. */
#define DATA_ALIGNMENT(TYPE, BASIC_ALIGN) (BASIC_ALIGN)

/* Alignment of a constant being placed in memory */
#define CONSTANT_ALIGNMENT(CONSTANT, BASIC_ALIGN) (BASIC_ALIGN)

/* unaligned memory accesses don't work */
#define STRICT_ALIGNMENT 1

/* only really support 32-bit integers */
#define MAX_FIXED_MODE_SIZE 32

#define TARGET_FLOAT_FORMAT IEEE_FLOAT_FORMAT

/*** Type layout ***/

#define INT_TYPE_SIZE 32
#define SHORT_TYPE_SIZE 16
#define CHAR_TYPE_SIZE 8
#define FLOAT_TYPE_SIZE 32
#define DOUBLE_TYPE_SIZE 64
#define LONG_DOUBLE_TYPE_SIZE 64

/* I don't like signed chars, they confuse me */
#define DEFAULT_SIGNED_CHAR 0

/* Name of data type to use for wide characters */
#define WCHAR_TYPE "long int"

/* The size of that type */
#define WCHAR_TYPE_SIZE 32

/*** Escape sequences ***/

/* nothing defined */

/*** Basic Characteristics of Registers ***/

/* 64 int + 64 float + 1 fake */
#define FIRST_PSEUDO_REGISTER 129

/* Registers used for fixed purposes, not available for allocation */
/* fixed registers:
 * r63 Stack pointer
 * r62 Frame pointer
 * r61 Optimiser-controlled memory/shadow stack
 * r55 Current block ref
 * r54 Link register
 */
#define FIXED_REGISTERS \
  { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    1 }

/* Registers clobbered by function calls, and fixed registers */
#define CALL_USED_REGISTERS \
  { 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, \
    1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    1 }

/* If defined & nonzero, compiler avoids putting variables in registers in
 * functions which use setjmp
 */
/* fixme, when setjmp/longjmp are implemented properly */
/*
#define NON_SAVING_SETJMP 1
*/

/*** Order of Allocation of Registers ***/

/* probably not the best ordering */
#define REG_ALLOC_ORDER \
  {  7,  6,  5,  4,  3,  2,  1,  0,  8,  9, 10, 11, 12, 13, 14, 15, \
    16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, \
    56, 57, 58, 59, 60, \
    71, 70, 69, 68, 67, 66, 65, 64, 72, 73, 74, 75, 76, 77, 78, 79, \
    80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, \
    96, 97, 98, 99 }

/*** How Values Fit in Registers ***/

/* Number of consecutive hard registers starting at REGNO required to
 * hold value of mode MODE
 */
#define HARD_REGNO_NREGS(REGNO, MODE) \
  (((REGNO)>=FIRST_DOP_FLOAT_REGISTER && \
    (REGNO)<=LAST_DOP_FLOAT_REGISTER) ? 1 : \
   ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

/* permissible to store a value of mode MODE in hard reg number (starting at)
 * REGNO
 */
#define HARD_REGNO_MODE_OK(REGNO, MODE) \
  dop_hard_regno_mode_ok(REGNO, MODE)

/* Nonzero if value of mode MODE1 is accessible in mode MODE2 without copying */
#define MODES_TIEABLE_P(MODE1, MODE2) \
  dop_modes_tieable_p(MODE1, MODE2)

/*** Handling Leaf Functions ***/

/* Nothing defined */

/*** Registers That Form a Stack ***/

/* Nothing defined */

/*** Register Classes ***/

enum reg_class
{
  NO_REGS,
  FLOAT_REGS,
  PTR_REGS,
  GENERAL_REGS,
/*  FLOAT_OR_GENERAL_REGS,*/
  ALL_REGS,
  LIM_REG_CLASSES
};

#define N_REG_CLASSES (int) LIM_REG_CLASSES

#define REG_CLASS_NAMES \
{ \
  "NO_REGS", \
  "FLOAT_REGS", \
  "PTR_REGS", \
  "GENERAL_REGS", \
/*  "FLOAT_OR_GENERAL_REGS", */\
  "ALL_REGS" \
}

/* first integer in each sub-initializer corresponds to regs 0-31,
 * the second integer to 32-63 etc.
 */
#define REG_CLASS_CONTENTS \
{ \
  /* NO_REGS */ \
  { 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000 }, \
  /* FLOAT_REGS (f0-f35) */ \
  { 0x00000000, 0x00000000, 0xFFFFFFFF, 0x0000000F, 0x00000000 }, \
  /* PTR_REGS (r56-r63) */ \
  { 0x00000000, 0xFF000000, 0x00000000, 0x00000000, 0x00000000 }, \
  /* GENERAL_REGS (r0-r30, r56-r63) */ \
  { 0x7FFFFFFF, 0xFF000000, 0x00000000, 0x00000000, 0x00000000 }, \
  /* FLOAT_OR_GENERAL_REGS */ \
/*  { 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0x00000000 }, */\
  /* ALL_REGS */ \
  { 0x7FFFFFFF, 0xFF000000, 0xFFFFFFFF, 0x0000000F, 0x00000001 } \
}

/* returns the smallest class containing reg number REGNO */
#define REGNO_REG_CLASS(REGNO) dop_regno_class(REGNO)

/* all general registers can be base registers too */
#define BASE_REG_CLASS GENERAL_REGS

/* all general registers can be index registers */
#define INDEX_REG_CLASS GENERAL_REGS

/* 'r' == general regs.
 * we also have special classes for:
 *   - float registers: 'f'
 *   - ptr registers (base of ldm/stm): 't'
 */
#define REG_CLASS_FROM_LETTER(CHAR) \
    (((CHAR) == 'f') ? FLOAT_REGS \
  : ((CHAR) == 't') ? PTR_REGS \
 /* : ((CHAR) == 'R') ? FLOAT_OR_GENERAL_REGS */\
  : NO_REGS)

/* non-zero if register NUM ok for base */
/* I don't understand the full implications of this reg_renumber stuff. */
#define REGNO_OK_FOR_BASE_P(REGNO) \
     (((REGNO) < FIRST_DOP_FLOAT_REGISTER) \
  || (reg_renumber[REGNO] < FIRST_DOP_FLOAT_REGISTER) \
  || ((REGNO) == ARG_POINTER_REGNUM) \
  || (reg_renumber[REGNO] == ARG_POINTER_REGNUM))

#define REGNO_OK_FOR_INDEX_P(REGNO) \
  REGNO_OK_FOR_BASE_P(REGNO)

/* maybe issues with this (cris port), so fixme I guess */
#define PREFERRED_RELOAD_CLASS(X, CLASS) \
  dop_preferred_reload_class((X), (CLASS))

/* Can say which modes can't go in certain reload classes */
/*#define LIMIT_RELOAD_CLASS(MODE, CLASS) \
  dop_limit_reload_class((MODE), (CLASS))*/

/* Registers which can't be copied to other registers without using
 * memory. Also REG_MOVE_COST is important, but it doesn't say so in the
 * manual!
 */
#define SECONDARY_MEMORY_NEEDED(CLASS1, CLASS2, MODE) \
  dop_secondary_memory_needed((CLASS1), (CLASS2), (MODE))

#define CANNOT_CHANGE_MODE_CLASS(FROM, TO, CLASS) \
  dop_cannot_change_mode_class((FROM), (TO), (CLASS))

/* Maximum number of consecutive registers of class CLASS needed to hold a
 * value of mode MODE. Should be maximum value of HARD_REGNO_NREGS(regno,mode)
 * for all regno values in the class class.
 */
#define CLASS_MAX_NREGS(CLASS, MODE) \
  dop_class_max_nregs((CLASS), (MODE))

/* Ranges of integer values for constraint letter C. */
#define CONST_OK_FOR_LETTER_P(VALUE, C) \
  dop_const_ok_for_letter_p((VALUE), (C))

/* Ranges of doubles for constraint letter C. */
#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C) \
  dop_const_double_ok_for_letter_p((VALUE), (C))

/* Can be used to distinguish, say, between different types of memory
 * constraint. Might be useful.
 */
#define EXTRA_CONSTRAINT(VALUE, C) \
  dop_extra_constraint((VALUE), (C))

/*** Basic Stack Layout ***/

#define STACK_GROWS_DOWNWARDS 1
#define FRAME_GROWS_DOWNWARDS 1
#define ARGS_GROW_DOWNWARDS 0

/* Offset from frame pointer of first local variable slot to be allocated */
#define STARTING_FRAME_OFFSET 0

/* Offset from argument pointer register to first argument's address.
 * If ARGS_GROW_DOWNWARD, this is the offset to location above first arg's
 * address.
 */
#define FIRST_PARM_OFFSET(FUNDECL) 0

/* Offset from SP to item dynamically allocated on stack, eg by alloca */
#define STACK_DYNAMIC_OFFSET(FUNDECL) 0

/*** Exception Handling Support ***/

/* nothing defined */

/*** Stack Checking ***/

/* nothing defined */

/*** Registers That Address the Stack Frame ***/

#define STACK_POINTER_REGNUM 63
#define FRAME_POINTER_REGNUM 62
#define ARG_POINTER_REGNUM 128

/* No static chain is defined at the moment */
#define STATIC_CHAIN_REGNUM 0

/*** Eliminating Frame Pointer and Arg Pointer ***/

/* don't need a frame pointer for simple leaf functions, probably */
#define FRAME_POINTER_REQUIRED 0

/* Initial difference between frame pointer and stack pointer */
#define INITIAL_FRAME_POINTER_OFFSET(DEPTH) { \
    (DEPTH) = dop_frame_stack_difference(); \
    fprintf(stderr, "Depthvar=%d\n", (DEPTH)); \
  }

/* Substitutions for getting rid of arg pointer and frame pointer references
 */
#define ELIMINABLE_REGS				\
 {{ARG_POINTER_REGNUM, STACK_POINTER_REGNUM},	\
  {ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM},	\
  {FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}}

/* Always allow eliminination by substituting TO for FROM */
#define CAN_ELIMINATE(FROM, TO) 1

/* defines difference between pairs of eliminable registers */
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
  do { \
    (OFFSET) = dop_initial_elimination_offset((FROM), (TO)); \
  } while (0)

/*** Passing function arguments on the stack ***/

/* fixme - this is stolen from the ARM port, not sure of meaning
 * The amount can be found in the variable current_function_outgoing_args_size
 */
#define ACCUMULATE_OUTGOING_ARGS 1

/* This means caller does not pop any of its arguments that were passed on
 * the stack
 */
#define RETURN_POPS_ARGS(FUNDECL, FUNTYPE, SIZE) 0

/*** Passing Arguments in Registers ***/

/* C expression that controls whether a function argument is passed in a
 * register, and which register.
 */
#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
  dop_function_arg(&(CUM), (MODE), (TYPE), (NAMED))

/* Number of words at the beginning of an argument which must be put in
 * registers. Zero if entirely in registers, or entirely in memory.
 */
#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) \
  dop_function_arg_partial_nregs(&(CUM), (MODE), (TYPE), (NAMED))

#define FUNCTION_ARG_PASS_BY_REFERENCE(CUM, MODE, TYPE, NAMED) \
  MUST_PASS_IN_STACK(MODE, TYPE)

/* C type detailing args passed in registers */
typedef struct {
  /* the number of registers of arguments passed so far */
  int nregs;
  /* number of float args */
  int fregs;
} CUMULATIVE_ARGS;

/* Initialises CUM at beginning of argument list */
#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, FNDECL, NAMED) \
  dop_init_cumulative_args(&(CUM), (FNTYPE), (LIBNAME), (FNDECL), (NAMED))

/* C statement to update summarizer CUM to advance past an argument in the
 * argument list.
 */
#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED) \
  dop_function_arg_advance(&(CUM), (MODE), (TYPE), (NAMED))

/* First eight (integer,float) registers are used for arguments */
#define FUNCTION_ARG_REGNO_P(REGNO) \
  (((REGNO) < 8) || (((REGNO) >= 64) && ((REGNO) <= 71)))

/*** How Scalar Function Values Are Returned ***/

#define FUNCTION_VALUE(VALTYPE, FUNC) \
  LIBCALL_VALUE (TYPE_MODE (VALTYPE))

/* return floating-point results in floating-point register */
#define LIBCALL_VALUE(MODE) \
  ((GET_MODE_CLASS (MODE) == MODE_FLOAT \
    || GET_MODE_CLASS (MODE) == MODE_COMPLEX_FLOAT) \
    ? gen_rtx_REG ((MODE), FIRST_DOP_FLOAT_REGISTER) \
    : gen_rtx_REG ((MODE), FIRST_DOP_GENERAL_REGISTER))

/* 1 if REGNO is a possible return result -- r0 or f0 ?? */
/* fixme if reg 1 needs to be specified for 64-bit int results */
#define FUNCTION_VALUE_REGNO_P(REGNO) \
  (((REGNO)==FIRST_DOP_GENERAL_REGISTER) \
  || ((REGNO)==FIRST_DOP_FLOAT_REGISTER))

/* UNDOCUMENTED, perhaps it's deprecated
 * (moved to TARGET_INITIALIZER thing)
 */
#define RETURN_IN_MEMORY(TYPE) \
  dop_return_in_memory (TYPE)

/*** How Large Values Are Returned ***/

/* nothing defined */

/*** Caller-Saves Register Allocation ***/

/* nothing defined */

/*** Function Entry and Exit ***/

/* nothing defined (target hooks) */

/*** Generating Code for Profiling ***/

#define FUNCTION_PROFILER(FILE, LABELNO) \
  error ("no FUNCTION_PROFILER for DOP")

/*** Permitting tail calls ***/

/* nothing defined */

/*** Implementing the Varargs Macros ***/

/* Nothing defined */

/*** Trampolines for Nested Functions ***/

/* fixme! Trampolines will be hard, I suspect */
#define TRAMPOLINE_SIZE 32

/* alignment, in bits */
#define TRAMPOLINE_ALIGNMENT 32

#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT) \
  do { \
    /* nothing */ \
  } while (0)

/*** Implicit Calls to Library Routines ***/

/* Generate calls to ISO C library functions memcpy, memmove and memset
 * rather than BSD functions bcopy and bzero.
 */
#define TARGET_MEM_FUNCTIONS

/*** Addressing Modes ***/

#define HAVE_PRE_DECREMENT 1
#define HAVE_POST_INCREMENT 1

/* C expression which is 1 if X is a constant which is a valid address */
#define CONSTANT_ADDRESS_P(X) \
  (GET_CODE(X)==SYMBOL_REF && CONSTANT_POOL_ADDRESS_P(X))

/*#define CONSTANT_ADDRESS_P(X) \
  (CONSTANT_P(X))*/

/* Can only have base+index */
#define MAX_REGS_PER_ADDRESS 2

#ifdef REG_OK_STRICT
#define LEGITIMATE_ADDRESS_STRICT 1
#else
#define LEGITIMATE_ADDRESS_STRICT 0
#endif

/* this involves gcc internals nastiness, worry about later */
#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, LABEL) \
  if (dop_legitimate_address(MODE, X, LEGITIMATE_ADDRESS_STRICT)) \
    goto LABEL

/* affect meaning of GO_IF_LEGITIMATE_ADDRESS in subtle and horrendous ways */
/*#undef LEGITIMATE_ADDRESS_STRICT*/

/* fixme */
#ifdef REG_OK_STRICT

/* in the strict variant, we must reject a pseudo register which has not been
 * allocated to a hard register.
 */
#define REG_OK_FOR_BASE_P(X) \
  ((REGNO(X) < FIRST_DOP_FLOAT_REGISTER) \
   || (REGNO(X) == ARG_POINTER_REGNUM))

#define REG_OK_FOR_INDEX_P(X) REG_OK_FOR_BASE_P(X)

#else

/* in the non-strict variant, we can allow pseudo registers too */

#define REG_OK_FOR_BASE_P(X) \
  ((REGNO(X) < FIRST_DOP_FLOAT_REGISTER) \
   || (REGNO(X) >= FIRST_PSEUDO_REGISTER) \
   || (REGNO(X) == ARG_POINTER_REGNUM))

#define REG_OK_FOR_INDEX_P(X) REG_OK_FOR_BASE_P(X)

#endif

#define LEGITIMIZE_ADDRESS(X, OLDX, MODE, WIN) {}

/* fixme */
#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR, LABEL)

/* fixme */
#define LEGITIMATE_CONSTANT_P(X) \
  (GET_CODE(X)==CONST_INT || GET_CODE(X)==CONST_DOUBLE \
   || CONSTANT_ADDRESS_P(X))

/*** Condition Code Status ***/

/* nothing defined */

/*** Defining Relative Costs of Operations ***/

/* We cannot move float regs to or from non-float regs cheaply.
 * This isn't just an optimisation hint, it is vital for producing correct
 * code.
 */
#define REGISTER_MOVE_COST(MODE, FROM, TO) \
  (((FROM) == FLOAT_REGS && (TO) != FLOAT_REGS) ? 20 : \
   ((FROM) != FLOAT_REGS && (TO) == FLOAT_REGS) ? 20 : \
  /* ((FROM) == FLOAT_OR_GENERAL_REGS || (TO) == FLOAT_OR_GENERAL_REGS) ? 100 : */\
   2)

/* quite high branch cost? Is supposed to generate alternative code sequences
 * where possible.
 */
#define BRANCH_COST 3

/* Byte accesses are as fast as word accesses */
#define SLOW_BYTE_ACCESS 0

/* Calling function addresses in a register is smaller and faster */
/* ...but breaks a lot at the moment? */
#define NO_FUNCTION_CSE 0

/*** Adjusting the Instruction Scheduler ***/

/* nothing defined */

/*** Dividing the Output into Sections (Texts, Data, ...) ***/

#define TEXT_SECTION_ASM_OP "\ttext"
#define DATA_SECTION_ASM_OP "\tdata"
#define READONLY_DATA_SECTION_ASM_OP "\trodata"
#define BSS_SECTION_ASM_OP "\tbss"

/*** Position Independent Code ***/

/* nothing defined */

/*** Overall Framework of an Assembler File ***/

#define ASM_APP_ON ""
#define ASM_APP_OFF ""

/*** Output of Data ***/

#define ASM_OUTPUT_ASCII(FILE, STRING, LENGTH) \
  dop_output_ascii((FILE), (STRING), (LENGTH))

/*** Output of Uninitialized Variables ***/

#define ASM_OUTPUT_COMMON(STREAM, NAME, SIZE, ROUNDED) \
  do { \
    bss_section(); \
    fprintf((STREAM), "\tglobal _%s\n", (NAME)); \
    assemble_name((STREAM), (NAME)); \
    fprintf((STREAM), ":\n\treserve %d\n", (SIZE)); \
  } while (0)

#define ASM_OUTPUT_LOCAL(STREAM, NAME, SIZE, ROUNDED) \
  do { \
    bss_section(); \
    assemble_name_und((STREAM), (NAME)); \
    fprintf((STREAM), ":\n\treserve %d\n", (SIZE)); \
  } while (0)

/*** Output and Generation of Labels ***/

/*
#define ASM_OUTPUT_LABEL(STREAM, NAME) \
  fprintf((STREAM), "_%s:\n", (NAME))
*/

#define ASM_OUTPUT_LABEL(STREAM, NAME) \
  do { \
    assemble_name_und((STREAM), (NAME)); \
    fprintf((STREAM), ":\n"); \
  } while (0)

#define ASM_OUTPUT_SYMBOL_REF(STREAM, SYM) \
  assemble_name_und((STREAM), XSTR((SYM), 0))

#define ASM_GENERATE_INTERNAL_LABEL(LABEL, PREFIX, NUM) \
  sprintf((LABEL), "$%s%ld", (PREFIX), (NUM))

/*** Macros Controlling Initialization Routines ***/

/* nothing defined */

/*** Output of Assembler Instructions ***/

#define REGISTER_NAMES \
{ \
  "r0",  "r1",  "r2",  "r3",  "r4",  "r5",  "r6",  "r7", \
  "r8",  "r9",  "r10", "r11", "r12", "r13", "r14", "r15", \
  "r16", "r17", "r18", "r19", "r20", "r21", "r22", "r23", \
  "r24", "r25", "r26", "r27", "r28", "r29", "r30", "r31", \
  "r32", "r33", "r34", "r35", "r36", "r37", "r38", "r39", \
  "r40", "r41", "r42", "r43", "r44", "r45", "r46", "r47", \
  "r48", "r49", "r50", "r51", "r52", "r53", "r54", "r55", \
  "r56", "r57", "r58", "r59", "r60", "r61", "r62", "r63", \
  "f0",  "f1",  "f2",  "f3",  "f4",  "f5",  "f6",  "f7", \
  "f8",  "f9",  "f10", "f11", "f12", "f13", "f14", "f15", \
  "f16", "f17", "f18", "f19", "f20", "f21", "f22", "f23", \
  "f24", "f25", "f26", "f27", "f28", "f29", "f30", "f31", \
  "f32", "f33", "f34", "f35", "f36", "f37", "f38", "f39", \
  "f40", "f41", "f42", "f43", "f44", "f45", "f46", "f47", \
  "f48", "f49", "f50", "f51", "f52", "f53", "f54", "f55", \
  "f56", "f57", "f58", "f59", "f60", "f61", "f62", "f63", \
  "argp" \
}

#define PRINT_OPERAND(FILE, X, CODE) \
  dop_print_operand((FILE), (X), (CODE))

/* print address of an operand.
 * always use domain 0 (normal memory access)
 */
#define PRINT_OPERAND_ADDRESS(FILE, ADDR) \
  dop_print_operand_address((FILE), (ADDR))

#undef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX ""

#define ASM_FPRINTF_EXTENSIONS(FILE, ARGS, P) \
  case 'r': \
    fputs(reg_names [va_arg(ARGS, int)], FILE); \
    break;

/* This one's used by assemble_name */
#define ASM_OUTPUT_LABELREF(STREAM, NAME) \
  fprintf((STREAM), "_%s", (NAME)) 

/*** Output of Dispatch Tables ***/

#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM, VALUE) \
  fprintf((STREAM), "\tdcw _$L%d\n", (VALUE))

/* poisonous case label
#define ASM_OUTPUT_CASE_LABEL(FILE, PREFIX, NUM, JUMPTABLE) \
  ASM_OUTPUT_INTERNAL_LABEL(FILE, PREFIX, NUM)
*/

/*** Assembler Commands for Exception Regions ***/

/* nothing defined */

/*** Assembler Commands for Alignment ***/

#define ASM_OUTPUT_SKIP(FILE, A) \
  fprintf((FILE), "\treserve %d", (A))

/* don't output skip in text section */
#define ASM_NO_SKIP_IN_TEXT 1

#define ASM_OUTPUT_ALIGN(STREAM, POWER) \
  fprintf((STREAM), "\talign %d\n", 1<<(POWER));

/*** Macros Affecting All Debugging Formats ***/

#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

/*** Specific Options for DBX Output ***/

#define DBX_DEBUGGING_INFO

#define ASM_STABS_OP "\t// "

#define ASM_STABD_OP "\t// "

#define ASM_STABN_OP "\t// "

/*** Open-Ended Hooks for DBX Format ***/

/* nothing defined */

/*** File Names in DBX Format ***/

/*** Cross Compilation and Floating Point ***/

/* don't need to define these here */

/*** Mode Switching Instructions ***/

/* no different modes */

/*** Miscellaneous Parameters ***/

#define PREDICATE_CODES \
  {"dop_symbol_operand", {SYMBOL_REF}}, \
  {"dop_call_valid_operand", {SYMBOL_REF, REG}}, \
  {"dop_branch_valid_operand", {SYMBOL_REF, LABEL_REF, REG, CODE_LABEL}}, \
  {"dop_multi_mem_operand", {REG}}, \
  {"dop_reg_or_imm_operand", {REG, CONST_INT, CONST_DOUBLE}}, \
  {"dop_argp_operand", {REG}}, \
  {"arith_operand", {REG, CONST_INT}}, \
  {"arith_or_neg_operand", {REG, CONST_INT}}, \
  {"arith_or_inv_operand", {REG, CONST_INT}}, \
  {"mvc_const_operand", {CONST_INT}}, \
  {"symbol_operand", {SYMBOL_REF}}, \
  {"symbol_or_const_operand", {SYMBOL_REF, CONST_INT}}, \
  {"multi_insn_const_operand", {SYMBOL_REF, CONST_INT}}, \
  {"fp_op2_operand", {REG, CONST_INT, CONST_DOUBLE}},
  

#define CASE_VECTOR_MODE SImode

/* #define CASE_VECTOR_PC_RELATIVE 0 */

/* operations on sub-register-sized integer modes always take place in full-
 * sized register mode
 */
#define WORD_REGISTER_OPERATIONS 1

/* smaller-than-register loads are zero extended */
#define LOAD_EXTEND_OP(MODE) ZERO_EXTEND

/* Max number of bytes we can move in a single instruction
 * (is this right for the new multi-mem instructions?)
 */
#define MOVE_MAX 12

/* We can operate on integers of INPREC bits as if they had OUTPREC bits.
 * Manual says "When TRULY_NOOP_TRUNCATION returns 1 for a pair of sizes for 
 * modes for which MODES_TIEABLE_P is 0, suboptimal code can result. If this
 * is the case, making TRULY_NOOP_TRUNCATION return 0 in such cases may
 * improve things."
 */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* value stored by s<cc> instruction is -1 for true, 0 for false */
#define STORE_FLAG_VALUE -1

#define Pmode SImode

#define FUNCTION_MODE SImode

/*** DOP specific macros ***/

/*#define DOP_FIRST_SPECIAL_REGISTER 59*/
#define FIRST_DOP_FLOAT_REGISTER 64
#define LAST_DOP_FLOAT_REGISTER 127
#define FIRST_DOP_GENERAL_REGISTER 0

/* Convert from bytes to ints (round up) */
#define DOP_NUM_INTS(X) (((X) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

#define DOP_NUM_REGS(MODE) \
  DOP_NUM_INTS (GET_MODE_SIZE (MODE))

/* something in gcc is broken I think (nicked from ARM port) */
#define DOP_NUM_REGS2(MODE, TYPE) \
  DOP_NUM_INTS((MODE)==BLKmode ? int_size_in_bytes(TYPE) \
                               : GET_MODE_SIZE(MODE))

extern rtx dop_compare_op0, dop_compare_op1;

#endif     /* dop.h */

