/* Support functions for dop gcc port
 * (c) J Brown 2002
 */

#include <assert.h>

/* list of headers nicked from another port */
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "tree.h"
#include "obstack.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "reload.h"
#include "function.h"
#include "expr.h"
#include "optabs.h"
#include "toplev.h"
#include "recog.h"
#include "ggc.h"
#include "except.h"
#include "c-pragma.h"
#include "integrate.h"
#include "tm_p.h"
#include "target.h"
#include "target-def.h"
#include "langhooks.h"

rtx dop_compare_op0, dop_compare_op1;

#undef DEBUG

/* um, what? */
#undef TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE dop_target_asm_function_prologue

#undef TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE dop_target_asm_function_epilogue

#undef TARGET_ASM_GLOBALIZE_LABEL
#define TARGET_ASM_GLOBALIZE_LABEL dop_globalize_label

/* never put small data in text (?) */
#undef TARGET_IN_SMALL_DATA_P
#define TARGET_IN_SMALL_DATA_P dop_target_in_small_data_p

/* struct return */
#undef TARGET_STRUCT_VALUE_RTX
#define TARGET_STRUCT_VALUE_RTX dop_target_struct_value_rtx

#undef TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY dop_target_return_in_memory

/*
#undef TARGET_ASM_SELECT_RTX_SECTION
#define TARGET_ASM_SELECT_RTX_SECTION dop_select_rtx_section

#undef TARGET_ASM_SELECT_SECTION
#define TARGET_ASM_SELECT_SECTION dop_select_section
*/

#undef TARGET_ASM_BYTE_OP
#define TARGET_ASM_BYTE_OP "\tdcb\t"
#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\tdch\t"
#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\tdcw\t"
#undef TARGET_ASM_UNALIGNED_HI_OP
#define TARGET_ASM_UNALIGNED_HI_OP "\tdch\t"
#undef TARGET_ASM_UNALIGNED_SI_OP
#define TARGET_ASM_UNALIGNED_SI_OP "\tdcw\t"

static void dop_target_asm_function_prologue PARAMS ((FILE* file, HOST_WIDE_INT size));
static void dop_target_asm_function_epilogue PARAMS ((FILE* file, HOST_WIDE_INT size));
static void dop_globalize_label PARAMS ((FILE* file, const char* name));
static bool dop_target_in_small_data_p PARAMS ((rtx treenode));
/*static void dop_select_rtx_section(mode, x, align);
static void dop_select_section(exp, reloc, align);*/

static rtx dop_target_struct_value_rtx PARAMS ((tree fndecl, int incoming));

static int dop_target_return_in_memory PARAMS ((tree type, tree fntype));

/* warning: TARGET_INITIALIZER refers to some of those dirty macros!
 * so, this must come after the #define lines above
 */
struct gcc_target targetm = TARGET_INITIALIZER;

void dop_globalize_label(stream, name)
    FILE* stream;
    const char* name;
{
  fprintf(stream, "\tglobal _%s\n", name);
}

bool dop_target_in_small_data_p(treenode)
    rtx treenode ATTRIBUTE_UNUSED;
{
  return 1;
}

rtx dop_target_struct_value_rtx(fndecl, incoming)
    tree fndecl;
    int incoming;
{
  return gen_rtx_REG(Pmode, 0);
}

/*
static void dop_select_rtx_section(mode, x, align)
    enum machine_mode mode;
    rtx x;
    unsigned HOST_WIDE_INT align;
{
  //force_data_section();
}

static void dop_select_section(exp, reloc, align)
    tree exp;
    int reloc;
    unsigned HOST_WIDE_INT align;
{
  force_data_section();
}

void dop_readonly_data_section(void)
{
  force_data_section();
}
*/

/* we can only put float data in float regs */
int dop_hard_regno_mode_ok(regno, mode)
    int regno;
    enum machine_mode mode;
{
  if (regno >= FIRST_DOP_GENERAL_REGISTER
      && regno < FIRST_DOP_FLOAT_REGISTER)
    {
      if (GET_MODE_SIZE (mode) > UNITS_PER_WORD)
        return (regno & 1) == 0;
      else
        return 1;
    }
  else if (regno >= FIRST_DOP_FLOAT_REGISTER
           && regno <= LAST_DOP_FLOAT_REGISTER)
    /* We shouldn't put non-float values in float registers, since
       they might get corrupted by rounding, etc.  */
    return GET_MODE_CLASS (mode)==MODE_FLOAT || 
           GET_MODE_CLASS (mode)==MODE_COMPLEX_FLOAT;

  return 0;
}

int dop_modes_tieable_p(mode1, mode2)
    enum machine_mode mode1;
    enum machine_mode mode2;
{
  int mc1 = GET_MODE_CLASS(mode1);
  int mc2 = GET_MODE_CLASS(mode2);
  
  return mc1==mc2;
  
  /* DImode is never tieable */
/*  if ((mode1 == DImode && mode2 != DImode)
      || (mode1 != DImode && mode2 == DImode))
  {
    return 0;
  }
  
  if ((mc1 == MODE_FLOAT || mc1 == MODE_COMPLEX_FLOAT)
      && !(mc2 == MODE_FLOAT || mc2 == MODE_COMPLEX_FLOAT))
  {
    return 0;
  }
  
  if ((mc2 == MODE_FLOAT || mc2 == MODE_COMPLEX_FLOAT)
      && !(mc1 == MODE_FLOAT || mc1 == MODE_COMPLEX_FLOAT))
  {
    return 0;
  }
  
  return 1;*/
}

int dop_class_max_nregs(class, mode)
    int class;
    enum machine_mode mode;
{
  return (class==FLOAT_REGS) ? 1 : DOP_NUM_REGS(mode);
}

void dop_init_cumulative_args (pcum, fntype, libname, fndecl, n_named_args)
    CUMULATIVE_ARGS * pcum;
    tree fntype;
    rtx libname ATTRIBUTE_UNUSED;
    tree fndecl ATTRIBUTE_UNUSED;
    int n_named_args ATTRIBUTE_UNUSED;
{
  /* those ARM guys know what they're doing, so copy their stuff for this... */
  int hidden_arg = (fntype && aggregate_value_p (TREE_TYPE (fntype), fntype));
  /* if we're returning an aggregate type, must start other args at 1 */
  pcum->nregs = hidden_arg ? 1 : 0;
  /* (fntype && AGGREGATE_TYPE_P(fntype)) ? 1 : 0; */
  pcum->fregs = 0;
/*  fprintf(stderr, "We %s using a hidden arg.\n",
    hidden_arg ? "are" : "are not");*/
  #ifdef DEBUG
  fprintf(stderr, "Init cumulative args, nregs=%d fregs=%d\n",
    pcum->nregs, pcum->fregs);
  #endif
}

/* returns zero to put the arg on the stack, or a hard register in which to
 * store the arg
 */
rtx dop_function_arg(pcum, mode, type, named)
    CUMULATIVE_ARGS * pcum;
    enum machine_mode mode;
    tree type;
    int named;
{
  if (GET_MODE_CLASS(mode) == MODE_FLOAT
   || GET_MODE_CLASS(mode) == MODE_COMPLEX_FLOAT)
  {
    if (!named || pcum->fregs >= 8
        || type==void_type_node || MUST_PASS_IN_STACK(mode, type))
    {
/*      fprintf(stderr, "Unnamed float register!\n");*/
      return NULL_RTX;
    }
    else
    {
      /*
      fprintf(stderr, "Gen rtx reg %d\n", pcum->fregs);
      */
/*      fprintf(stderr, "Named float register < 4\n");*/
      return gen_rtx_REG(mode, pcum->fregs+FIRST_DOP_FLOAT_REGISTER);
    }
  }
  else
  {
    if (!named || pcum->nregs >= 8
        || type==void_type_node || MUST_PASS_IN_STACK(mode, type))
    {
      return NULL_RTX;
    }
    else
    {
      rtx retval;
      if (mode == BLKmode)
      {
        /*fprintf(stderr, "I have found a BLKmode arg\n");*/
      }
      retval = gen_rtx_REG(mode, pcum->nregs);
/*      fprintf(stderr, "Function arg %d:\n", pcum->nregs);
      print_rtl(stderr, retval);
      fprintf(stderr, "\n");*/
      return retval;
    }
  }
}

void dop_function_arg_advance(cum, mode, type, named)
    CUMULATIVE_ARGS* cum;
    enum machine_mode mode;
    tree type;
    int named ATTRIBUTE_UNUSED;
{
  if (GET_MODE_CLASS(mode) == MODE_FLOAT
   || GET_MODE_CLASS(mode) == MODE_COMPLEX_FLOAT)
  {
    cum->fregs += 1;
/*    fprintf(stderr, "*Fmode advance: %d %d\n", cum->fregs,
      DOP_NUM_REGS2(mode, type));*/
  }
  else
  {
    #ifdef DEBUG
    fprintf(stderr, "Advance nregs (%d) by %d\n", cum->nregs,
      DOP_NUM_REGS2(mode, type));
    #endif
    cum->nregs += DOP_NUM_REGS2(mode, type);
  }
}

int dop_function_arg_partial_nregs(cum, mode, type, named)
    CUMULATIVE_ARGS* cum;
    enum machine_mode mode;
    tree type;
    int named ATTRIBUTE_UNUSED;
{
  int num = DOP_NUM_REGS2(mode, type);
  int set = (GET_MODE_CLASS(mode) == MODE_FLOAT) ? cum->fregs : cum->nregs;

  /* all must go on the stack */
  if (set >= 8) return 0;

  /* this number in registers */
  if (num + set > 8) {
    #ifdef DEBUG
    fprintf(stderr, "num=%d set=%d partial regs=%d\n", num, set, 8-set);
    #endif
    return 8-set;  /* 4-set ?? */
  }

  /* otherwise, all in registers */
  return 0;
}

int dop_target_return_in_memory(type, fntype)
    tree type, fntype;
{
  HOST_WIDE_INT size;
  
  if (!AGGREGATE_TYPE_P (type))
    return 0;
  
  size = int_size_in_bytes (type);
  
  /* Return aggregates in memory if larger than a word or variable-sized.  */
  return size < 0 || size > UNITS_PER_WORD;
}

int dop_return_in_memory(type)
    tree type;
{
  return dop_target_return_in_memory(type, NULL);
}

int dop_regno_class (regno)
    int regno;
{
  if (regno >= 56 && regno < 64) return PTR_REGS;
  else if (regno < 56) return GENERAL_REGS;
  else if (regno >= 64 && regno < FIRST_PSEUDO_REGISTER) return FLOAT_REGS;
  else return NO_REGS;
}

int dop_legitimate_register(reg, strict)
    rtx reg;
    int strict;
{
  int regno;
  
  /* not a register */
  if (GET_CODE(reg) != REG /*&&
      GET_CODE(reg) != SUBREG*/) return 0;
  
  regno = REGNO(reg);
  
  if (strict)
  {
    /* reloaded & no physical register */
    return REGNO(reg) < FIRST_DOP_FLOAT_REGISTER
        || REGNO(reg) == ARG_POINTER_REGNUM;
  }
  
  return (regno < FIRST_DOP_FLOAT_REGISTER)
       || (regno == 128)
       || (regno >= FIRST_PSEUDO_REGISTER);
}

int dop_legitimate_address (mode, addr, strict)
    enum machine_mode mode;
    rtx addr;
    int strict;
{
  if (dop_legitimate_register (addr, strict))
    return 1;
  else if (GET_CODE (addr) == LABEL_REF)
    return 0;
  else if (GET_CODE (addr) == PLUS
           && dop_legitimate_register (XEXP (addr, 0), strict)
           && GET_CODE (XEXP (addr, 1)) == CONST_INT)
    {
      int offset = INTVAL (XEXP (addr, 1));
      switch (mode)
        {
        case SImode:
        case SFmode:
        case DFmode:
          return offset >= -512 && offset < 512;

        case HImode:
          return offset >= -256 && offset < 256;

        case QImode:
          return offset >= -128 && offset < 128;

        case BLKmode:
          return offset >= -512 && offset < 512;

        default:
          return 0;
        }
    }
  else if (GET_CODE (addr) == PLUS
           && dop_legitimate_register (XEXP (addr, 0), strict)
           && dop_legitimate_register (XEXP (addr, 1), strict))
    return 1;
  
  return 0;
}

void dop_print_reg_expire(stream, x)
    FILE * stream;
    rtx x;
{
  rtx regdead = find_reg_note(current_output_insn, REG_DEAD, x);

  asm_fprintf(stream, "%s%r", regdead ? "~" : "", REGNO(x));
}

void assemble_name_und(stream, name)
    FILE* stream;
    const char* name;
{
  if (name[0] == '*')
    fputc('_', stream);
  
  assemble_name(stream, name);
}

void dop_print_operand (stream, x, code)
    FILE * stream;
    rtx x;
    int code;
{
  switch (code)
  {
    case 'd':
    {
      unsigned int val = INTVAL(x);
      if (val<256)
        fprintf(stream, "%u", val);
      else
        fprintf(stream, "0x%x", val);
    }
    break;
   
    case 'k':
    {
      signed val = INTVAL(x);
      if (val >= -128 && val < 128)
      {
        fprintf(stream, "%u", val & 0xff);
      }
      else abort();
    }
    break;

    case 'z':
    {
      if (GET_CODE(x) == LABEL_REF)
      {
        assemble_name_und(stream, XSTR(x, 0));
      }
      else
      {
        fprintf(stderr, "Trying to use 'z' constraint for rtl:\n");
        print_rtl(stderr, x);
        fprintf(stderr, "\n");
        abort();
      }
    }
    break;
   
    /* kind of assumes there is no fractional part... */
    case 'G':
    {
      if (GET_CODE(x) == CONST_DOUBLE)
      {
        REAL_VALUE_TYPE r;
        HOST_WIDE_INT val;

        REAL_VALUE_FROM_CONST_DOUBLE(r, x);
        val = REAL_VALUE_UNSIGNED_FIX(r);

        if (val<256)
          fprintf(stream, "%lu", val);
        else
          fprintf(stream, "0x%lx", val);
      }
      else abort();
    }
    break;

    case 'v':
    {
      unsigned int val = ~INTVAL(x);
      if (val<256)
        fprintf(stream, "%u", val);
      else
        fprintf(stream, "0x%x", val);
    }
    break;
   
    case 'e':
    {
      unsigned int val = XINT(x, 0);
      
      if ((val & 0x0000ffff) == val)
      {
        fputs("el", stream);
      }
      else if ((val & 0xffff0000) == val)
      {
        fputs("eh", stream);
      }
      else if (((val & 0x0000ffff) | 0xffff0000) == val)
      {
        fputs("fl", stream);
      }
      else if (((val & 0xffff0000) | 0x0000ffff) == val)
      {
        fputs("fh", stream);
      }
    }
    break;
   
    case 'E':
    {
      if (GET_CODE(x) == REG)
        dop_print_reg_expire(stream, x);
      else
        goto Default;
    }
    break;
   
    case 'b':
    {
      if (GET_CODE(x) == PLUS)
      {
        asm_fprintf(stream, "%r", REGNO(XEXP(x, 0)));
      }
      else if (GET_CODE(x) == REG)
      {
        asm_fprintf(stream, "%r", REGNO(x));
      }
      else abort();
    }
    break;
   
    case 'o':
    {
      if (GET_CODE(x) == PLUS)
      {
        fprintf(stream, "%ld", INTVAL(XEXP(x, 1)));
      }
      else if (GET_CODE(x) == CONST_INT)
      {
        fprintf(stream, "%ld", INTVAL(x));
      }
      else abort();
    }
    break;

    case 'R':
    {
      asm_fprintf(stream, "%r", REGNO(x));
    }
    break;
    
    case 'S':
    {
      asm_fprintf(stream, "%r", REGNO(x)+1);
    }
    break;

    case 'T':
    {
      asm_fprintf(stream, "%r", REGNO(x)+2);
    }
    break;

    case 'U':
    {
      asm_fprintf(stream, "%r", REGNO(x)+3);
    }
    break;

    /* call - indirect register means just write register here
     * erm this might be wrong, and actually need an extra level of indirection.
     * fixme.
     */
    case 'c':
    {
      if (GET_CODE(x) == MEM && GET_CODE(XEXP(x, 0)) == REG)
      {
        dop_print_reg_expire(stream, XEXP(x, 0));
/*        asm_fprintf(stream, "%r", REGNO(XEXP(x, 0)));*/
      }
      else goto Default;
    }
    break;

    case 'M':
    {
      if ((GET_CODE(x) == MEM ||
           GET_CODE(x) == POST_INC ||
           GET_CODE(x) == PRE_DEC) &&
          GET_CODE(XEXP(x, 0)) == REG)
      {
/*        dop_print_reg_expire(XEXP(x, 0));*/
        asm_fprintf(stream, "%r", REGNO(XEXP(x, 0)));
      }
      else
      {
        fprintf(stderr, "'M' constraint with rtx:\n");
        print_rtl(stderr, x);
        fprintf(stderr, "\n");
        abort();
      }
    }
    break;
   
    case 'p':
    if (GET_CODE (x) == MEM &&
        (GET_CODE (XEXP (x, 0)) == POST_INC ||
         GET_CODE (XEXP (x, 0)) == PRE_DEC) &&
        GET_CODE (XEXP (XEXP (x, 0), 0)) == REG)
    {
      asm_fprintf(stream, "%r*", REGNO (XEXP (XEXP (x, 0), 0)));
    }
    else abort();
    break;
   
    default:
    {
      Default:
      if (x==0) abort();
      
      if (GET_CODE(x) == REG)
      {
        asm_fprintf(stream, "%r", REGNO(x));
      }
      else if (GET_CODE(x) == MEM)
      {
        /*int dom = MEM_VOLATILE_P(x) ? 1 : 0;*/
        dop_print_operand_address(stream, XEXP(x, 0));
      }
      else if (GET_CODE(x) == CONST_DOUBLE)
      {
        fprintf(stream, "#0.5 !!");
        abort();
      }
      else if (GET_CODE(x) == CONST_INT)
      {
        fprintf(stream, "%ld", INTVAL(x));
      }
      else if (GET_CODE(x) == SYMBOL_REF)
      {
        assemble_name_und(stream, XSTR(x, 0));
/*        fprintf(stream, "_%s", XSTR(x, 0));*/
      }
      else if (GET_CODE(x) == PLUS &&
               GET_CODE(XEXP(x, 0)) == SYMBOL_REF &&
               GET_CODE(XEXP(x, 1)) == CONST_INT)
      {
        assemble_name_und(stream, XSTR(XEXP(x, 0), 0));
        fprintf(stream, " + %ld", INTVAL(XEXP(x, 1)));
      }
      else if (GET_CODE(x) == PLUS)
      {
        print_rtl(stream, x);
      }
      else if (GET_CODE(x)==CONST &&
                 GET_CODE(XEXP(x, 0))==PLUS &&
                   GET_CODE(XEXP(XEXP(x, 0), 0))==SYMBOL_REF &&
                   GET_CODE(XEXP(XEXP(x, 0), 1))==CONST_INT)
      {
        char* symname = XSTR(XEXP(XEXP(x, 0), 0), 0);
        int offset = INTVAL(XEXP(XEXP(x, 0), 1));
        assemble_name_und(stream, symname);
        fprintf(stream, " + %ld", offset);
      }
      else
      {
        abort();
      }
    }
  }
}

void dop_print_operand_address (stream, x)
    FILE* stream;
    rtx x;
{
 /* print_rtl(stderr, x);
  fputs("\n", stderr);*/
  if (GET_CODE(x) == REG) {
    /*
    asm_fprintf(stream, "@0[%r, #0]", REGNO(x));
    */
    asm_fprintf(stream, "[");
    dop_print_reg_expire(stream, x);
    asm_fprintf(stream, ", #0]");
  } else if (GET_CODE(x) == PLUS
             && GET_CODE(XEXP(x, 0)) == REG
             && GET_CODE(XEXP(x, 1)) == CONST_INT) {
    /*
    asm_fprintf(stream, "@0[%r, #%d]", REGNO(XEXP(x, 0)), INTVAL(XEXP(x, 1)));
    */
    asm_fprintf(stream, "[");
    dop_print_reg_expire(stream, XEXP(x, 0));
    asm_fprintf(stream, ", #%d]", INTVAL(XEXP(x, 1)));
  } else if (GET_CODE(x) == PLUS
             && GET_CODE(XEXP(x, 0)) == REG
             && GET_CODE(XEXP(x, 1)) == REG) {
    /*
    asm_fprintf(stream, "@0[%r, %r]", REGNO(XEXP(x, 0)), REGNO(XEXP(x, 1)));
    */
    asm_fprintf(stream, "[");
    dop_print_reg_expire(stream, XEXP(x, 0));
    asm_fprintf(stream, ", ");
    dop_print_reg_expire(stream, XEXP(x, 1));
    asm_fprintf(stream, "]");
  } else if (GET_CODE(x) == SYMBOL_REF) {
    assemble_name_und(stream, XSTR(x, 0));
  } else {
    print_rtl(stderr, x);
    fprintf(stderr, "\nSome unsupported address found\n");
    abort();
  }
}

void dop_output_ascii(file, string, length)
    FILE* file;
    const char* string;
    int length;
{
  int i;
  
  fprintf(file, "\tdcs \"");
  
  for (i=0; i<length; i++)
  {
    unsigned int c = ((const unsigned char*)string)[i];
    if (c=='\"') fputs("\\\"", file);
    else if (c=='\\') fputs("\\\\", file);
    else if (isprint(c)) fputc(c, file);
    else fprintf(file, "\\&%.2x", c);
  }
  
  fprintf(file, "\"\n");
}

void dop_output_float(stream, value)
    FILE* stream;
    REAL_VALUE_TYPE value;
{
  long int wi;
  REAL_VALUE_TO_TARGET_SINGLE(value, wi);
  fprintf(stream, "\tdcw %lx\n", wi);
}

void dop_output_double(stream, value)
    FILE* stream;
    REAL_VALUE_TYPE value;
{
  long int wi[2];
  REAL_VALUE_TO_TARGET_DOUBLE(value, wi);
  fprintf(stream, "\tdcw %lx\n\tdcw %lx\n", wi[0], wi[1]);
}

int dop_const_ok_for_letter_p(val, c)
    unsigned int val;
    int c;
{
  switch (c)
  {
    case 'I':
    {
      /* constant for ALU operations: 0xff000000 0x00ff0000 0x0000ff00.. */
      return (val & 0xff000000) == val || (val & 0x00ff0000) == val
          || (val & 0x0000ff00) == val || (val & 0x000000ff) == val;
    }
    break;
    
    case 'J':
    {
      /* quantities which, if sign extended (contracted), are valid for
       * byte-wide quantity
       */
/*      fprintf(stderr, "Executing this line\n");*/
      return ((signed int)val) >= -128 && ((signed int)val) < 128;
    }
    break;
    
    case 'K':
      {
        return (val & 0xffff) == val;
      }
      break;
    
    case 'L':
      {
        return (val & 0xffff0000) == val;
      }
      break;
        
    case 'M':
    {
      /* valid combinations for mvc instruction */
      return (val & 0xffff0000) == val
          || (val & 0x0000ffff) == val
          || ((val & 0xffff0000) | 0x0000ffff) == val
          || ((val & 0x0000ffff) | 0xffff0000) == val;
    }
    break;
    
    case 'N':
    {
      /* Valid constants for shift instruction.  */
      return val >= 0 && val < 32;
    }
    break;
    
    case 'O':
    {
      /* negative constant for ALU operations */
      return (-val & 0xff000000) == -val || (-val & 0x00ff0000) == -val
          || (-val & 0x0000ff00) == -val || (-val & 0x000000ff) == -val;
    }
    break;
    
    case 'P':
    {
      /* inverse constant for ALU operations */
      return (~val & 0xff000000) == ~val || (~val & 0x00ff0000) == ~val
          || (~val & 0x0000ff00) == ~val || (~val & 0x000000ff) == ~val;
    }
    break;
  }

  return 0;
}

/* fixme! */
int dop_const_double_ok_for_letter_p(val, c)
    rtx val;
    int c;
{
  switch (c)
  {
    case 'G':
    {
      REAL_VALUE_TYPE r, trunc;
      HOST_WIDE_INT intval, lo, hi;
      
      REAL_VALUE_FROM_CONST_DOUBLE (r, val);
      REAL_VALUE_TO_INT (&lo, &hi, r);
      REAL_VALUE_FROM_INT (trunc, lo, hi, DFmode);
      
      if (!REAL_VALUES_EQUAL (trunc, r))
        return 0;
      
      intval = REAL_VALUE_FIX (r);
      
      return intval == (intval & 0xff000000)
          || intval == (intval & 0x00ff0000)
          || intval == (intval & 0x0000ff00)
          || intval == (intval & 0x000000ff);
    }
    break;
  }
  return 0;
}

int dop_extra_constraint(value, c)
    rtx value;
    char c;
{
  switch (c)
  {
    case 's':
      return GET_CODE (value) == SYMBOL_REF;

    case 'z':
      return GET_CODE (value) == LABEL_REF;

    case 'y':
    {
      /* A derefenced register suitable for ldm/stm instruction base ptr, ie
       * r56-r63.
       */
      if (GET_CODE(value)==MEM && GET_CODE(XEXP(value, 0))==REG &&
          ((REGNO(XEXP(value, 0))>=56 && REGNO(XEXP(value, 0))<=63) ||
           REGNO(XEXP(value, 0))>=FIRST_PSEUDO_REGISTER))
      {
        return 1;
      }
    }
    break;

/*    case 't':
    {
      if (GET_CODE(value)==REG
          && ((REGNO(value) >= 56 && REGNO(value) <= 63)
              || REGNO(value) >= FIRST_PSEUDO_REGISTER))
        return 1;
    }
    break; */
 
/*    case 'x':
    {
      if (GET_CODE(value)==MEM)
      {
        rtx x = XEXP (value, 0);
        if (dop_legitimate_address (GET_MODE (x), x, 0))
        {
          if (GET_CODE (x) != POST_INC && GET_CODE (x) != PRE_DEC)
            return 1;
        }
      }
    }
    break;*/

    default:
    abort();
  }
  return 0;
}

rtx dop_gen_compare_reg(code, x, y)
  enum rtx_code code;
  rtx x, y;
{
  enum machine_mode mode = SImode;
  rtx temp = gen_reg_rtx(mode);

  if (!register_operand (y, SImode)
      && !arith_operand (y, SImode))
    y = force_reg (SImode, y);

  emit_move_insn (temp, gen_rtx_fmt_ee (code, mode, x, y));
  
  return temp;
}

rtx dop_gen_setcc(code, x, y)
    enum rtx_code code;
    rtx x, y;
{
  if (!register_operand (y, SImode)
      && !arith_operand (y, SImode))
    y = force_reg (SImode, y);
  
  return gen_rtx_fmt_ee(code, SImode, x, y);
}

rtx dop_gen_load_multiple(int base_regno, int count, rtx from, int up,
                          int write_back, int unchanging_p, int in_struct_p,
                          int scalar_p)
{
  int i=0, j;
  int sign = up ? 1 : -1;
  int soff = up ? 0 : -4;
  rtx result;
  
  result = gen_rtx_PARALLEL(VOIDmode,
    rtvec_alloc(count + (write_back ? 1 : 0)));
  
  if (write_back)
  {
    XVECEXP(result, 0, 0) = gen_rtx_SET (GET_MODE (from), from,
                                         plus_constant (from, count*4*sign));
    i++;
  }
  
  for (j=0; j<count; i++, j++)
  {
    rtx mem = gen_rtx_MEM (SImode, plus_constant (from, soff+j*4*sign));
    RTX_UNCHANGING_P (mem) = unchanging_p;
    MEM_IN_STRUCT_P (mem) = in_struct_p;
    MEM_SCALAR_P (mem) = scalar_p;
    XVECEXP (result, 0, i)
      = gen_rtx_SET(VOIDmode, gen_rtx_REG (SImode, base_regno + j), mem);
  }
  
  return result;
}

int dop_call_saved_regs_size(void)
{
  int i, saved = 0;
  /* int regs, minus stack ptr (never saved) */
  for (i=8; i<64; i++)
  {
    if (regs_ever_live[i] && i!=63) saved+=4;
  }
  /* float regs */
  for (i=72; i<128; i++)
  {
    if (regs_ever_live[i]) saved+=8;
  }
  return saved;
}

static void acc_push PARAMS ((int*, int));

static void acc_push(n, amt)
    int* n;
    int amt;
{
  (*n) += amt;
}

static void zero_diff PARAMS ((FILE*, int*));

static void zero_diff(file, n)
    FILE* file;
    int* n;
{
  int amt = *n;

  if (amt < 0)
  {
    amt = -amt;
    assert(amt < (1<<24));

    if (amt & 0xff0000)
      asm_fprintf(file, "\tsub r63,r63,#%d\n", amt & 0xff0000);

    if (amt & 0xff00)
      asm_fprintf(file, "\tsub r63,r63,#%d\n", amt & 0xff00);

    if (amt & 0xff)
      asm_fprintf(file, "\tsub r63,r63,#%d\n", amt & 0xff);
  }
  else if (amt > 0)
  {
    assert(amt < (1<<24));

    if (amt & 0xff0000)
      asm_fprintf(file, "\tadd r63,r63,#%d\n", amt & 0xff0000);

    if (amt & 0xff00)
      asm_fprintf(file, "\tadd r63,r63,#%d\n", amt & 0xff00);

    if (amt & 0xff)
      asm_fprintf(file, "\tadd r63,r63,#%d\n", amt & 0xff);
  }
  /* we've repayed the difference... */
  *n = 0;
}

typedef struct {
  int regnum[3];
  int count;
} reg123;

static void reg_group_start(regs)
    reg123* regs;
{
  regs->count = 0;
}

static void reg_group_write(file, insn, regs, reg)
    FILE* file;
    char* insn;
    reg123* regs;
    int reg;
{
  regs->regnum[regs->count] = reg;
  regs->count++;
  if (regs->count == 3)
  {
    asm_fprintf(file, "\t%s r63*,[%r, %r, %r]\n",
      insn, regs->regnum[0], regs->regnum[1], regs->regnum[2]);
    regs->count = 0;
  }
}

static void reg_group_end(file, insn, regs)
    FILE* file;
    char* insn;
    reg123* regs;
{
  switch (regs->count)
  {
    case 0:
    break;
    
    case 1:
    asm_fprintf(file, "\t%s r63*,[%r]\n", insn, regs->regnum[0]);
    break;
    
    case 2:
    asm_fprintf(file, "\t%s r63*,[%r, %r]\n", insn, regs->regnum[0],
      regs->regnum[1]);
    break;
    
    case 3:
    asm_fprintf(file, "\t%s r63*,[%r, %r, %r]\n",
      insn, regs->regnum[0], regs->regnum[1], regs->regnum[2]);
    break;
  }
}

static void dop_target_asm_function_prologue(file, size)
    FILE* file;
    HOST_WIDE_INT size;
{
  int r63_used, i;
  reg123 reggroup;
  int vsp = 0;
  
  asm_fprintf(file, "\t// locals=%d\n", get_frame_size());
  asm_fprintf(file, "\t// call_saved=%d\n", dop_call_saved_regs_size());
  asm_fprintf(file, "\t// outgoing_args=%d\n", 
    current_function_outgoing_args_size);
  asm_fprintf(file, "\t// pretend=%d\n", current_function_pretend_args_size);
  
  r63_used = regs_ever_live[63];
  regs_ever_live[63] = 0;

  reg_group_start(&reggroup);
  
  /* pretend args */
  
  for (i=7; i>=0; i--)
  {
    if (i*4 >= 32-current_function_pretend_args_size)
    {
      reg_group_write(file, "stm.d", &reggroup, i);
    }
  }
  
  for (i=62; i>=8; i--)
  {
    if (regs_ever_live[i])
    {
      reg_group_write(file, "stm.d", &reggroup, i);
    }
  }
  
  reg_group_end(file, "stm.d", &reggroup);
  
  reg_group_start(&reggroup);
  
  for (i=127; i>=72; i--)
  {
    if (regs_ever_live[i])
    {
      reg_group_write(file, "stmf.d", &reggroup, i);
    }
  }
  
  reg_group_end(file, "stmf.d", &reggroup);
  
  if (frame_pointer_needed)
  {
    acc_push(&vsp, -size);
    zero_diff(file, &vsp);
    fprintf(file, "\tmov r62, r63\n");
    acc_push(&vsp, -current_function_outgoing_args_size);
    zero_diff(file, &vsp);
  }
  else
  {
    acc_push(&vsp, -(size+current_function_outgoing_args_size));
    zero_diff(file, &vsp);
  }
  
  regs_ever_live[63] = r63_used;
}

static void dop_target_asm_function_epilogue(file, size)
    FILE* file;
    HOST_WIDE_INT size;
{
  int vsp = 0, i;
  reg123 reggroup;
  
  acc_push(&vsp, size + current_function_outgoing_args_size);
  zero_diff(file, &vsp);
  
  reg_group_start(&reggroup);
  
  for (i=72; i<127; i++)
  {
    if (regs_ever_live[i])
    {
      reg_group_write(file, "ldmf.i", &reggroup, i);
    }
  }
  
  reg_group_end(file, "ldmf.i", &reggroup);
  
  reg_group_start(&reggroup);
  
  for (i=8; i<63; i++)
  {
    if (regs_ever_live[i])
    {
      reg_group_write(file, "ldm.i", &reggroup, i);
    }
  }
  
  reg_group_end(file, "ldm.i", &reggroup);

  acc_push(&vsp, current_function_pretend_args_size);
  zero_diff(file, &vsp);

  asm_fprintf(file, "\tret\n");
}

#if 0
static void dop_target_asm_function_prologue(file, size)
    FILE* file;
    HOST_WIDE_INT size;
{
  int i, j, saved=0;
  int pretend = current_function_pretend_args_size;
  int r60_used;
  int any_genregs_saved = 0;
  int any_floatregs_saved = 0;
  int offset = 0;

  /* this isn't part of "size" */
  size += current_function_outgoing_args_size;

  asm_fprintf(file, "\t// locals=%d call_saved=%d outgoing_args=%d\n",
    get_frame_size(), dop_call_saved_regs_size(),
    current_function_outgoing_args_size);
  asm_fprintf(file, "\t// pretend=%d\n", pretend);

  if (pretend > 0)
  {
    acc_push(&offset, -4);
    zero_diff(file, &offset);
    asm_fprintf(file, "\tstm r60,@0{r3-%r}\n", 4-pretend/4);
    acc_push(&offset, -pretend+4);
  }

  /* temporarily remove r60 from saved reg list, if it's there */
  r60_used = regs_ever_live[60];
  regs_ever_live[60] = 0;

  for (i=4; i<64; i++)
  {
    if (regs_ever_live[i]) any_genregs_saved = 1;
  }
  
  if (any_genregs_saved) acc_push(&offset, -4);

  for (i=63; i>=4; i--)
  {
    if (regs_ever_live[i])
    {
      for (j=i; i>=4 && regs_ever_live[i]; i--) ;
      zero_diff(file, &offset);
      asm_fprintf(file, "\tstm r60,@0~{%r-%r}\n", j, i+1);
      acc_push(&offset, (i-j)*4);
      saved += j-i;
    }
  }

  if (any_genregs_saved) acc_push(&offset, 4);

  for (i=68; i<128; i++)
  {
    if (regs_ever_live[i]) any_floatregs_saved = 1;
  }
  
  if (any_floatregs_saved) acc_push(&offset, -8);
  
  /* save float regs */
  for (i=127; i>=68; i--)
  {
    if (regs_ever_live[i])
    {
      for (j=i; i>=68 && regs_ever_live[i]; i--) ;
      zero_diff(file, &offset);
      asm_fprintf(file, "\tstmf r60,@0~{%r-%r}\n", j, i+1);
      acc_push(&offset, (i-j)*8);
    }
  }

  if (any_floatregs_saved) acc_push(&offset, 8);

  if (frame_pointer_needed)
  {
    acc_push(&offset, -size+current_function_outgoing_args_size);
    zero_diff(file, &offset);
    /* frame pointer should point to start of local variables */
    asm_fprintf(file, "\tmov r61,r60\n");
    acc_push(&offset, -current_function_outgoing_args_size);
    zero_diff(file, &offset);
  }
  else
  {
    acc_push(&offset, -size);
    zero_diff(file, &offset);
  }

  /* this should really be done better */  
/*  if (size > 0)
  {
    assert(size < (1<<24));

    if (size & 0xff0000)
      asm_fprintf(file, "\tsub r60,r60,#%d\n", size & 0xff0000);

    if (size & 0xff00)
      asm_fprintf(file, "\tsub r60,r60,#%d\n", size & 0xff00);

    if (size & 0xff)
      asm_fprintf(file, "\tsub r60,r60,#%d\n", size & 0xff);
  }*/

  /* restore r60 */
  regs_ever_live[60] = r60_used;
}

static void dop_target_asm_function_epilogue(file, size)
    FILE* file;
    HOST_WIDE_INT size;
{
  int i, j;
  int pretend = current_function_pretend_args_size;
  int r60_used;
  int offset = 0;

  /* this isn't part of "size" */
  size += current_function_outgoing_args_size;
  
/*  if (size > 0)
  {
    assert(size < (1<<24));

    if (size & 0xff0000)
      asm_fprintf(file, "\tadd r60,r60,#%d\n", size & 0xff0000);

    if (size & 0xff00)
      asm_fprintf(file, "\tadd r60,r60,#%d\n", size & 0xff00);

    if (size & 0xff);
      asm_fprintf(file, "\tadd r60,r60,#%d\n", size & 0xff);
  }*/
  
  acc_push(&offset, size);

  /* temporarily remove r60 from saved reg list, if it's there */
  r60_used = regs_ever_live[60];
  regs_ever_live[60] = 0;

  /* restore float regs */
  for (i=68; i<128; i++)
  {
    if (regs_ever_live[i])
    {
      for (j=i; i<128 && regs_ever_live[i]; i++) ;
      zero_diff(file, &offset);
      asm_fprintf(file, "\tldmf r60,@0{%r-%r}\n", j, i-1);
      acc_push(&offset, (i-j)*8);
    }
  }
  
  for (i=4; i<64; i++)
  {
    if (regs_ever_live[i])
    {
      for (j=i; i<64 && regs_ever_live[i]; i++) ;
      zero_diff(file, &offset);
      asm_fprintf(file, "\tldm r60,@0{%r-%r}\n", j, i-1);
      acc_push(&offset, (i-j)*4);
    }
  }

  acc_push(&offset, pretend);

/*  if (pretend > 0)
  {
    asm_fprintf(file, "\tadd r60,r60,#%d\n", pretend);
  }*/
  zero_diff(file, &offset);

  asm_fprintf(file, "\tret\n");
  
  /* restore r60 */
  regs_ever_live[60] = r60_used;
}

#endif

/**************

 old stack pointer -->      +----+   \
                            |    |   |
                            |    |   | incoming stack args
                            +----+  /
                            |    |   \
                            |    |   |  pretend args
 (virtual) arg pointer -->  +----+  /
                            |    |   \
                            |    |   |  call saved args
                            +----+  /
                            |    |   \
                            |    |   |  locals
 (real) frame pointer -->   +----+  /
                            |    |   \
                            |    |   |  outgoing args
 current stack pointer -->  +----+  /

**************/

unsigned int dop_initial_elimination_offset(from, to)
    unsigned int from;
    unsigned int to;
{
  int locals = get_frame_size();
  int call_saved = dop_call_saved_regs_size();
  int outgoing_args = current_function_outgoing_args_size;
  int pretend = current_function_pretend_args_size;

 /* fprintf(stderr, "locals=%d call_saved=%d outgoing_args=%d\n", locals,
    call_saved, outgoing_args);*/

  if (from==ARG_POINTER_REGNUM && to==STACK_POINTER_REGNUM)
  {
    return locals + call_saved + outgoing_args;
  }
  else if (from==ARG_POINTER_REGNUM && to==FRAME_POINTER_REGNUM)
  {
    return locals + call_saved;
  }
  else if (from==FRAME_POINTER_REGNUM && to==STACK_POINTER_REGNUM)
  {
    return outgoing_args;
  }
  else
  {
    abort();
  }

  return 0;
}

/* we can't load most floating-point constants into a register directly */
enum reg_class dop_preferred_reload_class(x, class)
    rtx x;
    enum reg_class class;
{
  if (GET_CODE(x) == CONST_DOUBLE && !dop_const_double_ok_for_letter_p(x, 'G'))
    return NO_REGS;

  return class;
}

/* try to force float values into float regs, damn inconvenient otherwise */
enum reg_class dop_limit_reload_class(mode, class)
    enum machine_mode mode;
    enum reg_class class;
{
  switch (GET_MODE_CLASS(mode))
  {
    case MODE_FLOAT:
    case MODE_COMPLEX_FLOAT: return FLOAT_REGS; /* FLOAT_OR_GENERAL_REGS; */

    case MODE_INT:
    case MODE_COMPLEX_INT: return GENERAL_REGS;

    default: return class;
  }
}

/* nonzero if objects of mode m in registers of class1 can only be copied to 
   registers of class class2 by storing a register of class1 into memory and 
   loading that memory location into a register of class2.
*/

int dop_secondary_memory_needed(class1, class2, mode)
    enum reg_class class1;
    enum reg_class class2;
    enum machine_mode mode;
{
  return (class1 == FLOAT_REGS && class2 != FLOAT_REGS)
      || (class2 == FLOAT_REGS && class1 != FLOAT_REGS);
}

int dop_cannot_change_mode_class(from, to, class)
    enum machine_mode from, to;
    enum reg_class class;
{
  return GET_MODE_SIZE(from) != GET_MODE_SIZE(to)
    ? reg_classes_intersect_p(FLOAT_REGS, (class)) : 0;
}

/*int dop_secondary_memory_needed_mode(mode)
    enum machine_mode mode;
{
  if (GET_MODE_CLASS(mode) == MODE_FLOAT)
  {
    return mode;
  }
  
  if (GET_MODE_SIZE(mode) >= 4)
  {
    return mode;
  }
  
  return mode_for_size(BITS_PER_WORD, GET_MODE_CLASS(mode), 0);
}*/

/* predicates */

int dop_illegal_mvc_const(op, mode)
    rtx op;
    enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE(op) == CONST_INT)
  {
    return dop_const_ok_for_letter_p(XINT(op, 0), 'N');
  }
  else
    return 0;
}

int dop_legal_mvc_const(op, mode)
    rtx op;
    enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE(op) == CONST_INT)
    return dop_const_ok_for_letter_p(XINT(op, 0), 'M');
  else
    return 0;
}

int dop_not_illegal_const(op, mode)
    rtx op;
    enum machine_mode mode;
{
  if (dop_illegal_mvc_const(op, mode)) {
    return 0;
  }
  return general_operand(op, mode);
}

/*
int any_operand (op, mode)
     rtx op ATTRIBUTE_UNUSED;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  
  return 1;
}
*/

int dop_symbol_operand(op, mode)
    rtx op;
    enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_MODE(op)!=mode) return 0;
  
  if (GET_CODE(op)==SYMBOL_REF) return 1;
  
/*  if (GET_CODE(op)==CONST &&
        GET_CODE(XEXP(op, 0))==PLUS &&
          GET_CODE(XEXP(XEXP(op, 0), 0))==SYMBOL_REF &&
          GET_CODE(XEXP(XEXP(op, 0), 1))==CONST_INT)
  {
    return 1;
  }*/
  
  return 0;
}

int dop_call_valid_operand(op, mode)
    rtx op;
    enum machine_mode mode;
{
  return GET_CODE(op)==SYMBOL_REF ||
    (GET_CODE(op)==REG && GET_MODE_CLASS(mode)==MODE_INT);
}

int dop_branch_valid_operand(op, mode)
    rtx op;
    enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return GET_CODE(op)==SYMBOL_REF || GET_CODE(op)==REG
      || GET_CODE(op)==LABEL_REF || GET_CODE(op)==CODE_LABEL;
}

int dop_multi_mem_operand(op, mode)
    rtx op;
    enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return GET_MODE(op)==mode && GET_CODE(op)==MEM && GET_CODE(XEXP(op, 0))==REG
         && ((REGNO(XEXP(op, 0))>=56 && REGNO(XEXP(op, 0))<=63)
             || REGNO(XEXP(op, 0))>=FIRST_PSEUDO_REGISTER);
}

int load_multiple_operation(op, mode)
    rtx op;
    enum machine_mode mode;
{
  HOST_WIDE_INT count = XVECLEN(op, 0);
  int dest_regno;
  HOST_WIDE_INT i = 1, base = 0;
  rtx elt, src_addr;
  
  if (GET_CODE (XVECEXP (op, 0, 0)) != SET)
    return 0;

  /* might be write back */
  if (GET_CODE (SET_SRC (elt = XVECEXP (op, 0, 0))) == PLUS)
  {
    i++;
    base = 1;
    
    if (GET_CODE (SET_DEST (elt)) != REG
        || GET_CODE (XEXP (SET_SRC (elt), 0)) != REG
        || GET_CODE (XEXP (SET_SRC (elt), 1)) != CONST_INT
        || INTVAL (XEXP (SET_SRC (elt), 1)) != (count-1)*4)
      return 0;
  }

  /* Perform a quick check so we don't blow up below.  */
  if (count <= i
      || GET_CODE (XVECEXP (op, 0, i - 1)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, i - 1))) != REG
      || GET_CODE (SET_SRC (XVECEXP (op, 0, i - 1))) != MEM)
    return 0;

  dest_regno = REGNO (SET_DEST (XVECEXP (op, 0, i - 1)));
  src_addr = XEXP (SET_SRC (XVECEXP (op, 0, i - 1)), 0);

  for (; i<count; i++)
  {
    elt = XVECEXP(op, 0, i);
    
    if (GET_CODE(elt) != SET
        || GET_CODE(SET_DEST(elt)) != REG
        || GET_MODE(SET_DEST(elt)) != SImode
        || REGNO(SET_DEST(elt)) != (unsigned int)(dest_regno + i - base)
        || GET_CODE (SET_SRC(elt)) != MEM
        || GET_MODE (SET_SRC(elt)) != SImode
        || GET_CODE (XEXP (SET_SRC(elt), 0)) != PLUS
        || !rtx_equal_p(XEXP (XEXP (SET_SRC (elt), 0), 0), src_addr)
        || GET_CODE (XEXP (XEXP (SET_SRC (elt), 0), 1)) != CONST_INT
        || INTVAL (XEXP (XEXP (SET_SRC (elt), 0), 1)) != (i - base) * 4)
      return 0;
  }
  
  return 1;
}

int dop_reg_or_imm_operand(op, mode)
    rtx op;
    enum machine_mode mode;
{
  return GET_CODE(op)==CONST_INT || GET_CODE(op)==CONST_DOUBLE
      || (GET_MODE(op)==mode && GET_CODE(op)==REG);
}

int
arith_operand (rtx op, enum machine_mode mode)
{
  if (GET_CODE (op) == CONST_INT &&
      dop_const_ok_for_letter_p (INTVAL (op), 'I'))
    return 1;
  
  return register_operand (op, mode);
}

int
arith_or_neg_operand (rtx op, enum machine_mode mode)
{
  if (GET_CODE (op) == CONST_INT &&
      (dop_const_ok_for_letter_p (INTVAL (op), 'I')
       || dop_const_ok_for_letter_p (INTVAL (op), 'O')))
    return 1;
  
  return register_operand (op, mode);
}

int
arith_or_inv_operand (rtx op, enum machine_mode mode)
{
  if (GET_CODE (op) == CONST_INT &&
      (dop_const_ok_for_letter_p (INTVAL (op), 'I')
       || dop_const_ok_for_letter_p (INTVAL (op), 'P')))
    return 1;
  
  return register_operand (op, mode);
}

int
mvc_const_operand (rtx op, enum machine_mode mode)
{
  return (GET_CODE (op) == CONST_INT &&
          dop_const_ok_for_letter_p (INTVAL (op), 'M'));
}

int
multi_insn_const_operand (rtx op, enum machine_mode mode)
{
  return GET_CODE (op) == SYMBOL_REF
         || (GET_CODE (op) == CONST_INT
             && !dop_const_ok_for_letter_p (INTVAL (op), 'I')
             && !dop_const_ok_for_letter_p (INTVAL (op), 'M')
             && !dop_const_ok_for_letter_p (INTVAL (op), 'O')
             && !dop_const_ok_for_letter_p (INTVAL (op), 'P'));
}

int
symbol_operand (rtx op, enum machine_mode mode)
{
  return GET_CODE (op) == SYMBOL_REF;
}

int
symbol_or_const_operand (rtx op, enum machine_mode mode)
{
  return GET_CODE (op) == SYMBOL_REF
         || GET_CODE (op) == CONST_INT;
}

int fp_op2_operand (rtx op, enum machine_mode mode)
{
  return (GET_CODE (op) == CONST_DOUBLE
          && dop_const_double_ok_for_letter_p (op, 'G'))
         || register_operand (op, mode);
}

/* stolen from ARM port */
int dop_reload_memory_operand(op, mode)
    rtx op;
    enum machine_mode mode;
{
  int regno = true_regnum (op);

  return (!CONSTANT_P (op)
	  && (regno == -1
	      || (GET_CODE (op) == REG
		  && REGNO (op) >= FIRST_PSEUDO_REGISTER)));
}

/*int dop_intreg_operand(op, mode)
    rtx op;
    enum machine_mode mode;
{
  return GET_MODE(op)==mode &&
           GET_CODE(op)==REG && (GET_MODE_CLASS(mode)==MODE_INT
                              || GET_MODE_CLASS(mode)==MODE_COMPLEX_INT);
}*/

/*
int dop_float_operand(op, mode)
    rtx op;
    enum machine_mode mode;
{
  return GET_MODE(op)==mode && GET_CODE(op)==REG && 
                                (GET_MODE_CLASS(mode)==MODE_FLOAT
                              || GET_MODE_CLASS(mode)==MODE_COMPLEX_FLOAT);
}

int dop_float_op2(op, mode)
    rtx op;
    enum machine_mode mode;
{
  return (GET_MODE(op)==mode && GET_CODE(op)==REG && 
                                 (GET_MODE_CLASS(mode)==MODE_FLOAT
                               || GET_MODE_CLASS(mode)==MODE_COMPLEX_FLOAT))
       || CONSTANT_P(op);
}*/

int dop_argp_operand(op, mode)
    rtx op;
    enum machine_mode mode;
{
  return GET_MODE(op)==mode && GET_CODE(op)==REG && REGNO(op)==128;
}

static rtx
dop_subword (enum machine_mode mode, rtx x, int word)
{
  if (GET_CODE (x) == MEM)
    return adjust_address (x, SImode, word * UNITS_PER_WORD);
  
  return simplify_gen_subreg (SImode, x, mode, word * UNITS_PER_WORD);
}

void
dop_move_double (rtx dest, rtx src)
{
  rtx dest_word[2], src_word[2];
  int i;
  int first;
  
  if ((GET_CODE (dest) == REG
       && REGNO_REG_CLASS (REGNO (dest)) == FLOAT_REGS)
      || (GET_CODE (src) == REG
          && REGNO_REG_CLASS (REGNO (src)) == FLOAT_REGS))
    abort ();
  
  for (i = 0; i < 2; i++)
    {
      dest_word[i] = dop_subword (GET_MODE (dest), dest, i);
      src_word[i] = dop_subword (GET_MODE (dest), src, i);
    }
  
  first = (GET_CODE (dest_word[0]) == REG
           && reg_overlap_mentioned_p (dest_word[0], src_word[1]))
          ? 1 : 0;
  
  emit_move_insn (dest_word[first], src_word[first]);
  emit_move_insn (dest_word[1 - first], src_word[1 - first]);
}
