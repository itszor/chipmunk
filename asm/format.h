#ifndef FORMAT_H
#define FORMAT_H 1

#include <config.h>

#include "defs.h"

// Currently n, might need to be extended though. Opcode field is 6 bits wide.
typedef enum {
  opcode_MOV=0,   /* 0 */
  opcode_NOT,
  opcode_LSL,
  opcode_LSR,
  opcode_ASR,     /* 4 */
  opcode_ROR,
  opcode_AND,
  opcode_IOR,
  opcode_EOR,     /* 8 */
  opcode_BIC,
  opcode_ADD,
  opcode_SUB,
  opcode_RSB,     /* 12 */
  opcode_MUL,
  opcode_DIV,
  opcode_UDIV,
  opcode_MOD,     /* 16 */
  opcode_UMOD,
  opcode_MLH,
  opcode_UMLH,
  opcode_MVC,     /* 20 */
  opcode_CMP,
  opcode_UCMP,
  opcode_CMPI,
  opcode_UCMPI,   /* 24 */
  opcode_BFX,
  opcode_LDR,
  opcode_STR,
  opcode_LDM,     /* 28 */
  opcode_STM,
  opcode_SWI,
  opcode_MOVF,
  opcode_NEGF,    /* 32 */
  opcode_ABSF,
  opcode_SQRF,
  opcode_FLTF,
  opcode_FIXF,    /* 36 */
  opcode_ADDF,
  opcode_SUBF,
  opcode_MULF,
  opcode_DIVF,    /* 40 */
  opcode_CMPF,
  opcode_LDF,
  opcode_STF,
  opcode_LDMF,    /* 44 */
  opcode_STMF,
  opcode_TRAPZ,
  opcode_TRAPN,
  opcode_CBR,     /* 48 */
  opcode_CALL,
  opcode_JUMP,
  opcode_RET,
  opcode_SPL,     /* 52 */
  opcode_LDX=61, 
  opcode_LDY=62,
  opcode_LDZ=63
} fmt_opcode_type;

typedef struct {
#ifndef BIGENDIAN
  uint5 unused   : 26;
  uint5 opcode   : 6;
#else
  uint5 opcode   : 6;
  uint5 unused   : 26;
#endif
} fmt_generic_inst;

/* prec_sat is for saturating arithmetic (integer ops) or precision (float ops).
 * values are:
 * VALUE  FLOAT             INT
 *  0     single-precision  wraparound (normal ALU) arithmetic
 *  1     double-precision  signed saturating arithmetic
 *  2     undefined         unsigned saturating arithmetic
 *  3     undefined         undefined
 */
typedef struct {
#ifndef BIGENDIAN
  uint5 rd       : 6;
  uint5 rm       : 6;
  uint5 rn       : 6;
  uint5 unused   : 3;
  uint5 prec_sat : 2;
  uint5 ln       : 1;
  uint5 lm       : 1;
  uint5 i        : 1;
  uint5 opcode   : 6;
#else
  uint5 opcode   : 6;
  uint5 i        : 1;
  uint5 lm       : 1;
  uint5 ln       : 1;
  uint5 prec_sat : 2;
  uint5 unused   : 3;
  uint5 rn       : 6;
  uint5 rm       : 6;
  uint5 rd       : 6;
#endif
} fmt_alu_inst;

typedef struct {
#ifndef BIGENDIAN
  uint5 rd       : 6;
  uint5 rm       : 6;
  uint5 mantissa : 8;
  uint5 bytenum  : 2;
  uint5 prec_sat : 2;
  uint5 lm       : 1;
  uint5 i        : 1;
  uint5 opcode   : 6;
#else
  uint5 opcode   : 6;
  uint5 i        : 1;
  uint5 lm       : 1;
  uint5 prec_sat : 2;
  uint5 bytenum  : 2;
  uint5 mantissa : 8;
  uint5 rm       : 6;
  uint5 rd       : 6;
#endif
} fmt_alui_inst;

typedef struct {
#ifndef BIGENDIAN
  uint5 rd       : 6;
  uint5 imm      : 16;
  uint5 unused   : 1;
  uint5 value    : 1;
  uint5 replace  : 1;
  uint5 high     : 1;
  uint5 opcode   : 6;
#else
  uint5 opcode   : 6;
  uint5 high     : 1;
  uint5 replace  : 1;
  uint5 value    : 1;
  uint5 unused   : 1;
  uint5 imm      : 16;
  uint5 rd       : 6;
#endif
} fmt_mvc_inst;

typedef struct {
#ifndef BIGENDIAN
  uint5 rc       : 6;
  uint5 rm       : 6;
  uint5 rn       : 6;
  uint5 prec_sat : 2;
  uint5 lc       : 1;
  uint5 ln       : 1;
  uint5 cond     : 3;
  uint5 lm       : 1;
  uint5 opcode   : 6;
#else
  uint5 opcode   : 6;
  uint5 lm       : 1;
  uint5 cond     : 3;
  uint5 ln       : 1;
  uint5 lc       : 1;
  uint5 prec_sat : 2;
  uint5 rn       : 6;
  uint5 rm       : 6;
  uint5 rc       : 6;
#endif
} fmt_cmp_inst;

/* there is no precision/saturation or lc for cmpi, so cmp will have to do in
 * some circumstances. This is a bit evil. There is no floating-point
 * compare-immediate. Fixme I guess.
 */
typedef struct {
#ifndef BIGENDIAN
  uint5 rc       : 6;
  uint5 rm       : 6;
  uint5 mantissa : 8;
  uint5 bytenum  : 2;
  uint5 cond     : 3;
  uint5 lm       : 1;
  uint5 opcode   : 6;
#else
  uint5 opcode   : 6;
  uint5 lm       : 1;
  uint5 cond     : 3;
  uint5 bytenum  : 2;
  uint5 mantissa : 8;
  uint5 rm       : 6;
  uint5 rc       : 6;
#endif
} fmt_cmpi_inst;

/*
 * single-memory transfer now has no "domain", but a "volatile" field instead
 * which means it should never be deleted/reordered/merged etc.
 */
typedef struct {
#ifndef BIGENDIAN
  uint5 rd       : 6;
  uint5 rb       : 6;
  uint5 ri       : 6;
  uint5 li       : 1;
  uint5 unused   : 1;
  uint5 lb       : 1;
  uint5 volatil  : 1;
  uint5 width    : 2;
  uint5 ld       : 1;
  uint5 i        : 1;
  uint5 opcode   : 6;
#else
  uint5 opcode   : 6;
  uint5 i        : 1;
  uint5 ld       : 1;
  uint5 width    : 2;
  uint5 volatil  : 1;
  uint5 lb       : 1;
  uint5 unused   : 1;
  uint5 li       : 1;
  uint5 ri       : 6;
  uint5 rb       : 6;
  uint5 rd       : 6;
#endif
} fmt_smem_inst;

typedef struct {
#ifndef BIGENDIAN
  uint5 rd       : 6;
  uint5 rb       : 6;
  uint5 offset   : 8;
  uint5 lb       : 1;
  uint5 volatil  : 1;
  uint5 width    : 2;
  uint5 ld       : 1;
  uint5 i        : 1;
  uint5 opcode   : 6;
#else
  uint5 opcode   : 6;
  uint5 i        : 1;
  uint5 ld       : 1;
  uint5 width    : 2;
  uint5 volatil  : 1;
  uint5 lb       : 1;
  uint5 offset   : 8;
  uint5 rb       : 6;
  uint5 rd       : 6;
#endif
} fmt_smemi_inst;

typedef struct {
#ifndef BIGENDIAN
  uint5 rd       : 6;
  uint5 rm       : 6;
  uint5 start    : 5;
  uint5 end      : 5;
  uint5 unused   : 1;
  uint5 signxtnd : 1;
  uint5 ld       : 1;
  uint5 lm       : 1;
  uint5 opcode   : 6;
#else
  uint5 opcode   : 6;
  uint5 lm       : 1;
  uint5 ld       : 1;
  uint5 signxtnd : 1;
  uint5 unused   : 1;
  uint5 end      : 5;
  uint5 start    : 5;
  uint5 rm       : 6;
  uint5 rd       : 6;
#endif
} fmt_bitfield_inst;

typedef struct {
#ifndef BIGENDIAN
  uint5 ri       : 6;
  uint5 rj       : 6;
  uint5 rk       : 6;
  uint5 rb       : 3;
  uint5 up       : 1;
  uint5 writebk  : 1;
  uint5 li       : 1;
  uint5 lj       : 1;
  uint5 lk       : 1;
  uint5 opcode   : 6;
#else
  uint5 opcode   : 6;
  uint5 lk       : 1;
  uint5 lj       : 1;
  uint5 li       : 1;
  uint5 writebk  : 1;
  uint5 up       : 1;
  uint5 rb       : 3;
  uint5 rk       : 6;
  uint5 rj       : 6;
  uint5 ri       : 6;
#endif
} fmt_mmem_inst;

typedef struct {
#ifndef BIGENDIAN
  uint5 rc       : 6;
  uint5 rx       : 6;
  uint5 ry       : 6;
  uint5 unused   : 3;
  uint5 lc       : 1; /* fix asm to use lc,lx,ly! */
  uint5 lx       : 1;
  uint5 ly       : 1;
  uint5 ix       : 1;
  uint5 iy       : 1;
  uint5 opcode   : 6;
#else
  uint5 opcode   : 6;
  uint5 iy       : 1;
  uint5 ix       : 1;
  uint5 ly       : 1;
  uint5 lx       : 1;
  uint5 lc       : 1; /* fix asm to use lc,lx,ly! */
  uint5 unused   : 3;
  uint5 ry       : 6;
  uint5 rx       : 6;
  uint5 rc       : 6;
#endif
} fmt_tfer_inst;

typedef struct {
#ifndef BIGENDIAN
  uint5 rd       : 6;
  uint5 rm       : 6;
  uint5 rn       : 6;
  uint5 split    : 5;
  uint5 ln       : 1;
  uint5 lm       : 1;
  uint5 shift    : 1;
  uint5 opcode   : 6;
#else
  uint5 opcode   : 6;
  uint5 shift    : 1;
  uint5 lm       : 1;
  uint5 ln       : 1;
  uint5 split    : 5;
  uint5 rn       : 6;
  uint5 rm       : 6;
  uint5 rd       : 6;
#endif
} fmt_spl_inst;

typedef struct {
#ifndef BIGENDIAN
  uint5 number   : 26;
  uint5 opcode   : 6;
#else
  uint5 opcode   : 6;
  uint5 number   : 26;
#endif
} fmt_noop_inst;

typedef enum {
  cond_EQ,
  cond_NE,
  cond_GE,
  cond_GT,
  cond_LE,
  cond_LT,
  cond_AND,
  cond_EOR,
  cond_NAND = 6,
  cond_NEOR = 7
} fmt_condition;

typedef enum {
  width_BYTE,
  width_HALFWORD,
  width_WORD,
  width_TAGWORD
} fmt_datawidth;

typedef union {
  uint5 flat;
  fmt_generic_inst generic;
  fmt_alu_inst alu;
  fmt_alui_inst alui;
  fmt_mvc_inst mvc;
  fmt_cmp_inst cmp;
  fmt_cmpi_inst cmpi;
  fmt_smem_inst smem;
  fmt_smemi_inst smemi;
  fmt_bitfield_inst bitfield;
  fmt_spl_inst spl;
  fmt_mmem_inst mmem;
  fmt_tfer_inst tfer;
  fmt_noop_inst noop;
} fmt_inst;

#endif
