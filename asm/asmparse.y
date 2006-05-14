%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include <elf.h>

#include <defs.h>
#include <bset.h>
#include <cnew.h>
#include <list.h>
#include <clist.h>
#include <hash.h>
#include <map.h>
#include <buffer.h>
#include <config.h>
#include <fixendian.h>

#include "format.h"
#include "parsedefs.h"
#include "elfutil.h"
#include "chasm.h"

#define YYDEBUG 1

void yyerror(char*);
int yylex(void);

static void encodeconstant(uint5 value, uint5* mantissa, uint5* rotate);
void endianfixbundle(bundle* inst);

jt_list* program;
extern int lineno;

image* elfimage;

int chain;

%}

/* Bison declarations */

%union {
  uint5 val;
  char* name;
  jt_buffer* name_buf;
  bundle* inst;
/*  block_info* block;
  block_info* datablock;*/
  reginfo reg;
  livedecl* liveness;
  jt_bset* bitmask;
  jt_clist* instlist;
  jt_clist* datalist;
  jt_list* blocks;
  jt_list* dirlist;
  datum_info* datum;
  reference ref;
  sectionedint secint;
  reginfo3 rlist;
  void* nothing;
}

%start input

%token <val> INTEGER
%token <val> BINOP
%token <val> TRIOP
%token <val> SMEMOP
%token <val> MMEMOP
%token <val> CMPOP
%token <val> TRAPOP
%token <val> BFXOP
%token <val> NOOPOP
%token <val> MVCOP
%token <val> FBINOP
%token <val> FTRIOP
%token <val> FCMPOP
%token <val> FFLTOP
%token <val> FFIXOP
%token <val> FMEMOP
%token <val> FMMEMOP
%token <val> TFEROP0
%token <val> TFEROP1
%token <val> TFEROP2
%token <val> TFEROP3
%token <val> SPLOP
%token <val> LDTOP
%token <name> LABEL
%token <name_buf> STRING
%token ON
%token ELSE
%token CALL
%token LINK
%token RET
%token JUMP
%token BLOCK ENDBLOCK
%token REGPREFIX FREGPREFIX
%token EXPIRE
%token CARRYPOSTFIX
%token <val> CONDITION
%token BYTESIZE
%token HALFSIZE
%token WORDSIZE
%token TAGSIZE
%token <name> LIVEONENTRY
%token <name> LIVEONEXIT
%token DATA RODATA BSS TEXT
%token DCB DCH DCW DCS RES ALIGN
%token END GLOBAL WEAK
%token <val> HEXNUMBER

%type <reg> reg ereg
%type <reg> freg efreg
%type <inst> instr binop ibinop triop itriop swiop mvcop ldtop
%type <inst> tferop tferop3 tferop2 tferop1 tferop0 itrapop itrapiop
%type <inst> smemop simemop mmemop cmpop icmpop trapop trapiop bfop
%type <inst> fbinop fibinop ftriop fitriop fmemop fimemop fmmemop
%type <inst> fcmpop ffixop ffltop splop
%type <datum> item dcb dch dcw dcs reserve align
%type <val> width ftype fullempty lohi writebk volatil updown
%type <bitmask> reglist
%type <nothing> block codeblock codeblocks binding globalbinding weakbinding
%type <nothing> datablocks datablock rodatablocks rodatablock bssblocks bssblock
%type <instlist> instrs
%type <datalist> items
%type <dirlist> directives
%type <liveness> directive livedirective
%type <ref> direct indirect ref
%type <blocks> input
%type <secint> expr immediate
%type <val> intornegint
%type <rlist> ereg123 efreg123

%token YYERROR_VERBOSE

%left '+'
%left '-'
%left NEG  /* unary minus */

%%

/* Grammar rules */

input:	/*empty */ { $$ = 0; }
	| input block
  {
    jt_list* item = jt_list_add(&$1);
    item->data = $2;
  /* fprintf(stderr, "Adding block %p\n", $2);*/
    program = item;
    /* don't implicit chain the first block */
    chain = 0;
    $$ = item;
  }
;

/*
block:	codeblock	{ fprintf(stdout, "Found block\n"); }
			| datablock
;
*/

block: TEXT codeblocks      { $$ = $2; }
     | DATA datablocks      { $$ = $2; }
     | RODATA rodatablocks  { $$ = $2; }
     | BSS bssblocks        { $$ = $2; }
     | binding              { $$ = $1; }
;

binding: globalbinding
       | weakbinding
;

globalbinding: GLOBAL LABEL
  {
    rebindsymbol(&elfimage->symbolmap, $2, &elfimage->symtab, STB_GLOBAL);
    $$ = 0;
  }
;

weakbinding: WEAK LABEL
  {
    rebindsymbol(&elfimage->symbolmap, $2, &elfimage->symtab, STB_WEAK);
    $$ = 0;
  }
;

datablocks: /* empty */  { $$ = 0; }
  | datablocks datablock
  | binding              { $$ = 0; }
  | items
  {
    serialisedata(elfimage->data.content, $1, &elfimage->reldata);
    $$ = 0;
  }
;

datablock: LABEL ':' items
  {
    processsymbol(elfimage, $1, elfimage->data.content->length, sizeof(uint5), 
      STT_OBJECT, SHN_DATA);
    serialisedata(elfimage->data.content, $3, &elfimage->reldata);
    $$ = 0;
  }
;

rodatablocks: /* empty */  { $$ = 0; }
  | rodatablocks rodatablock
  | binding      { $$ = 0; }
  | items
  {
    serialisedata(elfimage->rodata.content, $1, &elfimage->relrodata);
    $$ = 0;
  }
;

rodatablock: LABEL ':' items
  {
    processsymbol(elfimage, $1, elfimage->rodata.content->length,
      sizeof(uint5), STT_OBJECT, SHN_RODATA);
    serialisedata(elfimage->rodata.content, $3, &elfimage->relrodata);
    $$ = 0;
  }
;

bssblocks: /* empty */  { $$ = 0; }
         | bssblocks bssblock
         | binding      { $$ = 0; }
;

bssblock: LABEL ':' items
  {
    uint5 bsslen;

    if (elfimage->bss.content->length == 0)
      bsslen = 0;
    else
      bsslen = *((uint5*)elfimage->bss.content->buffer);
      
    processsymbol(elfimage, $1, bsslen, sizeof(uint5), STT_OBJECT, SHN_BSS);
    serialisebss(elfimage->bss.content, $3, &elfimage->relbss);
    $$ = 0;
  }
;

items:	/* empty */
  {
    $$ = jt_clist_new();
  }
	| items item
  {
    jt_clist* newitem = jt_clist_append($1);
    newitem->data = $2;
    $$ = $1;
  }
  | items binding
  {
    $$ = $1;
  }
;

item:	dcb | dch | dcw | dcs | reserve | align
;

dcb:	DCB expr
  {
    datum_info* item = newdata();
    item->type = type_BYTE;
    item->content.value = $2.offset;
    if ($2.symbolname)
    {
      adddelayedreloc(&item->delayedrelocs, $2.symbolname, R_CHAM_DCV, 0);
    }
    $$ = item;
  }
;

dch:	DCH expr
  {
    datum_info* item = newdata();
    item->type = type_HALF;
    item->content.value = $2.offset;
    if ($2.symbolname)
    {
      adddelayedreloc(&item->delayedrelocs, $2.symbolname, R_CHAM_DCV, 0);
    }
    $$ = item;
  }
;

dcw:	DCW expr
  {
    datum_info* item = newdata();
    item->type = type_WORD;
    item->content.value = $2.offset;
    if ($2.symbolname)
    {
      adddelayedreloc(&item->delayedrelocs, $2.symbolname, R_CHAM_DCV, 0);
    }
    $$ = item;
  }
;

dcs:  DCS STRING
  {
    datum_info* item = newdata();
    item->type = type_STRING;
    item->content.ptr = $2;
    $$ = item;
  }
;

reserve: RES INTEGER
  {
    datum_info* item = newdata();
    item->type = type_RESERVE;
    item->content.value = $2;
    $$ = item;
  }
;

align: ALIGN INTEGER
  {
    datum_info* item = newdata();
    item->type = type_ALIGN;
    item->content.value = $2;
    $$ = item;
  }
;

codeblocks: /* empty */           { $$ = 0; }
          | codeblocks codeblock  { $$ = $1; }
          | codeblocks binding    { $$ = $1; }
;

codeblock: LABEL ':'
  {
  /*  jt_map* entry;
    mapentry* sym;
    uint5 mysymno = elfimage->symtab.buf->length/sizeof(Elf32_Sym);  // groo
        
    if ((entry = jt_map_lookup(elfimage->symbolmap, $1)))
    {
      sym = entry->value;
      while (sym->backpatches)
      {
        backpatch* patch = sym->backpatches->data;
        Elf32_Rel myrel;
      
        // add relocation for this entry
        myrel.r_offset = patch->value;
        myrel.r_info = ELF32_R_INFO(mysymno, R_386_JMP_SLOT);
        jt_buffer_append(elfimage->reltext.buf, &myrel, sizeof(myrel));
        fprintf(stderr, "Forward relocation: %s at %d\n", $1, patch->value);
      
        jt_delete(patch);
        jt_list_removehead(&sym->backpatches);
      }
      sym->symtabentry = mysymno;  // this entry
    }
    else  // new symbol, no previous forward references
    {
      sym = newmapentry($1, STB_LOCAL, mysymno);
      jt_map_insert(&elfimage->symbolmap, $1, sym);
    }
    
    newsymbol(&elfimage->strtab, &elfimage->symtab, $1, 
      elfimage->text.buf->length, sizeof(uint5), sym->binding, STT_FUNC,
      SHN_TEXT);
    fprintf(stderr, "Symbol: %s at %d\n", $1, elfimage->text.buf->length);*/
    
    if (chain)
    {
      bundle* inst = newinst(2);
      uint5 start;
     /* char* privlabel = genlabel();*/
      #ifdef DEBUG
      fprintf(stderr, "Implicit chain to %s\n", $1);
      #endif
      
      /* build ldx:jump instruction */
      inst->inst[0].noop.opcode = opcode_LDX;
      inst->inst[0].noop.number = 0;
      inst->inst[1].tfer.opcode = opcode_JUMP;
      inst->inst[1].tfer.ix = 1;
      inst->inst[1].tfer.rx = 0;
      inst->inst[1].tfer.iy = 0;
      inst->inst[1].tfer.ry = 0;
      inst->inst[1].tfer.rc = 0;
      adddelayedreloc(&inst->delayedrelocs, $1, R_CHAM_INDIRECT, 0);

      endianfixbundle(inst);

      start = jt_buffer_append(elfimage->text.content, inst->inst, 
        inst->len*sizeof(uint5));

      while (inst->delayedrelocs)
      {
        delayedreloc* dr = inst->delayedrelocs->data;
        
        newreloc(&elfimage->reltext, &elfimage->symbolmap,
          dr->name, dr->type, start+dr->offset*4);
          
        jt_delete(dr);
        jt_list_removehead(&inst->delayedrelocs);
      }
    }
    #ifdef DEBUG
    else
    {
      fprintf(stderr, "No implicit chain\n");
    }
    #endif
    
    processsymbol(elfimage, $1, elfimage->text.content->length, sizeof(uint5),
      STT_FUNC, SHN_TEXT);
    /* implicitly chain the next block, until it knows otherwise */
    chain = 1;
  }
  directives
  {
 /*   while ($4)
    {
      livedecl* dir = $4->data;
      switch (dir->type)
      {
        case LIVEONENTRY:
        $<block>3->liveonentry = dir->bitmask;
        break;

        case LIVEONEXIT:
        $<block>3->liveonexit = dir->bitmask;
        break;
      }
      jt_delete(dir);
      jt_list_removehead(&$4);
    }*/
  }
  instrs
  {
    jt_clist* scan;
/*    jt_clist *newitem; */
    jt_buffer* text = elfimage->text.content;
  /*  fmt_inst inst;*/

/*  fprintf(stderr, "Mid-rule wotsit: $6=%.8x, $6->next=%.8x\n", $6, $6->next);
*/
/*    for (scan=$6->next; scan->data; scan=scan->next)
    {
      fmt_inst inst;
      inst.flat = *((uint5*)scan->data);
//      fprintf(stderr, "Append %.8x\n", inst.flat);
      block_appendi($<block>3, inst);
    }*/
    
    for (scan=$6->next; scan->data; scan=scan->next)
    {
      bundle* instr = scan->data;
      uint5 start;

      assert(instr);

      endianfixbundle(instr);

      start = jt_buffer_append(text, instr->inst, instr->len*sizeof(uint5));

      while (instr->delayedrelocs)
      {
        delayedreloc* dr = instr->delayedrelocs->data;

        newreloc(&elfimage->reltext, &elfimage->symbolmap,
          dr->name, dr->type, start+dr->offset*4);

        jt_delete(dr);
        jt_list_removehead(&instr->delayedrelocs);
      }
    }
  }
;

instrs:		/* empty */
  {
    $$ = jt_clist_new();
  }
	| instrs instr
  {
    jt_clist* newitem = jt_clist_append($1);
    newitem->data = $2;
    $$ = $1;  /* keep the same anchor element */
  }
;

instr:    binop
        | ibinop
        | triop
        | itriop
        | mvcop
        | cmpop
        | icmpop
        | trapop
        | trapiop
        | itrapop
        | itrapiop
        | smemop
        | simemop
        | mmemop
        | bfop
        | splop
        | fbinop
        | fibinop
        | ftriop
        | fitriop
        | fcmpop
        | fmemop
        | fimemop
        | fmmemop
        | ffixop
        | ffltop
        | swiop
        | ldtop
        | tferop
;

reg:	REGPREFIX INTEGER
  {
    if ($2<0 || $2>63)
    {
      fprintf(stderr, "Bad register (%d) at line %d\n", $2, lineno);
      exit(1);
    }
    $$.reg = $2;
    $$.expire = 0;
  }
;

ereg: reg         { $$ = $1; $$.expire = 0; }
    | EXPIRE reg  { $$ = $2; $$.expire = 1; }
;

freg:	FREGPREFIX INTEGER
  {
    if ($2<0 || $2>63)
    {
      fprintf(stderr, "Bad register (%d) at line %d\n", $2, lineno);
      exit(1);
    }
    $$.reg = $2;
    $$.expire = 0;
  }
;

efreg: freg  			 { $$ = $1; $$.expire = 0; }
     | EXPIRE freg { $$ = $2; $$.expire = 1; }
;

immediate:	'#' expr	{ $$ = $2; }
/*  fprintf(stderr, "Immediate expr: (%d+%d*%s)\n", $2.offset, $2.multiplier, 
    $2.symbolname ? $2.symbolname : "(null)");*/
;

intornegint: INTEGER { $$ = $1; }
       | '-' INTEGER { $$ = -$2; }

/* this is totally evil. */
expr:	INTEGER
  {
    sectionedint num;
    num.offset = $1;
    num.symbolname = 0;
    num.multiplier = 1;
    $$ = num;
  }
  | LABEL
  {
    sectionedint num;
    num.offset = 0;
    num.symbolname = $1;
    num.multiplier = 1;
    $$ = num;
  }
  | '(' expr ')'   { $$ = $2; }
  | expr '+' expr
  {
    $$ = proparith($1, '+', $3);
  }
  | expr '-' expr
  {
    $$ = proparith($1, '-', $3);
  }
  | '-' expr %prec NEG
  {
    sectionedint zero;
    zero.symbolname = NULL;
    zero.offset = 0;
    zero.multiplier = 1;
    $$ = proparith(zero, '-', $2);
  }
;

binop:	BINOP reg ',' ereg
  {
    fmt_inst inst;
    
    inst.flat = 0;

    inst.alu.opcode = $1;

    inst.alu.rd = $2.reg;
    inst.alu.rm = 0;
    inst.alu.lm = 0;
    inst.alu.rn = $4.reg;
    inst.alu.ln = $4.expire;
    inst.alu.i = 0;
    
    $$ = ci(inst);
  }
;

fbinop:  FBINOP '.' ftype freg ',' efreg
  {
    fmt_inst inst;

    inst.flat = 0;

    inst.alu.opcode = $1;

    inst.alu.prec_sat = fwidth($3);

    inst.alu.rd = $4.reg;
    inst.alu.rm = 0;
    inst.alu.lm = 0;
    inst.alu.rn = $6.reg;
    inst.alu.ln = $6.expire;
    inst.alu.i = 0;
    
    $$ = ci(inst);
  }
;

ibinop:	BINOP reg ',' immediate
  {
    fmt_inst inst;
    uint5 mantissa, bytenum;

    inst.flat = 0;

    inst.alui.opcode = $1;

    inst.alui.rd = $2.reg;
    inst.alui.lm = 0;
    encodeconstant($4.offset, &mantissa, &bytenum);
    inst.alui.mantissa = mantissa;
    inst.alui.bytenum = bytenum;
    inst.alui.i = 1;
    
    $$ = ci(inst);
/*    fprintf(stderr, "Here, immediate binop value=%d\n", inst.flat);*/
  }
;

fibinop:	FBINOP '.' ftype freg ',' immediate
  {
    fmt_inst inst;
    uint5 mantissa, bytenum;

    inst.flat = 0;

    inst.alui.opcode = $1;

    inst.alui.prec_sat = fwidth($3);

    inst.alui.rd = $4.reg;
    inst.alui.lm = 0;
    encodeconstant($6.offset, &mantissa, &bytenum);
    inst.alui.mantissa = mantissa;
    inst.alui.bytenum = bytenum;
    inst.alui.i = 1;
    
    $$ = ci(inst);
/*    fprintf(stderr, "Here, immediate binop value=%d\n", inst.flat);*/
  }
;

triop:	TRIOP reg ',' ereg ',' ereg
  {
    fmt_inst inst;

    inst.flat = 0;

    inst.alu.opcode = $1;

    inst.alu.rd = $2.reg;
    inst.alu.rm = $4.reg;
    inst.alu.lm = $4.expire;
    inst.alu.rn = $6.reg;
    inst.alu.ln = $6.expire;
    inst.alu.i = 0;
    
    $$ = ci(inst);
  }
;

ftriop:	FTRIOP '.' ftype freg ',' efreg ',' efreg
  {
    fmt_inst inst;

    inst.flat = 0;

    inst.alu.opcode = $1;

    inst.alu.prec_sat = fwidth($3);

    inst.alu.rd = $4.reg;
    inst.alu.rm = $6.reg;
    inst.alu.lm = $6.expire;
    inst.alu.rn = $8.reg;
    inst.alu.ln = $8.expire;
    inst.alu.i = 0;
    
    $$ = ci(inst);
  }
;

itriop:	TRIOP reg ',' ereg ',' immediate
  {
    fmt_inst inst;
    uint5 mantissa, bytenum;

    inst.flat = 0;

    inst.alui.opcode = $1;

    inst.alui.rd = $2.reg;
    inst.alui.rm = $4.reg;
    inst.alui.lm = $4.expire;
    encodeconstant($6.offset, &mantissa, &bytenum);
    inst.alui.mantissa = mantissa;
    inst.alui.bytenum = bytenum;
    inst.alui.i = 1;
    
    $$ = ci(inst);
/*    fprintf(stderr, "Here, immediate triop value=%.8x\n", inst.flat);*/
  }
;

fitriop:	FTRIOP '.' ftype freg ',' efreg ',' immediate
  {
    fmt_inst inst;
    uint5 mantissa, bytenum;

    inst.flat = 0;

    inst.alui.opcode = $1;

    inst.alui.prec_sat = fwidth($3);

    inst.alui.rd = $4.reg;
    inst.alui.rm = $6.reg;
    inst.alui.lm = $6.expire;
    encodeconstant($8.offset, &mantissa, &bytenum);
    inst.alui.mantissa = mantissa;
    inst.alui.bytenum = bytenum;
    inst.alui.i = 1;
    
    $$ = ci(inst);
/*    fprintf(stderr, "Here, immediate triop value=%.8x\n", inst.flat);*/
  }
;

lohi: 'l' { $$ = 'l'; }
    | 'h' { $$ = 'h'; }
;

fullempty: /* nothing */ { $$ = ' '; }
                   | 'f' { $$ = 'f'; }
                   | 'e' { $$ = 'e'; }
;

mvcop:  MVCOP '.' fullempty lohi reg ',' immediate
  {
    bundle* inst = newinst(1);
/*    Elf32_Rel myrel;
    jt_buffer* text = elfimage->text.buf;*/
    uint5 reltype;
    
    inst->inst[0].mvc.opcode = $1;
    inst->inst[0].mvc.rd = $5.reg;

    if ($7.offset != 0 && $7.symbolname)
    {
      fprintf(stderr, "Non-zero offsets not implemented at %d\n", lineno);
     /* exit(1);*/
    }

    switch ($4)
    {
      case 'l':
      inst->inst[0].mvc.imm = $7.offset & 0xffff;
      inst->inst[0].mvc.high = 0;
      reltype = R_CHAM_ADRL;
      break;
      
      case 'h':
      inst->inst[0].mvc.imm = $7.offset >> 16;
      inst->inst[0].mvc.high = 1;
      reltype = R_CHAM_ADRH;
      break;
      
      default:
      fprintf(stderr, "Bad halfword specifier for MVCOP at %d\n", lineno);
      exit(1);
    }

    switch ($3)
    {
      case ' ':
      inst->inst[0].mvc.replace = 0;
      inst->inst[0].mvc.value = 0;
      break;
      
      case 'f':
      inst->inst[0].mvc.replace = 1;
      inst->inst[0].mvc.value = 1;
      break;
      
      case 'e':
      inst->inst[0].mvc.replace = 1;
      inst->inst[0].mvc.value = 0;
      break;
      
      default:
      fprintf(stderr, "Bad emptiness value for MVCOP at %d\n", lineno);
    }

    if ($7.symbolname)
    {
      if ($7.multiplier != 1)
      {
        fprintf(stderr, "You can only add a symbol reference currently at "
          "line %d\n", lineno);
        exit(1);
      }
    
      /* need to add a relocation entry or something */
      adddelayedreloc(&inst->delayedrelocs, $7.symbolname, reltype, 0);

      /*fprintf(stderr, "Added MVC relocation for symbol %s\n", $7.symbolname);*/
    }
        
    $$ = inst;
  }
;

cmpop:	CMPOP '.' CONDITION reg ',' ereg ',' ereg
  {
    fmt_inst inst;
    
    inst.flat = 0;
    
    inst.cmp.opcode = $1;

    inst.cmp.rc = $4.reg;
    inst.cmp.lc = 0;
    inst.cmp.rm = $6.reg;
    inst.cmp.lm = $6.expire;
    inst.cmp.rn = $8.reg;
    inst.cmp.ln = $8.expire;

    inst.cmp.cond = $3;
    
/*    inst.cmp.i = 0;*/
    
    $$ = ci(inst);
  }
;

fcmpop:	FCMPOP '.' ftype '.' CONDITION reg ',' efreg ',' efreg
  {
    fmt_inst inst;
    
    inst.flat = 0;
    
    inst.cmp.opcode = $1;

    inst.cmp.prec_sat = fwidth($3);

    inst.cmp.rc = $6.reg;
    inst.cmp.lc = 0;
    inst.cmp.rm = $8.reg;
    inst.cmp.lm = $8.expire;
    inst.cmp.rn = $10.reg;
    inst.cmp.ln = $10.expire;

    inst.cmp.cond = $5;
    
/*    inst.cmp.i = 0;*/
    
    $$ = ci(inst);
  }
;

icmpop:	CMPOP '.' CONDITION reg ',' ereg ',' immediate
  {
    fmt_inst inst;
    uint5 mantissa, bytenum;
    
    inst.flat = 0;
    
    inst.cmpi.opcode = $1+2;

    inst.cmpi.rc = $4.reg;
    inst.cmpi.rm = $6.reg;
    inst.cmpi.lm = $6.expire;
    
    encodeconstant($8.offset, &mantissa, &bytenum);
    
    inst.cmpi.mantissa = mantissa;
    inst.cmpi.bytenum = bytenum;

    inst.cmpi.cond = $3;
    
/*    inst.cmpi.i = 1;*/
    
    $$ = ci(inst);
  }
;

trapop:	TRAPOP '.' CONDITION LABEL ',' ereg ',' ereg
  {
    bundle* inst = newinst(2);
    
    inst->inst[1].cmp.opcode = $1;

    inst->inst[1].cmp.lc = 0;
    inst->inst[0].noop.opcode = opcode_LDZ;
    inst->inst[0].noop.number = 0;
    inst->inst[1].cmp.rc = 0;
    inst->inst[1].cmp.rm = $6.reg;
    inst->inst[1].cmp.lm = $6.expire;
    inst->inst[1].cmp.rn = $8.reg;
    inst->inst[1].cmp.ln = $8.expire;

/*    inst.cmp.i = 0;*/
    
    inst->inst[0].cmp.cond = $3;

    adddelayedreloc(&inst->delayedrelocs, $4, R_CHAM_INDIRECT, 0);
    
    $$ = inst;
  }
;

itrapop:	TRAPOP '.' CONDITION LABEL ',' ereg ',' immediate
  {
    bundle* inst = newinst(2);
    uint5 mantissa, bytenum;
    
    inst->len = 2;
    
    inst->inst[1].cmpi.opcode = $1+1;

    inst->inst[0].noop.opcode = opcode_LDZ;
    inst->inst[0].noop.number = 0;
    inst->inst[1].cmpi.rc = 0;
    inst->inst[1].cmpi.rm = $6.reg;
    inst->inst[1].cmpi.lm = $6.expire;
        
    encodeconstant($8.offset, &mantissa, &bytenum);
    
    inst->inst[1].cmpi.mantissa = mantissa;
    inst->inst[1].cmpi.bytenum = bytenum;

    inst->inst[1].cmpi.cond = $3;
    
    adddelayedreloc(&inst->delayedrelocs, $4, R_CHAM_INDIRECT, 0);
    
/*    inst.cmpi.i = 1;*/
    
    $$ = inst;
  }
;

trapiop:	TRAPOP '.' CONDITION ereg ',' ereg ',' ereg
  {
    fmt_inst inst;
    
    inst.flat = 0;
    
    inst.cmp.opcode = $1;

    inst.cmp.rc = $4.reg;
    inst.cmp.lc = $4.expire;
    inst.cmp.rm = $6.reg;
    inst.cmp.lm = $6.expire;
    inst.cmp.rn = $8.reg;
    inst.cmp.ln = $8.expire;
    
/*    inst.cmp.i = 0;*/
    
    $$ = ci(inst);
  }
;

itrapiop:	TRAPOP '.' CONDITION ereg ',' ereg ',' immediate
  {
    fmt_inst inst;
    uint5 mantissa, bytenum;
    
    inst.flat = 0;
    
    inst.cmpi.opcode = $1+1;

    inst.cmpi.rc = $4.reg;
    inst.cmpi.rm = $6.reg;
    inst.cmpi.lm = $6.expire;
        
    encodeconstant($8.offset, &mantissa, &bytenum);
    
    inst.cmpi.mantissa = mantissa;
    inst.cmpi.bytenum = bytenum;

    inst.cmpi.cond = $3;
    
/*    inst.cmpi.i = 1;*/
    
    $$ = ci(inst);
  }
;

width:	'b' { $$ = 'b'; }
      | 'h'	{ $$ = 'h'; }
      | 'w'	{ $$ = 'w'; }
      | 't' { $$ = 't'; }
;

volatil: /* empty */ { $$ = 0; }
       | 'v'         { $$ = 1; }

smemop:	SMEMOP '.' width volatil ereg ',' '[' ereg ',' ereg ']'
  {
    fmt_inst inst;

    inst.flat = 0;

    inst.smem.opcode = $1;

    inst.smem.volatil = $4;
        
    inst.smem.rd = $5.reg;
    inst.smem.ld = $5.expire;
    inst.smem.rb = $8.reg;
    inst.smem.lb = $8.expire;
    inst.smem.ri = $10.reg;
    inst.smem.li = $10.expire;

    inst.smem.width = iwidth($3);
    
    inst.smem.i = 0;
    
    $$ = ci(inst);
  }
;

ftype:	's' { $$ = 's'; }
      | 'd' { $$ = 'd'; }
      | 'X' { $$ = 'X'; }
      | 'Y' { $$ = 'Y'; }
;

fmemop:	FMEMOP '.' ftype volatil efreg ',' '[' ereg ',' ereg ']'
  {
    fmt_inst inst;

    inst.flat = 0;

    inst.smem.opcode = $1;

    inst.smem.volatil = $4;
        
    inst.smem.rd = $5.reg;
    inst.smem.ld = $5.expire;
    inst.smem.rb = $8.reg;
    inst.smem.lb = $8.expire;
    inst.smem.ri = $10.reg;
    inst.smem.li = $10.expire;

    inst.smem.width = fwidth($3);
    
    inst.smem.i = 0;
    
    $$ = ci(inst);
  }
;

simemop: SMEMOP '.' width volatil ereg ',' '[' ereg ',' '#' intornegint ']'
  {
    fmt_inst inst;
    sint5 offsetvalue;
    const sint5 div[] = {1,2,4,4};
    
    inst.flat = 0;
    
    inst.smemi.opcode = $1;
    
    inst.smemi.volatil = $4;
    
    inst.smemi.rd = $5.reg;
    inst.smemi.ld = $5.expire;
    inst.smemi.rb = $8.reg;
    inst.smemi.lb = $8.expire;
    
    inst.smemi.width = iwidth($3);
    
    offsetvalue = (sint5)$11 / div[inst.smemi.width];
    
    if (offsetvalue<-128 || offsetvalue>127)
    {
      fprintf(stderr, "Immediate value %d out of range at %d\n",
        $11, lineno);
      exit(1);
    }
    
    inst.smemi.offset = offsetvalue;
    inst.smemi.i = 1;
    
    $$ = ci(inst);
  }
;

fimemop: FMEMOP '.' ftype volatil efreg ',' '[' ereg ',' '#' intornegint ']'
  {
    fmt_inst inst;
    sint5 offsetvalue;
    const sint5 div[] = {4,4,4,4};
    
    inst.flat = 0;
    
    inst.smemi.opcode = $1;
    
    inst.smemi.volatil = $4;
    
    inst.smemi.rd = $5.reg;
    inst.smemi.ld = $5.expire;
    inst.smemi.rb = $8.reg;
    inst.smemi.lb = $8.expire;
    
    inst.smemi.width = fwidth($3);
    
    offsetvalue = (sint5)$11 / div[inst.smemi.width];
    
    if (offsetvalue<-128 || offsetvalue>127)
    {
      fprintf(stderr, "Immediate value %d out of range at %d\n",
        $11, lineno);
      exit(1);
    }
    
    inst.smemi.offset = offsetvalue;
    inst.smemi.i = 1;
    
    $$ = ci(inst);
  }
;

/*
writeback: 			{ $$ = 0; }
	| '!'					{ $$ = 1; }
;

afterbefore: 'a' { $$ = 0; }
					 | 'b' { $$ = 1; }
;
*/

updown:	'i' { $$ = 1; }
      | 'd' { $$ = 0; }
;

writebk: /* nothing */ { $$ = 0; }
       | '*'           { $$ = 1; }
;

ereg123: ereg ',' ereg ',' ereg
  {
    reginfo3 rlist;
    rlist.r[0] = $1;
    rlist.r[1] = $3;
    rlist.r[2] = $5;
    $$ = rlist;
  }
  | ereg ',' ereg
  {
    reginfo3 rlist;
    rlist.r[0] = $1;
    rlist.r[1] = $3;
    rlist.r[2].reg = 63;
    rlist.r[2].expire = 0;
    $$ = rlist;
  }
  | ereg
  {
    reginfo3 rlist;
    rlist.r[0] = $1;
    rlist.r[1].reg = rlist.r[2].reg = 63;
    rlist.r[1].expire = rlist.r[2].expire = 0;
    $$ = rlist;
  }
;

mmemop: MMEMOP '.' updown ereg writebk ',' '[' ereg123 ']'
  {
    fmt_inst inst;

    inst.flat = 0;

    inst.mmem.opcode = $1;
    
    if ($4.reg >= 56 && $4.reg <= 63)
    {
      inst.mmem.rb = $4.reg & 7;
    }
    else
    {
      fprintf(stderr, "Bad base register (%d) at %d\n",
        $4.reg, lineno);
      exit(1);
    }
    
    inst.mmem.writebk = $5;
    inst.mmem.up = $3;
    
    inst.mmem.ri = $8.r[0].reg;
    inst.mmem.rj = $8.r[1].reg;
    inst.mmem.rk = $8.r[2].reg;
    inst.mmem.li = $8.r[0].expire;
    inst.mmem.lj = $8.r[1].expire;
    inst.mmem.lk = $8.r[2].expire;
    
    $$ = ci(inst);
  }
;

efreg123: efreg ',' efreg ',' efreg
  {
    reginfo3 rlist;
    rlist.r[0] = $1;
    rlist.r[1] = $3;
    rlist.r[2] = $5;
    $$ = rlist;
  }
  | efreg ',' efreg
  {
    reginfo3 rlist;
    rlist.r[0] = $1;
    rlist.r[1] = $3;
    rlist.r[2].reg = 63;
    rlist.r[2].expire = 0;
    $$ = rlist;
  }
  | efreg
  {
    reginfo3 rlist;
    rlist.r[0] = $1;
    rlist.r[1].reg = rlist.r[2].reg = 63;
    rlist.r[1].expire = rlist.r[2].expire = 0;
    $$ = rlist;
  }
;

fmmemop: FMMEMOP '.' updown ereg writebk ',' '[' efreg123 ']'
  {
    fmt_inst inst;

    inst.flat = 0;

    inst.mmem.opcode = $1;
    
    if ($4.reg >= 56 && $4.reg <= 63)
    {
      inst.mmem.rb = $4.reg & 7;
    }
    else
    {
      fprintf(stderr, "Bad base register (%d) at %d\n",
        $4.reg, lineno);
      exit(1);
    }

    inst.mmem.writebk = $5;
    inst.mmem.up = $3;
    
    inst.mmem.ri = $8.r[0].reg;
    inst.mmem.rj = $8.r[1].reg;
    inst.mmem.rk = $8.r[2].reg;
    inst.mmem.li = $8.r[0].expire;
    inst.mmem.lj = $8.r[1].expire;
    inst.mmem.lk = $8.r[2].expire;

    $$ = ci(inst);
  }
;

bfop:	BFXOP reg ',' ereg '<' INTEGER ',' INTEGER '>'
  {
    fmt_inst inst;
    
    inst.flat = 0;
    
    inst.bitfield.opcode = opcode_BFX;
    inst.bitfield.rd = $2.reg;
    inst.bitfield.ld = $2.expire;
    inst.bitfield.rm = $4.reg;
    inst.bitfield.lm = $4.expire;
    inst.bitfield.start = $6;
    inst.bitfield.end = $8;
    inst.bitfield.signxtnd = 0;
    
    $$ = ci(inst);
  }
  | BFXOP '.' 's' reg ',' ereg '<' INTEGER ',' INTEGER '>'
  {
    fmt_inst inst;
    
    inst.flat = 0;
    
    inst.bitfield.opcode = opcode_BFX;
    inst.bitfield.rd = $4.reg;
    inst.bitfield.ld = $4.expire;
    inst.bitfield.rm = $6.reg;
    inst.bitfield.lm = $6.expire;
    inst.bitfield.start = $8;
    inst.bitfield.end = $10;
    inst.bitfield.signxtnd = 1;
    
    $$ = ci(inst);
  }
;

splop: SPLOP reg ',' ereg ',' ereg ',' '#' INTEGER
  {
    fmt_inst inst;
    
    inst.flat = 0;
    
    inst.spl.opcode = opcode_SPL;
    inst.spl.shift = 0;
    inst.spl.rd = $2.reg;
    inst.spl.rm = $4.reg;
    inst.spl.lm = $4.expire;
    inst.spl.rn = $6.reg;
    inst.spl.ln = $6.expire;
    inst.spl.split = $9;
    
    $$ = ci(inst);
  }
  | SPLOP '.' 's' reg ',' ereg ',' ereg ',' '#' INTEGER
  {
    fmt_inst inst;
    
    inst.flat = 1;
    
    inst.spl.opcode = opcode_SPL;
    inst.spl.shift = 0;
    inst.spl.rd = $4.reg;
    inst.spl.rm = $6.reg;
    inst.spl.lm = $6.expire;
    inst.spl.rn = $8.reg;
    inst.spl.ln = $8.expire;
    inst.spl.split = $11;
    
    $$ = ci(inst);
  }
;

ffixop: FFIXOP '.' ftype reg ',' efreg
  {
    fmt_inst inst;

    inst.flat = 0;

    inst.alu.opcode = $1;
    
    inst.alu.rd = $4.reg;
    inst.alu.rm = 0;
    inst.alu.lm = 0;
    inst.alu.rn = $6.reg;
    inst.alu.ln = $6.expire;
    inst.alu.i = 0;
    
    inst.alu.prec_sat = fwidth($3);
    
    $$ = ci(inst);
  }
;

ffltop: FFLTOP '.' ftype freg ',' ereg
  {
    fmt_inst inst;

    inst.flat = 0;

    inst.alu.opcode = $1;
    
    inst.alu.prec_sat = fwidth($3);
    
    inst.alu.rd = $4.reg;
    inst.alu.rm = 0;
    inst.alu.lm = 0;
    inst.alu.rn = $6.reg;
    inst.alu.ln = $6.expire;
    inst.alu.i = 0;
    
    $$ = ci(inst);
  }
;

swiop: NOOPOP INTEGER
  {
    fmt_inst inst;
    
    inst.flat = 0;
    
    inst.noop.opcode = $1;
    inst.noop.number = $2;
    
    $$ = ci(inst);
  }
;

ldtop: LDTOP INTEGER
  {
    fmt_inst inst;
    
    inst.flat = 0;
    
    inst.noop.opcode = $1;
    inst.noop.number = $2;
    
    $$ = ci(inst);
  }
;

ref: direct
   | indirect
;

direct:	LABEL
  {
    reference ref;

    ref.direct = 1;
    ref.to.label = $1;

    $$ = ref;
  }
;

indirect: ereg
  {
    reference ref;

    ref.direct = 0;
    ref.to.reg.reg = $1.reg;
    ref.to.reg.expire = $1.expire;

    $$ = ref;
  }
;

tferop3:	TFEROP3 ereg ',' ref ',' ref
  {
    bundle* inst = newinst(1);
    uint5 num = $4.direct + $6.direct, fill=0;
    
    inst->inst[num].tfer.opcode = $1;
    inst->inst[num].tfer.rc = $2.reg;
    inst->inst[num].tfer.lc = $2.expire;

    if ((inst->inst[num].tfer.ix = $4.direct))
    {
      inst->inst[fill].noop.opcode = opcode_LDX;
      inst->inst[fill].noop.number = 0;
      inst->inst[num].tfer.rx = 0;
      adddelayedreloc(&inst->delayedrelocs, $4.to.label, R_CHAM_INDIRECT, 
        fill++);
    }
    else
    {
      inst->inst[num].tfer.rx = $4.to.reg.reg;
      inst->inst[num].tfer.lx = $4.to.reg.expire;
    }

    if ((inst->inst[num].tfer.iy = $6.direct))
    {
      inst->inst[fill].noop.opcode = opcode_LDY;
      inst->inst[fill].noop.number = 0;
      inst->inst[num].tfer.ry = 0;
      adddelayedreloc(&inst->delayedrelocs, $6.to.label, R_CHAM_INDIRECT,
        fill++);
    }
    else
    {
      inst->inst[num].tfer.ry = $6.to.reg.reg;
      inst->inst[num].tfer.ly = $6.to.reg.expire;
    }

    inst->len = fill+1;
    
    $$ = inst;
  }
;

tferop2:	TFEROP2 ref ',' ref
  {
    bundle* inst = newinst(1);
    uint5 num = $2.direct + $4.direct, fill=0;
    
    inst->inst[num].tfer.opcode = $1;
    inst->inst[num].tfer.rc = 0;
    inst->inst[num].tfer.lc = 0;
    
    if ((inst->inst[num].tfer.ix = $2.direct))
    {
      inst->inst[fill].noop.opcode = opcode_LDX;
      inst->inst[fill].noop.number = 0;
      inst->inst[num].tfer.rx = 0;
      adddelayedreloc(&inst->delayedrelocs, $2.to.label, R_CHAM_INDIRECT,
        fill++);
    }
    else
    {
      inst->inst[num].tfer.rx = $2.to.reg.reg;
      inst->inst[num].tfer.lx = $2.to.reg.expire;
    }

    if ((inst->inst[num].tfer.iy = $4.direct))
    {
      inst->inst[fill].noop.opcode = opcode_LDY;
      inst->inst[fill].noop.number = 0;
      inst->inst[num].tfer.ry = 0;
      adddelayedreloc(&inst->delayedrelocs, $4.to.label, R_CHAM_INDIRECT,
        fill++);
    }
    else
    {
      inst->inst[num].tfer.ry = $4.to.reg.reg;
      inst->inst[num].tfer.ly = $4.to.reg.expire;
    }

    inst->len = fill+1;
    
    $$ = inst;
  }
;

tferop1:	TFEROP1 ref
  {
    bundle* inst = newinst(1);
    uint5 num = $2.direct, fill=0;
    
    inst->inst[num].flat = 0;
    
    inst->inst[num].tfer.opcode = $1;
    inst->inst[num].tfer.rc = 0;
    inst->inst[num].tfer.lc = 0;
    
    if ((inst->inst[num].tfer.ix = $2.direct))
    {
      inst->inst[fill].noop.opcode = opcode_LDX;
      inst->inst[fill].noop.number = 0;
      inst->inst[num].tfer.rx = 0;
      adddelayedreloc(&inst->delayedrelocs, $2.to.label, R_CHAM_INDIRECT,
        fill++);
    }
    else
    {
      inst->inst[num].tfer.rx = $2.to.reg.reg;
      inst->inst[num].tfer.lx = $2.to.reg.expire;
    }
    
    inst->inst[num].tfer.iy = 0;
    inst->inst[num].tfer.ry = 0;
    inst->inst[num].tfer.ly = 0;

    inst->len = fill+1;
    
    $$ = inst;
  }
;

tferop0:	TFEROP0
  {
    fmt_inst inst;
    
    inst.flat = 0;
    
    inst.tfer.opcode = $1;
    inst.tfer.rc = 0;
    inst.tfer.ix = 0;
    inst.tfer.rx = 0;
    inst.tfer.lx = 0;
    inst.tfer.iy = 0;
    inst.tfer.ry = 0;
    inst.tfer.ly = 0;
    
    $$ = ci(inst);
  }
;

tferop:	tferop3  { chain = 0; $$ = $1; }
      | tferop2  { chain = 0; $$ = $1; }
      | tferop1  { chain = 0; $$ = $1; }
      | tferop0  { chain = 0; $$ = $1; }
;

reglist:  /* empty */ { $$ = 0; }
	| reg								{ $$ = jt_bset_new(64); jt_bset_SET($$, $1.reg); }
	| reglist ',' reg		{ jt_bset_SET($1, $3.reg); $$ = $1; }
;

directives:	/* empty */
  {
    $$ = 0;
  }
  | directives '%' directive
  {
    jt_list* newdirective;
    newdirective = jt_list_add(&$1);
    newdirective->data = $3;
    $$ = newdirective;
  }
;

directive:	livedirective
;

livedirective:
  LIVEONENTRY reglist
  {
    livedecl* liveness = jt_new(livedecl);
    liveness->type = LIVEONENTRY;
    liveness->bitmask = $2;
    fprintf(stderr, "Found liveonentry:\n");
    fprintf(stderr, "%.8x %.8x\n", $2->bits[1], $2->bits[0]);
    $$ = liveness;
  }
  | LIVEONEXIT reglist
  {
    livedecl* liveness = jt_new(livedecl);
    liveness->type = LIVEONEXIT;
    liveness->bitmask = $2;
    fprintf(stderr, "Found liveonexit:\n");
    fprintf(stderr, "%.8x %.8x\n", $2->bits[1], $2->bits[0]);
    $$ = liveness;
  }
;

%%

bundle* ci(fmt_inst inst)
{
  bundle* x = newinst(1);
  x->inst[0] = inst;
  return x;
}

bundle* newinst(uint5 length)
{
  int i;
  bundle* inst = jt_new(bundle);
  inst->len = length;
  inst->delayedrelocs = 0;
  for (i=0; i<MAXINSN; i++) inst->inst[i].flat = 0;
  return inst;
}

datum_info* newdata(void)
{
  datum_info* data = jt_new(datum_info);
  data->delayedrelocs = 0;
  return data;
}

sectionedint proparith(sectionedint a, char oper, sectionedint b)
{
  sectionedint res;

  if (a.symbolname && b.symbolname)
  {
    fprintf(stderr, "Name collision in immediate offset (%s, %s) at line %d\n", 
      a.symbolname, b.symbolname, lineno);
    exit(1);
  }
  
  switch (oper)
  {
    case '+':
    res.offset = a.offset + b.offset;
    res.multiplier = a.symbolname ? a.multiplier : b.multiplier;
    break;
    
    case '-':
    res.offset = a.offset - b.offset;
    res.multiplier = a.symbolname ? a.multiplier : -b.multiplier;
    break;
    
    default:
    fprintf(stderr, "Unknown operator %c at line %d\n", oper, lineno);
    exit(1);
  }

  
  res.symbolname = a.symbolname ? a.symbolname :
    b.symbolname ? b.symbolname : 0;

  return res;
}

void serialisedata(jt_buffer* buf, jt_clist* from, section* relsection)
{
  jt_clist* scan;

  for (scan=from->next; scan->data; scan=scan->next)
  {
    datum_info* item = (datum_info*)scan->data;
    sint5 start = -1;

    switch (item->type)
    {
      case type_BYTE:
      start = jt_buffer_append(buf, &item->content.value, 1);
      break;

      case type_HALF:
      {
        uint4 datum = item->content.value;
        datum = FIXHALF(datum);
        start = jt_buffer_append(buf, &datum, 2);
      }
      break;

      case type_WORD:
      {
        uint5 datum = item->content.value;
        datum = FIXWORD(datum);
        start = jt_buffer_append(buf, &datum, 4);
      }
      break;

      case type_STRING:
      {
        jt_buffer* strbuf = item->content.ptr;
        jt_buffer_append(buf, strbuf->buffer, strbuf->length);
      }
      break;

      case type_ALIGN:
      {
        /* improbably complex */
        uint5 addedlength = buf->length + item->content.value - 1;
        uint5 paddedlength = addedlength - (addedlength % item->content.value);
        uint5 pad = paddedlength - buf->length;
        if (pad > 0)
        {
          void* blank = calloc(pad, 1);
          jt_buffer_append(buf, blank, pad);
          free(blank);
        }
      }
      break;

      case type_RESERVE:
      {
        /* reserve will put null bytes in the output file unless it's the
           bss section (see serialisebss())
         */
        void* blank = calloc(item->content.value, 1);
        jt_buffer_append(buf, blank, item->content.value);
        free(blank);
      }
      break;
    }
    
    if (start != -1)
    {
      while (item->delayedrelocs)
      {
        delayedreloc* dr = item->delayedrelocs->data;
        
        newreloc(relsection, &elfimage->symbolmap,
          dr->name, dr->type, start+dr->offset*4);
        
        jt_delete(dr);
        jt_list_removehead(&item->delayedrelocs);
      }
    }
  }
}

void serialisebss(jt_buffer* buf, jt_clist* from, section* relsection)
{
  jt_clist* scan;
  uint5* length;
  
  if (buf->length == 0)
  {
    uint5 word = 0;
    jt_buffer_append(buf, &word, sizeof(word));
  }
  
  assert(buf->length==4);
  
  length = (uint5*)buf->buffer;
  
  /* bss consists of a single word containing the length required... */
  
  for (scan=from->next; scan->data; scan=scan->next)
  {
    datum_info* item = (datum_info*)scan->data;
    
    switch (item->type)
    {
      case type_BYTE:
      case type_HALF:
      case type_WORD:
      case type_STRING:
      {
        fprintf(stderr, "Bad data type for bss section at %d\n", lineno);
        exit(1);
      }
      break;
      
      case type_ALIGN:
      {
        uint5 align = item->content.value;
        uint5 addedlength = (*length) + item->content.value + align - 1;
        uint5 paddedlength = addedlength - (addedlength % align);
        uint5 pad = paddedlength - buf->length;
        (*length) += pad;
      }
      break;
      
      case type_RESERVE:
      {
        uint5 paddedres = (item->content.value + 3) & ~3;
        (*length) += paddedres;
    /*    fprintf(stderr, "Adding %d\n", item->content.value);*/
      }
      break;
    }
  }
}

static void encodeconstant(uint5 value, uint5* mantissa, uint5* bytenum)
{
  uint5 i;
  
  for (i=0; i<4; i++)
  {
    if ((value & (0xff << i*8)) == value) break;
  }

  if (i==4)
  {
    fprintf(stderr, "Bad constant around line %d: %x\n", lineno, value);
    exit(1);
  }

  *mantissa = value >> (i*8);
  *bytenum = i;
}

void endianfixbundle(bundle* inst)
{
  uint5 i;
  for (i=0; i<inst->len; i++)
  {
    inst->inst[i].flat = FIXWORD(inst->inst[i].flat);
  }
}

static char* genlabel(void)
{
  static int num = 1;
  char* lab = malloc(20);
  sprintf(lab, "_$internal$%d", num++);
  return lab;
}

int fwidth(char w)
{
  switch (w)
  {
    case 's': return 0;
    case 'd': return 1;
  }
  fprintf(stderr, "Unrecognized precision (%c) at line %d\n", w, lineno);
  exit(1);
}

int iwidth(char w)
{
  switch (w)
  {
    case 'b': return 0;
    case 'h': return 1;
    case 'w': return 2;
    case 't': return 3;
  }
  fprintf(stderr, "Unknown width (%c) at line %d\n", w, lineno);
  exit(1);
}

void yyerror(char* arg)
{
  fprintf(stdout, "Error in input (%s), line %d\n", arg, lineno);
}

int main(int argc, char** argv)
{
  FILE* outf, *inf;
  image* eimage;
  char* outfile = "out.cho", *infile = 0;
  int i;
  
  for (i=1; i<argc;)
  {
    if (strcmp(argv[i], "-o")==0)
    {
      ++i;
      outfile = argv[i++];
      /*fprintf(stderr, "Writing output to %s\n", outfile);*/
    }
    else if (strcmp(argv[i], "-v")==0)
    {
      fprintf(stderr, "DOP assembler v. 0.3\n");
      exit(0);
    }
    else
    {
      infile = argv[i++];
      /*fprintf(stderr, "Reading input from %s\n", infile);*/
    }
  }

  if (infile)
  {
    inf = freopen(infile, "r", stdin);
  }

  eimage = create_elf();

  outf = fopen(outfile, "w");
  write_elf(eimage, outf);
  fclose(outf);

  /*
  fprintf(stderr, "Linking program at %p\n", program);
  link(program);*/
  
/*  f = fopen("chasm.out", "w");
  serialise(f, program);
  fclose(f);*/
  
  return 0;
}
