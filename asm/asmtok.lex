/* Chameleon assembler tokenizer */

%{
#include <string.h>
#include <ctype.h>

#include <defs.h>
#include <list.h>
#include <clist.h>
#include <map.h>
#include <buffer.h>

#include "format.h"
#include "block.h"
#include "parsedefs.h"
#include "asmparse.tab.h"

int lineno = 0;

static jt_buffer* process_string(char* in);

%}

DIGIT	[0-9]
ID	_[_a-zA-Z0-9.$\*]+
HEXDIGIT [0-9a-fA-F]

%%

{DIGIT}+	     sscanf(yytext, "%u", &yylval.val); return INTEGER;

0x{HEXDIGIT}+  sscanf(&yytext[2], "%x", &yylval.val); return INTEGER;

"#"|"%"|"!"		return yytext[0];

".c"/[^s]		return CARRYPOSTFIX;

"."|","		return yytext[0];

"*"|"-"		return yytext[0];

"+"       return yytext[0];

"("|")"   return yytext[0];

"@"       return yytext[0];

"{"|"}"		return yytext[0];

"["|"]"		return yytext[0];

"<"|">"		return yytext[0];

":"				return yytext[0];

[idsablfewhtv]			return yytext[0];

r/{DIGIT}+	return REGPREFIX;

f/{DIGIT}+	return FREGPREFIX;

"mov"		yylval.val = opcode_MOV; return BINOP;

"not"		yylval.val = opcode_NOT; return BINOP;

"lsl"		yylval.val = opcode_LSL; return TRIOP;

"lsr"		yylval.val = opcode_LSR; return TRIOP;

"asr"		yylval.val = opcode_ASR; return TRIOP;

"ror"		yylval.val = opcode_ROR; return TRIOP;

"and"		yylval.val = opcode_AND; return TRIOP;

"ior"		yylval.val = opcode_IOR; return TRIOP;

"eor"		yylval.val = opcode_EOR; return TRIOP;

"bic"		yylval.val = opcode_BIC; return TRIOP;

"add"		yylval.val = opcode_ADD; return TRIOP;

"sub"		yylval.val = opcode_SUB; return TRIOP;

"rsb"		yylval.val = opcode_RSB; return TRIOP;

"mul"		yylval.val = opcode_MUL; return TRIOP;

"div"		yylval.val = opcode_DIV; return TRIOP;

"udiv"          yylval.val = opcode_UDIV; return TRIOP;

"mod"		yylval.val = opcode_MOD; return TRIOP;

"umod"          yylval.val = opcode_UMOD; return TRIOP;

"mlh"           yylval.val = opcode_MLH; return TRIOP;

"umlh"          yylval.val = opcode_UMLH; return TRIOP;

"ldr"		yylval.val = opcode_LDR; return SMEMOP;

"str"		yylval.val = opcode_STR; return SMEMOP;

"ldm"		yylval.val = opcode_LDM; return MMEMOP;

"stm"		yylval.val = opcode_STM; return MMEMOP;

"cmp"   yylval.val = opcode_CMP; return CMPOP;

"ucmp"	yylval.val = opcode_UCMP; return CMPOP;

"eq"    yylval.val = cond_EQ; return CONDITION;

"ne"		yylval.val = cond_NE; return CONDITION;

"ge"    yylval.val = cond_GE; return CONDITION;

"gt"    yylval.val = cond_GT; return CONDITION;

"le"    yylval.val = cond_LE; return CONDITION;

"lt"    yylval.val = cond_LT; return CONDITION;

"andl"  yylval.val = cond_AND; return CONDITION;

"eorl"  yylval.val = cond_EOR; return CONDITION;

"nandl"  yylval.val = cond_NAND; return CONDITION;

"neorl"   yylval.val = cond_NEOR; return CONDITION;

"swi"   yylval.val = opcode_SWI; return NOOPOP;

"mvc"   yylval.val = opcode_MVC; return MVCOP;

"bfx"   yylval.val = opcode_BFX; return BFXOP;

"movf"	yylval.val = opcode_MOVF; return FBINOP;

"negf"	yylval.val = opcode_NEGF; return FBINOP;

"absf"	yylval.val = opcode_ABSF; return FBINOP;

"sqrf"	yylval.val = opcode_SQRF; return FBINOP;

"fltf"	yylval.val = opcode_FLTF; return FFLTOP;

"fixf"	yylval.val = opcode_FIXF; return FFIXOP;

"addf"	yylval.val = opcode_ADDF; return FTRIOP;

"subf"	yylval.val = opcode_SUBF; return FTRIOP;

"mulf"	yylval.val = opcode_MULF; return FTRIOP;

"divf"	yylval.val = opcode_DIVF; return FTRIOP;

"cmpf"	yylval.val = opcode_CMPF; return FCMPOP;

"ldf"   yylval.val = opcode_LDF; return FMEMOP;

"stf"	yylval.val = opcode_STF; return FMEMOP;

"ldmf"	yylval.val = opcode_LDMF; return FMMEMOP;

"stmf"	yylval.val = opcode_STMF; return FMMEMOP;

"trap"	yylval.val = opcode_TRAPZ; return TRAPOP;

"cbr"	yylval.val = opcode_CBR; return TFEROP3;

"call"	yylval.val = opcode_CALL; return TFEROP2;

"jump"	yylval.val = opcode_JUMP; return TFEROP1;

"ret"	yylval.val = opcode_RET; return TFEROP0;

"spl"   yylval.val = opcode_SPL; return SPLOP;

"ldx"   yylval.val = opcode_LDX; return LDTOP;

"ldy"   yylval.val = opcode_LDY; return LDTOP;

"ldz"   yylval.val = opcode_LDZ; return LDTOP;

on				return ON;

"else"		return ELSE;

liveonentry	return LIVEONENTRY;

liveonexit	return LIVEONEXIT;

expire|x|"~"		return EXPIRE;

data		return DATA;

rodata  return RODATA;

bss     return BSS;

text    return TEXT;

dcb		return DCB;

dch		return DCH;

dcw		return DCW;

dcs   return DCS;

reserve		return RES;

align   return ALIGN;

global	return GLOBAL;

weak		return WEAK;

"//".*$		/* ignore single-line comment */

";".*$		/* asm comment style */

{ID}	yylval.name = strdup(yytext); return LABEL;

\".*\"	yylval.name_buf = process_string(yytext); return STRING;

[ \t]+	/* ignore whitespace */

\n	lineno++;

.		printf("Unrecognized character: %s (%d)\n", yytext, yytext[0]);

%%

#ifndef yywrap
int yywrap(void)        /* do this avoid to do -lfl */
{
  return 1;
}
#endif

/* Process some escape sequences */
static jt_buffer* process_string(char* in)
{
  int len = strlen(in);
  int i;
  jt_buffer* outbuf = jt_buffer_new(16);
  char* out;
  
  for (i=1; i<len-1;)
  {
    if (in[i] == '\\')
    {
      switch (in[++i])
      {
        case 'n':
        jt_buffer_append(outbuf, "\n", 1);
        i++;
        break;
        
        case 't':
        jt_buffer_append(outbuf, "\n", 1);
        i++;
        break;
        
        case 'r':
        jt_buffer_append(outbuf, "\r", 1);
        i++;
        break;
        
        case '\\':
        jt_buffer_append(outbuf, "\\", 1);
        i++;
        break;
        
        case '"':
        jt_buffer_append(outbuf, "\"", 1);
        i++;
        break;
        
        case '&':
        {
          int accum = 0;
          int letter;
          int count;
          for (count=0; count<2; count++)
          {
            letter = in[++i];
            
            if (letter>='0' && letter<='9')
              accum = 16*accum + letter - '0';
            else
              accum = 16*accum + tolower(letter) - 'a' + 10;
          }
          jt_buffer_append(outbuf, &accum, 1);
          i++;
        }
        break;
        
        default:
        fprintf(stderr, "Unrecognized \\ escape at %d\n", lineno);
        exit(1);
      }
    }
    else
    {
      jt_buffer_append(outbuf, &in[i++], 1);
    }
  }
  jt_buffer_append(outbuf, "\0", 1);

  return outbuf;
}
