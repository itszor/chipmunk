#ifndef PARSEDEFS
#define PARSEDEFS 1

#include <elf.h>

#include <defs.h>
#include <bset.h>
#include <buffer.h>
#include <map.h>
#include <list.h>
#include <clist.h>

#include "format.h"

typedef struct {
  uint5 reg;
  uint5 expire;
} reginfo;

typedef struct {
  reginfo r[3];
} reginfo3;

typedef struct {
  uint5 type;
  jt_bset* bitmask;
} livedecl;

typedef struct {
  uint5 type;
  union {
    uint5 value;
    void* ptr;
  } content;
  jt_list* delayedrelocs;
} datum_info;

typedef struct {
  char* name;
  uint5 value;
} pair;

typedef struct {
  uint5 type;
  uint5 reg;
  pair x, y;
} terminate_info;

typedef enum {
  type_BYTE,
  type_HALF,
  type_WORD,
  type_STRING,
  type_RESERVE,
  type_ALIGN
} datatype;

typedef struct {
  uint5* base;
  uint5 length;
  uint5 size;
} buffer;

typedef struct {
  uint5 direct;
  union {
    reginfo reg;
    char* label;
  } to;
} reference;

#define MAXINSN 3

typedef struct {
  fmt_inst inst[MAXINSN];
  uint5 len;
  jt_list* delayedrelocs;
} bundle;

typedef struct {
  uint5 value;
  uint5 reloctype;
  uint5 size;
  struct section* relsection;
} backpatch;

typedef struct {
  char* symbolname;
  uint5 offset;
  sint5 multiplier;
} sectionedint;

typedef struct {
  char* name;
  uint5 type;
  uint5 offset;
} delayedreloc;

#ifndef STREQ
#define STREQ(A,B) (!strcmp((A),(B)))
#endif

extern struct image* elfimage;

struct section;

/* function prototypes */

extern void fiddleopcode(fmt_inst* inst, uint5 expire);

extern void serialisedata(jt_buffer* buf, jt_clist* from,
  struct section* relsection);

extern void serialisebss(jt_buffer* buf, jt_clist* from,
  struct section* relsection);

extern sectionedint proparith(sectionedint a, char o, sectionedint b);

extern bundle* ci(fmt_inst inst);

extern bundle* newinst(uint5 length);

extern datum_info* newdata(void);

extern int fwidth(char);

extern int iwidth(char);

extern int yyparse(void);

#endif
