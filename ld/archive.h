#ifndef ARCHIVE_H
#define ARCHIVE_H 1

#include <defs.h>

#include "program.h"

typedef struct {
  uint5* loc;
  char* name;
  uint5* nameidx;
  uint5 count;
} archive_symtab;

extern void loadarchive(FILE* ar, program* prog, jt_clist* objects);

#endif
