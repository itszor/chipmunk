#ifndef OBJECT_H
#define OBJECT_H 1

#include <defs.h>

extern uint5 loadsection(section* src, FILE* f);

extern image* loadimage(FILE* f, int inlibrary);

extern char* symbolname(Elf32_Sym* sym, section* strtab);

extern void readsymbols(section* symtab, section* strtab, jt_map** symbolmap);

extern Elf32_Sym* getsymbol(image* from, jt_map* map, char* name);

extern void findglobals(image* in, jt_map** global);

#endif
