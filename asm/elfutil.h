#ifndef ELFUTIL_H
#define ELFUTIL_H 1

#include <buffer.h>

#include "parsedefs.h"
#include "format.h"

struct section {
  jt_buffer* content;
  Elf32_Shdr header;
};

typedef struct section section;

struct image {
  Elf32_Ehdr ehdr;
  section null;
  section bss;
  section data;
  section rodata;
  section comment;
  section shstrtab;
  section text;
  section strtab;
  section symtab;
  section reltext;
  section reldata;
  section relrodata;
  section relbss;
  jt_map* symbolmap;
  char used;
  char library;
};

#define IMAGE_UNUSED 0
#define IMAGE_USED 1

typedef enum {
  SHN_SHSTRTAB = 1,
  SHN_TEXT,
  SHN_DATA,
  SHN_RODATA,
  SHN_BSS,
  SHN_SYMTAB,
  SHN_STRTAB,
  SHN_RELTEXT,
  SHN_RELDATA,
  SHN_RELRODATA,
  SHN_RELBSS,
  SHN_COUNT
} section_numbers;

typedef struct image image;

typedef struct {
  char* name;
  uint5 binding;
  uint5 symtabentry;
  image* in_image;
  uint5 index;
  jt_list* backpatches;
} mapentry;

typedef struct {
  uint5 offset;
  uint5 parent;
  uint5 meta;
  uint5 count;
} indexentry;

#define R_CHAM_NONE 0
#define R_CHAM_INDIRECT 1
#define R_CHAM_ADRL 2
#define R_CHAM_ADRH 3
#define R_CHAM_DCV 4
#define R_CHAM_NUM 5

extern void fillshdr(Elf32_Shdr* shdr,
  uint5, uint5, uint5, uint5, uint5, uint5, uint5, uint5, uint5);

extern void fixshdr(Elf32_Shdr* shdr);

extern void fillphdr(Elf32_Phdr* phdr,
  uint5, uint5, uint5, uint5, uint5, uint5, uint5, uint5);

extern void fixsymtab(Elf32_Sym*, uint5 num);

extern void fixrel(Elf32_Rel* rel);

extern void newreloc(section* rel, jt_map** symbolmap, char* name,
  uint5 type, uint5 offset);

extern void createandname(section* sec, section* names, char* name);

extern mapentry* newmapentry(char* name, uint5 binding, uint5 stentry);

extern uint5 newsymbol(section* strtab, section* symtab, char* name, Elf32_Addr
  value, Elf32_Word size, uint5 binding, uint5 type, uint5 section);

extern void processsymbol(image* img, char* name, Elf32_Addr
  value, Elf32_Word size, uint5 type, uint5 section);
  
extern void rebindsymbol(jt_map** symbolmap, char* name, section* symtab,
  uint5 binding);

extern uint5 pad(uint5);

extern void adddelayedreloc(jt_list** addto, char* name, uint5 type,
  uint5 offset);

extern void undefinedrefs(jt_map* node, void* data);

#endif
