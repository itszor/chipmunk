#ifndef PROGRAM_H
#define PROGRAM_H 1

#include <stdio.h>
#include <elf.h>

#include <buffer.h>

#include "elfutil.h"

typedef struct {
  jt_buffer* buf;
  Elf32_Phdr header;
} psection;

typedef struct {
  Elf32_Ehdr ehdr;
  psection pstext;
  psection psindex;
  psection psdata;
  psection psrodata;
  psection psname;
  psection psbss;
  section null;
  section shstrtab;
  section text;
  section name;
  section index;
  section data;
  section bss;
  section rodata;
  section strtab;
  section symtab;
  jt_map* globalsymbolmap;
  struct {
    uint5 data;
    uint5 rodata;
    uint5 name;
    uint5 bss;
    uint5 heap;
  } start;
} program;

typedef enum {
  PHN_INDEX,
  PHN_TEXT,
  PHN_DATA,
  PHN_RODATA,
  PHN_NAME,
  PHN_BSS,
  PHN_COUNT
} program_headers;

typedef enum {
  PSHN_NULL,
  PSHN_SHSTRTAB,
  PSHN_TEXT,
  PSHN_NAME,
  PSHN_INDEX,
  PSHN_DATA,
  PSHN_RODATA,
  PSHN_BSS,
  PSHN_STRTAB,
  PSHN_SYMTAB,
  PSHN_COUNT
} program_sections;

#define MAINBASE 0x10000000
#define IDXBASE 0x0
/*
#define STATICDATABASE 0x80000000
*/

extern uint5 pad4k(uint5);

extern program* create_program(void);

extern void make_headers(program* prog);

extern void write_elf(program* prog, FILE* f);

#endif
