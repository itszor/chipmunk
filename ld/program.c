#include <string.h>

#include <config.h>
#include <fixendian.h>

#include "program.h"

#undef DEBUGENDIAN

program* create_program(void)
{
  program* prog = jt_new(program);
  char emptystr[] = "";
  Elf32_Sym emptysym;
  
  prog->globalsymbolmap = 0;
  
  prog->shstrtab.content = jt_buffer_new(16);
  
  jt_buffer_append(prog->shstrtab.content, emptystr, 1);
  
  prog->shstrtab.header.sh_name = jt_buffer_append(prog->shstrtab.content,
    ".shstrtab", strlen(".shstrtab")+1);
  
  createandname(&prog->text, &prog->shstrtab, ".text");
  createandname(&prog->name, &prog->shstrtab, ".name");
  createandname(&prog->index, &prog->shstrtab, ".index");
  createandname(&prog->data, &prog->shstrtab, ".data");
  createandname(&prog->bss, &prog->shstrtab, ".bss");
  createandname(&prog->rodata, &prog->shstrtab, ".rodata");
  createandname(&prog->strtab, &prog->shstrtab, ".strtab");
  createandname(&prog->symtab, &prog->shstrtab, ".symtab");
  
  jt_buffer_append(prog->strtab.content, emptystr, 1);
  
  emptysym.st_name = 0;
  emptysym.st_value = 0;
  emptysym.st_size = 0;
  emptysym.st_info = 0;
  emptysym.st_other = 0;
  emptysym.st_shndx = SHN_UNDEF;
  jt_buffer_append(prog->symtab.content, &emptysym, sizeof(Elf32_Sym));
    
  return prog;
}

uint5 pad4k(uint5 value)
{
  return (value+4095) & ~4095;
}

void make_headers(program* prog)
{
  uint5 length;
  mapentry* mentry;
  uint5 progptr;
  
  mentry = jt_map_find(prog->globalsymbolmap, "__start");
  if (!mentry)
  {
    fprintf(stderr, "No __start symbol, add crt0.cho to object list\n");
    exit(1);
  }
  
  if (mentry->index==-1u)
  {
    // index isn't right in global map
    mentry = jt_map_find(mentry->in_image->symbolmap, "__start");
  }

#ifdef DEBUG
  fprintf(stderr, "__start symbol at %d\n", mentry->index);
#endif

  memset(prog->ehdr.e_ident, 0, EI_NIDENT);
  prog->ehdr.e_ident[EI_MAG0] = ELFMAG0;
  prog->ehdr.e_ident[EI_MAG1] = ELFMAG1;
  prog->ehdr.e_ident[EI_MAG2] = ELFMAG2;
  prog->ehdr.e_ident[EI_MAG3] = ELFMAG3;
  prog->ehdr.e_ident[EI_CLASS] = ELFCLASS32;
  prog->ehdr.e_ident[EI_DATA] = ELFDATA2LSB;
  prog->ehdr.e_ident[EI_VERSION] = EV_CURRENT;
  
  prog->ehdr.e_type = FIXHALF(ET_EXEC);
  prog->ehdr.e_machine = FIXHALF(0x5423);
  prog->ehdr.e_version = FIXWORD(EV_CURRENT);
  prog->ehdr.e_entry = FIXWORD(mentry->index); /* Start symbol */
  prog->ehdr.e_phoff = FIXWORD(sizeof(prog->ehdr));
  { int word = sizeof(prog->ehdr) + PHN_COUNT*sizeof(Elf32_Phdr);
    word = FIXWORD(word);
    prog->ehdr.e_shoff = word; }
  prog->ehdr.e_flags = FIXWORD(0);
  prog->ehdr.e_ehsize = FIXHALF(sizeof(prog->ehdr));
  prog->ehdr.e_phentsize = FIXHALF(sizeof(Elf32_Phdr));
  prog->ehdr.e_phnum = FIXHALF(PHN_COUNT);
  prog->ehdr.e_shentsize = FIXHALF(sizeof(Elf32_Shdr));
  prog->ehdr.e_shnum = FIXHALF(PSHN_COUNT);
  prog->ehdr.e_shstrndx = FIXHALF(PSHN_SHSTRTAB);
  
  /* Section headers */
  length = sizeof(Elf32_Ehdr)
         + sizeof(Elf32_Phdr) * PHN_COUNT
         + sizeof(Elf32_Shdr) * PSHN_COUNT;
  
  fillshdr(&prog->null.header, SHT_NULL,      /* type */
                               0,             /* flags */
                               0x0,           /* addr */
                               0,             /* offset */
                               0,             /* size */
                               SHN_UNDEF,     /* link */
                               0,             /* info */
                               0,             /* addralign */
                               0);            /* entsize */

  fillshdr(&prog->shstrtab.header, SHT_STRTAB,      /* type */
                                   SHF_STRINGS,             /* flags */
                                   0x0,           /* addr */
                                   length,             /* offset */
                                   prog->shstrtab.content->length, /* size */
                                   SHN_UNDEF,     /* link */
                                   0,             /* info */
                                   0,             /* addralign */
                                   0);            /* entsize */
  
  length += pad(prog->shstrtab.content->length);

  fillshdr(&prog->text.header, SHT_PROGBITS,      /* type */
                                   SHF_EXECINSTR | SHF_ALLOC,    /* flags */
                                   0x1000,           /* addr */
                                   length,             /* offset */
                                   prog->text.content->length, /* size */
                                   SHN_UNDEF,     /* link */
                                   0,             /* info */
                                   4,             /* addralign */
                                   0);            /* entsize */
  
  length += pad(prog->text.content->length);

  fillshdr(&prog->name.header, SHT_PROGBITS,      /* type */
                                   SHF_ALLOC,    /* flags */
                                   0x1000,           /* addr */
                                   length,             /* offset */
                                   prog->name.content->length, /* size */
                                   SHN_UNDEF,     /* link */
                                   0,             /* info */
                                   4,             /* addralign */
                                   0);            /* entsize */
  
  length += pad(prog->name.content->length);

  fillshdr(&prog->index.header, SHT_PROGBITS,      /* type */
                                   SHF_EXECINSTR | SHF_ALLOC,    /* flags */
                                   0x0,           /* addr */
                                   length,             /* offset */
                                   prog->index.content->length, /* size */
                                   SHN_UNDEF,     /* link */
                                   0,             /* info */
                                   4,             /* addralign */
                                   0);            /* entsize */
  
  length += pad(prog->index.content->length);

  fillshdr(&prog->data.header, SHT_PROGBITS,      /* type */
                                   SHF_ALLOC | SHF_WRITE,    /* flags */
                                   0x2000,           /* addr */
                                   length,             /* offset */
                                   prog->data.content->length, /* size */
                                   SHN_UNDEF,     /* link */
                                   0,             /* info */
                                   4,             /* addralign */
                                   0);            /* entsize */
  
  length += pad(prog->data.content->length);

  fillshdr(&prog->rodata.header, SHT_PROGBITS,      /* type */
                                   SHF_ALLOC,    /* flags */
                                   0x3000,           /* addr */
                                   length,             /* offset */
                                   prog->rodata.content->length, /* size */
                                   SHN_UNDEF,     /* link */
                                   0,             /* info */
                                   4,             /* addralign */
                                   0);            /* entsize */
  
  length += pad(prog->rodata.content->length);

  fillshdr(&prog->bss.header, SHT_PROGBITS,      /* type */
                                   SHF_ALLOC | SHF_WRITE,    /* flags */
                                   0x4000,           /* addr */
                                   length,             /* offset */
                                   prog->bss.content->length, /* size */
                                   SHN_UNDEF,     /* link */
                                   0,             /* info */
                                   4,             /* addralign */
                                   0);            /* entsize */
  
  length += pad(prog->bss.content->length);

  fillshdr(&prog->strtab.header, SHT_STRTAB,      /* type */
                                   SHF_STRINGS,    /* flags */
                                   0x0,           /* addr */
                                   length,             /* offset */
                                   prog->strtab.content->length, /* size */
                                   SHN_UNDEF,     /* link */
                                   0,             /* info */
                                   0,             /* addralign */
                                   0);            /* entsize */
  
  length += pad(prog->strtab.content->length);

  fillshdr(&prog->symtab.header, SHT_SYMTAB,      /* type */
                                   0,    /* flags */
                                   0x0,           /* addr */
                                   length,             /* offset */
                                   prog->symtab.content->length, /* size */
                                   SHN_UNDEF,     /* link */
                                   0,             /* info */
                                   4,             /* addralign */
                                   sizeof(Elf32_Sym));      /* entsize */

/*  length += pad(prog->symtab.content->length);*/

  /* Program headers */
  fillphdr(&prog->psindex.header, PT_LOAD,    /* type */
                          FIXWORD(prog->index.header.sh_offset), /* offset */
                                  IDXBASE,    /* vaddr */
                                  0,          /* paddr */
                                  prog->index.content->length, /* filesz */
                                  prog->index.content->length, /* memsz */
                                  0,          /* flags */
                                  4096);      /* align */

  progptr = MAINBASE;

  fillphdr(&prog->pstext.header, PT_LOAD,    /* type */
                          FIXWORD(prog->text.header.sh_offset), /* offset */
                                  progptr,    /* vaddr */
                                  0,          /* paddr */
                                  prog->text.content->length, /* filesz */
                                  prog->text.content->length, /* memsz */
                                  0,          /* flags */
                                  4096);      /* align */
  
  progptr += pad4k(prog->text.content->length);

  fillphdr(&prog->psdata.header, PT_LOAD,    /* type */
                          FIXWORD(prog->data.header.sh_offset), /* offset */
                                  progptr,    /* vaddr */
                                  0,          /* paddr */
                                  prog->data.content->length, /* filesz */
                                  prog->data.content->length, /* memsz */
                                  0,          /* flags */
                                  4096);      /* align */
  
  progptr += pad4k(prog->data.content->length);

  fillphdr(&prog->psrodata.header, PT_LOAD,    /* type */
                          FIXWORD(prog->rodata.header.sh_offset), /* offset */
                                  progptr,    /* vaddr */
                                  0,          /* paddr */
                                  prog->rodata.content->length, /* filesz */
                                  prog->rodata.content->length, /* memsz */
                                  0,          /* flags */
                                  4096);      /* align */

  progptr += pad4k(prog->rodata.content->length);

  fillphdr(&prog->psname.header, PT_LOAD,    /* type */
                          FIXWORD(prog->name.header.sh_offset), /* offset */
                                  progptr,    /* vaddr */
                                  0,          /* paddr */
                                  prog->name.content->length, /* filesz */
                                  prog->name.content->length, /* memsz */
                                  0,          /* flags */
                                  4096);      /* align */

  progptr += pad4k(prog->name.content->length);

  fillphdr(&prog->psbss.header, PT_LOAD,    /* type */
                          FIXWORD(prog->bss.header.sh_offset), /* offset */
                                  progptr,    /* vaddr */
                                  0,          /* paddr */
                                  prog->bss.content->length, /* filesz */
                          FIXWORD(*((uint5*)prog->bss.content->buffer)),
                                                       /* memsz */
                                  0,          /* flags */
                                  4096);      /* align */
}

#ifdef DEBUGENDIAN

static void* allocateandread(FILE* f, int size)
{
  void* blk = jt_newarray(char, size);
  fread(blk, size, 1, f);
  return blk;
}

static void compare(char* blkname, void* orig, void* repl, int length)
{
  int i;
  for (i=0; i<length; i++)
  {
    int o = ((uint3*)orig)[i], r = ((uint3*)repl)[i];
    int ow = ((uint5*)orig)[i/4], rw = ((uint5*)repl)[i/4];
    if (o != r)
    {
      fprintf(stderr, "Section %s differs at %d\n", blkname, i);
      fprintf(stderr, "orig: %d (%.8x)  repl: %d (%.8x)\n", o, ow, r, rw);
    }
  }
}

#endif


void write_elf(program* prog, FILE* f)
{
#ifdef DEBUG
  uint5 told;
#endif
#ifdef DEBUGENDIAN
  FILE* cf = fopen("panic-doptest", "r");
  void* cpy;
#endif

#ifdef DEBUGENDIAN

#define fwrite(B,S,N,F) \
  fwrite((B),(S),(N),(F)); \
  cpy = allocateandread(cf, (S)); \
  compare(#B, (B), cpy, (S)); \
  jt_delete(cpy);

#endif

  fixsymtab(prog->symtab.content->buffer,
    prog->symtab.content->length / sizeof(Elf32_Sym));

  // write ELF header
  fwrite(&prog->ehdr, sizeof(Elf32_Ehdr), 1, f);
  
  // write program headers
  fwrite(&prog->psindex.header, sizeof(Elf32_Phdr), 1, f);
  fwrite(&prog->pstext.header, sizeof(Elf32_Phdr), 1, f);
  fwrite(&prog->psdata.header, sizeof(Elf32_Phdr), 1, f);
  fwrite(&prog->psrodata.header, sizeof(Elf32_Phdr), 1, f);
  fwrite(&prog->psname.header, sizeof(Elf32_Phdr), 1, f);
  fwrite(&prog->psbss.header, sizeof(Elf32_Phdr), 1, f);
  
  // write section headers
  fwrite(&prog->null.header, sizeof(Elf32_Shdr), 1, f);
  fwrite(&prog->shstrtab.header, sizeof(Elf32_Shdr), 1, f);
  fwrite(&prog->text.header, sizeof(Elf32_Shdr), 1, f);
  fwrite(&prog->name.header, sizeof(Elf32_Shdr), 1, f);
  fwrite(&prog->index.header, sizeof(Elf32_Shdr), 1, f);
  fwrite(&prog->data.header, sizeof(Elf32_Shdr), 1, f);
  fwrite(&prog->bss.header, sizeof(Elf32_Shdr), 1, f);
  fwrite(&prog->rodata.header, sizeof(Elf32_Shdr), 1, f);
  fwrite(&prog->strtab.header, sizeof(Elf32_Shdr), 1, f);
  fwrite(&prog->symtab.header, sizeof(Elf32_Shdr), 1, f);
  
  // write program/section data (overlaps, is that right?)
  fwrite(prog->shstrtab.content->buffer, pad(prog->shstrtab.content->length),
         1, f);
  #ifdef DEBUG
  told = ftell(f);
  fprintf(stderr, "I was told %d for beginning of text", told);
  #endif
  fwrite(prog->text.content->buffer, pad(prog->text.content->length), 1, f);
  fwrite(prog->name.content->buffer, pad(prog->name.content->length), 1, f);
  #ifdef DEBUG
  told = ftell(f);
  fprintf(stderr, " and I was told %d for index\n", told);
  #endif
  fwrite(prog->index.content->buffer, pad(prog->index.content->length), 1, f);
  fwrite(prog->data.content->buffer, pad(prog->data.content->length), 1, f);
  fwrite(prog->rodata.content->buffer, pad(prog->rodata.content->length), 1, f);
  fwrite(prog->bss.content->buffer, pad(prog->bss.content->length), 1, f);
  fwrite(prog->strtab.content->buffer, pad(prog->strtab.content->length), 1, f);
  fwrite(prog->symtab.content->buffer, pad(prog->symtab.content->length), 1, f);

#ifdef DEBUGENDIAN
  fclose(cf);
#endif
}
