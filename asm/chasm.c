#include <stdio.h>
#include <string.h>

#include <config.h>
#include <fixendian.h>
#include <buffer.h>

#include "elfutil.h"
#include "chasm.h"

#undef DEBUGENDIAN

image* create_elf(void)
{
  image* img = jt_new(image);
  uint5 length;
  char emptystr[] = "";
  Elf32_Sym emptysym;
  
  /* create & name sections */
  img->shstrtab.content = jt_buffer_new(16);
  
  /* empty string */
  jt_buffer_append(img->shstrtab.content, emptystr, 1);
  
  img->shstrtab.header.sh_name = jt_buffer_append(img->shstrtab.content,
    ".shstrtab", strlen(".shstrtab")+1);
  
  createandname(&img->bss, &img->shstrtab, ".bss");
  createandname(&img->data, &img->shstrtab, ".data");
  createandname(&img->rodata, &img->shstrtab, ".rodata");
  createandname(&img->comment, &img->shstrtab, ".comment");
  createandname(&img->text, &img->shstrtab, ".text");
  createandname(&img->strtab, &img->shstrtab, ".strtab");
  createandname(&img->symtab, &img->shstrtab, ".symtab");
  createandname(&img->reltext, &img->shstrtab, ".rel.text");
  createandname(&img->reldata, &img->shstrtab, ".rel.data");
  createandname(&img->relrodata, &img->shstrtab, ".rel.rodata");
  createandname(&img->relbss, &img->shstrtab, ".rel.bssdata");
  
  // null entries at start of strtab and symtab
  jt_buffer_append(img->strtab.content, emptystr, 1);
  emptysym.st_name = 0;
  emptysym.st_value = 0;
  emptysym.st_size = 0;
  emptysym.st_info = 0;
  emptysym.st_other = 0;
  emptysym.st_shndx = SHN_UNDEF;
  jt_buffer_append(img->symtab.content, &emptysym, sizeof(emptysym));
  
  img->symbolmap = 0;
  
  memset(img->ehdr.e_ident, 0, EI_NIDENT);
  img->ehdr.e_ident[EI_MAG0] = ELFMAG0;
  img->ehdr.e_ident[EI_MAG1] = ELFMAG1;
  img->ehdr.e_ident[EI_MAG2] = ELFMAG2;
  img->ehdr.e_ident[EI_MAG3] = ELFMAG3;
  img->ehdr.e_ident[EI_CLASS] = ELFCLASS32;
  img->ehdr.e_ident[EI_DATA] = ELFDATA2LSB;
  img->ehdr.e_ident[EI_VERSION] = EV_CURRENT;

  img->ehdr.e_type = FIXHALF(ET_REL);
  img->ehdr.e_machine = FIXHALF(0x5423);
  img->ehdr.e_version = FIXWORD(EV_CURRENT);
  img->ehdr.e_entry = FIXWORD(0x0);   // virtual entry address
  img->ehdr.e_phoff = FIXWORD(0x0);   // no program header
  img->ehdr.e_shoff = FIXWORD(sizeof(img->ehdr));
  img->ehdr.e_flags = FIXWORD(0x0);
  img->ehdr.e_ehsize = FIXHALF(sizeof(img->ehdr));
  img->ehdr.e_phentsize = FIXHALF(sizeof(Elf32_Phdr));
  img->ehdr.e_phnum = FIXHALF(0);
  img->ehdr.e_shentsize = FIXHALF(sizeof(Elf32_Shdr));
  img->ehdr.e_shnum = FIXHALF(SHN_COUNT);
  img->ehdr.e_shstrndx = FIXHALF(SHN_SHSTRTAB);  // shstrtab will be at index 0?
  
  length = sizeof(Elf32_Ehdr) + sizeof(Elf32_Shdr) * SHN_COUNT;
  
  fillshdr(&img->null.header, SHT_NULL,  /* type */
                              0,         /* flags */
                              0x0,       /* addr */
                              0,         /* offset */
                              0,         /* size */
                              SHN_UNDEF, /* link */
                              0,         /* info */
                              0,         /* addralign */
                              0);        /* entsize */

  fillshdr(&img->shstrtab.header, SHT_STRTAB,                    /* type */
                                  0,                             /* flags */
                                  0x0,                           /* addr */
                                  length,                        /* offset */
                                  img->shstrtab.content->length, /* size */
                                  SHN_UNDEF,                     /* link */
                                  0,                             /* info */
                                  1,                             /* addralign */
                                  0);                            /* entsize */
  
  length += pad(img->shstrtab.content->length);
  
  elfimage = img;
  
  if (yyparse())
  {
    fprintf(stderr, "Parse errors, no output produced\n");
    exit(1);
  }
  
  jt_map_forall(img->symbolmap, undefinedrefs, img);
  
  fillshdr(&img->text.header, SHT_PROGBITS,                  /* type */
                              SHF_EXECINSTR | SHF_ALLOC,     /* flags */
                              0x1000,                        /* addr */
                              length,                        /* offset */
                              img->text.content->length,     /* size */
                              SHN_UNDEF,                     /* link */
                              0,                             /* info */
                              4,                             /* addralign */
                              0);                            /* entsize */
  
  length += pad(img->text.content->length);
    
  fillshdr(&img->data.header, SHT_PROGBITS,                  /* type */
                              SHF_ALLOC | SHF_WRITE,         /* flags */
                              0x2000,                        /* addr */
                              length,                        /* offset */
                              img->data.content->length,     /* size */
                              SHN_UNDEF,                     /* link */
                              0,                             /* info */
                              4,                             /* addralign */
                              0);                            /* entsize */
  
  length += pad(img->data.content->length);

  fillshdr(&img->rodata.header, SHT_PROGBITS,                  /* type */
                                SHF_ALLOC,                     /* flags */
                                0x3000,                        /* addr */
                                length,                        /* offset */
                                img->rodata.content->length,   /* size */
                                SHN_UNDEF,                     /* link */
                                0,                             /* info */
                                4,                             /* addralign */
                                0);                            /* entsize */
  
  length += pad(img->rodata.content->length);

  fillshdr(&img->bss.header, SHT_PROGBITS,                  /* type */
                             SHF_ALLOC | SHF_WRITE,         /* flags */
                             0x4000,                        /* addr */
                             length,                        /* offset */
                             img->bss.content->length,      /* size */
                             SHN_UNDEF,                     /* link */
                             0,                             /* info */
                             4,                             /* addralign */
                             0);                            /* entsize */
 
  #ifdef DEBUG 
  fprintf(stderr, "Awooga writing %d for bss length\n", 
    img->bss.content->length);
  #endif
  
  length += pad(img->bss.content->length);
  
  fillshdr(&img->symtab.header, SHT_SYMTAB,                    /* type */
                                0,                             /* flags */
                                0x0,                           /* addr */
                                length,                        /* offset */
                                img->symtab.content->length,   /* size */
                                SHN_STRTAB,                    /* link */
                                0,                             /* info */
                                4,                             /* addralign */
                                sizeof(Elf32_Sym));            /* entsize */

  length += pad(img->symtab.content->length);
  
  fillshdr(&img->strtab.header, SHT_STRTAB,                    /* type */
                                0,                             /* flags */
                                0x0,                           /* addr */
                                length,                        /* offset */
                                img->strtab.content->length,   /* size */
                                SHN_UNDEF,                     /* link */
                                0,                             /* info */
                                1,                             /* addralign */
                                0);                            /* entsize */
  
  length += pad(img->strtab.content->length);
  
  fillshdr(&img->reltext.header, SHT_REL,                       /* type */
                                 0,                             /* flags */
                                 0x0,                           /* addr */
                                 length,                        /* offset */
                                 img->reltext.content->length,  /* size */
                                 SHN_SYMTAB,                    /* link */
                                 0,                             /* info */
                                 4,                             /* addralign */
                                 sizeof(Elf32_Rel));            /* entsize */

  length += pad(img->reltext.content->length);
  
  fillshdr(&img->reldata.header, SHT_REL,                       /* type */
                                 0,                             /* flags */
                                 0x0,                           /* addr */
                                 length,                        /* offset */
                                 img->reldata.content->length,  /* size */
                                 SHN_SYMTAB,                    /* link */
                                 0,                             /* info */
                                 4,                             /* addralign */
                                 sizeof(Elf32_Rel));            /* entsize */

  length += pad(img->reldata.content->length);
  
  fillshdr(&img->relrodata.header, SHT_REL,                      /* type */
                                   0,                            /* flags */
                                   0x0,                          /* addr */
                                   length,                       /* offset */
                                   img->relrodata.content->length,/* size */
                                   SHN_SYMTAB,                   /* link */
                                   0,                            /* info */
                                   4,                            /* addralign */
                                   sizeof(Elf32_Rel));           /* entsize */

  length += pad(img->relrodata.content->length);
  
  fillshdr(&img->relbss.header, SHT_REL,                       /* type */
                                0,                             /* flags */
                                0x0,                           /* addr */
                                length,                        /* offset */
                                img->relbss.content->length,   /* size */
                                SHN_SYMTAB,                    /* link */
                                0,                             /* info */
                                4,                             /* addralign */
                                sizeof(Elf32_Rel));            /* entsize */
  
  return img;
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

void write_elf(image* img, FILE* f)
{
#ifdef DEBUGENDIAN
  FILE* cf = fopen("panic-cho", "r");
  void* cpy;
#endif

  /* We keep symtab host-endian til now because we sometimes read it after 
   * writing it
   */
  fixsymtab(img->symtab.content->buffer, 
    img->symtab.content->length/sizeof(Elf32_Sym));

  if (img->bss.content->length == 4)
  {
    uint5 word = *((uint5*)img->bss.content->buffer);
    word = FIXWORD(word);
    *((uint5*)img->bss.content->buffer) = word;
  }

#ifdef DEBUGENDIAN

#define fwrite(B,S,N,F) \
  fwrite((B),(S),(N),(F)); \
  cpy = allocateandread(cf, (S)); \
  compare(#B, (B), cpy, (S)); \
  jt_delete(cpy);

#endif

  // write ELF header
  fwrite(&img->ehdr, sizeof(Elf32_Ehdr), 1, f);

  // write section headers
  fwrite(&img->null.header, sizeof(Elf32_Shdr), 1, f);
  fwrite(&img->shstrtab.header, sizeof(Elf32_Shdr), 1, f);
  fwrite(&img->text.header, sizeof(Elf32_Shdr), 1, f);
  fwrite(&img->data.header, sizeof(Elf32_Shdr), 1, f);
  fwrite(&img->rodata.header, sizeof(Elf32_Shdr), 1, f);
  fwrite(&img->bss.header, sizeof(Elf32_Shdr), 1, f);
  fwrite(&img->symtab.header, sizeof(Elf32_Shdr), 1, f);
  fwrite(&img->strtab.header, sizeof(Elf32_Shdr), 1, f);
  fwrite(&img->reltext.header, sizeof(Elf32_Shdr), 1, f);
  fwrite(&img->reldata.header, sizeof(Elf32_Shdr), 1, f);
  fwrite(&img->relrodata.header, sizeof(Elf32_Shdr), 1, f);
  fwrite(&img->relbss.header, sizeof(Elf32_Shdr), 1, f);
  
  // write section data
  fwrite(img->shstrtab.content->buffer, pad(img->shstrtab.content->length), 1, f);
  fwrite(img->text.content->buffer, pad(img->text.content->length), 1, f);
  fwrite(img->data.content->buffer, pad(img->data.content->length), 1, f);
  fwrite(img->rodata.content->buffer, pad(img->rodata.content->length), 1, f);
  fwrite(img->bss.content->buffer, pad(img->bss.content->length), 1, f);
  fwrite(img->symtab.content->buffer, pad(img->symtab.content->length), 1, f);
  fwrite(img->strtab.content->buffer, pad(img->strtab.content->length), 1, f);
  fwrite(img->reltext.content->buffer, pad(img->reltext.content->length), 1, f);
  fwrite(img->reldata.content->buffer, pad(img->reldata.content->length), 1, f);
  fwrite(img->relrodata.content->buffer, pad(img->relrodata.content->length), 1, f);
  fwrite(img->relbss.content->buffer, pad(img->relbss.content->length), 1, f);

#ifdef DEBUGENDIAN
  fclose(cf);
#endif
}
