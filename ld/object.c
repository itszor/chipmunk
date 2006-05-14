#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <defs.h>
#include <cnew.h>
#include <clist.h>
#include <buffer.h>

#include "elfutil.h"
#include "format.h"
#include "program.h"
#include "object.h"

uint5 loadsection(section* sec, FILE* f)
{
  sec->content = jt_buffer_new(8);

  if (sec->header.sh_size > 0)
  {
    int expectedsize = pad(sec->header.sh_size);
    if (expectedsize > 300000000)
    {
      fprintf(stderr, "Very large section! %d bytes!\n", expectedsize);
      exit(1);
    }
    jt_buffer_extend(sec->content, expectedsize);
    return fread(sec->content->buffer, expectedsize, 1, f) * expectedsize;
  }
  return 0;
}

/*
 Must match write_elf in asm/elfutil.c
 */
image* loadimage(FILE* f, int inlibrary)
{
  image* img = jt_new(image);
  uint5 total = 0;
  section* sections[SHN_COUNT];
  const uint5 numsections = sizeof(sections)/sizeof(section*);
  uint5 i;

  /* yucky innit. */
  sections[0]  = &img->null;
  sections[1]  = &img->shstrtab;
  sections[2]  = &img->text;
  sections[3]  = &img->data;
  sections[4]  = &img->rodata;
  sections[5]  = &img->bss;
  sections[6]  = &img->symtab;
  sections[7]  = &img->strtab;
  sections[8]  = &img->reltext;
  sections[9]  = &img->reldata;
  sections[10] = &img->relrodata;
  sections[11] = &img->relbss;

  img->symbolmap = 0;

  total += fread(&img->ehdr, sizeof(Elf32_Ehdr), 1, f) * sizeof(Elf32_Ehdr);
  
  if (img->ehdr.e_ident[EI_MAG0] != ELFMAG0 ||
      img->ehdr.e_ident[EI_MAG1] != ELFMAG1 ||
      img->ehdr.e_ident[EI_MAG2] != ELFMAG2 ||
      img->ehdr.e_ident[EI_MAG3] != ELFMAG3)
  {
    fprintf(stderr, "Unrecognized file format\n");
    exit(1);
  }
  
  // read section headers
  for (i=0; i<numsections; i++)
  {
    total += fread(&sections[i]->header, sizeof(Elf32_Shdr), 1, f) *
               sizeof(Elf32_Shdr);
    fixshdr(&sections[i]->header);
  }
  
  // read section data
  for (i=0; i<numsections; i++)
  {
    total += loadsection(sections[i], f);
  }

#ifdef DEBUG  
  fprintf(stderr, "Read %d bytes total\n", total);
#endif
  
  /* fiddle symtab to make it host byte order */
  fixsymtab(img->symtab.content->buffer,
    img->symtab.content->length / sizeof(Elf32_Sym));
  
  img->used = IMAGE_UNUSED;
  img->library = inlibrary;
  
  return img;
}

char* symbolname(Elf32_Sym* sym, section* strtab)
{
  return buffer_IDX(strtab->content, char, sym->st_name);
}

/* add symbols in object file to local symbol map */
void readsymbols(section* symtab, section* strtab, jt_map** symbolmap)
{
  int i;
  Elf32_Sym* thissym;
  
  for (i=1; i<symtab->content->length/sizeof(Elf32_Sym); i++)
  {
    mapentry* mentry = jt_new(mapentry);

    thissym = buffer_IDX(symtab->content, Elf32_Sym, i);

    mentry->name = symbolname(thissym, strtab);
    mentry->binding = ELF32_ST_BIND(thissym->st_info);
    mentry->symtabentry = i;
    mentry->backpatches = 0;

#ifdef DEBUG
/*    fprintf(stderr, "Read symbol %s\n", mentry->name);*/
#endif
    jt_map_insert(symbolmap, mentry->name, mentry);
  }
}

/* Return pointer to symbol, given a name and an object file image */
Elf32_Sym* getsymbol(image* from, jt_map* map, char* name)
{
  mapentry* sym = jt_map_find(map, name);
  
  if (sym->symtabentry == 0) return 0;
  
  return buffer_IDX(from->symtab.content, Elf32_Sym, sym->symtabentry);
}

/* find global symbols defined in an object file, add to global symbol map.
   Overrides weak symbols with global symbols.
*/
void findglobals(image* in, jt_map** global)
{
  uint5 i;
  
  for (i=1; i<in->symtab.content->length/sizeof(Elf32_Sym); i++)
  {
    Elf32_Sym* sym = buffer_IDX(in->symtab.content, Elf32_Sym, i);
    char* name = symbolname(sym, &in->strtab);
    mapentry* oldmsym = jt_map_find(*global, name);
    uint5 newbinding = ELF32_ST_BIND(sym->st_info);
    
    if ((newbinding!=STB_GLOBAL && newbinding!=STB_WEAK) || 
        sym->st_shndx==SHN_UNDEF) continue;
    
    if (oldmsym)
    {
      if (!in->library)
      {
        /* We have a symbol with this name already */
        switch (oldmsym->binding)
        {
          case STB_GLOBAL:
          if (newbinding==STB_GLOBAL)
          {
            fprintf(stderr, "Multiply-defined global symbol %s\n", name);
            exit(1);
          }
          /* Else new symbol is weak, we can ignore it */
          break;

          case STB_WEAK:
          if (newbinding==STB_WEAK)
          {
            #ifdef DEBUG2
            fprintf(stderr, "Warning: multiply-defined weak symbol %s\n", name);
            #endif
          }
          else
          {
            /* Override old weak symbol */
            oldmsym->binding = STB_GLOBAL;
            oldmsym->symtabentry = i;
            oldmsym->in_image = in;
          }
          break;

          default:
          fprintf(stderr, "Non-global symbol in global symbol map? (%s)\n",
                  name);
          exit(1);
        }
      }
    }
    else
    {
      /* Generate a new global map entry */
      mapentry* msym = jt_new(mapentry);
      
      msym->name = name;
      msym->binding = newbinding;
      msym->symtabentry = i;
      msym->in_image = in;
      msym->index = -1u;
      msym->backpatches = 0;
      
      jt_map_insert(global, name, msym);
    }
  }
}
