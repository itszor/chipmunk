#include <assert.h>
#include <stdio.h>
#include <string.h>

#include <elf.h>

#include <config.h>
#include <fixendian.h>

#include "parsedefs.h"
#include "elfutil.h"

void fillshdr(Elf32_Shdr* shdr,
              uint5 type,
              uint5 flags,
              uint5 addr,
              uint5 offset,
              uint5 size,
              uint5 link,
              uint5 info,
              uint5 addralign,
              uint5 entsize)
{
  shdr->sh_name = FIXWORD(shdr->sh_name); /* name set already! */
  shdr->sh_type = FIXWORD(type);
  shdr->sh_flags = FIXWORD(flags);
  shdr->sh_addr = FIXWORD(addr);
  shdr->sh_offset = FIXWORD(offset);
  shdr->sh_size = FIXWORD(size);
  shdr->sh_link = FIXWORD(link);
  shdr->sh_info = FIXWORD(info);
  shdr->sh_addralign = FIXWORD(addralign);
  shdr->sh_entsize = FIXWORD(entsize);
}

void fixshdr(Elf32_Shdr* shdr)
{
  shdr->sh_name = FIXWORD(shdr->sh_name);
  shdr->sh_type = FIXWORD(shdr->sh_type);
  shdr->sh_flags = FIXWORD(shdr->sh_flags);
  shdr->sh_addr = FIXWORD(shdr->sh_addr);
  shdr->sh_offset = FIXWORD(shdr->sh_offset);
  shdr->sh_size = FIXWORD(shdr->sh_size);
  shdr->sh_link = FIXWORD(shdr->sh_link);
  shdr->sh_info = FIXWORD(shdr->sh_info);
  shdr->sh_addralign = FIXWORD(shdr->sh_addralign);
  shdr->sh_entsize = FIXWORD(shdr->sh_entsize);
}

void fillphdr(Elf32_Phdr* phdr,
              uint5 type,
              uint5 offset,
              uint5 vaddr,
              uint5 paddr,
              uint5 filesz,
              uint5 memsz,
              uint5 flags,
              uint5 align)
{
  phdr->p_type = FIXWORD(type);
  phdr->p_offset = FIXWORD(offset);
  phdr->p_vaddr = FIXWORD(vaddr);
  phdr->p_paddr = FIXWORD(paddr);
  phdr->p_filesz = FIXWORD(filesz);
  phdr->p_memsz = FIXWORD(memsz);
  phdr->p_flags = FIXWORD(flags);
  phdr->p_align = FIXWORD(align);
}

void fixsymtab(Elf32_Sym* sym, uint5 num)
{
  int i;
  for (i=0; i<num; i++)
  {
    sym[i].st_name = FIXWORD(sym[i].st_name);
    sym[i].st_value = FIXWORD(sym[i].st_value);
    sym[i].st_size = FIXWORD(sym[i].st_size);
    sym[i].st_shndx = FIXHALF(sym[i].st_shndx);
  }
}

void fixrel(Elf32_Rel* rel)
{
  rel->r_offset = FIXWORD(rel->r_offset);
  rel->r_info = FIXWORD(rel->r_info);
}

void createandname(section* sec, section* names, char* name)
{
  sec->content = jt_buffer_new(16);
  sec->header.sh_name = jt_buffer_append(names->content, name, strlen(name)+1);
}

// pad up to word boundary
uint5 pad(uint5 x)
{
  return (x+3) & ~3;
}

mapentry* newmapentry(char* name, uint5 binding, uint5 stentry)
{
  mapentry* ent = jt_new(mapentry);
  
  ent->name = name;
  ent->binding = binding;
  ent->symtabentry = stentry;
  ent->backpatches = 0;
  
  return ent;
}

/*
 Add a new symbol to the symbol table, putting its name in the string table
 */
uint5 newsymbol(section* strtab, section* symtab, char* name, Elf32_Addr
  value, Elf32_Word size, uint5 binding, uint5 type, uint5 section)
{
  Elf32_Sym sym;
  
  assert(name);
  
  sym.st_name = jt_buffer_append(strtab->content, name, strlen(name)+1);
  sym.st_value = value;
  sym.st_size = size;
  sym.st_info = ELF32_ST_INFO(binding, type);
  sym.st_other = 0;
  sym.st_shndx = section;
  
  return jt_buffer_append(symtab->content, &sym, sizeof(sym));
}

/*
 We call processsymbol when a new label is found. Any previous references
 to this symbol are backpatched by generating relocations for them now we
 know where they start. A symbol for the label is put into the symbol table.
 */
void processsymbol(image* img, char* name, Elf32_Addr
  value, Elf32_Word size, uint5 type, uint5 section)
{
  mapentry* sym;
  uint5 mysymno = img->symtab.content->length/sizeof(Elf32_Sym);  // groo
  uint5 binding = STB_LOCAL;

  if ((sym = jt_map_find(img->symbolmap, name)))
  {
    while (sym->backpatches)
    {
      backpatch* patch = sym->backpatches->data;
      Elf32_Rel myrel;

      /* add relocation for this entry */
      myrel.r_offset = patch->value;
      myrel.r_info = ELF32_R_INFO(mysymno, patch->reloctype);
      fixrel(&myrel);
      jt_buffer_append(patch->relsection->content, &myrel, sizeof(myrel));
      /*fprintf(stderr, "Forward relocation: %s at %d\n", name, patch->value);*/

      jt_delete(patch);
      jt_list_removehead(&sym->backpatches);
    }
    sym->symtabentry = mysymno;  // this entry
  }
  else  // new symbol, no previous forward references
  {
    sym = newmapentry(name, STB_LOCAL, mysymno);
    jt_map_insert(&img->symbolmap, name, sym);
  }

  // this is a nasty hack.
  binding = (type==STT_NOTYPE && section==SHN_UNDEF)
    ? STB_GLOBAL : sym->binding;

  newsymbol(&img->strtab, &img->symtab, name, 
    value, size, binding, type, section);
}

/*
 Change the binding of a symbol, either before it's been previously referenced
 or retrospectively.
 */
void rebindsymbol(jt_map** symbolmap, char* name, section* symtab,
  uint5 binding)
{
  mapentry* sym;

  /*fprintf(stderr, "Rebinding symbol %s\n", name);*/

  if ((sym = jt_map_find(*symbolmap, name)))
  {
    sym->binding = binding;

    if (sym->symtabentry != 0)
    {
      Elf32_Sym* rewritesym =
        buffer_IDX(symtab->content, Elf32_Sym, sym->symtabentry);

      rewritesym->st_info = ELF32_ST_INFO(binding, 
        ELF32_ST_TYPE(rewritesym->st_info));
    }
  }
  else
  {
    sym = newmapentry(name, binding, 0);
    jt_map_insert(symbolmap, name, sym);
  }
}

/*
 Generate a new relocation for a given target address. If we haven't found the
 referred-to symbol yet, delay the generation of the relocation until we do.
 */
void newreloc(section* rel, jt_map** symbolmap, char* name, uint5 type,
  uint5 offset)
{
  mapentry* sym;

  assert(name);
  
  if (!(sym = jt_map_find(*symbolmap, name)))
  {
    /* this is the first forward reference to this symbol */
    sym = newmapentry(name, STB_LOCAL, 0);
    jt_map_insert(symbolmap, name, sym);
  }

  if (sym->symtabentry == 0)
  {
    /* we haven't come across the symbol yet, add to patch list */
    backpatch* patch = jt_new(backpatch);

    patch->value = offset;
    patch->reloctype = type;
    patch->size = 4;
    patch->relsection = rel;

    jt_list_add(&sym->backpatches);
    sym->backpatches->data = patch;
    
    /* fprintf(stderr, "Delaying relocation %s at %d\n", name, offset);*/
  }
  else
  {
    /* we've already defined this symbol, create relocation immediately */
    Elf32_Rel myrel;
    myrel.r_offset = offset;
    myrel.r_info = ELF32_R_INFO(sym->symtabentry, type);
    fixrel(&myrel);
    jt_buffer_append(rel->content, &myrel, sizeof(myrel));
    /*fprintf(stderr, "Backward relocation: %s at %d\n", name, offset);*/
  }
}

void adddelayedreloc(jt_list** addto, char* name, uint5 type,
  uint5 offset)
{
  delayedreloc* dr = jt_new(delayedreloc);

  dr->name = name;
  dr->type = type;
  dr->offset = offset;

  jt_list_add(addto);
  (*addto)->data = dr;
}

void undefinedrefs(jt_map* node, void* data)
{
  image* img = (image*) data;
  mapentry* thisentry = node->value;
  
  if (thisentry->symtabentry == 0)
  {
    /*fprintf(stderr, "Global/undefined symbol: %s\n", thisentry->name);*/
    processsymbol(img, thisentry->name, 0, sizeof(uint5), STT_NOTYPE,
      SHN_UNDEF);
  }
}

