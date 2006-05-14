/* Creates an executable file from a bunch of relocatable files */

#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <defs.h>
#include <cnew.h>
#include <clist.h>
#include <buffer.h>
#include <config.h>
#include <fixendian.h>

#include "elfutil.h"
#include "format.h"
#include "program.h"
#include "object.h"
#include "archive.h"

#define SHN_HEAP_MAGIC (SHN_COUNT+1)

/* Put global symbols from map into a global symbol table.
*/
void globalsymtabhelper(jt_map* node, void* data)
{
  program* prog = data;
  mapentry* mentry = node->value;
  image* thisimage = mentry->in_image;
  Elf32_Sym* sym = getsymbol(thisimage, thisimage->symbolmap, 
    node->name);
  uint5 sectionoffset = 0;  // offset of concatenated section

  assert(sym);

  if (thisimage->used != IMAGE_USED) return;
  
  /* ew, dirty. */
/*  localsym->index += sectionoffset;*/

#ifdef DEBUG
  fprintf(stderr, "Adding symbol %s to global symbol table\n", node->name);
#endif
  
  newsymbol(&prog->strtab, &prog->symtab, node->name,
    sym->st_value + sectionoffset, sym->st_size, ELF32_ST_BIND(sym->st_info),
    ELF32_ST_TYPE(sym->st_info), sym->st_shndx);
}

/* generate a global symbol table from a global symbol map
*/
void makeglobalsymtab(program* prog)
{
  jt_map_forall(prog->globalsymbolmap, globalsymtabhelper, prog);
}

/* Index program binary, also sets index field to correct offset in data
   areas
*/
void indexify(section* index, section* symtab, section* strtab,
  jt_map* symbolmap, program* prog)
{
  uint5 i;
  uint5 initial = index->content->length / sizeof(indexentry);
  uint5 idxno = initial;
  
  for (i=1; i<symtab->content->length/sizeof(Elf32_Sym); i++)
  {
    Elf32_Sym* sym = buffer_IDX(symtab->content, Elf32_Sym, i);
    char* symname = symbolname(sym, strtab);
    mapentry* msym = jt_map_find(symbolmap, symname);
    indexentry entry;

    switch (sym->st_shndx)
    {
      case SHN_TEXT:
      {
        char sname[256];
        uint5 soffset;

        sprintf(sname, "%d%s", idxno, symname);
        soffset = jt_buffer_append(prog->name.content, sname, strlen(sname)+1);
      
        entry.offset = pad(prog->text.content->length) + sym->st_value;
        entry.offset = FIXWORD(entry.offset);
        entry.parent = 0;
        entry.meta = FIXWORD(soffset);
        entry.count = 0;
        jt_buffer_append(index->content, &entry, sizeof(indexentry));

        msym->index = sizeof(indexentry) * idxno++;

        #ifdef DEBUG
        fprintf(stderr, "Symbol %s at index %d\n", symbolname(sym, strtab), 
                msym->index);
        #endif
      }
      break;

      case SHN_DATA:
      /* !!! sym->st_value is probably wrong! Check */
      msym->index = pad(prog->data.content->length) + sym->st_value;
      break;
      
      case SHN_RODATA:
      msym->index = pad(prog->rodata.content->length) + sym->st_value;
      break;
      
      case SHN_BSS:
      {
        uint5 length = *(uint5*)prog->bss.content->buffer;
        msym->index = pad(length) + sym->st_value;
      }
      break;
      
      case SHN_HEAP_MAGIC:
      msym->index = 0;
      break;
      
      default:
      msym->index = -1u;
      break;
    }
  }
}

/* Add a section "src" to section "dest", offsetting each relocation in
   reloc by the initial length of dest
*/
void merge(section* dest, section* src, section* reloc)
{
  uint5 initlength;
  uint5 i;
  
  assert((src->content->length & 3)==0);
  
  initlength = jt_buffer_append(dest->content, src->content->buffer, 
    src->content->length);

  /* ensure correct alignment... */  
  assert(!(initlength & 3));
  
  for (i=0; i<reloc->content->length/sizeof(Elf32_Rel); i++)
  {
    Elf32_Rel* myrel = buffer_IDX(reloc->content, Elf32_Rel, i);
    uint5 word;
    
    /* offset relocation */
    word = FIXWORD(myrel->r_offset);
    word += initlength;
    myrel->r_offset = FIXWORD(word);
  }
}

/* bss is kept in TARGET byte order */
void mergebss(section* dest, section* src)
{
  uint5 destlen, srclen;
  
  if (dest->content->length == 0)
  {
    uint5 word = 0;
    #ifdef DEBUG
    fprintf(stderr, "(Generating new dest bss buffer)\n");
    #endif
    jt_buffer_append(dest->content, &word, sizeof(word));
  }
  
  assert(dest->content->length == 4);
  #ifdef DEBUG
  if (src->content->length > 0)
  {
    fprintf(stderr, "src->content->length is %d\n", src->content->length);
  }
  #endif
  
  destlen = *((uint5*)dest->content->buffer);
  destlen = FIXWORD(destlen);
  srclen = (src->content->length == 0) ? 0 : *((uint5*)src->content->buffer);
  srclen = FIXWORD(srclen);
  
  #ifdef DEBUG
  fprintf(stderr, "Merging bss, destlen=%d, srclen=%d\n", destlen, srclen);
  #endif
  
  destlen += srclen;
  destlen = FIXWORD(destlen);
  
  *((uint5*)dest->content->buffer) = destlen;
}

uint5 offsetfrommapentry(program* prog, image* img, mapentry* lmsym)
{
  Elf32_Sym* sym = buffer_IDX(img->symtab.content, Elf32_Sym, 
                              lmsym->symtabentry);

/*  
  fprintf(stderr, "Symtabentry for %p=%d\n", lmsym, lmsym->symtabentry);
*/
  
  switch (sym->st_shndx)
  {
    case SHN_DATA: return prog->start.data;
    case SHN_RODATA: return prog->start.rodata;
    case SHN_BSS: return prog->start.bss;
    case SHN_HEAP_MAGIC:
    { 
#ifdef DEBUG
      fprintf(stderr, "Casting heap magic at %p\n", prog->start.heap);
#endif
      return prog->start.heap;
    }
  }
  
  return 0;
}

/* perform relocation on a (text/data) segment, looking for symbols in
   local table first, then global table
*/
void relocate(section* sec, section* reloc, image* local, program* global)
{
  uint5 i;
  
  for (i=0; i<reloc->content->length/sizeof(Elf32_Rel); i++)
  {
    Elf32_Rel* rel = buffer_IDX(reloc->content, Elf32_Rel, i);
    uint5 symtabentry;
    Elf32_Sym* sym;
    char* name;
    uint5 symvalue = 0;
    uint5* reladdr;
    
    fixrel(rel);

    symtabentry = ELF32_R_SYM(rel->r_info);
    sym = buffer_IDX(local->symtab.content, Elf32_Sym, symtabentry);
    name = symbolname(sym, &local->strtab);

    reladdr = buffer_IDX(sec->content, uint5, rel->r_offset/sizeof(uint5));

    #ifdef DEBUG
    fprintf(stderr, "name=%s\n", name);
    fprintf(stderr, "rel->r_offset = %.8x (%.8x rev)\n",
      rel->r_offset, FIXWORD(rel->r_offset));
    fprintf(stderr, "reladdr=%p\n", reladdr);
    #endif

    assert(rel->r_offset >= 0 && rel->r_offset < sec->content->length);

    if (sym->st_shndx == SHN_UNDEF)
    {
      mapentry* gmsym = jt_map_find(global->globalsymbolmap, name);
      mapentry* lmsym;
      image* farimage;

      /* binutils-isms? */
      if (strcmp(name, "_$Linitialized.1")==0 ||
          strcmp(name, "___CTOR_LIST__")==0 ||
          strcmp(name, "___DTOR_LIST__")==0) continue;

      if (!gmsym)
      {
        fprintf(stderr, "Unresolved global %s\n", name);
        exit(1);
      }

      farimage = gmsym->in_image;

      lmsym = jt_map_find(farimage->symbolmap, name);

      if (!lmsym)
      {
        fprintf(stderr, "Can't find %s in %p\n", name, lmsym);
        exit(1);
      }

      symvalue = lmsym->index + offsetfrommapentry(global, farimage, lmsym);
    }
    else
    {
      mapentry* lmsym = jt_map_find(local->symbolmap, name);
      symvalue = lmsym->index + offsetfrommapentry(global, local, lmsym);
    }
    
    switch (ELF32_R_TYPE(rel->r_info))
    {
      /* Why on earth did this seem like a smart idea? */
      case R_CHAM_INDIRECT:
      {
        fmt_inst inst;
        uint5 i;
        uint5 poke = 0;
        
        inst.flat = *reladdr;
        inst.flat = FIXWORD(inst.flat);
        inst.noop.number = symvalue;
        #ifdef DEBUG
        fprintf(stderr, "inst.flat=%.8x\n", inst.flat);
        fprintf(stderr, "inst.generic.opcode=%d\n", inst.generic.opcode);
        #endif
        
        for (i=1; i<=2; i++)
        {
          fmt_inst inst2;
          inst2.flat = reladdr[i];
          inst2.flat = FIXWORD(inst2.flat);
          /* a possibly-block terminating instruction */
          if ((inst2.generic.opcode >= opcode_TRAPZ &&
               inst2.generic.opcode <= opcode_RET))
          {
            switch (inst.generic.opcode)
            {
              case opcode_LDX:
              inst2.tfer.rx = symvalue >> 26;
              poke = 1;
              break;
              
              case opcode_LDY:
              inst2.tfer.ry = symvalue >> 26;
              poke = 1;
              break;
              
              case opcode_LDZ:
              inst2.cmp.rc = symvalue >> 26;
              poke = 1;
              break;
              
              default:
              fprintf(stderr, "Bad relocation (not pointing to LDX, LDY, LDZ)"
                " for %s\n", name);
            }
            if (poke)
            {
              reladdr[i] = FIXWORD(inst2.flat);
              reladdr[0] = FIXWORD(inst.flat);
              #ifdef DEBUG
              fprintf(stderr, "Written %d for %s at %.8x\n", symvalue, name,
                rel->r_offset+i*4);
              #endif
              break;
            }
          }
          #ifdef DEBUG
          else
          {
            if (!(inst.generic.opcode == opcode_LDX &&
                  inst2.generic.opcode == opcode_LDY))
            {
              fprintf(stderr, "non-relocation opcode: %d\n", 
                inst2.generic.opcode);
              fprintf(stderr, "base inst opcode: %d\n",
                inst.generic.opcode);
            }
          }
          #endif
        }
        if (!poke)
        {
          fprintf(stderr, "Relocation failed for %s\n", name);
          abort();
        }
      }
      break;
      
      case R_CHAM_ADRL:
      {
        fmt_inst inst;
        
        inst.flat = *reladdr;
        inst.flat = FIXWORD(inst.flat);
        inst.mvc.imm = symvalue;
        *reladdr = FIXWORD(inst.flat);
        
        #ifdef DEBUG
        fprintf(stderr, "Writing %d (%x) for ADRL %s\n", inst.mvc.imm,
          inst.mvc.imm, name);
        #endif
      }
      break;
      
      case R_CHAM_ADRH:
      {
        fmt_inst inst;
        
        inst.flat = *reladdr;
        inst.flat = FIXWORD(inst.flat);
        inst.mvc.imm = symvalue >> 16;
        *reladdr = FIXWORD(inst.flat);

        #ifdef DEBUG
        fprintf(stderr, "Writing %d (%x) for ADRH %s\n", inst.mvc.imm,
          inst.mvc.imm, name);
        #endif
      }
      break;
      
      case R_CHAM_DCV:
      {
        uint5 word;
        #ifdef DEBUG
        fprintf(stderr, "Initial value: %d   ", *reladdr);
        #endif
        word = *reladdr;
        word = FIXWORD(word);
        word += symvalue;
        *reladdr = FIXWORD(word);
        #ifdef DEBUG
        fprintf(stderr, "Added %d (%x) for %s\n", symvalue, symvalue, name);
        #endif
      }
      break;
      
      default:
      fprintf(stderr, "Bad relocation type: %d\n", ELF32_R_TYPE(rel->r_info));
    }
  }
}

void closedependencies(program* prog, jt_clist* objects, char* contains)
{
  mapentry* thissym = jt_map_find(prog->globalsymbolmap, contains);
  image* definedin;
  int i, numsym;
  
  if (!thissym)
  {
    fprintf(stderr, "Unresolved symbol [%s]\n", contains);
    return;
  }
  
  definedin = thissym->in_image;
  
  assert(definedin);
  
  if (definedin->used == IMAGE_USED) return;
  
  #ifdef DEBUG
  fprintf(stderr, "Pulling in object at %p\n", definedin);
  #endif
  
  definedin->used = IMAGE_USED;
  
  numsym = definedin->symtab.content->length/sizeof(Elf32_Sym);
  
  #ifdef DEBUG
  fprintf(stderr, "Scanning %d symbols...\n", numsym);
  #endif
  
  for (i=1; i<numsym; i++)
  {
    Elf32_Sym* sym = buffer_IDX(definedin->symtab.content, Elf32_Sym, i);
    
    if (sym->st_shndx == SHN_UNDEF)
    {
      char* symname = symbolname(sym, &definedin->strtab);
      #ifdef DEBUG
      fprintf(stderr, "Trying to resolve [%s]\n", symname);
      #endif
      closedependencies(prog, objects, symname);
    }
  }
}

void mergeused(program* prog, jt_clist* objects)
{
  jt_clist* scan;
  
  for (scan=objects->next; scan->data; scan=scan->next)
  {
    image* thisimg = scan->data;

    #ifdef DEBUG
    fprintf(stderr, "Image has bss size of %d, and is %sused\n",
      thisimg->bss.content->length, thisimg->used==IMAGE_USED ?
        "" : "not ");
    #endif

    if (thisimg->used == IMAGE_USED)
    {
      indexify(&prog->index, &thisimg->symtab, &thisimg->strtab,
        thisimg->symbolmap, prog);
      
      merge(&prog->text, &thisimg->text, &thisimg->reltext);
      merge(&prog->data, &thisimg->data, &thisimg->reldata);
      merge(&prog->rodata, &thisimg->rodata, &thisimg->relrodata);
      mergebss(&prog->bss, &thisimg->bss);
    }
  }
}

void initialiseheap(program* prog)
{
  /* fake object file */
  image* heapobj = jt_new(image);
  /* Generate new global map entry */
  mapentry* msym = jt_new(mapentry);
  mapentry* localsym;
  Elf32_Sym emptysym, heapsym;
  char* heapname = "____dop_heapbase___";
  
  heapobj->shstrtab.content = jt_buffer_new(16);
  
  jt_buffer_append(heapobj->shstrtab.content, "", 1);
  
  heapobj->shstrtab.header.sh_name = jt_buffer_append(heapobj->shstrtab.content,
    ".shstrtab", strlen(".shstrtab")+1);
    
  createandname(&heapobj->strtab, &heapobj->shstrtab, ".strtab");
  createandname(&heapobj->symtab, &heapobj->shstrtab, ".symtab");

  // null entries at start of strtab and symtab
  jt_buffer_append(heapobj->strtab.content, "", 1);
  emptysym.st_name = 0;
  emptysym.st_value = 0;
  emptysym.st_size = 0;
  emptysym.st_info = 0;
  emptysym.st_other = 0;
  emptysym.st_shndx = SHN_UNDEF;
  jt_buffer_append(heapobj->symtab.content, &emptysym, sizeof(emptysym));
  
  heapsym.st_name = jt_buffer_append(heapobj->strtab.content, heapname, 
    strlen(heapname)+1);
  heapsym.st_value = 0;
  heapsym.st_size = 0;
  heapsym.st_info = 0;
  heapsym.st_other = 0;
  heapsym.st_shndx = SHN_HEAP_MAGIC;
  jt_buffer_append(heapobj->symtab.content, &heapsym, sizeof(heapsym));
  
  heapobj->symbolmap = 0;
  localsym = newmapentry(heapname, STB_GLOBAL, 1);
  jt_map_insert(&heapobj->symbolmap, heapname, localsym);
  
  msym->name = heapname;
  msym->binding = STB_GLOBAL;
  msym->symtabentry = 1;
  msym->in_image = heapobj;
  msym->index = 0;
  msym->backpatches = 0;
  
  jt_map_insert(&prog->globalsymbolmap, msym->name, msym);
}

/* does not copy path */
void addlibrarypath(jt_clist* libs, char* path)
{
  jt_clist* newlib = jt_clist_append(libs);
  newlib->data = path;
}

void loadobject(program* prog, jt_clist* objects, char* name)
{
  jt_clist* newobj = jt_clist_append(objects);
  FILE* f = fopen(name, "r");
  image* thisimage;

  if (!f)
  {
    fprintf(stderr, "Couldn't open file %s\n", name);
    exit(1);
  }

  newobj->data = thisimage = loadimage(f, 0);

  #ifdef DEBUG
  fprintf(stderr, "I have loaded image with bss of %d\n", 
    thisimage->bss.content->length);
  #endif

  fclose(f);

  readsymbols(&thisimage->symtab, &thisimage->strtab, 
              &thisimage->symbolmap);

  findglobals(thisimage, &prog->globalsymbolmap);

  /*
  indexify(&prog->index, &thisimage->symtab, &thisimage->strtab,
    thisimage->symbolmap, prog);

  // hmm, all .o files are added to final program regardless of whether
  // they have any reachable code. Is this ok?
  merge(&prog->text, &thisimage->text, &thisimage->reltext);
  merge(&prog->data, &thisimage->data, &thisimage->reldata);
  merge(&prog->rodata, &thisimage->rodata, &thisimage->reldata);
  merge(&prog->bss, &thisimage->bss, &thisimage->reldata);
  */

  /* indicate this object shouldn't be added again */
  thisimage->used = IMAGE_UNUSED;
}

void loadlibrary(program* prog, jt_clist* objects, jt_clist* librarypaths,
                 jt_map** includedlibs, char* name)
{
  jt_clist* lib;
  char* libname;
  FILE* libfile;
  int found = 0;

  #ifdef DEBUG
  fprintf(stderr, "Include library lib%s.a\n", name);
  #endif

  if (jt_map_lookup(*includedlibs, name))
  {
    #ifdef DEBUG
    fprintf(stderr, "Skipping already-included library lib%s.a\n",
      argv[i]);
    #endif
    return;
  }
  else
  {
    jt_map_insert(includedlibs, name, 0);
  }

  for (lib = librarypaths->next; lib->data; lib=lib->next)
  {
    char* path = lib->data;

    libname = malloc(strlen(name) + strlen(path) + 10);

    sprintf(libname, "%s/lib%s.a", path, name);

    libfile = fopen(libname, "r");

    if (libfile && !found)
    {
      #ifdef DEBUG
      fprintf(stderr, "Hit library %s\n", libname);
      #endif
      loadarchive(libfile, prog, objects);
      found = 1;
      fclose(libfile);
    }
    else
    {
      #ifdef DEBUG
      fprintf(stderr, "(Can't/won't open %s)\n", libname);
      #endif
    }

    free(libname);
  }

  if (!found)
  {
    fprintf(stderr, "Can't find library lib%s.a\n", name);
    exit(1);
  }
}

int main(int argc, char* argv[])
{
  jt_clist* objects = jt_clist_new();
  jt_clist* librarypaths = jt_clist_new();
  jt_map* includedlibs = 0;
  program* prog = create_program();
  char* outfile = 0;
  uint5 i;
  jt_clist* walk;
  enum {
    output_LIB,
    output_EXE
  } outputtype = output_EXE;
  FILE* f;

/*  
  addlibrarypath(librarypaths, "./");
*/ 

  #ifdef DEBUG
  for (i=0; i<argc; i++)
  {
    fprintf(stderr, "%s ", argv[i]);
  }
  fprintf(stderr, "\n");
  #endif

  for (i=1; i<argc;)
  {
    #ifdef DEBUG2
    fprintf(stderr, "arg %d: %s\n", i, argv[i]);
    #endif
    
    if (strcmp(argv[i], "-exe")==0)
    {
      outputtype = output_EXE;
      i++;
    }
    else if (strcmp(argv[i], "-lib")==0)
    {
      outputtype = output_LIB;
      i++;
    }
    else if (strcmp(argv[i], "-o")==0)
    {
      outfile = strdup(argv[++i]);
      i++;
    }
    else if (strncmp(argv[i], "-L", 2)==0)
    {
      argv[i] += 2;

      #ifdef DEBUG
      fprintf(stderr, "Add %s to search path\n", argv[i]);
      #endif

      addlibrarypath(librarypaths, strdup(argv[i]));
      i++;
    }
    else if (strncmp(argv[i], "-l", 2)==0)
    {
      char* name = argv[i]+2;
      jt_clist* lib;
      int found = 0;

      if (jt_map_lookup(includedlibs, name))
      {
        #ifdef DEBUG
        fprintf(stderr, "Skipping already-included library lib%s.a\n",
          argv[i]);
        #endif
        i++;
        continue;
      }
      else
      {
        jt_map_insert(&includedlibs, name, 0);
      }

      for (lib = librarypaths->next; lib->data; lib=lib->next)
      {
        char* path = lib->data;
        char* libname;
        FILE* libfile;

        libname = malloc(strlen(name) + strlen(path) + 10);

        sprintf(libname, "%s/lib%s.a", path, name);

        libfile = fopen(libname, "r");

        if (libfile && !found)
        {
          #ifdef DEBUG
          fprintf(stderr, "Hit library %s\n", libname);
          #endif
          loadarchive(libfile, prog, objects);
          found = 1;
          fclose(libfile);
        }
        else
        {
          #ifdef DEBUG
          fprintf(stderr, "(Can't/won't open %s)\n", libname);
          #endif
        }

        free(libname);
      }

      if (!found)
      {
        fprintf(stderr, "Can't find library lib%s.a\n", name);
        exit(1);
      }
      i++;
    }
    else if (strcmp(argv[i], "-v")==0)
    {
      fprintf(stderr, "DOP linker v. 0.3\n");
      exit(0);
    }
    else
    {
      char* name = argv[i];
      int length = strlen(name);
      char* ext = &name[length-2];
      if (strcmp(ext, ".o")==0)
      {
        loadobject(prog, objects, argv[i]);
      }
      else if (strcmp(ext, ".a")==0)
      {
        char* name = argv[i];
        FILE* libfile = fopen(name, "ro");
        loadarchive(libfile, prog, objects);
        fclose(libfile);
      }
      else
      {
        fprintf(stderr, "Unknown extension: %s (name %s)\n", ext, name);
        exit(1);
      }
      i++;
    }
  }

  /* deal with heap (makes dummy image!) */
  initialiseheap(prog);
  
  if (!outfile)
  {
    outfile = "a.out";
  }

  /* iteratively include object files by propagating use information,
   * starting at __start symbol
   */
  #ifdef DEBUG
  fprintf(stderr, "Closing dependencies...\n");
  #endif
  closedependencies(prog, objects, "__start");
  
  #ifdef DEBUG
  fprintf(stderr, "Merging binary...\n");
  #endif
  mergeused(prog, objects);

  prog->start.data = MAINBASE + pad4k(prog->text.content->length);
  prog->start.rodata = prog->start.data + pad4k(prog->data.content->length);
  prog->start.name = prog->start.rodata + pad4k(prog->rodata.content->length);
  prog->start.bss = prog->start.name + pad4k(prog->name.content->length);
  { uint5 word = *((uint5*)prog->bss.content->buffer);
    word = FIXWORD(word);
    prog->start.heap = prog->start.bss + pad4k(word); }
  
  #ifdef DEBUG
  fprintf(stderr, "prog->start.data=%x\n", prog->start.data);
  fprintf(stderr, "prog->start.rodata=%x\n", prog->start.rodata);
  fprintf(stderr, "prog->start.name=%x\n", prog->start.name);
  fprintf(stderr, "prog->start.bss=%x\n", prog->start.bss);
  fprintf(stderr, "prog->start.heap=%x\n", prog->start.heap);
  
  fprintf(stderr, "Building symbol table...\n");
  #endif

  makeglobalsymtab(prog);
  
  #ifdef DEBUG
  fprintf(stderr, "Relocating...\n");
  #endif
  for (walk=objects->next; walk->data; walk=walk->next)
  {
    image* thisimage = walk->data;
    if (thisimage->used == IMAGE_USED)
    {
      relocate(&prog->text, &thisimage->reltext, thisimage, prog);
      relocate(&prog->data, &thisimage->reldata, thisimage, prog);
      relocate(&prog->rodata, &thisimage->relrodata, thisimage, prog);
      relocate(&prog->bss, &thisimage->relbss, thisimage, prog);
      #ifdef DEBUG
      fprintf(stderr, "Done a relocation pass\n");
      #endif
    }
  }
  
  #ifdef DEBUG
  fprintf(stderr, "Done relocating\n");
  #endif
  
  make_headers(prog);
  
  #ifdef DEBUG
  fprintf(stderr, "Writing %s\n", outfile);
  #endif
  f = fopen(outfile, "w");
  if (!f) {
    fprintf(stderr, "Failed to open output file %s\n", outfile);
    exit(1);
  }
  write_elf(prog, f);
  fclose(f);
  
  return 0;
}
