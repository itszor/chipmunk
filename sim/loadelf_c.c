#include <stdio.h>
#include <elf.h>

#include <defs.h>
#include <config.h>
#include <fixendian.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/alloc.h>

#undef DEBUG_LOAD

CAMLprim value load_elf_c(value ml_filename)
{
  char* filename;
  FILE* f;
  Elf32_Ehdr ehdr;
  Elf32_Phdr* phdr;
  Elf32_Shdr* shdr;
  int i;
  value* ml_writeword;
  CAMLparam1(ml_filename);
  CAMLlocal3(ml_res, ml_val, ml_pokeat);
  int sectiontable = 0x80000000;
  int lastidx = 0;
  
  ml_writeword = caml_named_value("write word");
  
  filename = String_val(ml_filename);

#ifdef DEBUG_LOAD
  fprintf(stderr, "Loading %s (C)\n", filename);
#endif
  
  f = fopen(filename, "r");
  if (!f)
  {
    fprintf(stderr, "Can't open file '%s'\n", filename);
    exit(1); // throw an exception!
  }
  fread(&ehdr, sizeof(Elf32_Ehdr), 1, f);
  if (ehdr.e_ident[EI_MAG0] != ELFMAG0 ||
      ehdr.e_ident[EI_MAG1] != ELFMAG1 ||
      ehdr.e_ident[EI_MAG2] != ELFMAG2 ||
      ehdr.e_ident[EI_MAG3] != ELFMAG3)
  {
    fprintf(stderr, "'%s' doesn't look like an ELF file\n", filename);
    exit(1);
  }
  phdr = calloc(FIXHALF(ehdr.e_phnum), FIXHALF(ehdr.e_phentsize));
  fread(phdr, FIXHALF(ehdr.e_phnum), FIXHALF(ehdr.e_phentsize), f);
  shdr = calloc(FIXHALF(ehdr.e_shnum), FIXHALF(ehdr.e_shentsize));
  fread(shdr, FIXHALF(ehdr.e_shnum), FIXHALF(ehdr.e_shentsize), f);
  
  for (i=0; i<FIXHALF(ehdr.e_phnum); i++)
  {
    Elf32_Phdr* psec = &phdr[i];
    int j, paddedsize;

#ifdef DEBUG_LOAD
    fprintf(stderr, "Reading program section %d from offset %d\n", i,
      FIXWORD(psec->p_offset));
#endif
    fseek(f, FIXWORD(psec->p_offset), SEEK_SET);

    /* write base of wotsit to section table */
    ml_pokeat = copy_int32(sectiontable + i*4);
    ml_val = copy_int32(FIXWORD(psec->p_vaddr));
#ifdef DEBUG_LOAD
    fprintf(stderr, "Writing section addr %.8x to %.8x\n",
      FIXWORD(psec->p_vaddr),
      sectiontable + i*4);
#endif
    callback2(*ml_writeword, ml_pokeat, ml_val);

    paddedsize = (FIXWORD(psec->p_filesz)+3) & ~3;

    for (j=0; j<paddedsize; j+=4)
    {
      int val, addr;
/*      CAMLlocal2(ml_val, ml_pokeat);*/
      addr = FIXWORD(psec->p_vaddr) + j;
      fread(&val, sizeof(int), 1, f);
      val = FIXWORD(val);
#ifdef DEBUG_LOAD
      fprintf(stderr, "Writing %.8x to %.8x\n", val, addr);
#endif
      ml_pokeat = copy_int32(addr);
      ml_val = copy_int32(val);
      callback2(*ml_writeword, ml_pokeat, ml_val);
    }
    
    if (i==0)
    {
      lastidx = paddedsize/16;
#ifdef DEBUG_LOAD
      fprintf(stderr, "Number of index entries: %d\n", lastidx);
#endif
    }
  }

  ml_pokeat = copy_int32(0x80000018);
  ml_val = copy_int32(lastidx);
  callback2(*ml_writeword, ml_pokeat, ml_val);

#ifdef DEBUG_LOAD
  fprintf(stderr, "Done.\n");
#endif
  
  free(shdr);
  free(phdr);
  
  fclose(f);
  
  ml_res = copy_int32(FIXWORD(ehdr.e_entry));
  
  CAMLreturn(ml_res);
}

