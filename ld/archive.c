#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <ar.h>

#include "defs.h"
#include "cnew.h"
#include "archive.h"
#include "program.h"
#include "object.h"

uint5 readword(FILE* ar)
{
#ifndef BIGENDIAN
  unsigned char x[4];
  int rc;
  rc = fread(x, 1, 4, ar);
  assert(rc==4);
  return x[3] | (x[2]<<8) | (x[1]<<16) | (x[0]<<24);
#else
  int x;
  fread(&x, 4, 1, ar);
  return x;
#endif
}

/* returns 0 on success, 1 if error (not sym tab) */
uint5 readsymtab(FILE* ar, struct ar_hdr* header, archive_symtab* symtab)
{
  uint5 numsym, i, strtabsize, stroffset;
  uint5* offsets, *strings;
  char* strtab;
  int rc;

  if (header->ar_name[0] != '/')
  {
    return 1;
  }
  
  numsym = readword(ar);
  
  offsets = jt_newarray(uint5, numsym);
  
  #ifdef DEBUG
  fprintf(stderr, "Reading %d symbols...\n", numsym);
  #endif
  
  sscanf(header->ar_size, "%d", &strtabsize);
  /* subtract number of things which are provably not strings */
  strtabsize -= 4*(numsym+1);
  
  for (i=0; i<numsym; i++)
  {
    offsets[i] = readword(ar);
  /*  fprintf(stderr, "Offset[%d]=%d\n", i, offsets[i]);*/
  }

  strtab = jt_newarray(char, strtabsize);
  strings = jt_newarray(uint5, numsym);
  
  rc = fread(strtab, 1, strtabsize, ar);
  
  assert(rc==strtabsize);

  for (stroffset=0, i=0; i<numsym; i++)
  {
    strings[i] = stroffset;
    /* skip string and trailing \0 */
   /* fprintf(stderr, "sym: %s\n", &strtab[stroffset]);*/
    stroffset += strlen(&strtab[stroffset])+1;
  }

  symtab->count = numsym;
  symtab->loc = offsets;
  symtab->name = strtab;
  symtab->nameidx = strings;
  
  return 0;
}

void loadarchive(FILE* ar, program* prog, jt_clist* objects)
{
  char armagic[SARMAG];
  struct ar_hdr arheader;
  int entry = 0;
  int havesymtab = 0;
  
  fread(armagic, 1, SARMAG, ar);
  
  if (strncmp(armagic, ARMAG, SARMAG) != 0)
  {
    fprintf(stderr, "This does not look like an archive file\n");
    exit(1);
  }
  
  #ifdef DEBUG
  fprintf(stderr, "Got archive file!\n");
  #endif
  
  while (!feof(ar))
  {
    long posn = ftell(ar);
    int size;
    
    if ((posn & 1) == 1)
    {
      /* odd byte boundary, we must read another character, a newline */
      int c = fgetc(ar);
      if (c != '\n')
      {
        fprintf(stderr, "Was expecting newline padding for header\n");
        exit(1);
      }
    }
    
    /* first thing we try to read from the file this iteration: if it
     * fails, we've hit the end of the file (feof() didn't seem to work)
     */
    if (fread(&arheader, 1, sizeof(arheader), ar) != sizeof(arheader))
      break;

    if (strncmp(arheader.ar_fmag, ARFMAG, sizeof(ARFMAG)-1) != 0)
    {
      fprintf(stderr, "Bad archive header\n");
      exit(1);
    }
    
    sscanf(arheader.ar_size, "%d", &size);
    posn = ftell(ar);

    #ifdef DEBUG
    fprintf(stderr, "Good archive header, length=%d\n", size);
    #endif

    if (entry==0 && !havesymtab && arheader.ar_name[0] == '/' && 
        arheader.ar_name[1] == ' ')
    {
      archive_symtab* symtab = jt_new(archive_symtab);
      /* unnamed first entry is probably a symtab */
      if (readsymtab(ar, &arheader, symtab))
      {
        fprintf(stderr, "Error reading symbol table\n");
        exit(1);
      }
      havesymtab = 1;
      /* don't really want one of those, was just being polite */
      free(symtab->loc);
      free(symtab->name);
      free(symtab->nameidx);
      free(symtab);
    }
    else if (arheader.ar_name[0] == '/' && arheader.ar_name[1] == '/')
    {
      int posn = ftell(ar), size;
      #ifdef DEBUG
      fprintf(stderr, "Detected long filename section\n");
      #endif
      sscanf(arheader.ar_size, "%d", &size);
      fseek(ar, posn+size, SEEK_SET);
      #ifdef DEBUG
      fprintf(stderr, "Skipped %d bytes\n", size);
      #endif
    }
    else
    {
      jt_clist* newobj = jt_clist_append(objects);
      image* img;
      #ifdef DEBUG
      uint5 i;
      #endif
      /* read a file from the archive */
      
      #ifdef DEBUG
      fprintf(stderr, "Original filename: ");
      if (arheader.ar_name[0]=='/')
      {
        fprintf(stderr, "(long)\n");
      }
      else
      {
        for (i=0; i<16; i++)
        {
          if (arheader.ar_name[i]=='/') break;
          fputc(arheader.ar_name[i], stderr);
        }
        fputc('\n', stderr);
      }
      #endif
      
      newobj->data = img = loadimage(ar, 1);

      readsymbols(&img->symtab, &img->strtab, &img->symbolmap);
      
      findglobals(img, &prog->globalsymbolmap);
    }

    /* position over file */
    fseek(ar, posn+size, SEEK_SET);
    entry++;
  }
  
  #ifdef DEBUG
  fprintf(stderr, "Read library, returning...\n");
  #endif
}
