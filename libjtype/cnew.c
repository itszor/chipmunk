#include <stdlib.h>
#include <stdio.h>

#include "cnew.h"

void* jt_safemalloc(size_t bytes)
{
  void* mem = malloc(bytes);

  if (!mem)
  {
    fprintf(stderr, "Out of memory!\n");
    exit(1);
  }

  return mem;
}

void* jt_safecalloc(size_t bytes, size_t num)
{
  void* mem = calloc(bytes, num);

  if (!mem)
  {
    fprintf(stderr, "Out of memory!\n");
    exit(1);
  }

  return mem;
}
