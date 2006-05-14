/* This code is no longer used */

/*
#include <stdio.h>

#include <bset.h>
#include <cnew.h>

#include "block.h"
#include "format.h"

block_info* block_new(void)
{
  block_info* block = jt_new(block_info);
  
  block->liveonentry = jt_bset_new(64);
  block->liveonexit = jt_bset_new(64);
  block->base.inst = jt_newarray(fmt_inst, (block->size = 16));
  block->length = 0;
  
  return block;
}

block_info* block_appendi(block_info* block, fmt_inst inst)
{
  if (++block->length == block->size)
    block->base.inst = realloc(block->base.inst, sizeof(uint5) * 
                               (block->size*=2));
  
  block->base.inst[block->length-1] = inst;
  
  return block;
}

block_info* block_appendd(block_info* block, datum_info* data)
{
  uint5 prev=block->length, extend = 0;
  switch (data->type)
  {
    case type_BYTE: extend = 1; break;
    case type_HALF: extend = 2; break;
    case type_WORD: extend = 4; break;
    case type_RESERVE: extend = data->value; break;
  }

  if ((block->length+=extend) >= block->size)
    block->base.data = realloc(block->base.data, (block->size*=2));

  switch (data->type)
  {
    case type_BYTE:
    block->base.data[prev] = data->value & 0xff;
    case type_HALF:
    block->base.data[prev+1] = (data->value>>8) & 0xff;
    case type_WORD:
    block->base.data[prev+2] = (data->value>>16) & 0xff;
    block->base.data[prev+3] = (data->value>>24) & 0xff;
  }  
  
  return block;
}
*/
