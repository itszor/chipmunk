#include "defs.h"
#include "bset.h"
#include "cnew.h"

static const uint3 nbit[] =
{
  0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4,
  1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
  1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
  1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
  3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
  1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
  3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
  2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
  3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
  3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
  4, 5, 5, 4, 5, 6, 6, 7, 5, 6, 6, 7, 6, 7, 7, 8
};

// returns number of set bits in a word
// not actually used at the moment, but I feel it should be here ;-)
uint5 jt_bset_setbits32(uint5 x)
{
  return nbit[x&0xff] + nbit[(x>>8)&0xff]
	     + nbit[(x>>16)&0xff] + nbit[(x>>24)&0xff];
}

jt_bset* jt_bset_new(uint5 length)
{
  jt_bset* bset = jt_new(jt_bset);
  uint5 i, words = (length+31) >> 5;
  
  bset->bits = jt_newarray(uint5, words);
  bset->length = length;
  
  for (i=0; i<words; i++) bset->bits[i] = 0;
  
  return bset;
}

void jt_bset_delete(jt_bset* bset)
{
  jt_delete(bset->bits);
  jt_delete(bset);
}

void jt_bset_union(jt_bset* dest, jt_bset* src1, jt_bset* src2)
{
  uint5 i, words = (src1->length+31) >> 5;
  
  for (i=0; i<words; i++)
    dest->bits[i] = src1->bits[i] | src2->bits[i];
}

void jt_bset_intersection(jt_bset* dest, jt_bset* src1,
                       jt_bset* src2)
{
  uint5 i, words = (src1->length+31) >> 5;
  
  for (i=0; i<words; i++)
    dest->bits[i] = src1->bits[i] & src2->bits[i];
}

void jt_bset_not(jt_bset* dest, jt_bset* src)
{
  uint5 i, words = (src->length+31) >> 5;
  
  for (i=0; i<words; i++)
    dest->bits[i] = ~src->bits[i];
}

void jt_bset_difference(jt_bset* dest, jt_bset* src1,
                        jt_bset* src2)
{
  jt_bset_not(dest, src2);
  jt_bset_intersection(dest, src1, dest);
}

void jt_bset_clear(jt_bset* myset)
{
  uint5 i, words = (myset->length+31) >> 5;

  for (i=0; i<words; i++)
    myset->bits[i] = 0;
}
