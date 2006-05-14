#ifndef JT_BSET_H
#define JT_BSET_H 1

#include "defs.h"

typedef struct {
  uint5* bits;
  uint5 length;
} jt_bset;

#define jt_bset_SET(B,N) ((B)->bits[(N)>>5] |= (1<<((N)&31)))
#define jt_bset_CLEAR(B,N) ((B)->bits[(N)>>5] &= ~(1<<((N)&31)))
#define jt_bset_TEST(B,N) ((B)->bits[(N)>>5] & (1<<((N)&31)))

// ok, this is an odd one out
extern uint5 jt_bset_setbits_uint5(uint5 word);

extern jt_bset* jt_bset_new(uint5 length);

extern void jt_bset_delete(jt_bset* bset);

extern void jt_bset_union(jt_bset* dest, jt_bset* src1,
                          jt_bset* src2);
                          
extern void jt_bset_intersection(jt_bset* dest, jt_bset* src1,
                                 jt_bset* src2);
                                 
extern void jt_bset_not(jt_bset* dest, jt_bset* src);

extern void jt_bset_difference(jt_bset* dest, jt_bset* src1, 
                               jt_bset* src2);

extern void jt_bset_clear(jt_bset* myset);

#endif
