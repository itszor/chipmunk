#ifndef JT_CLIST_H
#define JT_CLIST_H 1

#include "list.h"

typedef struct jt_clist_t {
  void* data;
  struct jt_clist_t* prev;
  struct jt_clist_t* next;
} jt_clist;

extern jt_clist* jt_clist_new(void);
extern jt_clist* jt_clist_append(jt_clist*);
extern jt_clist* jt_clist_prepend(jt_clist*);
extern void jt_clist_delinkitem(jt_clist*);
extern void jt_clist_moveitem(jt_clist* to, jt_clist* from);

#endif
