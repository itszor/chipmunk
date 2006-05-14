/*
**    Name: list.h
**
**    Date: Wed Sep 16 23:20:08 1998
**
*/

#ifndef JT_LIST_H
#define JT_LIST_H 1

#include "defs.h"

typedef struct jt_list_t {
  void* data;
  struct jt_list_t* prev;
  struct jt_list_t* next;
} jt_list;

typedef jt_list* (*jt_listsearch_fn)(jt_list*, void*);

extern jt_list* jt_list_add(jt_list** oldhead);

extern void jt_list_removehead(jt_list** head);

extern jt_list* jt_list_insertitem(jt_list** head, jt_list* before);

extern void jt_list_delinkitem(jt_list** head, jt_list* item);

extern jt_list* jt_list_itemfromdata(jt_list* li, void* data);

extern void jt_list_destroy(jt_list* li);

extern jt_list* jt_list_nthitem(jt_list* li, int item);

extern uint5 jt_list_length(jt_list* head);

extern jt_list* jt_list_end(jt_list* head);

extern jt_list* jt_list_search(jt_list* head, jt_listsearch_fn srch,
                               void* data);

extern jt_list* jt_list_reverse(jt_list*);

#endif
