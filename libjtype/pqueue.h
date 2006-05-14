#ifndef JT_PQUEUE_H
#define JT_PQUEUE_H 1

#include "defs.h"

typedef struct {
  void* data;
  uint5 priority;
} jt_pqueueitem;

typedef struct {
  jt_pqueueitem** item;
  uint5 length, size;
} jt_pqueue;

// Not particularly tidy, because we're counting from zero
#define pq_LEFT(X) ((((X)+1)<<1)-1)
#define pq_RIGHT(X) (((X)+1)<<1)
#define pq_PARENT(X) ((((X)+1)>>1)-1)

extern jt_pqueue* jt_pqueue_new(void);

extern void jt_pqueue_delete(jt_pqueue* pq);

extern jt_pqueueitem* jt_pqueue_newitem(uint5 key);

extern void jt_pqueue_deleteitem(jt_pqueueitem* it);

extern jt_pqueueitem* jt_pqueue_insert(jt_pqueue* pq, uint5 priority);

extern jt_pqueueitem* jt_pqueue_head(jt_pqueue* pq);

extern jt_pqueueitem* jt_pqueue_extract(jt_pqueue* pq);

extern void jt_pqueue_heapify(jt_pqueue* pq, uint5 i);

#endif
