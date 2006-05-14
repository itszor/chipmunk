#include <assert.h>

#include "cnew.h"
#include "pqueue.h"

// Priority queue handling: algorithmi from The White Book (naturally)

jt_pqueue* jt_pqueue_new(void)
{
  jt_pqueue* pq = jt_new(jt_pqueue);
  pq->item = jt_newarray(jt_pqueueitem*, 4);
  pq->length = 0;
  pq->size = 4;
  
  return pq;
}

void jt_pqueue_delete(jt_pqueue* pq)
{
  // free things pointed to by pq->data[n]->item...
  jt_delete(pq->item);
  jt_delete(pq);
}

jt_pqueueitem* jt_pqueue_newitem(uint5 priority)
{
  jt_pqueueitem* pqi = jt_new(jt_pqueueitem);
  
  pqi->priority = priority;
  pqi->data = 0;
  
  return pqi;
}

void jt_pqueue_deleteitem(jt_pqueueitem* pqi)
{
  jt_delete(pqi);
}

jt_pqueueitem* jt_pqueue_insert(jt_pqueue* pq, uint5 priority)
{
  sint5 i;
  
  if (++pq->length==pq->size)
    pq->item = realloc(pq->item, sizeof(jt_pqueueitem*) * (pq->size*=2));
  
  i = pq->length-1;

  while (i>0 && pq->item[pq_PARENT(i)]->priority > priority)
  {
    pq->item[i] = pq->item[pq_PARENT(i)];
    i = pq_PARENT(i);
  }
  
  return pq->item[i] = jt_pqueue_newitem(priority);
}

jt_pqueueitem* jt_pqueue_head(jt_pqueue* pq)
{
  return pq->length ? pq->item[0] : 0;
}

// you're expected to deallocate the item yourself after calling this
jt_pqueueitem* jt_pqueue_extract(jt_pqueue* pq)
{
  jt_pqueueitem* max;

  if (!pq->length) return 0;
  
  max = pq->item[0];
  pq->item[0] = pq->item[--pq->length];
  jt_pqueue_heapify(pq, 0);
  
  return max;
}

void jt_pqueue_heapify(jt_pqueue* pq, uint5 i)
{
  uint5 l = pq_LEFT(i), r = pq_RIGHT(i), largest;

  largest = (l<pq->length && pq->item[l]->priority<pq->item[i]->priority)
              ? l : i;

  if (r<pq->length && pq->item[r]->priority<pq->item[largest]->priority)
    largest = r;

  if (largest != i)
  {
    jt_pqueueitem* temp = pq->item[i];
    pq->item[i] = pq->item[largest];
    pq->item[largest] = temp;
    jt_pqueue_heapify(pq, largest);
  }
}

