/*
 *  Circular list handling functions
 */

#include <assert.h>

#include "cnew.h"
#include "defs.h"
#include "clist.h"

jt_clist* jt_clist_new(void)
{
  jt_clist* x = jt_new(jt_clist);
  x->data = 0;
  x->prev = x;
  x->next = x;
  return x;
}

jt_clist* jt_clist_append(jt_clist* sentry)
{
  jt_clist* n = jt_new(jt_clist), *after;
  n->data = 0;
  
  after = sentry->prev;
  after->next = n;
  n->prev = after;
  n->next = sentry;
  sentry->prev = n;
  
  return n;
}

jt_clist* jt_clist_prepend(jt_clist* sentry)
{
  jt_clist* n = jt_new(jt_clist), *before;
  n->data = 0;
  
  before = sentry->next;
  before->prev = n;
  n->next = before;
  n->prev = sentry;
  sentry->next = n;
  
  return n;
}

void jt_clist_delinkitem(jt_clist* entry)
{
  assert(entry->data != 0);
  entry->prev->next = entry->next;
  entry->next->prev = entry->prev;
  jt_delete(entry);
}

/* moves item 'from' after item 'to' */
void jt_clist_moveitem(jt_clist* to, jt_clist* from)
{
  /* delink 'from' from previous location */
  from->prev->next = from->next;
  from->next->prev = from->prev;
  /* tie 'from' into new location */
  from->next = to->next;
  from->prev = to;
  /* insert 'from' after 'to' */
  to->next->prev = from;
  to->next = from;
}


