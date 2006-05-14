/*
**    Name: list.c
**
**    Date: Sat Jun 27 15:24:02 1998
**
*/

#include <stdlib.h>

#include "cnew.h"
#include "list.h"

jt_list* jt_list_add(jt_list** oldhead)
{
  jt_list* item = jt_new(jt_list);
  item->data = 0;
  item->prev = *oldhead;
  item->next = 0;
  if (*oldhead) (*oldhead)->next = item;

  return *oldhead = item;
}

void jt_list_removehead(jt_list** head)
{
  jt_list* prev = (*head)->prev;
  if (*head) jt_delete(*head);
  *head = prev;
}

jt_list* jt_list_insertitem(jt_list** head, jt_list* before)
{
  jt_list* item = jt_new(jt_list);
  if (!before)
  {
    item->next = item->prev = 0;
    *head = item;
    return item;
  }
  if (before->prev) before->prev->next = item;
  before->next = item;
  item->prev = before->prev;
  item->next = before;
  return item;
}

// Delinks an item without freeing its data pointer */
void jt_list_delinkitem(jt_list** head, jt_list* item)
{
  if (!item) return;
  if (item->prev) item->prev->next = item->next;
  if (item->next) item->next->prev = item->prev;
  if (*head == item) *head = item->prev;
  jt_delete(item);
}

jt_list* jt_list_itemfromdata(jt_list* li, void* data)
{
  for (; li; li = li->prev) if (li->data == data) return li;

  return 0;
}

void jt_list_destroy(jt_list* li)
{
  while (li) jt_list_removehead(&li);
}

jt_list* jt_list_nthitem(jt_list* li, int item)
{
  while (item>0)
  {
    li = li->prev;
    if (!li) return 0;   // return zero if item not in list
    item--;
  }
  return li;
}

uint5 jt_list_length(jt_list* head)
{
  int count = 0;

  while (head)
  {
    count++;
    head = head->prev;
  }
  return count;
}

jt_list* jt_list_end(jt_list* head)
{
  for (; head && head->prev; head=head->prev);
  return head;
}

jt_list* jt_list_search(jt_list* head, jt_listsearch_fn srch, void* data)
{
  for (; head; head=head->prev)
  {
    if (srch(head, data)) return head;
  }

  return 0;
}

jt_list* jt_list_reverse(jt_list* in)
{
  jt_list* walk;
  jt_list* new_end = 0;

  /* reverse of empty list is empty list */
  if (!in) return 0;

  for (walk=in; walk;)
  {
    jt_list* prev = walk->prev;
    jt_list* next = walk->next;
    walk->prev = next;
    walk->next = prev;
    /* before overwriting walk with end-of-list marker, remember
       the last element
    */
    if (!prev) new_end = walk;
    walk = prev;
  }
  
  return new_end;
}
