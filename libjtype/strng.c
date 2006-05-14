#include <string.h>
#include <stdarg.h>
#include <stddef.h>

#include "cnew.h"
#include "strng.h"
#include "list.h"

jt_strng* jt_strng_new(const char* val)
{
  int len = strlen(val)+1;
  jt_strng* newstring = jt_buffer_new(len);

  strcpy(newstring->buffer, val);
  newstring->length = len;

  return newstring;
}

void jt_strng_delete(jt_strng* mystr)
{
  jt_delete(mystr->buffer);
  jt_delete(mystr);
}

jt_strng* jt_strng_append(jt_strng* orig, jt_strng* append)
{
  int newlength = orig->length + append->length - 1;
  int origlength = orig->length;
  
  jt_buffer_extend(orig, newlength);
  strcpy(&((char*)orig->buffer)[origlength-1], append->buffer);

  orig->length = newlength;

  return orig;
}

jt_strng* jt_strng_append_c(jt_strng* orig, const char* append)
{
  int applen = strlen(append)+1;
  int newlength = orig->length + applen - 1;
  int origlength = orig->length;
  
  jt_buffer_extend(orig, newlength);
  strcpy(&((char*)orig->buffer)[origlength-1], append);

  orig->length = newlength;
  
  return orig;
}

uint5 jt_strng_len(jt_strng* str)
{
  return str->length-1;
}

char* jt_strng_c(jt_strng* str)
{
  return str->buffer;
}

/* return a new strng, made from a slice of an old strng.
   Should obey python-style slicing semantics, eg:
   start & end are positions between characters.
   start & end can be negative, then they count from the end of the string.
   start & end can be STRNG_OPEN, then they count up to the end (or start).
*/
jt_strng* jt_strng_slice(jt_strng* in, int start, int end)
{
  int length = jt_strng_len(in), i;
  jt_strng* newstrng = 0;

  if (start==STRNG_OPEN) start = 0;
  if (end==STRNG_OPEN) end = length;
  
  if (start<0) start += length;
  if (end<0) end += length;
  
  if (start<0) start = 0;
  if (end>length) end = length;
  
  if (end <= start)
  {
    /* empty */
    return jt_strng_new("");
  }
  
  newstrng = jt_buffer_new(1+(end-start));
  for (i=start; i<end; i++)
  {
    ((char*)newstrng->buffer)[i-start] = ((char*)in->buffer)[i];
  }
  /* zero-terminate */
  ((char*)newstrng->buffer)[end-start] = '\0';

  newstrng->length = 1+(end-start);

  return newstrng;
}

/* construct a list of strings */
jt_list* jt_strng_list_new(const char* newstr, ...)
{
  va_list ap;
  jt_list* list = 0;
  
  jt_list_add(&list);
  list->data = jt_strng_new(newstr);
  
  va_start(ap, newstr);
  for (;;)
  {
    char* next = va_arg(ap, char*);
    if (!next) break;
    jt_list_add(&list);
    list->data = jt_strng_new(next);
  }
  va_end(ap);
  
  /* return list in 'natural' order */
  return jt_list_reverse(list);
}

/* delete a list of strings */
void jt_strng_list_delete(jt_list* list)
{
  while (list)
  {
    jt_strng_delete(list->data);
    jt_list_removehead(&list);
  }
}

/* join a list of strings, separated with 'joinwith' */
jt_strng* jt_strng_list_join(jt_list* strlist, const char* joinwith)
{
  jt_list* walk;
  jt_strng* accum = jt_strng_new("");
  
  for (walk=strlist; walk; walk=walk->prev)
  {
    jt_strng* thisstr = walk->data;
    jt_strng_append(accum, thisstr);
    if (walk->prev)
    {
      jt_strng_append_c(accum, joinwith);
    }
  }
  return accum;
}

/* split a list at any of [a set of] character */
jt_list* jt_strng_list_split(jt_strng* mystr, const char* splitat)
{
  jt_list* acc = 0;
  char* nextsplit;
  int runner = 0, done = 0;
  
  do
  {
    ptrdiff_t thislen;
    jt_strng* part = 0;
    nextsplit = strpbrk(&((char*)mystr->buffer)[runner], splitat);
    if (nextsplit != NULL)
    {
      /* WARNING pointer arithmetic -- how portable is this? */
      thislen = (char*)nextsplit - ((char*)mystr->buffer + runner);
      part = jt_strng_slice(mystr, runner, runner+thislen);
      /* +1 to skip over split char */
      runner += thislen+1;
    }
    else
    {
      part = jt_strng_slice(mystr, runner, STRNG_OPEN);
      done = 1;
    }

    jt_list_add(&acc);
    acc->data = part;
  } while (!done);
  
  return jt_list_reverse(acc);
}
