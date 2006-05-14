#include <string.h>

#include "cnew.h"
#include "buffer.h"

jt_buffer* jt_buffer_new(uint5 init)
{
  jt_buffer* buf = jt_new(jt_buffer);
  
  buf->buffer = jt_newarray(char, init);
  buf->length = 0;
  buf->size = init;
  
  return buf;
}

void jt_buffer_delete(jt_buffer* buf)
{
  jt_delete(buf->buffer);
  jt_delete(buf);
}

uint5 jt_buffer_append(jt_buffer* buf, void* data, uint5 len)
{
  uint5 newlength = buf->length + len;
  
  while (newlength >= buf->size)
    buf->buffer = realloc(buf->buffer, buf->size *= 2);
  
  memcpy(&((char*)buf->buffer)[buf->length], data, len);
  
  buf->length = newlength;
  
  return buf->length - len;
}

void jt_buffer_extend(jt_buffer* buf, uint5 len)
{
  uint5 oldlen = buf->length;
  if (len < buf->size)
  {
    buf->length = len;
    return;
  }
  
  buf->buffer = realloc(buf->buffer, buf->size = len);
  
  memset(&((char*)buf->buffer)[oldlen], 0, len-oldlen);
  
  buf->length = buf->size = len;
}
