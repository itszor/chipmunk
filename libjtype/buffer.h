#ifndef JT_BUFFER_H
#define JT_BUFFER_H 1

#include "defs.h"

typedef struct {
  void* buffer;
  uint5 length;
  uint5 size;
} jt_buffer;

#define buffer_IDX(B,T,N) \
  &(((T*)(B)->buffer)[(N)])

extern jt_buffer* jt_buffer_new(uint5 init);

extern void jt_buffer_delete(jt_buffer* buf);

extern uint5 jt_buffer_append(jt_buffer* buf, void* data, uint5 len);

extern void jt_buffer_extend(jt_buffer* buf, uint5 newlength);

#endif
