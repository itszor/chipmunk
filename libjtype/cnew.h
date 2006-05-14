/*
**    Name: allocv.h
**
**    Date: Mon Jul  6 17:26:58 1998
**
*/

#ifndef JT_CNEW_H
#define JT_CNEW_H 1

#include <stdlib.h>

#define jt_new(n) jt_safemalloc(sizeof(n))
#define jt_newarray(n, s) jt_safecalloc(sizeof(n), s)
#define jt_delete(n) free(n)

extern void* jt_safemalloc(size_t bytes);
extern void* jt_safecalloc(size_t bytes, size_t num);

#endif
