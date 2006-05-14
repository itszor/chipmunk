#ifndef JT_STRNG_H
#define JT_STRNG_H 1

/* hi-level string functionality using jtype's buffer */

#include <limits.h>

#include "defs.h"
#include "buffer.h"
#include "list.h"

typedef jt_buffer jt_strng;

#define STRNG_OPEN INT_MAX

extern jt_strng* jt_strng_new(const char* val);
extern void jt_strng_delete(jt_strng* foo);
extern jt_strng* jt_strng_append(jt_strng* orig, jt_strng* append);
extern jt_strng* jt_strng_append_c(jt_strng* orig, const char* app);
extern uint5 jt_strng_len(jt_strng* str);
extern char* jt_strng_c(jt_strng* str);
extern jt_strng* jt_strng_slice(jt_strng*, int, int);
extern jt_list* jt_strng_list_new(const char*, ...);
extern void jt_strng_list_delete(jt_list*);
extern jt_strng* jt_strng_list_join(jt_list*, const char*);
extern jt_list* jt_strng_list_split(jt_strng* mystr, const char*);

#endif
