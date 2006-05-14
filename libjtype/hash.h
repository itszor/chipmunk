#ifndef JT_HASH_H
#define JT_HASH_H 1

#include "defs.h"
#include "list.h"

typedef struct {
  uint5 key;
	void* data;
} jt_hashentry;

typedef struct {
  jt_list** table;
	uint5 entries;
	uint5 size;
} jt_hash;

typedef void (*jt_hashdestructor_fn)(void* e);

extern jt_hash* jt_hash_new(uint5 size);

extern jt_hashentry* jt_hash_insert(jt_hash* hash, uint5 key);

extern void* jt_hash_remove(jt_hash* hash, uint5 key);

extern void jt_hash_delete(jt_hash* hash, uint5 key, jt_hashdestructor_fn f);

extern void jt_hash_nuke(jt_hash* hash, jt_hashdestructor_fn destructor);

extern jt_hashentry* jt_hash_lookup(jt_hash* hash, uint5 key);

extern void* jt_hash_find(jt_hash* hash, uint5 key);

#endif
