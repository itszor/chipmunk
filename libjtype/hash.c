#include <stdio.h>
#include <math.h>

#include "cnew.h"
#include "hash.h"

jt_hash* jt_hash_new(uint5 size)
{
  jt_hash* htab = jt_new(jt_hash);
	uint5 i;
	
	htab->entries = 0;
	size = htab->size = 1<<(int)(log((float)size)/log(2)+1.0);
	htab->table = jt_newarray(jt_list*, size);
	
	for (i=0; i<size; i++)
	{
	  htab->table[i] = 0;
	}
	
	return htab;
}

// erm; any old nonsense really
#define HASHFN(K,S) (((K)*17011)&((S)-1))

jt_hashentry* jt_hash_insert(jt_hash* hash, uint5 key)
{
  jt_hashentry* entry = 0;
	uint5 loc = HASHFN(key, hash->size);
	jt_list* item, *theitem=0;
	
	for (item=hash->table[loc]; item; item=item->prev)
	{
	  jt_hashentry* h = (jt_hashentry*) item->data;

		if (h->key==key)
		{
		  theitem = item;
			entry = item->data;
	    break;
		}
	}
	
	if (!theitem)
	{
	  theitem = jt_list_add(&hash->table[loc]);
  	theitem->data = entry = jt_new(jt_hashentry);
  	entry->key = key;
  	entry->data = 0;
		hash->entries++;
	}

	return entry;
}

// this gives you back your data pointer, cos you have to free that yourself
void* jt_hash_remove(jt_hash* hash, uint5 key)
{
  uint5 loc = HASHFN(key, hash->size);
  jt_list* item;
	void* hashentrydata=0;
	
	for (item=hash->table[loc]; item;)
	{
	  jt_list* prev = item->prev;
		jt_hashentry* h = (jt_hashentry*) item->data;

		if (h->key==key)
		{
		  hashentrydata = h->data;
		  jt_delete(h);
		  jt_list_delinkitem(&hash->table[loc], item);
			hash->entries--;
		}
		
		item = prev;
	}
	
	return hashentrydata;
}

// delete an entry, calling a destructor function
void jt_hash_delete(jt_hash* hash, uint5 key, jt_hashdestructor_fn destructor)
{
  void* data = jt_hash_remove(hash, key);
	destructor(data);
}

// destroy an entire hash table, plus all its contents
void jt_hash_nuke(jt_hash* hash, jt_hashdestructor_fn destructor)
{
	uint5 i;
	
	for (i=0; i<hash->size; i++)
	{
	  while (hash->table[i])
		{
		  if (destructor) destructor(hash->table[i]->data);
			jt_list_removehead(&hash->table[i]);
		}
	}

	free(hash->table);
	free(hash);
}

jt_hashentry* jt_hash_lookup(jt_hash* hash, uint5 key)
{
	uint5 loc = HASHFN(key, hash->size);
	jt_list* item;
	
	for (item=hash->table[loc]; item; item=item->prev)
	{
	  jt_hashentry* h = (jt_hashentry*) item->data;
		if (h->key==key) return h;
	}

	return 0;
}

/* get data from key */
void* jt_hash_find(jt_hash* hash, uint5 key)
{
  jt_hashentry* hentry = jt_hash_lookup(hash, key);

  if (!hentry) return 0;
  
  return hentry->data;
}
