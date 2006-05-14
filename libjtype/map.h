#ifndef JT_MAP_H
#define JT_MAP_H 1

#include "defs.h"
#include "cnew.h"

typedef struct jt_map_t {
  char* name;
  void* value;
  struct jt_map_t* left;
  struct jt_map_t* right;
} jt_map;

typedef void (*jt_map_iterfn)(jt_map* node, void* data);

extern jt_map* jt_map_newnode(char* name, void* value);

extern jt_map* jt_map_insert(jt_map** root, char* name, void* value);

extern jt_map* jt_map_lookup(jt_map* node, char* name);

extern void* jt_map_find(jt_map* node, char* name);

extern void jt_map_forall(jt_map* root, jt_map_iterfn iterfn, void* data);

#endif
