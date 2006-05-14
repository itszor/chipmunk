#include <string.h>

#include "map.h"

jt_map* jt_map_newnode(char* name, void* value)
{
  jt_map* node = jt_new(jt_map);
  node->name = name;
  node->value = value;
  node->left = 0;
  node->right = 0;
  return node;
}

jt_map* jt_map_insert(jt_map** root, char* name, void* value)
{
  if (!*root)
    return *root = jt_map_newnode(name, value);
  
  if (strcmp(name, (*root)->name)<0)
  {
    if ((*root)->left)
      return jt_map_insert(&(*root)->left, name, value);
    else
      return (*root)->left = jt_map_newnode(name, value);
  }
  else
  {
    if ((*root)->right)
      return jt_map_insert(&(*root)->right, name, value);
    else
      return (*root)->right = jt_map_newnode(name, value);
  }
  
  return 0;
}

jt_map* jt_map_lookup(jt_map* node, char* name)
{
  sint5 cmp;

  if (!node || !name) return 0;
  
  cmp = strcmp(name, node->name);
  
  if (cmp == 0) return node;
    
  if (cmp < 0) return jt_map_lookup(node->left, name);
    else return jt_map_lookup(node->right, name);
}

/* return data ptr for item called "name", or zero if not there */
void* jt_map_find(jt_map* node, char* name)
{
  jt_map* mentry = jt_map_lookup(node, name);
  
  if (!mentry) return 0;
  
  return mentry->value;
}

void jt_map_forall(jt_map* root, jt_map_iterfn iterfn, void* data)
{
  if (!root) return;
  
  if (root->left) jt_map_forall(root->left, iterfn, data);
  if (root->right) jt_map_forall(root->right, iterfn, data);
  
  iterfn(root, data);
}
