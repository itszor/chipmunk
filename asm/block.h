#ifndef BLOCK_H
#define BLOCK_H 1

#include <bset.h>

#include "format.h"
#include "parsedefs.h"

// needs much refinement
typedef union {
  struct {
    uint5 data      : 1;
    uint5 transfer  : 3;
    uint5 funcstart : 1;
    uint5 funcend   : 1;
    uint5 loopstart : 1;
    uint5 loopend   : 1;
    uint5 reg       : 6;
  } bits;
  uint5 flat;
} block_type_bits;

typedef struct {
  uint5 type;
  uint5 offset;
  uint5 refcount;
} block_ref;

typedef struct block_info_t {
  block_type_bits type;
  union {
    fmt_inst* inst;
    uint3* data;
  } base;
  uint5 length;
  uint5 size;
  char* name;
  uint5 number;
  jt_bset* liveonentry;
  jt_bset* liveonexit;
  struct block_info_t* x;
  char* x_name;
  struct block_info_t* y;
  char* y_name;
  block_ref* ref;
} block_info;

typedef enum {
  tfer_FORK,
  tfer_CALL,
  tfer_JUMP,
  tfer_CALLIND,
  tfer_JUMPIND,
  tfer_RET,
  tfer_FREE
} block_transfer_type;

extern block_info* block_new(void);
block_info* block_appendi(block_info* block, fmt_inst inst);
block_info* block_appendd(block_info* block, datum_info* data);

#endif

