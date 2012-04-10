#ifndef _SMALL_FUNC_
#define _SMALL_FUNC_

#include "small_bucket.h"

typedef struct small_allocs {
  small_bucket* bucket;
  size_t page_addr;
  struct small_allocs* next;
} small_allocs;

small_bucket* find_small(void*);
void* add_to_small(pid_t);

#endif
