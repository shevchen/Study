#ifndef _SMALL_FUNC_
#define _SMALL_FUNC_

#include "small_bucket.h"

typedef struct small_allocs {
  small_bucket* bucket;
  size_t page_addr;
  struct small_allocs* next;
} small_allocs;

small_bucket* find_small(void* ptr);
void* add_another_small(pid_t pid);

#endif
