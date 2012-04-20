#ifndef _LARGE_FUNC_
#define _LARGE_FUNC_

#include "large_bucket.h"

void release_free_large(pid_t pid, large_bucket* new_bucket);
void* local_alloc(size_t length);
void* get_from_global(size_t length);

#endif
