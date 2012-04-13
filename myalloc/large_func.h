#ifndef _LARGE_FUNC_
#define _LARGE_FUNC_

#include "large_bucket.h"

void release_free_large(pid_t, large_bucket*);
void* local_alloc(size_t);
void* get_from_global(size_t);

#endif
