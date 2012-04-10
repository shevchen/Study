#ifndef _LARGE_FUNC_
#define _LARGE_FUNC_

#include "large_bucket.h"
#include "map.h"

void release_free_large(pid_t, large_bucket*);
void* try_alloc(large_bucket**, size_t, bucket_list*);
void* local_alloc(size_t);
void* get_from_global(size_t);

#endif
