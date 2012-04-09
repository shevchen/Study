#ifndef _MAP_H_
#define _MAP_H_

#include <unistd.h>
#include "small_bucket.h"
#include "large_bucket.h"

#define A 239
#define B 366
#define MOD 32768

typedef struct bucket_list {
  pid_t pid;
  size_t total_memory;
  small_bucket* small;
  large_bucket* large;
  struct bucket_list* next;
} bucket_list;

typedef struct small_allocs {
  small_bucket* bucket;
  size_t page_addr;
  struct small_allocs* next;
} small_allocs;

void* get_memory(size_t);
small_bucket* find_small(void*);
void add_small_bucket_mem(small_bucket*, size_t);
void* try_alloc(large_bucket**, size_t);
void* get_from_global(size_t);
small_bucket* get_small_buckets(pid_t);
large_bucket** get_large_buckets_addr(pid_t);
void release_large_bucket(pid_t, large_bucket*);
size_t get_size(void*);

#endif
