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
  small_bucket* small;
  large_bucket* large;
  struct bucket_list* next;
} bucket_list;

size_t get_hash(size_t n) {
  return (A * n + B) % MOD;
}

static void* free_page = NULL;
static size_t free_size = 0;

void* get_memory(size_t len) {
  // only for little local segments
  if (free_page == NULL || free_size < len) {
    free_size = getpagesize();
    free_page = mmap(NULL, free_size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  }
  free_size -= len;
  void* ptr = free_page;
  free_page += len;
  return ptr;
}

typedef struct small_allocs {
  small_bucket* bucket;
  size_t page_addr;
  struct small_allocs* next;
} small_allocs;

#endif
