#include <sys/types.h>
#include <sys/mman.h>
#include <unistd.h>
#include <stdio.h>
#include "map.h"
#include "large_bucket.h"

void* add_large(size_t size) {
  int PAGE_SIZE = getpagesize();
  size_t sz = sizeof(size_t);
  size_t pages = (size + sz + PAGE_SIZE - 1) / PAGE_SIZE;
  pid_t pid = getpid();
  large_bucket** buckets_addr = get_large_buckets_addr(pid);
  void* ptr = try_alloc(buckets_addr, pages);
  if (ptr == NULL) {
    ptr = get_from_global(pages);
  }
  printf("Large bucket of size %d allocated at %x in thread %d\n", pages * PAGE_SIZE, (size_t)ptr, pid);
  *(size_t*)ptr = pages;
  return ptr + sz;
}

void free_large(void* ptr) {
  size_t sz = sizeof(size_t);
  size_t pages = *(size_t*)(ptr - sz);
  large_bucket* new_bucket = (large_bucket*)get_memory(sizeof(large_bucket));
  pid_t pid = getpid();
  printf("Large bucket of size %d freed at %x in thread %d\n", pages * getpagesize(), (size_t)(ptr - sz), pid);
  new_bucket->memory = ptr - sz;
  new_bucket->pages = pages;
  release_large_bucket(pid, new_bucket);
}
