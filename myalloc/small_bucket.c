#include <sys/types.h>
#include <sys/mman.h>
#include <unistd.h>
#include <stdio.h>
#include "map.h"

void* add_small() {
  pid_t pid = getpid();
  void* ptr = add_to_small(pid);
  printf("Small bucket allocated at %x in thread %d\n", (size_t)ptr, pid);
  return ptr;
}

void free_small(small_bucket* buck, void* ptr) {
  size_t ps = getpagesize();
  size_t sz = sizeof(size_t);
  size_t maskN = (size_t)ptr % (ps * SMALL_BUCKET_PAGES) / ps;
  size_t bit = (size_t)ptr % ps * (sizeof(size_t) * 8) / ps;
  buck->mask[maskN] ^= 1 << bit;
  printf("Small bucket freed at %x in thread %d\n", (size_t)ptr, getpid());
}
