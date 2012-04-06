#include <sys/types.h>
#include <sys/mman.h>
#include <unistd.h>
#include "map.c"

static large_bucket* global_buckets = NULL;

void* get_from_global(size_t pages, pid_t pid) {
  void* ptr = try_alloc(&global_buckets, pages, pid);
  if (ptr == NULL) {
    ptr = mmap(NULL, pages * getpagesize(), PROT_READ | PROT_WRITE, MAP_ANONYMOUS, -1, 0);
  }
  return ptr;
}

void* add_large(size_t size) {
  int PAGE_SIZE = getpagesize();
  size_t pages = (size + 2 * sizeof(size_t) + PAGE_SIZE - 1) / PAGE_SIZE;
  pid_t pid = getpid();
  large_bucket* buckets = get_large_buckets(pid);
  void* ptr = try_alloc(&buckets, pages, pid);
  return ptr != NULL ? ptr : get_from_global(pages, pid);
}

void free_large(void* ptr) {
  size_t sz = sizeof(size_t);
  size_t pages = *((size_t*)ptr - sz);
  large_bucket new_bucket = {
    .memory = ptr - 2 * sz,
    .pages = pages,
    .next = NULL
  };
  add_large_bucket(getpid(), &new_bucket);
}
