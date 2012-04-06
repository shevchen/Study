#include <sys/types.h>
#include <sys/mman.h>
#include <unistd.h>
#include "map.c"

static large_bucket* global_buckets = NULL;

void* get_from_global(size_t pages) {
  void* ptr = try_alloc(&global_buckets, pages);
  if (ptr == NULL) {
    ptr = mmap(NULL, pages * getpagesize(), PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  }
  return ptr;
}

void* add_large(size_t size) {
  int PAGE_SIZE = getpagesize();
  size_t sz = sizeof(size_t);
  size_t pages = (size + sz + PAGE_SIZE - 1) / PAGE_SIZE;
  pid_t pid = getpid();
  large_bucket* buckets = get_large_buckets(pid);
  void* ptr = try_alloc(&buckets, pages);
  if (ptr == NULL) {
    ptr = get_from_global(pages);
  }
  *(size_t*)ptr = pages;
  return ptr + sz;
}

void free_large(void* ptr) {
  size_t sz = sizeof(size_t);
  size_t pages = *(size_t*)(ptr - sz);
  large_bucket* new_bucket = (large_bucket*)get_memory(sizeof(large_bucket));
  new_bucket->memory = ptr - sz;
  new_bucket->pages = pages;
  add_large_bucket(getpid(), new_bucket);
}
