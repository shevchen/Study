#include <sys/types.h>
#include <sys/mman.h>
#include <unistd.h>
#include <stdio.h>
#include "map.h"
#include "large_bucket.h"

void* add_large(size_t length) {
  void* ptr = local_alloc(length);
  if (ptr == NULL) {
    ptr = get_from_global(length);
  }
  printf("Large bucket of size %d allocated at %x in thread %d\n", length, (size_t)ptr, getpid());
  *(size_t*)ptr = length;
  return ptr + sizeof(size_t);
}

void free_large(void* ptr) {
  size_t sz = sizeof(size_t);
  size_t length = *(size_t*)(ptr - sz);
  large_bucket* new_bucket = (large_bucket*)get_memory(sizeof(large_bucket));
  pid_t pid = getpid();
  printf("Large bucket of size %d freed at %x in thread %d\n", length, (size_t)(ptr - sz), pid);
  new_bucket->memory = ptr - sz;
  new_bucket->length = length;
  release_free(pid, new_bucket);
}
