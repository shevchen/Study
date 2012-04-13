#include <sys/types.h>
#include <sys/mman.h>
#include <unistd.h>
#include "large_func.h"
#include "large_bucket.h"

void* add_large(size_t length) {
  length += sizeof(size_t);
  void* ptr = local_alloc(length);
  if (ptr == NULL) {
    ptr = get_from_global(length);
  }
  *(size_t*)ptr = length;
  return ptr + sizeof(size_t);
}

void free_large(void* ptr) {
  size_t sz = sizeof(size_t);
  size_t length = *(size_t*)(ptr - sz);
  large_bucket* new_bucket = (large_bucket*)get_memory(sizeof(large_bucket));
  pid_t pid = getpid();
  new_bucket->memory = ptr - sz;
  new_bucket->length = length;
  release_free_large(pid, new_bucket);
}
