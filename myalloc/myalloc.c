#include <string.h>
#include <signal.h>
#include "small_bucket.h"
#include "large_bucket.h"

void* malloc(size_t size) {
  if (size == 0) {
    return NULL;
  }
  if (size <= max_small_size()) {
    return add_small();
  }
  size += sizeof(size_t);
  return add_large(size);
}

void* calloc(size_t elems, size_t bytes_each) {
  size_t bytes = elems * bytes_each;
  void* ptr = malloc(bytes);
  return memset(ptr, (int)0, bytes);
}

void free(void* ptr) {
  if (ptr != NULL) {
    small_bucket* small = get_small(ptr);
    if (small != NULL) {
      free_small(ptr, small);
    } else {
      free_large(ptr);
    }
  }
}

void* realloc(void* old, size_t size) {
  void* ptr = malloc(size);
  if (size > max_small_size()) {
    size += sizeof(size_t);
  }
  if (old != NULL) {
    size_t old_size = get_size(old);
    if (ptr != NULL) {
      memcpy(ptr, old, old_size < size ? old_size : size);
    }
    free(old);
  }
  return ptr;
}

void* memalign(size_t align, size_t size) {
  raise(SIGILL);
}

int posix_memalign(void** memptr, size_t align, size_t size) {
  raise(SIGILL);
}

void* valloc(size_t size) {
  raise(SIGILL);
}
