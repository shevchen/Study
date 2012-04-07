#include <string.h>
#include "small_bucket.c"
#include "large_bucket.c"

void* malloc(size_t size) {
  if (size == 0) {
    return NULL;
  }
  if (size > getpagesize() / (sizeof(size_t) * 8)) {
    return add_large(size);
  }
  return add_small();
}

void* calloc(size_t elems, size_t bytes_each) {
  size_t bytes = elems * bytes_each;
  void* ptr = malloc(bytes);
  return memset(ptr, (int)0, bytes);
}

void free(void* ptr) {
  if (ptr != NULL) {
    small_bucket* buck = find_small(ptr);
    if (buck == NULL) {
      free_large(ptr);
    } else {
      free_small(buck, ptr);
    }
  }
}

void* realloc(void* old, size_t size) {
  void* ptr = malloc(size);
  if (old != NULL) {
    size_t old_size = get_size(old);
    if (ptr != NULL) {
      memcpy(ptr, old, old_size < size ? old_size : size);
    }
    free(old);
  }
  return ptr;
}
