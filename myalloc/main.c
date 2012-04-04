void* malloc(size_t size) {
  if (size == 0) {
    return NULL;
  }
  if (size <= max_small_size) {
    return add_small(size);
  } else {
    return add_large(size);
  }
}

void* calloc(size_t elems, size_t bytes_each) {
  size_t bytes = elems * bytes_each;
  void* ptr = malloc(bytes);
  return memset(ptr, 0, bytes);
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

void free(void* ptr) {
  if (ptr != NULL) {
    size_t size = get_size(ptr);
    if (size <= max_small_size) {
      free_small(ptr);
    } else {
      free_large(ptr);
    }
  }
}
