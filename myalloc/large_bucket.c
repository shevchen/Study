#define MIN_LEAVE 4096

typedef struct {
  void* memory;
  size_t size;
  large_bucket* next;
} large_bucket;

void* add_large(size_t size) {
  pid_t pid = getpid();
  size_t sz = sizeof(size_t);
  large_bucket* buck = get_large_bucket(pid);
  large_bucket* last = NULL;
  while (buck != NULL) {
    if (buck->size >= size) {
      buck->memory[0] = (size_t)pid;
      buck->memory[sz] = size;
      size_t memory_left = buck->size - size;
      if (memory_left >= MIN_LEAVE) {
        buck->memory += size;
        buck->size -= size;
        return buck->memory - size + 2 * sz;
      }
      munmap(buck->memory + size, memory_left);
      if (last != NULL) {
        last->next = buck->next;
      } else {
        remove_large_bucket(pid);
      }
      return buck_memory + 2 * sz;
    }
    last = buck;
    buck = buck->next;
  }
  return get_from_global(size);
}

void free_large(void* ptr) {
  size_t sz = sizeof(size_t);
  size_t size = *(ptr - sz);
  large_bucket new_bucket = {ptr - 2 * sz, size, NULL};
  add_large_bucket(getpid(), &new_bucket);
}
