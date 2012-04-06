#include "small_bucket.h"

void* add_small() {
  pid_t pid = getpid();
  small_bucket* buck = get_small_bucket(pid);
  while (buck != NULL) {
    size_t mask = buck->mask;
    if (mask != (size_t)(-1)) {
      size_t free = 0;
      while (mask & (1 << free)) {
        ++free;
      }
      mask |= 1 << free;
      return buck->memory + memory_per_bit * free;
    }
    buck = buck->next;
  }
  small_bucket new_bucket = {
    .mask = 1,
    .memory = mmap(NULL, MAX_SMALL, PROT_READ | PROT_WRITE, MAP_ANONYMOUS, -1, 0),
    .next = NULL
  };
  add_small_bucket(pid, &new_bucket);
  return new_bucket.memory;
}

void free_small(void* ptr) {
  size_t sz = sizeof(size_t);
  pid_t pid = (pid_t)(size_t)*(ptr - 2 * sz);
  small_bucket* buck = get_small_bucket(pid, 1);
  size_t busy_bits = (size_t)*(ptr - sz);
  while (buck != NULL) {
    if (ptr >= buck->memory && ptr < buck->memory + MAX_SMALL) {
      size_t lo = (size_t)(ptr - buck_memory) / memory_per_bit;
      size_t hi = lo + busy_bits;
      buck->mask &= ~((1 << hi) - (1 << lo));
      return;
    }
    buck = buck->next;
  }
}
