#include "small_bucket.h"

void* add_small(size_t size) {
  pid_t pid = getpid();
  size_t need_bits = (size + memory_per_bit - 1) / memory_per_bit;
  size_t sz = sizeof(size_t);
  small_bucket* buck = get_small_bucket(pid);
  while (buck != NULL) {
    size_t mask = buck->mask;
    for (size_t i = 0; i <= BITS - need_bits; ++i) {
      if (!(mask & ((1 << need_bits) - 1))) {
        mask |= (1 << (i + need_bits)) - (1 << i);
        buck->memory[i] = (size_t)pid;
        buck->memory[i + sz] = need_bits;
        return buck->memory + i * memory_per_bit + 2 * sz;
      }
    }
    buck = buck->next;
  }
  small_bucket new_bucket = {(1 << need_bits) - 1, mmap(NULL, MAX_SMALL, PROT_READ | PROT_WRITE, MAP_ANONYMOUS, -1, 0), NULL};
  new_bucket.memory[0] = (size_t)pid;
  new_bucket.memory[sz] = size - 2 * sz;
  add_small_bucket(pid, &new_bucket);
  return new_bucket.memory + 2 * sz;
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
