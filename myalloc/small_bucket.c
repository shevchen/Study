#include <sys/types.h>
#include <sys/mman.h>
#include <unistd.h>
#include "map.c"
#include <stdio.h>

void* add_small() {
  pid_t pid = getpid();
  small_bucket* buck = get_small_buckets(pid);
  small_bucket** copy = &buck;
  while (buck != NULL) {
    size_t mask = buck->mask;
    if (mask != (size_t)(-1)) {
      size_t free = 0;
      while (mask & (1 << free)) {
        ++free;
      }
      mask |= 1 << free;
      void* ptr = buck->memory + SMALL_BUCKET_PAGES * getpagesize() / sizeof(size_t) * free;
      printf("Small bucket allocated at %x\n", (size_t)ptr);
      return ptr;
    }
    buck = buck->next;
  }
  small_bucket* new_bucket = (small_bucket*)get_memory(sizeof(small_bucket));
  new_bucket->mask = 1;
  new_bucket->memory = mmap(NULL, SMALL_BUCKET_PAGES * getpagesize(), PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0),
  set_small_bucket_by_memory(new_bucket, new_bucket->memory);
  new_bucket->next = *copy;
  *copy = new_bucket;
  return new_bucket->memory;
}

void free_small(void* ptr) {
  small_bucket* buck = get_small_bucket_by_memory(ptr);
  size_t ps = SMALL_BUCKET_PAGES * getpagesize();
  size_t bit = buck->memory % ps * sizeof(size_t) / ps;
  buck->mask ^= 1 << bit;
  printf("Small bucket freed at %x\n", (size_t)ptr);
}
