#include <sys/types.h>
#include <sys/mman.h>
#include <unistd.h>
#include "map.c"
#include <stdio.h>

void* get_aligned_memory(size_t len) {
  void* not_aligned = mmap(NULL, 2 * len, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0),
  void* to = not_aligned + 2 * len - not_aligned % len;
  void* from = to - len;
  munmap(not_aligned, from - not_aligned);
  munmap(to, not_aligned + 2 * len - to);
  return from;
}

void* add_small() {
  pid_t pid = getpid();
  small_bucket* buck = get_small_buckets(pid);
  small_bucket** copy = &buck;
  while (buck != NULL) {
    int i;
    for (i = 0; i < SMALL_BUCKET_PAGES; ++i) {
      size_t mask = buck->mask[i];
      if (mask != (size_t)(-1)) {
        size_t free = 0;
        while (mask & (1 << free)) {
          ++free;
        }
        buck->mask[i] |= 1 << free;
        void* ptr = buck->memory + getpagesize() / sizeof(size_t) * free;
        printf("Small bucket allocated at %x in thread %d\n", (size_t)ptr, pid);
        return ptr;
      }
    }
    buck = buck->next;
  }
  small_bucket* new_bucket = (small_bucket*)get_memory(sizeof(small_bucket));
  new_bucket->mask[0] = 1;
  new_bucket->memory = get_aligned_memory(SMALL_BUCKET_PAGES * getpagesize());
  set_small_bucket_by_memory(new_bucket, new_bucket->memory);
  new_bucket->next = *copy;
  *copy = new_bucket;
  printf("Small bucket allocated at %x\n", (size_t)new_bucket->memory);
  return new_bucket->memory;
}

void free_small(void* ptr) {
  small_bucket* buck = get_small_bucket_by_memory(ptr);
  size_t ps = getpagesize();
  size_t sz = sizeof(size_t);
  size_t maskN = ptr / ps * sz / SMALL_BUCKET_PAGES;
  size_t bit = ptr % ps * sizeof(size_t) / ps;
  buck->mask[maskN] ^= 1 << bit;
  printf("Small bucket freed at %x in thread %d\n", (size_t)ptr, getpid());
}
