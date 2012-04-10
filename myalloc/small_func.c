#include <unistd.h>
#include <pthread.h>
#include <sys/mman.h>
#include "map.h"
#include "small_func.h"

static small_allocs* alloc_map[MOD] = {NULL};
static pthread_mutex_t alloc_mutex = PTHREAD_MUTEX_INITIALIZER;

small_bucket* find_small(void* ptr) {
  size_t page_addr = (size_t)ptr / (getpagesize() * SMALL_BUCKET_PAGES);
  small_allocs* allocs = alloc_map[get_hash(page_addr)];
  while (allocs != NULL) {
    if (allocs->page_addr == page_addr) {
      return allocs->bucket;
    }
    allocs = allocs->next;
  }
  return NULL;
}

static void add_small_bucket_mem(small_bucket* bucket, size_t page_addr) {
  size_t hash = get_hash(page_addr);
  small_allocs* new_alloc = (small_allocs*)get_memory(sizeof(small_allocs));
  new_alloc->bucket = bucket;
  new_alloc->page_addr = page_addr;
  pthread_mutex_lock(&alloc_mutex);
  new_alloc->next = alloc_map[hash];
  alloc_map[hash] = new_alloc;
  pthread_mutex_unlock(&alloc_mutex);
}

static void* get_aligned_memory(size_t len) {
  size_t not_aligned = (size_t)mmap(NULL, 2 * len, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  size_t to = not_aligned + 2 * len - not_aligned % len;
  size_t from = to - len;
  munmap((void*)not_aligned, from - not_aligned);
  munmap((void*)to, not_aligned + 2 * len - to);
  return (void*)from;
}

void* add_to_small(pid_t pid) {
  bucket_list* list = get_all_buckets(pid);
  small_bucket* buck = list->small;
  size_t ps = getpagesize();
  while (buck != NULL) {
    pthread_mutex_lock(&buck->mutex);
    int i;
    for (i = 0; i < SMALL_BUCKET_PAGES; ++i) {
      size_t mask = buck->mask[i];
      if (mask != (size_t)(-1)) {
        size_t free = 0;
        while (mask & (1 << free)) {
          ++free;
        }
        buck->mask[i] |= 1 << free;
        void* ptr = buck->memory + ps / (sizeof(size_t) * 8) * free;
        pthread_mutex_unlock(&buck->mutex);
        return ptr;
      }
    }
    pthread_mutex_unlock(&buck->mutex);
    buck = buck->next;
  }
  small_bucket* new_bucket = (small_bucket*)get_memory(sizeof(small_bucket));
  new_bucket->mask[0] = 1;
  new_bucket->memory = get_aligned_memory(SMALL_BUCKET_PAGES * ps);
  add_small_bucket_mem(new_bucket, (size_t)new_bucket->memory / (SMALL_BUCKET_PAGES * ps));
  pthread_mutex_lock(&list->small_mutex);
  new_bucket->next = list->small;
  list->small = new_bucket;
  pthread_mutex_unlock(&list->small_mutex);
  return new_bucket->memory;
}
