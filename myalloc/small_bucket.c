#include <unistd.h>
#include <pthread.h>
#include "small_func.h"

void* add_small() {
  return add_another_small(getpid());
}

void free_small(void* ptr, small_bucket* buck) {
  size_t ps = getpagesize();
  size_t sz = sizeof(size_t);
  size_t maskN = (size_t)ptr % (ps * SMALL_BUCKET_PAGES) / ps;
  size_t bit = (size_t)ptr % ps * (sizeof(size_t) * 8) / ps;
  pthread_mutex_lock(&buck->mutex);
  buck->mask[maskN] ^= 1 << bit;
  pthread_mutex_unlock(&buck->mutex);
}

small_bucket* get_small(void* ptr) {
  return find_small(ptr);
}

size_t max_small_size() {
  return getpagesize() / (sizeof(size_t) * 8);
}
