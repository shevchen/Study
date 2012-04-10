#include <unistd.h>
#include <sys/mman.h>
#include <pthread.h>
#include "small_func.h"
#include "large_func.h"
#include "map.h"

static void* free_page = NULL;
static size_t free_size = 0;
static pthread_mutex_t local_mutex = PTHREAD_MUTEX_INITIALIZER;

void* get_memory(size_t len) {
  // only for little local segments
  pthread_mutex_lock(&local_mutex);
  if (free_page == NULL || free_size < len) {
    free_size = getpagesize();
    free_page = mmap(NULL, free_size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  }
  free_size -= len;
  void* ptr = free_page;
  free_page += len;
  pthread_mutex_unlock(&local_mutex);
  return ptr;
}

static size_t get_hash(size_t n) {
  return (A * n + B) % MOD;
}

static bucket_list* map[MOD] = {NULL};
static pthread_mutex_t main_mutex[MOD] = {PTHREAD_MUTEX_INITIALIZER};

bucket_list* get_all_buckets(pid_t pid) {
  size_t hash = get_hash(pid);
  bucket_list* list = map[hash];
  while (list != NULL) {
    if (list->pid == pid) {
      return list;
    }
    list = list->next;
  }
  bucket_list* new_list = (bucket_list*)get_memory(sizeof(bucket_list));
  new_list->pid = pid;
  new_list->total_memory = 0;
  pthread_mutex_init(&new_list->small_mutex, NULL);
  pthread_mutex_init(&new_list->large_mutex, NULL);
  pthread_mutex_lock(&main_mutex[hash]);
  new_list->next = map[hash];
  map[hash] = new_list;
  pthread_mutex_unlock(&main_mutex[hash]);
  return new_list;
}

size_t get_size(void* ptr) {
  size_t ps = getpagesize();
  small_bucket* small = find_small(ptr);
  if (small == NULL) {
    return *(size_t*)(ptr - sizeof(size_t));
  }
  return getpagesize() / (sizeof(size_t) * 8);
}
