#include <unistd.h>
#include <pthread.h>
#include <sys/mman.h>
#include "large_bucket.h"
#include "map.h"

static large_bucket* global_buckets = NULL;
static size_t global_total_memory = 0;
static pthread_mutex_t global_buckets_mutex = PTHREAD_MUTEX_INITIALIZER;

static void add_to_global(large_bucket* bucket) {
  pthread_mutex_lock(&global_buckets_mutex);
  bucket->next = global_buckets;
  global_buckets = bucket;
  global_total_memory += bucket->length;
  pthread_mutex_unlock(&global_buckets_mutex);
}

static void clear_global_memory() {
  pthread_mutex_lock(&global_buckets_mutex);
  while (global_total_memory > MAX_GLOBAL_MEMORY) {
    global_total_memory -= global_buckets->length;
    if ((size_t)global_buckets->memory % getpagesize() == 0) {
      munmap(global_buckets->memory, global_buckets->length);
    }
    global_buckets = global_buckets->next;
  }
  pthread_mutex_unlock(&global_buckets_mutex);
}

static void clear_local_memory(bucket_list* list) {
  // needs external lock
  while (list->total_memory > MAX_LOCAL_MEMORY) {
    large_bucket* first = list->large;
    large_bucket* next = first->next;
    list->total_memory -= first->length;
    add_to_global(first);
    list->large = next;
  }
  if (global_total_memory > 2 * MAX_GLOBAL_MEMORY) {
    clear_global_memory();
  }
}

static void release_large_bucket(pid_t pid, large_bucket* new_bucket, bucket_list* list) {
  // needs external lock
  new_bucket->next = list->large;
  list->large = new_bucket;
  list->total_memory += new_bucket->length;
  if (list->total_memory >= 2 * MAX_LOCAL_MEMORY) {
    clear_local_memory(list);
  }
}

void release_free_large(pid_t pid, large_bucket* new_bucket) {
  bucket_list* list = get_all_buckets(pid);
  pthread_mutex_lock(&list->large_mutex);
  release_large_bucket(pid, new_bucket, list);
  pthread_mutex_unlock(&list->large_mutex);
}

static void add_useful_part(void* memory, size_t length) {
  // needs external lock
  large_bucket* new_bucket = (large_bucket*)get_memory(sizeof(large_bucket));
  new_bucket->memory = memory;
  new_bucket->length = length;
  pid_t pid = getpid();
  release_large_bucket(pid, new_bucket, get_all_buckets(pid));
}

static void* try_alloc(large_bucket** buckets, size_t length, bucket_list* all) {
  // not thread-safe, needs to be locked externally
  large_bucket* buck = *buckets;
  large_bucket* last = NULL;
  while (buck != NULL) {
    if (buck->length >= length) {
      if (last != NULL) {
        last->next = buck->next;
      } else {
        *buckets = (*buckets)->next;
      }
      if (buck->length >= length + MIN_USEFUL_BYTES) {
        add_useful_part(buck->memory + length, buck->length - length);
      }
      if (all != NULL) {
        all->total_memory -= buck->length;
      }
      return buck->memory;
    }
    last = buck;
    buck = buck->next;
  }
  return NULL;
}

void* local_alloc(size_t length) {
  bucket_list* list = get_all_buckets(getpid());
  pthread_mutex_lock(&list->large_mutex);
  void* ptr = try_alloc(&list->large, length, list);
  pthread_mutex_unlock(&list->large_mutex);
  return ptr;
}

void* get_from_global(size_t length) {
  pthread_mutex_lock(&global_buckets_mutex);
  void* ptr = try_alloc(&global_buckets, length, NULL);
  pthread_mutex_unlock(&global_buckets_mutex);
  if (ptr == NULL) {
    size_t page_size = getpagesize();
    int delta = (page_size - length) % page_size;
    if (delta < 0) {
      delta += page_size;
    }
    ptr = mmap(NULL, length + delta, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (delta >= MIN_USEFUL_BYTES) {
      bucket_list* all = get_all_buckets(getpid());
      pthread_mutex_lock(&all->large_mutex);
      add_useful_part(ptr + length, delta);
      pthread_mutex_unlock(&all->large_mutex);
    }
  }
  return ptr;
}
