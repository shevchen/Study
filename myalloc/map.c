#include <unistd.h>
#include <sys/mman.h>
#include <pthread.h>
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

static void* get_aligned_memory(size_t len) {
  size_t not_aligned = (size_t)mmap(NULL, 2 * len, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  size_t to = not_aligned + 2 * len - not_aligned % len;
  size_t from = to - len;
  munmap((void*)not_aligned, from - not_aligned);
  munmap((void*)to, not_aligned + 2 * len - to);
  return (void*)from;
}

static size_t get_hash(size_t n) {
  return (A * n + B) % MOD;
}

static bucket_list* map[MOD] = {NULL};
static pthread_mutex_t main_mutex[MOD] = {PTHREAD_MUTEX_INITIALIZER};

static large_bucket* global_buckets = NULL;
static pthread_mutex_t global_buckets_mutex = PTHREAD_MUTEX_INITIALIZER;

static small_allocs* alloc_map[MOD] = {NULL};
static pthread_mutex_t alloc_mutex = PTHREAD_MUTEX_INITIALIZER;

static bucket_list* get_all_buckets(pid_t pid) {
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

void add_small_bucket_mem(small_bucket* bucket, size_t page_addr) {
  size_t hash = get_hash(page_addr);
  small_allocs* new_alloc = (small_allocs*)get_memory(sizeof(small_allocs));
  new_alloc->bucket = bucket;
  new_alloc->page_addr = page_addr;
  pthread_mutex_lock(&alloc_mutex);
  new_alloc->next = alloc_map[hash];
  alloc_map[hash] = new_alloc;
  pthread_mutex_unlock(&alloc_mutex);
}

static void add_to_global(large_bucket* bucket) {
  pthread_mutex_lock(&global_buckets_mutex);
  bucket->next = global_buckets;
  global_buckets = bucket;
  pthread_mutex_unlock(&global_buckets_mutex);
}

static void clear_local_memory(bucket_list* list) {
  pthread_mutex_lock(&list->large_mutex);
  while (list->total_memory >= MAX_LOCAL_MEMORY) {
    large_bucket* first = list->large;
    list->total_memory -= first->length;
    add_to_global(first);
    list->large = first->next;
  }
  pthread_mutex_unlock(&list->large_mutex);
}

static void release_large_bucket(pid_t pid, large_bucket* new_bucket) {
  // needs external lock
  bucket_list* list = get_all_buckets(pid);
  new_bucket->next = list->large;
  list->large = new_bucket;
  list->total_memory += new_bucket->length;
  if (list->total_memory >= 2 * MAX_LOCAL_MEMORY) {
    clear_local_memory(list);
  }
}

void release_free(pid_t pid, large_bucket* new_bucket) {
  bucket_list* list = get_all_buckets(pid);
  pthread_mutex_lock(&list->large_mutex);
  release_large_bucket(pid, new_bucket);
  pthread_mutex_unlock(&list->large_mutex);
}

static void add_useful_part(void* memory, size_t length) {
  large_bucket* new_bucket = (large_bucket*)get_memory(sizeof(large_bucket));
  new_bucket->memory = memory;
  new_bucket->length = length;
  release_large_bucket(getpid(), new_bucket);
}

void* try_alloc(large_bucket** buckets, size_t length, bucket_list* all) {
  // not thread-safe itself, needs to be locked from the outside
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
        buck->length = length;
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
      add_useful_part(ptr + length, delta);
    }
  }
  return ptr;
}

size_t get_size(void* ptr) {
  size_t ps = getpagesize();
  small_bucket* small = find_small(ptr);
  if (small == NULL) {
    return *(size_t*)(ptr - sizeof(size_t));
  }
  return getpagesize() / (sizeof(size_t) * 8);
}
