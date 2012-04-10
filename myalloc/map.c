#include <unistd.h>
#include <sys/mman.h>
#include "map.h"

static void* free_page = NULL;
static size_t free_size = 0;

void* get_memory(size_t len) {
  // only for little local segments
  if (free_page == NULL || free_size < len) {
    free_size = getpagesize();
    free_page = mmap(NULL, free_size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  }
  free_size -= len;
  void* ptr = free_page;
  free_page += len;
  return ptr;
}

static size_t get_hash(size_t n) {
  return (A * n + B) % MOD;
}

static small_allocs* alloc_map[MOD];

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
  new_alloc->next = alloc_map[hash];
  alloc_map[hash] = new_alloc;
}

static bucket_list* map[MOD];

static large_bucket* global_buckets = NULL;

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
  new_list->next = map[hash];
  map[hash] = new_list;
  return new_list;
}

static void add_useful_part(void* memory, size_t length) {
  large_bucket* new_bucket = (large_bucket*)get_memory(sizeof(large_bucket));
  new_bucket->memory = memory;
  new_bucket->length = length;
  release_large_bucket(getpid(), new_bucket);
}

void* try_alloc(large_bucket** buckets, size_t length, bucket_list* all) {
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
  return try_alloc(&list->large, length, list);
}

void* get_from_global(size_t length) {
  void* ptr = try_alloc(&global_buckets, length, NULL);
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

static void add_to_global(large_bucket* bucket) {
  bucket->next = global_buckets;
  global_buckets = bucket;
}

small_bucket* get_small_buckets(pid_t pid) {
  bucket_list* list = get_all_buckets(pid);
  return list->small;
}

static void clear_local_memory(bucket_list* list) {
  while (list->total_memory >= MAX_LOCAL_MEMORY) {
    large_bucket* first = list->large;
    list->total_memory -= first->length;
    add_to_global(first);
    list->large = first->next;
  }
}

void release_large_bucket(pid_t pid, large_bucket* new_bucket) {
  bucket_list* list = get_all_buckets(pid);
  new_bucket->next = list->large;
  list->large = new_bucket;
  list->total_memory += new_bucket->length;
  if (list->total_memory >= 2 * MAX_LOCAL_MEMORY) {
    clear_local_memory(list);
  }
}

size_t get_size(void* ptr) {
  size_t ps = getpagesize();
  small_bucket* small = find_small(ptr);
  if (small == NULL) {
    return *(size_t*)(ptr - sizeof(size_t));
  }
  return getpagesize() / (sizeof(size_t) * 8);
}
