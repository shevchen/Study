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

static small_allocs* alloc_map[MOD];

static size_t get_hash(size_t n) {
  return (A * n + B) % MOD;
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
  new_alloc->next = alloc_map[hash];
  alloc_map[hash] = new_alloc;
}

static bucket_list* map[MOD];

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
  new_list->next = map[hash];
  map[hash] = new_list;
  return new_list;
}

small_bucket* get_small_buckets(pid_t pid) {
  bucket_list* list = get_all_buckets(pid);
  return list->small;
}

large_bucket* get_large_buckets(pid_t pid) {
  bucket_list* list = get_all_buckets(pid);
  return list->large;
}

void release_large_bucket(pid_t pid, large_bucket* new_bucket) {
  bucket_list* list = get_all_buckets(pid);
  new_bucket->next = list->large;
  list->large = new_bucket;
}

size_t get_size(void* ptr) {
  size_t ps = getpagesize();
  small_bucket* small = find_small(ptr);
  if (small == NULL) {
    return *(size_t*)(ptr - sizeof(size_t));
  }
  return getpagesize() / (sizeof(size_t) * 8);
}
