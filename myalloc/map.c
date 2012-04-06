#include "map.h"

static bucket_list* map[MOD];

static void* get_buckets(pid_t pid, int small) {
  size_t hash = get_hash(pid);
  bucket_list* list = map[hash];
  while (list != NULL) {
    if (list->pid == pid) {
      return small ? (void*)list->small : (void*)list->large;
    }
    list = list->next;
  }
  bucket_list new_list = {
    .pid = pid,
    .small =  NULL,
    .large =  NULL,
    .next = map[hash]
  };
  map[hash] = &new_list;
  return small ? (void*)(new_list.small) : (void*)(new_list.large);
}

small_bucket* get_small_buckets(pid_t pid) {
  return (small_bucket*)get_buckets(pid, 1);
}

large_bucket* get_large_buckets(pid_t pid) {
  return (large_bucket*)get_buckets(pid, 0);
}

void add_small_bucket(pid_t pid, small_bucket* new_bucket) {
  bucket_list* list = map[get_hash(pid)];
  new_bucket->next = list->small;
  list->small = new_bucket;
}

void add_large_bucket(pid_t pid, large_bucket* new_bucket) {
  bucket_list* list = map[get_hash(pid)];
  new_bucket->next = list->large;
  list->large = new_bucket;
}

size_t get_size(void* ptr) {
  return *((size_t*)ptr - sizeof(size_t));
}
