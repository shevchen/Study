#ifndef _MAP_H_
#define _MAP_H_

#include <unistd.h>
#include <pthread.h>
#include "small_bucket.h"
#include "large_bucket.h"

#define A 239
#define B 366
#define MOD 32768

typedef struct bucket_list {
  pid_t pid;
  size_t total_memory;
  pthread_mutex_t small_mutex, large_mutex;
  small_bucket* small;
  large_bucket* large;
  struct bucket_list* next;
} bucket_list;

void* get_memory(size_t len);
size_t get_hash(size_t n);
bucket_list* get_all_buckets(pid_t pid);
size_t get_size(void* ptr);

#endif
