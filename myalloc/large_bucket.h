#ifndef _LARGE_BUCKET_H_
#define _LARGE_BUCKET_H_

#define MIN_USEFUL_PAGES 1

typedef struct large_bucket {
  void* memory;
  size_t pages;
  struct large_bucket* next;
} large_bucket;

void* add_large(size_t);
void free_large(void*);

#endif
