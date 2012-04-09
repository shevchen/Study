#ifndef _SMALL_BUCKET_H_
#define _SMALL_BUCKET_H_

#define SMALL_BUCKET_PAGES 32

typedef struct small_bucket {
  size_t mask[SMALL_BUCKET_PAGES];
  void* memory;
  struct small_bucket* next;
} small_bucket;

void* add_small();
void free_small(small_bucket*, void*);

#endif
