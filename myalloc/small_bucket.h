#include <unistd.h>

#define SMALL_BUCKET_PAGES 1

typedef struct small_bucket {
  size_t mask;
  void* memory;
  struct small_bucket* next;
} small_bucket;
