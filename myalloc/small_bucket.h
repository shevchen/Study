#include <unistd.h>

typedef struct small_bucket {
  size_t mask;
  void* memory;
  struct small_bucket* next;
} small_bucket;
