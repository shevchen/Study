typedef struct large_bucket {
  void* memory;
  size_t pages;
  struct large_bucket* next;
} large_bucket;

void* try_alloc(large_bucket** buckets, size_t pages) {  
  large_bucket* buck = *buckets;
  large_bucket* last = NULL;
  while (buck != NULL) {
    if (buck->pages >= pages) {
      if (last != NULL) {
        last->next = buck->next;
      } else {
        *buckets = (*buckets)->next;
      }
      return buck->memory;
    }
    last = buck;
    buck = buck->next;
  }
  return NULL;
}
