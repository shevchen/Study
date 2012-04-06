typedef struct large_bucket {
  void* memory;
  size_t pages;
  struct large_bucket* next;
} large_bucket;

void* try_alloc(large_bucket** buckets, size_t pages, pid_t pid) {  
  large_bucket* buck = *buckets;
  large_bucket* last = NULL;
  while (buck != NULL) {
    if (buck->pages >= pages) {
      size_t* mem = (size_t*)buck->memory;
      size_t sz = sizeof(size_t);
      mem[0] = (size_t)pid;
      mem[sz] = pages;
      if (last != NULL) {
        last->next = buck->next;
      } else {
        *buckets = (*buckets)->next;
      }
      return mem;
    }
    last = buck;
    buck = buck->next;
  }
  return NULL;
}
