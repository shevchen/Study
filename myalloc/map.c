typedef struct bucket_list {
  pid_t pid;
  small_bucket* small;
  large_bucket* large;
  bucket_list* next;
} bucket_list;

#define A 239
#define B 366
#define MOD 2048

bucket_list* map[MOD];

size_t get_hash(pid_t pid) {
  return (A * (size_t)pid + B) % MOD;
}

bucket* get_bucket(pid_t pid, int small) {
  size_t hash = get_hash(pid);
  bucket_list* list = map[hash];
  while (list != NULL) {
    if (list.pid == pid) {
      return small ? list->small : list->large;
    }
    list = list->next;
  }
  bucket_list new_list = {pid, alloc_small_bucket(), alloc_large_bucket(), map[hash]};
  map[hash] = new_list;
  return small ? new_list->small : new_list->large;
}
