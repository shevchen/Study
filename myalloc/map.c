typedef struct {
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

void* get_bucket(pid_t pid, int small) {
  size_t hash = get_hash(pid);
  bucket_list* list = map[hash];
  while (list != NULL) {
    if (list.pid == pid) {
      return small ? list->small : list->large;
    }
    list = list->next;
  }
  bucket_list new_list = {pid, NULL, NULL, map[hash]};
  map[hash] = new_list;
  return small ? new_list->small : new_list->large;
}

void add_bucket(pid_t pid, void* new_bucket, int small) {
  bucket_list* list = map[get_hash(pid)];
  if (small) {
    small_bucket* buck = (small_bucket*)new_bucket;
    buck->next = list->small;
    list->small = buck;
  } else {
    large_bucket* buck = (large_bucket*)new_bucket;
    buck->next = list->large;
    list->large = buck;
  }
}
