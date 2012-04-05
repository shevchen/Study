typedef struct {
  pid_t pid;
  small_bucket* small;
  large_bucket* large;
  bucket_list* next;
} bucket_list;

#define A 239
#define B 366
#define MOD 32768

static bucket_list* map[MOD];

static size_t get_hash(pid_t pid) {
  return (A * (size_t)pid + B) % MOD;
}

static void* get_bucket(pid_t pid, int small) {
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

small_bucket* get_small_bucket(pid_t pid) {
  return (small_bucket*)get_bucket(pid, 1);
}

large_bucket* get_large_bucket(pid_t pid) {
  return (large_bucket*)get_bucket(pid, 0);
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

void remove_large_bucket(pid_t pid) {
  bucket_list* list = map[get_hash(pid)];
  if (list->large != NULL) {
    list->large = list->large->next;
  }
}
