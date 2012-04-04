#define BITS 32
#define MAX_SMALL 4096

const int memory_per_bit = MAX_SMALL / BITS;

typedef struct {
  size_t mask;
  void* memory;
  small_bucket* next;
} small_bucket;

void* add_small(size_t size) {
  pid_t pid = getpid();
  small_bucket* buck = (small_bucket*)get_bucket(pid, 1);
  size_t need_bits = (size + memory_per_bit - 1) / memory_per_bit;
  size_t sz = sizeof(size_t);
  while (buck != NULL) {
    size_t mask = buck->mask;
    for (size_t i = 0; i <= BITS - need_bits; ++i) {
      if (!(mask & ((1 << need_bits) - 1))) {
        mask |= (1 << (i + need_bits)) - (1 << i);
        buck->memory[i] = (void)(size_t)pid;
        buck->memory[i + sz] = (void)(size - 2 * sz);
        return buck->memory + i * sizeof(void) + 2 * sz;
      }
    }
    buck = buck->next;
  }
  small_bucket new_bucket = {(1 << need_bits) - 1, mmap(NULL, MAX_SMALL, PROT_READ | PROT_WRITE, MAP_ANONYMOUS, -1, 0), NULL};
  new_bucket.memory[0] = (size_t)pid;
  new_bucket.memory[sz] = size - 2 * sz;
  add_bucket(pid, &new_bucket, 1);
  return new_bucket.memory + 2 * sz;
}
