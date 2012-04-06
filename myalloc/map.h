#include <unistd.h>
#include "small_bucket.h"
#include "large_bucket.h"

#define A 239
#define B 366
#define MOD 32768

typedef struct bucket_list {
  pid_t pid;
  small_bucket* small;
  large_bucket* large;
  struct bucket_list* next;
} bucket_list;

size_t get_hash(pid_t pid) {
  return (A * (size_t)pid + B) % MOD;
}
