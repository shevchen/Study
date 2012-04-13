#include <unistd.h>

void main() {
  int ITER = 1000;
  int* mem[ITER];
  int i;
  for (i = 0; i < ITER; ++i) {
    mem[i] = (int*)malloc(sizeof(int) * (i % 23 + i % 2 * 1000));
  }
  for (i = 0; i < ITER; ++i) {
    free(mem[i]);
  }
}
