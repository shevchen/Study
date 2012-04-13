#include <unistd.h>
#include <stdio.h>

void main() {
  int ITER = 100000;
  int PERIOD = 1;
  int i;
  for (i = 0; i < ITER; ++i) {
    size_t sz = sizeof(int) * i;
    int* mem = (int*)malloc(sz);
    mem = (int*)realloc(mem, 2 * sz);
    free(mem);
    if (i % PERIOD == PERIOD - 1) {
      printf("Step %d\n", i / PERIOD + 1);
    }
  }
}
