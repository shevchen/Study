#include <unistd.h>

void main() {
  int* mem = (int*)malloc(200 * sizeof(int));
  int i;
  for (i = 20; i < 80; ++i) {
    mem[i] = i;
  }
  mem[0] = 1;
  mem[199] = 1;
  int sum = 0;
  for (i = 30; i <= 50; ++i) {
    sum += mem[i];
  }
  printf("%d = 840\n", sum);
  free(mem);
  mem = (int*)malloc(305 * sizeof(int));
  mem[0] = 239;
  mem[304] = 366;
  size_t* mem2 = (size_t*)malloc(1000000 * sizeof(size_t));
  mem2[123456] = 654321;
  mem2[0] = 0;
  mem2[999999] = 999999;
  printf("%d = 654321\n", mem2[123456]);
  free(mem2);
  free(mem);
}
