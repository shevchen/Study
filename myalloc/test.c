#include "main.c"
#include <stdio.h>

void main() {
  int* mem = (int*)malloc(200);
  printf("allocated mem\n");
  int i;
  for (i = 20; i < 80; ++i) {
    mem[i] = i;
  }
  int sum = 0;
  for (i = 30; i <= 50; ++i) {
    sum += mem[i];
  }
  printf("%d = 840\n", sum);
  free(mem);
  printf("freed mem\n");
  mem = (int*)malloc(30);
  printf("allocated mem\n");
  size_t* mem2 = (size_t*)malloc(1000000);
  printf("allocated mem2\n");
  mem2[123456] = 654321;
  printf("%d = 654321\n", mem2[123456]);
  free(mem2);
  printf("freed mem2\n");
  free(mem);
  printf("freed mem\n");
}
