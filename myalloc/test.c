#include <stdio.h>

void main() {
  int* mem = (int*)malloc(32760);
  int* mem2 = (int*)malloc(32760);
  free(mem2);
  free(mem);
  mem = (int*)malloc(16370);
  mem2 = (int*)malloc(16370);
  free(mem);
  free(mem2);
}
