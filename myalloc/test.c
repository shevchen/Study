#include <stdio.h>

void main() {
  int* mem = (int*)malloc(200);
  free(mem);
  mem = (int*)malloc(200);
  int* mem2 = (int*)malloc(200);
  free(mem);
  free(mem2);
}
