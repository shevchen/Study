#include <sys/mman.h>
#include "main.c"

void main() {
  int* mmapped = (int*)mmap(NULL, 100, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  mmapped[41] = 3;
  int* mem = (int*)malloc(200);
  mem[150] = 4;
}
