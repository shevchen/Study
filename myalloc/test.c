#include "main.c"

void main() {
  int* mem = (int*)malloc(200);
  mem[150] = 4;
}
