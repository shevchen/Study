#ifndef ROPE_H_
#define ROPE_H_

#include <stddef.h>

typedef struct rope {
  char c;
  size_t size;
  int priority;
  struct rope *left, *right;
} rope;

rope* merge(rope* left, rope* right);

#endif
