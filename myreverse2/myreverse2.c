#include <string.h>
#include <stdlib.h>
#include "rope.h"

#define buf_size 10

static rope* root = NULL;

static int print(rope* n) {
  if (n == NULL) {
    return;
  }
  if (print(n->right) == -1) {
    return -1;
  }
  if (write(1, &n->c, sizeof(char)) == -1) {
    return -1;
  }
  if (print(n->left) == -1) {
    return -1;
  }
  return 0;
}

static int print_rope() {
  print(root);
  char c = '\n';
  return write(1, &c, sizeof c);
}

static void free_rope(rope* n) {
  if (n == NULL) {
    return;
  }
  free_rope(n->left);
  free_rope(n->right);
  free(n);
}

static int create_rope()
{
  size_t i;
  char c;
  root = NULL;
  for (i = 0; i < buf_size; ++i) {
    int count = read(0, &c, sizeof c);
    if (count <= 0) {
      return count;
    }
    if (c == '\n') {
      return i;
    }
    rope* new_node = malloc(sizeof(rope));
    new_node->c = c;
    new_node->size = 1;
    new_node->priority = rand();
    new_node->left = NULL;
    new_node->right = NULL;
    root = merge(root, new_node);
  }
  return buf_size + 1;
}

int main() {
  srand(time(NULL));
  while (1) {
    int chars = create_rope();
    if (chars  == -1) {
      return -1;
    }
    if (chars <= buf_size) {
      if (print_rope() == -1) {
        return -1;
      }
    }
    free_rope(root);
    if (chars == 0) {
      return 0;
    }
  }
}