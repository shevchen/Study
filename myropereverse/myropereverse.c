#include <string.h>
#include <stdlib.h>
#include "rope.h"

#define buf_size 10

static char buffer[buf_size];
static rope* root = NULL;
static size_t rope_size = 0;
static int too_long = 0;

static int print(rope* n) {
  if (n == NULL) {
    return 0;
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

static int print_rope(int eof) {
  if (print(root) == -1) {
    return -1;
  }
  if (eof) {
    return 0;
  }
  char c = '\n';
  return write(1, &c, sizeof c);
}

static void free_nodes(rope* n) {
  if (n == NULL) {
    return;
  }
  free_nodes(n->left);
  free_nodes(n->right);
  free(n);
}

static void free_rope() {
  free_nodes(root);
  root = NULL;
  rope_size = 0;
}

static int create_rope()
{
  int count = read(0, buffer, buf_size);
  if (count <= 0) {
    return count;
  }
  size_t i;
  for (i = 0; i < count; ++i) {
    if (buffer[i] == '\n') {
      if (too_long) {
        too_long = 0;
      } else {
        print_rope(0);
        free_rope();
      }
    } else if (too_long) {
    } else if (rope_size  + 1 >= buf_size) {
      free_rope();
      too_long = 1;
    } else {
      rope* new_node = malloc(sizeof(rope));
      new_node->c = buffer[i];
      new_node->priority = rand();
      new_node->left = NULL;
      new_node->right = NULL;
      root = merge(root, new_node);
      rope_size++;
    }
  }
}

int main() {
  srand(time(NULL));
  while (1) {
    int chars = create_rope();
    if (chars <= 0) {
      if (chars == 0 && rope_size > 0) {
        print_rope(1);
        free_rope();
      }
      return chars;
    }
  }
}
