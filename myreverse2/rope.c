#include "rope.h"

rope* merge(rope* left, rope* right) {
  if (left == NULL) {
    return right;
  }
  if (right == NULL) {
    return left;
  }
  if (right->priority > left->priority) {
    right->left = merge(left, right->left);
    right->size += left->size;
    return right;
  } else {
    left->right = merge(left->right, right);
    left->size += right->size;
    return left;
  }
}
