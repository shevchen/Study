#include "rope.h"

rope* merge(rope* left, rope* right) {
  if (left == NULL) {
    return right;
  }
  if (right == NULL) {
    return left;
  }
  if (right->priority > left->priority) {
    right->right = merge(right->left, right->right);
    right->left = left;
    right->size += left->size;
    return right;
  } else {
    left->left = merge(left->left, left->right);
    left->right = right;
    left->size += right->size;
    return left;
  }
}
