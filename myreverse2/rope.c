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
    return right;
  } else {
    left->right = merge(left->right, right);
    return left;
  }
}
