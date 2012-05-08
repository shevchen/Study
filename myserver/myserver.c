#include "perform_io.h"
#include "watch.h"

int main(int argc, char** argv) {
  if (fork()) {
    perform_io();
  } else {
    watch(argc, argv);
  }
  return 0;
}
