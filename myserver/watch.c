#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include "perform_io.h"

static int my_sockets[MAX_FD];
static size_t my_sock_num = 0;

int add_my_socket(int fd) {
  if (my_sock_num == MAX_FD) {
    printf("Too many file descriptors.");
    return -1;
  }
  if (listen(fd, 1)) {
    printf("Unable to listen to the socket.");
    return -1;
  }
  printf("Added listening socket with fd %d\n", fd);
  my_sockets[my_sock_num++] = fd;
  return 0;
}

void watch() {
  size_t i;
  struct sockaddr* addr = (struct sockaddr*)malloc(sizeof(struct sockaddr));
  int len = sizeof(struct sockaddr);
  for (i = 0; i < my_sock_num; ++i) {
    int new_fd = accept(my_sockets[i], addr, &len);
    if (new_fd >= 0) {
      printf("Detected new client, new socket fd: %d\n", new_fd);
      add_fd(new_fd, addr, len);
    }
  }
  free(addr);
}
