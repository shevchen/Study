#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include "perform_io.h"
#include "watch.h"

static struct addrinfo *hints, *servinfo;

void init(char* port) {
  if (!getaddrinfo(NULL, port, hints, &servinfo)) {
    struct addrinfo* cur = servinfo;
    while (cur != NULL) {
      int fd = socket(cur->ai_family, cur->ai_socktype | SOCK_NONBLOCK, cur->ai_protocol);
      if (fd >= 0 && !bind(fd, cur->ai_addr, cur->ai_addrlen)) {
        printf("Binding socket (fd %d) on port %s\n", fd, port);
        add_my_socket(fd);
      }
      cur = cur->ai_next;
    }
  }
}

int main(int argc, char** argv) {
  hints = (struct addrinfo*)malloc(sizeof(struct addrinfo));
  servinfo = (struct addrinfo*)malloc(sizeof(struct addrinfo));
  memset(hints, 0, sizeof(struct addrinfo));
  hints->ai_family = AF_UNSPEC;
  hints->ai_socktype = SOCK_STREAM;
  hints->ai_flags = AI_PASSIVE;
  int i;
  for (i = 1; i < argc; ++i) {
    init(argv[i]);
  }
  freeaddrinfo(servinfo);
  perform_io();
  return 0;
}
