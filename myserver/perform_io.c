#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <poll.h>
#include "perform_io.h"
#include "sock_io.h"
#include "msg_queue.h"
#include "watch.h"

static struct pollfd poll_list[MAX_FD];
static msg_queue msgs[MAX_FD];
static size_t nfds = 0;

static const int MS_TIMEOUT = 500;

static const char prefix[] = "Connection from ";

int add_fd(int fd, struct sockaddr* addr, int len) {
  if (nfds == MAX_FD) {
    printf("Too many file descriptors.");
    return -1;
  }
  poll_list[nfds].fd = fd;
  poll_list[nfds].events = POLLIN;
  msgs[nfds] = *(msg_queue*)malloc(sizeof(msg_queue));
  memset(&msgs[nfds], 0, sizeof(msg_queue));
  printf("Accepted fd %d added\n", fd);
  printf("Here I need to extract an IP address from addr and multicast it\n");
  ++nfds;
  return 0;
}

void close_fd(size_t id) {
  poll_list[id].events = 0;
  close(poll_list[id].fd);
}

void perform_io() {
  while (1) {
    watch();
    printf("Working...\n");
    int ready = poll(poll_list, nfds, MS_TIMEOUT);
    if (ready == -1) {
      printf("I/O error\n");
      return;
    }
    if (ready == 0) {
      continue;
    }
    size_t count;
    for (count = 0; count < nfds; ++count) {
      if (poll_list[count].revents & POLLIN) {
        printf("Receiving message at fd %d\n", poll_list[count].fd);
        recv_message(poll_list, count, msgs, nfds);
      }
    }
    for (count = 0; count < nfds; ++count) {
      if (poll_list[count].revents & POLLOUT) {
        printf("Sending message from fd %d\n", poll_list[count].fd);
        send_message(&poll_list[count], msgs + count);
      }
    }
  }
}
