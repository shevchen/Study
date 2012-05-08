#include <unistd.h>
#include <stdio.h>
#include <poll.h>
#include "sock_io.h"

typedef struct pollfd pollfd;

static const size_t MAX_FD = 65536;
static pollfd poll_list[MAX_FD];
static fdinfo info_list[MAX_FD];
static size_t fd_count = 0;
static pthread_mutex_t fd_mutex = PTHREAD_MUTEX_INITIALIZER;

static const int MS_TIMEOUT = 15000;

void add_fd(int fd) {
  pthread_mutex_lock(&fd_mutex);
  if (fd_count == MAX_FD) {
    printf("Too many file descriptors.");
    return;
  }
  poll_list[fd_count].fd = fd;
  poll_list[fd_count].events = POLLIN | POLLOUT;
  info_list[fd_count].messages = NULL;
  info_list[fd_count].next = NULL;
  ++fd_count;
  pthread_mutex_unlock(&fd_mutex);
}

void perform_io() {
  while (1) {
    size_t nfds = fd_count;
    if (poll(poll_list, nfds, MS_TIMEOUT) == -1) {
      printf("I/O error.");
      return;
    }
    size_t count;
    for (count = 0; count < nfds; ++count) {
      if (poll_list[i].revents & POLLIN) {
        if (!fork()) {
          exit(recv_message(poll_list[i].fd, info_list[i]));
        }
      }
    }
    for (count = 0; count < nfds; ++count) {
      if (poll_list[i].revents & POLLOUT) {
        if (!fork()) {
          exit(send_message(poll_list[i].fd, info_list[i]));
        }
      }
    }
  }
}
