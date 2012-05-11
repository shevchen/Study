#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <poll.h>
#include <netinet/in.h>
#include "perform_io.h"
#include "sock_io.h"
#include "msg_queue.h"
#include "watch.h"

#define MAX_IP_LENGTH 50

static struct pollfd poll_list[MAX_FD];
static msg_queue msgs[MAX_FD];
static size_t nfds = 0;
static size_t nfds_alive = 0;

static const int MS_TIMEOUT = 500;

static const char* prefix = "Connection from ";
static char ip[MAX_IP_LENGTH];

int add_fd(int fd, struct sockaddr* saddr, int len) {
  if (nfds == MAX_FD) {
    printf("Too many file descriptors.");
    return -1;
  }
  char* addr = (char*)saddr;
  poll_list[nfds].fd = fd;
  poll_list[nfds].events = POLLIN;
  msgs[nfds] = *(msg_queue*)malloc(sizeof(msg_queue));
  memset(&msgs[nfds], 0, sizeof(msg_queue));
  msgs[nfds].is_alive = 1;
  printf("Accepted fd %d added\n", fd);
  message* m = (message*)malloc(sizeof(message));
  if ((u_char)*(addr + sizeof(u_char)) == AF_INET) {
    struct sockaddr_in* sa = (struct sockaddr_in*)addr;
    inet_ntop(AF_INET, &(sa->sin_addr), ip, MAX_IP_LENGTH);
  } else {
    struct sockaddr_in6* sa = (struct sockaddr_in6*)addr;
    inet_ntop(AF_INET6, &(sa->sin6_addr), ip, MAX_IP_LENGTH);
  }
  size_t ip_len = strlen(ip);
  ip[ip_len++] = '\n';
  size_t pref_len = strlen(prefix);
  m->str = malloc(pref_len + ip_len);
  memcpy(m->str, prefix, pref_len);
  memcpy(m->str + pref_len, ip, ip_len);
  m->length = pref_len + ip_len;
  m->receivers = nfds_alive;
  printf("Here I extract an IP address from addr and multicast it\n");
  send_all(poll_list, msgs, nfds, nfds_alive, m);
  ++nfds;
  ++nfds_alive;
  return 0;
}

void close_fd(size_t id) {
  close(poll_list[id].fd);
  poll_list[id].fd = -1;
  poll_list[id].events = 0;
  msgs[id].is_alive = 0;
  --nfds_alive;
}

void perform_io() {
  while (1) {
    watch();
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
        read_message(poll_list, count, msgs, nfds, nfds_alive);
      }
    }
    for (count = 0; count < nfds; ++count) {
      if (poll_list[count].revents & POLLOUT) {
        printf("Sending message from fd %d\n", poll_list[count].fd);
        print_message(&poll_list[count], msgs + count);
      }
    }
  }
}
