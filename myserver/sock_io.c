#include <stdlib.h>
#include <unistd.h>
#include <poll.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include "msg_queue.h"

#define BUFFER_SIZE 21

static char buffer[BUFFER_SIZE];
static size_t buf_size = 0;
static int ignore = 0;

void recv_message(struct pollfd* all_polls, size_t id, msg_queue* all_msgs, size_t nfds) {
  int bytes = recv(all_polls[id].fd, buffer, BUFFER_SIZE - buf_size, MSG_DONTWAIT);
  if (bytes > 0) {
    size_t next_start = 0;
    size_t i;
    for (i = buf_size; i < buf_size + bytes; ++i) {
      if (buffer[i] == '\n') {
        if (ignore) {
          ignore = 0;
        } else {
          size_t len = i - next_start + 1;
          message* m = (message*)malloc(sizeof(message));
          m->length = len;
          m->str = malloc(len);
          memcpy(m->str, buffer + next_start, len);
          m->receivers = nfds;
          size_t j;
          for (j = 0; j < nfds; ++j) {
            add_message(all_msgs + j, m);
            all_polls[j].events |= POLLOUT;
          }
        }
        next_start = i + 1;
      }
    }
    if (next_start == 0 && buf_size + bytes == BUFFER_SIZE) {
      buf_size = 0;
      ignore = 1;
    }
    buf_size = buf_size + bytes - next_start;
    if (next_start > 0) {
      memmove(buffer, buffer + next_start, buf_size);
    }
  }
}

void send_message(struct pollfd* poll, msg_queue* msgs) {
  size_t bytes_sent;
  message* m = get_message(msgs, &bytes_sent);
  int bytes = send(poll->fd, m->str + bytes_sent, m->length - bytes_sent, MSG_DONTWAIT);
  if (bytes > 0) {
    bytes_sent += bytes;
    if (bytes_sent == m->length) {
      remove_message(msgs);
      if (--(m->receivers) == 0) {
        free(m->str);
        free(m);
      }
      if (msgs->size == 0) {
        poll->events &= ~POLLOUT;
      }
    } else {
      update_sent(msgs, bytes_sent);
    }
  }
}
