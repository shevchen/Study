#include <stdlib.h>
#include <unistd.h>
#include <poll.h>
#include <string.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include "msg_queue.h"
#include "perform_io.h"

#define BUFFER_SIZE 21

static char buffer[BUFFER_SIZE];

void send_all(struct pollfd* all_polls, msg_queue* all_msgs, size_t nfds, size_t nfds_alive, message* m) {
  m->receivers = nfds_alive;
  size_t i;
  for (i = 0; i < nfds; ++i) {
    if (all_msgs[i].is_alive) {
      printf("Added message to the queue of fd %d\n", all_polls[i].fd);
      add_message(all_msgs + i, m);
      all_polls[i].events |= POLLOUT;
    }
  }
}

void read_message(struct pollfd* all_polls, size_t id, msg_queue* all_msgs, size_t nfds, size_t nfds_alive) {
  int bytes = recv(all_polls[id].fd, buffer, BUFFER_SIZE, MSG_DONTWAIT);
  if (bytes == 0) {
    close_fd(id);
    return;
  }
  if (bytes > 0) {
    printf("Received message at fd %d\n", all_polls[id].fd);
    size_t next_start = 0;
    size_t i;
    msg_queue* queue = all_msgs + id;
    for (i = 0; i < bytes; ++i) {
      if (buffer[i] == '\n') {
        size_t len = i - next_start + 1;
        if (queue->part_bytes != 0) {
          size_t full_size = len + queue->part_bytes;
          message* m = queue->part_written;
          if (!queue->ignore && full_size <= BUFFER_SIZE) {
            m->str = realloc(m->str, full_size);
            memcpy(m->str + queue->part_bytes, buffer + next_start, len);
            m->length = full_size;
            send_all(all_polls, all_msgs, nfds, nfds_alive, m);
          } else {
            free(m->str);
            free(m);
          }
          queue->part_written = NULL;
          queue->ignore = 0;
          queue->part_bytes = 0;
        } else {
          message* m = (message*)malloc(sizeof(message));
          m->length = len;
          m->str = malloc(len);
          memcpy(m->str, buffer + next_start, len);
          send_all(all_polls, all_msgs, nfds, nfds_alive, m);
        }
        next_start = i + 1;
      }
    }
    if (next_start < bytes && !queue->ignore) {
      size_t len = bytes - next_start;
      if (queue->part_bytes != 0) {
        if (queue->part_bytes + len > BUFFER_SIZE) {
          queue->ignore = 1;
        } else {
          message* m = queue->part_written;
          m->str = realloc(m->str, m->length + len);
          memcpy(m->str + m->length, buffer + next_start, len);
          m->length += len;
          queue->part_bytes += len;
        }
      } else {
        message* m = (message*)malloc(sizeof(message));
        m->length = len;
        m->str = malloc(len);
        memcpy(m->str, buffer + next_start, len);
        queue->part_written = m;
        queue->ignore = 0;
        queue->part_bytes = len;
      }
    }
  }
}

void print_message(struct pollfd* poll, msg_queue* msgs) {
  size_t bytes_sent;
  message* m = get_message(msgs, &bytes_sent);
  int bytes = send(poll->fd, m->str + bytes_sent, m->length - bytes_sent, MSG_DONTWAIT);
  if (bytes > 0) {
    printf("Sent message from fd %d\n", poll->fd);
    bytes_sent += bytes;
    if (bytes_sent == m->length) {
      remove_message(msgs);
      if (--(m->receivers) == 0) {
        printf("Freeing message\n");
        free(m->str);
        free(m);
      }
      printf("Receivers left: %d\n", m->receivers);
      if (msgs->size == 0) {
        poll->events &= ~POLLOUT;
      }
    } else {
      update_sent_bytes(msgs, bytes_sent);
    }
  }
}
