#ifndef SOCK_IO_H_
#define SOCK_IO_H_

#include <unistd.h>
#include <poll.h>
#include "msg_queue.h"

void send_all(struct pollfd* all_polls, msg_queue* all_msgs, size_t nfds, size_t nfds_alive, message* m);

void read_message(struct pollfd* all_polls, size_t id, msg_queue* all_msgs, size_t nfds, size_t nfds_alive);

void print_message(struct pollfd* poll, msg_queue* msgs);

#endif
