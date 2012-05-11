#ifndef PERFORM_IO_H_
#define PERFORM_IO_H_

#include <sys/types.h>
#include <sys/socket.h>

#define MAX_FD 65536

int add_fd(int fd, struct sockaddr* saddr, int len);

void close_fd(size_t id);

void perform_io();

#endif
