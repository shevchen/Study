#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/ip.h>

int main() {
  struct addrinfo hints, *res;
  int fd;

  memset(&hints, 0, sizeof hints);
  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags = AI_PASSIVE;
  
  getaddrinfo(NULL, "12347", &hints, &res);

  printf("ai_flags: %d\nai_family: %d\nai_socktype: %d\nai_protocol: %d\nai_addrlen: %d\nai_canonname: %s\n port: %d\n", res->ai_flags, res->ai_family, res->ai_socktype, res->ai_protocol, res->ai_addrlen, res->ai_canonname, ((struct sockaddr_in*)(res->ai_addr))->sin_port);
  return 0;
}
