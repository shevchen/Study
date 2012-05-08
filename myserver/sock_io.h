typedef struct message {
  char* str;
  size_t length;
  size_t receivers;
  struct message* next;
} message;

typedef struct fdinfo {
  message* messages;
  struct fdinfo* next;
} fdinfo;

int recv_message(int fd, fdinfo* info);

int send_message(int fd, fdinfo* info);
