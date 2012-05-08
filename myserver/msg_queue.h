#ifndef MSG_QUEUE_H_
#define MSG_QUEUE_H_

#include <unistd.h>

typedef struct {
  char* str;
  size_t length;
  size_t receivers;
} message;

typedef struct node {
  message* m;
  size_t bytes_sent;
  struct node *prev, *next;
} node;

typedef struct {
  node *head, *tail;
  size_t size;
} msg_queue;

void add_message(msg_queue* queue, message* m);

message* get_message(msg_queue* queue, size_t* bytes_sent);

void update_sent(msg_queue* queue, size_t bytes_sent);

void remove_message(msg_queue* queue);

#endif
