#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include "msg_queue.h"

static const size_t MAX_QUEUE_SIZE = 100;

void add_message(msg_queue* queue, message* m) {
  if (queue->size == MAX_QUEUE_SIZE) {
    remove_message(queue);
  }
  node* n = (node*)malloc(sizeof(node));
  n->m = m;
  n->bytes_sent = 0;
  n->prev = NULL;
  n->next = queue->tail;
  if (queue->tail == NULL) {
    queue->tail = n;
    queue->head = n;
  } else {
    queue->tail->prev = n;
    queue->tail = n;
  }
  printf("Added message %s\n", m->str);
  ++(queue->size);
}
    
message* get_message(msg_queue* queue, size_t* bytes_sent) {
  if (queue->head == NULL) {
    return NULL;
  }
  *bytes_sent = queue->head->bytes_sent;
  return queue->head->m;
}

void update_sent_bytes(msg_queue* queue, size_t bytes_sent) {
  if (queue->head == NULL) {
    return;
  }
  queue->head->bytes_sent = bytes_sent;
}

void remove_message(msg_queue* queue) {
  if (queue->head == NULL) {
    return;
  }
  node* old = queue->head;
  queue->head = old->prev;
  printf("Removed message %s\n", old->m->str);
  free(old);
  if (queue->head == NULL) {
    queue->tail = NULL;
  } else {
    queue->head->next = NULL;
  }
  --(queue->size);
}
