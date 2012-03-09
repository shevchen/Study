#include <string.h>

#define buf_size 10

struct buffer
{
  char buf[buf_size], temp_buf[buf_size];
  int busy_pos, free_pos;
};

int read_chars(struct buffer* buf)
{
  int first = read(0, buf->buf + buf->free_pos, buf_size - buf->free_pos);
  if (first == -1)
  {
    return -1;
  }
  if (buf->free_pos + first < buf_size)
  {
    buf->free_pos += first;
    return first;
  }
  int second = read(0, buf->buf, buf->busy_pos); 
  if (second == -1)
  {
    return -1;
  }
  buf->free_pos = second;
  return first + second;
}

int reverse(struct buffer* buf, int from, int to)
{
  int len = 0;
  void* a;
  if (to >= from)
  {
    len = to - from + 1;
    a = memcpy(buf->temp_buf, buf->buf + from, len);
  }
  else
  {
    len = buf_size - from;
    a = memcpy(buf->temp_buf, buf->buf + from, len);
    len += to + 1;
    a = memcpy(buf->temp_buf + buf_size - from, buf->buf, to + 1);
  }
  int i;
  for (i = 0; i < (len - 1) / 2; ++i)
  {
    char c = buf->temp_buf[i];
    buf->temp_buf[i] = buf->temp_buf[len - 2 - i];
    buf->temp_buf[len - 2 - i] = c;
  }
  return write(1, buf->temp_buf, len);
}

int next_pos(int pos)
{
  return (pos + 1) % buf_size;
}

int write_chars(struct buffer* buf)
{
  int written = 0;
  while (1)
  {
    int i;
    for (i = buf->busy_pos; i != buf->free_pos; i = next_pos(i))
    {
      if (buf->buf[i] == '\n')
      {
        break;
      }
    }
    if (i == buf->free_pos)
    {
      buf->busy_pos = buf->free_pos;
      return written;
    }
    int reversed = reverse(buf, buf->busy_pos, i);
    if (reversed == -1)
    {
      return -1;
    }
    written += reversed;
    buf->busy_pos = next_pos(i);
  }
  return written;
}

int main()
{
  struct buffer buf;
  buf.busy_pos = 0;
  buf.free_pos = 0;
  int count;
  do
  {
    count = read_chars(&buf);
    if (count == -1)
    {
      return -1;
    }
    if (write_chars(&buf) == -1)
    {
      return -1;
    }
  } while (count == buf_size);
  return 0;
}
