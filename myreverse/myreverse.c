#include <string.h>

#define buf_size 10

typedef struct buffer
{
  char in[buf_size], out[buf_size];
  int in_size, out_size, skip_next;
} buf_t;

int read_chars(buf_t* buf)
{
  int count = read(0, buf->in, buf_size);
  if (count >= 0)
  { 
    buf->in_size = count;
  }
  return count;
}

int reverse(char* out, int len)
{
  int i;
  int act_len = len;
  while (act_len > 0 && (out[act_len - 1] == '\n' || out[act_len - 1] == '\r'))
  {
    act_len--;
  }
  for (i = 0; i < act_len / 2; ++i)
  {
    char c = out[i];
    out[i] = out[act_len - 1 - i];
    out[act_len - 1 - i] = c;
  }
  return write(1, out, len);
}

int write_chars(buf_t* buf)
{
  int pos = 0;
  while (pos < buf->in_size)
  {
    int start = pos;
    while (pos < buf->in_size && buf->in[pos] != '\n')
    {
      pos++;
    }
    if (pos == buf->in_size)
    {
      if (start == 0)
      {
        buf->skip_next = 1;
        buf->out_size = 0;
      }
      else
      {
        memcpy(buf->out, buf->in + start, pos - start);
        buf->out_size = pos - start;
      }
    }
    else
    {
      if (buf->skip_next || buf->out_size + pos - start + 1 > buf_size)
      {
        buf->skip_next = 0;
      }
      else
      {
        memcpy(buf->out + buf->out_size, buf->in + start, pos - start + 1);
        if (reverse(buf->out, buf->out_size + pos - start + 1) == -1)
        {
          return -1;
        }
      }
      buf->out_size = 0;
      pos++;
    }    
  }
  return 0;
}

int main()
{
  buf_t buf;
  buf.in_size = 0;
  buf.out_size = 0;
  buf.skip_next = 0;
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
  if (buf.out_size > 0)
  {
    if (reverse(buf.out, buf.out_size) == -1)
    {
      return -1;
    }
  }
  return 0;
}
