#include <string.h>

#define buf_size 10

typedef struct
{
  char in[buf_size];
  int in_size, skip_next;
} buf_t;

static int read_chars(buf_t* buf)
{
  int count = read(0, buf->in + buf->in_size, buf_size - buf->in_size);
  if (count > 0)
  {
    buf->in_size += count;
  }
  return count;
}

static int write_chars(char* out, int len)
{
  int pos = 0;
  while (pos < len)
  {
    int count = write(1, out + pos, len - pos);
    if (count == -1)
    {
      return -1;
    }
    pos += count;
  }
  return 0;
}

static int reverse(char* out, int len)
{
  int left = 0, right = len - 1;
  while (right >= 0 && (out[right] == '\n' || out[right] == '\r'))
  {
    right--;
  }
  while (left < right) 
  {
    char c = out[left];
    out[left] = out[right];
    out[right] = c;
    left++, right--;
  }
  return write_chars(out, len);
}

static int process(buf_t* buf, int last)
{
  int pos = 0;
  int sz = buf->in_size;
  buf->in_size = 0;
  while (pos < sz)
  {
    int start = pos;
    while (pos < sz && buf->in[pos] != '\n')
    {
      pos++;
    }
    if (pos == sz && last)
    {
      pos--;
    }
    if (pos == sz)
    {
      if (start == 0)
      {
        buf->skip_next = 1;
      }
      else
      {
        memmove(buf->in, buf->in + start, pos - start);
        buf->in_size = pos - start;
      }
    }
    else
    {
      if (buf->skip_next)
      {
        buf->skip_next = 0;
      }
      else
      {
        if (reverse(buf->in + start, pos - start + 1) == -1)
        {
          return -1;
        }
      }
      pos++;
    }    
  }
  return 0;
}

int main()
{
  buf_t buf;
  buf.in_size = 0;
  buf.skip_next = 0;
  while (1)
  {
    int count = read_chars(&buf);
    if (count == -1)
    {
      return -1;
    }
    if (process(&buf, count == 0) == -1)
    {
      return -1;
    }
    if (count == 0)
    {
      return 0;
    }
  }
}
