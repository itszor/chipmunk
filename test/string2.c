#ifdef __i386__
#include <stdio.h>

void writestring(char* x)
{
  printf(x);
}
#endif

void writechar(int x)
{
#ifdef __i386__
  putchar(x);
#else
  _serial_output_byte(x);
#endif
}

void writeint(int x)
{
  if (x>9) writeint(x/10);
  writechar('0'+x%10);
}
