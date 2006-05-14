#include "sstring.h"

void loop(int);

void loop(int c)
{
  int d;
  for (d=0; d<c; d++)
  {
    writeint(d);
    writechar('\n');
  }
}

int main(int argc, char* argv[])
{
  char* x = "Hello world\n";
  writestring(x);
  writeint(12345);
  writechar('\n');
  loop(1000);
  return 0;
}
