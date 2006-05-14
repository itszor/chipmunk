#include "sstring.h"

int myfun(int a, int b)
{
  return a-b;
}

int main(int argc, char* argv[])
{
  int x;
  x = myfun(5, 7);
  writeint(x);
  return 0;
}
