#include <limits.h>
#include <strng.h>

void slicetest(jt_strng* mystr, int start, int end)
{
  jt_strng* slice = jt_strng_slice(mystr, start, end);
  printf("%s[%d:%d] = %s\n", jt_strng_c(mystr), start, end, jt_strng_c(slice));
  jt_strng_delete(slice);
}

int main(int argc, char* argv[])
{
  jt_strng* mystr = jt_strng_new("/var/tmp/");
  jt_strng* leaf = jt_strng_new("banana.c");
  jt_list* list;
  int i;
  
  printf("mystr length: %d, leaf length: %d\n", jt_strng_len(mystr),
    jt_strng_len(leaf));
  
  jt_strng_append(mystr, leaf);
  
  printf("%s\n", jt_strng_c(mystr));
  jt_strng_delete(mystr);

  mystr = jt_strng_new("/var/foo/");
  
  jt_strng_append_c(mystr, "cactus.txt");
  
  printf("%s\n", jt_strng_c(mystr));
  
  for (i=0; i<10; i++)
  {
    jt_strng_append_c(mystr, "*");
  }

  printf("%s\n", jt_strng_c(mystr));
  
  for (i=0; i<10; i++)
  {
    jt_strng* foo = jt_strng_new("+");
    jt_strng_append(mystr, foo);
    jt_strng_delete(foo);
  }
  
  printf("%s\n", jt_strng_c(mystr));
  
  jt_strng_delete(mystr);
  jt_strng_delete(leaf);
  
  list = jt_strng_list_new("Hello", "there", "cabbage", 0);
  mystr = jt_strng_list_join(list, "/");
  jt_strng_list_delete(list);

  printf("%s\n", jt_strng_c(mystr));
  
  jt_strng_delete(mystr);

  list = jt_strng_list_new("Hello", 0);
  mystr = jt_strng_list_join(list, "/");
  jt_strng_list_delete(list);

  printf("%s\n", jt_strng_c(mystr));
  
  jt_strng_delete(mystr);

  mystr = jt_strng_new("fox");

  slicetest(mystr, 0, 3);
  slicetest(mystr, 1, 3);
  slicetest(mystr, 0, 2);
  slicetest(mystr, -1, STRNG_OPEN);
  slicetest(mystr, STRNG_OPEN, 1);
  slicetest(mystr, STRNG_OPEN, -1);
  slicetest(mystr, 2, 2);
  slicetest(mystr, 2, 1);

  jt_strng_delete(mystr);

  mystr = jt_strng_new("Hello/there/kiddies");
  list = jt_strng_list_split(mystr, "/");
  jt_strng_delete(mystr);
  {
    jt_list* walk;
    for (walk=list; walk; walk=walk->prev)
    {
      printf("%s\n", jt_strng_c(walk->data));
    }
  }
  mystr = jt_strng_list_join(list, "\\");
  jt_strng_list_delete(list);
  printf("%s\n", jt_strng_c(mystr));
  jt_strng_delete(mystr);

  printf("INT_MAX is %d\n", INT_MAX);

  return 0;
}
