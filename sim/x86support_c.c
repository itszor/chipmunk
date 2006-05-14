#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/alloc.h"

#define FUNCTION_ADDR(X) \
  CAMLprim value X##_addr(void) \
  { \
    CAMLparam0(); \
    CAMLlocal1(ml_addr); \
    unsigned int c_addr = (unsigned int) &(X); \
    ml_addr = copy_int32(c_addr); \
    CAMLreturn(ml_addr); \
  }

void int32_array_put(value ml_array, value ml_pos, value ml_val)
{
  CAMLparam3(ml_array, ml_pos, ml_val);
  unsigned int c_val;
  
  c_val = Int32_val(ml_val);

  modify(&Field(ml_array, Int_val(ml_pos)), copy_int32(c_val));

  CAMLreturn0;
}

FUNCTION_ADDR(int32_array_put);

CAMLprim value int32_array_get(value ml_array, value ml_pos)
{
  CAMLparam2(ml_array, ml_pos);
  CAMLlocal2(ml_i32ptr, ml_val);
  unsigned int c_val;
  
  ml_i32ptr = Field(ml_array, Int_val(ml_pos));
  c_val = Int32_val(ml_i32ptr);
  
  ml_val = copy_int32(c_val);
  
  CAMLreturn(ml_val);
}

FUNCTION_ADDR(int32_array_get);

FUNCTION_ADDR(copy_int32);

void invoke1(value ml_code, value ml_arg1)
{
  CAMLparam2(ml_code, ml_arg1);
  register int code asm ("edi") = (int)String_val(ml_code);
  register int localroots asm ("eax") = (int) &local_roots;
  register int arg1 asm ("ecx") = (int) &ml_arg1;
  register int res asm ("eax");
  
  asm volatile ("call *%%edi" : "=r" (res)
                              : "r" (localroots), "r" (code), "r" (arg1)
                              : "edx", "ebx", "ebp", "esi", "memory");
  
  CAMLreturn0;
}

void invoke2(value ml_code, value ml_arg1, value ml_arg2)
{
  CAMLparam2(ml_code, ml_arg1);
  register int code asm ("edi") = (int)String_val(ml_code);
  register int localroots asm ("eax") = (int) &local_roots;
  register int arg1 asm ("ecx") = (int) &ml_arg1;
  register int arg2 asm ("edx") = (int) &ml_arg2;
  register int res asm ("eax");
  
  asm volatile ("call *%%edi" : "=r" (res)
                              : "r" (localroots), "r" (code), "r" (arg1),
                                "r" (arg2)
                              : "ebx", "ebp", "esi", "memory");
  
  CAMLreturn0;
}

void invoke3(value ml_code, value ml_arg1, value ml_arg2, value ml_arg3)
{
  CAMLparam2(ml_code, ml_arg1);
  register int code asm ("edi") = (int)String_val(ml_code);
  register int localroots asm ("eax") = (int) &local_roots;
  register int arg1 asm ("ecx") = (int) &ml_arg1;
  register int arg2 asm ("edx") = (int) &ml_arg2;
  register int arg3 asm ("ebx") = (int) &ml_arg3;
  register int res asm ("eax");
  
  asm volatile ("call *%%edi" : "=r" (res)
                              : "r" (localroots), "r" (code), "r" (arg1),
                                "r" (arg2), "r" (arg3)
                              : "ebp", "esi", "memory");
  
  CAMLreturn0;
}

void invoke4(value ml_code, value ml_arg1, value ml_arg2, value ml_arg3,
             value ml_arg4)
{
  CAMLparam2(ml_code, ml_arg1);
  register int code asm ("edi") = (int)String_val(ml_code);
  register int localroots asm ("eax") = (int) &local_roots;
  register int arg1 asm ("ecx") = (int) &ml_arg1;
  register int arg2 asm ("edx") = (int) &ml_arg2;
  register int arg3 asm ("ebx") = (int) &ml_arg3;
  register int arg4 asm ("esi") = (int) &ml_arg4;
  register int res asm ("eax");
  
  asm volatile ("call *%%edi" : "=r" (res)
                              : "r" (localroots), "r" (code), "r" (arg1),
                                "r" (arg2), "r" (arg3), "r" (arg4)
                              : "ebp", "memory");
  
  CAMLreturn0;
}

void test(value ml_arg)
{
  CAMLparam1(ml_arg);
  CAMLlocal1(ml_foo);
  
  ml_foo = int32_array_get(ml_arg, Int_val(4));
  int32_array_put(ml_arg, Int_val(4), copy_int32(Int32_val(ml_foo)+1));
  
  CAMLreturn0;
}
