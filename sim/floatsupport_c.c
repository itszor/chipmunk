/* Floating-point operations -- single precision accuracy,
 * double-precision format
 */

#include <math.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/alloc.h>

typedef union {
  double as_double;
  struct {
    unsigned int part[2];
  } as_int;
} doublebits;

typedef union {
  float as_float;
  unsigned int as_int;
} floatbits;

CAMLprim value float_add_c(value ml_a, value ml_b)
{
  float res;
  float op1, op2;
  CAMLparam2(ml_a, ml_b);
  CAMLlocal1(ml_res);
  
  op1 = Double_val(ml_a);
  op2 = Double_val(ml_b);
  
  res = op1+op2;
  
  ml_res = copy_double(res);
  
  CAMLreturn(ml_res);
}

CAMLprim value float_sub_c(value ml_a, value ml_b)
{
  float res;
  float op1, op2;
  CAMLparam2(ml_a, ml_b);
  CAMLlocal1(ml_res);
  
  op1 = Double_val(ml_a);
  op2 = Double_val(ml_b);
  
  res = op1-op2;
  
  ml_res = copy_double(res);
  
  CAMLreturn(ml_res);
}

CAMLprim value float_mul_c(value ml_a, value ml_b)
{
  float res;
  float op1, op2;
  CAMLparam2(ml_a, ml_b);
  CAMLlocal1(ml_res);
  
  op1 = Double_val(ml_a);
  op2 = Double_val(ml_b);
  
  res = op1*op2;
  
  ml_res = copy_double(res);
  
  CAMLreturn(ml_res);
}

CAMLprim value float_div_c(value ml_a, value ml_b)
{
  float res;
  float op1, op2;
  CAMLparam2(ml_a, ml_b);
  CAMLlocal1(ml_res);
  
  op1 = Double_val(ml_a);
  op2 = Double_val(ml_b);
  
  res = op1/op2;
  
  ml_res = copy_double(res);
  
  CAMLreturn(ml_res);
}

CAMLprim value float_eq_c(value ml_a, value ml_b)
{
  int res;
  float op1, op2;
  CAMLparam2(ml_a, ml_b);
  CAMLlocal1(ml_res);
  
  op1 = Double_val(ml_a);
  op2 = Double_val(ml_b);
  
  res = (op1==op2) ? -1 : 0;
  
  ml_res = copy_int32(res);
  
  CAMLreturn(ml_res);
}

CAMLprim value float_le_c(value ml_a, value ml_b)
{
  int res;
  float op1, op2;
  CAMLparam2(ml_a, ml_b);
  CAMLlocal1(ml_res);
  
  op1 = Double_val(ml_a);
  op2 = Double_val(ml_b);
  
  res = (op1<=op2) ? -1 : 0;
  
  ml_res = copy_int32(res);
  
  CAMLreturn(ml_res);
}

CAMLprim value float_lt_c(value ml_a, value ml_b)
{
  int res;
  float op1, op2;
  CAMLparam2(ml_a, ml_b);
  CAMLlocal1(ml_res);
  
  op1 = Double_val(ml_a);
  op2 = Double_val(ml_b);
  
  res = (op1<op2) ? -1 : 0;
  
  ml_res = copy_int32(res);
  
  CAMLreturn(ml_res);
}

CAMLprim value float_mov_c(value ml_a)
{
  float op1;
  CAMLparam1(ml_a);
  CAMLlocal1(ml_res);
  
  op1 = Double_val(ml_a);
  
  ml_res = copy_double(op1);
  
  CAMLreturn(ml_res);
}

CAMLprim value float_neg_c(value ml_a)
{
  float op1;
  float res;
  CAMLparam1(ml_a);
  CAMLlocal1(ml_res);
  
  op1 = Double_val(ml_a);
  
  res = -op1;
  
  ml_res = copy_double(res);
  
  CAMLreturn(ml_res);
}

CAMLprim value float_abs_c(value ml_a)
{
  float op1;
  float res;
  CAMLparam1(ml_a);
  CAMLlocal1(ml_res);
  
  op1 = Double_val(ml_a);
  
  res = fabsf(op1);
  
  ml_res = copy_double(res);
  
  CAMLreturn(ml_res);
}

CAMLprim value float_sqr_c(value ml_a)
{
  float op1;
  float res;
  CAMLparam1(ml_a);
  CAMLlocal1(ml_res);
  
  op1 = Double_val(ml_a);
  
  res = sqrt(op1);
  
  ml_res = copy_double(res);
  
  CAMLreturn(ml_res);
}

CAMLprim value float_fix_c(value ml_a)
{
  float op1;
  CAMLparam1(ml_a);
  CAMLlocal1(ml_res);
  
  op1 = Double_val(ml_a);
    
  ml_res = copy_int32((int)op1);
  
  CAMLreturn(ml_res);
}

CAMLprim value float_flt_c(value ml_a)
{
  int op1;
  CAMLparam1(ml_a);
  CAMLlocal1(ml_res);
  
  op1 = Int32_val(ml_a);
    
  ml_res = copy_double((float)op1);
  
  CAMLreturn(ml_res);
}

CAMLprim value float_to_int32_c(value ml_a)
{
  floatbits op1;
  CAMLparam1(ml_a);
  CAMLlocal1(ml_res);
  
  op1.as_float = Double_val(ml_a);
    
  ml_res = copy_int32(op1.as_int);
  
  CAMLreturn(ml_res);
}

CAMLprim value float_from_int32_c(value ml_a)
{
  floatbits op1;
  CAMLparam1(ml_a);
  CAMLlocal1(ml_res);
  
  op1.as_int = Int32_val(ml_a);
  
  ml_res = copy_double(op1.as_float);
  
  CAMLreturn(ml_res);
}

CAMLprim value double_to_int32_0_c(value ml_a)
{
  doublebits op1;
  CAMLparam1(ml_a);
  CAMLlocal1(ml_res);
  
  op1.as_double = Double_val(ml_a);
    
  ml_res = copy_int32(op1.as_int.part[0]);
  
  CAMLreturn(ml_res);
}

CAMLprim value double_to_int32_1_c(value ml_a)
{
  doublebits op1;
  CAMLparam1(ml_a);
  CAMLlocal1(ml_res);
  
  op1.as_double = Double_val(ml_a);
    
  ml_res = copy_int32(op1.as_int.part[1]);
  
  CAMLreturn(ml_res);
}

CAMLprim value double_from_int32s_c(value ml_a, value ml_b)
{
  doublebits op1;
  CAMLparam2(ml_a, ml_b);
  CAMLlocal1(ml_res);
  
  op1.as_int.part[0] = Int32_val(ml_a);
  op1.as_int.part[1] = Int32_val(ml_b);
  
  ml_res = copy_double(op1.as_double);
  
  CAMLreturn(ml_res);
}
