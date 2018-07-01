#include "include/cyclone/types.h"

#define my_obj_is_int(x)  ((unsigned long)(x) & (unsigned long)1)
//
///**
// * Convert from an object to an integer.
// */
//#define my_obj_obj2int(x) ((long)(x)>>1)
//
//+#define sexp_unbox_fixnum(n)   (((sexp_sint_t)((sexp_uint_t)(n) & ~SEXP_FIXNUM_TAG))/(sexp_sint_t)(1uL<<SEXP_FIXNUM_BITS))
#define my_obj_obj2int(n)   (((long)((ulong)(n) & ~1))/(long)(1uL<<1))
//
///**
// * Convert from an integer to an object.
// */
//#define my_obj_int2obj(c) ((void *)((((long)c)<<1) | 1))
#define my_obj_int2obj(n)    ((void *) ((((long)(n))*(long)(1uL<<1)) | 1))
//+#define sexp_make_fixnum(n)    ((sexp) ((((sexp_sint_t)(n))*(sexp_sint_t)(1uL<<SEXP_FIXNUM_BITS)) | SEXP_FIXNUM_TAG))

void main()
{
  object tmp;
  int n = -1;

  tmp = my_obj_int2obj(-1);
  printf("%p\n", my_obj_int2obj(-1));
  printf("%d\n", my_obj_obj2int(tmp));
  printf("%d\n", my_obj_is_int(tmp));

  tmp = my_obj_int2obj(-0x7fffffff);
  printf("%p\n", my_obj_int2obj(-0x7fffffff));
  printf("%d\n", my_obj_obj2int(tmp));
  printf("%d\n", my_obj_is_int(tmp));

  printf("%x\n", (uint)~1);
}
