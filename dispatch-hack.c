
#include "cyclone/types.h"
#include "cyclone/runtime.h"

#ifdef C_HACKED_APPLY
# if defined(C_MACOSX) || defined(__MINGW32__) || defined(__CYGWIN__)
extern void C_do_apply_hack(void *proc, C_word *args, int count) C_noret;
# else
extern void _C_do_apply_hack(void *proc, C_word *args, int count) C_noret;
#  define C_do_apply_hack _C_do_apply_hack
# endif
#endif

/*
#ifdef C_HACKED_APPLY
  C_word *buf = C_temporary_stack_limit;
  void *proc;
#endif

  if(c < 4) C_bad_min_argc(c, 4);

  if(C_immediatep(fn) || C_header_bits(fn) != C_CLOSURE_TYPE) {
    barf(C_NOT_A_CLOSURE_ERROR, "apply", fn);
  }

  va_start(v, fn);

  for(i = n; i > 1; --i) {
    x = va_arg(v, C_word);
#ifdef C_HACKED_APPLY
    *(buf++) = x;
#endif

#ifdef C_HACKED_APPLY
    if(buf >= C_temporary_stack_bottom) barf(C_TOO_MANY_PARAMETERS_ERROR, "apply");

    *(buf++) = x;
#else

#ifdef C_HACKED_APPLY
  // 3 additional args + 1 slot for stack-pointer + two for stack-alignment to 16 bytes 
  buf = alloca((n + 6) * sizeof(C_word));
# ifdef __x86_64__
  // XXX Shouldn't this check for C_SIXTY_FOUR in general? 
  buf = (void *)C_align16((C_uword)buf);
# endif
  buf[ 0 ] = n + 2;
  buf[ 1 ] = fn;
  buf[ 2 ] = k;
  C_memcpy(&buf[ 3 ], C_temporary_stack_limit, n * sizeof(C_word));
  proc = (void *)C_block_item(fn, 0);
  C_do_apply_hack(proc, buf, n + 3);
#else
  C_do_apply(n, fn, k);
#endif
*/

void do_dispatch(void *data, int argc, function_type func, object clo, object *b) {
//  switch(argc) {case 0:func(data,0,clo);
//case 1:func(data,1,clo,*(b+0));
//
//  default:
//  {
//   char buf[1024];
//   snprintf(buf, 1023, "Unhandled number of function arguments: %d\n", argc); 
//   Cyc_rt_raise_msg(data, buf);
//  }
//  }
}
