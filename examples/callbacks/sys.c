#include "cyclone/types.h"
#include "cyclone/runtime.h"
#include "sys.h"
#include <unistd.h>

/**
 * This variable corresponds to the Scheme function in the generated C file
 * that we wish to call into.
 */
extern object __glo_signal_91done;

/**
 * Code for the C thread. 
 * 
 * In our application we just call the trampoline function to setup a call 
 * into Scheme code. In a real application this thread would probably do 
 * quite a bit more work in C, only calling into Scheme code as necessary.
 */
void *c_thread(void *parent_thd)
{
  printf("Hello from C thread\n");
  sleep(1);
  printf("C calling into SCM\n");

  object obj = scm_call_no_gc(parent_thd, __glo_signal_91done, boolean_t);

  printf("C received: ");
  Cyc_write(NULL, obj, stdout);
  printf("\n");
  return NULL;
}


///////////////////////////////////////////////////////////////////////////////
//
// Should not need to customize below here:
//
///////////////////////////////////////////////////////////////////////////////


/**
 * Scheme function calls into this function when it is done.
 * We store results and longjmp back to where we started, at the
 * bottom of the trampoline (we only jump once).
 */
void after_call_scm(gc_thread_data *thd, int argc, object k, object result)
{
  thd->gc_cont = result;
  longjmp(*(thd->jmp_start), 1);
}

/**
 * Call into Scheme function
 */
void call_scm(gc_thread_data *thd, object fnc, object obj)
{
  mclosure0(after, (function_type)after_call_scm); 
  ((closure)fnc)->fn(thd, 2, fnc, &after, obj);
}

/**
 * Setup a quick-and-dirty thread object and use it to
 * make a call into Scheme code.
 *
 * Note this call is made in a limited way, and is only
 * designed for a quick call. There is no support for
 * performing any memory allocation by the Scheme code
 * other than temporary objects in the nursery. The 
 * returned object will need to either be an immediate
 * or re-allocated (EG: malloc) before returning it
 * to the C layer.
 */
object scm_call_no_gc(gc_thread_data *parent_thd, object fnc, object arg)
{
  long stack_size = 100000;
  char *stack_base = (char *)&stack_size;
  char *stack_traces[MAX_STACK_TRACES];
  gc_thread_data thd = {0};
  jmp_buf jmp;
  thd.jmp_start = &jmp;
  thd.stack_start = stack_base;
#if STACK_GROWTH_IS_DOWNWARD
  thd.stack_limit = stack_base - stack_size;
#else
  thd.stack_limit = stack_base + stack_size;
#endif
  thd.stack_traces = stack_traces;

  thd.thread_id = pthread_self();
  thd.thread_state = CYC_THREAD_STATE_RUNNABLE;

  // Copy parameter objects from the calling thread
  object parent = parent_thd->param_objs; // Unbox parent thread's data
  object child = NULL;
  while (parent) {
    if (thd.param_objs == NULL) {
      alloca_pair(p, NULL, NULL);
      thd.param_objs = p;
      child = thd.param_objs;
    } else {
      alloca_pair(p, NULL, NULL);
      cdr(child) = p;
      child = p;
    }
    alloca_pair(cc, car(car(parent)), cdr(car(parent)));
    car(child) = cc;
    parent = cdr(parent);
  }

  // Setup trampoline and call into Scheme
  //
  // When the Scheme call is done we return result back to C
  //
  // It is very important to know that the result, IF ON THE STACK,
  // is further up the stack than the caller and will be overwritten
  // by subsequent C calls on this thread. Thus the caller will want
  // to immediately create a copy of the object...
  //
  if (!setjmp(*(thd.jmp_start))) {
    call_scm(&thd, fnc, arg);
  } else {
    return(thd.gc_cont);
  }
}

/**
 * Called by Scheme to create the C thread.
 * This is required by sample app since we start
 * from a Scheme thread.
 */
void start_c_thread(gc_thread_data *thd)
{
  pthread_t thread;
  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
  if (pthread_create(&thread, &attr, c_thread, thd)) {
    fprintf(stderr, "Error creating a new thread\n");
    exit(1);
  }
  pthread_attr_destroy(&attr);
}
