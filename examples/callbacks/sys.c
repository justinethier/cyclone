#include "cyclone/types.h"
#include "cyclone/runtime.h"
#include "sys.h"
#include <unistd.h>

/**
 * This variable corresponds to the Scheme function (in the generated C file)
 * that we wish to call into.
 */
extern object __glo_signal_91done;

/**
 * Scheme function calls into this function when it is done.
 * We store results and longjmp back to where we started, at the
 * bottom of the trampoline (we only jump once).
 */
void after_call_scm(gc_thread_data *thd, int argc, object k, object result)
{
  // TODO: need to check this, does NOT work if result is a stack obj!!
  thd->gc_cont = result;
  longjmp(*(thd->jmp_start), 1);
}

/**
 * Call into Scheme function
 */
void call_scm(gc_thread_data *thd, object obj)
{
  printf("Hello from C thread\n");
  sleep(1);
  printf("C calling into SCM\n");

  mclosure0(after, (function_type)after_call_scm); 
  ((closure)__glo_signal_91done)->fn(thd, 2, __glo_signal_91done, &after, obj);
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
void c_trampoline(gc_thread_data *parent_thd, function_type fnc, object arg)
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
  thd.stack_traces = stack_traces; //calloc(MAX_STACK_TRACES, sizeof(char *));
  //thd.stack_trace_idx = 0;
  //thd.stack_prev_frame = NULL;
  //thd.gc_cont = NULL;

  thd.thread_id = pthread_self();

  //thd.exception_handler_stack = NULL; // Default

  // Copy thread params from the calling thread
  object parent = parent_thd->param_objs; // Unbox parent thread's data
  object child = NULL;
  //thd.param_objs = NULL;
  while (parent) {
    if (thd.param_objs == NULL) {
      alloca_pair(p, NULL, NULL);
      thd.param_objs = p;
      //thd.param_objs = gc_alloc_pair(thd, NULL, NULL);
      child = thd.param_objs;
    } else {
      //pair_type *p = gc_alloc_pair(thd, NULL, NULL);
      alloca_pair(p, NULL, NULL);
      cdr(child) = p;
      child = p;
    }
    //car(child) = gc_alloc_pair(thd, car(car(parent)), cdr(car(parent)));
    alloca_pair(cc, car(car(parent)), cdr(car(parent)));
    car(child) = cc;
    parent = cdr(parent);
  }
  // Done initializing parameter objects


  if (!setjmp(*(thd.jmp_start))) {
    //wait_and_signal(&thd);
    fnc(&thd, arg);
  } else {
    printf("Received: ");
    Cyc_write(&thd, thd.gc_cont, stdout);
    printf("\n");
  }
}

/**
 * C thread. In our application we just call the trampoline function
 * to setup a call into Scheme code. In a real application this thread
 * could do quite a bit more work in C, occasionally calling into 
 * Scheme code as necessary.
 */
void *c_thread(void *arg)
{
  c_trampoline(arg, (function_type)call_scm, boolean_t);
  return NULL;
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
