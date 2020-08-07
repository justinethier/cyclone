#include "cyclone/types.h"
#include "cyclone/runtime.h"
#include "sys.h"
#include <unistd.h>

extern object __glo_signal_91done;

// Scheme function calls into this when it is done.
// We store results and longjmp back to where we started.
void after_call_scm(gc_thread_data *thd, int argc, object k, object result)
{
  // TODO: need to check this, does NOT work if result is a stack obj!!
  thd->gc_cont = result;
  longjmp(*(thd->jmp_start), 1);
}

// Call into Scheme function
void call_scm(gc_thread_data *thd, object obj)
{
  mclosure0(after, (function_type)after_call_scm); 
  ((closure)__glo_signal_91done)->fn(thd, 2, __glo_signal_91done, &after, obj);
}

void wait_and_signal(gc_thread_data *thd)
{
  // print, wait couple secs, signal SCM side
  printf("Hello from C thread\n");
  sleep(1);
  printf("C calling into SCM\n");
  call_scm(thd, boolean_t);
}

void c_trampoline(void)
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
  thd.stack_trace_idx = 0;
  thd.stack_prev_frame = NULL;
  thd.gc_cont = NULL;

  // TODO: many more initializations required, need to figure out
  // minimum set to optimize for micro / short / long calls into Scheme.
  // We will make different assumptions in each case


  // TODO: setup exception handler and parameter objects

  // TODO: test this actually works, throw an error in test program
  thd.exception_handler_stack = NULL; // Default
  thd.param_objs = NULL;

  if (!setjmp(*(thd.jmp_start))) {
    wait_and_signal(&thd);
  } else {
    printf("Received: ");
    Cyc_write(&thd, thd.gc_cont, stdout);
    printf("\n");
  }
}

void *c_thread(void *arg)
{
  c_trampoline();
  return NULL;
}

void start_c_thread(void)
{
  pthread_t thread;
  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
  if (pthread_create(&thread, &attr, c_thread, NULL)) {
    fprintf(stderr, "Error creating a new thread\n");
    exit(1);
  }
  pthread_attr_destroy(&attr);
}
