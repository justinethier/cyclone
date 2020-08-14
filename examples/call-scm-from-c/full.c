#include "cyclone/types.h"
#include "cyclone/runtime.h"
#include "full.h"
#include <ck_pr.h>
#include <unistd.h>

/**
 * These variables correspond to Scheme functions in the generated C file
 * that we wish to call into.
 *
 * These names are from the compiled C files but can also be 
 * generated using icyc:
 *
 *     cyclone> (mangle-global 'signal-done)
 *     "__glo_signal_91done"
 *
 */
extern object __glo_signal_91done;
extern object __glo_sum_91numbers;
extern object __glo_print_91result;

/**
 * Code for the C thread. 
 * 
 * In our application we just call the trampoline function to setup a call 
 * into Scheme code. In a real application this thread would probably do 
 * quite a bit more work in C, only calling into Scheme code as necessary.
 */
void *c_thread(void *parent_thd)
{
  object obj, fnc, args[1];
  printf("Hello from C thread\n");
  printf("C calling into SCM\n");

  fnc = Cyc_scm_call(parent_thd, __glo_sum_91numbers, 0, NULL);

  printf("\nC received: ");
  Cyc_write(NULL, fnc, stdout);
  printf("\n");

  args[0] = fnc;
  obj = Cyc_scm_call(parent_thd, __glo_print_91result, 1, args);

  printf("\nC received: ");
  Cyc_write(NULL, obj, stdout);
  printf("\n");

  args[0] = boolean_t;
  obj = Cyc_scm_call(parent_thd, __glo_signal_91done, 1, args);

  printf("\nC received: ");
  Cyc_write(NULL, obj, stdout);
  printf("\n");
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
