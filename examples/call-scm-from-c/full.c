#include "cyclone/types.h"
#include "cyclone/runtime.h"
#include "full.h"
#include <ck_pr.h>
#include <unistd.h>

/**
 * This variable corresponds to the Scheme function in the generated C file
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

void *Cyc_init_thread(object thread_and_thunk, int argc, object *args);

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

  fnc = scm_call_with_gc(parent_thd, __glo_sum_91numbers, 0, NULL);

  printf("\nC received: ");
  Cyc_write(NULL, fnc, stdout);
  printf("\n");

  args[0] = fnc;
  obj = scm_call_with_gc(parent_thd, __glo_print_91result, 1, args);

  printf("\nC received: ");
  Cyc_write(NULL, obj, stdout);
  printf("\n");

  args[0] = boolean_t;
  obj = scm_call_with_gc(parent_thd, __glo_signal_91done, 1, args);

  printf("\nC received: ");
  Cyc_write(NULL, obj, stdout);
  printf("\n");
  return NULL;
}


///////////////////////////////////////////////////////////////////////////////
//
// Should not need to customize below here:
//
///////////////////////////////////////////////////////////////////////////////

void cleanup_and_return(gc_thread_data *thd, int argc, object k, object result)
{
  // Cleaup thread object per Cyc_exit_thread
  gc_remove_mutator(thd);
  ck_pr_cas_int((int *)&(thd->thread_state), CYC_THREAD_STATE_RUNNABLE,
                CYC_THREAD_STATE_TERMINATED);

  // Return to local C caller
  vector vec = thd->scm_thread_obj;
  gc_thread_data *local = opaque_ptr(vec->elements[4]);
  local->gc_cont = result;
  longjmp(*(local->jmp_start), 1);
}

/**
 * Scheme function calls into this function when it is done.
 * We store results and longjmp back to where we started, at the
 * bottom of the trampoline (we only jump once).
 */
void after_call_scm(gc_thread_data *thd, int argc, object k, object result)
{
  mclosure0(clo, cleanup_and_return);
  object buf[1]; buf[0] = result;
  GC(thd, &clo, buf, 1);
}

/**
 * Call into Scheme function
 */
//void call_scm(gc_thread_data *thd, object fnc, object obj)
//{
//  mclosure0(after, (function_type)after_call_scm); 
//  ((closure)fnc)->fn(thd, 2, fnc, &after, obj);
//}
//
//void call_thunk(void *data, int argc, object self, object k)
//{
//
//}

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
object scm_call_with_gc(gc_thread_data *parent_thd, object fnc, int argc, object *args)
{
  jmp_buf l;
  gc_thread_data local;
  local.gc_cont = NULL;
  local.jmp_start = &l;

  gc_thread_data *td = malloc(sizeof(gc_thread_data));
  gc_add_new_unrunning_mutator(td); /* Register this thread */
  make_c_opaque(co, td);
  make_utf8_string(NULL, name_str, "");

  make_c_opaque(co_parent_thd, parent_thd);
  make_c_opaque(co_this_thd, &local);
  mclosure0(after, (function_type)after_call_scm); 

  make_empty_vector(vec);
  vec.num_elements = 7;
  vec.elements = alloca(sizeof(object) * 5);
  vec.elements[0] = find_or_add_symbol("cyc-thread-obj");
  vec.elements[1] = fnc;
  vec.elements[2] = &co;
  vec.elements[3] = &name_str;
  vec.elements[4] = &co_this_thd; //boolean_f;
  vec.elements[5] = &co_parent_thd;
  vec.elements[6] = &after;

  make_pair(thread_and_thunk, &vec, fnc); // TODO: OK we are not clearing vec[5]? I think so... 

  if (!setjmp(*(local.jmp_start))) {
    Cyc_init_thread(&thread_and_thunk, argc, args);
  }

  return local.gc_cont;
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
