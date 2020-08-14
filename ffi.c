/** 
 * Cyclone Scheme
 * https://github.com/justinethier/cyclone
 *
 * Copyright (c) 2020, Justin Ethier
 * All rights reserved.
 *
 * FFI module to support calling Scheme code from C.
 */

#include "cyclone/types.h"
#include "cyclone/runtime.h"
#include <ck_pr.h>
#include <unistd.h>

void *Cyc_init_thread(object thread_and_thunk, int argc, object *args);

/**
 * After the Scheme call finishes, we wind down the GC / Heap used
 * for the call and perform a minor GC to ensure any returned object
 * is on the heap and safe to use.
 */
static void Cyc_return_from_scm_call(gc_thread_data *thd, int argc, object k, object result)
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
static void Cyc_after_scm_call(gc_thread_data *thd, int argc, object k, object result)
{
  mclosure0(clo, Cyc_return_from_scm_call);
  object buf[1]; buf[0] = result;
  GC(thd, &clo, buf, 1);
}

/**
 * Setup a full call into Scheme code.
 *
 * This is somewhat expensive as we setup a new thread object and
 * register it with our GC. On the other hand the called code
 * can do anything "normal" Scheme code does, and any returned
 * objects will be on the heap and available for use by the caller.
 */
object Cyc_scm_call(gc_thread_data *parent_thd, object fnc, int argc, object *args)
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
  mclosure0(after, (function_type)Cyc_after_scm_call); 

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

///////////////////////////////////////////////////////////////////////////////
//
// Simplified interface with no support for GC
//
///////////////////////////////////////////////////////////////////////////////

/**
 * Scheme function calls into this function when it is done.
 * We store results and longjmp back to where we started, at the
 * bottom of the trampoline (we only jump once).
 */
static void no_gc_after_call_scm(gc_thread_data *thd, int argc, object k, object result)
{
  thd->gc_cont = result;
  longjmp(*(thd->jmp_start), 1);
}

/**
 * Call into Scheme function
 */
static void no_gc_call_scm(gc_thread_data *thd, object fnc, object obj)
{
  mclosure0(after, (function_type)no_gc_after_call_scm); 
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
object Cyc_scm_call_no_gc(gc_thread_data *parent_thd, object fnc, object arg)
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
    no_gc_call_scm(&thd, fnc, arg);
  }

  return(thd.gc_cont);
}
