/** 
 * Cyclone Scheme
 * Copyright (c) 2014, Justin Ethier
 * All rights reserved.
 *
 * This file contains the C runtime code used only by the main program module.
 */

#ifndef CYCLONE_RUNTIME_MAIN_H
#define CYCLONE_RUNTIME_MAIN_H

#include "cyclone/types.h"

long global_stack_size = 0;
long global_heap_size = 0;

static void c_entry_pt(void *,int,closure,closure);
static void Cyc_main(long stack_size,long heap_size,char *stack_base);

static void Cyc_main (stack_size,heap_size,stack_base)
     long stack_size,heap_size; char *stack_base;
{char in_my_frame;
 mclosure0(clos_halt,&Cyc_halt);  /* Halt program if final closure is reached */
 gc_ans[0] = &clos_halt;
 gc_num_ans = 1;

 /* Initialize stack trace table */
 Cyc_st_init();

 {
  /* Setup first function to execute */
  mclosure0(entry_pt,&c_entry_pt);
  gc_cont = &entry_pt;

  /* Allocate heap area for second generation. */
  /* Use calloc instead of malloc to assure pages are in main memory. */
#if DEBUG_SHOW_DIAG
  printf("main: Allocating and initializing heap...\n");
#endif

  Cyc_heap = gc_heap_create(heap_size / 2, 0, 0);
  Cyc_thread = malloc(sizeof(gc_thread_data));
  gc_thread_data_init(Cyc_thread, stack_base, stack_size);
  

  // JAE TODO: clean up below (and all of this old code, really)
  bottom = calloc(1,heap_size);
  allocp = (char *) ((((long) bottom)+7) & -8);
  alloc_end = allocp + heap_size - 8;
  
  dhallocp = dhbottom = calloc(1, heap_size);
  dhalloc_limit = dhallocp + (long)((heap_size - 8) * 0.90);
  dhalloc_end = dhallocp + heap_size - 8;
#if DEBUG_SHOW_DIAG
  printf("main: heap_size=%ld  allocp=%p  alloc_end=%p\n",
         (long) heap_size,(void *)allocp,(void *)alloc_end);
  printf("main: Try a larger heap_size if program bombs.\n");
  printf("Starting...\n");
#endif
  start = clock(); /* Start the timing clock. */

  /* Tank, load the jump program... */
  setjmp(jmp_main);
#if DEBUG_GC
  printf("Done with GC\n");
#endif

// JAE - note for the general case, setjmp will return the data pointer's addy
  if (type_of(gc_cont) == cons_tag || prim(gc_cont)) {
    Cyc_apply_from_buf(Cyc_thread, gc_num_ans, gc_cont, gc_ans);
  } else {
    do_dispatch(Cyc_thread, gc_num_ans, ((closure)gc_cont)->fn, gc_cont, gc_ans);
  }

  printf("Internal error: should never have reached this line\n"); exit(0);}}

#endif /* CYCLONE_RUNTIME_MAIN_H */
