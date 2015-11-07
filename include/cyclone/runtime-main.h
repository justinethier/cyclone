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

 /* Initialize stack trace table */
 Cyc_st_init();

 {
  /* Setup first function to execute */
  mclosure0(entry_pt,&c_entry_pt);

  /* Allocate heap area for second generation. */
  /* Use calloc instead of malloc to assure pages are in main memory. */
#if DEBUG_SHOW_DIAG
  printf("main: Allocating and initializing heap...\n");
#endif

  Cyc_heap = gc_heap_create(heap_size / 2, 0, 0);
  Cyc_num_mutators = 1; // TODO: alloca this using a vpbuffer, or maybe another type of data structure
  Cyc_mutators = malloc(sizeof(gc_thread_data *) * Cyc_num_mutators);

// TODO: from here, break this out into a separate function that
// could spin up additional threads
// would need mutator_num, stack args.
// don't want to waste stack space though, so maybe inits above
// get moved to the caller of Cyc_main, and Cyc_main becomes
// that separate function
// int mutator_num = 0;
  Cyc_mutators[0] = malloc(sizeof(gc_thread_data));
  gc_thread_data_init(Cyc_mutators[0], 0, stack_base, stack_size);
  
  Cyc_mutators[0]->gc_cont = &entry_pt;
  Cyc_mutators[0]->gc_args[0] = &clos_halt;
  Cyc_mutators[0]->gc_num_args = 1;

  /* Tank, load the jump program... */
  setjmp(*(Cyc_mutators[0]->jmp_start));
#if DEBUG_GC
  printf("Done with GC\n");
#endif

// JAE - note for the general case, setjmp will return the data pointer's addy
  if (type_of(Cyc_mutators[0]->gc_cont) == cons_tag || prim(Cyc_mutators[0]->gc_cont)) {
    Cyc_apply_from_buf(Cyc_mutators[0], Cyc_mutators[0]->gc_num_args, Cyc_mutators[0]->gc_cont, Cyc_mutators[0]->gc_args);
  } else {
    do_dispatch(Cyc_mutators[0], Cyc_mutators[0]->gc_num_args, ((closure)(Cyc_mutators[0]->gc_cont))->fn, Cyc_mutators[0]->gc_cont, Cyc_mutators[0]->gc_args);
  }

  printf("Internal error: should never have reached this line\n"); exit(0);}}

#endif /* CYCLONE_RUNTIME_MAIN_H */
