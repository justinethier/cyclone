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
static void Cyc_main(long stack_size, char *stack_base);
static void Cyc_heap_init(long heap_size);

static void Cyc_heap_init(long heap_size)
{
  /* Initialize stack trace table
     TODO: will eventually be relocated to a per-thread operation */
  Cyc_st_init();

  /* Allocate heap area for second generation. */
#if DEBUG_SHOW_DIAG
  printf("main: Allocating and initializing heap...\n");
#endif
  Cyc_heap = gc_heap_create(heap_size / 2, 0, 0);
  Cyc_mutators = malloc(sizeof(gc_thread_data *) * Cyc_num_mutators);
  Cyc_num_mutators = 1; // TODO: alloca this using a vpbuffer, or maybe another type of data structure
}

static void Cyc_main (long stack_size, char *stack_base)
{
  mclosure0(clos_halt,&Cyc_halt);  // Halt if final closure is reached
  mclosure0(entry_pt,&c_entry_pt); // First function to execute
  Cyc_mutators[0] = malloc(sizeof(gc_thread_data));
  gc_thread_data_init(Cyc_mutators[0], 0, stack_base, stack_size);
  
  Cyc_mutators[0]->gc_cont = &entry_pt;
  Cyc_mutators[0]->gc_args[0] = &clos_halt;
  Cyc_mutators[0]->gc_num_args = 1;

  /* Tank, load the jump program... */
  setjmp(*(Cyc_mutators[0]->jmp_start));
// TODO: note, if longjmp is passed 0 it will return 1. need to
// account for that there (add one to mut_num) and here (subtract 1 unless 0)
#if DEBUG_GC
  printf("Done with GC\n");
#endif

// JAE - note for the general case, setjmp will return the data pointer's addy
  if (type_of(Cyc_mutators[0]->gc_cont) == cons_tag || prim(Cyc_mutators[0]->gc_cont)) {
    Cyc_apply_from_buf(Cyc_mutators[0], Cyc_mutators[0]->gc_num_args, Cyc_mutators[0]->gc_cont, Cyc_mutators[0]->gc_args);
  } else {
    do_dispatch(Cyc_mutators[0], Cyc_mutators[0]->gc_num_args, ((closure)(Cyc_mutators[0]->gc_cont))->fn, Cyc_mutators[0]->gc_cont, Cyc_mutators[0]->gc_args);
  }

  printf("Internal error: should never have reached this line\n"); exit(0);
}

#endif /* CYCLONE_RUNTIME_MAIN_H */
