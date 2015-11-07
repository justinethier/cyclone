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
  // TODO: alloca this using a vpbuffer, or maybe another type of data structure??
  // Will need this list for later use, but only by the collector thread. so it would be
  // nice if there was a way to allocate mutators that avoids expensive synchronization...
  // need to think on this when adding thread support, after upgrading the collector
  Cyc_num_mutators = 1; 
  Cyc_mutators = calloc(Cyc_num_mutators, sizeof(gc_thread_data *));
}

#endif /* CYCLONE_RUNTIME_MAIN_H */
