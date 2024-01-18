/** 
 * Cyclone Scheme
 * Copyright (c) 2014, Justin Ethier
 * All rights reserved.
 *
 * This file contains the C runtime code used only by the main program module.
 */

#ifndef CYCLONE_RUNTIME_MAIN_H
#define CYCLONE_RUNTIME_MAIN_H

long global_stack_size = 0;
long global_heap_size = 0;

static void c_entry_pt(void *data, object clo, int argc, object * args);
static void Cyc_heap_init(long heap_size);

static void Cyc_heap_init(long heap_size)
{
  /* Allocate heap area for second generation. */
#if DEBUG_SHOW_DIAG
  printf("main: Allocating and initializing heap...\n");
#endif
  gc_init_heap(heap_size);
  gc_start_collector();
}

#endif                          /* CYCLONE_RUNTIME_MAIN_H */
