/** 
 * Cyclone Scheme
 * https://github.com/justinethier/cyclone
 *
 * Copyright (c) 2015-2016, Justin Ethier
 * All rights reserved.
 *
 * Heap garbage collector used by the Cyclone runtime for major collections.
 *
 * Tracing GC algorithm is based on the one from "Implementing an on-the-fly 
 * garbage collector for Java", by Domani et al.
 *
 * The heap implementation (alloc / sweep, etc) is based on code from Chibi Scheme.
 *
 * Note there is also a minor GC (in runtime.c) that collects objects allocated 
 * on the stack, based on "Cheney on the MTA" (but without the copying collector). 
 */

#include <ck_array.h>
#include <ck_pr.h>
#include "cyclone/types.h"
#include <stdint.h>
#include <time.h>
//#define DEBUG_THREADS // Debugging!!!
#ifdef DEBUG_THREADS
#include <sys/syscall.h>        /* Linux-only? */
#endif

/* HEAP definitions, based off heap from Chibi scheme */
#define gc_heap_first_block(h) ((object)(h->data + gc_heap_align(gc_free_chunk_size)))
#define gc_heap_last_block(h) ((object)((char*)h->data + h->size - gc_heap_align(gc_free_chunk_size)))
#define gc_heap_end(h) ((object)((char*)h->data + h->size))
#define gc_heap_pad_size(s) (sizeof(struct gc_heap_t) + (s) + gc_heap_align(1))
#define gc_free_chunk_size (sizeof(gc_free_list))

#define gc_align(n, bits) (((n)+(1<<(bits))-1)&(((uintptr_t)-1)-((1<<(bits))-1)))
// 64-bit is 3, 32-bit is 2
//#define gc_word_align(n) gc_align((n), 2)
#define gc_heap_align(n) gc_align(n, 5)

////////////////////
// Global variables

// Note: will need to use atomics and/or locking to access any
// variables shared between threads
static int gc_color_mark = 1;   // Black, is swapped during GC
static int gc_color_clear = 3;  // White, is swapped during GC
// unfortunately this had to be split up; const colors are located in types.h

static int gc_status_col = STATUS_SYNC1;
static int gc_stage = STAGE_RESTING;

// Does not need sync, only used by collector thread
static void **mark_stack = NULL;
static int mark_stack_len = 0;
static int mark_stack_i = 0;

// Lock to protect the heap from concurrent modifications
static pthread_mutex_t heap_lock;

// Cached heap statistics
static int cached_heap_free_sizes[3] = { 0, 0, 0 };
static int cached_heap_total_sizes[3] = { 0, 0, 0 };

// Data for each individual mutator thread
ck_array_t Cyc_mutators, old_mutators;
static pthread_mutex_t mutators_lock;

static void my_free(void *p, size_t m, bool d)
{
  free(p);
  return;
}

static void *my_malloc(size_t b)
{
  return malloc(b);
}

static void *my_realloc(void *r, size_t a, size_t b, bool d)
{
  return realloc(r, b);
}

static struct ck_malloc my_allocator = {
  .malloc = my_malloc,
  .free = my_free,
  .realloc = my_realloc
};

/////////////
// Functions

// Perform one-time initialization before mutators can be executed
void gc_initialize()
{
  if (ck_array_init(&Cyc_mutators, CK_ARRAY_MODE_SPMC, &my_allocator, 10) == 0) {
    fprintf(stderr, "Unable to initialize mutator array\n");
    exit(1);
  }

  if (ck_array_init(&old_mutators, CK_ARRAY_MODE_SPMC, &my_allocator, 10) == 0) {
    fprintf(stderr, "Unable to initialize mutator array\n");
    exit(1);
  }
  // Initialize collector's mark stack
  mark_stack_len = 128;
  mark_stack = vpbuffer_realloc(mark_stack, &(mark_stack_len));

  // Here is as good a place as any to do this...
  if (pthread_mutex_init(&(heap_lock), NULL) != 0) {
    fprintf(stderr, "Unable to initialize heap_lock mutex\n");
    exit(1);
  }
  if (pthread_mutex_init(&(mutators_lock), NULL) != 0) {
    fprintf(stderr, "Unable to initialize mutators_lock mutex\n");
    exit(1);
  }
}

// Add data for a new mutator
void gc_add_mutator(gc_thread_data * thd)
{
  pthread_mutex_lock(&mutators_lock);
  if (ck_array_put_unique(&Cyc_mutators, (void *)thd) < 0) {
    fprintf(stderr, "Unable to allocate memory for a new thread, exiting\n");
    exit(1);
  }
  ck_array_commit(&Cyc_mutators);
  pthread_mutex_unlock(&mutators_lock);
}

// Remove selected mutator from the mutator list.
// This is done for terminated threads. Note data is queued to be
// freed, to prevent accidentally freeing it while the collector
// thread is potentially accessing it.
void gc_remove_mutator(gc_thread_data * thd)
{
  pthread_mutex_lock(&mutators_lock);
  if (!ck_array_remove(&Cyc_mutators, (void *)thd)) {
    fprintf(stderr, "Unable to remove thread data, exiting\n");
    exit(1);
  }
  ck_array_commit(&Cyc_mutators);
  // Place on list of old mutators to cleanup
  if (ck_array_put_unique(&old_mutators, (void *)thd) < 0) {
    fprintf(stderr, "Unable to add thread data to GC list, existing\n");
    exit(1);
  }
  ck_array_commit(&old_mutators);
  pthread_mutex_unlock(&mutators_lock);
}

void gc_free_old_thread_data()
{
  ck_array_iterator_t iterator;
  gc_thread_data *m;
  int freed = 0;

  pthread_mutex_lock(&mutators_lock);
  CK_ARRAY_FOREACH(&old_mutators, &iterator, &m) {
//printf("JAE DEBUG - freeing old thread data...");
    gc_thread_data_free(m);
    if (!ck_array_remove(&old_mutators, (void *)m)) {
      fprintf(stderr, "Error removing old mutator data\n");
      exit(1);
    }
    freed = 1;
//printf(" done\n");
  }
  if (freed) {
    ck_array_commit(&old_mutators);
//printf("commited old mutator data deletions\n");
  }
  pthread_mutex_unlock(&mutators_lock);
}

gc_heap *gc_heap_create(int heap_type, size_t size, size_t max_size,
                        size_t chunk_size)
{
  gc_free_list *free, *next;
  gc_heap *h;
  size_t padded_size = gc_heap_pad_size(size);
  h = malloc(padded_size);      // TODO: mmap?
  if (!h)
    return NULL;
  h->size = size;
  //h->free_size = size;
  cached_heap_total_sizes[heap_type] += size;
  cached_heap_free_sizes[heap_type] += size;
  h->chunk_size = chunk_size;
  h->max_size = max_size;
  h->data = (char *)gc_heap_align(sizeof(h->data) + (uintptr_t) & (h->data));
  h->next = NULL;
  free = h->free_list = (gc_free_list *) h->data;
  next = (gc_free_list *) (((char *)free) + gc_heap_align(gc_free_chunk_size));
  free->size = 0;               // First one is just a dummy record
  free->next = next;
  next->size = size - gc_heap_align(gc_free_chunk_size);
  next->next = NULL;
#if GC_DEBUG_PRINTFS
  fprintf(stderr, "DEBUG h->data addr: %p\n", &(h->data));
  fprintf(stderr, "DEBUG h->data addr: %p\n", h->data);
  fprintf(stderr, ("heap: %p-%p data: %p-%p size: %zu\n"),
          h, ((char *)h) + gc_heap_pad_size(size), h->data, h->data + size,
          size);
  fprintf(stderr, ("first: %p end: %p\n"), (object) gc_heap_first_block(h),
          (object) gc_heap_end(h));
  fprintf(stderr, ("free1: %p-%p free2: %p-%p\n"), free,
          ((char *)free) + free->size, next, ((char *)next) + next->size);
#endif
  return h;
}

/**
 * Print heap usage information.
 * Before calling this function the current thread must have the heap lock
 */
void gc_print_stats(gc_heap * h)
{
  gc_free_list *f;
  unsigned int free, free_chunks, free_min, free_max;
  for (; h; h = h->next) {
    free = 0;
    free_chunks = 0;
    free_min = h->size;
    free_max = 0;
    for (f = h->free_list; f; f = f->next) {
      free += f->size;
      free_chunks++;
      if (f->size < free_min && f->size > 0)
        free_min = f->size;
      if (f->size > free_max)
        free_max = f->size;
    }
    if (free == 0){
      // Page is completely unused
      free = h->size;
      free_chunks = 1;
      free_min = free_max = h->size;
    }
    fprintf(stderr,
            "Heap page size=%u, used=%u, free=%u, free chunks=%u, min=%u, max=%u\n",
            h->size, h->size - free, free, free_chunks, free_min, free_max);
  }
}

// Copy given object into given heap object
char *gc_copy_obj(object dest, char *obj, gc_thread_data * thd)
{
  // NOTE: no additional type checking because this is called from gc_move
  // which already does that

  switch (type_of(obj)) {
  case pair_tag:{
      list hp = dest;
      hp->hdr.mark = thd->gc_alloc_color;
      type_of(hp) = pair_tag;
      car(hp) = car(obj);
      cdr(hp) = cdr(obj);
      return (char *)hp;
    }
  case macro_tag:{
      macro_type *hp = dest;
      mark(hp) = thd->gc_alloc_color;
      type_of(hp) = macro_tag;
      hp->fn = ((macro) obj)->fn;
      hp->num_args = ((macro) obj)->num_args;
      return (char *)hp;
    }
  case closure0_tag:{
      closure0_type *hp = dest;
      mark(hp) = thd->gc_alloc_color;
      type_of(hp) = closure0_tag;
      hp->fn = ((closure0) obj)->fn;
      hp->num_args = ((closure0) obj)->num_args;
      return (char *)hp;
    }
  case closure1_tag:{
      closure1_type *hp = dest;
      mark(hp) = thd->gc_alloc_color;
      type_of(hp) = closure1_tag;
      hp->fn = ((closure1) obj)->fn;
      hp->num_args = ((closure1) obj)->num_args;
      hp->element = ((closure1) obj)->element;
      return (char *)hp;
    }
  case closureN_tag:{
      int i;
      closureN_type *hp = dest;
      mark(hp) = thd->gc_alloc_color;
      type_of(hp) = closureN_tag;
      hp->fn = ((closureN) obj)->fn;
      hp->num_args = ((closureN) obj)->num_args;
      hp->num_elements = ((closureN) obj)->num_elements;
      hp->elements = (object *) (((char *)hp) + sizeof(closureN_type));
      for (i = 0; i < hp->num_elements; i++) {
        hp->elements[i] = ((closureN) obj)->elements[i];
      }
      return (char *)hp;
    }
  case vector_tag:{
      int i;
      vector_type *hp = dest;
      mark(hp) = thd->gc_alloc_color;
      type_of(hp) = vector_tag;
      hp->num_elements = ((vector) obj)->num_elements;
      hp->elements = (object *) (((char *)hp) + sizeof(vector_type));
      for (i = 0; i < hp->num_elements; i++) {
        hp->elements[i] = ((vector) obj)->elements[i];
      }
      return (char *)hp;
    }
  case bytevector_tag:{
      int i;
      bytevector_type *hp = dest;
      mark(hp) = thd->gc_alloc_color;
      type_of(hp) = bytevector_tag;
      hp->len = ((bytevector) obj)->len;
      hp->data = (((char *)hp) + sizeof(bytevector_type));
      memcpy(hp->data, ((bytevector) obj)->data, hp->len);
      return (char *)hp;
    }
  case string_tag:{
      char *s;
      string_type *hp = dest;
      s = ((char *)hp) + sizeof(string_type);
      memcpy(s, string_str(obj), string_len(obj) + 1);
      mark(hp) = thd->gc_alloc_color;
      type_of(hp) = string_tag;
      string_len(hp) = string_len(obj);
      string_str(hp) = s;
      return (char *)hp;
    }
  case integer_tag:{
      integer_type *hp = dest;
      mark(hp) = thd->gc_alloc_color;
      type_of(hp) = integer_tag;
      hp->value = ((integer_type *) obj)->value;
      return (char *)hp;
    }
  case double_tag:{
      double_type *hp = dest;
      mark(hp) = thd->gc_alloc_color;
      type_of(hp) = double_tag;
      hp->value = ((double_type *) obj)->value;
      return (char *)hp;
    }
  case port_tag:{
      port_type *hp = dest;
      mark(hp) = thd->gc_alloc_color;
      type_of(hp) = port_tag;
      hp->fp = ((port_type *) obj)->fp;
      hp->mode = ((port_type *) obj)->mode;
      return (char *)hp;
    }
  case cvar_tag:{
      cvar_type *hp = dest;
      mark(hp) = thd->gc_alloc_color;
      type_of(hp) = cvar_tag;
      hp->pvar = ((cvar_type *) obj)->pvar;
      return (char *)hp;
    }
  case c_opaque_tag:{
      c_opaque_type *hp = dest;
      mark(hp) = thd->gc_alloc_color;
      type_of(hp) = c_opaque_tag;
      hp->ptr = ((c_opaque_type *) obj)->ptr;
      return (char *)hp;
    }
  case mutex_tag:{
      mutex_type *hp = dest;
      mark(hp) = thd->gc_alloc_color;
      type_of(hp) = mutex_tag;
      // NOTE: don't copy mutex itself, caller will do that (this is a special case)
      return (char *)hp;
    }
  case cond_var_tag:{
      cond_var_type *hp = dest;
      mark(hp) = thd->gc_alloc_color;
      type_of(hp) = cond_var_tag;
      // NOTE: don't copy cond_var itself, caller will do that (this is a special case)
      return (char *)hp;
    }
  case forward_tag:
    return (char *)forward(obj);
  case eof_tag:
  case primitive_tag:
  case boolean_tag:
  case symbol_tag:
    break;
  default:
    fprintf(stderr, "gc_copy_obj: bad tag obj=%p obj.tag=%d\n", (object) obj,
            type_of(obj));
    exit(1);
  }
  return (char *)obj;
}

int gc_grow_heap(gc_heap * h, int heap_type, size_t size, size_t chunk_size)
{
  size_t cur_size, new_size;
  gc_heap *h_last, *h_new;
  pthread_mutex_lock(&heap_lock);
  // Compute size of new heap page
// experimental code for growing heap gradually using fibonnaci sequence.
// but with boyer benchmarks there is more thrashing with this method,
// so for now it is not used. If it is used again, the initial heaps will
// need to start at a lower size (EG 1 MB).
  {
    size_t prev_size = GROW_HEAP_BY_SIZE;
    new_size = 0;
    h_last = h;
    while (h_last->next) {
      if (new_size < HEAP_SIZE) {
        new_size = prev_size + h_last->size;
        prev_size = h_last->size;
      } else {
        new_size = HEAP_SIZE;
      }
      h_last = h_last->next;
    }
    if (new_size == 0)
      new_size = prev_size + h_last->size;
#if GC_DEBUG_TRACE
    fprintf(stderr, "Growing heap %d new page size = %zu\n", heap_type,
            new_size);
#endif
  }
//  h_last = gc_heap_last(h);
//  cur_size = h_last->size;
//  new_size = cur_size; //gc_heap_align(((cur_size > size) ? cur_size : size) * 2);
  // allocate larger pages if size will not fit on the page
  //new_size = gc_heap_align(((cur_size > size) ? cur_size : size));
  // Done with computing new page size
  h_new = gc_heap_create(heap_type, new_size, h_last->max_size, chunk_size);
  h_last->next = h_new;
  pthread_mutex_unlock(&heap_lock);
#if GC_DEBUG_TRACE
  fprintf(stderr, "DEBUG - grew heap\n");
#endif
  return (h_new != NULL);
}

void *gc_try_alloc(gc_heap * h, int heap_type, size_t size, char *obj,
                   gc_thread_data * thd)
{
  gc_free_list *f1, *f2, *f3;
  pthread_mutex_lock(&heap_lock);
  for (; h; h = h->next) {      // All heaps
    // TODO: chunk size (ignoring for now)

    for (f1 = h->free_list, f2 = f1->next; f2; f1 = f2, f2 = f2->next) {        // all free in this heap
      if (f2->size >= size) {   // Big enough for request
        // TODO: take whole chunk or divide up f2 (using f3)?
        if (f2->size >= (size + gc_heap_align(1) /* min obj size */ )) {
          f3 = (gc_free_list *) (((char *)f2) + size);
          f3->size = f2->size - size;
          f3->next = f2->next;
          f1->next = f3;
        } else {                /* Take the whole chunk */
          f1->next = f2->next;
        }
        // Copy object into heap now to avoid any uninitialized memory issues
        gc_copy_obj(f2, obj, thd);
        //h->free_size -= gc_allocated_bytes(obj, NULL, NULL);
        cached_heap_free_sizes[heap_type] -=
            gc_allocated_bytes(obj, NULL, NULL);
        pthread_mutex_unlock(&heap_lock);
        return f2;
      }
    }
  }
  pthread_mutex_unlock(&heap_lock);
  return NULL;
}

void *gc_alloc(gc_heap_root * hrt, size_t size, char *obj, gc_thread_data * thd,
               int *heap_grown)
{
  void *result = NULL;
  gc_heap *h = NULL;
  int heap_type;
  size_t max_freed = 0, sum_freed = 0, total_size;
  // TODO: check return value, if null (could not alloc) then 
  // run a collection and check how much free space there is. if less
  // the allowed ratio, try growing heap.
  // then try realloc. if cannot alloc now, then throw out of memory error
  size = gc_heap_align(size);
  if (size <= 32) {
    h = hrt->small_obj_heap;
    heap_type = HEAP_SM;
  } else if (size <= 64) {
    h = hrt->medium_obj_heap;
    heap_type = HEAP_MED;
  } else {
    h = hrt->heap;
    heap_type = HEAP_REST;
  }

  result = gc_try_alloc(h, heap_type, size, obj, thd);
  if (!result) {
    // A vanilla mark&sweep collector would collect now, but unfortunately
    // we can't do that because we have to go through multiple stages, some
    // of which are asynchronous. So... no choice but to grow the heap.
    gc_grow_heap(h, heap_type, size, 0);
    *heap_grown = 1;
    result = gc_try_alloc(h, heap_type, size, obj, thd);
    if (!result) {
      fprintf(stderr, "out of memory error allocating %zu bytes\n", size);
      exit(1);                  // could throw error, but OOM is a major issue, so...
    }
  }
#if GC_DEBUG_VERBOSE
  fprintf(stderr, "alloc %p size = %zu, obj=%p, tag=%d, mark=%d\n", result,
          size, obj, type_of(obj), mark(((object) result)));
  // Debug check, should no longer be necessary
  //if (is_value_type(result)) {
  //  printf("Invalid allocated address - is a value type %p\n", result);
  //}
#endif
  return result;
}

size_t gc_allocated_bytes(object obj, gc_free_list * q, gc_free_list * r)
{
  tag_type t;
#if GC_SAFETY_CHECKS
  if (is_value_type(obj)) {
    fprintf(stderr,
            "gc_allocated_bytes - passed value type %p q=[%p, %d] r=[%p, %d]\n",
            obj, q, q->size, r, r->size);
    exit(1);
  }
#endif
  t = type_of(obj);
  if (t == pair_tag)
    return gc_heap_align(sizeof(pair_type));
  if (t == macro_tag)
    return gc_heap_align(sizeof(macro_type));
  if (t == closure0_tag)
    return gc_heap_align(sizeof(closure0_type));
  if (t == closure1_tag)
    return gc_heap_align(sizeof(closure1_type));
  if (t == closureN_tag) {
    return gc_heap_align(sizeof(closureN_type) +
                         sizeof(object) *
                         ((closureN_type *) obj)->num_elements);
  }
  if (t == vector_tag) {
    return gc_heap_align(sizeof(vector_type) +
                         sizeof(object) * ((vector_type *) obj)->num_elements);
  }
  if (t == bytevector_tag) {
    return gc_heap_align(sizeof(bytevector_type) +
                         sizeof(char) * ((bytevector) obj)->len);
  }
  if (t == string_tag) {
    return gc_heap_align(sizeof(string_type) + string_len(obj) + 1);
  }
  if (t == integer_tag)
    return gc_heap_align(sizeof(integer_type));
  if (t == double_tag)
    return gc_heap_align(sizeof(double_type));
  if (t == port_tag)
    return gc_heap_align(sizeof(port_type));
  if (t == cvar_tag)
    return gc_heap_align(sizeof(cvar_type));
  if (t == c_opaque_tag)
    return gc_heap_align(sizeof(c_opaque_type));
  if (t == mutex_tag)
    return gc_heap_align(sizeof(mutex_type));
  if (t == cond_var_tag)
    return gc_heap_align(sizeof(cond_var_type));

  fprintf(stderr, "gc_allocated_bytes: unexpected object %p of type %d\n", obj,
          t);
  exit(1);
  return 0;
}

gc_heap *gc_heap_last(gc_heap * h)
{
  while (h->next)
    h = h->next;
  return h;
}

size_t gc_heap_total_size(gc_heap * h)
{
  size_t total_size = 0;
  pthread_mutex_lock(&heap_lock);
  while (h) {
    total_size += h->size;
    h = h->next;
  }
  pthread_mutex_unlock(&heap_lock);
  return total_size;
}

//size_t gc_heap_total_free_size(gc_heap *h)
//{
//  size_t total_size = 0;
//  pthread_mutex_lock(&heap_lock);
//  while(h) {
//    total_size += h->free_size;
//    h = h->next;
//  }
//  pthread_mutex_unlock(&heap_lock);
//  return total_size;
//}

size_t gc_sweep(gc_heap * h, int heap_type, size_t * sum_freed_ptr)
{
  size_t freed, max_freed = 0, heap_freed = 0, sum_freed = 0, size;
  object p, end;
  gc_free_list *q, *r, *s;
  gc_heap *orig_heap_ptr = h;

  //
  // Lock the heap to prevent issues with allocations during sweep
  // It sucks to have to use a coarse-grained lock like this, but let's
  // be safe and prevent threading issues right now. Once the new GC
  // works we can go back and try to speed things up (if possible)
  // by using more fine-grained locking. Can also profile to see
  // how much time is even spent sweeping
  //
  pthread_mutex_lock(&heap_lock);
  for (; h; h = h->next) {      // All heaps
#if GC_DEBUG_TRACE
    fprintf(stderr, "sweep heap %p, size = %zu\n", h, (size_t) h->size);
#endif
    p = gc_heap_first_block(h);
    q = h->free_list;
    end = gc_heap_end(h);
    while (p < end) {
      // find preceding/succeeding free list pointers for p
      for (r = q->next; r && ((char *)r < (char *)p); q = r, r = r->next) ;

      if ((char *)r == (char *)p) {     // this is a free block, skip it
        p = (object) (((char *)p) + r->size);
#if GC_DEBUG_VERBOSE
        fprintf(stderr, "skip free block %p size = %zu\n", p, r->size);
#endif
        continue;
      }
      size = gc_heap_align(gc_allocated_bytes(p, q, r));

#if GC_SAFETY_CHECKS
      if (!is_object_type(p)) {
        fprintf(stderr, "sweep: invalid object at %p", p);
        exit(1);
      }
      if ((char *)q + q->size > (char *)p) {
        fprintf(stderr, "bad size at %p < %p + %u", p, q, q->size);
        exit(1);
      }
      if (r && ((char *)p) + size > (char *)r) {
        fprintf(stderr, "sweep: bad size at %p + %zu > %p", p, size, r);
        exit(1);
      }
#endif

      if (mark(p) == gc_color_clear) {
#if GC_DEBUG_VERBOSE
        fprintf(stderr, "sweep is freeing unmarked obj: %p with tag %d\n", p,
                type_of(p));
#endif
        mark(p) = gc_color_blue;        // Needed?
        if (type_of(p) == mutex_tag) {
#if GC_DEBUG_VERBOSE
          fprintf(stderr, "pthread_mutex_destroy from sweep\n");
#endif
          if (pthread_mutex_destroy(&(((mutex) p)->lock)) != 0) {
            fprintf(stderr, "Error destroying mutex\n");
            exit(1);
          }
        } else if (type_of(p) == cond_var_tag) {
#if GC_DEBUG_VERBOSE
          fprintf(stderr, "pthread_cond_destroy from sweep\n");
#endif
          if (pthread_cond_destroy(&(((cond_var) p)->cond)) != 0) {
            fprintf(stderr, "Error destroying condition variable\n");
            exit(1);
          }
        }
        // free p
        heap_freed += size;
        if (((((char *)q) + q->size) == (char *)p) && (q != h->free_list)) {
          /* merge q with p */
          if (r && r->size && ((((char *)p) + size) == (char *)r)) {
            // ... and with r
            q->next = r->next;
            freed = q->size + size + r->size;
            p = (object) (((char *)p) + size + r->size);
          } else {
            freed = q->size + size;
            p = (object) (((char *)p) + size);
          }
          q->size = freed;
        } else {
          s = (gc_free_list *) p;
          if (r && r->size && ((((char *)p) + size) == (char *)r)) {
            // merge p with r
            s->size = size + r->size;
            s->next = r->next;
            q->next = s;
            freed = size + r->size;
          } else {
            s->size = size;
            s->next = r;
            q->next = s;
            freed = size;
          }
          p = (object) (((char *)p) + freed);
        }
        if (freed > max_freed)
          max_freed = freed;
      } else {
//#if GC_DEBUG_VERBOSE
//        fprintf(stderr, "sweep: object is marked %p\n", p);
//#endif
        p = (object) (((char *)p) + size);
      }
    }
    //h->free_size += heap_freed;
    cached_heap_free_sizes[heap_type] += heap_freed;
    sum_freed += heap_freed;
    heap_freed = 0;
  }

// DEBUGGING:
//fprintf(stderr, "Heap %d diagnostics:\n", heap_type);
//gc_print_stats(orig_heap_ptr);

  pthread_mutex_unlock(&heap_lock);
  if (sum_freed_ptr)
    *sum_freed_ptr = sum_freed;
  return max_freed;
}

void gc_thr_grow_move_buffer(gc_thread_data * d)
{
  if (!d)
    return;

  if (d->moveBufLen == 0) {     // Special case
    d->moveBufLen = 128;
    d->moveBuf = NULL;
  } else {
    d->moveBufLen *= 2;
  }

  d->moveBuf = realloc(d->moveBuf, d->moveBufLen * sizeof(void *));
#if GC_DEBUG_TRACE
  fprintf(stderr, "grew moveBuffer, len = %d\n", d->moveBufLen);
#endif
}

void gc_thr_add_to_move_buffer(gc_thread_data * d, int *alloci, object obj)
{
  if (*alloci == d->moveBufLen) {
    gc_thr_grow_move_buffer(d);
  }

  d->moveBuf[*alloci] = obj;
  (*alloci)++;
}

// Generic buffer functions
void **vpbuffer_realloc(void **buf, int *len)
{
  return realloc(buf, (*len) * sizeof(void *));
}

void **vpbuffer_add(void **buf, int *len, int i, void *obj)
{
  if (i == *len) {
    *len *= 2;
    buf = vpbuffer_realloc(buf, len);
  }
  buf[i] = obj;
  return buf;
}

void vpbuffer_free(void **buf)
{
  free(buf);
}

// END heap definitions

// Tri-color GC section

/////////////////////////////////////////////
// GC functions called by the Mutator threads

/**
 * Clear thread data read/write fields
 */
void gc_zero_read_write_counts(gc_thread_data * thd)
{
  pthread_mutex_lock(&(thd->lock));
#if GC_SAFETY_CHECKS
  if (thd->last_read < thd->last_write) {
    fprintf(stderr,
            "gc_zero_read_write_counts - last_read (%d) < last_write (%d)\n",
            thd->last_read, thd->last_write);
  } else if (thd->pending_writes) {
    fprintf(stderr,
            "gc_zero_read_write_counts - pending_writes (%d) is not zero\n",
            thd->pending_writes);
  }
#endif
  thd->last_write = 0;
  thd->last_read = 0;
  thd->pending_writes = 0;
  pthread_mutex_unlock(&(thd->lock));
}

/**
 * Move pending writes to last_write
 */
void gc_sum_pending_writes(gc_thread_data * thd, int locked)
{
  if (!locked) {
    pthread_mutex_lock(&(thd->lock));
  }
  thd->last_write += thd->pending_writes;
  thd->pending_writes = 0;
  if (!locked) {
    pthread_mutex_unlock(&(thd->lock));
  }
}

/**
 * Determine if object lives on the thread's stack
 */
int gc_is_stack_obj(gc_thread_data * thd, object obj)
{
  char tmp;
  object low_limit = &tmp;
  object high_limit = thd->stack_start;
  return (stack_overflow(low_limit, obj) && stack_overflow(obj, high_limit));
}

/**
 * Helper function for gc_mut_update
 */
static void mark_stack_or_heap_obj(gc_thread_data * thd, object obj)
{
  if (gc_is_stack_obj(thd, obj)) {
    // Set object to be marked after moved to heap by next GC.
    // This avoids having to recursively examine the stack now, 
    // which we have to do anyway during minor GC.
    grayed(obj) = 1;
  } else {
    // Value is on the heap, mark gray right now
    gc_mark_gray(thd, obj);
  }
}

/**
 * Write barrier for updates to heap-allocated objects
 * The key for this barrier is to identify stack objects that contain
 * heap references, so they can be marked to avoid collection.
*/
void gc_mut_update(gc_thread_data * thd, object old_obj, object value)
{
  int status = ck_pr_load_int(&gc_status_col),
      stage = ck_pr_load_int(&gc_stage);
  if (ck_pr_load_int(&(thd->gc_status)) != STATUS_ASYNC) {
    pthread_mutex_lock(&(thd->lock));
    mark_stack_or_heap_obj(thd, old_obj);
    mark_stack_or_heap_obj(thd, value);
    pthread_mutex_unlock(&(thd->lock));
  } else if (stage == STAGE_TRACING) {
//fprintf(stderr, "DEBUG - GC async tracing marking heap obj %p ", old_obj);
//Cyc_display(old_obj, stderr);
//fprintf(stderr, "\n");
    pthread_mutex_lock(&(thd->lock));
    mark_stack_or_heap_obj(thd, old_obj);
    pthread_mutex_unlock(&(thd->lock));
#if GC_DEBUG_VERBOSE
    if (is_object_type(old_obj) && mark(old_obj) == gc_color_clear) {
      fprintf(stderr,
              "added to mark buffer (trace) from write barrier %p:mark %d:",
              old_obj, mark(old_obj));
      Cyc_display(old_obj, stderr);
      fprintf(stderr, "\n");
    }
#endif
  }
}

void gc_mut_cooperate(gc_thread_data * thd, int buf_len)
{
  int i, status_c, status_m;
#if GC_DEBUG_VERBOSE
  int debug_print = 0;
#endif

  // Handle any pending marks from write barrier
  gc_sum_pending_writes(thd, 0);

  // I think below is thread safe, but this code is tricky.
  // Worst case should be that some work is done twice if there is
  // a race condition
  //
  // TODO: should use an atomic comparison here
  status_c = ck_pr_load_int(&gc_status_col);
  status_m = ck_pr_load_int(&(thd->gc_status));
  if (status_m != status_c) {
    ck_pr_cas_int(&(thd->gc_status), status_m, status_c);
    if (status_m == STATUS_ASYNC) {
      // Async is done, so clean up old mark data from the last collection
      gc_zero_read_write_counts(thd);
    } else if (status_m == STATUS_SYNC2) {
#if GC_DEBUG_VERBOSE
      debug_print = 1;
#endif
      // Mark thread "roots":
      // Begin my marking current continuation, which may have already
      // been on the heap prior to latest minor GC
      pthread_mutex_lock(&(thd->lock));
      gc_mark_gray(thd, thd->gc_cont);
      for (i = 0; i < thd->gc_num_args; i++) {
        gc_mark_gray(thd, thd->gc_args[i]);
      }
      // Also, mark everything the collector moved to the heap
      for (i = 0; i < buf_len; i++) {
        gc_mark_gray(thd, thd->moveBuf[i]);
      }
      pthread_mutex_unlock(&(thd->lock));
      thd->gc_alloc_color = ck_pr_load_int(&gc_color_mark);
    }
  }
#if GC_DEBUG_VERBOSE
  if (debug_print) {
    fprintf(stderr, "coop mark gc_cont %p\n", thd->gc_cont);
    for (i = 0; i < thd->gc_num_args; i++) {
      fprintf(stderr, "coop mark gc_args[%d] %p\n", i, thd->gc_args[i]);
    }
    for (i = 0; i < buf_len; i++) {
      fprintf(stderr, "coop mark from move buf %i %p\n", i, thd->moveBuf[i]);
    }
  }
#endif

  // Initiate collection cycle if free space is too low.
  // Threshold is intentially low because we have to go through an
  // entire handshake/trace/sweep cycle, ideally without growing heap.
  if (ck_pr_load_int(&gc_stage) == STAGE_RESTING &&
      ((cached_heap_free_sizes[HEAP_SM] <
        cached_heap_total_sizes[HEAP_SM] * GC_COLLECTION_THRESHOLD) ||
       (cached_heap_free_sizes[HEAP_MED] <
        cached_heap_total_sizes[HEAP_MED] * GC_COLLECTION_THRESHOLD) ||
       (cached_heap_free_sizes[HEAP_REST] <
        cached_heap_total_sizes[HEAP_REST] * GC_COLLECTION_THRESHOLD))) {
#if GC_DEBUG_TRACE
    fprintf(stdout,
            "Less than %f%% of the heap is free, initiating collector\n",
            100.0 * GC_COLLECTION_THRESHOLD);
#endif
    ck_pr_cas_int(&gc_stage, STAGE_RESTING, STAGE_CLEAR_OR_MARKING);

  }
}

/////////////////////////////////////////////
// Collector functions

/**
 * Mark the given object gray if it is on the heap.
 * Note marking is done implicitly by placing it in a buffer,
 * to avoid repeated re-scanning.
 *
 * This function must be executed once the thread lock has been acquired.
 */
void gc_mark_gray(gc_thread_data * thd, object obj)
{
  // From what I can tell, no other thread would be modifying
  // either object type or mark. Both should be stable once the object is placed
  // into the heap, with the collector being the only thread that changes marks.
  if (is_object_type(obj) && mark(obj) == gc_color_clear) {     // TODO: sync??
    // Place marked object in a buffer to avoid repeated scans of the heap.
// TODO:
// Note that ideally this should be a lock-free data structure to make the
// algorithm more efficient. So this code (and the corresponding collector
// trace code) should be converted at some point.
    thd->mark_buffer = vpbuffer_add(thd->mark_buffer,
                                    &(thd->mark_buffer_len),
                                    thd->last_write, obj);
    (thd->last_write)++;        // Already locked, just do it...
  }
}

/**
 * Add a pending write to the mark buffer.
 * These are pended because they are written in a batch during minor GC.
 * To prevent race conditions we wait until all of the writes are made before
 * updating last write.
 *
 * TODO: figure out a new name for this function.
 */
void gc_mark_gray2(gc_thread_data * thd, object obj)
{
  if (is_object_type(obj) && mark(obj) == gc_color_clear) {
    thd->mark_buffer = vpbuffer_add(thd->mark_buffer,
                                    &(thd->mark_buffer_len),
                                    (thd->last_write + thd->pending_writes),
                                    obj);
    thd->pending_writes++;
  }
}

void gc_collector_trace()
{
  ck_array_iterator_t iterator;
  gc_thread_data *m;
  int clean = 0;
  while (!clean) {
    clean = 1;

    CK_ARRAY_FOREACH(&Cyc_mutators, &iterator, &m) {
// TODO: ideally, want to use a lock-free data structure to prevent
// having to use a mutex here. see corresponding code in gc_mark_gray
      pthread_mutex_lock(&(m->lock));
      while (m->last_read < m->last_write) {
        clean = 0;
#if GC_DEBUG_VERBOSE
        fprintf(stderr,
                "gc_mark_black mark buffer %p, last_read = %d last_write = %d\n",
                (m->mark_buffer)[m->last_read], m->last_read, m->last_write);
#endif
        gc_mark_black((m->mark_buffer)[m->last_read]);
        gc_empty_collector_stack();
        (m->last_read)++;       // Inc here to prevent off-by-one error
      }
      pthread_mutex_unlock(&(m->lock));

      // Try checking the condition once more after giving the
      // mutator a chance to respond, to prevent exiting early.
      // This is experimental, not sure if it is necessary
      if (clean) {
        pthread_mutex_lock(&(m->lock));
        if (m->last_read < m->last_write) {
#if GC_SAFETY_CHECKS
          fprintf(stderr,
                  "gc_collector_trace - might have exited trace early\n");
#endif
          clean = 0;
        } else if (m->pending_writes) {
          clean = 0;
        }
        pthread_mutex_unlock(&(m->lock));
      }
    }
  }
}

// TODO: seriously consider changing the mark() macro to color(),
// and sync up the header variable. that would make all of this code
// bit clearer...

void gc_mark_black(object obj)
{
  // TODO: is sync required to get colors? probably not on the collector
  // thread (at least) since colors are only changed once during the clear
  // phase and before the first handshake.
  int markColor = ck_pr_load_int(&gc_color_mark);
  if (is_object_type(obj) && mark(obj) != markColor) {
    // Gray any child objects
    // Note we probably should use some form of atomics/synchronization
    // for cons and vector types, as these pointers could change.
    switch (type_of(obj)) {
    case pair_tag:{
        gc_collector_mark_gray(obj, car(obj));
        gc_collector_mark_gray(obj, cdr(obj));
        break;
      }
    case closure1_tag:
      gc_collector_mark_gray(obj, ((closure1) obj)->element);
      break;
    case closureN_tag:{
        int i, n = ((closureN) obj)->num_elements;
        for (i = 0; i < n; i++) {
          gc_collector_mark_gray(obj, ((closureN) obj)->elements[i]);
        }
        break;
      }
    case vector_tag:{
        int i, n = ((vector) obj)->num_elements;
        for (i = 0; i < n; i++) {
          gc_collector_mark_gray(obj, ((vector) obj)->elements[i]);
        }
        break;
      }
    case cvar_tag:{
        cvar_type *c = (cvar_type *) obj;
        object pvar = *(c->pvar);
        if (pvar) {
          gc_collector_mark_gray(obj, pvar);
        }
        break;
      }
    default:
      break;
    }
    if (mark(obj) != gc_color_red) {
      // Only blacken objects on the heap
      mark(obj) = markColor;
    }
#if GC_DEBUG_VERBOSE
    if (mark(obj) != gc_color_red) {
      fprintf(stderr, "marked %p %d\n", obj, markColor);
    } else {
      fprintf(stderr, "not marking stack obj %p %d\n", obj, markColor);
    }
#endif
  }
}

void gc_collector_mark_gray(object parent, object obj)
{
  // "Color" objects gray by adding them to the mark stack for further processing.
  //
  // Note that stack objects are always colored red during creation, so
  // they should never be added to the mark stack. Which would be bad because it
  // could lead to stack corruption.
  if (is_object_type(obj) && mark(obj) == gc_color_clear) {
    mark_stack = vpbuffer_add(mark_stack, &mark_stack_len, mark_stack_i++, obj);
#if GC_DEBUG_VERBOSE
    fprintf(stderr, "mark gray parent = %p (%d) obj = %p\n", parent,
            type_of(parent), obj);
#endif
  }
}

void gc_empty_collector_stack()
{
  // Mark stack is only used by the collector thread, so no sync needed
  while (mark_stack_i > 0) {    // not empty
    mark_stack_i--;
//#if GC_DEBUG_VERBOSE
//    fprintf(stderr, "gc_mark_black mark stack %p \n",
//      mark_stack[mark_stack_i]);
//#endif
    gc_mark_black(mark_stack[mark_stack_i]);
  }
}

void gc_handshake(gc_status_type s)
{
  gc_post_handshake(s);
  gc_wait_handshake();
}

void gc_post_handshake(gc_status_type s)
{
  int status = ck_pr_load_int(&gc_status_col);
  while (!ck_pr_cas_int(&gc_status_col, status, s)) {
  }
}

void gc_wait_handshake()
{
  ck_array_iterator_t iterator;
  gc_thread_data *m;
  int statusm, statusc, thread_status, i, buf_len;
  struct timespec tim;
  tim.tv_sec = 0;
  tim.tv_nsec = 1000000;        // 1 millisecond

  CK_ARRAY_FOREACH(&Cyc_mutators, &iterator, &m) {
    while (1) {
      // TODO: use an atomic comparison
      statusc = ck_pr_load_int(&gc_status_col);
      statusm = ck_pr_load_int(&(m->gc_status));
      if (statusc == statusm) {
        // Handshake succeeded, check next mutator
        break;
      }

      thread_status = ck_pr_load_int((int *)&(m->thread_state));
      if (thread_status == CYC_THREAD_STATE_BLOCKED ||
          thread_status == CYC_THREAD_STATE_BLOCKED_COOPERATING) {
        if (statusm == STATUS_ASYNC) {  // Prev state
          ck_pr_cas_int(&(m->gc_status), statusm, statusc);
          // Async is done, so clean up old mark data from the last collection
          gc_zero_read_write_counts(m);
        } else if (statusm == STATUS_SYNC1) {
          ck_pr_cas_int(&(m->gc_status), statusm, statusc);
        } else if (statusm == STATUS_SYNC2) {
//printf("DEBUG - is mutator still blocked?\n");
          pthread_mutex_lock(&(m->lock));
          // Check again, if thread is still blocked we need to cooperate
          if (ck_pr_cas_int((int *)&(m->thread_state),
                            CYC_THREAD_STATE_BLOCKED,
                            CYC_THREAD_STATE_BLOCKED_COOPERATING)
              ||
              ck_pr_cas_int((int *)&(m->thread_state),
                            CYC_THREAD_STATE_BLOCKED_COOPERATING,
                            CYC_THREAD_STATE_BLOCKED_COOPERATING)
              ) {
//printf("DEBUG - update mutator GC status\n");            
            ck_pr_cas_int(&(m->gc_status), statusm, statusc);
//printf("DEBUG - collector is cooperating for blocked mutator\n");            
            buf_len =
                gc_minor(m, m->stack_limit, m->stack_start, m->gc_cont, NULL,
                         0);
            // Handle any pending marks from write barrier
            gc_sum_pending_writes(m, 1);
            // Mark thread "roots", based on code from mutator's cooperator
            gc_mark_gray(m, m->gc_cont);
            //for (i = 0; i < m->gc_num_args; i++) {
            //  gc_mark_gray(m, m->gc_args[i]);
            //}
            // Also, mark everything the collector moved to the heap
            for (i = 0; i < buf_len; i++) {
              gc_mark_gray(m, m->moveBuf[i]);
            }
            m->gc_alloc_color = ck_pr_load_int(&gc_color_mark);
          }
          pthread_mutex_unlock(&(m->lock));
        }
      } else if (thread_status == CYC_THREAD_STATE_TERMINATED) {
        // Thread is no longer running
        break;
      }
      // At least for now, just give up quantum and come back to
      // this quickly to test again. This probably could be more
      // efficient.
      nanosleep(&tim, NULL);
    }
  }
}

/////////////////////////////////////////////
// GC Collection cycle

void debug_dump_globals();

// Main collector function
void gc_collector()
{
  int old_clear, old_mark, heap_type;
  size_t freed = 0, max_freed = 0, total_size, total_free;
#if GC_DEBUG_TRACE
  time_t gc_collector_start = time(NULL);
#endif
  //clear : 
  ck_pr_cas_int(&gc_stage, STAGE_RESTING, STAGE_CLEAR_OR_MARKING);
  // exchange values of markColor and clearColor
  old_clear = ck_pr_load_int(&gc_color_clear);
  old_mark = ck_pr_load_int(&gc_color_mark);
  while (!ck_pr_cas_int(&gc_color_clear, old_clear, old_mark)) {
  }
  while (!ck_pr_cas_int(&gc_color_mark, old_mark, old_clear)) {
  }
#if GC_DEBUG_TRACE
  fprintf(stderr, "DEBUG - swap clear %d / mark %d\n", gc_color_clear,
          gc_color_mark);
#endif
  gc_handshake(STATUS_SYNC1);
#if GC_DEBUG_TRACE
  fprintf(stderr, "DEBUG - after handshake sync 1\n");
#endif
  //mark : 
  gc_handshake(STATUS_SYNC2);
#if GC_DEBUG_TRACE
  fprintf(stderr, "DEBUG - after handshake sync 2\n");
#endif
  ck_pr_cas_int(&gc_stage, STAGE_CLEAR_OR_MARKING, STAGE_TRACING);
  gc_post_handshake(STATUS_ASYNC);
#if GC_DEBUG_TRACE
  fprintf(stderr, "DEBUG - after post_handshake async\n");
#endif
  gc_mark_globals();
  gc_wait_handshake();
#if GC_DEBUG_TRACE
  fprintf(stderr, "DEBUG - after wait_handshake async\n");
#endif
  //trace : 
  gc_collector_trace();
#if GC_DEBUG_TRACE
  fprintf(stderr, "DEBUG - after trace\n");
  //debug_dump_globals();
#endif
  ck_pr_cas_int(&gc_stage, STAGE_TRACING, STAGE_SWEEPING);
  //
  //sweep : 
  max_freed = gc_sweep(gc_get_heap()->heap, HEAP_REST, &freed);
  max_freed = gc_sweep(gc_get_heap()->small_obj_heap, HEAP_SM, &freed);
  max_freed = gc_sweep(gc_get_heap()->medium_obj_heap, HEAP_MED, &freed);

  for (heap_type = 0; heap_type < 2; heap_type++) {
    while (cached_heap_free_sizes[heap_type] <
           (cached_heap_total_sizes[heap_type] * GC_FREE_THRESHOLD)) {
#if GC_DEBUG_TRACE
      fprintf(stdout, "Less than %f%% of the heap %d is free, growing it\n",
              100.0 * GC_FREE_THRESHOLD, heap_type);
#endif
      if (heap_type == HEAP_SM) {
        gc_grow_heap(gc_get_heap()->small_obj_heap, heap_type, 0, 0);
      } else if (heap_type == HEAP_MED) {
        gc_grow_heap(gc_get_heap()->medium_obj_heap, heap_type, 0, 0);
      } else if (heap_type == HEAP_REST) {
        gc_grow_heap(gc_get_heap()->heap, heap_type, 0, 0);
      }
    }
  }
#if GC_DEBUG_TRACE
  total_size = cached_heap_total_sizes[HEAP_SM] +
      cached_heap_total_sizes[HEAP_MED] + cached_heap_total_sizes[HEAP_REST];
  total_free = cached_heap_free_sizes[HEAP_SM] +
      cached_heap_free_sizes[HEAP_MED] + cached_heap_free_sizes[HEAP_REST];
  fprintf(stderr,
          "sweep done, total_size = %zu, total_free = %zu, freed = %zu, max_freed = %zu, elapsed = %zu\n",
          total_size, total_free, freed, max_freed,
          time(NULL) - gc_collector_start);
#endif
#if GC_DEBUG_TRACE
  fprintf(stderr, "cleaning up any old thread data\n");
#endif
  gc_free_old_thread_data();
  // Idle the GC thread
  ck_pr_cas_int(&gc_stage, STAGE_SWEEPING, STAGE_RESTING);
}

void *collector_main(void *arg)
{
  int stage;
  struct timespec tim;

#ifdef DEBUG_THREADS
  pthread_t tid = pthread_self();
  int sid = syscall(SYS_gettid);
  printf("GC thread LWP id is %d\n", sid);
  //printf("GC thread POSIX thread id is %d\n", tid);
#endif
  tim.tv_sec = 0;
//JAE TODO: this is still not good enough, seems memory grows still grows fast with this.
//alternatively, may want to consider shrinking the heap if possible after a collection, if it is
//sparse enough (would be difficult to do without relocations, though
  tim.tv_nsec = 100 * NANOSECONDS_PER_MILLISECOND;
  while (1) {
    stage = ck_pr_load_int(&gc_stage);
    if (stage != STAGE_RESTING) {
      gc_collector();
    }
    nanosleep(&tim, NULL);
  }
  return NULL;
}

static pthread_t collector_thread;

void gc_start_collector()
{
  if (pthread_create
      (&collector_thread, NULL, collector_main, &collector_thread)) {
    fprintf(stderr, "Error creating collector thread\n");
    exit(1);
  }
}

/////////////////////////////////////////////
// END tri-color marking section
/////////////////////////////////////////////

// Initialize runtime data structures for a thread.
// Must be called on the target thread itself during startup,
// to verify stack limits are setup correctly.
void gc_thread_data_init(gc_thread_data * thd, int mut_num, char *stack_base,
                         long stack_size)
{
  char stack_ref;
  thd->stack_start = stack_base;
#if STACK_GROWTH_IS_DOWNWARD
  thd->stack_limit = stack_base - stack_size;
#else
  thd->stack_limit = stack_base + stack_size;
#endif
  if (stack_overflow(stack_base, &stack_ref)) {
    fprintf(stderr,
            "Error: Stack is growing in the wrong direction! Rebuild with STACK_GROWTH_IS_DOWNWARD changed to %d\n",
            (1 - STACK_GROWTH_IS_DOWNWARD));
    exit(1);
  }
  thd->stack_traces = calloc(MAX_STACK_TRACES, sizeof(char *));
  thd->stack_trace_idx = 0;
  thd->stack_prev_frame = NULL;
  thd->mutations = NULL;
  thd->exception_handler_stack = NULL;
//  thd->thread = NULL;
  thd->thread_state = CYC_THREAD_STATE_NEW;
  //thd->mutator_num = mut_num;
  thd->jmp_start = malloc(sizeof(jmp_buf));
  thd->gc_args = malloc(sizeof(object) * NUM_GC_ARGS);
  thd->gc_num_args = 0;
  thd->moveBufLen = 0;
  gc_thr_grow_move_buffer(thd);
  thd->gc_alloc_color = ck_pr_load_int(&gc_color_clear);
  thd->gc_status = ck_pr_load_int(&gc_status_col);
  thd->pending_writes = 0;
  thd->last_write = 0;
  thd->last_read = 0;
  thd->mark_buffer = NULL;
  thd->mark_buffer_len = 128;
  thd->mark_buffer =
      vpbuffer_realloc(thd->mark_buffer, &(thd->mark_buffer_len));
  if (pthread_mutex_init(&(thd->lock), NULL) != 0) {
    fprintf(stderr, "Unable to initialize thread mutex\n");
    exit(1);
  }
}

void gc_thread_data_free(gc_thread_data * thd)
{
  if (thd) {
    if (pthread_mutex_destroy(&thd->lock) != 0) {
      // TODO: can only destroy the lock if it is unlocked. need to make sure we
      // can guarantee that is the case prior to making this call
      // On the other hand, can we just use sleep and a loop to retry??
      fprintf(stderr, "Thread mutex is locked, unable to free\n");
      exit(1);
    }
    if (thd->jmp_start)
      free(thd->jmp_start);
    if (thd->gc_args)
      free(thd->gc_args);
    if (thd->moveBuf)
      free(thd->moveBuf);
    if (thd->mark_buffer)
      free(thd->mark_buffer);
    if (thd->stack_traces)
      free(thd->stack_traces);
    if (thd->mutations) {
      clear_mutations(thd);
    }
    free(thd);
  }
}

/**
 * Called explicitly from a mutator thread to let the collector know
 * it (may) block for an unknown period of time.
 *
 * The current continuation is required so that we can trace over it 
 * in case the collector has to cooperate for the mutator.
 */
void gc_mutator_thread_blocked(gc_thread_data * thd, object cont)
{
  if (!ck_pr_cas_int((int *)&(thd->thread_state),
                     CYC_THREAD_STATE_RUNNABLE, CYC_THREAD_STATE_BLOCKED)) {
    fprintf(stderr,
            "Unable to change thread from runnable to blocked. status = %d\n",
            thd->thread_state);
    exit(1);
  }
  thd->gc_cont = cont;
  thd->gc_num_args = 0;         // Will be set later, after collection
}

void Cyc_apply_from_buf(void *data, int argc, object prim, object * buf);

/**
 * Called explicitly from a mutator thread to let the collector know
 * that it has finished blocking. In addition, if the collector 
 * cooperated on behalf of the mutator while it was blocking, the mutator
 * will move any remaining stack objects to the heap and longjmp.
 */
void gc_mutator_thread_runnable(gc_thread_data * thd, object result)
{
  char stack_limit;
  // Transition from blocked back to runnable using CAS.
  // If we are unable to transition back, assume collector
  // has cooperated on behalf of this mutator thread.
  if (!ck_pr_cas_int((int *)&(thd->thread_state),
                     CYC_THREAD_STATE_BLOCKED, CYC_THREAD_STATE_RUNNABLE)) {
//printf("DEBUG - Collector cooperated, wait for it to finish. status is %d\n", thd->thread_state);
    // wait for the collector to finish
    pthread_mutex_lock(&(thd->lock));
    pthread_mutex_unlock(&(thd->lock));
    // update thread status
    while (!ck_pr_cas_int((int *)&(thd->thread_state),
                          CYC_THREAD_STATE_BLOCKED_COOPERATING,
                          CYC_THREAD_STATE_RUNNABLE)) {
    }
    // Setup value to send to continuation
    thd->gc_args[0] = result;
    thd->gc_num_args = 1;
    // Move any remaining stack objects (should only be the result?) to heap
    gc_minor(thd, &stack_limit, thd->stack_start, thd->gc_cont, thd->gc_args,
             thd->gc_num_args);
    // Handle any pending marks from write barrier
    gc_sum_pending_writes(thd, 0);
//printf("DEBUG - Call into gc_cont after collector coop\n");
    // Whoa.
    longjmp(*(thd->jmp_start), 1);
  } else {
    // Collector didn't do anything; make a normal continuation call
    if (type_of(thd->gc_cont) == pair_tag || prim(thd->gc_cont)) {
      thd->gc_args[0] = result;
      Cyc_apply_from_buf(thd, 1, thd->gc_cont, thd->gc_args);
    } else {
      (((closure) (thd->gc_cont))->fn) (thd, 1, thd->gc_cont, result);
    }
  }
}

//// Unit testing:
//int main(int argc, char **argv) {
//  int a = 1, b = 2, c = 3, i;
//  void **buf = NULL;
//  int size = 1;
//
//  buf = vpbuffer_realloc(buf, &size);
//  printf("buf = %p, size = %d\n", buf, size);
//  buf = vpbuffer_add(buf, &size, 0, &a);
//  printf("buf = %p, size = %d\n", buf, size);
//  buf = vpbuffer_add(buf, &size, 1, &b);
//  printf("buf = %p, size = %d\n", buf, size);
//  buf = vpbuffer_add(buf, &size, 2, &c);
//  printf("buf = %p, size = %d\n", buf, size);
//  buf = vpbuffer_add(buf, &size, 3, &a);
//  printf("buf = %p, size = %d\n", buf, size);
//  buf = vpbuffer_add(buf, &size, 4, &b);
//  printf("buf = %p, size = %d\n", buf, size);
//  for (i = 5; i < 20; i++) {
//    buf = vpbuffer_add(buf, &size, i, &c);
//  }
//
//  for (i = 0; i < 20; i++){
//    printf("%d\n", *((int *) buf[i]));
//  }
//  vpbuffer_free(buf);
//  printf("buf = %p, size = %d\n", buf, size);
//  return 0;
//}
//
