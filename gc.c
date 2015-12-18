/** 
 * Cyclone Scheme
 * Copyright (c) 2015, Justin Ethier
 * All rights reserved.
 *
 * Primary garbage collector used by the Cyclone runtime. 
 * Based on the tracing GC algorithm from:
 * "Implementing an on-the-fly garbage collector for Java", by Domani et al.
 *
 * The heap implementation (alloc / sweep, etc) is based on code from Chibi Scheme.
 */

#include "cyclone/types.h"

////////////////////
// Global variables

static const int NANOSECONDS_PER_MILLISECOND = 1000000;

// Note: will need to use atomics and/or locking to access any
// variables shared between threads
static int        gc_color_mark = 1; // Black, is swapped during GC
static int        gc_color_clear = 3; // White, is swapped during GC
//static const int  gc_color_grey = 4; // TODO: appears unused, clean up
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
// Note this assumes a single overall heap "chain". Code would need to
// be modified to support multiple independent heaps
static int cached_heap_free_size = 0;
static int cached_heap_total_size = 0;

// Data for each individual mutator thread
static gc_thread_data **Cyc_mutators;
static int Cyc_num_mutators;

/////////////
// Functions

// Perform one-time initialization before mutators can be executed
void gc_initialize()
{
  // TODO: alloca this using a vpbuffer, or maybe another type of data structure??
  // Will need this list for later use, but only by the collector thread. so it would be
  // nice if there was a way to allocate mutators that avoids expensive synchronization...
  // need to think on this when adding thread support, after upgrading the collector
  Cyc_num_mutators = 1; 
  Cyc_mutators = calloc(Cyc_num_mutators, sizeof(gc_thread_data *));

  // Initialize collector's mark stack
  mark_stack_len = 128;
  mark_stack = vpbuffer_realloc(mark_stack, &(mark_stack_len));

  // Here is as good a place as any to do this...
  if (pthread_mutex_init(&(heap_lock), NULL) != 0) {
    fprintf(stderr, "Unable to initialize heap_lock mutex\n");
    exit(1);
  }
}

// Add data for a new mutator
int gc_add_mutator(gc_thread_data *thd)
{
  // TODO: need to sync access to these static variables. both here and
  // elsewhere in the module!!
  int i;
  for (i = 0; i < Cyc_num_mutators; i++) {
    if (!Cyc_mutators[i]) {
      Cyc_mutators[i] = thd;
      return i;
    }
  }
  // TODO: unable to create any more mutators. what to do???
  fprintf(stderr, "Unable to create a new thread, exiting\n");
  exit(1);
  return -1;
}

gc_heap *gc_heap_create(size_t size, size_t max_size, size_t chunk_size)
{
  gc_free_list *free, *next;
  gc_heap *h;
  // TODO: mmap?
  h = malloc(gc_heap_pad_size(size));
  if (!h) return NULL;
  h->size = size;
  //h->free_size = size;
  cached_heap_total_size += size;
  cached_heap_free_size += size;
  h->chunk_size = chunk_size;
  h->max_size = max_size;
  h->data = (char *) gc_heap_align(sizeof(h->data) + (uint)&(h->data)); 
  h->next = NULL;
  free = h->free_list = (gc_free_list *)h->data;
  next = (gc_free_list *)(((char *) free) + gc_heap_align(gc_free_chunk_size));
  free->size = 0; // First one is just a dummy record
  free->next = next;
  next->size = size - gc_heap_align(gc_free_chunk_size);
  next->next = NULL;
#if GC_DEBUG_PRINTFS
  fprintf(stderr, "DEBUG h->data addr: %p\n", &(h->data));
  fprintf(stderr, "DEBUG h->data addr: %p\n", h->data);
  fprintf(stderr, ("heap: %p-%p data: %p-%p size: %d\n"),
          h, ((char*)h)+gc_heap_pad_size(size), h->data, h->data + size, size);
  fprintf(stderr, ("first: %p end: %p\n"),
          (object)gc_heap_first_block(h), (object)gc_heap_end(h));
  fprintf(stderr, ("free1: %p-%p free2: %p-%p\n"),
          free, ((char*)free)+free->size, next, ((char*)next)+next->size);
#endif
  return h;
}

// Copy given object into given heap object
char *gc_copy_obj(object dest, char *obj, gc_thread_data *thd)
{
  // NOTE: no additional type checking because this is called from gc_move
  // which already does that

  switch(type_of(obj)){
    case cons_tag: {
      list hp = dest;
      hp->hdr.mark = thd->gc_alloc_color;
      type_of(hp) = cons_tag;
      car(hp) = car(obj);
      cdr(hp) = cdr(obj);
      return (char *)hp;
    }
    case macro_tag: {
      macro_type *hp = dest;
      mark(hp) = thd->gc_alloc_color;
      type_of(hp) = macro_tag;
      hp->fn = ((macro) obj)->fn;
      hp->num_args = ((macro) obj)->num_args;
      return (char *)hp;
    }
    case closure0_tag: {
      closure0_type *hp = dest;
      mark(hp) = thd->gc_alloc_color;
      type_of(hp) = closure0_tag;
      hp->fn = ((closure0) obj)->fn;
      hp->num_args = ((closure0) obj)->num_args;
      return (char *)hp;
    }
    case closure1_tag: {
      closure1_type *hp = dest;
      mark(hp) = thd->gc_alloc_color;
      type_of(hp) = closure1_tag;
      hp->fn = ((closure1) obj)->fn;
      hp->num_args = ((closure1) obj)->num_args;
      hp->elt1 = ((closure1) obj)->elt1;
      return (char *)hp;
    }
    case closure2_tag: {
      closure2_type *hp = dest;
      mark(hp) = thd->gc_alloc_color;
      type_of(hp) = closure2_tag;
      hp->fn = ((closure2) obj)->fn;
      hp->num_args = ((closure2) obj)->num_args;
      hp->elt1 = ((closure2) obj)->elt1;
      hp->elt2 = ((closure2) obj)->elt2;
      return (char *)hp;
    }
    case closure3_tag: {
      closure3_type *hp = dest;
      mark(hp) = thd->gc_alloc_color;
      type_of(hp) = closure3_tag;
      hp->fn = ((closure3) obj)->fn;
      hp->num_args = ((closure3) obj)->num_args;
      hp->elt1 = ((closure3) obj)->elt1;
      hp->elt2 = ((closure3) obj)->elt2;
      hp->elt3 = ((closure3) obj)->elt3;
      return (char *)hp;
    }
    case closure4_tag: {
      closure4_type *hp = dest;
      mark(hp) = thd->gc_alloc_color;
      type_of(hp) = closure4_tag;
      hp->fn = ((closure4) obj)->fn;
      hp->num_args = ((closure4) obj)->num_args;
      hp->elt1 = ((closure4) obj)->elt1;
      hp->elt2 = ((closure4) obj)->elt2;
      hp->elt3 = ((closure4) obj)->elt3;
      hp->elt4 = ((closure4) obj)->elt4;
      return (char *)hp;
    }
    case closureN_tag: {
      int i;
      closureN_type *hp = dest;
      mark(hp) = thd->gc_alloc_color;
      type_of(hp) = closureN_tag;
      hp->fn = ((closureN) obj)->fn;
      hp->num_args = ((closureN) obj)->num_args;
      hp->num_elt = ((closureN) obj)-> num_elt;
      hp->elts = (object *)(((char *)hp) + sizeof(closureN_type));
      for (i = 0; i < hp->num_elt; i++) {
        hp->elts[i] = ((closureN) obj)->elts[i];
      }
      return (char *)hp;
    }
    case vector_tag: {
      int i;
      vector_type *hp = dest;
      mark(hp) = thd->gc_alloc_color;
      type_of(hp) = vector_tag;
      hp->num_elt = ((vector) obj)-> num_elt;
      hp->elts = (object *)(((char *)hp) + sizeof(vector_type));
      for (i = 0; i < hp->num_elt; i++) {
        hp->elts[i] = ((vector) obj)->elts[i];
      }
      return (char *)hp;
    }
    case string_tag: {
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
    case integer_tag: {
      integer_type *hp = dest; 
      mark(hp) = thd->gc_alloc_color;
      type_of(hp) = integer_tag;
      hp->value = ((integer_type *) obj)->value;
      return (char *)hp;
    }
    case double_tag: {
      double_type *hp = dest;
      mark(hp) = thd->gc_alloc_color;
      type_of(hp) = double_tag;
      hp->value = ((double_type *) obj)->value;
      return (char *)hp;
    }
    case port_tag: {
      port_type *hp = dest;
      mark(hp) = thd->gc_alloc_color;
      type_of(hp) = port_tag;
      hp->fp = ((port_type *) obj)->fp;
      hp->mode = ((port_type *) obj)->mode;
      return (char *)hp;
    }
    case cvar_tag: {
      cvar_type *hp = dest;
      mark(hp) = thd->gc_alloc_color;
      type_of(hp) = cvar_tag;
      hp->pvar = ((cvar_type *) obj)->pvar;
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
      fprintf(stderr, "gc_copy_obj: bad tag obj=%p obj.tag=%ld\n",(object) obj, type_of(obj));
      exit(1);
  }
  return (char *)obj;
}

int gc_grow_heap(gc_heap *h, size_t size, size_t chunk_size)
{
  size_t cur_size, new_size;
  gc_heap *h_last, *h_new;
  pthread_mutex_lock(&heap_lock);
  h_last = gc_heap_last(h);
  cur_size = h_last->size;
  // JAE - For now, just add a new page
  new_size = cur_size; //gc_heap_align(((cur_size > size) ? cur_size : size) * 2);
  h_new = gc_heap_create(new_size, h_last->max_size, chunk_size);
  h_last->next = h_new;
  pthread_mutex_unlock(&heap_lock);
#if GC_DEBUG_TRACE
  fprintf(stderr, "DEBUG - grew heap\n");
#endif
  return (h_new != NULL);
}

void *gc_try_alloc(gc_heap *h, size_t size, char *obj, gc_thread_data *thd) 
{
  gc_free_list *f1, *f2, *f3;
  pthread_mutex_lock(&heap_lock);
  for (; h; h = h->next) { // All heaps
    // TODO: chunk size (ignoring for now)

    for (f1 = h->free_list, f2 = f1->next; f2; f1 = f2, f2 = f2->next) { // all free in this heap
      if (f2->size >= size) { // Big enough for request
        // TODO: take whole chunk or divide up f2 (using f3)?
        if (f2->size >= (size + gc_heap_align(1) /* min obj size */)) {
          f3 = (gc_free_list *) (((char *)f2) + size);
          f3->size = f2->size - size;
          f3->next = f2->next;
          f1->next = f3;
        } else { /* Take the whole chunk */
          f1->next = f2->next;
        }
        // Copy object into heap now to avoid any uninitialized memory issues
        gc_copy_obj(f2, obj, thd);
        //h->free_size -= gc_allocated_bytes(obj, NULL, NULL);
        cached_heap_free_size -= gc_allocated_bytes(obj, NULL, NULL);
        pthread_mutex_unlock(&heap_lock);
        return f2;
      }
    }
  }
  pthread_mutex_unlock(&heap_lock);
  return NULL; 
}

//TODO: need a heap lock.
//lock during - alloc, sweep? but now sweep becomes a stop the world...
// maybe only lock during each individual operation, not for a whole
// sweep or alloc

void *gc_alloc(gc_heap *h, size_t size, char *obj, gc_thread_data *thd, int *heap_grown) 
{
  void *result = NULL;
  size_t max_freed = 0, sum_freed = 0, total_size;
  // TODO: check return value, if null (could not alloc) then 
  // run a collection and check how much free space there is. if less
  // the allowed ratio, try growing heap.
  // then try realloc. if cannot alloc now, then throw out of memory error
  size = gc_heap_align(size);
  result = gc_try_alloc(h, size, obj, thd);
  if (!result) {
    // TODO: may want to consider not doing this now, and implementing gc_collect as
    // part of the runtime, since we would have all of the roots, stack args, 
    // etc available there.
//    max_freed = gc_collect(h); TODO: this does not work yet!
//
//    total_size = gc_heap_total_size(h);
//    if (((max_freed < size) ||
//         ((total_size > sum_freed) &&
//          (total_size - sum_freed) > (total_size * 0.75))) // Grow ratio
//        && ((!h->max_size) || (total_size < h->max_size))) {
      gc_grow_heap(h, size, 0);
      *heap_grown = 1;
//    }
    result = gc_try_alloc(h, size, obj, thd);
    if (!result) {
      fprintf(stderr, "out of memory error allocating %d bytes\n", size);
      exit(1); // TODO: throw error???
    }
  }
#if GC_DEBUG_TRACE
  fprintf(stderr, "alloc %p size = %d, obj=%p, tag=%ld, mark=%d\n", result, size, obj, type_of(obj), mark(((object)result)));
  //// TODO: Debug check, remove (ifdef it) once GC is stabilized
  //if (is_value_type(result)) {
  //  printf("Invalid allocated address - is a value type %p\n", result);
  //}
#endif
  return result;
}

size_t gc_allocated_bytes(object obj, gc_free_list *q, gc_free_list *r)
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
  if (t == cons_tag) return gc_heap_align(sizeof(cons_type));
  if (t == macro_tag) return gc_heap_align(sizeof(macro_type));
  if (t == closure0_tag) return gc_heap_align(sizeof(closure0_type));
  if (t == closure1_tag) return gc_heap_align(sizeof(closure1_type));
  if (t == closure2_tag) return gc_heap_align(sizeof(closure2_type));
  if (t == closure3_tag) return gc_heap_align(sizeof(closure3_type));
  if (t == closure4_tag) return gc_heap_align(sizeof(closure4_type));
  if (t == closureN_tag){
    return gc_heap_align(sizeof(closureN_type) + sizeof(object) * ((closureN_type *)obj)->num_elt);
  }
  if (t == vector_tag){
    return gc_heap_align(sizeof(vector_type) + sizeof(object) * ((vector_type *)obj)->num_elt);
  }
  if (t == string_tag){
    return gc_heap_align(sizeof(string_type) + string_len(obj) + 1);
  }
  if (t == integer_tag) return gc_heap_align(sizeof(integer_type));
  if (t == double_tag) return gc_heap_align(sizeof(double_type));
  if (t == port_tag) return gc_heap_align(sizeof(port_type));
  if (t == cvar_tag) return gc_heap_align(sizeof(cvar_type));
  
  fprintf(stderr, "gc_allocated_bytes: unexpected object %p of type %ld\n", obj, t);
  exit(1);
  return 0;
}

gc_heap *gc_heap_last(gc_heap *h)
{
  while (h->next)
    h = h->next;
  return h;
}

size_t gc_heap_total_size(gc_heap *h)
{
  size_t total_size = 0;
  pthread_mutex_lock(&heap_lock);
  while(h) {
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

size_t gc_sweep(gc_heap *h, size_t *sum_freed_ptr)
{
  size_t freed, max_freed=0, heap_freed = 0, sum_freed=0, size;
  object p, end;
  gc_free_list *q, *r, *s;

  //
  // Lock the heap to prevent issues with allocations during sweep
  // It sucks to have to use a coarse-grained lock like this, but let's
  // be safe and prevent threading issues right now. Once the new GC
  // works we can go back and try to speed things up (if possible)
  // by using more fine-grained locking. Can also profile to see
  // how much time is even spent sweeping
  //
  pthread_mutex_lock(&heap_lock);
  for (; h; h = h->next) { // All heaps
#if GC_DEBUG_TRACE
    fprintf(stderr, "sweep heap %p, size = %d\n", h, h->size);
#endif
    p = gc_heap_first_block(h);
    q = h->free_list;
    end = gc_heap_end(h);
    while (p < end) {
      // find preceding/succeeding free list pointers for p
      for (r = q->next; r && ((char *)r < (char *)p); q=r, r=r->next);

      if ((char *)r == (char *)p) { // this is a free block, skip it
        p = (object) (((char *)p) + r->size);
#if GC_DEBUG_TRACE
        fprintf(stderr, "skip free block %p size = %d\n", p, r->size);
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
        fprintf(stderr, "sweep: bad size at %p + %d > %p", p, size, r);
        exit(1);
      }
#endif

      if (mark(p) == gc_color_clear) {
#if GC_DEBUG_VERBOSE
        fprintf(stderr, "sweep is freeing unmarked obj: %p with tag %ld\n", p, type_of(p));
#endif
        mark(p) = gc_color_blue; // Needed?
        // free p
        heap_freed += size;
        if (((((char *)q) + q->size) == (char *)p) && (q != h->free_list)) {
          /* merge q with p */
          if (r && r->size && ((((char *)p)+size) == (char *)r)) {
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
          s = (gc_free_list *)p;
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
        p = (object)(((char *)p) + size);
      }
    }
    //h->free_size += heap_freed;
    cached_heap_free_size += heap_freed;
    sum_freed += heap_freed;
    heap_freed = 0;
  }
  pthread_mutex_unlock(&heap_lock);
  if (sum_freed_ptr) *sum_freed_ptr = sum_freed;
  return max_freed;
}

void gc_thr_grow_move_buffer(gc_thread_data *d)
{
  if (!d) return;

  if (d->moveBufLen == 0) { // Special case
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

void gc_thr_add_to_move_buffer(gc_thread_data *d, int *alloci, object obj)
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


// void gc_init()
// {
// }
// END heap definitions


/*
Rough plan for how to implement new GC algorithm. We need to do this in
phases in order to have any hope of getting everything working. Let's prove
the algorithm out, then extend support to multiple mutators if everything
looks good.

PHASE 1 - separation of mutator and collector into separate threads

need to syncronize access (preferably via atomics) for anything shared between the 
collector and mutator threads.

can cooperate be part of a minor gc? in that case, the 
marking could be done as part of allocation

but then what exactly does that mean, to mark gray? because
objects moved to the heap will be set to mark color at that 
point (until collector thread finishes). but would want
objects on the heap referenced by them to be traced, so 
I suppose that is the purpose of the gray, to indicate
those still need to be traced. but need to think this through,
do we need the markbuffer and last read/write? do those make
  sense with mta approach (assume so)???

ONLY CONCERN - what happens if an object on the stack 
has a reference to an object on the heap that is collected?
but how would this happen? collector marks global roots before
telling mutators to go to async, and once mutators go async
any allocations will not be collected. also once collectors go
async they have a chance to markgray, which will include the write
barrier. so given that, is it still possible for an old heap ref to 
sneak into a stack object during the async phase?

more questions on above point:
- figure out how/if after cooperation/async, can a stack object pick
  up a reference to a heap object that will be collected during that GC cycle?
  need to be able to prevent this somehow...

- need to figure out real world use case(s) where this could happen, to try and
  figure out how to address this problem

from my understanding of the paper, the write barrier prevents this. consider, at the
start of async, the mutator's roots, global roots, and anything on the write barrier
have been marked. any new objects will be allocated as marked. that way, anything the
mutator could later access is either marked or will be after tracing. the only exception
is if the mutator changes a reference such that tracing will no longer find an object.
but the write barrier prevents this - during tracing a heap update causes the old
object to be marked as well. so it will eventually be traced, and there should be no
dangling objects after GC completes.

PHASE 2 - multi-threaded mutator (IE, more than one stack thread):

- how does the collector handle stack objects that reference objects from 
  another thread's stack?
  * minor GC will only relocate that thread's objects, so another thread's would not
    be moved. however, if another thread references one of the GC'd thread's
    stack objects, it will now get a forwarding pointer. even worse, what if the
    other thread is blocked and the reference becomes corrupt due to the stack
    longjmp? there are major issues with one thread referencing another thread's
    objects.
  * had considered adding a stack bit to the object header. if we do this and
    initialize it during object creation, a thread could in theory detect
    if an object belongs to another thread. but it might be expensive because
    a read barrier would have to be used to check the object's stack bit and
    address (to see if it is on this heap).
  * alternatively, how would one thread pick up a reference to another one's
    objects? are there any ways to detect these events and deal with them?
    it might be possible to detect such a case and allocate the object on the heap,
    replacing it with a fwd pointer. unfortunately that means we need a read 
    barrier (ick) to handle forwarding pointers in arbitrary places
  * but does that mean we need a fwd pointer to be live for awhile? do we need
    a read barrier to get this to work? obviously we want to avoid a read barrier
    at all costs.
- what are the real costs of allowing forwarding pointers to exist outside of just
  minor GC? assume each runtime primitive would need to be updated to handle the
  case where the obj is a fwd pointer - is it just a matter of each function 
  detecting this and (possibly) calling itself again with the 'real' address?
  obviously that makes the runtime slower due to more checks, but maybe it is
  not *so* bad?
*/

// tri-color GC section, WIP

/////////////////////////////////////////////
// GC functions called by the Mutator threads

/**
 * Determine if object lives on the thread's stack
 */
int gc_is_stack_obj(gc_thread_data *thd, object obj)
{
  char tmp;
  object low_limit = &tmp;
  object high_limit = thd->stack_start;
  return (check_overflow(low_limit, obj) && 
          check_overflow(obj, high_limit));
}

/**
Write barrier for updates to heap-allocated objects
Plans:
The key for this barrier is to identify stack objects that contain
heap references, so they can be marked to avoid collection.
*/
void gc_mut_update(gc_thread_data *thd, object old_obj, object value)
{
  int status = ATOMIC_GET(&gc_status_col),
      stage = ATOMIC_GET(&gc_stage);
  if (thd->gc_status != STATUS_ASYNC) {
//fprintf(stderr, "DEBUG - GC sync marking heap obj %p ", old_obj);
//Cyc_display(old_obj, stderr);
//fprintf(stderr, " and new value %p ", value);
//Cyc_display(value, stderr);
////fprintf(stderr, " for heap object ");
//fprintf(stderr, "\n");
    pthread_mutex_lock(&(thd->lock));
    gc_mark_gray(thd, old_obj);
    // Check if value is on the heap. If so, mark gray right now,
    // otherwise set it to be marked after moved to heap by next GC
    if (gc_is_stack_obj(thd, value)) {
      grayed(value) = 1;
    } else {
      gc_mark_gray(thd, value);
    }
    pthread_mutex_unlock(&(thd->lock));
  } else if (stage == STAGE_TRACING) {
//fprintf(stderr, "DEBUG - GC async tracing marking heap obj %p ", old_obj);
//Cyc_display(old_obj, stderr);
//fprintf(stderr, "\n");
    pthread_mutex_lock(&(thd->lock));
    gc_mark_gray(thd, old_obj);
    pthread_mutex_unlock(&(thd->lock));
#if GC_DEBUG_VERBOSE
    if (is_object_type(old_obj) && mark(old_obj) == gc_color_clear) {
      fprintf(stderr, "added to mark buffer (trace) from write barrier %p:mark %d:", old_obj, mark(old_obj));
      Cyc_display(old_obj, stderr);
      fprintf(stderr, "\n");
    }
#endif
  }
// TODO: concerned there may be an issue here with a stack object
// having a 'tree' of references that contains heap objects. these
// objects would be skipped and would never be grayed by the current
// code:
//
//  the paper marks both the heap location being written to and the
//  value being written. not sure it makes sense to mark the value 
//  as it will always be on the stack - issue is if any obj's it is
//  referencing are on the heap. this is where that stack bit might
//  come in handy.
//
//  do we want to mark gray immediately during add mutator, or wait
//  until minor GC? YES - I think for mutators we need to mark the
//  object gray immediately. otherwise if we delay until GC, a sweep
//  may have already finished up and freed such an obj that would
//  otherwise not have been freed if we had waited.
//
//  again, only potential issue seems to be if a stack obj could ref
//  something else on the heap - can that happen? I think this can only
//  happen if the heap obj it refs is linked to a root, because stack
//  objs are so short lived??
//
//  also we already know if objects are on the stack due to their color (RED).
//  so can use this to not mark red values. otherwise probably do want
//  to mark the 'y' as well (per paper) to prevent timing issues when we wait
//
// do have some concern though that mark_gray will stop when a stack obj
// is detected, and the code will not examine any refs held by the stack obj.
}

// TODO: still need to handle case where a mutator is blocked
void gc_mut_cooperate(gc_thread_data *thd, int buf_len)
{
  int i, status = ATOMIC_GET(&gc_status_col),
      stage = ATOMIC_GET(&gc_stage);
#if GC_DEBUG_VERBOSE
  int debug_print = 0;
#endif

  // Handle any pending marks from write barrier
  pthread_mutex_lock(&(thd->lock));
  thd->last_write += thd->pending_writes;
  thd->pending_writes = 0;
  pthread_mutex_unlock(&(thd->lock));

  if (thd->gc_status != status) {
    if (thd->gc_status == STATUS_ASYNC) {
      // Async is done, so clean up old mark data from the last collection
      pthread_mutex_lock(&(thd->lock));
      thd->last_write = 0;
      thd->last_read = 0;
      thd->pending_writes = 0;
      pthread_mutex_unlock(&(thd->lock));
    }
    else if (thd->gc_status == STATUS_SYNC2) {
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
      thd->gc_alloc_color = ATOMIC_GET(&gc_color_mark);
    }
    thd->gc_status = status;
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
  if (stage == STAGE_RESTING &&
      (cached_heap_free_size < (cached_heap_total_size * 0.50))){
#if GC_DEBUG_TRACE
    fprintf(stdout, "Less than 50%% of the heap is free, initiating collector\n");
#endif
    ATOMIC_SET_IF_EQ(&gc_stage, STAGE_RESTING, STAGE_CLEAR_OR_MARKING);

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
void gc_mark_gray(gc_thread_data *thd, object obj)
{
  // From what I can tell, no other thread would be modifying
  // either object type or mark. Both should be stable once the object is placed
  // into the heap, with the collector being the only thread that changes marks.
  if (is_object_type(obj) && mark(obj) == gc_color_clear) { // TODO: sync??
    // Place marked object in a buffer to avoid repeated scans of the heap.
// TODO:
// Note that ideally this should be a lock-free data structure to make the
// algorithm more efficient. So this code (and the corresponding collector
// trace code) should be converted at some point.
    thd->mark_buffer = vpbuffer_add(thd->mark_buffer, 
                                    &(thd->mark_buffer_len),
                                    thd->last_write,
                                    obj);
    (thd->last_write)++; // Already locked, just do it...
  }
}

void gc_mark_gray2(gc_thread_data *thd, object obj)
{
  if (is_object_type(obj) && mark(obj) == gc_color_clear) {
    thd->mark_buffer = vpbuffer_add(thd->mark_buffer, 
                                    &(thd->mark_buffer_len),
                                    (thd->last_write + thd->pending_writes),
                                    obj);
    thd->pending_writes++;
  }
}

// TODO: before doing this, may want to debug a bit and see what is
// being passed to gc_mut_update. see if those objects tend to have
// any heap refs. may need to add debug code to do that...
//
//
//// This is called from the heap write barrier. The issue here is that
//// this is not called during GC, so obj and some of its refs may be
//// on the stack. So scan all refs and mark the ones that are on the heap
//void gc_mark_gray_rec(gc_thread_data *thd, object obj)
//{
//  int mark;
//
//  if (is_object_type(obj)) {
//    mark = mark(obj);
//
//// TODO: if we leave red as red and keep going, this could hang
//// if there is a cycle!!
//  }&& mark(obj) == gc_color_clear) { // TODO: sync??
//
//}

void gc_collector_trace()
{
  gc_thread_data *m;
  int clean = 0, i;
  while (!clean) {
    clean = 1;
    // TODO: need to sync access to mutator int/void data, UNLESS
    // the collector thread is the only one that is using these
    // fields.
    for (i = 0; i < Cyc_num_mutators; i++) {
      m = Cyc_mutators[i];
// TODO: ideally, want to use a lock-free data structure to prevent
// having to use a mutex here. see corresponding code in gc_mark_gray

//TODO: I think this locking is too coarse. observing immediate failures with the recent change to g_mark_gray locking and
//wonder if the problem is that this locking will prevent a batch of changes from being seen.
//you know, do we really need locking here? the last read/write can be made atomic, and any reads/writes to mark buffer can
//be made atomic as well. I think we may need a dirty flag to let the collector know something is happening when the mark buffer
//needs to be resized, but other than that it this good enough?
//on the other hand, a central issue with this collector is when can we be sure that we are existing tracing at the right time, and
//not too early? because an early exit here will surely mean that objects are incorrectly freed 
      pthread_mutex_lock(&(m->lock));
      while (m->last_read < m->last_write) {
        clean = 0;
#if GC_DEBUG_VERBOSE
        fprintf(stderr, "gc_mark_black mark buffer %p, last_read = %d last_write = %d\n", 
          (m->mark_buffer)[m->last_read],
          m->last_read, m->last_write);
#endif
        gc_mark_black((m->mark_buffer)[m->last_read]);
        gc_empty_collector_stack();
        (m->last_read)++; // Inc here to prevent off-by-one error
      }
      pthread_mutex_unlock(&(m->lock));

      // Try checking the condition once more after giving the
      // mutator a chance to respond, to prevent exiting early.
      // This is experimental, not sure if it is necessary
      if (clean) {
        pthread_mutex_lock(&(m->lock));
        if (m->last_read < m->last_write) {
          fprintf(stderr, "JAE DEBUG - might have exited trace early\n");
          clean = 0;
        }
        else if (m->pending_writes) {
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
  int markColor = ATOMIC_GET(&gc_color_mark);
  if (is_object_type(obj) && mark(obj) != markColor) {
    // Gray any child objects
    // Note we probably should use some form of atomics/synchronization
    // for cons and vector types, as these pointers could change.
    switch(type_of(obj)) {
      case cons_tag: {
        gc_collector_mark_gray(obj, car(obj));
        gc_collector_mark_gray(obj, cdr(obj));
        break;
      }
      case closure1_tag:
        gc_collector_mark_gray(obj, ((closure1) obj)->elt1);
        break;
      case closure2_tag:
        gc_collector_mark_gray(obj, ((closure2) obj)->elt1);
        gc_collector_mark_gray(obj, ((closure2) obj)->elt2);
      case closure3_tag:
        gc_collector_mark_gray(obj, ((closure3) obj)->elt1);
        gc_collector_mark_gray(obj, ((closure3) obj)->elt2);
        gc_collector_mark_gray(obj, ((closure3) obj)->elt3);
      case closure4_tag:
        gc_collector_mark_gray(obj, ((closure4) obj)->elt1);
        gc_collector_mark_gray(obj, ((closure4) obj)->elt2);
        gc_collector_mark_gray(obj, ((closure4) obj)->elt3);
        gc_collector_mark_gray(obj, ((closure4) obj)->elt4);
        break;
      case closureN_tag: {
        int i, n = ((closureN) obj)->num_elt;
        for (i = 0; i < n; i++) {
          gc_collector_mark_gray(obj, ((closureN) obj)->elts[i]);
        }
        break;
      }
      case vector_tag: {
        int i, n = ((vector) obj)->num_elt;
        for (i = 0; i < n; i++) {
          gc_collector_mark_gray(obj, ((vector) obj)->elts[i]);
        }
        break;
      }
      case cvar_tag: {
        cvar_type *c = (cvar_type *)obj;
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
    fprintf(stderr, "mark gray parent = %p (%ld) obj = %p\n", parent, type_of(parent), obj);
#endif
  }
}

void gc_empty_collector_stack()
{
  // Mark stack is only used by the collector thread, so no sync needed
  while (mark_stack_i > 0) { // not empty
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
  int status = ATOMIC_GET(&gc_status_col);
  while (!ATOMIC_SET_IF_EQ(&gc_status_col, status, s)){}
}

void gc_wait_handshake()
{
  int i, statusm, statusc;
  struct timespec tim;
  tim.tv_sec = 0;
  tim.tv_nsec = 1000000; // 1 millisecond

  // TODO: same as in other places, need to either sync access to
  // mutator vars, or ensure only the collector uses them
  for (i = 0; i < Cyc_num_mutators; i++) {
    while (1) {
      statusc = ATOMIC_GET(&gc_status_col);
      statusm = ATOMIC_GET(&(Cyc_mutators[i]->gc_status));
      if (statusc == statusm) {
        // Handshake succeeded, check next mutator
        break;
      }

      // At least for now, just give up quantum and come back to
      // this quickly to test again. This probably could be more
      // efficient.
      // TODO: also need to consider mutators that are blocked and
      // not cooperating.
      nanosleep(&tim, NULL);     
    }
  }
}

/////////////////////////////////////////////
// GC Collection cycle

//TODO: create function to print globals, ideally want names and the mark flag.
//then call before/after tracing to see if we can catch a global not being marked.
//want to rule out an issue here, since we have seen globals that were corrupted (IE, appears they were collected)
void debug_dump_globals();

// Main collector function
void gc_collector()
{
  int old_clear, old_mark;
  size_t freed = 0, max_freed = 0, total_size, total_free;
#if GC_DEBUG_TRACE
  time_t sweep_start = time(NULL);
#endif
  //clear : 
  ATOMIC_SET_IF_EQ(&gc_stage, STAGE_RESTING, STAGE_CLEAR_OR_MARKING);
  // exchange values of markColor and clearColor
  old_clear = ATOMIC_GET(&gc_color_clear);
  old_mark = ATOMIC_GET(&gc_color_mark);
  while(!ATOMIC_SET_IF_EQ(&gc_color_clear, old_clear, old_mark)){}
  while(!ATOMIC_SET_IF_EQ(&gc_color_mark, old_mark, old_clear)){}
#if GC_DEBUG_TRACE
  fprintf(stderr, "DEBUG - swap clear %d / mark %d\n", gc_color_clear, gc_color_mark);
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
  ATOMIC_SET_IF_EQ(&gc_stage, STAGE_CLEAR_OR_MARKING, STAGE_TRACING);
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
  ATOMIC_SET_IF_EQ(&gc_stage, STAGE_TRACING, STAGE_SWEEPING);
  //
  //sweep : 
  max_freed = gc_sweep(gc_get_heap(), &freed);
  total_size = cached_heap_total_size; //gc_heap_total_size(gc_get_heap());
  total_free = cached_heap_free_size; //gc_heap_total_free_size(gc_get_heap());

  if (total_free < (total_size * 0.10)) {
#if GC_DEBUG_TRACE
    fprintf(stdout, "Less than 10%% of the heap is free, growing it\n", 
      total_free, total_size);
#endif
    gc_grow_heap(gc_get_heap(), 0, 0);
  }
#if GC_DEBUG_TRACE
  fprintf(stderr, "sweep done, total_size = %d, total_free = %d, freed = %d, max_freed = %d, elapsed = %ld\n", 
    total_size, total_free, 
    freed, max_freed, time(NULL) - sweep_start);
#endif
  ATOMIC_SET_IF_EQ(&gc_stage, STAGE_SWEEPING, STAGE_RESTING);
}

void *collector_main(void *arg)
{
  int stage;
  struct timespec tim;
  tim.tv_sec = 0;
//JAE TODO: this is still not good enough, seems memory grows still grows fast with this.
//alternatively, may want to consider shrinking the heap if possible after a collection, if it is
//sparse enough (would be difficult to do without relocations, though
  tim.tv_nsec = 100 * NANOSECONDS_PER_MILLISECOND;
  while (1) {
    stage = ATOMIC_GET(&gc_stage);
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
  if (pthread_create(&collector_thread, NULL, collector_main, &collector_thread)) {
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
void gc_thread_data_init(gc_thread_data *thd, int mut_num, char *stack_base, long stack_size)
{
  char stack_ref;
  thd->stack_start = stack_base;
#if STACK_GROWS_DOWNWARD
  thd->stack_limit = stack_base - stack_size;
#else
  thd->stack_limit = stack_base + stack_size;
#endif
  if (check_overflow(stack_base, &stack_ref)){
    fprintf(stderr, 
      "Error: recompile with STACK_GROWS_DOWNWARD set to %d\n",
      (1 - STACK_GROWS_DOWNWARD));
    exit(1);
  }
  thd->stack_traces = calloc(MAX_STACK_TRACES, sizeof(char *));
  thd->stack_trace_idx = 0;
  thd->stack_prev_frame = NULL;
  //thd->mutator_num = mut_num;
  thd->jmp_start = malloc(sizeof(jmp_buf));
  thd->gc_args = malloc(sizeof(object) * NUM_GC_ANS);
  thd->gc_num_args = 0;
  thd->moveBufLen = 0;
  gc_thr_grow_move_buffer(thd);
  thd->gc_alloc_color = ATOMIC_GET(&gc_color_clear);
  thd->gc_status = ATOMIC_GET(&gc_status_col);
  thd->last_write = 0;
  thd->last_read = 0;
  thd->mark_buffer_len = 128;
  thd->mark_buffer = vpbuffer_realloc(thd->mark_buffer, &(thd->mark_buffer_len));
  if (pthread_mutex_init(&(thd->lock), NULL) != 0) {
    fprintf(stderr, "Unable to initialize thread mutex\n");
    exit(1);
  }
}

void gc_thread_data_free(gc_thread_data *thd)
{
  if (thd) {
    if (pthread_mutex_destroy(&thd->lock) != 0) {
      // TODO: can only destroy the lock if it is unlocked. need to make sure we
      // can guarantee that is the case prior to making this call
      // On the other hand, can we just use sleep and a loop to retry??
      fprintf(stderr, "Thread mutex is locked, unable to free\n");
      exit(1);
    }
    if (thd->jmp_start) free(thd->jmp_start);
    if (thd->gc_args) free(thd->gc_args);
    if (thd->moveBuf) free(thd->moveBuf);
    if (thd->mark_buffer) free(thd->mark_buffer);
    if (thd->stack_traces) free(thd->stack_traces);
    free(thd);
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
