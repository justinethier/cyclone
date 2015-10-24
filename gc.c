/* A basic mark-sweep GC
   As of now, the GC code is based off the implementation from chibi scheme

 Goals of this project:
 - write algorithms
 - add test cases
 - integrate with types
 - integrate with cyclone
 - extend to tri-color marking an on-the-fly collection
 - etc...
 */

#include "cyclone/types.h"

gc_heap *gc_heap_create(size_t size, size_t max_size, size_t chunk_size)
{
  gc_free_list *free, *next;
  gc_heap *h;
  // TODO: mmap?
  h = malloc(gc_heap_pad_size(size));
  if (!h) return NULL;
  h->size = size;
  h->chunk_size = chunk_size;
  h->max_size = max_size;
//printf("DEBUG h->data addr: %p\n", &(h->data));
  h->data = (char *) gc_heap_align(sizeof(h->data) + (uint)&(h->data)); 
//printf("DEBUG h->data addr: %p\n", h->data);
  h->next = NULL;
  free = h->free_list = (gc_free_list *)h->data;
  next = (gc_free_list *)(((char *) free) + gc_heap_align(gc_free_chunk_size));
  free->size = 0; // First one is just a dummy record
  free->next = next;
  next->size = size - gc_heap_align(gc_free_chunk_size);
  next->next = NULL;
#if GC_DEBUG_PRINTFS
  fprintf(stderr, ("heap: %p-%p data: %p-%p size: %d\n"),
          h, ((char*)h)+gc_heap_pad_size(size), h->data, h->data + size, size);
  fprintf(stderr, ("first: %p end: %p\n"),
          (object)gc_heap_first_block(h), (object)gc_heap_end(h));
  fprintf(stderr, ("free1: %p-%p free2: %p-%p\n"),
          free, ((char*)free)+free->size, next, ((char*)next)+next->size);
#endif
  return h;
}

int gc_grow_heap(gc_heap *h, size_t size, size_t chunk_size)
{
  size_t cur_size, new_size;
  gc_heap *h_last = gc_heap_last(h);
  cur_size = h_last->size;
  // JAE - For now, just add a new page
  new_size = cur_size; //gc_heap_align(((cur_size > size) ? cur_size : size) * 2);
  h_last->next = gc_heap_create(new_size, h_last->max_size, chunk_size);
  return (h_last->next != NULL);
}

void *gc_try_alloc(gc_heap *h, size_t size) 
{
  gc_free_list *f1, *f2, *f3;
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
        return f2;
      }
    }
  }
  return NULL; 
}

void *gc_alloc(gc_heap *h, size_t size, int *heap_grown) 
{
  void *result = NULL;
  size_t max_freed = 0, sum_freed = 0, total_size;
  // TODO: check return value, if null (could not alloc) then 
  // run a collection and check how much free space there is. if less
  // the allowed ratio, try growing heap.
  // then try realloc. if cannot alloc now, then throw out of memory error
  size = gc_heap_align(size);
  result = gc_try_alloc(h, size);
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
    result = gc_try_alloc(h, size);
    if (!result) {
      fprintf(stderr, "out of memory error allocating %d bytes\n", size);
      exit(1); // TODO: throw error???
    }
  }
#if GC_DEBUG_PRINTFS
  fprintf(stdout, "alloc %p size = %d\n", result, size);
#endif
  return result;
}

size_t gc_allocated_bytes(object obj)
{
  tag_type t;
  if (is_value_type(obj))
    return gc_heap_align(1);
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
  
//#if GC_DEBUG_PRINTFS
  fprintf(stderr, "gc_allocated_bytes: unexpected object %p of type %ld\n", obj, t);
  exit(1);
//#endif
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
  while(h) {
    total_size += h->size;
    h = h->next;
  }
  return total_size;
}

void gc_mark(gc_heap *h, object obj)
{
  if (nullp(obj) || is_value_type(obj) || mark(obj))
    return;

#if GC_DEBUG_PRINTFS
//  fprintf(stdout, "gc_mark %p\n", obj);
#endif
  ((list)obj)->hdr.mark = 1;
 // TODO: mark heap saves (??) 
 // could this be a write barrier?
 
 // Mark objects this one references
  if (type_of(obj) == cons_tag) {
    gc_mark(h, car(obj)); 
    gc_mark(h, cdr(obj)); 
  } else if (type_of(obj) == closure1_tag) {
    gc_mark(h, ((closure1) obj)->elt1); 
  } else if (type_of(obj) == closure2_tag) {
    gc_mark(h, ((closure2) obj)->elt1); 
    gc_mark(h, ((closure2) obj)->elt2); 
  } else if (type_of(obj) == closure3_tag) {
    gc_mark(h, ((closure3) obj)->elt1); 
    gc_mark(h, ((closure3) obj)->elt2); 
    gc_mark(h, ((closure3) obj)->elt3); 
  } else if (type_of(obj) == closure4_tag) {
    gc_mark(h, ((closure4) obj)->elt1); 
    gc_mark(h, ((closure4) obj)->elt2); 
    gc_mark(h, ((closure4) obj)->elt3); 
    gc_mark(h, ((closure4) obj)->elt4); 
  } else if (type_of(obj) == closureN_tag) {
    int i, n = ((closureN) obj)->num_elt;
    for (i = 0; i < n; i++) {
      gc_mark(h, ((closureN) obj)->elts[i]);
    }
  } else if (type_of(obj) == vector_tag) {
    int i, n = ((vector) obj)->num_elt;
    for (i = 0; i < n; i++) {
      gc_mark(h, ((vector) obj)->elts[i]);
    }
  }
}

size_t gc_sweep(gc_heap *h, size_t *sum_freed_ptr)
{
  size_t freed, max_freed=0, sum_freed=0, size;
  object p, end;
  gc_free_list *q, *r, *s;
  for (; h; h = h->next) { // All heaps
#if GC_DEBUG_CONCISE_PRINTFS
    fprintf(stdout, "sweep heap %p, size = %d\n", h, h->size);
#endif
    p = gc_heap_first_block(h);
    q = h->free_list;
    end = gc_heap_end(h);
    while (p < end) {
      // find preceding/succeeding free list pointers for p
      for (r = q->next; r && ((char *)r < (char *)p); q=r, r=r->next);

      if ((char *)r == (char *)p) { // this is a free block, skip it
        p = (object) (((char *)p) + r->size);
#if GC_DEBUG_PRINTFS
        fprintf(stdout, "skip free block %p size = %d\n", p, r->size);
#endif
        continue;
      }
      size = gc_heap_align(gc_allocated_bytes(p));
//fprintf(stdout, "check object %p, size = %d\n", p, size);
      
#if GC_DEBUG_CONCISE_PRINTFS
      // DEBUG
      if (!is_object_type(p))
        fprintf(stderr, "sweep: invalid object at %p", p);
      if ((char *)q + q->size > (char *)p)
        fprintf(stderr, "bad size at %p < %p + %u", p, q, q->size);
      if (r && ((char *)p) + size > (char *)r)
        fprintf(stderr, "sweep: bad size at %p + %d > %p", p, size, r);
      // END DEBUG
#endif

      if (!mark(p)) {
#if GC_DEBUG_PRINTFS
        fprintf(stdout, "sweep: object is not marked %p\n", p);
#endif
        // free p
        sum_freed += size;
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
#if GC_DEBUG_PRINTFS
//        fprintf(stdout, "sweep: object is marked %p\n", p);
#endif
        //if (mark(p) != 1) {
        //  printf("unexpected mark value %d\n", mark(p));
        //  exit(1);
        //}

        ((list)p)->hdr.mark = 0;
        p = (object)(((char *)p) + size);
      }
    }
  }
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
#if GC_DEBUG_CONCISE_PRINTFS
  printf("grew moveBuffer, len = %d\n", d->moveBufLen);
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

// void gc_init()
// {
// }
// END heap definitions

/* tri-color GC stuff, we will care about this later...
int colorWhite = 0;
int colorGray  = 1;
int colorBlack = 2;
int colorBlue  = 3;

typedef enum {STATUS_ASYNC, STATUS_SYNC1, STATUS_SYNC2} status_type;

// DLG globals
static void *swept;
static int dirty;
static void *scanned;

// TODO: mutator actions
// TODO: collector
// TODO: extentions
// TODO: proofs, etc
// TODO: revist design using content from kolodner
*/

// int main(int argc, char **argv) {
//   int i;
//   size_t freed = 0, max_freed = 0;
//   gc_heap *h = gc_heap_create(8 * 1024 * 1024, 0, 0);
//   void *obj1 = gc_alloc(h, sizeof(cons_type));
//   void *obj2 = gc_alloc(h, sizeof(cons_type));
//   void *objI = gc_alloc(h, sizeof(integer_type));
// 
//   for (i = 0; i < 1000000; i++) {
//     gc_alloc(h, sizeof(integer_type));
//     gc_alloc(h, sizeof(integer_type));
//   }
// 
//   // Build up an object graph to test collection...
//   ((integer_type *)objI)->hdr.mark = 0;
//   ((integer_type *)objI)->tag = integer_tag;
//   ((integer_type *)objI)->value = 42;
// 
//   ((list)obj2)->hdr.mark = 0;
//   ((list)obj2)->tag = cons_tag;
//   ((list)obj2)->cons_car = objI;
//   ((list)obj2)->cons_cdr = NULL;
// 
//   ((list)obj1)->hdr.mark = 0;
//   ((list)obj1)->tag = cons_tag;
//   ((list)obj1)->cons_car = obj2;
//   ((list)obj1)->cons_cdr = NULL;
// 
//   printf("(heap: %p size: %d)", h, (unsigned int)gc_heap_total_size(h));
//   gc_mark(h, obj1);
//   max_freed = gc_sweep(h, &freed);
//   printf("done, freed = %d, max_freed = %d\n", freed, max_freed);
//   for (i = 0; i < 10; i++) {
//     gc_alloc(h, sizeof(integer_type));
//     gc_alloc(h, sizeof(integer_type));
//   }
//   printf("(heap: %p size: %d)", h, (unsigned int)gc_heap_total_size(h));
//   gc_mark(h, obj1);
//   max_freed = gc_sweep(h, &freed);
//   printf("done, freed = %d, max_freed = %d\n", freed, max_freed);
// 
//   return 0;
// }
