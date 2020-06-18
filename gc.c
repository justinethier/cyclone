/** 
 * Cyclone Scheme
 * https://github.com/justinethier/cyclone
 *
 * Copyright (c) 2015-2016, Justin Ethier
 * All rights reserved.
 *
 * Heap garbage collector used by the Cyclone runtime for major collections.
 *
 * Concurrent Mark-Sweep GC algorithm is based on the one from 
 * "Implementing an on-the-fly garbage collector for Java", by Domani et al.
 *
 * Data structures for the heap implementation are based on code from Chibi Scheme.
 *
 * Note there is also a minor GC (in runtime.c) that collects objects allocated 
 * on the stack, based on "Cheney on the MTA".
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

// 64-bit is 3, 32-bit is 2
#define GC_BLOCK_BITS 5
#define GC_BLOCK_SIZE (1 << GC_BLOCK_BITS)

/* HEAP definitions, based off heap from Chibi scheme */
#define gc_heap_first_block(h) ((object)(h->data + gc_heap_align(gc_free_chunk_size)))
#define gc_heap_last_block(h) ((object)((char*)h->data + h->size - gc_heap_align(gc_free_chunk_size)))
#define gc_heap_end(h) ((object)((char*)h->data + h->size))
#define gc_heap_pad_size(s) (sizeof(struct gc_heap_t) + (s) + gc_heap_align(1))
#define gc_free_chunk_size (sizeof(gc_free_list))

#define gc_align(n, bits) (((n)+(1<<(bits))-1)&(((uintptr_t)-1)-((1<<(bits))-1)))

//#define gc_word_align(n) gc_align((n), 2)
#define gc_heap_align(n) gc_align(n, GC_BLOCK_BITS)

////////////////////
// Global variables

// Note: will need to use atomics and/or locking to access any
// variables shared between threads
static unsigned char gc_color_mark = 5;   // Black, is swapped during GC
static unsigned char gc_color_clear = 3;  // White, is swapped during GC
static unsigned char gc_color_purple = 1;  // There are many "shades" of purple, this is the most recent one
// unfortunately this had to be split up; const colors are located in types.h

static int gc_status_col = STATUS_SYNC1;
static int gc_stage = STAGE_RESTING;

// Does not need sync, only used by collector thread
static void **mark_stack = NULL;
static int mark_stack_len = 0;
static int mark_stack_i = 0;

// Data for the "main" thread which is guaranteed to always be there.
// Per SRFI 18:
//    All threads are terminated when the primordial
//    thread terminates (normally or not).
static gc_thread_data *primordial_thread = NULL;

/** Data new mutator threads that are not running yet */
static ck_array_t new_mutators;
/** Data for each individual mutator thread */
static ck_array_t Cyc_mutators;
static ck_array_t old_mutators;
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

/** Mark buffers 
 *
 * For these, we need a buffer than can grow as needed but that can also be
 * used concurrently by both a mutator thread and a collector thread.
 */

static mark_buffer *mark_buffer_init(unsigned initial_size)
{
  mark_buffer *mb = malloc(sizeof(mark_buffer));
  mb->buf = malloc(sizeof(void *) * initial_size);
  mb->buf_len = initial_size;
  mb->next = NULL;
  return mb;
}

static void *mark_buffer_get(mark_buffer *mb, unsigned i) // TODO: macro?
{
  while (i >= mb->buf_len) {
    // Not on this page, try the next one
    i -= mb->buf_len;
    mb = mb->next;
    if (mb == NULL) { // Safety check
      // For now this is a fatal error, could return NULL instead
      fprintf(stderr, "mark_buffer_get ran out of mark buffers, exiting\n");
      exit(1);
    }
  }
  return mb->buf[i];
}

static void mark_buffer_set(mark_buffer *mb, unsigned i, void *obj)
{
  // Find index i
  while (i >= mb->buf_len) {
    // Not on this page, try the next one
    i -= mb->buf_len;
    if (mb->next == NULL) { 
      // If it does not exist, allocate a new buffer
      mb->next = mark_buffer_init(mb->buf_len * 2);
    }
    mb = mb->next;
  }
  mb->buf[i] = obj;
}

static void mark_buffer_free(mark_buffer *mb)
{
  mark_buffer *next;
  while (mb) {
    next = mb->next;
    free(mb->buf);
    free(mb);
    mb = next;
  }
}

// END mark buffer

#if GC_DEBUG_TRACE
const int NUM_ALLOC_SIZES = 10;
static double allocated_size_counts[10] = {
  0,0,0,0,0,
  0,0,0,0,0};
static double allocated_obj_counts[25] = {
  0,0,0,0,0,
  0,0,0,0,0,
  0,0,0,0,0,
  0,0,0,0,0,
  0,0,0,0,0};
// TODO: allocated object sizes (EG: 32, 64, etc).
static double allocated_heap_counts[4] = {0, 0, 0, 0};

void print_allocated_obj_counts()
{
  int i;
  fprintf(stderr, "Allocated sizes:\n");
  fprintf(stderr, "Size, Allocations\n");
  for (i = 0; i < NUM_ALLOC_SIZES; i++){
    fprintf(stderr, "%d, %lf\n", 32 + (i*32), allocated_size_counts[i]);
  }
  fprintf(stderr, "Allocated objects:\n");
  fprintf(stderr, "Tag, Allocations\n");
  for (i = 0; i < 25; i++){
    fprintf(stderr, "%d, %lf\n", i, allocated_obj_counts[i]);
  }
  fprintf(stderr, "Allocated heaps:\n");
  fprintf(stderr, "Heap, Allocations\n");
  for (i = 0; i < 4; i++){
    fprintf(stderr, "%d, %lf\n", i, allocated_heap_counts[i]);
  }
}

void gc_log(FILE *stream, const char *format, ...)
{
  va_list vargs;
  time_t rawtime;
  struct tm * timeinfo;
  time ( &rawtime );
  timeinfo = localtime ( &rawtime );

  fprintf(stream, "%.2d:%.2d:%.2d - ", 
    timeinfo->tm_hour, timeinfo->tm_min, timeinfo->tm_sec);
  va_start(vargs, format);
  vfprintf(stream, format, vargs);
  fprintf(stream, "\n");
  va_end(vargs);
}
#endif

/////////////
// Functions

/** 
 * @brief Perform one-time initialization before mutators can be executed 
 */
void gc_initialize(void)
{
  if (ck_array_init(&Cyc_mutators, CK_ARRAY_MODE_SPMC, &my_allocator, 10) == 0) {
    fprintf(stderr, "Unable to initialize mutator array\n");
    exit(1);
  }

  if (ck_array_init(&new_mutators, CK_ARRAY_MODE_SPMC, &my_allocator, 10) == 0) {
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
  if (pthread_mutex_init(&(mutators_lock), NULL) != 0) {
    fprintf(stderr, "Unable to initialize mutators_lock mutex\n");
    exit(1);
  }
}

/**
 * @brief  Add data for a new mutator that is not yet scheduled to run.
 *         This is done so there is a record in the system even if the 
 *         thread is not running, to prevent race conditions for any
 *         functions (EG: thread-join!) that need to access the thread.
 * @param  thd  Thread data for the mutator
 */
void gc_add_new_unrunning_mutator(gc_thread_data * thd)
{
  pthread_mutex_lock(&mutators_lock);
  if (ck_array_put_unique(&new_mutators, (void *)thd) < 0) {
    fprintf(stderr, "Unable to allocate memory for a new thread, exiting\n");
    exit(1);
  }
  ck_array_commit(&new_mutators);
  pthread_mutex_unlock(&mutators_lock);
}

/**
 * @brief  Add data for a new mutator that is starting to run.
 * @param  thd  Thread data for the mutator
 */
void gc_add_mutator(gc_thread_data * thd)
{
  pthread_mutex_lock(&mutators_lock);
  if (ck_array_put_unique(&Cyc_mutators, (void *)thd) < 0) {
    fprintf(stderr, "Unable to allocate memory for a new thread, exiting\n");
    exit(1);
  }
  ck_array_commit(&Cyc_mutators);
  pthread_mutex_unlock(&mutators_lock);

  // Main thread is always the first one added
  if (primordial_thread == NULL) {
      primordial_thread = thd;
  } else {
    // At this point the mutator is running, so remove it from the new list
    pthread_mutex_lock(&mutators_lock);
    ck_array_remove(&new_mutators, (void *)thd);
    ck_array_commit(&new_mutators);
    pthread_mutex_unlock(&mutators_lock);
  }
}

/**
 * @brief Remove selected mutator from the mutator list.
 * This is done for terminated threads. Note data is queued to be
 * freed, to prevent accidentally freeing it while the collector
 * thread is potentially accessing it.
 * @param   thd   Thread data for the mutator
 */
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
    fprintf(stderr, "Unable to add thread data to GC list, exiting\n");
    exit(1);
  }
  ck_array_commit(&old_mutators);
  pthread_mutex_unlock(&mutators_lock);
}

/**
 * @brief Determine if the given mutator is in the list of active threads.
 * @param thd Thread data object of the m
 * @return A true value if the mutator is active, 0 otherwise.
 */
int gc_is_mutator_active(gc_thread_data *thd)
{
  ck_array_iterator_t iterator;
  gc_thread_data *m;
  CK_ARRAY_FOREACH(&Cyc_mutators, &iterator, &m) {
    if (m == thd) {
      return 1;
    }
  }
  return 0;
}

/**
 * @brief Determine if the given mutator is in the list of new threads.
 * @param thd Thread data object of the m
 * @return A true value if the mutator is found, 0 otherwise.
 */
int gc_is_mutator_new(gc_thread_data *thd)
{
  ck_array_iterator_t iterator;
  gc_thread_data *m;
  CK_ARRAY_FOREACH(&new_mutators, &iterator, &m) {
    if (m == thd) {
      return 1;
    }
  }
  return 0;
}

/**
 * @brief Free thread data for all terminated mutators
 */
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

/**
 * @brief Return the amount of free space on the heap
 * @param gc_heap Root of the heap
 * @return Free space in bytes
 */
uint64_t gc_heap_free_size(gc_heap *h) {
  uint64_t free_size = 0;
  for (; h; h = h->next){
    if (h->is_unswept == 1) { // Assume all free prior to sweep
      free_size += h->size;
    } else {
      free_size += (h->free_size);
    }
  }
  return free_size;
}

/**
 * @brief Create a new heap page. 
 *        The caller must hold the necessary locks.
 * @param  heap_type  Define the size of objects that will be allocated on this heap 
 * @param  size       Requested size (unpadded) of the heap
 * @param  thd        Calling mutator's thread data object
 * @return Pointer to the newly allocated heap page, or NULL
 *         if the allocation failed.
 */
gc_heap *gc_heap_create(int heap_type, size_t size, gc_thread_data *thd)
{
  gc_free_list *free, *next;
  gc_heap *h;
  size_t padded_size;
  size = gc_heap_align(size);
  padded_size = gc_heap_pad_size(size);
  h = malloc(padded_size);
  if (!h)
    return NULL;
  h->type = heap_type;
  h->size = size;
  h->ttl = 10;
  h->next_free = h;
  h->last_alloc_size = 0;
  thd->cached_heap_total_sizes[heap_type] += size;
  thd->cached_heap_free_sizes[heap_type] += size;
  h->data = (char *)gc_heap_align(sizeof(h->data) + (uintptr_t) & (h->data));
  h->next = NULL;
  h->num_unswept_children = 0;
  free = h->free_list = (gc_free_list *) h->data;
  next = (gc_free_list *) (((char *)free) + gc_heap_align(gc_free_chunk_size));
  free->size = 0;               // First one is just a dummy record
  free->next = next;
  next->size = size - gc_heap_align(gc_free_chunk_size);
  next->next = NULL;
#if GC_DEBUG_TRACE
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
  if (heap_type <= LAST_FIXED_SIZE_HEAP_TYPE) {
    h->block_size = (heap_type + 1) * 32;
//
    h->remaining = size - (size % h->block_size);
    h->data_end = h->data + h->remaining;
    h->free_list = NULL; // No free lists with bump&pop
// This is for starting with a free list, but we want bump&pop instead
//    h->remaining = 0;
//    h->data_end = NULL;
//    gc_init_fixed_size_free_list(h);
  } else {
    h->block_size = 0;
    h->remaining = 0;
    h->data_end = NULL;
  }
  // Lazy sweeping
  h->free_size = size;
  h->is_full = 0;
  h->is_unswept = 0;
  return h;
}

/**
 * @brief   Initialize free lists within a single heap page.
 *          Assumes that there is no data currently on the heap page!
 * @param   h     Heap page to initialize
 */
void gc_init_fixed_size_free_list(gc_heap *h)
{
  // for this flavor, just layer a free list on top of unitialized memory
  gc_free_list *next;
  //int i = 0;
  size_t remaining = h->size - (h->size % h->block_size) - h->block_size; // Starting at first one so skip it
  next = h->free_list = (gc_free_list *)h->data;
  //printf("data start = %p\n", h->data);
  //printf("data end = %p\n", h->data + h->size);
  while (remaining >= h->block_size) {
    //printf("%d init remaining=%d next = %p\n", i++, remaining, next);
    next->next = (gc_free_list *)(((char *) next) + h->block_size);
    next = next->next;
    remaining -= h->block_size;
  }
  next->next = NULL;
  h->data_end = NULL; // Indicate we are using free lists
} 

/**
 * @brief Diagnostic function to print all free lists on a fixed-size heap page
 * @param h Heap page to output
 */
void gc_print_fixed_size_free_list(gc_heap *h)
{
  gc_free_list *f = h->free_list;
  fprintf(stderr, "printing free list:\n");
  while(f) {
    fprintf(stderr, "%p\n", f);
    f = f->next;
  }
  fprintf(stderr, "done\n");
}

/**
 * @brief Essentially this is half of the sweep code, for sweeping bump&pop
 * @param h Heap page to convert
 */
static size_t gc_convert_heap_page_to_free_list(gc_heap *h, gc_thread_data *thd) 
{
  size_t freed = 0;
  object p;
  gc_free_list *next;
  int remaining = h->size - (h->size % h->block_size);
  if (h->data_end == NULL) return 0; // Already converted

  next = h->free_list = NULL;
  while (remaining > h->remaining) {
    p = h->data_end - remaining;
    //int tag = type_of(p);
    int color = mark(p);
//    printf("found object %d color %d at %p with remaining=%lu\n", tag, color, p, remaining);
    // free space, add it to the free list
    if (color != thd->gc_alloc_color &&
        color != thd->gc_trace_color) { //gc_color_clear) 
      // Run any finalizers
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
      } else if (type_of(p) == bignum_tag) {
        // TODO: this is no good if we abandon bignum's on the stack
        // in that case the finalizer is never called
#if GC_DEBUG_VERBOSE
        fprintf(stderr, "mp_clear from sweep\n");
#endif
        mp_clear(&(((bignum_type *)p)->bn));
      } else if (type_of(p) == c_opaque_tag && opaque_collect_ptr(p)) {
#if GC_DEBUG_VERBOSE
        fprintf(stderr, "free opaque pointer %p from sweep\n", opaque_ptr(p));
#endif
        free( opaque_ptr(p) );
      }

      // Free block
      freed += h->block_size;
      if (next == NULL) {
        next = h->free_list = p;
      }
      else {
        next->next = p;
        next = next->next;
      }
      h->free_size += h->block_size;
    }
    remaining -= h->block_size;
  }

  // Convert any empty space at the end
  while (remaining) {
    p = h->data_end - remaining;
//    printf("no object at %p fill with free list\n", p);
    if (next == NULL) {
      next = h->free_list = p;
    }
    else {
      next->next = p; //(gc_free_list *)(((char *) next) + h->block_size);
      next = next->next;
    }
    remaining -= h->block_size;
  }

  if (next) {
    next->next = NULL;
  }
  // Let GC know this heap is not bump&pop
  h->remaining = 0;
  h->data_end = NULL;
  return freed;
}

/**
 * @brief Sweep portion of the GC algorithm
 * @param h           Heap to sweep
 * @param thd           Thread data object for the mutator using this heap
 * @return Return the size of the largest object freed, in bytes
 *
 * This portion of the major GC algorithm is responsible for returning unused
 * memory slots to the heap. It is only called by the collector thread after
 * the heap has been traced to identify live objects.
 */
static gc_heap *gc_sweep_fixed_size(gc_heap * h, gc_thread_data *thd)
{
  short heap_is_empty;
  object p, end;
  gc_free_list *q, *r, *s;
#if GC_DEBUG_SHOW_SWEEP_DIAG
  gc_heap *orig_heap_ptr = h;
#endif
  gc_heap *rv = h;

  h->next_free = h;
  h->is_unswept = 0;

#if GC_DEBUG_SHOW_SWEEP_DIAG
  fprintf(stderr, "\nBefore sweep -------------------------\n");
  fprintf(stderr, "Heap %d diagnostics:\n", h->type);
  gc_print_stats(orig_heap_ptr);
#endif

  if (h->data_end != NULL) {
    // Special case, bump&pop heap
    gc_convert_heap_page_to_free_list(h, thd);
    heap_is_empty = 0; // For now, don't try to free bump&pop
  } else {
    //gc_free_list *next;
    size_t remaining = h->size - (h->size % h->block_size); // - h->block_size; // Remove first one??
    char *data_end = h->data + remaining;
    heap_is_empty = 1; // Base case is an empty heap
    end = (object)data_end;
    p = h->data;
    q = h->free_list;
    while (p < end) {
      // find preceding/succeeding free list pointers for p
      for (r = (q?q->next:NULL); r && ((char *)r < (char *)p); q = r, r = r->next) ;
      if ((char *)q == (char *)p || (char *)r == (char *)p) {     // this is a free block, skip it
        //printf("Sweep skip free block %p remaining=%lu\n", p, remaining);
        p = (object) (((char *)p) + h->block_size);
        continue;
      }
#if GC_SAFETY_CHECKS
      if (!is_object_type(p)) {
        fprintf(stderr, "sweep: invalid object at %p", p);
        exit(1);
      }
      if (type_of(p) > 21) {
        fprintf(stderr, "sweep: invalid object tag %d at %p", type_of(p), p);
        exit(1);
      }
#endif
      if (mark(p) != thd->gc_alloc_color && 
          mark(p) != thd->gc_trace_color) { //gc_color_clear) 
#if GC_DEBUG_VERBOSE
        fprintf(stderr, "sweep is freeing unmarked obj: %p with tag %d\n", p,
                type_of(p));
#endif
        // Run finalizers
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
        } else if (type_of(p) == bignum_tag) {
          // TODO: this is no good if we abandon bignum's on the stack
          // in that case the finalizer is never called
#if GC_DEBUG_VERBOSE
          fprintf(stderr, "mp_clear from sweep\n");
#endif
          mp_clear(&(((bignum_type *)p)->bn));
        }

        // free p
        //heap_freed += h->block_size;
        if (h->free_list == NULL) {
          // No free list, start one at p
          q = h->free_list = p;
          h->free_list->next = NULL;
          //printf("sweep reclaimed remaining=%d, %p, assign h->free_list\n", remaining, p);
        } else if ((char *)p < (char *)h->free_list) {
          // p is before the free list, prepend it as the start
          // note if this is the case, either there is no free_list (see above case) or 
          // the free list is after p, which is handled now. these are the only situations
          // where there is no q
          s = (gc_free_list *)p;
          s->next = h->free_list;
          q = h->free_list = p;
          //printf("sweep reclaimed remaining=%d, %p, assign h->free_list which was %p\n", remaining, p, h->free_list);
        } else {
          s = (gc_free_list *)p;
          s->next = r;
          q->next = s;
          //printf("sweep reclaimed remaining=%d, %p, q=%p, r=%p\n", remaining, p, q, r);
        }
        h->free_size += h->block_size;
      } else {
        //printf("sweep block is still used remaining=%d p = %p\n", remaining, p);
        heap_is_empty = 0;
      }
      //next->next = (gc_free_list *)(((char *) next) + h->block_size);
      //next = next->next;
      p = (object) (((char *)p) + h->block_size);
    }
  }
  // Free the heap page if possible.
  if (heap_is_empty) {
    if (h->type == HEAP_HUGE || (h->ttl--) <= 0) {
      rv = NULL; // Let caller know heap needs to be freed
    } else {
      // Convert back to bump&pop
      h->remaining = h->size - (h->size % h->block_size);
      h->data_end = h->data + h->remaining;
      h->free_list = NULL; // No free lists with bump&pop
    }
  } else {
    //(thd->heap->heap[h->type])->num_unswept_children--;
  }

#if GC_DEBUG_SHOW_SWEEP_DIAG
  fprintf(stderr, "\nAfter sweep -------------------------\n");
  fprintf(stderr, "Heap %d diagnostics:\n", h->type);
  gc_print_stats(orig_heap_ptr);
#endif

  return rv;
}

/**
 * @brief   Free a page of the heap
 * @param   page        Page to free
 * @param   prev_page   Previous page in the heap
 * @return  Previous page if successful, NULL otherwise
 */
gc_heap *gc_heap_free(gc_heap *page, gc_heap *prev_page)
{
  // At least for now, do not free first page
  if (prev_page == NULL || page == NULL) {
    return NULL;
  }
#if GC_DEBUG_TRACE
  fprintf(stderr, "DEBUG freeing heap type %d page at addr: %p\n", page->type, page);
#endif

  prev_page->next = page->next;
  free(page);
  return prev_page;
}

/**
 * @brief Determine if a heap page is empty.
 * @param h Heap to inspect. The caller should acquire any necessary locks.
 * @return A truthy value if the heap is empty, 0 otherwise.
 */
static int gc_is_heap_empty(gc_heap *h) 
{
  gc_free_list *f;
  if (!h) return 0;

  if (h->data_end) { // Fixed-size bump&pop
    return (h->remaining == (h->size - (h->size % h->block_size)));
  }

  if (!h->free_list) return 0;

  f = h->free_list;
  if (f->size != 0 || !f->next) return 0;

  f = f->next;
  return (f->size + gc_heap_align(gc_free_chunk_size)) == h->size;
}

/**
 * @brief Print heap usage information. Before calling this function the 
 *        current thread must have the heap lock
 * @param h Heap to analyze.
 */
void gc_print_stats(gc_heap * h)
{
  gc_free_list *f;
  unsigned int free, free_chunks, free_min, free_max;
  int heap_is_empty;
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
    if (free == 0){ // No free chunks
      free_min = 0;
    }
    heap_is_empty = gc_is_heap_empty(h);
    fprintf(stderr,
            "Heap type=%d, page size=%u, is empty=%d, used=%u, free=%u, free chunks=%u, min=%u, max=%u\n",
            h->type, h->size, heap_is_empty, h->size - free, free, free_chunks, free_min, free_max);
  }
}

/**
 * @brief Copy given object into given heap object
 * @param dest  Pointer to destination heap memory slot
 * @param obj   Object to copy
 * @param thd   Thread data object for the applicable mutator
 * @return The appropriate pointer to use for `obj`
 *
 * NOTE: There is no additional type checking because this function is
 * called from `gc_move` which already does that.
 */
char *gc_copy_obj(object dest, char *obj, gc_thread_data * thd)
{
  #if GC_DEBUG_TRACE
  allocated_obj_counts[type_of(obj)]++;
  #endif

  switch (type_of(obj)) {
  case closureN_tag:{
      closureN_type *hp = dest;
      mark(hp) = thd->gc_alloc_color;
      type_of(hp) = closureN_tag;
      hp->fn = ((closureN) obj)->fn;
      hp->num_args = ((closureN) obj)->num_args;
      hp->num_elements = ((closureN) obj)->num_elements;
      hp->elements = (object *) (((char *)hp) + sizeof(closureN_type));
      memcpy(hp->elements, ((closureN)obj)->elements, sizeof(object *) * hp->num_elements);
      return (char *)hp;
    }
  case pair_tag:{
      list hp = dest;
      hp->hdr.mark = thd->gc_alloc_color;
      hp->hdr.immutable = immutable(obj);
      type_of(hp) = pair_tag;
      car(hp) = car(obj);
      cdr(hp) = cdr(obj);
      return (char *)hp;
    }
  case string_tag:{
      char *s;
      string_type *hp = dest;
      s = ((char *)hp) + sizeof(string_type);
      memcpy(s, string_str(obj), string_len(obj) + 1);
      mark(hp) = thd->gc_alloc_color;
      immutable(hp) = immutable(obj);
      type_of(hp) = string_tag;
      string_num_cp(hp) = string_num_cp(obj);
      string_len(hp) = string_len(obj);
      string_str(hp) = s;
      return (char *)hp;
    }
  case double_tag:{
      double_type *hp = dest;
      mark(hp) = thd->gc_alloc_color;
      type_of(hp) = double_tag;
      hp->value = ((double_type *) obj)->value;
      return (char *)hp;
    }
  case vector_tag:{
      vector_type *hp = dest;
      mark(hp) = thd->gc_alloc_color;
      immutable(hp) = immutable(obj);
      type_of(hp) = vector_tag;
      hp->num_elements = ((vector) obj)->num_elements;
      hp->elements = (object *) (((char *)hp) + sizeof(vector_type));
      memcpy(hp->elements, ((vector)obj)->elements, sizeof(object *) * hp->num_elements);
      return (char *)hp;
    }
  case bytevector_tag:{
      bytevector_type *hp = dest;
      mark(hp) = thd->gc_alloc_color;
      immutable(hp) = immutable(obj);
      type_of(hp) = bytevector_tag;
      hp->len = ((bytevector) obj)->len;
      hp->data = (((char *)hp) + sizeof(bytevector_type));
      memcpy(hp->data, ((bytevector) obj)->data, hp->len);
      return (char *)hp;
    }
  case port_tag:{
      port_type *hp = dest;
      mark(hp) = thd->gc_alloc_color;
      type_of(hp) = port_tag;
      hp->fp = ((port_type *) obj)->fp;
      hp->mode = ((port_type *) obj)->mode;
      hp->flags = ((port_type *) obj)->flags;
      hp->line_num = ((port_type *) obj)->line_num;
      hp->col_num = ((port_type *) obj)->col_num;
      hp->buf_idx = ((port_type *) obj)->buf_idx;
      hp->tok_start = ((port_type *) obj)->tok_start;
      hp->tok_end = ((port_type *) obj)->tok_end;
      hp->tok_buf = ((port_type *) obj)->tok_buf;
      hp->tok_buf_len = ((port_type *) obj)->tok_buf_len;
      hp->mem_buf = ((port_type *)obj)->mem_buf;
      hp->mem_buf_len = ((port_type *)obj)->mem_buf_len;
      hp->str_bv_in_mem_buf = ((port_type *)obj)->str_bv_in_mem_buf;
      hp->str_bv_in_mem_buf_len = ((port_type *)obj)->str_bv_in_mem_buf_len;
      hp->read_len = ((port_type *)obj)->read_len;
      return (char *)hp;
    }
  case bignum_tag:{
      bignum_type *hp = dest;
      mark(hp) = thd->gc_alloc_color;
      type_of(hp) = bignum_tag;
      ((bignum_type *)hp)->bn.used = ((bignum_type *)obj)->bn.used;
      ((bignum_type *)hp)->bn.alloc = ((bignum_type *)obj)->bn.alloc;
      ((bignum_type *)hp)->bn.sign = ((bignum_type *)obj)->bn.sign;
      ((bignum_type *)hp)->bn.dp = ((bignum_type *)obj)->bn.dp;
      return (char *)hp;
    }
  case cvar_tag:{
      cvar_type *hp = dest;
      mark(hp) = thd->gc_alloc_color;
      type_of(hp) = cvar_tag;
      hp->pvar = ((cvar_type *) obj)->pvar;
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
  case atomic_tag:{
      atomic_type *hp = dest;
      mark(hp) = thd->gc_alloc_color;
      type_of(hp) = atomic_tag;
      hp->obj = ((atomic_type *)obj)->obj; // TODO: should access via CK atomic operations, though this may not be needed at all since we alloc directly on heap
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
  case closure1_tag:{
      closure1_type *hp = dest;
      mark(hp) = thd->gc_alloc_color;
      type_of(hp) = closure1_tag;
      hp->fn = ((closure1) obj)->fn;
      hp->num_args = ((closure1) obj)->num_args;
      hp->element = ((closure1) obj)->element;
      return (char *)hp;
    }
  case c_opaque_tag:{
      c_opaque_type *hp = dest;
      mark(hp) = thd->gc_alloc_color;
      immutable(hp) = immutable(obj);
      type_of(hp) = c_opaque_tag;
      hp->collect_ptr = ((c_opaque_type *) obj)->collect_ptr;
      hp->ptr = ((c_opaque_type *) obj)->ptr;
      return (char *)hp;
    }
  case forward_tag:
    return (char *)forward(obj);
  case eof_tag:
  case void_tag:
  case primitive_tag:
  case boolean_tag:
  case symbol_tag:
  case closure0_tag:
    break;
  case integer_tag:{
      integer_type *hp = dest;
      mark(hp) = thd->gc_alloc_color;
      type_of(hp) = integer_tag;
      hp->value = ((integer_type *) obj)->value;
      return (char *)hp;
    }
  case complex_num_tag:{
      complex_num_type *hp = dest;
      mark(hp) = thd->gc_alloc_color;
      type_of(hp) = complex_num_tag;
      hp->value = ((complex_num_type *) obj)->value;
      return (char *)hp;
    }
  default:
    fprintf(stderr, "gc_copy_obj: bad tag obj=%p obj.tag=%d\n", (object) obj,
            type_of(obj));
    exit(1);
  }
  return (char *)obj;
}

/**
 * @brief Grow a heap by allocating a new page.
 * @param h          Heap to be expanded
 * @param size       Not applicable, can set to 0
 * @param thd        Thread data for the mutator using this heap
 * @return A true value if the heap was grown, or 0 otherwise
 *
 * Heaps are increased in size by adding a newly-allocated page at the
 * end of the heap's linked list.
 *
 * Page size is determined by starting at the minimum page size and
 * increasing size using the Fibonnaci Sequence until reaching the
 * max size.
 */
gc_heap *gc_grow_heap(gc_heap * h, size_t size, gc_thread_data *thd)
{
  size_t new_size;
  gc_heap *h_last = h, *h_new;
  // Compute size of new heap page
  if (h->type == HEAP_HUGE) {
    new_size = gc_heap_align(size) + 128;
    while (h_last->next) {
      h_last = h_last->next;
    }
  } else {
    // Grow heap gradually using fibonnaci sequence.
    size_t prev_size = GROW_HEAP_BY_SIZE;
    new_size = 0;
    while (h_last->next) {
      if (new_size < HEAP_SIZE) {
        new_size = prev_size + h_last->size;
        prev_size = h_last->size;
        if (new_size > HEAP_SIZE) {
            new_size = HEAP_SIZE;
            break;
        }
      } else {
        new_size = HEAP_SIZE;
        break;
      }
      h_last = h_last->next;
    }
    if (new_size == 0) {
      new_size = prev_size + h_last->size;
      if (new_size > HEAP_SIZE) {
        new_size = HEAP_SIZE;
      }
    }
    // Fast-track heap page size if allocating a large block
    if (new_size < size && size < HEAP_SIZE) {
      new_size = HEAP_SIZE;
    }
#if GC_DEBUG_TRACE
    fprintf(stderr, "Growing heap %d new page size = %zu\n", h->type,
            new_size);
#endif
  }
  h_last = gc_heap_last(h_last); // Ensure we don't unlink any heaps
  // Done with computing new page size
  h_new = gc_heap_create(h->type, new_size, thd);
  h_last->next = h_new;
#if GC_DEBUG_TRACE
  fprintf(stderr, "DEBUG - grew heap\n");
#endif
  return h_last;
}

/**
 * @brief Attempt to allocate a new heap slot for the given object
 * @param h          Heap to allocate from
 * @param size       Size of the requested object, in bytes
 * @param obj        Object containing data that will be copied to the heap
 * @param thd        Thread data for the mutator using this heap
 * @return Pointer to the newly-allocated object, or `NULL` if allocation failed
 *
 * This function will fail if there is no space on the heap for the 
 * requested object.
 */
void *gc_try_alloc(gc_heap * h, size_t size, char *obj,
                   gc_thread_data * thd)
{
  gc_free_list *f1, *f2, *f3;

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

      if (h->type != HEAP_HUGE) {
        // Copy object into heap now to avoid any uninitialized memory issues
        #if GC_DEBUG_TRACE
        if (size < (32 * NUM_ALLOC_SIZES)) {
          allocated_size_counts[(size / 32) - 1]++;
        }
        #endif
        gc_copy_obj(f2, obj, thd);
        // Done after sweep now instead of with each allocation
        h->free_size -= size;
      } else {
        thd->heap_num_huge_allocations++;
      }
      return f2;
    }
  }
  return NULL;
}

/**
 * @brief Return number of unswept heaps
 * @param h   Heap we are starting from (assume first in the chain)
 * @return    Count of heaps that have not been swept yet.
 */
int gc_num_unswept_heaps(gc_heap *h)
{
  int count = 0;
  while (h) {
    if (h->is_unswept == 1 /*|| 
        gc_is_heap_empty(h)*/) {
      count++;
    }
    h = h->next;
  }
  return count;
}

void gc_start_major_collection(gc_thread_data *thd){
  if (ck_pr_load_int(&gc_stage) == STAGE_RESTING) {
#if GC_DEBUG_TRACE
    gc_log(stderr, "gc_start_major_collection - initiating collector");
#endif
    ck_pr_cas_int(&gc_stage, STAGE_RESTING, STAGE_CLEAR_OR_MARKING);
  }
}

void *gc_try_alloc_slow(gc_heap *h_passed, gc_heap *h, size_t size, char *obj, gc_thread_data *thd)
{
#ifdef CYC_HIGH_RES_TIMERS
long long tstamp = hrt_get_current();
#endif
  gc_heap *h_start = h, *h_prev;
  void *result = NULL;
  // Find next heap
  while (result == NULL) {
    h_prev = h;
    h = h->next;
    if (h == NULL) {
      // Wrap around to the first heap block
      h_prev = NULL;
      h = h_passed;
    }
    if (h == h_start) {
      // Tried all and no heap exists with free space
      break;
    }
    // check allocation status to make sure we can use it
    if (h->is_full) {
      continue; // Cannot sweep until next GC cycle
    } else if (h->is_unswept == 1 && !gc_is_heap_empty(h)) { // TODO: empty function does not support fixed-size heaps yet
      unsigned int h_size = h->size;
      //unsigned int prev_free_size = h->free_size;
      //if (h->is_unswept == 1) {
      //  prev_free_size = h_size; // Full size was cached
      //}
      gc_heap *keep = gc_sweep(h, thd); // Clean up garbage objects
#ifdef CYC_HIGH_RES_TIMERS
fprintf(stderr, "sweep heap %p \n", h);
hrt_log_delta("gc sweep", tstamp);
#endif
      h_passed->num_unswept_children--;
      if (!keep) {
#if GC_DEBUG_TRACE
  fprintf(stderr, "heap %p marked for deletion\n", h);
#endif
        // Heap marked for deletion, remove it and keep searching
        gc_heap *freed = gc_heap_free(h, h_prev);
        if (freed) {
          if (h_prev) {
            h = h_prev;
          } else {
            h = h_passed;
          }
          thd->cached_heap_total_sizes[h->type] -= h_size;
          continue;
        }
      }
    }
    result = gc_try_alloc(h, size, obj, thd);
    if (result) {
      h_passed->next_free = h;
      h_passed->last_alloc_size = size;
    } else {
      // TODO: else, assign heap full? YES for fixed-size, for REST maybe not??
      h->is_full = 1;
#if GC_DEBUG_TRACE
  fprintf(stderr, "heap %p is full\n", h);
#endif
    }
  }
  return result;
}

/**
 * @brief Same as `gc_try_alloc` but optimized for heaps for fixed-sized objects.
 * @param h          Heap to allocate from
 * @param size       Size of the requested object, in bytes
 * @param obj        Object containing data that will be copied to the heap
 * @param thd        Thread data for the mutator using this heap
 * @return Pointer to the newly-allocated object, or `NULL` if allocation failed
 *
 * This function will fail if there is no space on the heap for the 
 * requested object.
 */
static void *gc_try_alloc_fixed_size(gc_heap * h, size_t size, char *obj, gc_thread_data * thd)
{
    void *result;

    if (h->free_list) {
      result = h->free_list;
      h->free_list = h->free_list->next;
    } else if (h->remaining) {
      h->remaining -= h->block_size;
      result = h->data_end - h->remaining - h->block_size;
    } else {
      // Cannot allocate on this page, skip it
      result = NULL;
    }

    if (result) {
      // Copy object into heap now to avoid any uninitialized memory issues
      #if GC_DEBUG_TRACE
      if (size < (32 * NUM_ALLOC_SIZES)) {
        allocated_size_counts[(size / 32) - 1]++;
      }
      #endif
      gc_copy_obj(result, obj, thd);

      h->free_size -= size;
      return result;
    }
    return NULL;
}

void *gc_try_alloc_slow_fixed_size(gc_heap *h_passed, gc_heap *h, size_t size, char *obj, gc_thread_data *thd)
{
#ifdef CYC_HIGH_RES_TIMERS
long long tstamp = hrt_get_current();
#endif
  gc_heap *h_start = h, *h_prev;
  void *result = NULL;
  // Find next heap
  while (result == NULL) {
    h_prev = h;
    h = h->next;
    if (h == NULL) {
      // Wrap around to the first heap block
      h_prev = NULL;
      h = h_passed;
    }
    if (h == h_start) {
      // Tried all and no heap exists with free space
      break;
    }
    // check allocation status to make sure we can use it
    if (h->is_full) {
      continue; // Cannot sweep until next GC cycle
    } else if (h->is_unswept == 1 && !gc_is_heap_empty(h)) {
      unsigned int h_size = h->size;
      gc_heap *keep = gc_sweep_fixed_size(h, thd); // Clean up garbage objects
#ifdef CYC_HIGH_RES_TIMERS
fprintf(stderr, "sweep fixed size heap %p size %lu \n", h, size);
hrt_log_delta("gc sweep fixed size", tstamp);
#endif
      h_passed->num_unswept_children--;
      if (!keep) {
#if GC_DEBUG_TRACE
  fprintf(stderr, "heap %p marked for deletion\n", h);
#endif
        // Heap marked for deletion, remove it and keep searching
        gc_heap *freed = gc_heap_free(h, h_prev);
        if (freed) {
          if (h_prev) {
            h = h_prev;
          } else {
            h = h_passed;
          }
          thd->cached_heap_total_sizes[h->type] -= h_size;
          continue;
        }
      }
    }
    result = gc_try_alloc_fixed_size(h, size, obj, thd);
    if (result) {
      h_passed->next_free = h;
      h_passed->last_alloc_size = size;
    } else {
      // TODO: else, assign heap full? YES for fixed-size, for REST maybe not??
      h->is_full = 1;
#if GC_DEBUG_TRACE
  fprintf(stderr, "heap %p is full\n", h);
#endif
    }
  }
  return result;
}

/**
 * @brief A convenience function for allocating bignums
 * @param data  The mutator's thread data object
 * @return Pointer to a heap object for the bignum
 */
void *gc_alloc_bignum(gc_thread_data *data)
{
  int heap_grown, result;
  bignum_type *bn;
  bignum_type tmp;
  // No need to do this since tmp is always local
  //tmp.hdr.mark = gc_color_red;
  //tmp.hdr.grayed = 0;
  tmp.tag = bignum_tag;
  bn = gc_alloc(((gc_thread_data *)data)->heap, sizeof(bignum_type), (char *)(&tmp), (gc_thread_data *)data, &heap_grown);

  if ((result = mp_init(&bignum_value(bn))) != MP_OKAY) {
     fprintf(stderr, "Error initializing number %s",
                     mp_error_to_string(result));
     exit(1);
  }
  return bn;
}

/**
 * @brief A helper function to create a heap-allocated copy of a bignum
 * @param data  The mutator's thread data object
 * @param src   The bignum instance to copy to the heap
 * @return Pointer to the heap object
 */
void *gc_alloc_from_bignum(gc_thread_data *data, bignum_type *src)
{
  int heap_grown;
  return gc_alloc(((gc_thread_data *)data)->heap, sizeof(bignum_type), (char *)(src), (gc_thread_data *)data, &heap_grown);
}


/**
 * @brief Allocate memory on the heap for an object
 * @param hrt   The root of the heap to allocate from
 * @param size  Size of the object to allocate
 * @param obj   Object containing data to copy to the heap
 * @param thd   The requesting mutator's thread data object
 * @param heap_grown  Pointer to an "out" parameter that will be set to
 *                    `1` if the heap is grown in size.
 * @return Pointer to the heap object
 *
 * This function will attempt to grow the heap if it is full, and will
 * terminate the program if the OS is out of memory.
 */
void *gc_alloc(gc_heap_root * hrt, size_t size, char *obj, gc_thread_data * thd,
               int *heap_grown)
{
  void *result = NULL;
  gc_heap *h_passed, *h = NULL;
  int heap_type;
  void *(*try_alloc)(gc_heap * h, size_t size, char *obj, gc_thread_data * thd);
  void *(*try_alloc_slow)(gc_heap *h_passed, gc_heap *h, size_t size, char *obj, gc_thread_data *thd);
  size = gc_heap_align(size);
  if (size <= 32) {
    heap_type = HEAP_SM;
    //try_alloc = &gc_try_alloc;
    //try_alloc_slow = &gc_try_alloc_slow;
    // TODO:
    try_alloc = &gc_try_alloc_fixed_size;
    try_alloc_slow = &gc_try_alloc_slow_fixed_size;
  } else if (size <= 64) {
    heap_type = HEAP_64;
    //try_alloc = &gc_try_alloc;
    //try_alloc_slow = &gc_try_alloc_slow;
    // TODO:
    try_alloc = &gc_try_alloc_fixed_size;
    try_alloc_slow = &gc_try_alloc_slow_fixed_size;
// Only use this heap on 64-bit platforms, where larger objs are used more often
// Code from http://stackoverflow.com/a/32717129/101258
#if INTPTR_MAX == INT64_MAX
  } else if (size <= 96) {
    heap_type = HEAP_96;
    //try_alloc = &gc_try_alloc;
    //try_alloc_slow = &gc_try_alloc_slow;
    // TODO:
    try_alloc = &gc_try_alloc_fixed_size;
    try_alloc_slow = &gc_try_alloc_slow_fixed_size;
#endif
  } else if (size >= MAX_STACK_OBJ) {
    heap_type = HEAP_HUGE;
    try_alloc = &gc_try_alloc;
    try_alloc_slow = &gc_try_alloc_slow;
  } else {
    heap_type = HEAP_REST;
    try_alloc = &gc_try_alloc;
    try_alloc_slow = &gc_try_alloc_slow;
  }
  h = hrt->heap[heap_type];
  h_passed = h;
  // Start searching from the last heap page we had a successful
  // allocation from, unless the current request is for a smaller
  // block in which case there may be available memory closer to
  // the start of the heap.
  if (size >= h->last_alloc_size) {
    h = h->next_free;
  }
  // Fast path
  result = try_alloc(h, size, obj, thd);
  if (result) {
    h_passed->next_free = h;
    h_passed->last_alloc_size = size;
  } else {
    // Slow path, find another heap block
    h->is_full = 1;
    result = try_alloc_slow(h_passed, h, size, obj, thd);
#if GC_DEBUG_VERBOSE
fprintf(stderr, "slow alloc of %p\n", result);
#endif
    if (result) {
      // Check if we need to start a major collection
      if (heap_type != HEAP_HUGE && 
        (//(try_alloc == &gc_try_alloc_fixed_size && // Fixed-size object heap
         // h_passed->num_unswept_children < (GC_COLLECT_UNDER_UNSWEPT_HEAP_COUNT * 128)) ||
         h_passed->num_unswept_children < GC_COLLECT_UNDER_UNSWEPT_HEAP_COUNT)) {
//           gc_num_unswept_heaps(h_passed) < GC_COLLECT_UNDER_UNSWEPT_HEAP_COUNT)){
//        printf("major collection heap_type = %d h->num_unswept = %d, computed = %d\n", heap_type,  h_passed->num_unswept_children, gc_num_unswept_heaps(h_passed));
        //if (h_passed->num_unswept_children != gc_num_unswept_heaps(h_passed)) {
        //  printf("ERROR, counts do not match!\n");
        //}
        gc_start_major_collection(thd);
      }
    } else {
      // Slowest path, allocate a new heap block
      /* A vanilla mark&sweep collector would collect now, but unfortunately */
      /* we can't do that because we have to go through multiple stages, some */
      /* of which are asynchronous. So... no choice but to grow the heap. */
      gc_heap *last = gc_grow_heap(h, size, thd);
      *heap_grown = 1;
      result = try_alloc_slow(h_passed, last, size, obj, thd);
#if GC_DEBUG_VERBOSE
fprintf(stderr, "slowest alloc of %p\n", result);
#endif
      if (result) {
        // We had to allocate memory, start a major collection ASAP!
        if (heap_type != HEAP_HUGE) {
          gc_start_major_collection(thd);
        }
      } else {
        fprintf(stderr, "out of memory error allocating %zu bytes\n", size);
        fprintf(stderr, "Heap type %d diagnostics:\n", heap_type);
        gc_print_stats(h);
        exit(1);                  /* could throw error, but OOM is a major issue, so... */
      }
    }
  }

#if GC_DEBUG_TRACE
  allocated_heap_counts[heap_type]++;
#endif

#if GC_DEBUG_VERBOSE
  fprintf(stderr, "alloc %p size = %zu, obj=%p, tag=%d, mark=%d, thd->alloc=%d, thd->trace=%d\n", result,
          size, obj, type_of(obj), mark(((object) result)),
          thd->gc_alloc_color, thd->gc_trace_color);
  // Debug check, should no longer be necessary
  //if (is_value_type(result)) {
  //  printf("Invalid allocated address - is a value type %p\n", result);
  //}
#endif
  return result;
}

/**
 * @brief Get the number of bytes that will be allocated for `obj`.
 * @param obj Object to inspect
 * @param q   Previous free list pointer, set to `NULL` if not applicable
 * @param r   Next free list pointer, set to `NULL` if not applicable
 * @return Number of bytes, including any needed for alignment
 */
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
  if (t == closureN_tag) {
    return gc_heap_align(sizeof(closureN_type) +
                         sizeof(object) * 
                         ((closureN_type *) obj)->num_elements);
  }
  if (t == double_tag)
    return gc_heap_align(sizeof(double_type));
  if (t == closure1_tag)
    return gc_heap_align(sizeof(closure1_type));
  if (t == string_tag) {
    return gc_heap_align(sizeof(string_type) + string_len(obj) + 1);
  }
  if (t == vector_tag) {
    return gc_heap_align(sizeof(vector_type) +
                         sizeof(object) * ((vector_type *) obj)->num_elements);
  }
  if (t == bytevector_tag) {
    return gc_heap_align(sizeof(bytevector_type) +
                         sizeof(char) * ((bytevector) obj)->len);
  }
  if (t == macro_tag)
    return gc_heap_align(sizeof(macro_type));
  if (t == bignum_tag)
    return gc_heap_align(sizeof(bignum_type));
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
  if (t == atomic_tag)
    return gc_heap_align(sizeof(atomic_type));
  if (t == integer_tag)
    return gc_heap_align(sizeof(integer_type));
  if (t == complex_num_tag)
    return gc_heap_align(sizeof(complex_num_type));

  fprintf(stderr, "gc_allocated_bytes: unexpected object %p of type %d\n", obj,
          t);
  exit(1);
  return 0;
}

/**
 * @brief Get the heap's last page
 * @param h Heap to inspect
 * @return Pointer to the heap's last page
 *
 * This function does not do any locking, it is the responsibility of
 * the caller to hold the appropriate locks prior to calling.
 */
gc_heap *gc_heap_last(gc_heap * h)
{
  while (h->next)
    h = h->next;
  return h;
}

/**
 * @brief A convenient front-end to the actual gc_sweep function.
 */
void gc_collector_sweep()
{
  ck_array_iterator_t iterator;
  gc_thread_data *m;
  CK_ARRAY_FOREACH(&Cyc_mutators, &iterator, &m) {
    // Tracing is done, remove the trace color
    m->gc_trace_color = m->gc_alloc_color;
    // Let mutator know we are done tracing
    ck_pr_cas_8(&(m->gc_done_tracing), 0, 1);
  }
#if GC_DEBUG_TRACE
  fprintf(stderr, "all thread heap sweeps done\n");
#endif
}

/**
 * @brief Sweep portion of the GC algorithm
 * @param h           Heap to sweep
 * @param thd         Thread data object for the mutator using this heap
 * @return Pointer to the heap, or NULL if heap is to be freed
 *
 * This portion of the major GC algorithm is responsible for returning unused
 * memory slots to the heap. It is only called by the allocator to free up space
 * after the heap has been traced to identify live objects.
 */
gc_heap *gc_sweep(gc_heap * h, gc_thread_data *thd)
{
  size_t freed, size;
  object p, end;
  gc_free_list *q, *r, *s;
#if GC_DEBUG_SHOW_SWEEP_DIAG
  gc_heap *orig_heap_ptr = h;
#endif
  gc_heap *rv = h;
  //int markColor = ck_pr_load_8(&gc_color_mark);

  //h->next_free = h;
  h->last_alloc_size = 0;
  //h->free_size = 0;
  h->is_unswept = 0;

#if GC_DEBUG_SHOW_SWEEP_DIAG
  fprintf(stderr, "\nBefore sweep -------------------------\n");
  fprintf(stderr, "Heap %d diagnostics:\n", h->type);
  gc_print_stats(orig_heap_ptr);
#endif

  //for (; h; prev_h = h, h = h->next)       // All heaps
#if GC_DEBUG_TRACE
    fprintf(stderr, "sweep heap %p, size = %zu\n", h, (size_t) h->size);
#endif
#if GC_DEBUG_VERBOSE
    {
      gc_free_list *tmp = h->free_list;
      while (tmp) {
        fprintf(stderr, "free list %p\n", tmp);
        tmp = tmp->next;
      }
    }
#endif
    p = gc_heap_first_block(h);
    q = h->free_list;
    end = gc_heap_end(h);
    while (p < end) {
      // find preceding/succeeding free list pointers for p
      for (r = q->next; r && ((char *)r < (char *)p); q = r, r = r->next) ;

      if ((char *)r == (char *)p) {     // this is a free block, skip it
        p = (object) (((char *)p) + r->size);
        //h->free_size += r->size;
#if GC_DEBUG_VERBOSE
        fprintf(stderr, "skip free block %p size = %zu\n", p, r->size);
#endif
        continue;
      }
      size = gc_allocated_bytes(p, q, r);

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

      // Use the object's mark to determine if we keep it. 
      // Need to check for both colors because:
      // - Objects that are either newly-allocated or recently traced are given 
      //   the alloc color, and we need to keep them.
      // - If the collector is currently tracing, objects not traced yet will 
      //   have the trace/clear color. We need to keep any of those to make sure
      //   the collector has a chance to trace the entire heap.
      if (//mark(p) != markColor &&
          mark(p) != thd->gc_alloc_color && 
          mark(p) != thd->gc_trace_color) { //gc_color_clear) 
#if GC_DEBUG_VERBOSE
        fprintf(stderr, "sweep is freeing unmarked obj: %p with tag %d mark %d - alloc color %d trace color %d\n", p,
                type_of(p),
                mark(p),
                thd->gc_alloc_color, thd->gc_trace_color);
#endif
        //mark(p) = gc_color_blue;        // Needed?
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
        } else if (type_of(p) == bignum_tag) {
          // TODO: this is no good if we abandon bignum's on the stack
          // in that case the finalizer is never called
#if GC_DEBUG_VERBOSE
          fprintf(stderr, "mp_clear from sweep\n");
#endif
          mp_clear(&(((bignum_type *)p)->bn));
        }
        // free p
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
        h->free_size += size;
      } else {
//#if GC_DEBUG_VERBOSE
//        fprintf(stderr, "sweep: object is marked %p\n", p);
//#endif
        p = (object) (((char *)p) + size);
      }
    }
    // Free the heap page if possible.
    //
    // With huge heaps, this becomes more important. one of the huge
    // pages only has one object, so it is likely that the page
    // will become free at some point and could be reclaimed.
    //
    // The newly created flag is used to attempt to avoid situtaions
    // where a page is allocated because there is not enough free space,
    // but then we do a sweep and see it is empty so we free it, and
    // so forth. A better solution might be to keep empty heap pages
    // off to the side and only free them if there is enough free space
    // remaining without them.
    //
    // Experimenting with only freeing huge heaps
    if (gc_is_heap_empty(h)) {
      if (h->type == HEAP_HUGE || (h->ttl--) <= 0) {
        rv = NULL; // Let caller know heap needs to be freed
      }
    } else {
      //(thd->heap->heap[h->type])->num_unswept_children--;
    }

#if GC_DEBUG_SHOW_SWEEP_DIAG
  fprintf(stderr, "\nAfter sweep -------------------------\n");
  fprintf(stderr, "Heap %d diagnostics:\n", h->type);
  gc_print_stats(orig_heap_ptr);
#endif

  return rv;
}

/**
 * @brief Increase the size of the mutator's move buffer
 * @param d Mutator's thread data object
 */
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

// END heap definitions

// Tri-color GC section

/////////////////////////////////////////////
// GC functions called by the Mutator threads

/**
 * @brief Clear thread data read/write fields
 * @param thd Mutator's thread data object
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
 * @brief Move pending writes to 'last_write'
 * @param thd Mutator's thread data object
 * @param locked  Does the caller hold the mutator lock?
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

// /**
//  * @brief Determine if object lives on the thread's stack
//  * @param thd Mutator's thread data
//  * @param obj Object to inspect
//  * @return True if `obj` is on the mutator's stack, false otherwise
//  */
// int gc_is_stack_obj(gc_thread_data * thd, object obj)
// {
//   char tmp;
//   object low_limit = &tmp;
//   object high_limit = thd->stack_start;
//   return (stack_overflow(low_limit, obj) && stack_overflow(obj, high_limit));
// }

/**
 * @brief Helper function for `gc_mut_update`
 */
static void mark_stack_or_heap_obj(gc_thread_data * thd, object obj, int locked)
{
  char tmp;
  if (!is_object_type(obj) || type_of(obj) == boolean_tag) {
    return;
  } else if (gc_is_stack_obj(&tmp, thd, obj)) {
    // Set object to be marked after moved to heap by next GC.
    // This avoids having to recursively examine the stack now, 
    // which we have to do anyway during minor GC.
    grayed(obj) = 1;
  } else {
    // Value is on the heap, mark gray right now
    if (!locked) { pthread_mutex_lock(&(thd->lock)); }
    gc_mark_gray(thd, obj);
    if (!locked) { pthread_mutex_unlock(&(thd->lock)); }
  }
}

/**
 * @brief Write barrier for updates to heap-allocated objects
 * @param thd     Mutator's thread data
 * @param old_obj Old object value prior to the mutation
 * @param value   New object value
 *
 * The key for this barrier is to identify stack objects that contain
 * heap references, so they can be marked to avoid collection.
*/
void gc_mut_update(gc_thread_data * thd, object old_obj, object value)
{
  int //status = ck_pr_load_int(&gc_status_col),
      stage = ck_pr_load_int(&gc_stage);
  if (ck_pr_load_int(&(thd->gc_status)) != STATUS_ASYNC) {
    pthread_mutex_lock(&(thd->lock));
    mark_stack_or_heap_obj(thd, old_obj, 1);
    mark_stack_or_heap_obj(thd, value, 1);
    pthread_mutex_unlock(&(thd->lock));
  } else if (stage == STAGE_TRACING) {
//fprintf(stderr, "DEBUG - GC async tracing marking heap obj %p ", old_obj);
//Cyc_display(thd, old_obj, stderr);
//fprintf(stderr, "\n");
    mark_stack_or_heap_obj(thd, old_obj, 0);
#if GC_DEBUG_VERBOSE
    if (is_object_type(old_obj) && (mark(old_obj) == gc_color_clear ||
                                    mark(old_obj) == gc_color_purple)) {
      fprintf(stderr,
              "added to mark buffer (trace) from write barrier %p:mark %d:",
              old_obj, mark(old_obj));
      Cyc_display(thd, old_obj, stderr);
      fprintf(stderr, "\n");
    }
#endif
  }
}

/**
 * @brief Called by a mutator to cooperate with the collector thread
 * @param thd Mutator's thread data
 * @param buf_len Number of objects moved to the heap by the mutator during minor GC
 *
 * This function must be called periodically by each mutator to coordinate
 * with the collector. In our implementation it is called after minor GC.
 */
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
//printf("DEBUG - mutator is cooperating\n");            
      // Mark thread "roots":
      // Begin by marking current continuation, which may have already
      // been on the heap prior to latest minor GC
      pthread_mutex_lock(&(thd->lock));
      gc_mark_gray(thd, thd->gc_cont);
      for (i = 0; i < thd->gc_num_args; i++) {
        gc_mark_gray(thd, thd->gc_args[i]);
      }
      // Mark thread object, if applicable. Very likely this is its only ref
      if (thd->scm_thread_obj) {
        gc_mark_gray(thd, thd->scm_thread_obj);
      }
      if (thd->exception_handler_stack) {
        gc_mark_gray(thd, thd->exception_handler_stack);
      }
      if (thd->param_objs) {
        gc_mark_gray(thd, thd->param_objs);
      }
      // Also, mark everything the collector moved to the heap
      for (i = 0; i < buf_len; i++) {
        gc_mark_gray(thd, thd->moveBuf[i]);
      }
      pthread_mutex_unlock(&(thd->lock));
      thd->gc_alloc_color = ck_pr_load_8(&gc_color_mark);
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

  // If we have finished tracing, clear any "full" bits on the heap
  if(ck_pr_cas_8(&(thd->gc_done_tracing), 1, 0)) {
    int heap_type, unswept;
    gc_heap *h_tmp, *h_head;
#if GC_DEBUG_VERBOSE
fprintf(stdout, "done tracing, cooperator is clearing full bits\n");
#endif
    for (heap_type = 0; heap_type < NUM_HEAP_TYPES; heap_type++) {
      h_head = h_tmp = thd->heap->heap[heap_type];
      unswept = 0;
      for (; h_tmp; h_tmp = h_tmp->next) {
        if (h_tmp && h_tmp->is_full == 1) {
          h_tmp->is_full = 0;
          h_tmp->is_unswept = 1;
          unswept++;
        } else if (h_tmp->is_unswept == 1) {
          unswept++;
        }
      }
      if (h_head) {
        h_head->num_unswept_children = unswept;
        //printf("set num_unswept_children = %d computed = %d\n", h_head->num_unswept_children, gc_num_unswept_heaps(h_head));
      }
    }

    // At least for now, let the main thread help clean up any terminated threads
    if (thd == primordial_thread) {
#if GC_DEBUG_TRACE
      fprintf(stderr, "main thread is cleaning up any old thread data\n");
#endif
      gc_free_old_thread_data();
    }

    // Clear allocation counts to delay next GC trigger
    thd->heap_num_huge_allocations = 0;
    thd->num_minor_gcs = 0;
// TODO: can't do this now because we don't know how much of the heap is free, as none if it has
// been swept and we are sweeping incrementally
//
//    for (heap_type = 0; heap_type < 2; heap_type++) {
//      uint64_t free_size = gc_heap_free_size(thd->heap->heap[heap_type]),
//               threshold = (thd->cached_heap_total_sizes[heap_type]) * GC_FREE_THRESHOLD;
//      if (free_size < threshold) {
//        int i, new_heaps = (int)((threshold - free_size) / HEAP_SIZE);
//        if (new_heaps < 1) {
//          new_heaps = 1;
//        }
////#if GC_DEBUG_TRACE
//        fprintf(stderr, "Less than %f%% of the heap %d is free (%llu / %llu), growing it %d times\n",
//                100.0 * GC_FREE_THRESHOLD, heap_type, free_size, threshold, new_heaps);
////#endif
//if (new_heaps > 100){ exit(1);} // Something is wrong!
//        for(i = 0; i < new_heaps; i++){
//          gc_grow_heap(thd->heap->heap[heap_type], heap_type, 0, 0, thd);
//        }
//    //  while ( gc_heap_free_size(thd->heap->heap[heap_type]) < //thd->cached_heap_free_sizes[heap_type] <
//    //    if (heap_type == HEAP_SM) {
//    //      gc_grow_heap(thd->heap->heap[heap_type], heap_type, 0, 0, thd);
//    //    } else if (heap_type == HEAP_64) {
//    //      gc_grow_heap(thd->heap->heap[heap_type], heap_type, 0, 0, thd);
//    //    } else if (heap_type == HEAP_REST) {
//    //      gc_grow_heap(thd->heap->heap[heap_type], heap_type, 0, 0, thd);
//    //    }
//      }
//    }


    // DEBUG diagnostics
#if GC_DEBUG_SHOW_SWEEP_DIAG
    for (heap_type = 0; heap_type < NUM_HEAP_TYPES; heap_type++) {
      h_tmp = thd->heap->heap[heap_type];
      if (h_tmp) {
        fprintf(stderr, "From collector - Heap %d diagnostics:\n", heap_type);
        gc_print_stats(h_tmp);
      }
    }
#endif
  }

  thd->num_minor_gcs++;
  if (thd->num_minor_gcs % 10 == 9) { // Throttle a bit since usually we do not need major GC
    thd->cached_heap_free_sizes[HEAP_SM]   = gc_heap_free_size(thd->heap->heap[HEAP_SM]) ;
    thd->cached_heap_free_sizes[HEAP_64]   = gc_heap_free_size(thd->heap->heap[HEAP_64]) ;
    thd->cached_heap_free_sizes[HEAP_96]   = gc_heap_free_size(thd->heap->heap[HEAP_96]) ;
    thd->cached_heap_free_sizes[HEAP_REST] = gc_heap_free_size(thd->heap->heap[HEAP_REST]);

#if GC_DEBUG_VERBOSE
    fprintf(stderr, "heap %d free %zu total %zu\n", HEAP_SM, thd->cached_heap_free_sizes[HEAP_SM], thd->cached_heap_total_sizes[HEAP_SM]);
    if (thd->cached_heap_free_sizes[HEAP_SM] > thd->cached_heap_total_sizes[HEAP_SM]) {
      fprintf(stderr, "gc_mut_cooperate - Invalid cached heap sizes, free=%zu total=%zu\n", 
        thd->cached_heap_free_sizes[HEAP_SM], thd->cached_heap_total_sizes[HEAP_SM]);
      exit(1);
    }
    fprintf(stderr, "heap %d free %zu total %zu\n", HEAP_64, thd->cached_heap_free_sizes[HEAP_64], thd->cached_heap_total_sizes[HEAP_64]);
    if (thd->cached_heap_free_sizes[HEAP_64] > thd->cached_heap_total_sizes[HEAP_64]) {
      fprintf(stderr, "gc_mut_cooperate - Invalid cached heap sizes, free=%zu total=%zu\n", 
        thd->cached_heap_free_sizes[HEAP_64], thd->cached_heap_total_sizes[HEAP_64]);
      exit(1);
    }
    fprintf(stderr, "heap %d free %zu total %zu\n", HEAP_96, thd->cached_heap_free_sizes[HEAP_96], thd->cached_heap_total_sizes[HEAP_96]);
    if (thd->cached_heap_free_sizes[HEAP_96] > thd->cached_heap_total_sizes[HEAP_96]) {
      fprintf(stderr, "gc_mut_cooperate - Invalid cached heap sizes, free=%zu total=%zu\n", 
        thd->cached_heap_free_sizes[HEAP_96], thd->cached_heap_total_sizes[HEAP_96]);
      exit(1);
    }
    fprintf(stderr, "heap %d free %zu total %zu\n", HEAP_REST, thd->cached_heap_free_sizes[HEAP_REST], thd->cached_heap_total_sizes[HEAP_REST]);
    if (thd->cached_heap_free_sizes[HEAP_REST] > thd->cached_heap_total_sizes[HEAP_REST]) {
      fprintf(stderr, "gc_mut_cooperate - Invalid cached heap sizes, free=%zu total=%zu\n", 
        thd->cached_heap_free_sizes[HEAP_REST], thd->cached_heap_total_sizes[HEAP_REST]);
      exit(1);
    }
#endif

    // Initiate collection cycle if free space is too low.
    // Threshold is intentially low because we have to go through an
    // entire handshake/trace/sweep cycle, ideally without growing heap.
    if (ck_pr_load_int(&gc_stage) == STAGE_RESTING &&
        (
         //(gc_heap_free_size(thd->heap->heap[HEAP_SM]) < //thd->cached_heap_free_sizes[HEAP_SM] <
         (thd->cached_heap_free_sizes[HEAP_SM] <
          thd->cached_heap_total_sizes[HEAP_SM] * GC_COLLECTION_THRESHOLD) ||
         //(gc_heap_free_size(thd->heap->heap[HEAP_64]) < //thd->cached_heap_free_sizes[HEAP_64] <
         (thd->cached_heap_free_sizes[HEAP_64] <
          thd->cached_heap_total_sizes[HEAP_64] * GC_COLLECTION_THRESHOLD) ||
  #if INTPTR_MAX == INT64_MAX
         //(gc_heap_free_size(thd->heap->heap[HEAP_96]) < //thd->cached_heap_free_sizes[HEAP_96] <
         (thd->cached_heap_free_sizes[HEAP_96] <
          thd->cached_heap_total_sizes[HEAP_96] * GC_COLLECTION_THRESHOLD) ||
  #endif
         //(gc_heap_free_size(thd->heap->heap[HEAP_REST]) < //thd->cached_heap_free_sizes[HEAP_REST] <
         (thd->cached_heap_free_sizes[HEAP_REST] <
          thd->cached_heap_total_sizes[HEAP_REST] * GC_COLLECTION_THRESHOLD) ||
         // Separate huge heap threshold since these are typically allocated as whole pages
         (thd->heap_num_huge_allocations > 100)
          )) {
  #if GC_DEBUG_TRACE
      fprintf(stderr,
              "Less than %f%% of the heap is free, initiating collector\n",
              100.0 * GC_COLLECTION_THRESHOLD);
  #endif
      ck_pr_cas_int(&gc_stage, STAGE_RESTING, STAGE_CLEAR_OR_MARKING);
    }
  }
}

/////////////////////////////////////////////
// Collector functions

/**
 * @brief Mark the given object gray if it is on the heap.
 * @param thd Mutator's thread data
 * @param obj Object to gray
 *
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
  //
  // Note when marking we check for both clear and purple to prevent against 
  // timing issues when incrementing colors and since if we ever reach a
  // purple object during tracing we would want to mark it.
  // TODO: revisit if checking for gc_color_purple is truly necessary here and elsewhere.
  if (is_object_type(obj) && (mark(obj) == gc_color_clear ||
                              mark(obj) == gc_color_purple)) {     // TODO: sync??
    // Place marked object in a buffer to avoid repeated scans of the heap.
// TODO:
// Note that ideally this should be a lock-free data structure to make the
// algorithm more efficient. So this code (and the corresponding collector
// trace code) should be converted at some point.
    mark_buffer_set(thd->mark_buffer, thd->last_write, obj);
    (thd->last_write)++;        // Already locked, just do it...
  }
}

/**
 * @brief Add a pending write to the mark buffer.
 * @param thd Mutator's thread data
 * @param obj Object to gray
 *
 * These are pended because they are written in a batch during minor GC.
 * To prevent race conditions we wait until all of the writes are made before
 * updating last write.
 *
 * TODO: figure out a new name for this function.
 */
void gc_mark_gray2(gc_thread_data * thd, object obj)
{
  if (is_object_type(obj) && (mark(obj) == gc_color_clear ||
                              mark(obj) == gc_color_purple)) {
    mark_buffer_set(thd->mark_buffer, thd->last_write + thd->pending_writes, obj);
    thd->pending_writes++;
  }
}

/**
 * @brief "Color" objects gray by adding them to the mark stack for further processing.
 * @param parent Parent of object, used for debugging only
 * @param obj Object to mark
 *
 * Note that stack objects are always colored red during creation, so
 * they should never be added to the mark stack. Which would be bad because it
 * could lead to stack corruption.
 */
#if GC_DEBUG_VERBOSE
static void gc_collector_mark_gray(object parent, object obj)
{
  if (is_object_type(obj) && (mark(obj) == gc_color_clear ||
                              mark(obj) == gc_color_purple)) {
    mark_stack = vpbuffer_add(mark_stack, &mark_stack_len, mark_stack_i++, obj);
    fprintf(stderr, "mark gray parent = %p (%d) obj = %p\n", parent,
            type_of(parent), obj);
  } else if (is_object_type(obj)) {
    fprintf(stderr, "not marking gray, parent = %p (%d) obj = %p mark(obj) = %d, gc_color_clear = %d\n", parent,
            type_of(parent), obj, mark(obj), gc_color_clear);
  }
}
#else
//
// Attempt to speed this up by forcing an inline
//
#define gc_collector_mark_gray(parent, gobj) \
  if (is_object_type(gobj) && (mark(gobj) == gc_color_clear || mark(gobj) == gc_color_purple)) { \
    mark_stack = vpbuffer_add(mark_stack, &mark_stack_len, mark_stack_i++, gobj); \
  }
#endif

#if GC_DEBUG_VERBOSE
void gc_mark_black(object obj)
{
  // TODO: is sync required to get colors? probably not on the collector
  // thread (at least) since colors are only changed once during the clear
  // phase and before the first handshake.
  int markColor = ck_pr_load_8(&gc_color_mark);
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
    case atomic_tag: {
        atomic_type *a = (atomic_type *)obj;
        object o = ck_pr_load_ptr(&(a->obj));
        if (obj) {
          gc_collector_mark_gray(obj, o);
        }
      }
    default:
      break;
    }
    if (mark(obj) != gc_color_red) {
      // Only blacken objects on the heap
      mark(obj) = markColor;
    }
    if (mark(obj) != gc_color_red) {
      fprintf(stderr, "marked %p %d\n", obj, markColor);
    } else {
      fprintf(stderr, "not marking stack obj %p %d\n", obj, markColor);
    }
  }
}
#else
// See full version above for debugging purposes.
// Also sync any changes to this macro with the function version
#define gc_mark_black(obj) \
{ \
  int markColor = ck_pr_load_8(&gc_color_mark); \
  if (is_object_type(obj) && mark(obj) != markColor) { \
    switch (type_of(obj)) { \
    case pair_tag:{ \
        gc_collector_mark_gray(obj, car(obj)); \
        gc_collector_mark_gray(obj, cdr(obj)); \
        break; \
      } \
    case closure1_tag: \
      gc_collector_mark_gray(obj, ((closure1) obj)->element); \
      break; \
    case closureN_tag:{ \
        int i, n = ((closureN) obj)->num_elements; \
        for (i = 0; i < n; i++) { \
          gc_collector_mark_gray(obj, ((closureN) obj)->elements[i]); \
        } \
        break; \
      } \
    case vector_tag:{ \
        int i, n = ((vector) obj)->num_elements; \
        for (i = 0; i < n; i++) { \
          gc_collector_mark_gray(obj, ((vector) obj)->elements[i]); \
        } \
        break; \
      } \
    case cvar_tag:{ \
        cvar_type *c = (cvar_type *) obj; \
        object pvar = *(c->pvar); \
        if (pvar) { \
          gc_collector_mark_gray(obj, pvar); \
        } \
        break; \
      } \
    default: \
      break; \
    } \
    if (mark(obj) != gc_color_red) { \
      mark(obj) = markColor; \
    } \
  } \
}
#endif

/**
 * @brief The collector's tracing algorithm
 *
 * This function ensures all live objects are marked prior to transitioning
 * to the collector's sweep phase.
 */
void gc_collector_trace()
{
  ck_array_iterator_t iterator;
  gc_thread_data *m;
  int clean = 0, last_write;
  while (!clean) {
    clean = 1;

    CK_ARRAY_FOREACH(&Cyc_mutators, &iterator, &m) {
      pthread_mutex_lock(&(m->lock));
      // Try doing this loop (majority of tracing) without the lock. We
      // shouldn't need to be locked to do it anyway and we still lock
      // below as a fail-safe. One potential issue here is this would be 
      // broken if the mark buffer needs to be grown. But this is not a
      // problem because we will only go as far as the mutator already 
      // went with the version of last write we are holding here... so
      // we avoid that race condition.
      last_write = m->last_write;
      pthread_mutex_unlock(&(m->lock)); 
      while (m->last_read < last_write) {
        clean = 0;
#if GC_DEBUG_VERBOSE
        fprintf(stderr,
                "gc_mark_black mark buffer %p, last_read = %d last_write = %d\n",
                mark_buffer_get(m->mark_buffer, m->last_read), m->last_read, last_write);
#endif
        gc_mark_black(mark_buffer_get(m->mark_buffer, m->last_read));
        gc_empty_collector_stack();
        (m->last_read)++;       // Inc here to prevent off-by-one error
      }
      //pthread_mutex_unlock(&(m->lock));

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

/**
 * @brief Empty the collector's mark stack
 *
 * Objects on the stack are removed one at a time and marked
 */
void gc_empty_collector_stack()
{
  object obj;
  // Mark stack is only used by the collector thread, so no sync needed
  while (mark_stack_i > 0) {    // not empty
    mark_stack_i--;
//#if GC_DEBUG_VERBOSE
//    fprintf(stderr, "gc_mark_black mark stack %p \n",
//      mark_stack[mark_stack_i]);
//#endif
    obj = mark_stack[mark_stack_i];
    gc_mark_black(obj);
  }
}

/**
 * @brief Called by the collector thread to perform a handshake with 
 *        all of the mutators
 * @param s Transition to this GC status
 */
void gc_handshake(gc_status_type s)
{
  gc_post_handshake(s);
  gc_wait_handshake();
}

/**
 * @brief Change GC status to the given type
 * @param s Transition to this GC status
 */
void gc_post_handshake(gc_status_type s)
{
  int status = ck_pr_load_int(&gc_status_col);
  while (!ck_pr_cas_int(&gc_status_col, status, s)) {
  }
}

/**
 * @brief Wait for all mutators to handshake
 *
 * This function is always called by the collector. If a mutator
 * is blocked and cannot handshake, the collector will cooperate
 * on its behalf, including invoking a minor GC of the mutator's
 * stack, so major GC can proceed.
 */
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
            #if GC_DEBUG_TRACE
            fprintf(stderr, "DEBUG - collector is cooperating for blocked mutator\n");            
            #endif
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
            if (m->scm_thread_obj) {
              gc_mark_gray(m, m->scm_thread_obj);
            }
            if (m->exception_handler_stack) {
              gc_mark_gray(m, m->exception_handler_stack);
            }
            if (m->param_objs) {
              gc_mark_gray(m, m->param_objs);
            }
            // Also, mark everything the collector moved to the heap
            for (i = 0; i < buf_len; i++) {
              gc_mark_gray(m, m->moveBuf[i]);
            }
            m->gc_alloc_color = ck_pr_load_8(&gc_color_mark);
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

/**
 * @brief Main collector function
 */
void gc_collector()
{
  //int old_clear, old_mark;
#if GC_DEBUG_TRACE
  print_allocated_obj_counts();
  gc_log(stderr, "Starting gc_collector");
#endif
//fprintf(stderr, " - Starting gc_collector\n"); // TODO: DEBUGGING!!!
  //clear : 
  ck_pr_cas_int(&gc_stage, STAGE_RESTING, STAGE_CLEAR_OR_MARKING);
  // exchange values of markColor and clearColor
  //
  // We now increment both so that clear becomes the old mark color and a
  // new value is used for the mark color. The old clear color becomes
  // purple, indicating any of these objects are garbage
  ck_pr_add_8(&gc_color_purple, 2);
  ck_pr_add_8(&gc_color_clear, 2);
  ck_pr_add_8(&gc_color_mark, 2);
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
  gc_wait_handshake();
  gc_request_mark_globals(); // Wait until mutators have new mark color
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
  gc_collector_sweep();

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

/**
 * @brief A high-resolution sleep function.
 *
 * @param  ms  Sleep time in milliseconds
 */
void gc_sleep_ms(int ms)
{
  struct timespec tim;
  tim.tv_sec = 0;
  tim.tv_nsec = ms * NANOSECONDS_PER_MILLISECOND;
  nanosleep(&tim, NULL);
}

static pthread_t collector_thread;

/**
 * @brief Spawn the collector thread
 */
void gc_start_collector()
{
  if (pthread_create
      (&collector_thread, NULL, collector_main, NULL)) {
    fprintf(stderr, "Error creating collector thread\n");
    exit(1);
  }
}

/**
 * @brief Mark globals as part of the tracing collector
 * @param globals
 * @param global_table
 *
 * This is called by the collector thread
 */
void gc_mark_globals(object globals, object global_table)
{
#if GC_DEBUG_TRACE
  //fprintf(stderr, "(gc_mark_globals heap: %p size: %d)\n", h, (unsigned int)gc_heap_total_size(h));
  fprintf(stderr, "Cyc_global_variables %p\n", globals);
#endif
  // Mark global variables
  gc_mark_black(globals);  // Internal global used by the runtime
  // Marking it ensures all glos are marked
  {
    list l = global_table;
    for (; l != NULL; l = cdr(l)) {
      cvar_type *c = (cvar_type *) car(l);
      object glo = *(c->pvar);
      if (glo != NULL) {
#if GC_DEBUG_VERBOSE
        fprintf(stderr, "global pvar %p\n", glo);
#endif
        gc_mark_black(glo);     // Mark actual object the global points to
      }
    }
  }
}


/////////////////////////////////////////////
// END tri-color marking section
/////////////////////////////////////////////

/**
 * @brief Initialize runtime data structures for a thread.
 * @param thd Mutator's thread data
 * @param mut_num     Unused
 * @param stack_base  Bottom of the mutator's stack
 * @param stack_size  Max allowed size of mutator's stack before triggering minor GC
 *
 * Must be called on the target thread itself during startup,
 * to verify stack limits are setup correctly.
 */
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
  thd->mutation_buflen = 128;
  thd->mutation_count = 0;
  thd->mutations = 
      vpbuffer_realloc(thd->mutations, &(thd->mutation_buflen));
  thd->globals_changed = 1;
  thd->param_objs = NULL;
  thd->exception_handler_stack = NULL;
  thd->scm_thread_obj = NULL;
  thd->thread_state = CYC_THREAD_STATE_NEW;
  //thd->mutator_num = mut_num;
  thd->jmp_start = malloc(sizeof(jmp_buf));
  thd->gc_args = malloc(sizeof(object) * NUM_GC_ARGS);
  thd->gc_num_args = 0;
  thd->moveBufLen = 0;
  gc_thr_grow_move_buffer(thd);
  thd->gc_alloc_color = ck_pr_load_8(&gc_color_clear);
  thd->gc_trace_color = thd->gc_alloc_color;
  thd->gc_done_tracing = 0;
  thd->gc_status = ck_pr_load_int(&gc_status_col);
  thd->pending_writes = 0;
  thd->last_write = 0;
  thd->last_read = 0;
  thd->mark_buffer = mark_buffer_init(128);
  if (pthread_mutex_init(&(thd->lock), NULL) != 0) {
    fprintf(stderr, "Unable to initialize thread mutex\n");
    exit(1);
  }
  thd->heap_num_huge_allocations = 0;
  thd->num_minor_gcs = 0;
  thd->cached_heap_free_sizes = calloc(5, sizeof(uintptr_t));
  thd->cached_heap_total_sizes = calloc(5, sizeof(uintptr_t));
  thd->heap = calloc(1, sizeof(gc_heap_root));
  thd->heap->heap = calloc(1, sizeof(gc_heap *) * NUM_HEAP_TYPES);
  thd->heap->heap[HEAP_REST] = gc_heap_create(HEAP_REST, INITIAL_HEAP_SIZE, thd);
  thd->heap->heap[HEAP_SM] = gc_heap_create(HEAP_SM, INITIAL_HEAP_SIZE, thd);
  thd->heap->heap[HEAP_64] = gc_heap_create(HEAP_64, INITIAL_HEAP_SIZE, thd);
  if (sizeof(void *) == 8) { // Only use this heap on 64-bit platforms
    thd->heap->heap[HEAP_96] = gc_heap_create(HEAP_96, INITIAL_HEAP_SIZE, thd);
  }
  thd->heap->heap[HEAP_HUGE] = gc_heap_create(HEAP_HUGE, 1024, thd);
}

/**
 * @brief Free all data for the given mutator
 * @param thd Mutator's thread data object containing data to free
 */
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
    // Merge heaps for the terminating thread into the main thread's heap.
    // Eventually any data that is unused will be freed, but we need to
    // keep the heap pages for now because they could still contain live 
    // objects.
    // Lock the primordial thread (hopefully will not cause any deadlocks)
    // but don't bother locking thd since it is already done by now.

// TODO: need to figure out a new solution since we no longer have the heap lock!!!!

//    pthread_mutex_lock(&(primordial_thread->heap_lock));
    gc_merge_all_heaps(primordial_thread, thd);
//    pthread_mutex_unlock(&(primordial_thread->heap_lock));
    if (thd->cached_heap_free_sizes)
      free(thd->cached_heap_free_sizes);
    if (thd->cached_heap_total_sizes)
      free(thd->cached_heap_total_sizes);
    if (thd->jmp_start)
      free(thd->jmp_start);
    if (thd->gc_args)
      free(thd->gc_args);
    if (thd->moveBuf)
      free(thd->moveBuf);
    if (thd->mark_buffer)
      mark_buffer_free(thd->mark_buffer);
    if (thd->stack_traces)
      free(thd->stack_traces);
    if (thd->mutations) {
      free(thd->mutations);
    }
    free(thd);
  }
}

/**
 * @brief Merge one heap into another. 
 * @param hdest Heap that will receive new pages
 * @param hsrc  Heap that is being merged to the end of `hdest`
 *
 * This function assumes appropriate locks are already held.
 */
void gc_heap_merge(gc_heap *hdest, gc_heap *hsrc)
{
  gc_heap *last = gc_heap_last(hdest);
  last->next = hsrc;
}

/**
 * @brief Merge all thread heaps into another.
 * @param dest Heap receiving new pages
 * @param src  Heap containing pages to be appended
 *
 * Assumes appropriate locks are already held.
 */
void gc_merge_all_heaps(gc_thread_data *dest, gc_thread_data *src)
{
  gc_heap *hdest, *hsrc;
  int heap_type;

  for (heap_type = 0; heap_type < NUM_HEAP_TYPES; heap_type++) {
    hdest = dest->heap->heap[heap_type];
    hsrc = src->heap->heap[heap_type];
    if (hdest && hsrc) {
      gc_heap_merge(hdest, hsrc);
      ck_pr_add_ptr(&(dest->cached_heap_total_sizes[heap_type]), 
           ck_pr_load_ptr(&(src->cached_heap_total_sizes[heap_type])));
      ck_pr_add_ptr(&(dest->cached_heap_free_sizes[heap_type]), 
           ck_pr_load_ptr(&(src->cached_heap_free_sizes[heap_type])));
    }
  }
  ck_pr_add_int(&(dest->heap_num_huge_allocations), 
       ck_pr_load_int(&(src->heap_num_huge_allocations)));
#if GC_DEBUG_TRACE
  fprintf(stderr, "Finished merging old heap data\n");
#endif
}

/**
 * @brief Called explicitly from a mutator thread to let the collector know
 *        it (may) block for an unknown period of time.
 * @param thd Mutator's thread data
 * @param cont The mutator's current continuation. This is required so that we can trace over this object in case the collector has to cooperate for the mutator.
 */
void gc_mutator_thread_blocked(gc_thread_data * thd, object cont)
{
  thd->gc_cont = cont;
  thd->gc_num_args = 0;         // Will be set later, after collection
  if (!ck_pr_cas_int((int *)&(thd->thread_state),
                     CYC_THREAD_STATE_RUNNABLE, CYC_THREAD_STATE_BLOCKED)) {
    fprintf(stderr,
            "Unable to change thread from runnable to blocked. status = %d\n",
            thd->thread_state);
    exit(1);
  }
}

void Cyc_apply_from_buf(void *data, int argc, object prim, object * buf);

/**
 * @brief While a mutator has declared itself blocked, it is possible
 *        that an object on its stack may be copied to the heap by
 *        the collector. The purpose of this function is to copy
 *        such an object again to ensure all fields are updated
 *        to their latest values.
 * @param obj   Object to copy
 * @param thd   Thread data object for the applicable mutator
 */
void gc_recopy_obj(object obj, gc_thread_data *thd) 
{
  // Temporarily change obj type so we can copy it
  object fwd = forward(obj);
  tag_type tag = type_of(fwd);
  type_of(obj) = tag;
  #if GC_DEBUG_TRACE
  fprintf(stderr, "\n!!! Recopying object %p with tag %d !!!\n\n", obj, tag);
  #endif
  gc_copy_obj(fwd, obj, thd); // Copy it again
  type_of(obj) = forward_tag; // Restore forwarding pointer tag on stack obj
}

/**
 * @brief Called explicitly from a mutator thread to let the collector know
 * that it has finished blocking. 
 * @param thd Mutator's thread data
 * @param result  Data returned by the blocking function
 * @param maybe_copied An object used by the mutator while blocked that may 
 *                     have been copied to the heap by the collector.
 *
 * In addition, if the collector cooperated on behalf of the mutator while 
 * it was blocking, the mutator will move any remaining stack objects to 
 * the heap and longjmp.
 */
void gc_mutator_thread_runnable(gc_thread_data * thd, object result, object maybe_copied)
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
    // Check if obj was copied while we slept
    if (maybe_copied && 
        is_object_type(maybe_copied) && 
        gc_is_stack_obj(&stack_limit, thd, maybe_copied) &&
        type_of(maybe_copied) == forward_tag) {
      gc_recopy_obj(maybe_copied, thd);
    }
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
