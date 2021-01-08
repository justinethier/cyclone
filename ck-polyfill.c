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
#include "ck-polyfill.h"
#include <unistd.h>

static pthread_mutex_t glock;

void ck_polyfill_init()
{
  // will need to call this as soon as possible, perhaps from main()
  if (pthread_mutex_init(&(glock), NULL) != 0) {
    fprintf(stderr, "Unable to initialize global ck mutex\n");
    exit(1);
  }
}

// CK Hashset section
bool ck_hs_init(ck_hs_t *hs, unsigned int mode, ck_hs_hash_cb_t *hash_func,
    ck_hs_compare_cb_t *cmp, struct ck_malloc *alloc, unsigned long capacity, unsigned long seed)
{
  (*hs).hs = simple_hashset_create();
  if (pthread_mutex_init(&((*hs).lock), NULL) != 0) {
    fprintf(stderr, "Unable to initialize ck hashset mutex\n");
    exit(1);
  }
  return true;
}

void *ck_hs_get(ck_hs_t *hs, unsigned long hash, const void *key)
{
}

bool ck_hs_put(ck_hs_t *hs, unsigned long hash, const void *key)
{
}

// CK Array section
bool
ck_array_init(ck_array_t *array, unsigned int mode,
         struct ck_malloc *allocator, unsigned int initial_length)
{
  (*array).hs = hashset_create();
  if (pthread_mutex_init(&((*array).lock), NULL) != 0) {
    fprintf(stderr, "Unable to initialize ck array mutex\n");
    exit(1);
  }
  return true;
}

// DESCRIPTION
//      The ck_array_put_unique(3) function will attempt to insert the value of
//      pointer into the array pointed to by array.  This function may incur
//      additional memory allocations if not enough memory has been allocated in
//      the array for a new entry. The operation is also free to apply the opera-
//      tion immediately if there is an opportunity for elimination with a pend-
//      ing (uncommitted) remove operation. The function will not make any modi-
//      fications if the pointer already exists in the array.
// 
// RETURN VALUES
//      This function returns 1 if the pointer already exists in the array.  It
//      returns 0 if the put operation succeeded. It returns -1 on error due to
//      internal memory allocation failures.
int
ck_array_put_unique(ck_array_t *array, void *pointer)
{
  pthread_mutex_lock(&(array->lock));
  hashset_add(array->hs, pointer);
  pthread_mutex_unlock(&(array->lock));
  return true;
}

// DESCRIPTION
//      The ck_array_remove(3) function will attempt to remove the value of
//      pointer into the array pointed to by array. The operation is also free to
//      apply the operation immediately if there is an opportunity for elimina-
//      tion with a pending (uncommitted) put operation. If no elimination was
//      possible, the function may require to allocate more memory.
// 
// RETURN VALUES
//      This function returns true if the remove operation succeeded. It will
//      return false otherwise due to internal allocation failures or because the
//      value did not exist.
bool
ck_array_remove(ck_array_t *array, void *pointer){
  pthread_mutex_lock(&(array->lock));
  hashset_remove(array->hs, pointer);
  pthread_mutex_unlock(&(array->lock));
  return true;
}

// DESCRIPTION
//      The ck_array_commit(3) function will commit any pending put or remove
//      operations associated with the array. The function may end up requesting
//      the safe reclamation of memory actively being iterated upon by other
//      threads.
// 
// RETURN VALUES
//      This function returns true if the commit operation succeeded. It will
//      return false otherwise, and pending operations will not be applied.
bool ck_array_commit(ck_array_t *array) {
  // Nothing to do in this polyfill
  return true;
}


// TODO: global pthread mutex lock for this? obviously not ideal but the
// whole purpose of this module is a minimal interface for compatibility
// not speed

bool ck_pr_cas_int(int *target, int old_value, int new_value)
{
  bool result = false;
  pthread_mutex_lock(&glock);
  if (*target == old_value) {
    *target = new_value;
    result = true;
  }
  pthread_mutex_unlock(&glock);
  return result;
}

bool ck_pr_cas_ptr(void *target, void *old_value, void *new_value)
{
  bool result = false;
  pthread_mutex_lock(&glock);
  if ( *(void **)target == old_value ) {
    *(void **)target = new_value;
    result = true;
  }
  pthread_mutex_unlock(&glock);
  return result;
  // *(void **)v = set;                                                             
}

bool ck_pr_cas_8(uint8_t *target, uint8_t old_value, uint8_t new_value)
{
  bool result = false;
  pthread_mutex_lock(&glock);
  if (*target == old_value) {
    *target = new_value;
    result = true;
  }
  pthread_mutex_unlock(&glock);
  return result;
}

void
ck_pr_add_ptr(void *target, uintptr_t delta)
{
  pthread_mutex_lock(&glock);
  size_t value = (size_t) target;
  size_t d = (size_t) delta;
  size_t result = value + d;
  *(void **)target = (void *)result;
  // *(void **)v = set;                                                             
  pthread_mutex_unlock(&glock);
}

void
ck_pr_add_int(int *target, int delta)
{
  pthread_mutex_lock(&glock);
  (*target) += delta;
  pthread_mutex_unlock(&glock);
}

void
ck_pr_add_8(uint8_t *target, uint8_t delta)
{
  pthread_mutex_lock(&glock);
  (*target) += delta;
  pthread_mutex_unlock(&glock);
}

void *
ck_pr_load_ptr(const void *target)
{
  void *result;
  pthread_mutex_lock(&glock);
  result = *(void **)target;
  pthread_mutex_unlock(&glock);
  return result;
}

int
ck_pr_load_int(const int *target)
{
  int result;
  pthread_mutex_lock(&glock);
  result = *target;
  pthread_mutex_unlock(&glock);
  return result;
}

uint8_t
ck_pr_load_8(const uint8_t *target)
{
  uint8_t result;
  pthread_mutex_lock(&glock);
  result = *target;
  pthread_mutex_unlock(&glock);
  return result;
}


// Simple hashset

static const size_t prime_1 = 73;
static const size_t prime_2 = 5009;

struct simple_hashset_item_st {
    size_t hash;
    symbol_type* item;
};

struct simple_hashset_st {
    size_t nbits;
    size_t mask;

    size_t capacity;
    struct simple_hashset_item_st *items;
    size_t nitems;
    size_t n_deleted_items;

    hash_func_t hash_func;
};

size_t hash_function(const char* p, size_t len)
{
    size_t hash = 0;
    for (; *p; ++p)
        hash ^= *p + 0x9e3779b9 + (hash << 6) + (hash >> 2);
    return hash;
}

simple_hashset_t simple_hashset_create()
{
    simple_hashset_t set = (simple_hashset_t)calloc(1, sizeof(struct simple_hashset_st));

    if (set == NULL) {
        return NULL;
    }

    set->hash_func = hash_function;
    set->nbits = 3;
    set->capacity = (size_t)(1 << set->nbits);
    set->mask = set->capacity - 1;
    set->items = (struct simple_hashset_item_st*)calloc(set->capacity, sizeof(struct simple_hashset_item_st));
    if (set->items == NULL) {
        simple_hashset_destroy(set);
        return NULL;
    }
    set->nitems = 0;
    set->n_deleted_items = 0;
    return set;
}

void simple_hashset_set_hash_function(simple_hashset_t set, hash_func_t func)
{
    set->hash_func = func;
}

static int simple_hashset_add_member(simple_hashset_t set, symbol_type* key, size_t hash)
{
    size_t index;

    if (hash < 2) {
        return -1;
    }

    index = set->mask & (prime_1 * hash);

    while (set->items[index].hash != 0 && set->items[index].hash != 1) {
        if (set->items[index].hash == hash) {
            return 0;
        }
        else {
            /* search free slot */
            index = set->mask & (index + prime_2);
        }
    }

    ++set->nitems;
    if (set->items[index].hash == 1) {
        --set->n_deleted_items;
    }

    set->items[index].hash = hash;
    set->items[index].item = key;
    return 1;
}

static void set_maybe_rehash(simple_hashset_t set)
{
    struct simple_hashset_item_st *old_items;
    size_t old_capacity, index;


    if (set->nitems + set->n_deleted_items >= (double)set->capacity * 0.85) {
        old_items = set->items;
        old_capacity = set->capacity;
        ++set->nbits;
        set->capacity = (size_t)(1 << set->nbits);
        set->mask = set->capacity - 1;
        set->items = (struct simple_hashset_item_st*)calloc(set->capacity, sizeof(struct simple_hashset_item_st));
        set->nitems = 0;
        set->n_deleted_items = 0;
        //assert(set->items);
        for (index = 0; index < old_capacity; ++index) {
            simple_hashset_add_member(set, old_items[index].item, old_items[index].hash);
        }
        free(old_items);
    }
}

int simple_hashset_add(simple_hashset_t set, symbol_type* key)
{
// TODO: get from symbol type:, size_t key_len)
size_t key_len = strlen(key->desc);

    size_t hash = set->hash_func(key->desc, key_len);
    int rv = simple_hashset_add_member(set, key, hash);
    set_maybe_rehash(set);
    return rv;
}

int simple_hashset_is_member(simple_hashset_t set, symbol_type* key)
{
// TODO: get from symbol type, size_t key_len)
size_t key_len = strlen(key->desc);

    size_t hash = set->hash_func(key->desc, key_len);
    size_t index = set->mask & (prime_1 * hash);

    while (set->items[index].hash != 0) {
        if (set->items[index].hash == hash) {
            return index;
        } else {
            index = set->mask & (index + prime_2);
        }
    }
    return 0;
}



