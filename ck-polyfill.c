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
#include "ck_pr.h"
#include <unistd.h>

void ck_polyfill_init()
{
  // TODO: init any global locks, etc
  // will need to call this as soon as possible, perhaps from main()
}


// TODO: global pthread mutex lock for this? obviously not ideal but the
// whole purpose of this module is a minimal interface for compatibility
// not speed

bool ck_pr_cas_int(int *target, int old_value, int new_value)
{
  if (*target == old_value) {
    *target = new_value;
    return true;
  }
  return false;
}

bool ck_pr_cas_ptr(void *target, void *old_value, void *new_value)
{
  if ( *(void **)target == old_value ) {
    *(void **)target = new_value;
    return true;
  }
  return false;
  // *(void **)v = set;                                                             
}

bool ck_pr_cas_8(uint8_t *target, uint8_t old_value, uint8_t new_value)
{
  if (*target == old_value) {
    *target = new_value;
    return true;
  }
  return false;
}

bool ck_array_init(ck_array_t *array, unsigned int mode,
         struct ck_malloc *allocator, unsigned int initial_length)
{
  // TODO: init mutex, data structure
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
ck_array_put_unique(ck_array_t *array, void *pointer);

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
ck_array_remove(ck_array_t *array, void *pointer);


// DESCRIPTION
//      The ck_array_commit(3) function will commit any pending put or remove
//      operations associated with the array. The function may end up requesting
//      the safe reclamation of memory actively being iterated upon by other
//      threads.
// 
// RETURN VALUES
//      This function returns true if the commit operation succeeded. It will
//      return false otherwise, and pending operations will not be applied.
bool
ck_array_commit(ck_array_t *array);
