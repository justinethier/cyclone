#ifndef CYCLONE_CK_POLYFILL_H
#define CYCLONE_CK_POLYFILL_H

#include <stdbool.h>
#include <stdint.h>

// CK Array section
ck_array_t 
ck_array_iterator_t
ck_malloc
CK_ARRAY_MODE_SPMC

 ck_array_init(ck_array_t *array, unsigned int mode,
         struct ck_malloc *allocator, unsigned int initial_length);

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


  // TODO:

// Can we safely lock the array, make a copy, and interate over that????
#define CK_ARRAY_FOREACH(a, i, b)       \                                        
  (i)->snapshot = ck_pr_load_ptr(&(a)->active); \                                
  ck_pr_fence_load();       \                                                    
  for (unsigned int _ck_i = 0;        \                                          
      _ck_i < (a)->active->n_committed &&   \                                    
      ((*b) = (a)->active->values[_ck_i], 1); \                                  
      _ck_i++)                                                                   
                  
// CAS section
bool
ck_pr_cas_ptr(void *target, void *old_value, void *new_value);

bool
ck_pr_cas_int(int *target, int old_value, int new_value);

bool
ck_pr_cas_8(uint8_t *target, uint8_t old_value, uint8_t new_value);

#endif                          /* CYCLONE_CK_POLYFILL_H */
