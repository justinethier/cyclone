#ifndef CYCLONE_CK_POLYFILL_H
#define CYCLONE_CK_POLYFILL_H

#include "cyclone/types.h"
#include "cyclone/hashset.h"
#include <stdbool.h>
#include <stdint.h>

void ck_polyfill_init();

struct ck_malloc {                                                               
  void *(*malloc)(size_t);                                                       
  void *(*realloc)(void *, size_t, size_t, bool);                                
  void (*free)(void *, size_t, bool);                                            
};                                                                               

///////////////////////////////////////////////////////////////////////////////
// Simple hashset (hashset with string support)
    /* hash function */
    typedef size_t(*hash_func_t)(const char*, size_t);

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
//    struct simple_hashset_st;
    typedef struct simple_hashset_st *simple_hashset_t;


    struct hashmap_st;
    typedef struct hashmap_st *hashmap_t;

    /*
     * HASHSET FUNCTIONS
     */

    /* create hashset instance */
    simple_hashset_t simple_hashset_create(void);

    /* destroy hashset instance */
    void simple_hashset_destroy(simple_hashset_t set);

    /* set hash function */
    void simple_hashset_set_hash_function(simple_hashset_t set, hash_func_t func);
    
    /* add item into the hashset.
     *
     * @note 0 and 1 is special values, meaning nil and deleted items. the
     *       function will return -1 indicating error.
     *
     * returns zero if the item already in the set and non-zero otherwise
     */
    int simple_hashset_add(simple_hashset_t set, symbol_type* key);

    /* check if existence of the item
     *
     * returns non-zero if the item exists and zero otherwise
     */
    int simple_hashset_is_member(simple_hashset_t set, symbol_type* key);

///////////////////////////////////////////////////////////////////////////////
// CK Hashset section

#define CK_HS_MODE_OBJECT 0
#define CK_HS_MODE_SPMC 0

struct ck_hs {                                                                   
  pthread_mutex_t lock;
  simple_hashset_t hs;
};                                                                               

typedef struct ck_hs ck_hs_t;  

/*                                                                               
 * Hash callback function.                                                       
 */                                                                              
typedef unsigned long ck_hs_hash_cb_t(const void *, unsigned long);              
                                                                                 
/*                                                                               
 * Returns pointer to object if objects are equivalent.                          
 */                                                                              
typedef bool ck_hs_compare_cb_t(const void *, const void *);  

#define CK_HS_HASH(hs, hs_hash, value) 0

bool ck_hs_init(ck_hs_t *, unsigned int, ck_hs_hash_cb_t *,                      
    ck_hs_compare_cb_t *, struct ck_malloc *, unsigned long, unsigned long);     

void *ck_hs_get(ck_hs_t *, unsigned long, const void *);                         
bool ck_hs_put(ck_hs_t *, unsigned long, const void *);                          

/*
struct ck_hs {                                                                   
  struct ck_malloc *m;                                                           
  struct ck_hs_map *map;                                                         
  unsigned int mode;                                                             
  unsigned long seed;                                                            
  ck_hs_hash_cb_t *hf;                                                           
  ck_hs_compare_cb_t *compare;                                                   
};                                                                               
typedef struct ck_hs ck_hs_t;  


*/

///////////////////////////////////////////////////////////////////////////////
// CK Array section
struct ck_array {
  pthread_mutex_t lock;
  hashset_t hs;
};
typedef struct ck_array ck_array_t;

struct ck_array_iterator {
  int unused;
};
typedef struct ck_array_iterator ck_array_iterator_t;

#define CK_ARRAY_MODE_SPMC 0

// DESCRIPTION
//      The ck_array_init(3) function initializes the array pointed to by the
//      argument array.  The mode value must be CK_ARRAY_MODE_SPMC.  The
//      allocator argument must point to a ck_malloc data structure with valid
//      non-NULL function pointers initialized for malloc, free and realloc. The
//      initial_length specifies the initial length of the array. The value of
//      initial_length must be greater than or equal to 2. An array allows for
//      one concurrent put or remove operations in the presence of any number of
//      concurrent CK_ARRAY_FOREACH operations.
// 
// RETURN VALUES
//      This function returns true if the array was successfully created. It
//      returns false if the creation failed. Failure may occur due to internal
//      memory allocation failures or invalid arguments.
bool
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
  pthread_mutex_lock(&((a)->lock)); \
  int tmpc = (a)->hs->nitems; \
  void **tmp = alloca(sizeof(void *) * tmpc); \
  hashset_to_array((a)->hs, tmp); \
  pthread_mutex_unlock(&((a)->lock)); \
  if (tmpc > 0) { (*b) = tmp[0]; } \
  for (unsigned int _ck_i = 0; \
      _ck_i < tmpc; \
      _ck_i++, (*b) = tmp[_ck_i])                                                                   
                  
///////////////////////////////////////////////////////////////////////////////
// CK PR section
bool
ck_pr_cas_ptr(void *target, void *old_value, void *new_value);

bool
ck_pr_cas_int(int *target, int old_value, int new_value);

bool
ck_pr_cas_8(uint8_t *target, uint8_t old_value, uint8_t new_value);


void
ck_pr_add_ptr(void *target, uintptr_t delta);

void
ck_pr_add_int(int *target, int delta);

void
ck_pr_add_8(uint8_t *target, uint8_t delta);

void *
ck_pr_load_ptr(const void *target);

int
ck_pr_load_int(const int *target);

uint8_t
ck_pr_load_8(const uint8_t *target);

void ck_pr_store_ptr(void *target, void *value);
#endif                          /* CYCLONE_CK_POLYFILL_H */
