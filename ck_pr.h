#ifndef CYCLONE_CK_POLYFILL_H
#define CYCLONE_CK_POLYFILL_H

#include <stdbool.h>
#include <stdint.h>

  // toDO: ck_array support
  ck_array_iterator_t iterator;
  // TODO:

// Can we safely lock the array, make a copy, and interate over that????
#define CK_ARRAY_FOREACH(a, i, b)       \                                        
  (i)->snapshot = ck_pr_load_ptr(&(a)->active); \                                
  ck_pr_fence_load();       \                                                    
  for (unsigned int _ck_i = 0;        \                                          
      _ck_i < (a)->active->n_committed &&   \                                    
      ((*b) = (a)->active->values[_ck_i], 1); \                                  
      _ck_i++)                                                                   
                  

bool
ck_pr_cas_ptr(void *target, void *old_value, void *new_value);

bool
ck_pr_cas_int(int *target, int old_value, int new_value);

bool
ck_pr_cas_8(uint8_t *target, uint8_t old_value, uint8_t new_value);

#endif                          /* CYCLONE_CK_POLYFILL_H */
