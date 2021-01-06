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
