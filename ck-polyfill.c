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
#include <ck_pr.h>
#include <unistd.h>

// TODO: global pthread mutex lock for this? obviously not ideal but the
// whole purpose of this module is a minimal interface for compatibility
// not speed

bool
ck_pr_cas_int(int *target, int old_value, int new_value)
{
  if (*target == old_value) {
    *target = new_value;
    return true;
  }
  return false;
}
