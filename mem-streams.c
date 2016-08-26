/** 
 * Cyclone Scheme
 * https://github.com/justinethier/cyclone
 *
 * Copyright (c) 2014-2016, Justin Ethier
 * All rights reserved.
 *
 * This file contains platform-specific code for memory streams.
 */

#include "cyclone/types.h"
#include "cyclone/runtime.h"
#include <errno.h>
//#include <limits.h>
//#include <ctype.h>

/* These macros are hardcoded here to support functions in this module. */
#define closcall1(td, clo, a1) \
if (type_is_pair_prim(clo)) { \
   Cyc_apply(td, 0, (closure)(a1), clo); \
} else { \
   ((clo)->fn)(td, 1, clo, a1);\
}
#define return_closcall1(td, clo, a1) { \
 char top; \
 if (stack_overflow(&top, (((gc_thread_data *)data)->stack_limit))) { \
     object buf[1]; buf[0] = a1;\
     GC(td, clo, buf, 1); \
     return; \
 } else {\
     closcall1(td, (closure) (clo), a1); \
     return;\
 } \
}

port_type Cyc_io_open_output_string(void *data)
{
  make_port(p, NULL, 0);
  errno = 0;
#if CYC_HAVE_OPEN_MEMSTREAM
  p.fp = open_memstream(&(p.mem_buf), &(p.mem_buf_len));
#endif
  if (p.fp == NULL){
    Cyc_rt_raise2(data, "Unable to open memory stream", obj_int2obj(errno));
  }
  return p;
}

void Cyc_io_get_output_string(void *data, object cont, object port)
{
  port_type *p;
  Cyc_check_port(data, port);
  if (((port_type *)port)->mem_buf == NULL) {
    Cyc_rt_raise2(data, "Not an in-memory port", port);
  }
  p = (port_type *)port;
  if (p->fp) {
    fflush(p->fp);
  }
  {
    make_string_with_len(s, p->mem_buf, p->mem_buf_len);
    return_closcall1(data, cont, &s);
  }
}

