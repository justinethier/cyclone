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
  Cyc_check_port(data, port);
  if (((port_type *)port)->mem_buf == NULL) {
    Cyc_rt_raise2(data, "Not an in-memory port", port);
  }
  {
    port_type *p = (port_type *)port;
    make_string_with_len(s, p->mem_buf, p->mem_buf_len);
    return_closcall1(data, cont, &s);
  }
}

// TODO: when closing a memory port, need to free mem_buf if != NULL


// Hacky approach to a minimal string buffer implementation. This may change in the future
// For background see: http://stackoverflow.com/questions/539537/memory-buffer-as-file
//port_type Cyc_io_open_output_string(void *data)
//
//// TODO: no, this is too hacky. fuck it, just use fmemopen or open_memstream
//// for non-supported platforms we'll raise an error and not do anything
//{
//    FILE *f = fopen("/dev/null", "w");
//    int i;
//    int written = 0;
//    char *buf = malloc(100000);
//    make_port(p, NULL, 0); // TODO: probably wrong params, need to add buf to port obj and type
//    //setbuffer(f, buf, 100000);
//    if (0 != setvbuf (f, buf, _IOFBF, 100000)){
//      // TODO: raise() instead?
//      fprintf(stderr, "Unable to setvbuf!\n");
//      exit(1);
//    }
//    for (i = 0; i < 1000; i++) {
//        written += fprintf(f, "Number %d\n", i);
//    }
//    for (i = 0; i < written; i++) {
//        printf("%c", buf[i]);
//    }
//    return p;
//}
//
//// END string buffers

