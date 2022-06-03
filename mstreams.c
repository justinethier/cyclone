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

/* These macros are hardcoded here to support functions in this module. */
#define closcall1(td, clo, buf) \
if (obj_is_not_closure(clo)) { \
   Cyc_apply(td, clo, 1, buf ); \
} else { \
   ((clo)->fn)(td, clo, 1, buf); \
;\
}
#define return_closcall1(td, clo,a1) { \
 char top; \
 object buf[1]; buf[0] = a1;\
 if (stack_overflow(&top, (((gc_thread_data *)data)->stack_limit))) { \
     GC(td, clo, buf, 1); \
     return; \
 } else {\
     closcall1(td, (closure) (clo), buf); \
     return;\
 } \
}

int Cyc_have_mstreams()
{
#if CYC_HAVE_FMEMOPEN && CYC_HAVE_OPEN_MEMSTREAM
  return 1;
#else
  return 0;
#endif
}

object Cyc_heap_alloc_port(void *data, port_type *p);
port_type *Cyc_io_open_input_string(void *data, object str)
{
  // Allocate port on the heap so the location of mem_buf does not change
  port_type *p;
  make_input_port(sp, NULL, CYC_IO_BUF_LEN);

  Cyc_check_str(data, str);
  p = (port_type *)Cyc_heap_alloc_port(data, &sp);
  errno = 0;
#if CYC_HAVE_FMEMOPEN
  p->str_bv_in_mem_buf = malloc(sizeof(char) * (string_len(str) + 1));
  p->str_bv_in_mem_buf_len = string_len(str);
  memcpy(p->str_bv_in_mem_buf, string_str(str), string_len(str));
  p->fp = fmemopen(p->str_bv_in_mem_buf, string_len(str), "r");
#endif
  if (p->fp == NULL){
    Cyc_rt_raise2(data, "Unable to open input memory stream", obj_int2obj(errno));
  }
  return p;
}

port_type *Cyc_io_open_input_bytevector(void *data, object bv)
{
  // Allocate port on the heap so the location of mem_buf does not change
  port_type *p;
  make_input_port(sp, NULL, CYC_IO_BUF_LEN);

  Cyc_check_bvec(data, bv);
  p = (port_type *)Cyc_heap_alloc_port(data, &sp);
  errno = 0;
#if CYC_HAVE_FMEMOPEN
  p->str_bv_in_mem_buf = malloc(sizeof(char) * ((bytevector)bv)->len);
  p->str_bv_in_mem_buf_len = ((bytevector)bv)->len;
  memcpy(p->str_bv_in_mem_buf, ((bytevector)bv)->data, ((bytevector)bv)->len);
  p->fp = fmemopen(p->str_bv_in_mem_buf, ((bytevector)bv)->len, "r");
#endif
  if (p->fp == NULL){
    Cyc_rt_raise2(data, "Unable to open input memory stream", obj_int2obj(errno));
  }
  return p;
}

port_type *Cyc_io_open_output_string(void *data)
{
  // Allocate port on the heap so the location of mem_buf does not change
  port_type *p;
  make_port(sp, NULL, 0);
  p = (port_type *)Cyc_heap_alloc_port(data, &sp);
  errno = 0;
#if CYC_HAVE_OPEN_MEMSTREAM
  p->fp = open_memstream(&(p->str_bv_in_mem_buf), &(p->str_bv_in_mem_buf_len));
#endif
  if (p->fp == NULL){
    Cyc_rt_raise2(data, "Unable to open output memory stream", obj_int2obj(errno));
  }
  return p;
}

void Cyc_io_get_output_string(void *data, object cont, object port)
{
  port_type *p = (port_type *)port;
  Cyc_check_port(data, port);
  if (p->fp) {
    fflush(p->fp);
  }
  if (p->str_bv_in_mem_buf == NULL) {
    Cyc_rt_raise2(data, "Not an in-memory port", port);
  }
  {
    make_string_with_len(s, p->str_bv_in_mem_buf, p->str_bv_in_mem_buf_len);
    s.num_cp = Cyc_utf8_count_code_points((uint8_t *)string_str(&s));
    return_closcall1(data, cont, &s);
  }
}

void Cyc_io_get_output_bytevector(void *data, object cont, object port)
{
  port_type *p = (port_type *)port;
  Cyc_check_port(data, port);
  if (p->fp) {
    fflush(p->fp);
  }
  if (p->str_bv_in_mem_buf == NULL) {
    Cyc_rt_raise2(data, "Not an in-memory port", port);
  }
  {
    object bv;
    alloc_bytevector(data, bv, p->str_bv_in_mem_buf_len);
    memcpy(((bytevector)bv)->data, p->str_bv_in_mem_buf, p->str_bv_in_mem_buf_len);
    return_closcall1(data, cont, bv);
  }
}

