#include "cyclone.h"

// //#ifdef __x86_64__
// //# define C_AMD64_ABI_WEIRDNESS      , ...
// //#else
// //# define C_AMD64_ABI_WEIRDNESS
// //#endif
// typedef void (*Cyc_proc0) ( int, object);
// typedef void (*Cyc_proc1) ( int, object, object);
// //typedef void (*Cyc_proc2) ( object, object, object);
// 
// /* C_WORD_p<P>_<B>: List of ((2 ** P) * B) 'C_word' parameters */
// #define C_WORD_p0_0
// #define C_WORD_p1_0
// #define C_WORD_p2_0
// #define C_WORD_p3_0
// #define C_WORD_p4_0
// #define C_WORD_p5_0
// #define C_WORD_p6_0
// #define C_WORD_p7_0
// #define C_WORD_p0_1     object, //C_word,
// #define C_WORD_p1_1     object, object, //C_word, C_word,
// #define C_WORD_p2_1     C_WORD_p1_1 C_WORD_p1_1
// #define C_WORD_p3_1     C_WORD_p2_1 C_WORD_p2_1
// #define C_WORD_p4_1     C_WORD_p3_1 C_WORD_p3_1
// #define C_WORD_p5_1     C_WORD_p4_1 C_WORD_p4_1
// #define C_WORD_p6_1     C_WORD_p5_1 C_WORD_p5_1
// #define C_WORD_p7_1     C_WORD_p6_1 C_WORD_p6_1
// 
// /* Declaration macro from CHICKEN scheme */
// /* DECL_C_PROC_p0 (n0,  p7,p6,p5,p4,p3,p2,p1,p0):
//  *  declare function C_proc<n0>, which have <n0> 'C_word' parameters
//  *  (not counting last 'C_word C_AMD64_ABI_WEIRDNESS' one).
//  *  We must have:   n0 = SUM (i = 7 to 0, p<i> * (1 << i)).
//  * DECL_C_PROC_p<N+1> (...):
//  *  declare 2 as much functions as DECL_C_PROC_p<N>...
//  */
// #define DECL_C_PROC_p0( n0,  p7,p6,p5,p4,p3,p2,p1,p0) \
//     typedef void (*Cyc_proc##n0) (int, C_WORD_p7_##p7 C_WORD_p6_##p6 \
//                                         C_WORD_p5_##p5 C_WORD_p4_##p4 \
//                                         C_WORD_p3_##p3 C_WORD_p2_##p2 \
//                                         C_WORD_p1_##p1 C_WORD_p0_##p0 \
//                                         object);
//                                         //C_word C_AMD64_ABI_WEIRDNESS) ; //C_noret;
// #define DECL_C_PROC_p1( n0,n1,  p7,p6,p5,p4,p3,p2,p1) \
//         DECL_C_PROC_p0 (n0,  p7,p6,p5,p4,p3,p2,p1,0) \
//         DECL_C_PROC_p0 (n1,  p7,p6,p5,p4,p3,p2,p1,1)
// #define DECL_C_PROC_p2( n0,n1,n2,n3,  p7,p6,p5,p4,p3,p2) \
//         DECL_C_PROC_p1 (n0,n1,  p7,p6,p5,p4,p3,p2,0) \
//         DECL_C_PROC_p1 (n2,n3,  p7,p6,p5,p4,p3,p2,1)
// #define DECL_C_PROC_p3( n0,n1,n2,n3,n4,n5,n6,n7,  p7,p6,p5,p4,p3) \
//         DECL_C_PROC_p2 (n0,n1,n2,n3,  p7,p6,p5,p4,p3,0) \
//         DECL_C_PROC_p2 (n4,n5,n6,n7,  p7,p6,p5,p4,p3,1)
// 
// DECL_C_PROC_p1 (2,3,  0,0,0,0,0,0,1)
// DECL_C_PROC_p2 (4,5,6,7,  0,0,0,0,0,1)
// DECL_C_PROC_p3 (8,9,10,11,12,13,14,15,    0,0,0,0,1)
// DECL_C_PROC_p3 (16,17,18,19,20,21,22,23,  0,0,0,1,0)
// DECL_C_PROC_p3 (24,25,26,27,28,29,30,31,  0,0,0,1,1)
// DECL_C_PROC_p3 (32,33,34,35,36,37,38,39,  0,0,1,0,0)
// DECL_C_PROC_p3 (40,41,42,43,44,45,46,47,  0,0,1,0,1)
// DECL_C_PROC_p3 (48,49,50,51,52,53,54,55,  0,0,1,1,0)
// DECL_C_PROC_p3 (56,57,58,59,60,61,62,63,  0,0,1,1,1)
// DECL_C_PROC_p1 (64,65,  0,1,0,0,0,0,0)
// DECL_C_PROC_p0 (66,  0,1,0,0,0,0,1,0)
// DECL_C_PROC_p0 (67,  0,1,0,0,0,0,1,1)
// DECL_C_PROC_p2 (68,69,70,71,  0,1,0,0,0,1)
// DECL_C_PROC_p3 (72,73,74,75,76,77,78,79,  0,1,0,0,1)
// DECL_C_PROC_p3 (80,81,82,83,84,85,86,87,  0,1,0,1,0)
// DECL_C_PROC_p3 (88,89,90,91,92,93,94,95,  0,1,0,1,1)
// DECL_C_PROC_p3 (96,97,98,99,100,101,102,103,  0,1,1,0,0)
// DECL_C_PROC_p3 (104,105,106,107,108,109,110,111,  0,1,1,0,1)
// DECL_C_PROC_p3 (112,113,114,115,116,117,118,119,  0,1,1,1,0)
// DECL_C_PROC_p3 (120,121,122,123,124,125,126,127,  0,1,1,1,1)
// DECL_C_PROC_p0 (128,  1,0,0,0,0,0,0,0)

/**
 * Take list of args and call a function with them as params.
 *
 * Note memory scheme below is not compatible with GC, 
 * so macro calls funcs directly.
 */

// TODO: get VA args function below to work

/**
 * Receive a list of arguments and apply them to the given function
 */
void dispatch(int argc, function_type func, object clo, object cont, object args) {
  object b[argc + 1]; // OK to do this? Is this portable?
  int i; 

  argc++;
  b[0] = cont;
  for (i = 1; i < argc; i++){ 
    b[i] = car(args); 
    args = cdr(args); 
  } 

  do_dispatch(argc, func, clo, b);
}

/**
 * Same as above but for a varargs C function
 */
void dispatch_va(int argc, function_type_va func, object clo, object cont, object args) {
  object b[argc + 1]; // OK to do this? Is this portable?
  int i; 
 
  argc++;
  b[0] = cont;
  for (i = 1; i < argc; i++){ 
    b[i] = car(args); 
    args = cdr(args); 
  } 

  do_dispatch(argc, (function_type)func, clo, b);
}

void do_dispatch(int argc, function_type func, object clo, object *b) {
/* The following is based on the macro expansion code from CHICKEN's do_apply */

/* PTR_O_p<P>_<B>(o): list of COUNT = ((2 ** P) * B) '*(b+I)' arguments,
 * with offset I in range [o, o+COUNT-1].
 */
#define PTR_O_p0_0(o)
#define PTR_O_p1_0(o)
#define PTR_O_p2_0(o)
#define PTR_O_p3_0(o)
#define PTR_O_p4_0(o)
#define PTR_O_p5_0(o)
#define PTR_O_p6_0(o)
#define PTR_O_p7_0(o)
#define PTR_O_p0_1(o)   , *(b+(o))
#define PTR_O_p1_1(o)   , *(b+(o)), *(b+(o+1))
#define PTR_O_p2_1(o)   PTR_O_p1_1(o) PTR_O_p1_1(o+2)
#define PTR_O_p3_1(o)   PTR_O_p2_1(o) PTR_O_p2_1(o+4)
#define PTR_O_p4_1(o)   PTR_O_p3_1(o) PTR_O_p3_1(o+8)
#define PTR_O_p5_1(o)   PTR_O_p4_1(o) PTR_O_p4_1(o+16)
#define PTR_O_p6_1(o)   PTR_O_p5_1(o) PTR_O_p5_1(o+32)
#define PTR_O_p7_1(o)   PTR_O_p6_1(o) PTR_O_p6_1(o+64)

/* CASE_C_PROC_p0 (n0,  p6,p5,p4,p3,p2,p1,p0):
 *  let's note <N> = <n0> - 2; the macro inserts:
 *      case <N>: ((C_cproc<n0>)pr) (<n0>, fn, k, <rest>);
 *  where <rest> is:    *(b+1), ..., *(b+<N>)
 *  (<rest> is empty for <n0> == 2).
 *  We must have:   n0 = SUM (i = 7 to 0, p<i> * (1 << i)).
 * CASE_C_PROC_p<N+1> (...):
 *  like CASE_C_PROC_p<N>, but with doubled output...
 */
//    case (n0-2): (Cyc_proc##n0)func(n0-2, clo 
#define CASE_C_PROC_p0(n0,  p6,p5,p4,p3,p2,p1,p0) \
    case (n0-2): func(n0-2, clo \
PTR_O_p6_##p6(((n0-2)&0x80)+0)\
PTR_O_p5_##p5(((n0-2)&0xC0)+0)\
PTR_O_p4_##p4(((n0-2)&0xE0)+0)\
PTR_O_p3_##p3(((n0-2)&0xF0)+0)\
PTR_O_p2_##p2(((n0-2)&0xF8)+0)\
PTR_O_p1_##p1(((n0-2)&0xFC)+0)\
PTR_O_p0_##p0(((n0-2)&0xFE)+0));
#define CASE_C_PROC_p1( n0,n1,  p6,p5,p4,p3,p2,p1) \
        CASE_C_PROC_p0 (n0,  p6,p5,p4,p3,p2,p1,0) \
        CASE_C_PROC_p0 (n1,  p6,p5,p4,p3,p2,p1,1)
#define CASE_C_PROC_p2( n0,n1,n2,n3,  p6,p5,p4,p3,p2) \
        CASE_C_PROC_p1 (n0,n1,  p6,p5,p4,p3,p2,0) \
        CASE_C_PROC_p1 (n2,n3,  p6,p5,p4,p3,p2,1)
#define CASE_C_PROC_p3( n0,n1,n2,n3,n4,n5,n6,n7,  p6,p5,p4,p3) \
        CASE_C_PROC_p2 (n0,n1,n2,n3,  p6,p5,p4,p3,0) \
        CASE_C_PROC_p2 (n4,n5,n6,n7,  p6,p5,p4,p3,1)
  switch(argc) {
    CASE_C_PROC_p3 (2,3,4,5,6,7,8,9,  0,0,0,0)
    CASE_C_PROC_p3 (10,11,12,13,14,15,16,17,  0,0,0,1)
    CASE_C_PROC_p3 (18,19,20,21,22,23,24,25,  0,0,1,0)
    CASE_C_PROC_p3 (26,27,28,29,30,31,32,33,  0,0,1,1)
    CASE_C_PROC_p3 (34,35,36,37,38,39,40,41,  0,1,0,0)
    CASE_C_PROC_p3 (42,43,44,45,46,47,48,49,  0,1,0,1)
    CASE_C_PROC_p3 (50,51,52,53,54,55,56,57,  0,1,1,0)
    CASE_C_PROC_p3 (58,59,60,61,62,63,64,65,  0,1,1,1)
    CASE_C_PROC_p0 (66,  1,0,0,0,0,0,0)
    CASE_C_PROC_p0 (67,  1,0,0,0,0,0,1)
    CASE_C_PROC_p1 (68,69,  1,0,0,0,0,1)
    CASE_C_PROC_p2 (70,71,72,73,  1,0,0,0,1)
    CASE_C_PROC_p3 (74,75,76,77,78,79,80,81,  1,0,0,1)
    CASE_C_PROC_p3 (82,83,84,85,86,87,88,89,  1,0,1,0)
    CASE_C_PROC_p3 (90,91,92,93,94,95,96,97,  1,0,1,1)
    CASE_C_PROC_p3 (98,99,100,101,102,103,104,105,  1,1,0,0)
    CASE_C_PROC_p3 (106,107,108,109,110,111,112,113,  1,1,0,1)
    CASE_C_PROC_p3 (114,115,116,117,118,119,120,121,  1,1,1,0)
    CASE_C_PROC_p2 (122,123,124,125,  1,1,1,1,0)
    CASE_C_PROC_p1 (126,127,  1,1,1,1,1,0)
    CASE_C_PROC_p0 (128,  1,1,1,1,1,1,0)
  default: //barf(C_TOO_MANY_PARAMETERS_ERROR, "apply");
   // TODO: not good enough, throw an exception instead, using (error)
   printf("Unhandled number of function arguments: %d\n", argc); 
   exit(1);
  }
}

