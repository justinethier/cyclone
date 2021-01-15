# The Problem - Undefined Behavior calling Variadic C Functions

Our closures use the following declaration to store function pointers:

    typedef void (*function_type) ();

And in the closure object:

    typedef struct {
      gc_header_type hdr;
      tag_type tag;
      function_type fn;
      int num_args;
      int num_elements;
      object *elements;
    } closureN_type;

This works fine to store closures. The problem is that because these function pointers do not precisely match the function definitions we run into undefined behavior when attempting to call them. On native platforms this is fine. But WASM platforms will throw an exception when calling variadic functions.

For example when calling a pointer to the following function we get a "function mismatch" error:

    static void __lambda_687(void *data, int argc, closure _,object k_735958, 
                             object init_731083_733418, object o_731084_733419_raw, ...) {

This can be resolved by using the correct function pointer in the call:

    typedef void (*function_type_test) (void *, int, closure, object, object, object, ...);

This is problematic for a few reasons:

- The caller needs to know which cast to use. We can resolve that by extending the closure type and using a wrapper to call, however
- The number of arguments in the pointer will differ depending on the variadic function we are wrapping. This creates the same problem we attempt to solve using `dispatch.c`. We can make this work for up to N arguments by using the same approach but that is clumsy at best.

# Proposed Solution 

The ideal solution is to change the signature of all of our C functions to a common interface so the function pointer may be called correctly in all cases.


## Current Function Signatures

Our current function signature is as follows:

    static void __lambda_1195(void *data, int argc, object closure, object k

Where:

 * `data` is state data for the current thread
 * `argc` indicates how many arguments were sent by the caller. Generally only applicable for variadic functions.
 * `closure` is the caller's closure. Note this is ignored for global functions as closures are never applicable to them.
 * `k` is the continuation

In addition zero or more objects may be listed after that as well as an ellipsis `...` for variadic functions. For example:

    static void __lambda_1195(void *data, int argc, object self_7314094, object k_737772) ;
    static void __lambda_1189(void *data, int argc, closure _,object k_737760, object make_732525_733959, object name_732526_733960) ;
    static void __lambda_705(void *data, int argc, closure _,object k_735994, object x_731110_733428);
    static void __lambda_687(void *data, int argc, closure _,object k_735958, object init_731083_733418, object o_731084_733419_raw, ...);

TODO: `define-c` examples

    (define-c %write-bytevector
      "(void *data, int argc, closure _, object k, object bv, object port, object start, object end)"
      " return_closcall1(data, k, Cyc_write_bytevector(data, bv, port, start, end));"
      "(void *data, object ptr, object bv, object port, object start, object end)"
      " return Cyc_write_bytevector(data, bv, port, start, end);")

## New Signatures

TBD


    static void __lambda(void *data, object closure, object k, int argc, object *args) ;

TODO: how to call these functions, need to pack args prior to call

TODO: how to unpack args for a call. I think it would be simple, need to change compiler to point to members of `args` instead of directly to arguments

TODO: fixed and variadic examples (same?)


An added benefit of this, I think, is that we eliminate the restriction on number of function arguments.
though, if this is on the stack there still is a limit

TBD: no more need for dispatch.c?

