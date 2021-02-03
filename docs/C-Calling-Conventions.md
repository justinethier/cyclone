# Undefined Behavior calling Variadic C Functions

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

This works fine to store closures. The problem is that because these function pointers do not precisely match the function definitions we run into undefined behavior when attempting to call them. On native platforms this is fine. But web assembly platforms will throw an exception when calling variadic functions.

For example when calling a pointer to the following function we get a "function mismatch" error:

    static void __lambda_687(void *data, int argc, closure _,object k_735958, 
                             object init_731083_733418, object o_731084_733419_raw, ...) {

This can be resolved by using the correct function pointer in the call:

    typedef void (*function_type_test) (void *, int, closure, object, object, object, ...);

This is problematic for a few reasons:

- The caller needs to know which cast to use. We can resolve that by extending the closure type and using a wrapper to call, however
- The number of arguments in the pointer will differ depending on the variadic function we are wrapping. This creates the same problem we attempt to solve using `dispatch.c`. We can make this work for up to N arguments by using the same approach but that is clumsy at best.

Ultimately this is not a viable solution. We need another way to avoid undefined behavior in the generated C code before we can get Cyclone programs running in web assembly.

# Proposed Solution 

The ideal solution is to change the signature of our C functions to a common interface so the function pointer may be called correctly in all cases.

This calls back to [an earlier discussion](https://github.com/justinethier/cyclone/issues/193) on Cyclone's limit of ~128 function arguments:

> CHICKEN scheme has a similar problem with regarding to the number of function arguments. They eventually solved the problem at the end of the 4.x series of releases by changing the function calling convention to always receive a vector as one of the arguments, instead of explicitly passing all arguments directly as C arguments. That works, perhaps with some overhead of vector packing/unpacking, but is a massive change. The good news is that it is mostly transparent to user code; if (cyclone foreign) is used no changes may even be required to existing applications.

We can use a similar solution to avoid undefined behavior with the upside of allowing an unlimited (within reason) number of function arguments.

The main obstacle to this is the scale of the change. We will need to specifiy a new C calling convention, modify the compiler to emit code using the new convention, and modify functions in the runtime accordingly. There may also be impacts to the FFI. It would be ideal if the existing FFI could be used without modification, we will have to see if that is reasonable. Even if `define-c` needs to use a different syntax for C functions it should be possible to modify `(cyclone foreign)` such that user code does not need to be modified.

Once this is done there will no longer be a need for `dispatch.c`. Eliminating this module will give us a nice reduction in the size of `libcyclone.a` and generated executables.

## Current Function Signatures

It is important to first document and understand how the current system works before designing a new solution. Our current function signature is as follows:

    static void __lambda_1195(void *data, int argc, object closure, object k

Where:

 * `data` is state data for the current thread
 * `argc` indicates how many arguments were sent by the caller. Generally only applicable for variadic functions.
 * `closure` is the caller's closure. Note this is ignored for global functions as closures are never applicable to them.
 * `k` is the continuation to call into next. Note this is not necessarily present; it is often placed here as a result of the compiler's CPS conversion phase.

In addition zero or more objects may be listed after that as well as an ellipsis `...` for variadic functions. For example:

    static void __lambda_1195(void *data, int argc, object self_7314094, object k_737772) ;
    static void __lambda_1189(void *data, int argc, closure _,object k_737760, object make_732525_733959, object name_732526_733960) ;
    static void __lambda_705(void *data, int argc, closure _,object k_735994, object x_731110_733428);
    static void __lambda_687(void *data, int argc, closure _,object k_735958, object init_731083_733418, object o_731084_733419_raw, ...);

Note our `define-c` FFI requires the user to specify the same calling conventions:

    (define-c %write-bytevector
      "(void *data, int argc, closure _, object k, object bv, object port, object start, object end)"
      " return_closcall1(data, k, Cyc_write_bytevector(data, bv, port, start, end));"
      "(void *data, object ptr, object bv, object port, object start, object end)"
      " return Cyc_write_bytevector(data, bv, port, start, end);")

## New Calling Convention

We want a signature similar to this:

    static void __lambda(void *data, object closure, int argc, object *args) ;

That way we can pack all the extra arguments into `args` and call all functions using a single standard interface. 

An open question is whether to pack args using a C array or a vector.

Advantages to vector:
* can allocate vectors on the heap for extremely large numbers of arguments
* vector specifies the number of arguments provided (is this any better than `argc` though?)

Disadvantages:
- larger memory footprint
- more data to pack (object header, etc)
- more overhead to unpack?

Based on this overview the plan is to use C arrays rather than Cyclone vectors to pass arguments, as arrays are more efficient.

### Packing Arguments

A stack allocated array may be used to pack arguments as part of making a function call:

    object aa[2];
    aa[0] = arg1;
    aa[1] = arg2;

### Unpacking Arguments

After a function is called the arguments may be "unpacked" using simple array references:

   arg1 = args[0];
   arg2 = args[1];

We will need to change the compiler to point to members of `args` instead of directly to arguments.

TODO: any compilications here due to assumptions by our compiler, and how it compiles identifiers?

Varargs functions still expect to receive extra arguments as part of a list. A variation of the `load_varargs` macro shall be used to convert the argument array to a list.

## Performance

A key consideration is the impact to runtime performance. We do not want this solution to significantly slow down our programs if possible.

We will need to run benchmarks to measure the impact and make adjustments as needed. This must be part of the process to ensure the solution is as performant as possible.

### Optimization of Arrays

Thoughts on possible optimizations of argument arrays:

* It is best to allocate directly on the stack instead of using `alloca`.
* Find opportunities to reuse argument arrays if possible (when can we do this?)
* Use a single argument array as part of thread data? Is that practical?

### Runtime Safety Checks

We will want to add safety checks to make sure the minimum number of arguments are passed to a function. We may want to optionally disable emitting these checks from the compiler depending on the optimization level.

## Changes to the Runtime

Non-CPS primitives do not use our function signature so they do not need to be modified as part of this change. We will probably need to clean up how `apply` and the corresponding primitive wrappers work.

Any CPS functions using the signature will need to be converted.

We can eliminate dispatch.c.

## Changes to the Compiler

`(scheme cyclone cgen)` will need to emit code for the new signature.

TODO: Are there any complications in referencing vars from `args` rather than directly as C function arguments?

## Changes to the FFI

`define-c` needs to use the new signature. **TBD if there is an efficient way to do this without also requiring a migration of existing `define-c` forms. It would be great if existing code would continue to work, thus not making this a breaking change. Perhaps the compiler can detect the old signature and generate scaffolding accordingly.**

`(cyclone foreign)` will need to be modified to generate `define-c` forms that are compatible with the new signatures.

# Development Plan

- Modify compiler (scheme/cyclone/cgen.sld) to generate code using the new calling conventions. Test as best we can that C code is generated properly.
- Add necessary header definitions
- Modify runtime / primitives to use calling convention. Ensure runtime compiles with these changes in place.
- Modify FFI and define-c definitions in scheme files
- Bring up the compiler in stages. Will need to use the current version of Cyclone to generate a version with the new function signatures.

