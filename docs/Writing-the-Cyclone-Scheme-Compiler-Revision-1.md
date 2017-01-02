# Writing the Cyclone Scheme Compiler (Revision 1)

###### by [Justin Ethier](https://github.com/justinethier)

This is a revision of the [original write-up](Writing-the-Cyclone-Scheme-Compiler.md), written over a year ago in August 2015, when the compiler was self hosting but before the new garbage collector was written. This version includes everything that has happened since then, and attempts to provide a constructive background on how Cyclone was written.

Before we get started, I want to give a big **Thank You** to everyone that has contributed to the Scheme community. Cyclone is based on the community's latest revision of the Scheme language and wherever possible existing code from the community was reused or repurposed for this project, instead of starting from scratch. At the end of this document is a list of helpful online resources. Without high quality Scheme resources like these the Cyclone project would not have been possible.

In addition, developing [Husk Scheme](http://justinethier.github.io/husk-scheme) helped me gather much of the knowledge that would later be used to create Cyclone. In fact the primary motivation in building Cyclone was to go a step further and understand how to build a full, free-standing Scheme system. At this point Cyclone has eclipsed the speed and functionality of Husk and it is not clear if Husk will receive much more than bug fixes going forward. Maybe if there is greater interest from the community some of this work can be ported back to that project.

## Table of Contents

- [Overview](#overview)
- [Source-to-Source Transformations](#source-to-source-transformations)
- [C Code Generation](#c-code-generation)
- [Garbage Collector](#garbage-collector)
- [C Runtime](#c-runtime)
- [Data Types](#data-types)
- [Interpreter](#interpreter)
- [Macros](#macros)
- [Scheme Standards](#scheme-standards)
- [Future](#future)
- [Conclusion](#conclusion)
- [References](#references)

## Overview

Cyclone has a similar architecture to other modern compilers:

<img src="images/compiler.png" alt="flowchart of cyclone compiler">

First, an input file containing Scheme code is received on the command line and loaded into an abstract syntax tree (AST) by Cyclone's parser. From there a series of source-to-source transformations are performed on the AST to expand macros, perform optimizations, and make the code easier to compile to C. These intermediate representations (IR) can be printed out in a readable format to aid debugging. The final AST is then output as a `.c` file and the C compiler is invoked to create the final executable or object file.

The code is represented internally as an AST of regular Scheme objects.  Since Scheme represents both code and data using [S-expressions](https://en.wikipedia.org/wiki/S-expression), our compiler does not (in general) have to use custom abstract data types to store the code as would be the case with many other languages.

## Source-to-Source Transformations

### Overview

My primary inspiration for Cyclone was Marc Feeley's [The 90 minute Scheme to C compiler](http://churchturing.org/y/90-min-scc.pdf) (also [video](https://www.youtube.com/watch?v=TxOM9Y5YrCs) and [code](https://github.com/justinethier/nugget/tree/master/90-min-scc)). Over the course of 90 minutes, Feeley demonstrates how to compile Scheme to C code using source-to-source transformations, including closure and continuation-passing-style (CPS) conversions. 

As outlined in the presentation, some of the difficulties in compiling to C are:

> Scheme has, and C does not have
>
>  -  tail-calls a.k.a. tail-recursion optimization
>  -  first-class continuations
>  -  closures of indefinite extent
>  -  automatic memory management i.e. garbage collection (GC)
>
> Implications
>
>  -  cannot translate (all) Scheme calls into C calls
>  -  have to implement continuations
>  -  have to implement closures
>  -  have to organize things to allow GC
>
> The rest is easy!

To overcome these difficulties a series of source-to-source transformations are used to remove powerful features not provided by C, add constructs required by the C code, and restructure/relabel the code in preparation for generating C. The final code may be compiled direcly to C. Cyclone also includes many other intermediate transformations, including:

- Macro expansion
- Processing of globals
- [Alpha conversion](https://wiki.haskell.org/Alpha_conversion)
- [CPS conversion](https://en.wikipedia.org/wiki/Continuation-passing_style)
- [Closure conversion](http://matt.might.net/articles/closure-conversion/)

The 90-minute compiler ultimately compiles the code down to a single function and uses jumps to support continuations. This is a bit too limiting for a production compiler, so that part was not used.

### The Basic Pattern - Make Many Small Passes

Most of the transformations follow a similar pattern. A single function is used to recursively examine all of the code's AST, examining each piece of code a single time. This is efficient as long as code is only visited a single time.

This is a simple example, although instead of doing a straightforward transformation it calls `mark-mutable` to track mutations for a later phase:

    (define (analyze-mutable-variables exp)
      (cond 
        ((const? exp)    (void))
        ((prim? exp)     (void))
        ((ref? exp)      (void))
        ((quote? exp)    (void))
        ((lambda? exp)   
         (map analyze-mutable-variables (lambda->exp exp))
         (void))
        ((set!? exp)     
         (mark-mutable (set!->var exp))
         (analyze-mutable-variables (set!->exp exp)))
        ((if? exp)       
         (analyze-mutable-variables (if->condition exp))
         (analyze-mutable-variables (if->then exp))
         (analyze-mutable-variables (if->else exp)))
        ((app? exp)
         (map analyze-mutable-variables exp)
         (void))
        (else
         (error "unknown expression type: " exp))))

### Macro Expansion

Macro expansion is one of the first transformations. Any macros the compiler knows about are loaded as functions into a macro environment, and a single pass is made over the code. When the compiler finds a macro the code is expanded by calling the macro. The compiler then inspects the resulting code again in case the macro expanded into another macro.

At the lowest level, [explicit renaming](http://wiki.call-cc.org/explicit-renaming-macros) (ER) macros provide a simple, low-level macro system without requiring much more than `eval`. Many ER macros from [Chibi Scheme](https://github.com/ashinn/chibi-scheme) are used to implement the built-in macros in Cyclone.

Cyclone also supports the high-level `syntax-rules` system from the Scheme reports. Syntax rules is implemented as a huge ER macro ported from Chibi Scheme.

As a simple example the `let` macro below:

    (let ((square (lambda (x) (* x x))))
      (write (+ (square 10) 1)))

is expanded to:

    (((lambda (square) (write (+ (square 10) 1)))
      (lambda (x) (* x x))))

### CPS Conversion

The conversion to continuation passing style (CPS) makes continuations explicit in the compiled code. This is a critical step to make the Scheme code simple enough that it can be represented by C. As we will see later, the runtime's garbage collector also requires code in CPS form.

The basic idea is that each expression will produce a value that is consumed by the continuation of the expression. Continuations will be represented using functions. All of the code must be rewritten to accept a new continuation parameter `k` that will be called with the result of the expression. For example, considering the previous `let` example:

    (((lambda (square) (write (+ (square 10) 1)))
      (lambda (x) (* x x))))
    
the code in CPS form becomes:

    ((lambda (r)
       ((lambda (square)
          (square
            (lambda (r)
              ((lambda (r) (write r))
               (+ r 1)))
            10))
        r))
     (lambda (k x) (k (* x x))))

### CPS Optimizations

CPS conversion generates too much code and is inefficient for functions such as primitives that can return a result directly instead of calling into a continuation. So we need to optimize it to make the compiler practical. For example, the previous CPS code can be simplified to:

    ((lambda (k x) (k (* x x)))
      (lambda (r)
        (write (+ r 1)))
      10)

One of the most effective optimizations is inlining of primitives. That is, some runtime functions can be called directly, so an enclosing `lambda` is not needed to evaluate them. This can greatly reduce the amount of generated code.

There is also a contraction phase that eliminates other unnecessary `lambda`'s. There are a few other miscellaneous optimizations such as constant folding, which evaluates certain primitives at compile time if the parameters are constants.

To more efficiently identify optimizations an analysis pass is made over the code to build up a "database" of various attributes that determine which optimizations can be performed. The DB is a hash table of records with an entry for each variable and function. This idea was borrowed from CHICKEN.

In order to support the analysis DB a custom AST is used to represent functions during this phase, so that each one can be tagged with a unique identification number.

## C Code Generation

The compiler's code generation phase takes a single pass over the transformed Scheme code and outputs C code to the current output port (usually a `.c` file).

During this phase C code is sometimes saved for later use instead of being output directly. For example, when compiling a vector literal or a series of function arguments, the code is returned as a list of strings that separates variable declarations from C code in the "body" of the generated function.

The C code is carefully generated so that a Scheme library (`.sld` file) is compiled into a C module. Functions and variables exported from the library become C globals in the generated code.

## Garbage Collector

### Background: Cheney on the MTA
A runtime based on Henry Baker's paper [CONS Should Not CONS Its Arguments, Part II: Cheney on the M.T.A.](http://www.pipeline.com/~hbaker1/CheneyMTA.html) was used as it allows for fast code that meets all of the fundamental requirements for a Scheme runtime: tail calls, garbage collection, and continuations.

Baker explains how it works:

> We propose to compile Scheme by converting it into continuation-passing style (CPS), and then compile the resulting lambda expressions into individual C functions. Arguments are passed as normal C arguments, and function calls are normal C calls. Continuation closures and closure environments are passed as extra C arguments. Such a Scheme never executes a C return, so the stack will grow and grow ... eventually, the C "stack" will overflow the space assigned to it, and we must perform garbage collection. 

Cheney on the M.T.A. uses a copying garbage collector. By using static roots and the current continuation closure, the GC is able to copy objects from the stack to a pre-allocated heap without having to know the format of C stack frames. To quote Baker:

> the entire C "stack" is effectively the youngest generation in a generational garbage collector!

After GC is finished, the C stack pointer is reset using [`longjmp`](http://man7.org/linux/man-pages/man3/longjmp.3.html) and the GC calls its continuation. 

Here is a snippet demonstrating how C functions may be written using Baker's approach:

    object Cyc_make_vector(object cont, object len, object fill) {
      object v = NULL;
      int i;
      Cyc_check_int(len);

      // Memory for vector can be allocated directly on the stack
      v = alloca(sizeof(vector_type));

      // Populate vector object
      ((vector)v)->tag = vector_tag;
      ... 

      // Check if GC is needed, then call into continuation with the new vector
      return_closcall1(cont, v);
    }

[CHICKEN](http://www.call-cc.org/) was the first Scheme compiler to use Baker's approach.

### Cyclone's Hybrid Collector 

Baker's technique uses a copying collector for both the minor and major generations of collection. One of the drawbacks of using a copying collector for major GC is that it relocates all the live objects during collection. This is problematic for supporting native threads because an object can be relocated at any time, invalidating any references to the object. To prevent this either all threads must be stopped while major GC is running or a read barrier must be used each time an object is accessed. Both options add a potentially significant overhead so instead Cyclone uses another type of collector for the second generation.

Cyclone supports native threads by using a tri-color tracing collector based on the Doligez-Leroy-Gonthier (DLG) algorithm for major collections. Each thread contains its own stack that is collected using Cheney on the MTA during minor GC. Each object that survives a minor collection is copied from the stack to a newly-allocated slot on the heap. An advantage of this approach is that objects are not relocated once they are placed on the heap. In addition, major GC executes asynchronously so threads can continue to run concurrently even during collections.

More details are available in a separate [Garbage Collector](Garbage-collector.md) document.

TODO:
<img src="images/cyclone-contribs.png">

### Native Thread Support

Cyclone attempts to support multithreading in an efficient way that minimizes the amount of synchronization among threads. But objects are still copied a single time during minor GC. In order for an object to be shared among threads the application must guarantee that the object is no longer on the stack. This can be done by using synchronization primitives (such as a mutex) to coordinate access. It is also possible to initiate a minor GC for the calling thread to guarantee an object will henceforth not be relocated.

### Data Structures

TODO: code from Chibi scheme
TODO: not really related to this paper, but can allocation speedup for Cyclone be ported back to Chibi? Should look into that

## C Runtime
TODO: anything else to say about the C runtime???

yes, find that paper from Dybvig about writing Chez scheme, about how they made the runtime nice and fast. same applies here as well

here it is:
https://www.cs.indiana.edu/~dyb/pubs/hocs.pdf

The "money" quote is:

> My focus was instead on low-level details, like choosing efficient representations and generating good instruction sequences, and the compiler did include a peephole optimizer. High-level optimization is important, and we did plenty of that later, but low-level details often have more leverage in the sense that they typically affect a broader class of programs, if not all programs.

## Data Types

### Objects

Most Scheme data types are represented as allocated "objects" that contain a tag to identify the object type. For example:

    typedef struct {tag_type tag; double value;} double_type;

### Value Types

On the other hand, some data types can be represented using 30 bits or less and can be stored as value types using a technique from Lisp in Small Pieces. On many machines, addresses are multiples of four, leaving the two least significant bits free. [A brief explanation](http://stackoverflow.com/q/9272526/101258):

> The reason why most pointers are aligned to at least 4 bytes is that most pointers are pointers to objects or basic types that themselves are aligned to at least 4 bytes. Things that have 4 byte alignment include (for most systems): int, float, bool (yes, really), any pointer type, and any basic type their size or larger.

Due to the tag field, all Cyclone objects will have (at least) 4-byte alignment.

Cyclone uses this technique to store characters. The nice thing about value types is they do not have to be garbage collected because no extra data is allocated for them. 

## Interpreter

The [Metacircular Evaluator](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-26.html#%_sec_4.1) from [SICP](https://mitpress.mit.edu/sicp/full-text/book/book.html) was used as a starting point for `eval`.

TODO: explain analysis phase, and how this is a nice speedup

## Scheme Standards

Cyclone targets the [R<sup>7</sup>RS-small specification](https://github.com/justinethier/cyclone/raw/master/docs/r7rs.pdf). This spec is relatively new and provides incremental improvements from the popular [R<sup>5</sup>RS spec](http://www.schemers.org/Documents/Standards/R5RS/HTML/). Library (C module) support is the most important but there are also exceptions, system interfaces, and a more consistent API.

## Future

- Implement more of r7rs-large, have started on data structures
- implement more libraries (industria??)
- way to support eggs or other libraries? is that even worth the effort?
- benchmark

Andrew Appel used a similar runtime for [Standard ML of New Jersey](http://www.smlnj.org/) which is referenced by Baker's paper. Appel's book [Compiling with Continuations](http://www.amazon.com/Compiling-Continuations-Andrew-W-Appel/dp/052103311X) includes a section on how to implement compiler optimizations - many of which could be applied to Cyclone.

## Conclusion

TODO: this section is completely out of date, a better reference would be benchmark results from r7rs-benchmarks

From Feeley's presentation:

> Performance is not so bad with NO optimizations (about 6 times slower than Gambit-C with full optimization)

Compared to a similar compiler (CHICKEN), Cyclone's performance is worse but also "not so bad":

     $ time cyclone -d transforms.sld
    
    real    0m6.802s
    user    0m4.444s
    sys     0m1.512s

    $ time csc -t transforms.scm
    
    real    0m1.084s
    user    0m0.512s
    sys     0m0.380s

Thanks for reading!

Want to give Cyclone a try? Install a copy using [cyclone-bootstrap](https://github.com/justinethier/cyclone-bootstrap).


## References

- [CONS Should Not CONS Its Arguments, Part II: Cheney on the M.T.A.](http://www.pipeline.com/~hbaker1/CheneyMTA.html), by Henry Baker
- [CHICKEN Scheme](http://www.call-cc.org/)
- [Chibi Scheme](https://github.com/ashinn/chibi-scheme)
- [Compiling Scheme to C with closure conversion](http://matt.might.net/articles/compiling-scheme-to-c/), by Matt Might
- [Lisp in Small Pieces](http://pagesperso-systeme.lip6.fr/Christian.Queinnec/WWW/LiSP.html), by Christian Queinnec
- [R<sup>5</sup>RS Scheme Specification](http://www.schemers.org/Documents/Standards/R5RS/HTML/)
- [R<sup>7</sup>RS Scheme Specification](http://trac.sacrideo.us/wg/wiki)
- [Structure and Interpretation of Computer Programs](https://mitpress.mit.edu/sicp/full-text/book/book.html), by Harold Abelson and Gerald Jay Sussman
- [The 90 minute Scheme to C compiler](http://churchturing.org/y/90-min-scc.pdf), by Marc Feeley
