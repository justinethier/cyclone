[<img src="images/cyclone-logo-04-header.png" alt="cyclone-scheme">](http://github.com/justinethier/cyclone)

# User Manual

- [Introduction](#introduction)
- [Requirements](#requirements)
- [Installation](#installation)
- [Usage](#usage)
  - [Compiling Scheme Programs](#compiling-scheme-programs)
  - [Compiling Scheme Libraries](#compiling-scheme-libraries)
  - [Command Line Options](#command-line-options)
  - [Generated Files](#generated-files)
  - [Interpreter](#interpreter)
- [Language Details](#language-details)
- [Multithreaded Programming](#multithreaded-programming)
- [Foreign Function Interface](#foreign-function-interface)
  - [Writing a Scheme Function in C](#writing-a-scheme-function-in-c)
  - [Foreign Library](#foreign-library)
  - [Including a C Header File](#including-a-c-header-file)
  - [Linking to a C Library](#linking-to-a-c-library)
  - [Calling Scheme Functions from C](#calling-scheme-functions-from-c)
- [Licensing](#licensing)
- [References and Further Reading](#references-and-further-reading)


# Introduction
Cyclone is a brand-new Scheme-to-C compiler with the goal of supporting real-world application development using the R<sup>7</sup>RS Scheme Language standard. We provide modern features and a stable system capable of generating fast native binaries.

A variant of the [Cheney on the MTA](https://github.com/justinethier/cyclone/raw/master/docs/research-papers/CheneyMTA.pdf) technique is used to implement full tail recursion, continuations, generational garbage collection, and native threads. This is the same technique proposed by Henry Baker (Cheney on the MTA) and implemented first by CHICKEN Scheme. The difference is that our compiler allows multiple native threads, each with their own stack. A tracing garbage collector is used to manage the second-generation heap and perform major collections without "stopping the world".

Cyclone is developed by [Justin Ethier](https://github.com/justinethier). 

Bug reports and patches are welcome! Please report any issues using the [Issues Page](https://github.com/justinethier/cyclone/issues).

# Requirements

Cyclone has been tested under Linux on the x86, x86-64, and ARM platforms.

The following packages are required:

- make
- gcc
- [Concurrency Kit](http://concurrencykit.org/)

  NOTE: The best way to install `libck` is via a package manager such as `apt-get`. But if a package is not available for this library it can also be built from source. Just replace `0.6.0` below with the latest version available from their website:
    
        wget http://concurrencykit.org/releases/ck-0.6.0.tar.gz
        tar xfz ck-0.6.0.tar.gz ; cd ck-0.6.0 ; ./configure PREFIX=/usr && make all && sudo make install
        sudo ldconfig

On a Debian variant such as Ubuntu the necessary packages may be installed via the command:

    sudo apt-get install libck-dev make gcc

The following command can be used to install dependencies on Fedora, though `libck` will also need to be built from source:

    sudo yum install gcc make
    
# Installation

Cyclone cannot be built directly on a system that does not have Cyclone binaries installed because the compiler is [self-hosting](https://en.wikipedia.org/wiki/Self-hosting). The easiest way to install Cyclone binaries is to build from source using [cyclone-bootstrap](https://github.com/justinethier/cyclone-bootstrap):

    $ git clone https://github.com/justinethier/cyclone-bootstrap.git
    $ cd cyclone-bootstrap
    $ ./install.sh

Once Cyclone is installed, it can be rebuilt directly from the cyclone repository:

    $ make
    $ make test
    $ sudo make install

# Usage

## Compiling Scheme Programs

A Scheme program may be compiled using the `cyclone` command:

    $ cyclone  examples/fac.scm
    $ examples/fac
    3628800

## Compiling Scheme Libraries

Scheme code can be organized into libraries that are compiled separately from programs. Cyclone intends a library to represent a single C module (or file) when compiled.

Each library must be placed into a `.sld` file that corresponds to the library name. For example, the library 

    (scheme cyclone util) 

would be defined in its `.sld` file as:

    (define-library (scheme cyclone util)
      ... )

and should be located in the file

    scheme/cyclone/util.sld

Cyclone will automatically generate libraries when compiling a program. For example:

    $ cd cyclone/examples/game-of-life
    $ cyclone life.scm
    $ ./life

## Command Line Options

`cyclone` has the following command line options:

Option              | Notes
------------------- | -----
`-A directory`      | Append directory to the list of directories that are searched in order to locate imported libraries.
`-I directory`      | Prepend directory to the list of directories that are searched in order to locate imported libraries.
`-CP cc-commands`   | Specify a custom command line for the C compiler to compile a program module. See `Makefile.config` for an example of how to construct such a command line.
`-CE cc-commands`   | Specify a custom command line for the C compiler to compile an executable.
`-CL cc-commands`   | Specify a custom command line for the C compiler to compile a library module.
`-CS cc-commands`   | Specify a custom command line for the C compiler to compile a shared object module.
`-CLNK option`      | Specify a custom command to provide as a linker option, EG: "-lcurl".
`-Ox`               | Optimization level, higher means more optimizations will be used. Set to 0 to disable optimizations.
`-d`                | Only generate intermediate C files, do not compile them. This option will also show the C compiler commands that would have been used to compile the C file.
`-t`                | Show intermediate trace output in generated C files
`-h, --help`        | Display usage information
`-v`                | Display version information
`-vn`               | Display version number
`-batch`            | Automatically compile local library dependencies (enabled by default).
`-no-batch`         | Compile as a single unit, do not attempt to compile local library dependencies.
`-use-unsafe-prims` | Emit unsafe primitives. These primitives are faster but do not perform runtime type checking or bounds checking.
`-no-call-history`  | Do not track call history in the compiled code. This allows for a faster runtime at the cost of having no call history in the event of an exception.

## Generated Files

The following files are generated during the Cyclone compilation process:

File Extension | Installation Required | Notes
-------------- | ---------------- | -----
`.meta` | :heavy_check_mark: | These text files contain the expanded version of any macros exported by a Scheme library, and allow other modules to easily use those macros during compilation. This file is not generated when compiling a program.
`.c` | | C code file generated by Cyclone.
`.o` | :heavy_check_mark: | Object file generated by the C compiler from the corresponding `.c` file.
`.so` | :heavy_check_mark: | Shared Object files generated by the C compiler from the corresponding `.c` file. These are only generated for Scheme libraries and are used to allow loading a library at runtime.
(None) | :heavy_check_mark: | Final executable file generated by the C compiler when compiling a program.

When installing a library, all of the files indicated above must be installed as well as the corresponding `.sld` file.

## Interpreter

Scheme code can be evaluated interactively using the `icyc` command:

        $ icyc
        cyclone> (write 'hello-world)
        hello-world

# Language Details

Cyclone implements the Scheme language as documented by the [R<sup>7</sup>RS Scheme Specification](r7rs.pdf). 

A [R<sup>7</sup>RS Compliance Chart](Scheme-Language-Compliance.md) lists differences between the specification and Cyclone's implementation.

[API Documentation](API.md) is available for the libraries provided by Cyclone.

# Multithreaded Programming

## Overview

The [`srfi 18`](api/srfi/18.md) library may be imported to provide support for multithreaded programs. See the [SRFI 18 specification](http://srfi.schemers.org/srfi-18/srfi-18.html) for more background information.

Many helper functions are provided by [`(cyclone concurrent)`](api/cyclone/concurrent.md) to make it easier to write multithreaded programs.

## Memory Layout

Cyclone's garbage collector moves objects in memory from the first generation (on the stack) to the second generation (on the heap). This causes problems when an object is used by multiple threads, as the address another thread expects to find an object at may suddenly change. To prevent race conditions an object must be guaranteed to be on the heap prior to being used by another thread. 

The easiest way to meet this guarantee is to use one of the `make-shared` and `share-all!` functions from `(cyclone concurrent)`. Several of the other constructs from that library (such as futures and shared queues) use these functions internally to guarantee objects are safely shared between threads.

Finally, note there are some objects that are not relocated so the above does not apply:

- Characters and integers are stored using value types and do not need to be garbage collected.
- Symbols are stored in a global table rather than the stack/heap.
- Mutexes, Atomics, and other concurrency-oriented objects are always allocated on the heap since by definition they are used by more than one thread.

# Foreign Function Interface

## Writing a Scheme Function in C

The `define-c` special form can be used to define a function containing user-defined C code. This code will be carried through from the Scheme file all the way to the compiled C file. For example:

     (define-c Cyc-add-exception-handler
       "(void *data, int argc, closure _, object k, object h)"
       " gc_thread_data *thd = (gc_thread_data *)data;
         make_pair(c, h, thd->exception_handler_stack);
         thd->exception_handler_stack = &c;
         return_closcall1(data, k, &c); ")

The arguments to `define-c` are:

- Function name. These functions can be used in Scheme code just like any other function.
- A string with C function arguments, enclosed in parentheses. Some of these are required:
  - `data` - Internal thread data. Do not modify this unless you know what you are doing!
  - `argc` - Number of function arguments.
  - A closure, typically unused in this context.
  - `k` - Current continuation, typically the code will call into `k` with a result.
- A string containing the C function body. Remember that runtime functions are not allowed to return. In the example above, the `return_closcall1` macro is used to "return" a newly-allocated list to the current continuation.

Functions that may block must call the `set_thread_blocked` macro to let the system know the thread may block. After the blocking section is finished, the `return_thread_runnable` macro must be called to recover from the blocking operation and call into the current continuation. For example:

    object Cyc_mutex_lock(void *data, object cont, object obj) {
      mutex m = (mutex) obj;
      Cyc_check_mutex(data, obj);
      set_thread_blocked(data, cont);
      if (pthread_mutex_lock(&(m->lock)) != 0) {
        fprintf(stderr, "Error locking mutex\n");
        exit(1);
      }
      return_thread_runnable(data, boolean_t);
    }

The Cyclone runtime can be used as a reference for how to write your own C functions. A good starting point would be [`runtime.c`](../runtime.c) and [`types.h`](../include/cyclone/types.h).

## Foreign Library

The [`(cyclone foreign)`](api/cyclone/foreign.md) provides a convenient interface for integrating with C code, and provides higher-level constructs than `define-c`. See the API documentation for more information.

## Including a C Header File

A C header may be included using the `include-c-header` special form. This special form may be used either as part of a library definition:

    (define-library (example life)
      (include-c-header "../write-png.h")
      (export life)
      ... )

Or as part of a program (add any includes immediately after the `import` expression, if one is present):

    (import (scheme base)
            (example life)
            (example grid))
    (include-c-header "stdlib.h")
    (include-c-header "<stdio.h>")

By default this will generate an `#include` preprocessor directive with the name of the header file in double quotes. However, if `include-c-header` is passed a text string with angle brackets (EG: `"<stdio.h>"`), the generated C code will use angle brackets instead.

## Linking to a C Library

A Cyclone library may use the `c-linker-options` expression to instruct the compiler to include linker options when building an executable. For example:

    (define-library (cyclone curl)
      (include-c-header "<curl/curl.h>")
      (export curl-version)
      (c-linker-options "-lcurl")

## Calling Scheme Functions from C

C functions `Cyc_scm_call` and `Cyc_scm_call_no_gc` are provided to allow calling Scheme functions from C. 

The best way to get started with these functions is with the code examples in the Cyclone source tree under [`examples/call-scm-from-c`](../examples/call-scm-from-c).

# Licensing
Cyclone is available under the [MIT license](http://www.opensource.org/licenses/mit-license.php).

# References and Further Reading

- [CONS Should Not CONS Its Arguments, Part II: Cheney on the M.T.A.](https://github.com/justinethier/cyclone/raw/master/docs/research-papers/CheneyMTA.pdf), by Henry Baker
- [CHICKEN Scheme](http://www.call-cc.org/)
- [Chibi Scheme](https://github.com/ashinn/chibi-scheme)
- [Compiling Scheme to C with closure conversion](http://matt.might.net/articles/compiling-scheme-to-c/), by Matt Might
- Implementing an on-the-fly garbage collector for Java, by Domani et al
- [Lisp in Small Pieces](http://pagesperso-systeme.lip6.fr/Christian.Queinnec/WWW/LiSP.html), by Christian Queinnec
- Portable, Unobtrusive Garbage Collection for Multiprocessor Systems, by Damien Doligez and Georges Gonthier
- [R<sup>5</sup>RS Scheme Specification](http://www.schemers.org/Documents/Standards/R5RS/HTML/)
- [R<sup>7</sup>RS Scheme Specification](http://trac.sacrideo.us/wg/wiki)
- [Structure and Interpretation of Computer Programs](https://mitpress.mit.edu/sicp/full-text/book/book.html), by Harold Abelson and Gerald Jay Sussman
- [The 90 minute Scheme to C compiler](http://churchturing.org/y/90-min-scc.pdf), by Marc Feeley

