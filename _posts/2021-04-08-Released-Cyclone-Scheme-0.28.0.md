---
layout: post
title: Released Cyclone Scheme 0.28.0
excerpt: Allow an unlimited number of function arguments and provide friendly alternative names for SRFI libraries
---

Features

- Updated the compiler and runtime to allow a practically unlimited number of function arguments. 

  Although the calling conventions of our generated C code and runtime functions were changed, there is no impact to application developers. Existing code will continue to work without requiring modifications. This includes code using our FFI, though it may be necessary to update `define-c` definitions if there are unused parameters in order to prevent warnings from the C compiler. For example by refactoring to use the new calling conventions: 
      
      (define-c read-error
        "(void *data, object _, int argc, object *args)"
        " object port = args[1];
          object filename = args[2];
          object msg = args[3];
          ...

  No changes are a required if `(cyclone foreign)` is used to integrate with C.

- Provided alternative library names for each of the SRFI libraries. Generally these names follow the recommendations from R7RS Large - for example `(scheme list)` as a friendlier alternative to `(srfi 1)`. Where such a name does not exist we provide a name with the `(cyclone)` prefix:

  Library Name                            | SRFI Number | Description | External Documentation
  --------------------------------------- | ----------- | ----------- | ----------------------
  [`scheme list`](api/srfi/1.md)          | [`srfi 1`](api/srfi/1.md)     | List library | [Link](http://srfi.schemers.org/srfi-1/srfi-1.html)
  [`cyclone and-let*`](api/srfi/2.md)     | [`srfi 2`](api/srfi/2.md)     | `AND-LET*`: an `AND` with local bindings, a guarded `LET*` special form | [Link](http://srfi.schemers.org/srfi-2/srfi-2.html)
   N/A                                    | [`srfi 8`](api/srfi/8.md)     | Binding to multiple values - Included as part of `scheme base`. | [Link](http://srfi.schemers.org/srfi-8/srfi-8.html)
  [`cyclone threads`](api/srfi/18.md)     | [`srfi 18`](api/srfi/18.md)   | Multithreading support | [Link](http://srfi.schemers.org/srfi-18/srfi-18.html)
  [`cyclone random`](api/srfi/27.md)      | [`srfi 27`](api/srfi/27.md)   | Sources of random bits | [Link](http://srfi.schemers.org/srfi-27/srfi-27.html)
  [`cyclone format`](api/srfi/28.md)      | [`srfi 28`](api/srfi/28.md)   | Basic format strings | [Link](http://srfi.schemers.org/srfi-28/srfi-28.html)
  [`cyclone integer-bits`](api/srfi/60.md)| [`srfi 60`](api/srfi/60.md)   | Integers as bits | [Link](http://srfi.schemers.org/srfi-60/srfi-60.html)
  [`scheme hash-table`](api/srfi/69.md)   | [`srfi 69`](api/srfi/69.md)   | Basic hash tables | [Link](http://srfi.schemers.org/srfi-69/srfi-69.html)
  [`cyclone socket`](api/srfi/106.md)     | [`srfi 106`](api/srfi/106.md) | Basic socket interface | [Link](http://srfi.schemers.org/srfi-106/srfi-106.html)
  [`scheme box`](api/srfi/111.md)         | [`srfi 111`](api/srfi/111.md) | Boxes | [Link](http://srfi.schemers.org/srfi-111/srfi-111.html)
  [`scheme set`](api/srfi/113.md)         | [`srfi 113`](api/srfi/113.md) | Sets and bags | [Link](http://srfi.schemers.org/srfi-113/srfi-113.html)
  [`scheme list-queue`](api/srfi/117.md)  | [`srfi 117`](api/srfi/117.md) | Mutable queues | [Link](http://srfi.schemers.org/srfi-117/srfi-117.html)
  [`scheme generator`](api/srfi/121.md)   | [`srfi 121`](api/srfi/121.md) | Generators | [Link](http://srfi.schemers.org/srfi-121/srfi-121.html)
  [`scheme comparator`](api/srfi/128.md)  | [`srfi 128`](api/srfi/128.md) | Comparators | [Link](http://srfi.schemers.org/srfi-128/srfi-128.html)
  [`scheme sort`](api/srfi/132.md)        | [`srfi 132`](api/srfi/132.md) | Sort libraries | [Link](http://srfi.schemers.org/srfi-132/srfi-132.html)
  [`scheme vector`](api/srfi/133.md)      | [`srfi 133`](api/srfi/133.md) | Vector library (R7RS-compatible) | [Link](http://srfi.schemers.org/srfi-133/srfi-133.html)
  [`cyclone fixnum`](api/srfi/143.md)     | [`srfi 143`](api/srfi/143.md) | Fixnums | [Link](http://srfi.schemers.org/srfi-143/srfi-143.html)

- We are modifying version numbers going forward to use explicit three digit semantic versioning `major.minor.bugfix`.

Bug Fixes

- Arthur Maciel replaced high resolution code in the runtime to use `clock_gettime` instead of `gettimeofday`.
- Fixed the REPL to no longer automatically exit if an expression evaluates to EOF. However, the REPL will exit as a special case if the EOF character is entered directly, for example via CTRL-D on Linux. 

