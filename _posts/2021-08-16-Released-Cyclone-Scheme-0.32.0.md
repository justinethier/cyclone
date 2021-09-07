---
layout: post
title: Released Cyclone Scheme 0.32.0
excerpt: Added support for Alpine Linux and made several improvements to the garbage collector.
---

Features

- Initiate major garbage collections faster after allocating a huge object (larger than 500K). This allows the system to reclaim the memory faster and keep overall memory usage low for certain workloads.
- Cyclone will no longer memoize pure functions by default.
- Added build option `CYC_PTHREAD_SET_STACK_SIZE` to allow Cyclone to specify a thread stack size rather than using the OS default. EG:

      make CYC_PTHREAD_SET_STACK_SIZE=1 libcyclone.a

Bug Fixes

- @nmeum fixed `(scheme repl)` to flush the output port prior to writing the prompt, guaranteeing the prompt is written at the correct time. 
- Fixed `fxbit-set?` to properly handle negative values of `i`.
- Avoid unnecessary renaming of identifiers when the interpreter performs macro expansion.
- When allocating a large vector we now guarantee all vector elements are initialized before the major collector can trace those elements. This avoids the potential for a race condition which could lead to a segmentation fault.
- Ensure atomic objects are properly traced by the major garbage collector.

