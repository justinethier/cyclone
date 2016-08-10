---
layout: post
title: Announcing the Cyclone Scheme Compiler
excerpt: Initial announcement to comp.lang.scheme
---

# {{ page.title }}

Today I would like to announce the Cyclone Scheme-to-C compiler: 

http://justinethier.github.io/cyclone/ 

Cyclone consists of a compiler and interpreter written entirely in R7RS Scheme, as well as a C runtime. A large portion of the R7RS is supported including libraries, exceptions, continuations, and macros. 

A garbage collector inspired by Cheney on the M.T.A. is used by Cyclone to support full tail recursion, continuations, and generational collection. Scheme code is compiled into a series of C functions that never return and the stack is used as the first generation of the collector. But unlike Cheney on the M.T.A., an on-the-fly garbage collector is used to manage the heap instead of a copying collector. This allows Cyclone to perform major collections for application code running across multiple OS threads, without having an explicit stop-the-world phase. 

Cyclone is still a work in progress but is already powerful enough that it could be used for real tasks. For example, Cyclone can compile itself as well as most of the R7RS Scheme benchmarks provided by Larceny. I have included links at the end of this email to various resources that may be useful for getting started with Cyclone. 

Major features include: 

- Supports majority of R7RS 
- Explicit Renaming and Syntax Rules macros 
- Basic FFI for calling into C from Scheme 
- SRFI 18 - Multithreading, using native (OS) threads 
- SRFI 27 - Sources of Random Bits 
- SRFI 69 - Basic Hash Tables 
- Pretty printing 

Cyclone is still rough around the edges though and many features are not implemented at this time, such as: 

- Complex numbers, rational numbers, and bignums 
- Unicode support 

Being a new project, there are also problems of improving error handling, ensuring common functionality between compiler/interpreter, and improving compiler optimizations.

Again, this project is very much a work in progress and feedback is appreciated. 

Thanks, 

Justin 

----------------- 
User Manual 
http://justinethier.github.io/cyclone/docs/User-Manual 

R7RS Compatibility 
http://justinethier.github.io/cyclone/docs/Scheme-Language-Compliance 

Bug Tracker 
https://github.com/justinethier/cyclone/issues 

