---
layout: post
title: Released Cyclone Scheme 0.14
excerpt: Cyclone now automatically relocates stack objects when performing a mutation, preventing a whole range of race conditions in multithreaded application code.
---

Cyclone now automatically relocates any stack objects when performing a mutation. This prevents a whole range of race conditions that had previously been possible in multithreaded application code. And since this work is done by the Cyclone runtime no special code needs to be added to your applications.

Special thanks to Daniel Mendler, whose discussions were the inspiration for these changes.

Some background: 

There was a long-standing issue where a mutation (via `set-car!`, `vector-set!`, `set!`, etc) could allow a global object on the heap to reference objects on a thread's local stack. This is problematic because threads periodically relocate objects from their stack, and for performance reasons these objects are moved without any coordination between threads. Thus it is critical that objects on the stack are only used by the thread that owns them.

In the past we provided functions such as `make-shared` that could be called from application code to guarantee safety. However, this approach is error-prone and asks too much of anyone using Cyclone for multithreaded development. The proper solution is for Cyclone to avoid this situation in the first place.

Other Features

 - Added `CYC_HIGH_RES_TIMERS` to the runtime code to allow logging of timer information for the GC. Note this can be passed to the C compiler via the `-D` option.
