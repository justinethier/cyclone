---
layout: post
title: Released Cyclone Scheme 0.9.1
excerpt: This release fixes an intermittent crash affecting `read` and other I/O functions.
---

Bug Fixes

- Fixed a nasty bug where, while a mutator is blocked after calling `set_thread_blocked`, the collector may copy an object the mutator is using from the stack to the heap. In this case we need to ensure the object is not corrupted when it is copied and that we sync the object's fields back up once the mutator is unblocked. Currently this only affects port objects in the runtime. Generally `read` was affected more than other I/O functions.
- Fixed a handful of small garbage collection bugs from the previous round of development.
