---
layout: post
title: Released Cyclone Scheme 0.11.2
excerpt: This release provides a new concurrency library and bug fixes to SRFI 18.
---

Features

- Added a new library `(cyclone concurrent)` with support for atomics, futures, shared queues, and thread pools. As well as helpers for immutability and shared objects. Overall the goal is for this library to make it much easier to write multithreaded programs using Cyclone.
- Relocated existing libraries to `(cyclone match)` and `(cyclone test)`.
- Extended `mutex-lock!` to support an optional `timeout` parameter, per the SRFI 18 spec.

Bug Fixes

- Fixed `thread-join!` to wait properly both for running threads and new threads that have not yet been started.
