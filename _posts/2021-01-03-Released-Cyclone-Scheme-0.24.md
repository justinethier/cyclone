---
layout: post
title: Released Cyclone Scheme 0.24
excerpt: Happy New Year! This release fixes bugs related to type checking of vectors/records and prevents two segmentation faults in the runtime.
---

Bug Fixes

- Sean Lynch fixed a bug where record type predicates do not check the length of the target before checking if the vector is actually a record.
- Fixed `vector?` to no longer return true for instances of record types.
- Do not call `eval` from the runtime if `(scheme eval)` has not been imported. Instead we now raise a Scheme error in this case which prevents the possibility of a C segmentation violation. Thanks to Arthur Maciel for the bug report.
- When allocating very large vectors the object used to fill such a vector may not be transported to the heap. This was a nasty bug that could lead to random memory corruption. Fixed the minor garbage collector to properly track and transport these objects to prevent the possibility of memory corruption.
