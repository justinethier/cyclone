---
layout: post
title: Released Cyclone Scheme 0.5.3
excerpt: This release allows import of system libraries with arbitrary names and improves the performance of minor garbage collection.
---

Features

- On Arthur Maciel's suggestion, modified Cyclone to always check its system folder for a library if an import is not found. In conjunction with this, the `CYCLONE_LIBRARY_PATH` environment variable may be set to force Cyclone to look in a specific place other than the system folder for libraries.

- Decrease minor GC time by only tracing globals when one of them has changed.

- Added the `thread-join!` function to `(srfi 18)`.
