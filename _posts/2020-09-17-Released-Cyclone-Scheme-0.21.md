---
layout: post
title: Released Cyclone Scheme 0.21
excerpt: Various bug fixes and continuous integration support for FreeBSD.
---

Features

- Alex Arslan modified the Travis script to add support for FreeBSD continuous integration.
- Added additional functions to `(srfi 132)`: `vector-find-median!`, `vector-find-median`, `vector-select!`, `vector-select`, and `vector-separate!`.

Bug Fixes

- Updated the `Dockerfile` to use a non-interactive install. This prevents build problems with the official image on DockerHub.
- Improved `(scheme lazy)` to allow `force` and `make-promise` to accept an argument of any type. Improved representation of promises to more precisely differentiate them from other objects.
- Updated `(scheme lazy)` such that `force` will recursively force promises.
- Add type checking to record type accessor functions. We now raise an error if the passed object is of the wrong record type.
- Fix issues with expanding `cond-expand` expressions in libraries. Previously there would be issues with the expansion if the code needed to be within the context of a `begin`.
- Modified the reader to handle escaped intraline whitespace properly, per R7RS.
