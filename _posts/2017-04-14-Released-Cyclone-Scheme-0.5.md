---
layout: post
title: Released Cyclone Scheme 0.5
excerpt: With interpreter support for loading libraries via `import`, `icyc` can now be used to its full potential.
---

Features

- Cyclone now has support in the interpreter for loading libraries via `import`. This is probably the most important change in this release and allows `icyc` to be used to its full potential.
- Store parameter objects in such a way that changes to a parameter object do not affect other threads that use the same parameter object.

    The specific requirement from R<sup>7</sup>RS is:

    > `parameterize` must not change the associated values of any parameters in any thread other than the current thread and threads created inside the `parameterize` body.

- Added bignum support to SRFI 60 - integers as bits.
- Normalize the result of `string->number` such that bignums are only returned if the result truly is a bignum.
- Allow Cyclone to find `(cyclone)` prefixed libraries installed in Cyclone's system folder.
- Allow a library to export identifiers that are exported by another library. Previously a library could only export identifiers that it defined directly.
- Raise an error if an unknown identifier is found in a library's `export` list.
- Aric Belsito removed a hardcoded call to `cyclone` from the Makefile when building `generate-c`. Thanks!
- Allow `cyclone` to inline character comparison functions (`char=?`, etc) when only two arguments are present, for a significant speed improvement.

Bug Fixes

- Prevent exception handlers from being accidentally collected when the collector cooperates on behalf of a mutator.
- Fixed a regression where `string->number` returns `0` on bad input instead of `#f`.
