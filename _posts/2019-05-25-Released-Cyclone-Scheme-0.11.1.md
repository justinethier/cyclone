---
layout: post
title: Released Cyclone Scheme 0.11.1
excerpt: This release adds support for immutable objects.
---

Features

- Added support for immutable objects. Any quoted pairs, vectors, bytevectors, or strings will now be flagged as immutable, per R7RS.

Bug Fixes

- Fixed a bug where the compiler would not always validate the number of arguments passed to a locally-defined function.
