---
layout: post
title: Released Cyclone Scheme 0.6
excerpt: This release provides significant speed improvements to `read` and includes the SRFI 143 fixnum library for higher performance integer operations.
---

Features

- Added a faster version of `read`.
- Added SRFI 143 - Fixnums.

Bug Fixes

- Prevent `remainder` from crashing the runtime due to divide by zero.
- Avoid printing an unnecessary colon after certain error messages.
