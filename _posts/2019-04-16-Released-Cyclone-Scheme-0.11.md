---
layout: post
title: Released Cyclone Scheme 0.11
excerpt: This release includes updated build instructions for Mac as well as continued improvements to compiler performance and code validation.
---

Features

- During compilation validate the number of arguments passed to locally-defined functions.
- Improve performance of compiled code a bit by inlining code that tracks call history instead of using a dedicated function in the runtime.
- Updated build instructions for Mac, thanks Adam Feuer!

Bug Fixes

- Allow `exit` to return an integer value as the return code to the OS.
