---
layout: post
title: Released Cyclone Scheme 0.7
excerpt: This is the first Cyclone release with Unicode support.
---

Features

- Finally added Unicode support using UTF-8!
- Allow a program to have macros expand into a top-level `import` expression.
- Added continuous integration support thanks to Alex Arslan.

Bug Fixes

- Incorporated a patch from [0-8-15](https://github.com/0-8-15) to pass seconds to `thread-sleep!` instead of milliseconds. Fractional seconds are accepted as well for high-resolution timers.

