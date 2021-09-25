---
layout: post
title: Released Cyclone Scheme 0.33.0
excerpt: Allow easier macro debugging from the REPL using expand.
---

Features

- Allow easier macro debugging from the REPL by using `expand`. Passing a single expression as an argument will return the expanded expression:

      cyclone> (expand '(when #t (+ 1 2 3)))
      (if #t ((lambda () (+ 1 2 3))) )

- During compilation the compiler will now call itself as a subprocess to perform Scheme-to-C compilation. This allows Cyclone to free all of those resources before calling the C compiler to generate a binary, resulting in more efficient compilation.

Bug Fixes

- Do not inline calls to `system` as it could result in multiple calls of the same command.
