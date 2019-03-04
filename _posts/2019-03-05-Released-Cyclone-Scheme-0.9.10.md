---
layout: post
title: Released Cyclone Scheme 0.9.10
excerpt: This release improves the performance of record types and vectors.
---

Features

- Faster initialization of objects create via `define-record-type`.
- Generate faster compiled code for calls to `vector` that contain less than five arguments.
