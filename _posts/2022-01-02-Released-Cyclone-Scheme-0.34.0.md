---
layout: post
title: Released Cyclone Scheme 0.34.0
excerpt: Allow for more complete sandboxing when bootstrapping the compiler, and various bug fixes for libraries.
---

Features

- Separate include/library search directory options from "normal" compiler/linker options and place options passed via the `-COPT`/`-CLNK` command-line flags in-between. This allows overwriting the default search paths, since contrary to all other options, the search paths must be prepend for an `-I`/`-L` option to take precedence over an existing one.

Bug Fixes

- Prevent segmentation faults in the runtime when setting a global variable to itself.
- Do not throw an error when exporting a primitive that is not defined in the current module, as built-ins are always available in any context.

