---
layout: post
title: Released Cyclone Scheme 0.5.1
excerpt: The main focus of `0.5.1` is performance but this is also the first release to provide installation instructions for both Windows and Mac.
---

Features

- Thanks to Kashyap for adding build instructions for Windows (MSYS) and Mac!

- Allow `define-c` function definitions to optionally provide an additional non-CPS form of the function. This form is more efficient and will be used by compiled code whenever possible.

- Improved the compiler's CPS optimization phase to eliminate more unnecessary function calls. 

- Modified the GC to allow a given number of "huge" allocations to trigger GC. Previously GC was only triggered when smaller heap regions were below a certain percentage of free memory.

- Compiled code now directly accesses boxed mutable variables instead of using a wrapper function.

- Added command line options to `icyc` for evaluating an S-expression from the command line and for running as a script without the Cyclone banner text.

Bug Fixes

- Prevent potential memory corruption when working with large vectors that cannot be allocated on the stack.
