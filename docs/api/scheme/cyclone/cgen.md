---
layout: main
title: API
---

# C Code Generation Library

The `(scheme cyclone cgen)` library compiles scheme code to a Cheney-on-the-MTA C runtime.

*This library is used internally by the compiler and its API may change at any time.*

- [`mta:code-gen`](#mtacode-gen)
- [`emit`](#emit)
- [`emit*`](#emit-1)
- [`emits`](#emits)
- [`emits*`](#emits-1)
- [`emit-newline`](#emit-newline)

# mta:code-gen

    (mta:code-gen input-program program? lib-name lib-exports imported-globals globals c-headers required-libs src-file)

Convert the given input program - as pre-processed Scheme AST - into C code. This function cannot be called directly on a Scheme program, but must be called on Scheme code that has already been processed by all of the compiler's Scheme transformations, including CPS and closure conversions.

# emit

    (emit string)

`display` the string to the output port with a trailing newline.

# emit\*

    (emit* string ...)

Emit all of the given strings and add a trailing newline.

# emits

    (emits string)

`display` the string to the output port.

# emits\*

    (emits* string ...)

Call `emits` for each of the given strings.

# emit-newline

    (emit-newline)

`display` a newline to the current output port.

