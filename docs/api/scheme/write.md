---
layout: main
title: API
---

# Write Library

The `(scheme write)` library provides procedures for writing Scheme objects.

For more information see the [R<sup>7</sup>RS Scheme Specification](../../r7rs.pdf).

- [`display`](#display)
- [`write`](#write)
- [`write-simple`](#write-simple)
- [`write-shared`](#write-shared)

# display

    (display obj)
    (display obj port)

Write object to the given output port, or the current output if none is given. Strings and characters are output using only the characters that they represent, without any enclosing quotes, etc.

# write

    (write obj)
    (write obj port)

Write object to the given output port, or the current output if none is given. Objects are written exactly as they are represented in code.


# write-shared

    (write-shared obj)
    (write-shared obj port)

`write-shared` is the same as `write` because Cyclone does not support datum labels at this time.

# write-simple

    (write-simple obj)
    (write-simple obj port)

`write-simple` is the same as `write` because Cyclone does not support datum labels at this time.

