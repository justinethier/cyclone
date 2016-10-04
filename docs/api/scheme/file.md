---
layout: main
title: API
---

# File Library

The `(scheme file)` library provides procedures for accessing files.

For more information see the [R<sup>7</sup>RS Scheme Specification](../../r7rs.pdf).

- [`call-with-input-file` ](#call-with-input-file)
- [`call-with-output-file`](#call-with-output-file)
- [`with-input-from-file`](#with-input-from-file)
- [`with-output-to-file`](#with-output-to-file)

# call-with-input-file

    (call-with-input-file string proc)

Open given filename for input, pass the resulting port to `proc`, and close the port after `proc` returns.

# call-with-output-file

    (call-with-output-file string proc)

Open given filename for output, pass the resulting port to `proc`, and close the port after `proc` returns.

# with-input-from-file

    (with-input-from-file string thunk)

Open given filename for input and change the current input to that port for the duration of `thunk`.

# with-output-to-file

    (with-output-to-file string thunk)

Open given filename for output and change the current output to that port for the duration of `thunk`.
