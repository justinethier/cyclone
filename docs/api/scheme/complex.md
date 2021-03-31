---
layout: main
title: API
---

# Complex Library

The `(scheme complex)` library exports procedures which are typically only useful with non-real numbers.

For more information see the [R<sup>7</sup>RS Scheme Specification](../../r7rs.pdf).

- [`angle`](#angle)
- [`imag-part`](#imag-part)
- [`magnitude`](#magnitude)
- [`make-polar`](#make-polar)
- [`make-rectangular`](#make-rectangular)
- [`real-part`](#real-part)

# angle

    (angle z)

# imag-part

    (imag-part x)

Return the imaginary part of complex number `x`.

# magnitude

    (magnitude z)

# make-polar

    (make-polar r phi)

Return a complex number corresponding to the given polar coordinate.

# make-rectangular

    (make-rectangular x y)

Create a complex number with real component `x` and imaginary component `y`.

# real-part

    (real-part x)

Return the real part of complex number `x`.
