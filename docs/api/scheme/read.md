---
layout: main
title: API
---

# Read Library

The `(scheme read)` library provides procedures for reading Scheme objects.

For more information see the [R<sup>7</sup>RS Scheme Specification](../../r7rs.pdf).

- [`read`](#read)
- [`read-all`](#read-all)

#read

    (read)
    (read port)

Read a single Scheme object from the input port.

#read-all

    (read-all)
    (read-all port)

Read all objects from a port and return them as a list.
