---
layout: main
title: API
---

# Time Library

The `(scheme time)` library provides access to time-related values.

For more information see the [R<sup>7</sup>RS Scheme Specification](../../r7rs.pdf).

- [`current-jiffy`](#current-jiffy)
- [`current-second`](#current-second)
- [`jiffies-per-second`](#jiffies-per-second)

#current-jiffy

    (current-jiffy)

Get the number of jiffies since the program started. This function can return the same value if the program runs longer than approximately 72 minutes on a 32-bit platform.

#current-second

    (current-second)

Get the current number of seconds since the UNIX epoch. 

#jiffies-per-second

    (jiffies-per-second)

The number of jiffies per second.
