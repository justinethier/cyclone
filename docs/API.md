
# R<sup>7</sup>RS Libraries

This section of the Cyclone API is based on the [R<sup>7</sup>RS Scheme Specification](r7rs.pdf):

- Built-in primitives
- [`scheme base`](api/scheme/base.md)
- [`scheme case-lambda`](api/scheme/case-lambda.md)
- [`scheme char`](api/scheme/char.md)
- [`scheme cxr`](api/scheme/cxr.md)
- [`scheme eval`](api/scheme/eval.md)
- [`scheme file`](api/scheme/file.md)
- [`scheme inexact`](api/scheme/inexact.md)
- [`scheme lazy`](api/scheme/lazy.md)
- [`scheme load`](api/scheme/load.md)
- [`scheme process-context`](api/scheme/process-context.md)
- [`scheme read`](api/scheme/read.md)
- [`scheme time`](api/scheme/time.md)
- [`scheme write`](api/scheme/write.md)

# SRFI Library Support

Cyclone supports the following [Scheme Requests for Implementation (SRFI)](http://srfi.schemers.org/) libraries. Detailed information is available in the linked SRFI page as well as the provided Cyclone API:

- [`srfi 1`](api/srfi/1.md) - [List Library](http://srfi.schemers.org/srfi-1/srfi-1.html)
- [`srfi 2`](api/srfi/2.md) - [`and-let*`](http://srfi.schemers.org/srfi-2/srfi-2.html)
- [`srfi 8`](api/srfi/8.md) - [`receive`: Binding to multiple values](http://srfi.schemers.org/srfi-8/srfi-8.html) - Included as part of `scheme base`.
- [`srfi 18`](api/srfi/18.md) - [Multithreading support](http://srfi.schemers.org/srfi-18/srfi-18.html)
- [`srfi 27`](api/srfi/27.md) - [Sources of random bits](http://srfi.schemers.org/srfi-27/srfi-27.html)
- [`srfi 69`](api/srfi/69.md) - [Basic hash tables](http://srfi.schemers.org/srfi-69/srfi-69.html)
- [`srfi 111`](api/srfi/111.md) - [Boxes](http://srfi.schemers.org/srfi-111/srfi-111.html)
- [`srfi 117`](api/srfi/117.md) - [Mutable queues](http://srfi.schemers.org/srfi-117/srfi-117.html)
- [`srfi 132`](api/srfi/132.md) - [Sort Libraries](http://srfi.schemers.org/srfi-132/srfi-132.html)
- [`srfi 133`](api/srfi/133.md) - [Vector Library (R7RS-compatible)](http://srfi.schemers.org/srfi-133/srfi-133.html)

# Cyclone-specific

These libraries are used by the Cyclone compiler itself, and are subject to change:

- `scheme cyclone cgen`
- `scheme cyclone common`
- `scheme cyclone libraries`
- `scheme cyclone macros`
- `scheme cyclone transforms`
- `scheme cyclone util`
