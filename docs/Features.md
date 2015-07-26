# Features

This page summarizes the Scheme language features implemented by Cyclone.

## R<sup>7</sup>RS Compliance

This is the status of features implemented from the [R<sup>7</sup>RS Scheme Specification](http://trac.sacrideo.us/wg/wiki):

Section | Status | Comments
------- | ------ | ---------
2.2 Whitespace and comments | Partial | No datum or block comments
2.3 Other notations | |
2.4 Datum labels | No |
3.1 Variables, syntactic keywords, and regions | |
3.2 Disjointness of types | Yes |
3.3 External representations | |
3.4 Storage model | | No immutable types at this time.
3.5 Proper tail recursion | Yes |
4.1 Primitive expression types | |
4.2 Derived expression types | |
4.3 Macros | |
5.1 Programs | Yes |
5.2 Import declarations | |
5.3 Variable definitions | Partial | `define-values` is not implemented yet.
5.4 Syntax definitions | |
5.5 Record-type definitions | |
5.6 Libraries | Partial | Support is "good enough" but need to make it more robust
5.7 The REPL | Yes |
6.1 Equivalence predicates | Yes | `eqv?` is not implemented, it is just an alias to `eq?`
6.2 Numbers | Partial | Integers and reals are supported, but most numeric functions are missing at this time.
6.3 Booleans | Yes | `#true` and `#false` are not recognized by parser.
6.4 Pairs and lists | Yes | `member` functions are predicates, `member` and `assoc` do not accept `compare` argument.
6.5 Symbols | Yes |
6.6 Characters | Partial | No unicode support, `char-ci` predicates are not implemented.
6.7 Strings | Partial | Many functions are missing. Need to sync up with r7rs.
6.8 Vectors | Yes |
6.9 Bytevectors | |
6.10 Control features | |
6.11 Exceptions | Partial | Need to check against r7rs
6.12 Environments and evaluation | Partial |
6.13 Input and output | |
6.14 System interface | |

## Cyclone-specific

Cyclone also supports several non-standard features:

- `system`
- `command-line-arguments`
- what else?
