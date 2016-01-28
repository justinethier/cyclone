# R<sup>7</sup>RS Compliance

This is the status of Scheme programming language features implemented from the [R<sup>7</sup>RS Scheme Specification](r7rs.pdf):

Section | Status | Comments
------- | ------ | ---------
2.2 Whitespace and comments | Partial | No datum or block comments
2.3 Other notations | Partial |  Not all of these notations are implemented
2.4 Datum labels | No |
3.1 Variables, syntactic keywords, and regions | Yes |
3.2 Disjointness of types | Yes |
3.3 External representations | Yes |
3.4 Storage model | Yes | No immutable types at this time.
3.5 Proper tail recursion | Yes |
4.1 Primitive expression types | Partial | `include` and `include-ci` are not implemented, although `include` may be specified as part of a library definition.
4.2 Derived expression types | Partial | TODO: this is a large section. need to review support because most of this is included.
4.3 Macros | Partial | No support for `syntax-rules` but a lower-level explicit renaming macro system is implemented.
5.1 Programs | Yes |
5.2 Import declarations | Partial |
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
6.7 Strings | Partial | No unicode support, `string-ci` functions are not implemented.
6.8 Vectors | Yes |
6.9 Bytevectors | | Not supported yet.
6.10 Control features | Partial | The `map` and `for-each` families of functions only support one "data" argument - for example, `string-map` only accepts one string. `dynamic-wind` is limited, and does not work across calls to continuations.
6.11 Exceptions | Partial | Exceptions are implemented but error objects (and associated functions `error-object`, etc) are not at this time. 
6.12 Environments and evaluation | Partial | Only `eval` is implemented at this time.
6.13 Input and output | Partial | Functions do not differentiate between binary and textual ports. Do not have support for input/output strings or bytevectors.
6.14 System interface | Yes | 

