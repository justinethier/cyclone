# R<sup>7</sup>RS Compliance

This is the status of Scheme programming language features implemented from the [R<sup>7</sup>RS Scheme Specification](r7rs.pdf):

Section | Status | Comments
------- | ------ | ---------
2.2 Whitespace and comments | Yes | 
2.3 Other notations | Yes | 
2.4 Datum labels | No |
3.1 Variables, syntactic keywords, and regions | Yes |
3.2 Disjointness of types | Yes |
3.3 External representations | Yes |
3.4 Storage model | Yes |
3.5 Proper tail recursion | Yes |
4.1 Primitive expression types | Yes |
4.2 Derived expression types | Yes | 
4.2.1 Conditionals | Yes | 
4.2.2 Binding constructs | Yes |
4.2.3 Sequencing | Yes | 
4.2.4 Iteration | Yes |
4.2.5 Delayed evaluation | Yes |
4.2.6 Dynamic bindings | Yes | 
4.2.7 Exception handling | Yes |
4.2.8 Quasiquotation | Yes |
4.2.9 Case-lambda | Yes |
4.3 Macros | Yes | Support for `syntax-rules` and a lower-level explicit renaming macro system.
5.1 Programs | Yes |
5.2 Import declarations | Yes |
5.3 Variable definitions | Yes | 
5.4 Syntax definitions | Yes |
5.5 Record-type definitions | Yes | 
5.6 Libraries | Yes |
5.7 The REPL | Yes |
6.1 Equivalence predicates | Yes |
6.2 Numbers | Partial | Integers (fixnums and bignums), reals, and complex numbers are supported.
6.3 Booleans | Yes |
6.4 Pairs and lists | Yes |
6.5 Symbols | Yes |
6.6 Characters | Yes |
6.7 Strings | Yes |
6.8 Vectors | Yes |
6.9 Bytevectors | Yes | 
6.10 Control features | Yes | `dynamic-wind` is limited, and does not work across calls to continuations.
6.11 Exceptions | Yes | 
6.12 Environments and evaluation | Partial | Only `eval` is implemented at this time.
6.13 Input and output | Partial | Functions do not differentiate between binary and textual ports.
6.14 System interface | Yes | 

