TODO: list of features, table of RxRS features (??), etc

R<sup>7</sup>RS Compliance

Section | Status | Comments
------- | ------ | ---------
2.2 Whitespace and comments | |
2.3 Other notations | |
2.4 Datum labels | |
3.1 Variables, syntactic keywords, and regions | |
3.2 Disjointness of types | |
3.3 External representations | |
3.4 Storage model | | No immutable types at this time.
3.5 Proper tail recursion | Yes |
4.1 Primitive expression types | |
4.2 Derived expression types | |
4.3 Macros | |
5.1 Programs | |
5.2 Import declarations | |
5.3 Variable definitions | |
5.4 Syntax definitions | |
5.5 Record-type definitions | |
5.6 Libraries | |
5.7 The REPL | Yes |
6.1 Equivalence predicates | | `eqv?` is not implemented, it is just an alias to `eq?`
6.2 Numbers | |
6.3 Booleans | Yes | `#true` and `#false` are not recognized by parser.
6.4 Pairs and lists | Yes | `member` functions are predicates, `member` and `assoc` do not accept `compare` argument.
6.5 Symbols | Yes |
6.6 Characters | Partial | No unicode support, `char-ci` predicates are not implemented.
6.7 Strings | |
6.8 Vectors | Yes |
6.9 Bytevectors | |
6.10 Control features | |
6.11 Exceptions | |
6.12 Environments and evaluation | |
6.13 Input and output | |
6.14 System interface | |
