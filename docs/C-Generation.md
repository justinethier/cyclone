Some notes:

# Data Structures

The compiled code is stored in a list that contains:

- A string that contains the actual generated code.
- A list of strings that represent all of the local C variables that are necessary to support the generated code

For example:

    cyclone> (c-compile-prim '+ "")
    ("c_732" ("object c_732 = Cyc_sum(data,"))

# Compiled Code

TODO: explain each type of code:

    ((const? exp)       (c-compile-const exp))
    ((prim?  exp)       
    ((ref?   exp)       (c-compile-ref exp))
    ((quote? exp)       (c-compile-quote exp))
    ((if? exp)          (c-compile-if exp append-preamble cont trace cps?))
    ((tagged-list? '%closure exp)
      c-compile-closure
    ((define? exp)
      c-compile-global
    ((define-c? exp)
      c-compile-raw-global-lambda
    ((tagged-list? 'lambda exp)
    ((app? exp)         (c-compile-app exp append-preamble cont trace cps?))


# TODO: full code example?

full example of compile code, maybe fac.scm or another simple program
