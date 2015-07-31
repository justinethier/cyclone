
table of contents

- [References](#References)

## TODO

All of the references listed below were used to some extent to write Cyclone.

mention husk-scheme

## Scheme source-to-source transformations
One of the most important inspiration's for Cyclone was Marc Feeley's [The 90 minute Scheme to C compiler]() which includes a presentation, set of slides, and example code written in Gambit Scheme. Feeley includes an overview of how to compile Scheme to C code using source-to-source transformations, including closure and continuation-passing-style CPS conversions. The 90-minute scc ultimately compiles the code down to a single function and uses jumps to support continuations. This is a bit too limiting for a production compiler, so that part was not used.

## C Runtime
Henry Baker's paper [CONS Should Not CONS Its Arguments: Cheney on the M.T.A.](http://www.pipeline.com/~hbaker1/CheneyMTA.html) was used as the target runtime as it provides a reasonably fast approach that includes all of the fundamental requirements for a Scheme runtime: tail calls, garbage collection, and continuations.

TODO: explain brief overview of how it works, include some comments from him, explain coding style implications, etc


also mention value types from lisp in small pieces
also mention CHICKEN, a production-quality compiler that uses Baker's approach.

## Scheme standards

r7rs - library (C module) support is the most important, but also exceptions, improvements from r5rs, etc.


## Interpreter

used meta-circular interpreter from SICP as a starting point

## Macros

Chibi scheme explicit renaming macros provide an efficient place to start

## future

should consider optimizations from Andrew Appel's book compiling with continuations. he developed a similar compiler for Standard ML of New Jersey, which is referenced by Baker's paper.

## Conclusion

From Feeley's presentation:

> Performance is not so bad with NO optimizations (about 6 times slower than Gambit-C with full optimization)

TODO: include simple, rough measurements of compile time Cyclone vs CHICKEN (most similar compiler)

## References

- [CONS Should Not CONS Its Arguments, Part II: Cheney on the M.T.A.](http://www.pipeline.com/~hbaker1/CheneyMTA.html), by Henry Baker
- [CHICKEN Scheme](http://www.call-cc.org/)
- [Chibi Scheme](https://github.com/ashinn/chibi-scheme)
- [Compiling Scheme to C with closure conversion](http://matt.might.net/articles/compiling-scheme-to-c/), by Matt Might
- [Lisp in Small Pieces](http://pagesperso-systeme.lip6.fr/Christian.Queinnec/WWW/LiSP.html), by Christian Queinnec
- [R<sup>5</sup>RS Scheme Specification](http://www.schemers.org/Documents/Standards/R5RS/HTML/)
- [R<sup>7</sup>RS Scheme Specification](http://trac.sacrideo.us/wg/wiki)
- [Structure and Interpretation of Computer Programs](https://mitpress.mit.edu/sicp/full-text/book/book.html), by Harold Abelson and Gerald Jay Sussman
- [The 90 minute Scheme to C compiler](http://churchturing.org/y/90-min-scc.pdf), by Marc Feeley
