
table of contents

- [References](#References)

## TODO

This document covers some of the background on how Cyclone was written, including different aspects of the compiler and runtime system. Cyclone could not have been written without the abundance of Scheme resources online, and many of those resources are discussed. There is a list at the end of this document of the resources that were most helpful and/or influential in writing Cyclone.

Also, it is important to mention the [husk scheme](http://justinethier.github.io/husk-scheme) project. Husk is primarily an interpreter, but putting the project together originally led me to many of the resources and information that would later be used to build Cyclone. In fact, one of the main motivations in building Cyclone was to take the work a step further and understand what it was like not only to write a Scheme compiler, but also to understand what goes into the runtime system as well. Over time, some of the features and understanding gained in Cyclone may be folded back into Husk.

## Scheme source-to-source transformations
One of the most important inspiration's for Cyclone was Marc Feeley's [The 90 minute Scheme to C compiler]() which includes a presentation, set of slides, and example code written in Gambit Scheme. Feeley includes an overview of how to compile Scheme to C code using source-to-source transformations, including closure and continuation-passing-style (CPS) conversions. 

As outlined in the presentation, some of the difficulties in compiling to C are:

> Scheme has, and C does not have
>  -  tail-calls a.k.a. tail-recursion optimization
>  -  first-class continuations
>  -  closures of indefinite extent
>  -  automatic memory management i.e. GC
> Implications
>  -  cannot translate (all) Scheme calls into C calls
>  -  have to implement continuations
>  -  have to implement closures
>  -  have to organize things to allow GC
> The rest is easy!

A series of source-to-source transformations may be used to transform the original Scheme code into a set of code that may be compiled directly to C. Each step removes powerful features not provided by C, adds constructs needed by the C code, etc. Since Scheme represents code and data in the same way using [S-Expressions](https://en.wikipedia.org/wiki/S-expression), the code can remain in it's original form, without having to introduce special data types as would be the case with many other languages.

The 90-minute scc ultimately compiles the code down to a single function and uses jumps to support continuations. This is a bit too limiting for a production compiler, so that part was not used. Cyclone also includes many other intermediate transformations, including:

- Macro expansion
- Processing of globals
- Alpha conversion
- CPS conversion
- Closure conversion

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
