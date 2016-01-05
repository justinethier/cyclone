[<img src="docs/images/cyclone-logo-04-header.png" alt="cyclone-scheme">](http://justinethier.github.com/cyclone)

Cyclone is an experimental Scheme-to-C compiler that uses the [Cheney on the MTA](http://www.pipeline.com/~hbaker1/CheneyMTA.html) technique to implement full tail recursion, continuations, and generational garbage collection.

Getting Started
---------------

1. To install Cyclone on your machine for the first time use [**cyclone-bootstrap**](https://github.com/justinethier/cyclone-bootstrap) to build a set of binaries. 

2. After installing you can run the `cyclone` command to compile a single Scheme file:

        $ cyclone examples/fac.scm
        $ examples/fac
        3628800
    
    And the `icyc` command to start an interactive interpreter:
    
        $ icyc
        
                      :@
                    @@@
                  @@@@:
                `@@@@@+
               .@@@+@@@      Cyclone
               @@     @@     An experimental Scheme compiler
              ,@             https://github.com/justinethier/cyclone
              '@
              .@
               @@     #@     (c) 2014 Justin Ethier
               `@@@#@@@.     Version 0.0.1 (Pre-release)
                #@@@@@
                +@@@+
                @@#
              `@.
        
        cyclone> (write 'hello-world)
        hello-world

   You can use [`rlwrap`](http://linux.die.net/man/1/rlwrap) to make the interpreter more friendly, EG: `rlwrap icyc`.

3. If you need help learning the Scheme language try a classic textbook such as [Structure and Interpretation of Computer Programs](https://mitpress.mit.edu/sicp/full-text/book/book.html).

   Also, check out the [features](docs/Features.md) page for a list of language features currently implemented.

Documentation
-------------

[Features](docs/Features.md) is a list of the language features implemented so far. For more information about the Scheme language implemented by Cyclone, see the [R<sup>7</sup>RS Scheme Specification](docs/r7rs.pdf).

The [Development Guide](docs/Development.md) contains instructions for hacking on Cyclone.

[Writing the Cyclone Scheme Compiler](docs/Writing-the-Cyclone-Scheme-Compiler.md) provides high-level details on how the compiler was written and how it works.

References
----------

- [CONS Should Not CONS Its Arguments, Part II: Cheney on the M.T.A.](http://www.pipeline.com/~hbaker1/CheneyMTA.html), by Henry Baker
- [CHICKEN Scheme](http://www.call-cc.org/)
- [Chibi Scheme](https://github.com/ashinn/chibi-scheme)
- [Compiling Scheme to C with closure conversion](http://matt.might.net/articles/compiling-scheme-to-c/), by Matt Might
- Implementing an on-the-fly garbage collector for Java, by Domani et al
- [Lisp in Small Pieces](http://pagesperso-systeme.lip6.fr/Christian.Queinnec/WWW/LiSP.html), by Christian Queinnec
- Portable, Unobtrusive Garbage Collection for Multiprocessor Systems, by Damien Doligez and Georges Gonthier
- [R<sup>5</sup>RS Scheme Specification](http://www.schemers.org/Documents/Standards/R5RS/HTML/)
- [R<sup>7</sup>RS Scheme Specification](http://trac.sacrideo.us/wg/wiki)
- [Structure and Interpretation of Computer Programs](https://mitpress.mit.edu/sicp/full-text/book/book.html), by Harold Abelson and Gerald Jay Sussman
- [The 90 minute Scheme to C compiler](http://churchturing.org/y/90-min-scc.pdf), by Marc Feeley

License
-------
Copyright (C) 2014 [Justin Ethier](http://github.com/justinethier).

Cyclone is available under the [MIT license](http://www.opensource.org/licenses/mit-license.php).
