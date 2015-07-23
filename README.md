[<img src="docs/images/cyclone-logo-03-header.png" alt="cyclone-scheme">](http://justinethier.github.com/cyclone)

Cyclone is an experimental Scheme-to-C compiler that uses the [Cheney on the MTA](http://www.pipeline.com/~hbaker1/CheneyMTA.html) technique to implement full tail recursion, continuations, and generational garbage collection.

Getting Started
---------------

If you are installing Cyclone for the first time use [cyclone-bootstrap](https://github.com/justinethier/cyclone-bootstrap) to build a set of binaries. 

After installing you can read the Documentation section below for more.

Building
------------

If you already have a copy of Cyclone installed you can build from Scheme source. The following prerequisites are required:

- make
- gcc
- Existing installation of Cyclone

From the source directory, use the following commands to build and install:

    $ make
    $ make test
    $ sudo make install
    $ ./cyclone
    
By default everything is installed under `/usr/local`. This may be changed by passing a different `PREFIX`. For example:

    make PREFIX=/home/me install

You may then run the compiler using the `cyclone` command, and run the interpreter using `icyc`.

Documentation
-------------

Run the `cyclone` command to compile a single Scheme file, and the `icyc` command to start the interactive interpreter.

For more information about the Scheme language implemented by Cyclone, see the [R<sup>7</sup>RS Scheme Specification](http://trac.sacrideo.us/wg/wiki). Here is a list of included [features](FEATURES.md).

References
----------

- [CONS Should Not CONS Its Arguments, Part II: Cheney on the M.T.A.](http://www.pipeline.com/~hbaker1/CheneyMTA.html), by Henry Baker
- [CHICKEN Scheme](http://www.call-cc.org/)
- [Chibi Scheme](https://github.com/ashinn/chibi-scheme)
- [Compiling Scheme to C with closure conversion](http://matt.might.net/articles/compiling-scheme-to-c/), by Matt Might
- [Lisp in Small Pieces](http://pagesperso-systeme.lip6.fr/Christian.Queinnec/WWW/LiSP.html), by Christian Queinnec
- [R<sup>5</sup>RS Scheme Specification](http://www.schemers.org/Documents/Standards/R5RS/HTML/)
- [R<sup>7</sup>RS Scheme Specification](http://trac.sacrideo.us/wg/wiki)
- [Structure and Interpretation of Computer Programs](https://mitpress.mit.edu/sicp/full-text/book/book.html), by Harold Abelson and Gerald Jay Sussman
- [The 90 minute Scheme to C compiler](http://churchturing.org/y/90-min-scc.pdf), by Marc Feeley

License
-------
Copyright (C) 2014 [Justin Ethier](http://github.com/justinethier)

Cyclone is available under the [MIT license](http://www.opensource.org/licenses/mit-license.php).
