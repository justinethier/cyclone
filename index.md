---
layout: post
title: Cyclone Scheme
ghproj: "http://github.com/justinethier/cyclone/tree/master/"
---

Cyclone is an experimental Scheme-to-C compiler that uses a variant of the [Cheney on the MTA](http://www.pipeline.com/~hbaker1/CheneyMTA.html) technique to implement full tail recursion, continuations, and generational garbage collection. Unlike previous Cheney on the MTA compilers, Cyclone also allows execution of multiple native threads. An on-the-fly garbage collector is used to manage the second-generation heap and perform major collections without "stopping the world".

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

3. Read the documentation below for more information on how to use Cyclone.

Documentation
-------------

- The [User Manual](docs/User-Manual) covers in detail how to use Cyclone, and provides information and API documentation on the Scheme language features implemented by Cyclone.

- Cyclone's [Garbage Collector](docs/Garbage-Collector) is documented at a high-level. This document includes details on extending Cheney on the MTA to support multiple stacks and fusing that approach with a tri-color marking collector.

- The [Benchmarks](docs/Benchmarks) page compares the performance of Cyclone with other R<sup>7</sup>RS Schemes using a common set of benchmarks.

- [Writing the Cyclone Scheme Compiler](docs/Writing-the-Cyclone-Scheme-Compiler) provides high-level details on how the compiler was written and how it works.

- Finally, if you need another resource to start learning the Scheme language you may want to try a classic textbook such as [Structure and Interpretation of Computer Programs](https://mitpress.mit.edu/sicp/full-text/book/book.html).

Example Programs
----------------

Cyclone provides several example programs, including:

- [Threading]({{ page.ghproj }}examples/threading) - Various examples of multi-threaded programs.

- [Tail Call Optimization]({{ page.ghproj }}examples/tail-call-optimization.scm) - A simple example of Scheme tail call optimization; this program runs forever, calling into two mutually recursive functions.

- [Game of Life]({{ page.ghproj }}examples/game-of-life) - The [Conway's game of life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life) example program and libraries from R<sup>7</sup>RS.

- [Game of Life PNG Image Generator]({{ page.ghproj }}examples/game-of-life-png) - A modified version of game of life that uses libpng to create an image of each iteration instead of writing it to console. This example also demonstrates basic usage of the C Foreign Function Interface (FFI):

<img class="doc" src="docs/images/game-of-life-gliders.gif">

- Finally, the largest program is the compiler itself. Most of the code is contained in a series of libraries which are used by [`cyclone.scm`]({{ page.ghproj }}cyclone.scm) and [`icyc.scm`]({{ page.ghproj }}icyc.scm) to create executables for Cyclone's compiler and interpreter.

License
-------
Copyright (C) 2014 [Justin Ethier](http://github.com/justinethier).

Cyclone is available under the [MIT license](http://www.opensource.org/licenses/mit-license.php).
