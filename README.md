[<img src="docs/images/cyclone-logo-04-header.png" alt="cyclone-scheme">](http://github.com/justinethier/cyclone)

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

The [User Manual](docs/User-Manual.md) covers in detail how to use Cyclone, and provides information and API documentation on the Scheme language features implemented by Cyclone.

[Writing the Cyclone Scheme Compiler](docs/Writing-the-Cyclone-Scheme-Compiler.md) provides high-level details on how the compiler was written and how it works.

Cyclone's [Garbage Collector](docs/Garbage-Collector.md) is documented at a high-level. This document includes details on extending Cheney on the MTA to support multiple stacks and fusing that approach with a tri-color marking collector.

Finally, if you need another resource to start learning the Scheme language please try a classic textbook such as [Structure and Interpretation of Computer Programs](https://mitpress.mit.edu/sicp/full-text/book/book.html).

Benchmarks
----------

The following [benchmarks from Larceny](http://www.larcenists.org/benchmarksGenuineR7Linux.html) give an indication how well Cyclone performs compared with other R<sup>7</sup>RS Schemes. 

These benchmarks were recorded on a system with an Intel Core i5 CPU @ 2.20 GHz and indicate elapsed time. A longer bar indicates worse performance - however if there is no bar at all it means the benchmark could not be completed in a reasonable amount of time.

<img src="docs/images/benchmarks/gabriel.png">

Benchmark | Cyclone | Chibi | Chicken
--------- | ------- | ----- | -------
browse    | 77      | 439   | 30
deriv     | 39      | 212   | 13
destruc   | 136     | 197   | 20
diviter   | 51      | 122.9 | 8
divrec    | 70      | 108   | 29
puzzle    | 184     | -     | 32
triangl   | 95      | 201   | 26.6
tak       | 70      | 105   | 28.9
takl      | 132     | -     | 78.7
ntakl     | 152     | 193   | 77.9
cpstak    | 92      | -     | 35
ctak      | 7.884   | -     | 8.6

License
-------
Copyright (C) 2014 [Justin Ethier](http://github.com/justinethier).

Cyclone is available under the [MIT license](http://www.opensource.org/licenses/mit-license.php).
