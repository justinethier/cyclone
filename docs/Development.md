# Development Guide

- [Environment](#environment)
- [Building](#building)
- [Testing a Build](#testing-a-build)

## Environment

During development it is a good idea to clone the `cyclone-bootstrap` repository as well as the `cyclone` one. Place both directories in the same parent directory and you can use `sync.sh` to copy individual compiled files to `cyclone-bootstrap`, or `make bootstrap` to copy everything.

## Building

Please use cyclone-bootstrap if you are installing Cyclone on a machine for the first time. Otherwise, if you already have a copy of Cyclone installed you can build from Scheme source. 

The following prerequisites are required:

- make
- gcc

From the source directory, use the following commands to build and install:

    $ make
    $ make test
    $ sudo make install
    $ cyclone
    
By default everything is installed under `/usr/local`. This may be changed by passing a different `PREFIX`. For example:

    make PREFIX=/home/me install

## Testing a Build

`make test` may be used to perform basic testing. 

To make sure everything works, install a modified copy of Cyclone and run the following to rebuild the libraries, compiler, interpreter, and examples from source:

    $ make clean
    $ make
    $ make test
    $ make bootstrap

This confirms that the compiler - with any changes - can still be built from source, and syncs any changes up to `cyclone-bootstrap`. Before checking in a set of changes or releasing a build it is also a good idea to do a rebuild of the bootstrap repo also, to make sure it still works.

