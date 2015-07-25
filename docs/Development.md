Building
------------

Please use cyclone-bootstrap if you are installing Cyclone on a machine for the first time. Otherwise, if you already have a copy of Cyclone installed you can build from Scheme source. 

The following prerequisites are required:

- make
- gcc

From the source directory, use the following commands to build and install:

    $ make
    $ make test
    $ sudo make install
    $ ./cyclone
    
By default everything is installed under `/usr/local`. This may be changed by passing a different `PREFIX`. For example:

    make PREFIX=/home/me install

