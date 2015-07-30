## Add a primitive

- Add function/definitions to runtime.h and runtime.c
- sudo make install-includes
- sudo make install-libs
- Add to prim? section in transforms.scm. Some functions may need to added to the next section in the file, so they are not constant-folded (IE, evaluated at compile time).
- Add to the c-compile-primitive section in cgen.scm.

INSTALL .SCM FILES, then running cyclone will pick up the changes. I think that simplifies the below... may let a make take care of it

- cyclone scheme/cyclone/transforms.sld && cyclone scheme/cyclone/cgen.sld
- sudo cp scheme/cyclone/transforms.* /usr/local/share/cyclone/scheme/cyclone/
  and cgen
- run cyclone to build sld's again
- cyclone cyclone.scm , install new cyclone


- TODO: compile files in what order? And copy them where? I think sld files need to be built and copied, and then cyclone and icyc need to be rebuilt linking against them?
- TODO: what about eval.scm?
