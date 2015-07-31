## Add a primitive

WIP set of instructions for doing this. working to refine this down:

- Add function/definitions to runtime.h and runtime.c
- Add to prim? section in transforms.scm. Some functions may need to added to the next section in the file, so they are not constant-folded (IE, evaluated at compile time).
- Add to the c-compile-primitive section in cgen.scm.
- install modified .scm files
- cyclone scheme/cyclone/cgen.sld
- copy modified files to cyclone-bootstrap, including cgen.c
- install cyclone-bootstrap
- run 'make clean ; make && make bootstrap' from cyclone repo
- run 'make clean ; ./install' from bootstrap repo

