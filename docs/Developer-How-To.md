## Add a primitive

- Add function/definitions to runtime.h and runtime.c
- Add to prim? section in transforms.scm. Some functions may need to added to the next section in the file, so they are not constant-folded (IE, evaluated at compile time).
- Add to the c-compile-primitive section in cgen.scm.
- TODO: compile files in what order? And copy them where? I think sld files need to be built and copied, and then cyclone and icyc need to be rebuilt linking against them?
- TODO: what about eval.scm?
