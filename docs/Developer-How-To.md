## Add a primitive

- Add function/definitions to `runtime.h` and `runtime.c`
- Rebuild and install runtime library.
- Add to `prim?` section in `transforms.sld`. Some functions may need to added to the next section in the file, so they are not constant-folded (IE, evaluated at compile time).
- Add above the `c-compile-primitive` section in `cgen.sld`. Some functions may need to be added in multiple places to indicate they take additional arguments, call their continuation, etc.

TODO: need to develop this section better to come up with a workable/optimal approach to building things:

- Install modified `.sld` files -- ???
- Compile: `cyclone scheme/cyclone/cgen.sld`
- Copy modified files to cyclone-bootstrap, including `cgen.c`
- install cyclone-bootstrap
- run `make clean ; make && make bootstrap` from cyclone repo
- run `make clean ; ./install` from bootstrap repo

- Add primitives to the list in eval.sld. Rebuild one more time.
