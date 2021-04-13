# R7RS Library Interface Library

The `(scheme cyclone libraries)` library implements r7rs libraries.

*This library is used internally by the compiler and its API may change at any time.*

- [`library?`](#library)
- [`library-exists?`](#library-exists)
- [`lib:list->import-set`](#liblist-import-set)
- [`lib:name`](#libname)
- [`lib:name->string`](#libname-string)
- [`lib:name->symbol`](#libname-symbol)
- [`lib:name->unique-string`](#libname-unique-string)
- [`lib:result`](#libresult)
- [`lib:exports`](#libexports)
- [`lib:rename-exports`](#librename-exports)
- [`lib:imports`](#libimports)
- [`lib:body`](#libbody)
- [`lib:includes`](#libincludes)
- [`lib:include-c-headers`](#libinclude-c-headers)
- [`lib:import->filename`](#libimport-filename)
- [`lib:import->metalist`](#libimport-metalist)
- [`lib:import->path`](#libimport-path)
- [`lib:read-imports`](#libread-imports)
- [`lib:import->export-list`](#libimport-export-list)
- [`lib:resolve-meta`](#libresolve-meta)
- [`lib:get-all`](#libget-all)
- [`lib:get-all-import-deps`](#libget-all-import-deps)
- [`lib:get-dep-list`](#libget-dep-list)
- [`lib:imports->idb`](#libimports-idb)
- [`lib:idb:ids`](#libidb:ids)

# library?

    (library? obj)

Predicate - return `#t` if the given `obj` a `define-library` S-expression or `#f` otherwise.

# library-exists?

    (library-exists? import-set)
    (library-exists? import-set file-extension)

Determine if a library exists on the file system for the given import set.

Checks for the default file extension of `.sld` unless `file-extension` is provided.

# lib:list->import-set

    (lib:list->import-set lis)

Convert a raw list to an import set. For example, a list might be `(srfi 18)` containing the number `18`. An import set contains only symbols or sub-lists. Any numbers are converted to the corresponding symbol.

This is also a convenient time to do any name conversions from an alias to the actual library, so any such conversion will also be performed.

# lib:name

    (lib:name ast)

Return the library name as an import set. For example `(scheme base)`.

# lib:name->string

    (lib:name->string name)

Convert name (as list of symbols) to a mangled string.

# lib:name->symbol

    (lib:name->symbol name)

Convert library name to a unique symbol.

# lib:name->unique-string

    (lib:name->unique-string name)

Convert name (as list of symbols) to a mangled string guaranteed to be unique.

# lib:result

    (lib:result result)
    
Helper function that returns `result` unless `result` is `#f` in which case the empty list is returned as a default value.

# lib:exports

    (lib:exports ast)

Return the library's exports.

# lib:rename-exports

   (lib:rename-exports ast)

Return the library's exports that are renamed.

# lib:imports

    (lib:imports ast)

Return the library's imports.

# lib:body

    (lib:body ast)

Return the given library's body. IE, the contents of `begin`.

# lib:includes

    (lib:includes ast)

Retun the library's include directives.

# lib:include-c-headers

    (lib:include-c-headers ast)

Return the library's `include-c-headers` directives.

# lib:import->filename

    (lib:import->filename import)
    (lib:import->filename import extension)
    (lib:import->filename import extension append-path)
    (lib:import->filename import extension append-path prepend-path)

Resolve library filename given an import `import`. 

Options:

- `extension`, assumes ".sld" file extension if one is not specified.
- `append-path`, list of strings
- `prepend-path`, list of strings

# lib:import->metalist

    (lib:import->metalist import append-dirs prepend-dirs)

Given an import set `import` find the associated `.meta` file, if it exists, and return its contents. An empty list is returned if a file cannot be found.

# lib:import->path

    (lib:import->path import append-dirs prepend-dirs include)

Get path to directory that contains the library.

# lib:read-imports

    (lib:read-imports import append-dirs prepend-dirs expander)

Given a single import from an import-set, open the corresponding library file and retrieve the library's import-set.

# lib:import->export-list

    (lib:import->export-list import append-dirs prepend-dirs expander)

Read export list for a given import.

# lib:resolve-meta

    (lib:resolve-meta imports append-dirs prepend-dirs)

Return contents of all `.meta` files for the given import sets `imports`.

# lib:get-all

    (lib:get-all ast tag)

Get all instances of given tagged list from a library definition, and collect the contents of them into a single list.

# lib:get-all-import-deps

    (lib:get-all-import-deps imports append-dirs prepend-dirs expander)

Given an import set, get all dependant import names that are required
The list of deps is intended to be returned in order, such that the
libraries can be initialized properly in sequence.

# lib:get-dep-list

    (lib:get-dep-list imports)

Given a list of alists `(library-name . imports)`, resolve all of the dependencies and return an ordered list of library names such that each library is encounted after the libraries it imports (IE, it's dependencies).

# lib:imports->idb

    (lib:imports->idb imports append-dirs prepend-dirs expander)

Take a list of imports and create a "database" from them
consisting of maps between each exported identifier and the
library that imports that identifier. 

# lib:idb:ids

    (lib:idb:ids db)

Take an idb "database" `db` and create a list of identifiers that are imported. EG: `((call/cc . (scheme base)))` ==> `(call/cc)`
