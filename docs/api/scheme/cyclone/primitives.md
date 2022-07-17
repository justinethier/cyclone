# Primitives Library

The `(scheme cyclone primitives)` library contains information about Cyclone's scheme primitives.

*This library is used internally by the compiler and its API may change at any time.*

- [`prim?`](#prim)
- [`prim-call?`](#prim-call)
- [`prim->c-func`](#prim-c-func)
- [`prim/data-arg?`](#primdata-arg)
- [`prim/c-var-assign`](#primc-var-assign)
- [`prim/cvar?`](#primcvar)
- [`prim:check-arg-count`](#primcheck-arg-count)
- [`prim:mutates?`](#primmutates)
- [`prim:cont?`](#primcont)
- [`prim:cont/no-args?`](#primcontno-args)
- [`prim:arg-count?`](#primarg-count)
- [`prim:allocates-object?`](#primallocates-object)

# prim?

    (prim? obj)

Determine if the given object is a symbol referring to a primitive.

# prim-call?

    (prim-call? exp)

Determine if the given expression `exp` is a call to a primitive.

# prim->c-func

    (prim->c-func sym use-alloca? emit-unsafe)

Returns text containing the C function that is used to implement primitive `sym`.

If `emit-unsafe` is true then an unsafe version of the primtive, if available, will be returned.
 
# prim/data-arg?

    (prim/data-arg? sym)

Primitive indicating if the primitive requires passing thread data as its first argument.

# prim/c-var-assign

    (prim/c-var-assign sym)

Return the C data type of variable used to assign the result of primitive `sym`, if applicable. `#f` is returned otherwise.

# prim/cvar?

    (prim/cvar? sym)

Determine if primitive `sym` creates a C variable.

# prim:check-arg-count

    (prim:check-arg-count sym num-args expected)

Return `#f` the primitive `sym` cannot accept the given number of arguments `num-args` given the expected number of arguments `expected`, and `#t` otherwise.

# prim:mutates?

    (prim:mutates? sym)

Does primitive `sym` mutate any of its arguments?

# prim:cont?

    (prim:cont? sym)

Should the compiler pass a continuation as the function's first parameter?

# prim:cont/no-args?

    (prim:cont/no-args? sym)

Is `sym` a primitive function that passes a continuation or thread data but has no other arguments?

# prim:arg-count?

    (prim:arg-count? sym)

Should the compiler pass an integer arg count as the function's first parameter?

# prim:allocates-object?

    (prim:allocates-object? sym use-alloca?)

Does primitive `sym` allocate an object?

