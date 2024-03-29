# Foreign Library

The `(cyclone foreign)` provides a convenient interface for integrating with C code. It is based in concept on the `(chicken foreign)` module from CHICKEN Scheme. Similarly to that module, this library manipulates the C code directly before it is compiled to a native binary. It is not possible to call these forms at runtime.

# API

- [`c-code`](#c-code)
- [`c-value`](#c-value)
- [`c-define`](#c-define)
- [`c-define-type`](#c-define-type)
- [`opaque?`](#opaque)
- [`opaque-null?`](#opaque-null)
- [`make-opaque`](#make-opaque)

## c-code

*Syntax*

    (c-code CODE ...)

Insert C code directly into the compiled program. Each `CODE` parameter must be a string containing C code.

## c-value

*Syntax*

    (c-value CODE TYPE)

Generate code that takes the C code specified by the string `CODE` and converts it to a Scheme object of type `TYPE`.

## c-define

*Syntax*

    (c-define SCM-FUNC RETURN-TYPE C-FUNC TYPE ...)

Define a Scheme function `SCM-FUNC` returning an object of type `RETURN-TYPE`. The function will call C function specified by the string `C-FUNC` passed parameters of type specified by any `TYPE` arguments.

For example, to define a function that calls `strlen`:

    (c-define scm-strlen int "strlen" string)

Note that these definitions are introduced at the top-level.

## c-define-type

*Syntax*

    (c-define-type NAME TYPE (ARG-CONVERT (RET-CONVERT)))

Define a custom type with symbol `NAME` that is an alias of type `TYPE`. It is also possible to specify conversion functions `ARG-CONVERT` and `RET-CONVERT` to convert to/from this custom type.

EG, to define a type that consists of integers in Scheme and strings in C: 

    (c-define-type string-as-integer string number->string string->number)

## opaque?

    (opaque? obj)

Predicate to determine if `obj` is a C Opaque object.

## opaque-null?

    (opaque-null? obj)

Predicate to determine if `obj` is a C Opaque object that contains `NULL`.

## make-opaque

    (make-opaque)

Create a C Opaque object containing `NULL`.

# Type Specifiers

The following built-in specifiers may be used as a `TYPE` for forms in this module. 

Scheme | C
------ | -
`int` | `int`
`integer` | `int`
`bool` | `int`
`char` | `int`
`string` | `char *`
`symbol` | `const char *`
`bytevector` | `char *`
`float` | `double`
`double` | `double`
`bignum` | `mp_int`
`opaque` | `void *`
`c-void` | `void`
`thread-data` | `gc_thread_data *`

Useful notes:
- Use `opaque` if you need to handle any kind of C pointer.
- Use `string` to handle C `const char*` (`symbol` is strictly used to represent Scheme symbols).
- `thread-data` is a special type used to pass the current thread's `gc_thread_data` instance to a C function. These objects are passed implicitly when making a Scheme function call. For example:

       (c-define sub-big-nums bignum "sub_big_nums" thread-data bignum bignum)
       (sub-big-nums 999999999999999999999999 222222222222222222222222))

