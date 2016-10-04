---
layout: main
title: API
---

# Primitives

This section contains all of the primitives and other objects that are included in the Cyclone runtime. These objects are always available regardless of library imports.

For more information see the [R<sup>7</sup>RS Scheme Specification](../r7rs.pdf).

- [`*`](#)
- [`+`](#-1)
- [`-`](#-)
- [`/`](#-2)
- [`<`](#-3)
- [`<=`](#-4)
- [`=`](#-5)
- [`>`](#-6)
- [`>=`](#-7)
- [`apply                 `](#apply)
- [`boolean?              `](#boolean)
- [`bytevector            `](#bytevector)
- [`bytevector-append     `](#bytevector-append)
- [`bytevector-length     `](#bytevector-length)
- [`bytevector-u8-ref     `](#bytevector-u8-ref)
- [`bytevector-u8-set!    `](#bytevector-u8-set)
- [`bytevector?           `](#bytevector)
- [`caar                  `](#caar)
- [`cadr                  `](#cadr)
- [`car                   `](#car)
- [`cdar                  `](#cdar)
- [`cddr                  `](#cddr)
- [`cdr                   `](#cdr)
- [`char->integer         `](#char-integer)
- [`char?                 `](#char)
- [`close-input-port      `](#close-input-port)
- [`close-output-port     `](#close-output-port)
- [`close-port            `](#close-port)
- [`command-line-arguments`](#command-line-arguments)
- [`cons                  `](#cons)
- [`delete-file           `](#delete-file)
- [`eof-object?           `](#eof-object)
- [`eq?                   `](#eq)
- [`equal?                `](#equal)
- [`eqv?                  `](#eqv)
- [`error                 `](#error)
- [`exit                  `](#exit)
- [`file-exists?          `](#file-exists)
- [`integer->char         `](#integer-char)
- [`integer?              `](#integer)
- [`length                `](#length)
- [`list->string          `](#list-string)
- [`list->vector          `](#list-vector)
- [`make-bytevector       `](#make-bytevector)
- [`make-vector           `](#make-vector)
- [`null?                 `](#null)
- [`number->string        `](#number-string)
- [`number?               `](#number)
- [`open-input-file       `](#open-input-file)
- [`open-output-file      `](#open-output-file)
- [`pair?                 `](#pair)
- [`peek-char             `](#peek-char)
- [`port?                 `](#port)
- [`procedure?            `](#procedure)
- [`read-char             `](#read-char)
- [`real?                 `](#real)
- [`set-car!              `](#set-car)
- [`set-cdr!              `](#set-cdr)
- [`string->number        `](#string-number)
- [`string->symbol        `](#string-symbol)
- [`string-append         `](#string-append)
- [`string-cmp            `](#string-cmp)
- [`string-length         `](#string-length)
- [`string-ref            `](#string-ref)
- [`string-set!           `](#string-set)
- [`string?               `](#string)
- [`substring             `](#substring)
- [`symbol->string        `](#symbol-string)
- [`symbol?               `](#symbol)
- [`system                `](#system)
- [`vector-length         `](#vector-length)
- [`vector-ref            `](#vector-ref)
- [`vector-set!           `](#vector-set)
- [`vector?               `](#vector)

# \*

    (* z1 ...)

Return the product of the arguments.

# +

    (+ z1 ...)

Return the sum of the arguments.

# -

    (- z)

    (- z1 z2 ...)

With two or more arguments return their difference. With one argument return the additive inverse.

# /

    (/ z)

    (/ z1 z2 ...)

With two or more arguments return the quotient of the arguments. With one argument return the multiplicative inverse.

# <

    (< x1 x2 ...)

Return `#t` if the arguments are monotonically increasing, or `#f` otherwise.

# <=

    (<= x1 x2 ...)

Return `#t` if the arguments are monotonically non-decreasing, or `#f` otherwise.

# =

    (= x1 x2 ...)

Return `#t` if the arguments are equal, or `#f` otherwise.

# >

    (> x1 x2 ...)

Return `#t` if the arguments are monotonically decreasing, or `#f` otherwise.

# >=

    (>= x1 x2 ...)

Return `#t` if the arguments are monotonically non-increasing, or `#f` otherwise.

# apply

    (apply proc arg1 ... args)

The `apply` procedure calls `proc` with the elements of the list `(append (list arg1 ...) args)` as the actual arguments.

# boolean?

    (boolean? obj)

Determine if `obj` is a boolean.

# bytevector

    (bytevector byte ...)

Create a new bytevector consisting of the given bytes.

# bytevector-append

    (bytevector-append bytevector ...)

Append give bytevectors to create a new bytevector.

# bytevector-length

    (bytevector-length bytevector)

Return the length of the given bytevector.

# bytevector-u8-ref

    (bytevector-u8-ref bytevector k)

Return the bytevector element at index `k`.

# bytevector-u8-set!

    (bytevector-u8-set! bytevector k byte)

Change the value at index `k` of the bytevector to `byte`.

# bytevector?

    (bytevector? obj)

Determine if `obj` is a bytevector.

# caar

    (caar pair)

Return `(car (car pair))`

# cadr

    (cadr pair)

Return `(car (cdr pair))`

# car

    (car pair)

Return the contents of the car field of `pair`.

# cdar

    (cdar pair)

Return `(cdr (car pair))`

# cddr

    (cddr pair)

Return `(cdr (cdr pair))`

# cdr

    (cdr pair)

Return the contents of the cdr field of `pair`.

# char->integer

    (char->integer char)

Return the `char` as an integer value.

# char?

    (char? obj)

Determine if `obj` is a character.

# close-input-port

    (close-input-port port)

Close the given input port.

# close-output-port

    (close-output-port port)

Close the given output port.

# close-port

    (close-port port)

Close the given port.

# command-line-arguments

    (command-line-arguments)

Return the command line arguments to the program as a list of strings.

# cons

    (cons a b)

Create a new pair with its car field set to `a` and its cdr field set to `b`.

# delete-file

    (delete-file string)

Delete a file with the given filename.

# eof-object?

    (eof-object? obj)

Determine if the given object is an EOF object.

# eq?

    (eq? a b)

Determine if `a` and `b` are equal by comparing their pointer values. This operation is guaranteed to be a single comparison no matter the type of each object passed to the function.

# equal?

    (equal? a b)

Determine if `a` and `b` are equal by doing a "deep" comparison. For lists and vectors this means each element in the data structures will be compared until either the end of the structure is reached or an inequality is found. `equal?` is guaranteed to work for circular lists.

# eqv?

    (eqv? a b)

An alias of `eq?`.

# error

    (error message obj ...)

Raises an exception by calling `raise` with the given message and objects.

# exit

    (exit)

    (exit obj)

Exit the program.

# file-exists?

    (file-exists? string)

Determine if the file with filename `string` exists.

# integer->char

    (integer->char x)

Return a character with the same value as the given integer.

# integer?

    (integer? obj)

Determine if the given object is an integer.

# length

    (length list)

Returns the length of `list`.

# list->string

    (list->string list)

Convert the given list of characters to a string.

# list->vector

    (list->vector list)

Convert the given list to a vector.

# make-bytevector

    (make-bytevector k)

    (make-bytevector k byte)

Create a new bytevector of length `k`. If `byte` is provided, each element of the bytevector will be assigned this value.

# make-vector

    (make-vector k)

    (make-vector k obj)

Create a new vector of length `k`. If `obj` is provided, each element of the vector will be assigned this value.

# null?

    (null? obj)

Determine if the given object is the empty list.

# number->string

    (number->string z)

    (number->string z radix)

Return a string representation of the given number.

# number?

    (number? obj)

Determine if the given object is a number.

# open-input-file

    (open-input-file string)

Return an input port that can deliver data from the file `string`.

# open-output-file

    (open-output-file string)

Return an output port that can deliver data from the file `string`.

# pair?

    (pair? obj)

Determine if `obj` is a pair.

# peek-char

    (peek-char)

    (peek-char port)

Returns the next character available from the input port. If no characters are available and end-of-file object is returned.

# port?

    (port? obj)

Determine if `obj` is a port.

# procedure?

    (procedure? obj)

Determine if `obj` is a function.

# read-char

    (read-char)

    (read-char port)

Read a character from the input port.

# real?

    (real? obj)

Determine if `obj` is a real number.

# set-car!

    (set-car! pair obj)

Set the car field of `pair` to `obj`.

# set-cdr!

    (set-cdr! pair obj)

Set the car field of `pair` to `obj`.

# string->number

    (string->number string)

    (string->number string radix)

Return the number represented by the given string.

# string->symbol

    (string->symbol string)

Convert given string to a symbol.

# string-append

    (string-append string ...)

Returns a new string whose characters are the concatenation of the given strings.

# string-cmp

    (string-cmp string1 string2)

Compare both strings and return 0 if the strings are equal, a positive number if `string1` is "greater than" `string2`, and a negative number otherwise.

# string-length

    (string-length string)

Return the length of `string`.

# string-ref

    (string-ref string k)

Return the character at position `k` of `string`.

# string-set!

    (string-set! string k char)

Set the character of `string` at position `k` to `char`.

# string?

    (string? obj)

Determine if `obj` is a string.

# substring

    (substring string start end)

Return a newly-allocatd string consisting of the characters of `string` starting from position `start` and ending at `end`.

# symbol->string

    (symbol->string symbol)

Return a string based on the given symbol.

# symbol?

    (symbol? obj)

Determine if `obj` is a symbol.

# system

    (system string)

Execute an OS command `string` and return the resulting status as a number.

# vector-length

    (vector-length vector)

Return the length of `vector`.

# vector-ref

    (vector-ref vector k)

Return the element at position `k` of `vector`.

# vector-set!

    (vector-set! vector k obj)

Set the element of `vector` at position `k` to `obj`.

# vector?

    (vector? obj)

Determine if `obj` is a vector.

