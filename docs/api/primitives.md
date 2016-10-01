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
- [`cell                  `](#cell)
- [`cell-get              `](#cell-get)
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
- [`set-cell!             `](#set-cell)
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

#\*
#+
#-
#/
#<
#<=
#=
#>
#>=
#apply
#boolean?
#bytevector
#bytevector-append
#bytevector-length
#bytevector-u8-ref
#bytevector-u8-set!
#bytevector?
#caar
#cadr
#car
#cdar
#cddr
#cdr
#cell
#cell-get
#char->integer
#char?
#close-input-port
#close-output-port
#close-port
#command-line-arguments
#cons
#delete-file
#eof-object?
#eq?
#equal?
#eqv?
#error
#exit
#file-exists?
#integer->char
#integer?
#length
#list->string
#list->vector
#make-bytevector
#make-vector
#null?
#number->string
#number?
#open-input-file
#open-output-file
#pair?
#peek-char
#port?
#procedure?
#read-char
#real?
#set-car!
#set-cdr!
#set-cell!
#string->number
#string->symbol
#string-append
#string-cmp
#string-length
#string-ref
#string-set!
#string?
#substring
#symbol->string
#symbol?
#system
#vector-length
#vector-ref
#vector-set!
#vector?
