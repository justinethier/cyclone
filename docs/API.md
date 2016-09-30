[<img src="images/cyclone-logo-04-header.png" alt="cyclone-scheme">](http://github.com/justinethier/cyclone)

# API Documentation

- [R<sup>7</sup>RS Libraries](#r7rs-libraries)
- [SRFI Library Support](#srfi-library-support)
- [Cyclone Internals](#cyclone-internals)
- [Index](#index)

# R<sup>7</sup>RS Libraries

This section of the Cyclone API is based on the [R<sup>7</sup>RS Scheme Specification](r7rs.pdf):

- [Primitives](api/primitives.md)
- [`scheme base`](api/scheme/base.md)
- [`scheme case-lambda`](api/scheme/case-lambda.md)
- [`scheme char`](api/scheme/char.md)
- [`scheme complex`](api/scheme/complex.md)
- [`scheme cxr`](api/scheme/cxr.md)
- [`scheme eval`](api/scheme/eval.md)
- [`scheme file`](api/scheme/file.md)
- [`scheme inexact`](api/scheme/inexact.md)
- [`scheme lazy`](api/scheme/lazy.md)
- [`scheme load`](api/scheme/load.md)
- [`scheme process-context`](api/scheme/process-context.md)
- [`scheme read`](api/scheme/read.md)
- [`scheme time`](api/scheme/time.md)
- [`scheme write`](api/scheme/write.md)

# SRFI Library Support

Cyclone supports the following [Scheme Requests for Implementation (SRFI)](http://srfi.schemers.org/) libraries. Detailed information is available in the linked SRFI page as well as the provided Cyclone API:

- [`srfi 1`](api/srfi/1.md) - [List Library](http://srfi.schemers.org/srfi-1/srfi-1.html)
- [`srfi 2`](api/srfi/2.md) - [`and-let*`](http://srfi.schemers.org/srfi-2/srfi-2.html)
- [`srfi 8`](api/srfi/8.md) - [`receive`: Binding to multiple values](http://srfi.schemers.org/srfi-8/srfi-8.html) - Included as part of `scheme base`.
- [`srfi 18`](api/srfi/18.md) - [Multithreading support](http://srfi.schemers.org/srfi-18/srfi-18.html)
- [`srfi 27`](api/srfi/27.md) - [Sources of random bits](http://srfi.schemers.org/srfi-27/srfi-27.html)
- [`srfi 69`](api/srfi/69.md) - [Basic hash tables](http://srfi.schemers.org/srfi-69/srfi-69.html)
- [`srfi 106`](api/srfi/106.md) - [Basic socket interface](http://srfi.schemers.org/srfi-106/srfi-106.html)
- [`srfi 111`](api/srfi/111.md) - [Boxes](http://srfi.schemers.org/srfi-111/srfi-111.html)
- [`srfi 117`](api/srfi/117.md) - [Mutable queues](http://srfi.schemers.org/srfi-117/srfi-117.html)
- [`srfi 132`](api/srfi/132.md) - [Sort Libraries](http://srfi.schemers.org/srfi-132/srfi-132.html)
- [`srfi 133`](api/srfi/133.md) - [Vector Library (R7RS-compatible)](http://srfi.schemers.org/srfi-133/srfi-133.html)

# Cyclone Internals

These libraries are used by the Cyclone compiler itself and could change in the future:

- `scheme cyclone cgen`
- `scheme cyclone common`
- `scheme cyclone libraries`
- `scheme cyclone macros`
- `scheme cyclone transforms`
- `scheme cyclone util`

# Index

[`abs`](api/scheme/base.md#abs)
[`acos`](api/scheme/inexact.md#acos)
[`and`](api/scheme/base.md#and)
[`angle`](api/scheme/complex.md#angle)
[`any`](api/scheme/base.md#any)
[`append`](api/scheme/base.md#append)
[`asin`](api/scheme/inexact.md#asin)
[`assoc`](api/scheme/base.md#assoc)
[`assq`](api/scheme/base.md#assq)
[`assv`](api/scheme/base.md#assv)
[`atan`](api/scheme/inexact.md#atan)

- - -
[`begin`](api/scheme/base.md#begin)
[`boolean=?`](api/scheme/base.md#boolean)
[`bytevector-copy`](api/scheme/base.md#bytevector-copy)
[`bytevector-copy!`](api/scheme/base.md#bytevector-copy-1)

- - -
[`caaaar`](api/scheme/cxr.md#caaaar)
[`caaadr`](api/scheme/cxr.md#caaadr)
[`caaar`](api/scheme/cxr.md#caaar)
[`caadar`](api/scheme/cxr.md#caadar)
[`caaddr`](api/scheme/cxr.md#caaddr)
[`caadr`](api/scheme/cxr.md#caadr)
[`cadaar`](api/scheme/cxr.md#cadaar)
[`cadadr`](api/scheme/cxr.md#cadadr)
[`cadar`](api/scheme/cxr.md#cadar)
[`caddar`](api/scheme/cxr.md#caddar)
[`cadddr`](api/scheme/cxr.md#cadddr)
[`caddr`](api/scheme/cxr.md#caddr)
[`call/cc`](api/scheme/base.md#callcc)
[`call-with-current-continuation`](api/scheme/base.md#call-with-current-continuation)
[`call-with-input-file`](api/scheme/file.md#call-with-input-file)
[`call-with-output-file`](api/scheme/file.md#call-with-output-file)
[`call-with-port`](api/scheme/base.md#call-with-port)
[`call-with-values`](api/scheme/base.md#call-with-values)
[`case`](api/scheme/base.md#case)
[`case-lambda`](api/scheme/case-lambda.md#case-lambda)
[`cdaaar`](api/scheme/cxr.md#cdaaar)
[`cdaadr`](api/scheme/cxr.md#cdaadr)
[`cdaar`](api/scheme/cxr.md#cdaar)
[`cdadar`](api/scheme/cxr.md#cdadar)
[`cdaddr`](api/scheme/cxr.md#cdaddr)
[`cdadr`](api/scheme/cxr.md#cdadr)
[`cddaar`](api/scheme/cxr.md#cddaar)
[`cddadr`](api/scheme/cxr.md#cddadr)
[`cddar`](api/scheme/cxr.md#cddar)
[`cdddar`](api/scheme/cxr.md#cdddar)
[`cddddr`](api/scheme/cxr.md#cddddr)
[`cdddr`](api/scheme/cxr.md#cdddr)
[`ceiling`](api/scheme/base.md#ceiling)
[`char-alphabetic?`](api/scheme/char.md#char-alphabetic)
[`char<=?`](api/scheme/base.md#char)
[`char<?`](api/scheme/base.md#char-1)
[`char=?`](api/scheme/base.md#char-2)
[`char>=?`](api/scheme/base.md#char-3)
[`char>?`](api/scheme/base.md#char-4)
[`char-ci<=?`](api/scheme/char.md#char-ci)
[`char-ci<?`](api/scheme/char.md#char-ci-1)
[`char-ci=?`](api/scheme/char.md#char-ci-2)
[`char-ci>=?`](api/scheme/char.md#char-ci-3)
[`char-ci>?`](api/scheme/char.md#char-ci-4)
[`char-downcase`](api/scheme/char.md#char-downcase)
[`char-foldcase`](api/scheme/char.md#char-foldcase)
[`char-lower-case?`](api/scheme/char.md#char-lower-case)
[`char-numeric?`](api/scheme/char.md#char-numeric)
[`char-upcase`](api/scheme/char.md#char-upcase)
[`char-upper-case?`](api/scheme/char.md#char-upper-case)
[`char-whitespace?`](api/scheme/char.md#char-whitespace)
[`command-line`](api/scheme/process-context.md#command-line)
[`complex?`](api/scheme/base.md#complex)
[`cond`](api/scheme/base.md#cond)
[`cond-expand`](api/scheme/base.md#cond-expand)
[`cos`](api/scheme/inexact.md#cos)
[`create-environment`](api/scheme/eval.md#create-environment)
[`current-error-port`](api/scheme/base.md#current-error-port)
[`current-input-port`](api/scheme/base.md#current-input-port)
[`current-jiffy`](api/scheme/time.md#current-jiffy)
[`current-output-port`](api/scheme/base.md#current-output-port)
[`current-second`](api/scheme/time.md#current-second)

- - -
[`define-record-type`](api/scheme/base.md#define-record-type)
[`delay`](api/scheme/lazy.md#delay) 
[`delay-force`](api/scheme/lazy.md#delay-force) 
[`denominator`](api/scheme/base.md#denominator)
[`digit-value`](api/scheme/char.md#digit-value)
[`display`](api/scheme/write.md#display)
[`do`](api/scheme/base.md#do)
[`dynamic-wind`](api/scheme/base.md#dynamic-wind)

- - -
[`emergency-exit`](api/scheme/process-context.md#emergency-exit)
[`eof-object`](api/scheme/base.md#eof-object)
[`error`](api/scheme/base.md#error)
[`eval`](api/scheme/eval.md#eval)
[`even?`](api/scheme/base.md#even)
[`every`](api/scheme/base.md#every)
[`exact`](api/scheme/base.md#exact)
[`exact?`](api/scheme/base.md#exact)
[`exact-integer?`](api/scheme/base.md#exact-integer)
[`exp`](api/scheme/inexact.md#exp)
[`expt`](api/scheme/base.md#expt)

- - -
[`features`](api/scheme/base.md#features)
[`finite?`](api/scheme/inexact.md#finite)
[`floor`](api/scheme/base.md#floor)
[`floor/`](api/scheme/base.md#floor-1)
[`floor-quotient`](api/scheme/base.md#floor-quotient)
[`floor-remainder`](api/scheme/base.md#floor-remainder )
[`floor-remainder`](api/scheme/base.md#floor-remainder)
[`flush-output-port`](api/scheme/base.md#flush-output-port)
[`foldl`](api/scheme/base.md#foldl)
[`foldr`](api/scheme/base.md#foldr)
[`force`](api/scheme/lazy.md#force) 
[`for-each`](api/scheme/base.md#for-each)

- - -
[`gcd`](api/scheme/base.md#gcd)
[`get-environment-variable`](api/scheme/process-context.md#get-environment-variable)
[`get-environment-variables`](api/scheme/process-context.md#get-environment-variables)
[`get-output-bytevector`](api/scheme/base.md#get-output-bytevector)
[`get-output-string`](api/scheme/base.md#get-output-string)
[`guard`](api/scheme/base.md#guard)

- - -
[`imag-part`](api/scheme/complex.md#imag-part)
[`inexact`](api/scheme/base.md#inexact)
[`inexact?`](api/scheme/base.md#inexact-1)
[`infinite?`](api/scheme/inexact.md#infinite)
[`input-port?`](api/scheme/base.md#input-port)
[`input-port-open?`](api/scheme/base.md#input-port-open)

- - -
[`jiffies-per-second`](api/scheme/time.md#jiffies-per-second)

- - -
[`lcm`](api/scheme/base.md#lcm)
[`let`](api/scheme/base.md#let)
[`let*`](api/scheme/base.md#let-1)
[`letrec`](api/scheme/base.md#letrec)
[`letrec*`](api/scheme/base.md#letrec-1)
[`let*-values`](api/scheme/base.md#let-values)
[`let-values`](api/scheme/base.md#let-values-1)
[`list`](api/scheme/base.md#list)
[`list?`](api/scheme/base.md#list-1)
[`list-copy`](api/scheme/base.md#list-copy)
[`list-ref`](api/scheme/base.md#list-ref)
[`list-set!`](api/scheme/base.md#list-set)
[`list-tail`](api/scheme/base.md#list-tail)
[`load`](api/scheme/load.md#load)
[`log`](api/scheme/inexact.md#log)

- - -
[`magnitude`](api/scheme/complex.md#magnitude)
[`make-constructor`](api/scheme/base.md#make-constructor)
[`make-getter`](api/scheme/base.md#make-getter)
[`make-list`](api/scheme/base.md#make-list)
[`make-parameter`](api/scheme/base.md#make-parameter)
[`make-polar`](api/scheme/complex.md#make-polar)
[`make-promise`](api/scheme/lazy.md#make-promise) 
[`make-rectangular`](api/scheme/complex.md#make-rectangular)
[`make-setter`](api/scheme/base.md#make-setter)
[`make-string`](api/scheme/base.md#make-string)
[`make-type-predicate`](api/scheme/base.md#make-type-predicate)
[`map`](api/scheme/base.md#map)
[`max`](api/scheme/base.md#max)
[`member`](api/scheme/base.md#member)
[`memq`](api/scheme/base.md#memq)
[`memv`](api/scheme/base.md#memv)
[`min`](api/scheme/base.md#min)
[`modulo`](api/scheme/base.md#modulo)

- - -
[`nan?`](api/scheme/inexact.md#nan)
[`negative?`](api/scheme/base.md#negative)
[`newline`](api/scheme/base.md#newline)
[`not`](api/scheme/base.md#not)
[`numerator`](api/scheme/base.md#numerator)

- - -
[`odd?`](api/scheme/base.md#odd)
[`open-input-bytevector`](api/scheme/base.md#open-input-bytevector)
[`open-input-string`](api/scheme/base.md#open-input-string)
[`open-output-bytevector`](api/scheme/base.md#open-output-bytevector)
[`open-output-string`](api/scheme/base.md#open-output-string)
[`or`](api/scheme/base.md#or)
[`output-port?`](api/scheme/base.md#output-port)
[`output-port-open?`](api/scheme/base.md#output-port-open)

- - -
[`parameterize`](api/scheme/base.md#parameterize)
[`positive?`](api/scheme/base.md#positive)
[`promise?`](api/scheme/lazy.md#promise)

- - -
[`quasiquote`](api/scheme/base.md#quasiquote)
[`quotient`](api/scheme/base.md#quotient)

- - -
[`raise`](api/scheme/base.md#raise)
[`raise-continuable`](api/scheme/base.md#raise-continuable)
[`rational?`](api/scheme/base.md#rational)
[`read-all`](api/scheme/read.md#read-all)
[`read`](api/scheme/read.md#read)
[`read-line`](api/scheme/base.md#read-line)
[`read-string`](api/scheme/base.md#read-string)
[`real-part`](api/scheme/complex.md#real-part)
[`receive`](api/scheme/base.md#receive)
[`record?`](api/scheme/base.md#record)
[`remainder`](api/scheme/base.md#remainder)
[`reverse`](api/scheme/base.md#reverse)
[`round`](api/scheme/base.md#round)

- - -
[`setup-environment`](api/scheme/eval.md#setup-environment)
[`sin`](api/scheme/inexact.md#sin)
[`slot-set!`](api/scheme/base.md#slot-set)
[`sqrt`](api/scheme/inexact.md#sqrt)
[`square`](api/scheme/base.md#square)
[`string`](api/scheme/base.md#string)
[`string<=?`](api/scheme/base.md#string-1)
[`string<?`](api/scheme/base.md#string-2)
[`string=?`](api/scheme/base.md#string-3)
[`string>=?`](api/scheme/base.md#string-4)
[`string>?`](api/scheme/base.md#string-5)
[`string-ci<=?`](api/scheme/char.md#string-ci)
[`string-ci<?`](api/scheme/char.md#string-ci-1)
[`string-ci=?`](api/scheme/char.md#string-ci-2)
[`string-ci>=?`](api/scheme/char.md#string-ci-3)
[`string-ci>?`](api/scheme/char.md#string-ci-4)
[`string-copy`](api/scheme/base.md#string-copy)
[`string-copy!`](api/scheme/base.md#string-copy-1)
[`string-downcase`](api/scheme/char.md#string-downcase)
[`string-fill!`](api/scheme/base.md#string-fill)
[`string-foldcase`](api/scheme/char.md#string-foldcase)
[`string-for-each`](api/scheme/base.md#string-for-each)
[`string->list`](api/scheme/base.md#string-list)
[`string-map`](api/scheme/base.md#string-map)
[`string-upcase`](api/scheme/char.md#string-upcase)
[`string->utf8`](api/scheme/base.md#string-utf8)
[`string->vector`](api/scheme/base.md#string-vector)
[`symbol=?`](api/scheme/base.md#symbol)
[`syntax-error`](api/scheme/base.md#syntax-error)

- - -
[`tan`](api/scheme/inexact.md#tan)
[`truncate`](api/scheme/base.md#truncate)
[`truncate/`](api/scheme/base.md#truncate-1)
[`truncate-quotient`](api/scheme/base.md#truncate-quotient)
[`truncate-remainder`](api/scheme/base.md#truncate-remainder)
[`type-slot-offset`](api/scheme/base.md#type-slot-offset)

- - -
[`unless`](api/scheme/base.md#unless)
[`utf8->string`](api/scheme/base.md#utf8-string)

- - -
[`values`](api/scheme/base.md#values)
[`vector`](api/scheme/base.md#vector)
[`vector-append`](api/scheme/base.md#vector-append)
[`vector-copy`](api/scheme/base.md#vector-copy)
[`vector-copy!`](api/scheme/base.md#vector-copy-1)
[`vector-fill!`](api/scheme/base.md#vector-fill)
[`vector-for-each`](api/scheme/base.md#vector-for-each)
[`vector->list`](api/scheme/base.md#vector-list)
[`vector-map`](api/scheme/base.md#vector-map)
[`vector->string`](api/scheme/base.md#vector-string)

- - -
[`when`](api/scheme/base.md#when)
[`with-exception-handler`](api/scheme/base.md#with-exception-handler)
[`with-input-from-file`](api/scheme/file.md#with-input-from-file)
[`with-output-to-file`](api/scheme/file.md#with-output-to-file)
[`write`](api/scheme/write.md#write)
[`write-char`](api/scheme/base.md#write-char)
[`write-shared`](api/scheme/write.md#write-shared)
[`write-simple`](api/scheme/write.md#write-simple)
[`write-string`](api/scheme/base.md#write-string)

- - -
[`zero?`](api/scheme/base.md#zero)
