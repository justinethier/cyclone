# Base Library

The `(scheme base)` library exports many of the procedures and syntax bindings that are traditionally associated with Scheme.

For more information see the [R<sup>7</sup>RS Scheme Specification](../../r7rs.pdf).

- [`abs`](#abs)
- [`and`](#and)
- [`any`](#any)
- [`append`](#append)
- [`assoc`](#assoc)
- [`assq`](#assq)
- [`assv`](#assv)
- [`begin`](#begin)
- [`boolean=?`](#boolean)
- [`bytevector-copy`](#bytevector-copy)
- [`bytevector-copy!`](#bytevector-copy-1)
- [`call-with-current-continuation`](#call-with-current-continuation)
- [`call-with-port`](#call-with-port)
- [`call-with-values`](#call-with-values)
- [`call/cc`](#callcc)
- [`case`](#case)
- [`ceiling`](#ceiling)
- [`char<=?`](#char)
- [`char<?`](#char-1)
- [`char=?`](#char-2)
- [`char>=?`](#char-3)
- [`char>?`](#char-4)
- [`complex?`](#complex)
- [`cond`](#cond)
- [`cond-expand`](#cond-expand)
- [`cons-source`](#cons-source)
- [`current-error-port`](#current-error-port)
- [`current-input-port`](#current-input-port)
- [`current-output-port`](#current-output-port)
- [`define-record-type`](#define-record-type)
- [`denominator`](#denominator)
- [`do`](#do)
- [`dynamic-wind`](#dynamic-wind)
- [`eof-object`](#eof-object)
- [`error`](#error)
- [`even?`](#even)
- [`every`](#every)
- [`exact`](#exact)
- [`exact-integer?`](#exact-integer)
- [`exact?`](#exact)
- [`expt`](#expt)
- [`features`](#features)
- [`floor`](#floor)
- [`floor-quotient`](#floor-quotient)
- [`floor-remainder`](#floor-remainder)
- [`floor-remainder`](#floor-remainder )
- [`floor/`](#floor-1)
- [`flush-output-port`](#flush-output-port)
- [`foldl`](#foldl)
- [`foldr`](#foldr)
- [`for-each`](#for-each)
- [`gcd`](#gcd)
- [`get-output-bytevector`](#get-output-bytevector)
- [`get-output-string`](#get-output-string)
- [`guard`](#guard)
- [`guard-aux`](#guard-aux)
- [`inexact`](#inexact)
- [`inexact?`](#inexact-1)
- [`input-port-open?`](#input-port-open)
- [`input-port?`](#input-port)
- [`lcm`](#lcm)
- [`let`](#let)
- [`let*`](#let-1)
- [`let*-values`](#let-values)
- [`let-values`](#let-values-1)
- [`letrec`](#letrec)
- [`letrec*`](#letrec-1)
- [`list`](#list)
- [`list-copy`](#list-copy)
- [`list-ref`](#list-ref)
- [`list-set!`](#list-set)
- [`list-tail`](#list-tail)
- [`list?`](#list-1)
- [`make-constructor`](#make-constructor)
- [`make-getter`](#make-getter)
- [`make-list`](#make-list)
- [`make-parameter`](#make-parameter)
- [`make-setter`](#make-setter)
- [`make-string`](#make-string)
- [`make-type-predicate`](#make-type-predicate)
- [`map`](#map)
- [`max`](#max)
- [`member`](#member)
- [`memq`](#memq)
- [`memv`](#memv)
- [`min`](#min)
- [`modulo`](#modulo)
- [`negative?`](#negative)
- [`newline`](#newline)
- [`not`](#not)
- [`numerator`](#numerator)
- [`odd?`](#odd)
- [`open-input-bytevector`](#open-input-bytevector)
- [`open-input-string`](#open-input-string)
- [`open-output-bytevector`](#open-output-bytevector)
- [`open-output-string`](#open-output-string)
- [`or`](#or)
- [`output-port-open?`](#output-port-open)
- [`output-port?`](#output-port)
- [`positive?`](#positive)
- [`quasiquote`](#quasiquote)
- [`quotient`](#quotient)
- [`raise`](#raise)
- [`raise-continuable`](#raise-continuable)
- [`rational?`](#rational)
- [`read-line`](#read-line)
- [`read-string`](#read-string)
- [`receive`](#receive)
- [`record?`](#record)
- [`register-simple-type`](#register-simple-type)
- [`remainder`](#remainder)
- [`reverse`](#reverse)
- [`round`](#round)
- [`slot-set!`](#slot-set)
- [`square`](#square)
- [`string`](#string)
- [`string->list`](#string-list)
- [`string->utf8`](#string-utf8)
- [`string->vector`](#string-vector)
- [`string-copy`](#string-copy)
- [`string-copy!`](#string-copy-1)
- [`string-fill!`](#string-fill)
- [`string-for-each`](#string-for-each)
- [`string-map`](#string-map)
- [`string<=?`](#string)
- [`string<?`](#string-1)
- [`string=?`](#string-2)
- [`string>=?`](#string-3)
- [`string>?`](#string-4)
- [`symbol=?`](#symbol)
- [`syntax-error`](#syntax-error)
- [`syntax-rules`](#syntax-rules)
- [`truncate`](#truncate)
- [`truncate-quotient`](#truncate-quotient)
- [`truncate-remainder`](#truncate-remainder)
- [`truncate/`](#truncate-1)
- [`type-slot-offset`](#type-slot-offset)
- [`unless`](#unless)
- [`utf8->string`](#utf8-string)
- [`values`](#values)
- [`vector`](#vector)
- [`vector->list`](#vector-list)
- [`vector->string`](#vector-string)
- [`vector-append`](#vector-append)
- [`vector-copy`](#vector-copy)
- [`vector-copy!`](#vector-copy-1)
- [`vector-fill!`](#vector-fill)
- [`vector-for-each`](#vector-for-each)
- [`vector-map`](#vector-map)
- [`when`](#when)
- [`with-exception-handler`](#with-exception-handler)
- [`write-char`](#write-char)
- [`write-string`](#write-string)
- [`zero?`](#zero)


#abs
#and
#any
#append
#assoc
#assq
#assv
#begin
#boolean=?
#bytevector-copy
#bytevector-copy!
#call-with-current-continuation
#call-with-port
#call-with-values
#call/cc
#case
#ceiling
#char<=?
#char<?
#char=?
#char>=?
#char>?
#complex?
#cond
#cond-expand
#cons-source
#current-error-port
#current-input-port
#current-output-port
#define-record-type
#denominator
#do
#dynamic-wind
#eof-object
#error
#even?
#every
#exact
#exact-integer?
#exact?
#expt
#features
#floor
#floor-quotient 
#floor-remainder
#floor-remainder 
#floor/ 
#flush-output-port
#foldl
#foldr
#for-each
#gcd
#get-output-bytevector
#get-output-string
#guard
#guard-aux
#inexact
#inexact?
#input-port-open?
#input-port?
#lcm
#let
#let*
#let*-values
#let-values
#letrec
#letrec*
#list
#list-copy
#list-ref
#list-set!
#list-tail
#list?
#make-constructor
#make-getter
#make-list
#make-parameter
#make-setter
#make-string
#make-type-predicate
#map
#max
#member
#memq
#memv
#min
#modulo
#negative?
#newline
#not
#numerator
#odd?
#open-input-bytevector
#open-input-string
#open-output-bytevector
#open-output-string
#or
#output-port-open?
#output-port?
#positive?
#quasiquote
#quotient
#raise
#raise-continuable
#rational?
#read-line
#read-string
#receive
#record?
#register-simple-type
#remainder
#reverse
#round
#slot-set!
#square
#string
#string->list
#string->utf8
#string->vector
#string-copy
#string-copy!
#string-fill!
#string-for-each
#string-map
#string<=?
#string<?
#string=?
#string>=?
#string>?
#symbol=?
#syntax-error
#syntax-rules
#truncate
#truncate-quotient 
#truncate-remainder 
#truncate/ 
#type-slot-offset
#unless
#utf8->string
#values
#vector
#vector->list
#vector->string
#vector-append
#vector-copy
#vector-copy!
#vector-fill!
#vector-for-each
#vector-map
#when
#with-exception-handler
#write-char
#write-string
#zero?
