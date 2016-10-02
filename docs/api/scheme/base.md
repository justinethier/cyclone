---
layout: main
title: API
---

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
- [`parameterize`](#parameterize)
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
- [`string<=?`](#string-1)
- [`string<?`](#string-2)
- [`string=?`](#string-3)
- [`string>=?`](#string-4)
- [`string>?`](#string-5)
- [`symbol=?`](#symbol)
- [`syntax-error`](#syntax-error)
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
    (abs num)
#and
    (and {test1} ...)
#any
    (any pred lst)
#append
    (append list ...)
#assoc
    (assoc obj alist)
    (assoc obj alist compare)
#assq
    (assq obj alist)
#assv
    (assv obj alist)
#begin
    (begin {expression or definition} ...)
#boolean=?
    (boolean=? b1 b2 ...)
#bytevector-copy
    (bytevector-copy bytevector)
    (bytevector-copy bytevector start)
    (bytevector-copy bytevector start end)
#bytevector-copy!
    (bytevector-copy! to at from)
    (bytevector-copy! to at from start)
    (bytevector-copy! to at from start end)
#call-with-current-continuation
    (call-with-current-continuation proc)
#call-with-port
    (call-with-port port proc)
#call-with-values
    (call-with-values producer consumer)
#call/cc
    (call/cc proc)
#case
    (case {key} {clause1} {clause2} ...)
#ceiling
    (ceiling z)
#char<=?
    (char<=? c1 c2 c3 ...)
#char<?
    (char<?  c1 c2 c3 ...)
#char=?
    (char=?  c1 c2 c3 ...)
#char>=?
    (char>=? c1 c2 c3 ...)
#char>?
    (char>?  c1 c2 c3 ...)
#complex?
    (complex? obj) 
#cond
    (cond {clause1} {clause2} ...)
#cond-expand
    (cond-expand {ce-clause2} {ce-clause2} ...)
#current-error-port
    (current-error-port)
#current-input-port
    (current-input-port)
#current-output-port
    (current-output-port)
#define-record-type
    (define-record-type {name}
      {constructor} {pred} {field} ...)
#denominator
    (denominator n)
#do
    (do (({variable1} {init1} {step1})
         ...)
    ({test} {expression} ...)
    {command} ...)
#dynamic-wind
    (dynamic-wind before thunk after)
#eof-object
    (eof-object)
#error
    (error message obj ...)
#even?
    (even? num)
#every
    (every pred lst)
#exact
    (exact? num)
#exact-integer?
    (exact-integer? num)
#exact?
    (exact? num)
#expt
    (expt z1 z2)
#features
    (features) 
#floor
    (floor z)
#floor-quotient 
    (floor-quotient n m)
#floor-remainder
    (floor-remainder n m)
#floor/ 
    (floor/ n m)
#flush-output-port
    (flush-output-port)
    (flush-output-port port)
#foldl
    (foldl func accum lst)
#foldr
    (foldr func end lst)
#for-each
    (for-each proc list1 list2 ...)
#gcd
    (gcd n1 ...)
#get-output-bytevector
    (get-output-bytevector port)
#get-output-string
    (get-output-string port)
#guard
    (guard ({variable}
            {cond clause1} {cond clause2} ...)
      {body})
#inexact
    (inexact z)
#inexact?
    (inexact? num)
#input-port-open?
    (input-port-open? port)
#input-port?
    (input-port? port)
#lcm
    (lcm n1 ...)
#let
    (let {bindings} {body})
#let*
    (let* {bindings} {body})
#let*-values
    (let*-values {mv binding spec} {body})
#let-values
    (let-values {mv binding spec} {body})
#letrec
    (letrec {bindings} {body})
#letrec*
    (letrec* {bindings} {body})
#list
    (list obj ...)
#list-copy
    (list-copy lst)
#list-ref
    (list-ref lst k)
#list-set!
    (list-set! lst k obj)
#list-tail
    (list-tail lst k) 
#list?
    (list? o)
#make-constructor
    (make-constructor make name)
#make-getter
    (make-getter sym name idx)
#make-list
    (make-list k )
    (make-list k fill)
#make-parameter
    (make-parameter init)
    (make-parameter init converter)
#make-setter
    (make-setter sym name idx)
#make-string
    (make-string k)
    (make-string k fill)
#make-type-predicate
    (make-type-predicate pred name)
#map
    (map proc list1 list2 ...)
#max
    (max x1 x2 ...)
#member
    (member obj lst) 
    (member obj lst compare) 
#memq
    (memq obj lst)
#memv
    (memv obj lst)
#min
    (min x1 x2 ...)
#modulo
    (modulo a b)
#negative?
    (negative? n)
#newline
    (newline)
    (newline port) 
#not
    (not x)
#numerator
    (numerator n) n)
#odd?
    (odd? num)
#open-input-bytevector
    (open-input-bytevector bv)
#open-input-string
    (open-input-string string)
#open-output-bytevector
    (open-output-bytevector open-output-string)
#open-output-string
    (open-output-string)
#or
    (or {test1} ...)
#output-port-open?
    (output-port-open? port)
#output-port?
    (output-port? obj)
#parameterize
    (parameterize (({param1} {value1}) ...)
      {body})
#positive?
    (positive? n)
#quasiquote
    (quasiquote {qq template})
#quotient
    (quotient x y)
#raise
    (raise obj)
#raise-continuable
    (raise-continuable obj)
#rational?
    (rational? obj)
#read-line
    (read-line)
    (read-line port)
#read-string
    (read-string k)
    (read-string k port)
#receive
    (receive {formals} {expression} {body})
#record?
    (record? obj)
#remainder
    (remainder num1 num2)
#reverse
    (reverse lst)
#round
    (round z)
#slot-set!
    (slot-set! name obj idx val)
#square
    (square z)
#string
    (string char ...)
#string->list
    (string->list string)
    (string->list string start)
    (string->list string start end)
#string->utf8
    (string->utf8 string)
    (string->utf8 string start)
    (string->utf8 string start end)
#string->vector
    (string->vector string)
    (string->vector string start)
    (string->vector string start end)
#string-copy
    (string-copy string)
    (string-copy string start)
    (string-copy string end)
#string-copy!
    (string-copy! to at from)
    (string-copy! to at from start)
    (string-copy! to at from start end)
#string-fill!
    (string-fill! str fill)
    (string-fill! str fill start)
    (string-fill! str fill start end)
#string-for-each
    (string-for-each proc string1 string2 ...)
#string-map
    (string-map proc string1 string2 ...)
#string<=?
    (string<=? str1 str2)
#string<?
    (string<? str1 str2)
#string=?
    (string=? str1 str2)
#string>=?
    (string>=? str1 str2)
#string>?
    (string>? str1 str2)
#symbol=?
    (symbol=? symbol1 symbol2 symbol3 ...)
#syntax-error
    (syntax-error {message} {args} ...)
#truncate
    (truncate z)
#truncate-quotient 
    (truncate-quotient quotient)
#truncate-remainder 
    (truncate-remainder remainder)
#truncate/ 
    (truncate/ n m)
#type-slot-offset
    (type-slot-offset name sym)
#unless
    (unless {test} {expression1} {expression2} ...)
#utf8->string
    (utf8->string bytevector)
    (utf8->string bytevector start)
    (utf8->string bytevector start end)
#values
    (values obj ...)
#vector
    (vector obj ...)
#vector->list
    (vector->list vector)
    (vector->list vector start)
    (vector->list vector start end)
#vector->string
    (vector->string vector)
    (vector->string vector start)
    (vector->string vector start end)
#vector-append
    (vector-append vector ...)
#vector-copy
    (vector-copy vector)
    (vector-copy vector start)
    (vector-copy vector start end)
#vector-copy!
    (vector-copy! to at from)
    (vector-copy! to at from start)
    (vector-copy! to at from start end)
#vector-fill!
    (vector-fill! vector fill)
    (vector-fill! vector fill start)
    (vector-fill! vector fill start end)
#vector-for-each
    (vector-for-each proc vector1 vector2 ...)
#vector-map
    (vector-map proc vector1 vector2 ...)
#when
    (when {test} {expression1} {expression2} ...)
#with-exception-handler
    (with-exception-handler handler thunk)
#write-char
    (write-char char)
    (write-char char port)
#write-string
    (write-string string)
    (write-string string port)
#zero?
    (zero? n)
