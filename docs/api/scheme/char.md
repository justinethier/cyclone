# Char Library

The `(scheme char)` library provides the procedures for dealing with characters.

For more information see the [R<sup>7</sup>RS Scheme Specification](../../r7rs.pdf).

- [`char-alphabetic?`](#char-alphabetic)
- [`char-ci<=?`](#char-ci)
- [`char-ci<?`](#char-ci-1)
- [`char-ci=?`](#char-ci-2)
- [`char-ci>=?`](#char-ci-3)
- [`char-ci>?`](#char-ci-4)
- [`char-downcase`](#char-downcase)
- [`char-foldcase`](#char-foldcase)
- [`char-lower-case?`](#char-lower-case)
- [`char-numeric?`](#char-numeric)
- [`char-upcase`](#char-upcase)
- [`char-upper-case?`](#char-upper-case)
- [`char-whitespace?`](#char-whitespace)
- [`digit-value`](#digit-value)
- [`string-ci<=?`](#string-ci)
- [`string-ci<?`](#string-ci-1)
- [`string-ci=?`](#string-ci-2)
- [`string-ci>=?`](#string-ci-3)
- [`string-ci>?`](#string-ci-4)
- [`string-downcase`](#string-downcase)
- [`string-foldcase`](#string-foldcase)
- [`string-upcase`](#string-upcase)

#char-alphabetic
    (char-alphabetic? c)
#char-ci
    (char-ci<=? c1 c2 . cs)
#char-ci-1 
    (char-ci<? c1 c2 . cs)
#char-ci-2
    (char-ci=? c1 c2 . cs)
#char-ci-3 
    (char-ci>=? c1 c2 . cs)
#char-ci-4
    (char-ci>? c1 c2 . cs)
#char-downcase
    (char-downcase c)
#char-foldcase
    (char-foldcase c)
#char-lower-case
    (char-lower-case? c)
#char-numeric
    (char-numeric? c) 
#char-upcase
    (char-upcase c)
#char-upper-case
    (char-upper-case? c)
#char-whitespace
    (char-whitespace? c)
#digit-value
    (digit-value c)
#string-ci
    (string-ci<=? s1 s2)
#string-ci-1
    (string-ci<? s1 s2)
#string-ci-2
    (string-ci=? s1 s2)
#string-ci-3
    (string-ci>=? s1 s2)
#string-ci-4
    (string-ci>? s1 s2)
#string-downcase
    (string-downcase str)
#string-foldcase
    (string-foldcase str)
#string-upcase
    (string-upcase str)

