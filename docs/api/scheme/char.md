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

# char-alphabetic?

    (char-alphabetic? c)

Return `#t` if `c` is alphabetic and `#f` otherwise.

# char-ci<=?

    (char-ci<=? c1 c2 . cs)

Return `#t` if the results of converting all characters to the same case and  passing the arguments to `char->integer` are monotonically increasing or equal.

# char-ci<? 

    (char-ci<? c1 c2 . cs)

Return `#t` if the results of converting all characters to the same case and  passing the arguments to `char->integer` are respectively equal, monotonically increasing.

# char-ci=?

    (char-ci=? c1 c2 . cs)

Return `#t` if the results of converting all characters to the same case and passing the arguments to `char->integer` are equal.

# char-ci>=? 

    (char-ci>=? c1 c2 . cs)

Return `#t` if the results of converting all characters to the same case and  passing the arguments to `char->integer` are monotonically decreasing or equal.

# char-ci>?

    (char-ci>? c1 c2 . cs)

Return `#t` if the results of converting all characters to the same case and passing the arguments to `char->integer` are monotonically decreasing.

# char-downcase

    (char-downcase c)

Returns the lowercase equivalent of `c` if one exists, otherwise `c` is returned.

# char-foldcase

    (char-foldcase c)

# char-lower-case?

    (char-lower-case? c)

Return `#t` if `c` is lower case and `#f` otherwise.

# char-numeric?

    (char-numeric? c) 

Return `#t` if `c` is numeric and `#f` otherwise.

# char-upcase

    (char-upcase c)

Returns the uppercase equivalent of `c` if one exists, otherwise `c` is returned.

# char-upper-case?

    (char-upper-case? c)

Return `#t` if `c` is alphabetic and `#f` otherwise.

# char-whitespace?

    (char-whitespace? c)

Return `#t` if `c` is whitespace and `#f` otherwise.

# digit-value

    (digit-value c)

This procedure returns the numeric value (0 to 9) of its argument if it is a numeric digit (that is, if `char-numeric?` returns `#t`), or `#f` on any other character.

# string-ci<=? 

    (string-ci<=? s1 s2)

# string-ci<?

    (string-ci<? s1 s2)

# string-ci=? 

    (string-ci=? s1 s2)

Returns `#t` if all of the given strings are equal using a case-insensitive comparison, and false otherwise.

# string-ci>=?

    (string-ci>=? s1 s2)

# string-ci>? 

    (string-ci>? s1 s2)

# string-downcase

    (string-downcase str)

Return a newly-allocated string with any uppercase characters converted to lowercase.

# string-foldcase

    (string-foldcase str)

# string-upcase

    (string-upcase str)

Return a newly-allocated string with any lowercase characters converted to uppercase.

