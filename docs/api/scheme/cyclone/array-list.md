---
layout: main
title: API
---

# Array list library #

A simple [dynamic array][1] implementation, designed to be used to support
various other structures.

## Description ##

The scale factor for this implementation is 2, which is used by several other
languages' standard library implementation of dynamic arrays. The backing
structure is a Scheme vector, with a minimum size of 16 to avoid early resizing
on small structures.

## Definitions ##

When describing a procedure's arguments, ``array-list`` denotes an array list
argument.

## Limitations ##

Currently does not contain a full set of operations - more will be added if
needed or requested.

## Contents ##

- [`array-list`](#array-list)
- [`array-list?`](#array-list1)
- [`array-list-delete!`](#array-list-delete)
- [`array-list-empty?`](#array-list-empty) 
- [`array-list-insert!`](#array-list-insert)
- [`array-list-length`](#array-list-length)
- [`array-list-ref`](#array-list-ref)
- [`array-list-set!`](#array-list-set)
- [`make-array-list`](#make-array-list)

## Constructors ##

### array-list ###

```(array-list obj ...)```

Creates a new array list containing all of ``obj``, in that order.

**Time complexity:** *O(n)* (amortized), where *n* is ``(length obj)``.

**Space complexity:** *O(n)* (worst-case), where *n* is ``(length obj)``

### make-array-list ###

```(make-array-list k)```

Creates a new empty array list with a capacity of ``(max 16 k)`` elements.

**Time complexity:** *O(n)* (worst-case), where *n* is ``k``.

**Space complexity:** *O(n)* (worst-case), where *n* is ``k``.

## Predicates ##

### array-list? ###

```(array-list? obj)```

Returns ``#t`` if ``obj`` is an array list, and ``#f`` otherwise.

**Time complexity:** *O(1)* (worst-case)

**Space complexity:** *O(1)* (worst-case)

```(array-list-empty? array-list)```

Returns ``#t`` if ``array-list`` contains no items, and ``#f`` otherwise.

**Time complexity:** *O(1)* (worst-case)

**Space complexity:** *O(1)* (worst-case)

## Accessors ##

### array-list-length ###

```(array-list-length array-list)```

Returns the number of items stored in ``array-list``.

**Time complexity:** *O(1)* (worst-case)

**Space complexity:** *O(1)* (worst-case)

### array-list-ref ###

```(array-list-ref array-list k)```

Returns the ``k``th element of ``array-list`` (zero-indexed). It is an error if
``k`` is greater than, or equal to, ``(array-list-length array-list)``.

**Time complexity:** *O(1)* (worst-case)

**Space complexity:** *O(1)* (worst-case)

## Mutators ##

### array-list-delete! ###

```(array-list-delete! array-list)```
```(array-list-delete! array-list k)```

Deletes the element at position ``k`` in ``array-list`` (zero-indexed). If ``k``
is not provided, deletes the last element of ``array-list`` instead. Elements at
positions after ``k`` are 'slid over' to fill the 'gap' left by deleting the
element. It is an error if ``k`` is greater than, or equal to 
``(array-list-length array-list)``.

**Time complexity:** *O(n)* (worst-case), *O(1)* (amortized, when deleting last
element)

**Space complexity:** *O(n)* (worst-case), *O(1)* (amortized)

### array-list-insert! ###

```(array-list-insert! array-list obj)```
```(array-list-insert! array-list k obj)```

Inserts ``obj`` at position ``k`` in ``array-list`` (zero-indexed). If ``k`` is
not provided, inserts ``obj`` at the end of ``array-list``. Elements are `slid
over` away from the ``k``th position to create a 'gap' for ``obj`` if necessary.
It is an error if ``k`` is greater than ``(array-list-length array-list)``.

**Time complexity:** *O(n)* (worst-case), *O(1)* (amortized, when inserting at
the end)

**Space complexity:** *O(n)* (worst-case), *O(1)* (amortized)

### array-list-set! ###

```(array-list-set! array-list k obj)```

Replaces the element at position ``k`` in ``array-list`` with ``obj``
(zero-indexed). It is an error if ``k`` is greater than, or equal to,
``(array-list-length array-list)``.

**Time complexity:** *O(1)* (worst-case)

**Space complexity:** *O(1)* (worst-case)

[1]: https://en.wikipedia.org/wiki/Dynamic_array
