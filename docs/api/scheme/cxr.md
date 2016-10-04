# CxR Library

The `(scheme cxr)` library exports twenty-four procedures which are the compositions of from three to four `car` and `cdr` operations. For example `caddar` is defined by:

    (define caddar
      (lambda (x) (car (cdr (cdr (car x))))))

The procedures `car` and `cdr` themselves and the four two-level compositions are included in the base library.

For more information see the [R<sup>7</sup>RS Scheme Specification](../../r7rs.pdf).

- [`caaar`](#caaar)
- [`caadr`](#caadr)
- [`cadar`](#cadar)
- [`caddr`](#caddr)
- [`cdaar`](#cdaar)
- [`cdadr`](#cdadr)
- [`cddar`](#cddar)
- [`cdddr`](#cdddr)
- [`caaaar`](#caaaar)
- [`caaadr`](#caaadr)
- [`caadar`](#caadar)
- [`caaddr`](#caaddr)
- [`cadaar`](#cadaar)
- [`cadadr`](#cadadr)
- [`caddar`](#caddar)
- [`cadddr`](#cadddr)
- [`cdaaar`](#cdaaar)
- [`cdaadr`](#cdaadr)
- [`cdadar`](#cdadar)
- [`cdaddr`](#cdaddr)
- [`cddaar`](#cddaar)
- [`cddadr`](#cddadr)
- [`cdddar`](#cdddar)
- [`cddddr`](#cddddr)

#caaar

    (caaar list)

#caadr

    (caadr list)

#cadar

    (cadar list)

#caddr

    (caddr list)

#cdaar

    (cdaar list)

#cdadr

    (cdadr list)

#cddar

    (cddar list)

#cdddr

    (cdddr list)

#caaaar

    (caaaar list)

#caaadr

    (caaadr list)

#caadar

    (caadar list)

#caaddr

    (caaddr list)

#cadaar

    (cadaar list)

#cadadr

    (cadadr list)

#caddar

    (caddar list)

#cadddr

    (cadddr list)

#cdaaar

    (cdaaar list)

#cdaadr

    (cdaadr list)

#cdadar

    (cdadar list)

#cdaddr

    (cdaddr list)

#cddaar

    (cddaar list)

#cddadr

    (cddadr list)

#cdddar

    (cdddar list)

#cddddr

    (cddddr list)

