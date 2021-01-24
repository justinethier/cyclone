# Scheme code conventions

- [Introduction](#introduction)
- [Spacing](#srfi-libraries)
- [Line separation](#line-separation)
- [Indentation and alignment](#indentation-and-alignment)
- [Comments](#comments)
- [Names](#names)
- [Quotes](#quotes)

# Introduction 

This document presents a set of recommendations for Scheme code styling. Use them when possible. 

Most of the recommendations presented here follow, at least partially, [Riastradh's Lisp Style Rules](https://mumble.net/~campbell/scheme/style.txt) and scheme-wiki guides on [general Scheme style](http://community.schemewiki.org/?scheme-style), [variable naming conventions](http://community.schemewiki.org/?variable-naming-convention) and [comment style](http://community.schemewiki.org/?comment-style).

# Spacing

Lists inside lists should always be separated from other elements with **one** space.

  Not recommended:

```scheme
(foo(bar baz)quux)
(foo ( bar baz ) quux)
(foo  (bar baz)  quux)
```

  Recommended:

```scheme
(foo (bar baz) quux)
```

# Line separation

Do **not** place closing parentheses on their own lines. Scheme code is read through its indentation.

  NOT recommended:

```scheme
(define (factorial x)
  (if (< x 2)
      1
      (* x (factorial (- x 1
                      )
           )
      )
  )
)
```

  Recommended:

```scheme
(define (factorial x)
  (if (< x 2)
      1
      (* x (factorial (- x 1)))))
```

An exception may be when commenting out one line of code:

```scheme
(define (foo bar)
  (list (frob bar)
        (zork bar)
        ;; (zap bar)
        ))
```

But this is useful **only in development code**. For production code (ie. a package submission, Cyclone's code pull requests, etc) clean your code whenever possible.

A reasonable *permanent* exception is when dealing with long lists made to be expanded by *other people*, especially in files under version control. This eases the maintenance of the lists and clarifies version diffs. 

```scheme
(define colour-names         ;More colors are welcomed!
  '(blue
    cerulean
    green
    magenta
    purple
    red
    scarlet
    turquoise
    ))
```

# Indentation and alignment

We assume the following syntatic naming convention of elements in a list: 

    (operator operand1 operand2 ...)

So in `(+ 1 2 3 4)`, `+` is the `operator` and the numbers are the `operand`s.

For most `operator`s, `operand1` determines the alignment of the other `operand`s.

  NOT recommended:

```scheme
(+ (sqrt -1)
  (* x y)
  (+ p q))
```

  Recommended:

```scheme
(+ (sqrt -1)
   (* x y)
   (+ p q))

(cond ((null? l) (call-some-procedure))
      ((null? (car l)) (call-other-procedure))
      (else (recur-in-some-way))

(if (this-is-too-long-to-go-in-one-line)
    (do-this)
    (do-other-things))
```

If `operand1` is put in a different line from `operand`'s, it should be generally bellow `operand` (exceptions are discussed soon).

  NOT recommended:

```scheme
(+
   (sqrt -1)
   (* x y)
   (+ p q))
```

  Recommended:

```scheme
(+
 (sqrt -1)
 (* x y)
 (+ p q))

(cond 
 ((null? l) 
  (call-some-procedure))
 ((null? (car l)) 
  (call-other-procedure))
 (else (recur-in-some-way))

(if  ;not that recommended, but acceptable
 (this-is-too-long-to-go-in-one-line)
 (do-this)
 (do-other-things))
```

The same applies when the `operator` is a list, so 

  NOT recommended:

```scheme
((car x)
   (cdr x)
   foo)

((car x) (cdr x)
 foo)
```

  Recommended:

```scheme
((car x)
 (cdr x)
 foo)

((car x) (cdr x)
         foo)
```

There are special `operator`s with which `operand`s may be aligned differently. Usually this means that `operand1` will be placed bellow `operator`'s *second* letter. Here is a non-exhaustive list of examples:

```scheme
(lambda (x y)
  (+ x y))
    
(define (test x)
  (assert x))

(define *other-features* 
  '(r7rs 
    ieee-float
    full-unicode
    posix))

(define-library (cyclone lib)
  (import (scheme cyclone util))
  (export proc1
          var1
          macro1
          var2)
  ...)
    
(begin 
  (do-something)
  (do-something-else))

(define-syntax example
  (er-macro-transformer
    (lambda (expr rename compare)
      (cond ((null? (cdr expr)) #t)
            (else (list (rename 'if) 
                        (cons (rename 'and) (cddr expr))
                        #f))))))

(when a
  (do-b))
    
(unless c
  (call-d))
```
    
When working if long lists of literal data, consider the following:

  NOT recommended:

```scheme
("foo" "bar" "baz" "quux" "zot"
       "mumble" "frotz" "gargle" "mumph")
```

  Questionable, but acceptable:

```scheme
(3 1 4 1 5 9 2 6 5 3 5 8 9 7 9 3 2 3 8 4 6 2 6 4
   3 3 8 3 2 7 9 5 0 2 8 8 4 1 9 7 1 6 9 3 9 9 3)
```

  Recommended:

```scheme
("foo" "bar" "baz" "quux" "zot"
 "mumble" "frotz" "gargle" "mumph")

("foo"
 "bar" "baz" "quux" "zot"
 "mumble" "frotz" "gargle" "mumph")

(3 1 4 1 5 9 2 6 5 3 5 8 9 7 9 3 2 3 8 4 6 2 6 4
 3 3 8 3 2 7 9 5 0 2 8 8 4 1 9 7 1 6 9 3 9 9 3)
```

# Names

As a convention for the Cyclone project and its packages, symbolic names are written with English words separated by hyphens. 

Camel case names and underscores should NOT be used, except for those variables derived directly from another programming language without translation (i.e. when using FFI, but only when *really* needed).

  NOT recommended:

```scheme
XMLHttpRequest
foreach
append_map
```

  Recommended:

```scheme
xml-http-request
for-each
append-map
```

With regard to name choices, **meaningful names** are the preferred options. Abbreviating words or using contractions is not recommended, even for local variables, except for iteration indexes. To pick good name is an art, but prefer to be verbose rather than obscure.

  NOT recommended:

```scheme
(define (get-nxt-lambda-id!)
  (set! *lambda-id* (+ 1 *lambda-id*))
  *lambda-id*)
```

  Recommended

```scheme
(define (get-next-lambda-id!)
  (set! *lambda-id* (+ 1 *lambda-id*))
  *lambda-id*)
```    
    
## Scheme naming conventions

### Predicates (?)

Procedures or macros ending with a question mark return a **boolean**. They are called *predicates*.

  Examples:

```scheme
pair? procedure? proper-list?
```

Do NOT use a question mark if the procedure may return any object other than a boolean.

  The procedures bellow are non-examples, because although they interrogate their `operand`s, they don't return `#t` or `#f`:

```scheme
member assoc any every
```

### Destructive operations (!)

Procedures or macros ending with an exclamation mark aim the modification an object. Such procedures are called *destructive*.

  Examples: 

```scheme
set-car! append!
```

Do NOT use the exclamation mark for all procedures that cause mutation or side effects. Use it to identify procedures that exist **solely** for the purpose of destructive update (e.g. `set-car!`), or to distinguish a destructive (e.g. `append!`) variant of a procedure of which there also exists a purely functional variant (e.g. `append`).

### Variants (*)

It is possible to affix an asterisk to the end of a name to make a variation on a theme of the original name.

  Example: `let` -> `let*`

But **prefer a meaningful name over an asterisk**. The asterisk does not explain what the variation means.

### Conversions (->)

Use the 'hyphen' followed by the 'greater than' sign to form an *arrow* to designate type/representation conversion.

Example:

```scheme
string->number
symbol->string
```

### Mutable globals (* *)

Use one asterisk before and another after a name to represent a global, mutable variable/parameter.

```scheme
*main-path*
```

### Global constants (UPPERCASE or + +)

Global constants could be defined either way:

```scheme
INPUT-CODE
+input-code+
```

### Namespaces (:)

Use `:` to explicitly note the namespace from which a name comes. **Only** use when it is not possible to define a library (which would otherwise allow the use of `(prefix ...)` import clause) or when, even inside a library, there would be no other way to prevent name clashes.

    foo:procedure

# Comments 

Write comments only where the code is incapable of explaining itself. This is not a recommendation to avoid comments, but rather an incentive to **prefer self-explanatory code over explanatory comments**. 

The convention is as follows: 

- heading comments: at least 4 semicolons;
- top-level comments: 3 semicolons;
- comments on a particular code fragment: before that fragment and aligned with it, with 2 semicolons;
- margin comments: 1 semicolon and it can have more spaces from code and no space between `;` and the comment itself.

  Examples:

```Scheme
;;;; Some information about file and license

;;; This section of code accomplishes 3 main tasks
;;;   1. Foo.
;;;   2. Bar.
;;;   3. Baz.

(define (proc arg)
  ;; Arg may be a string or list of symbols
  (list arg
        +const+             ;added to optimize code
        (\*global-parameter\*))
```

# Quotes

This section includes quotes that are either useful and/or funny.

From [Riastradh's Lisp Style Rules](https://mumble.net/~campbell/scheme/style.txt): 

> Three principles guide this style, roughly ordered according to descending importance:
>  
> 1. The purpose of a program is to describe an idea, and not the way that the idea must be realized; the intent of the program's meaning,
>    rather than peripheral details that are irrelevant to its intent, should be the focus of the program, *irrespective* of whether a
>    human or a machine is reading it.
> 
> 2. The sum of the parts is easier to understand than the whole.
> 
> 3. Aesthetics matters.  No one enjoys reading an ugly program.


> Parenthetical Philosophy
> 
> The actual bracket characters (parentheses) are simply lexical tokens to which little significance should be assigned. Lisp programmers do not examine the brackets individually, or, Azathoth forbid, count brackets; instead they view the higher-level structures expressed in the program, especially as presented by the indentation.  Lisp is not about writing a sequence of serial instructions; it is about building complex structures by summing parts.  The composition of complex structures from parts is the focus of Lisp programs, and it should be readily apparent from the Lisp code.  Placing brackets haphazardly about the presentation is jarring to a Lisp programmer, who otherwise would not even have seen them for the most part.

> Avoid 'literate programming' like the plague.
> 
>   Rationale:  If the code is often incapable of explaining itself, then perhaps it should be written in a more expressive language. This may mean using a different programming language altogether, or, since we are talking about Lisp, it may mean simply building a combinator language or a macro language for the purpose.  `Literate programming' is the logical conclusion of languages incapable of explaining themselves; it is a direct concession of the inexpressiveness of the computer language implementing the program, to the extent that the only way a human can understand the program is by having it rewritten in a human language.

> Naming is subtle and elusive.  Bizarrely, it is simultaneously insignificant, because an object is independent of and unaffected by the many names by which we refer to it, and also of supreme importance, because it is what programming -- and, indeed, almost everything that we humans deal with -- is all about.  A full discussion of the concept of name lies far outside the scope of this document, and could surely fill not even a book but a library.

From scheme-wiki guide on [general Scheme style](http://community.schemewiki.org/?scheme-style):

> Don't put closing (or opening) parens on a line of their own. They get lonely easily. Seriously, it's superfluous information and takes up lines for nothing.

> If [any] subexpression [spans] onto multiple lines (...) put *every* [other] subexpression on a single line. 

> Rule 5: There are no rules. All of these rules are guidelines. If you choose to break one, do so. It's your code. But be aware that you are breaking one, and that you have a good reason for doing so. 
