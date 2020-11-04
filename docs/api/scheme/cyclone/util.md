# Util Library

The `(scheme cyclone util`) library contains various utility functions used internally the compiler.

- [`Cyc-er-compare?            `](#cyc-er-compare)
- [`Cyc-er-rename              `](#cyc-er-rename)
- [`app?                       `](#app)
- [`begin?                     `](#begin)
- [`const?                     `](#const) 
- [`define->exp                `](#define-exp) 
- [`define->var                `](#define-var)
- [`define-c?                  `](#define-c)
- [`define?                    `](#define)
- [`delete                     `](#delete)
- [`delete-duplicates          `](#delete-duplicates)
- [`env:_lookup-variable-value `](#envlookup-variable-value)
- [`env:add-binding-to-frame!  `](#envadd-binding-to-frame)
- [`env:all-values             `](#envall-values)
- [`env:all-variables          `](#envall-variables)
- [`env:define-variable!       `](#envdefine-variable) 
- [`env:enclosing-environment  `](#envenclosing-environment)
- [`env:extend-environment     `](#envextend-environment)
- [`env:first-frame            `](#envfirst-frame)
- [`env:frame-values           `](#envframe-values) 
- [`env:frame-variables        `](#envframe-variables)
- [`env:lookup                 `](#envlookup)
- [`env:lookup-variable-value  `](#envlookup-variable-value)
- [`env:make-frame             `](#envmake-frame)
- [`env:set-variable-value!    `](#envset-variable-value)
- [`env:the-empty-environment  `](#envthe-empty-environment)
- [`filter                     `](#filter)
- [`flatten                    `](#flatten)
- [`formals->list              `](#formals-list)
- [`gensym                     `](#gensym)
- [`identifier->symbol         `](#identifier-symbol)
- [`identifier=?               `](#identifier)
- [`identifier?                `](#identifier-1)
- [`if->condition              `](#if-condition)
- [`if->else                   `](#if-else)
- [`if->then                   `](#if-then)
- [`if-else?                   `](#if-else) 
- [`if?                        `](#if)
- [`lambda->exp                `](#lambda-exp)
- [`lambda->formals            `](#lambda-formals)
- [`lambda-formals->list       `](#lambda-formals-list)
- [`lambda-formals-type        `](#lambda-formals-type)
- [`lambda-varargs-var         `](#lambda-varargs-var)
- [`lambda-varargs?            `](#lambda-varargs)
- [`lambda?                    `](#lambda)
- [`length/obj                 `](#lengthobj)
- [`list-index2                `](#list-index2)
- [`list-insert-at!            `](#list-insert-at)
- [`list-prefix?               `](#list-prefix)
- [`mangle                     `](#mangle)
- [`mangle-global              `](#mangle-global)
- [`pack-lambda-arguments      `](#pack-lambda-arguments)
- [`pair->list                 `](#pair-list)
- [`quote?                     `](#quote) 
- [`ref?                       `](#ref)
- [`set!->exp                  `](#set-exp)
- [`set!->var                  `](#set-var)
- [`set!?                      `](#set)
- [`string-join                `](#string-join)
- [`string-replace-all         `](#string-replace-all)
- [`string-split               `](#string-split)
- [`tagged-list?               `](#tagged-list?)
- [`take                       `](#take)

# Cyc-er-compare?

# Cyc-er-rename

# app?

# begin?

# const? 

# define->exp 

# define->var 

# define-c?

# define? 

# delete

# delete-duplicates

# env:\_lookup-variable-value 

# env:add-binding-to-frame! 

# env:all-values

# env:all-variables

# env:define-variable! 

# env:enclosing-environment

# env:extend-environment 

# env:first-frame

# env:frame-values 

# env:frame-variables

# env:lookup

# env:lookup-variable-value 

# env:make-frame

# env:set-variable-value! 

# env:the-empty-environment

# filter

# flatten

# formals->list

# gensym

# identifier->symbol

# identifier=?

# identifier?

# if->condition 

# if->else 

# if->then 

# if-else? 

# if?

# lambda->exp 

# lambda->formals

# lambda-formals->list

# lambda-formals-type

# lambda-varargs-var

# lambda-varargs?

# lambda?

# length/obj

# list-index2

# list-insert-at!

# list-prefix?

# mangle

# mangle-global

# pack-lambda-arguments

# pair->list 

# quote? 

# ref? 

# set!->exp 

# set!->var 

# set!? 

# string-join

    (string-join list deliminator)

Create a single string from a list of strings, adding `deliminator` between each of the strings. `deliminator` may be either a character or string.

# string-replace-all

# string-split

    (string-split string deliminator)

Create a list of strings from the given string, creating a new one at each instance of the `deliminator` character.


# tagged-list?

# take

