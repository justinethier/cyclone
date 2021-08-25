# Transforms Library

The `(scheme cyclone transforms)` library performs Scheme-to-Scheme transformations, and also contains various utility functions used by the compiler.

*This library is used internally by the compiler and its API may change at any time.*

- [`*defined-macros*          `](#*defined-macros)
- [`*do-code-gen*             `](#*do-code-gen)
- [`*primitives*              `](#*primitives)
- [`*trace-level*             `](#*trace-level)
- [`alpha-convert             `](#alpha-convert)
- [`analyze-mutable-variables `](#analyze-mutable-variables)
- [`app->args                 `](#app-args) 
- [`app->fun                  `](#app-fun) 
- [`assq-remove-key           `](#assq-remove-key)
- [`assq-remove-keys          `](#assq-remove-keys)
- [`ast:lambda-formals->list  `](#astlambda-formals-list)
- [`ast:lambda-formals-type   `](#astlambda-formals-type)
- [`azip                      `](#azip)
- [`basename                  `](#basename)
- [`begin->exps               `](#begin-exps)
- [`built-in-syms             `](#built-in-syms)
- [`cell->value               `](#cell-value)
- [`cell-get->cell            `](#cell-get-cell)
- [`cell-get?                 `](#cell-get)
- [`cell?                     `](#cell)
- [`clear-mutables            `](#clear-mutables)
- [`closure->env              `](#closure-env)
- [`closure->fv               `](#closure-fv)
- [`closure->lam              `](#closure-lam) 
- [`closure-convert           `](#closure-convert)
- [`closure?                  `](#closure)
- [`cps-convert               `](#cps-convert)
- [`cyc:error                 `](#cycerror)
- [`define->lambda            `](#define-lambda) 
- [`define-lambda?            `](#define-lambda-1)
- [`difference                `](#difference)
- [`env-get->env              `](#env-get-env)
- [`env-get->field            `](#env-get-field)
- [`env-get->id               `](#env-get-id)
- [`env-get?                  `](#env-get)
- [`env-make->fields          `](#env-make-fields)
- [`env-make->id              `](#env-make-id)
- [`env-make->values          `](#env-make-values)
- [`env-make?                 `](#env-make)
- [`filter-unused-variables   `](#filter-unused-variables)
- [`free-vars                 `](#free-vars)
- [`get-macros                `](#get-macros)
- [`global-vars               `](#global-vars) 
- [`has-global?               `](#has-global)
- [`insert                    `](#insert)
- [`is-mutable?               `](#is-mutable)
- [`isolate-globals           `](#isolate-globals)
- [`lambda-num-args           `](#lambda-num-args)
- [`let->args                 `](#let-args)
- [`let->bindings             `](#let-bindings) 
- [`let->bound-vars           `](#let-bound-vars)
- [`let->exp                  `](#let-exp)
- [`let=>lambda               `](#letlambda)
- [`let?                      `](#let)
- [`letrec->args              `](#letrec-args)
- [`letrec->bindings          `](#letrec-bindings)
- [`letrec->bound-vars        `](#letrec-bound-vars)
- [`letrec->exp               `](#letrec-exp)
- [`letrec?                   `](#letrec)
- [`list->lambda-formals      `](#list-lambda-formals)
- [`list->pair                `](#list-pair)
- [`list-index                `](#list-index)
- [`mark-mutable              `](#mark-mutable)
- [`pos-in-list               `](#pos-in-list)
- [`precompute-prim-app?      `](#precompute-prim-app)
- [`reduce                    `](#reduce)
- [`remove                    `](#remove)
- [`set-cell!->cell           `](#set-cell-cell)
- [`set-cell!->value          `](#set-cell-value)
- [`set-cell!?                `](#set-cell)
- [`symbol<?                  `](#symbol)
- [`trace                     `](#trace)
- [`trace:debug               `](#tracedebug)
- [`trace:error               `](#traceerror)
- [`trace:info                `](#traceinfo)
- [`trace:warn                `](#tracewarn)
- [`union                     `](#union)
- [`wrap-mutables             `](#wrap-mutables)

# \*defined-macros\* 

# \*do-code-gen\*

# \*primitives\*

# \*trace-level\*

# alpha-convert 

# analyze-mutable-variables 

# app->args 

# app->fun 

# assq-remove-key 

# assq-remove-keys 

# ast:lambda-formals->list

# ast:lambda-formals-type

# azip 

# basename

# begin->exps 

# built-in-syms

# cell->value 

# cell-get->cell 

# cell-get? 

# cell? 

# clear-mutables

# closure->env 

# closure->fv 

# closure->lam 

# closure-convert 

# closure? 

# cps-convert 

# cyc:error

# define->lambda 

# define-lambda? 

# difference 

# env-get->env

# env-get->field 

# env-get->id 

# env-get? 

# env-make->fields 

# env-make->id 

# env-make->values 

# env-make? 

# filter-unused-variables 

# free-vars 

# get-macros

# global-vars 

# has-global? 

# insert

# is-mutable? 

# isolate-globals 

# lambda-num-args

# let->args 

# let->bindings 

# let->bound-vars 

# let->exp 

# let=>lambda 

# let? 

# letrec->args 

# letrec->bindings 

# letrec->bound-vars 

# letrec->exp 

# letrec? 

# list->lambda-formals 

# list->pair 

# list-index

# mark-mutable

# pos-in-list 

# precompute-prim-app? 

# reduce 

# remove 

# set-cell!->cell 

# set-cell!->value 

# set-cell!? 

# symbol<? 

# trace

# trace:debug

# trace:error

# trace:info

# trace:warn

# union 

# wrap-mutables 

