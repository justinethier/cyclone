# Transforms Library

The `(scheme cyclone transforms)` library performs Scheme-to-Scheme transformations, and also contains various utility functions used by the compiler.

- [`*defined-macros*          `](#*defined-macros)
- [`*do-code-gen*             `](#*do-code-gen)
- [`*trace-level*             `](#*trace-level)
- [`*primitives*              `](#*primitives)
- [`get-macros                `](#get-macros)
- [`built-in-syms             `](#built-in-syms)
- [`trace                     `](#trace)
- [`trace:error               `](#traceerror)
- [`trace:warn                `](#tracewarn)
- [`trace:info                `](#traceinfo)
- [`trace:debug               `](#tracedebug)
- [`cyc:error                 `](#cycerror)
- [`basename                  `](#basename)
- [`list-index                `](#list-index)
- [`symbol<?                  `](#symbol)
- [`insert                    `](#insert)
- [`remove                    `](#remove)
- [`union                     `](#union)
- [`difference                `](#difference)
- [`reduce                    `](#reduce)
- [`azip                      `](#azip)
- [`assq-remove-key           `](#assq-remove-key)
- [`assq-remove-keys          `](#assq-remove-keys)
- [`let?                      `](#let)
- [`let->bindings             `](#let-bindings) 
- [`let->exp                  `](#let-exp)
- [`let->bound-vars           `](#let-bound-vars)
- [`let->args                 `](#let-args)
- [`letrec?                   `](#letrec)
- [`letrec->bindings          `](#letrec-bindings)
- [`letrec->exp               `](#letrec-exp)
- [`letrec->bound-vars        `](#letrec-bound-vars)
- [`letrec->args              `](#letrec-args)
- [`lambda-num-args           `](#lambda-num-args)
- [`ast:lambda-formals-type   `](#astlambda-formals-type)
- [`ast:lambda-formals->list  `](#astlambda-formals-list)
- [`list->lambda-formals      `](#list-lambda-formals)
- [`list->pair                `](#list-pair)
- [`app->fun                  `](#app-fun) 
- [`app->args                 `](#app-args) 
- [`precompute-prim-app?      `](#precompute-prim-app)
- [`begin->exps               `](#begin-exps)
- [`define-lambda?            `](#define-lambda)
- [`define->lambda            `](#define-lambda-1) 
- [`closure?                  `](#closure)
- [`closure->lam              `](#closure-lam) 
- [`closure->env              `](#closure-env)
- [`closure->fv               `](#closure-fv)
- [`env-make?                 `](#env-make)
- [`env-make->id              `](#env-make-id)
- [`env-make->fields          `](#env-make-fields)
- [`env-make->values          `](#env-make-values)
- [`env-get?                  `](#env-get)
- [`env-get->id               `](#env-get-id)
- [`env-get->field            `](#env-get-field)
- [`env-get->env              `](#env-get-env)
- [`set-cell!?                `](#set-cell)
- [`set-cell!->cell           `](#set-cell-cell)
- [`set-cell!->value          `](#set-cell-value)
- [`cell?                     `](#cell)
- [`cell->value               `](#cell-value)
- [`cell-get?                 `](#cell-get)
- [`cell-get->cell            `](#cell-get-cell)
- [`expand                    `](#expand)
- [`expand-lambda-body        `](#expand-lambda-body)
- [`let=>lambda               `](#letlambda)
- [`isolate-globals           `](#isolate-globals)
- [`has-global?               `](#has-global)
- [`global-vars               `](#global-vars) 
- [`filter-unused-variables   `](#filter-unused-variables)
- [`free-vars                 `](#free-vars)
- [`clear-mutables            `](#clear-mutables)
- [`mark-mutable              `](#mark-mutable)
- [`is-mutable?               `](#is-mutable)
- [`analyze-mutable-variables `](#analyze-mutable-variables)
- [`wrap-mutables             `](#wrap-mutables)
- [`alpha-convert             `](#alpha-convert)
- [`cps-convert               `](#cps-convert)
- [`pos-in-list               `](#pos-in-list)
- [`closure-convert           `](#closure-convert)

#\*defined-macros\* 
#\*do-code-gen\*
#\*trace-level\*
#\*primitives\*
#get-macros
#built-in-syms
#trace
#trace:error
#trace:warn
#trace:info
#trace:debug
#cyc:error
#basename
#list-index
#symbol<? 
#insert
#remove 
#union 
#difference 
#reduce 
#azip 
#assq-remove-key 
#assq-remove-keys 
#let? 
#let->bindings 
#let->exp 
#let->bound-vars 
#let->args 
#letrec? 
#letrec->bindings 
#letrec->exp 
#letrec->bound-vars 
#letrec->args 
#lambda-num-args
#ast:lambda-formals-type
#ast:lambda-formals->list
#list->lambda-formals 
#list->pair 
#app->fun 
#app->args 
#precompute-prim-app? 
#begin->exps 
#define-lambda? 
#define->lambda 
#closure? 
#closure->lam 
#closure->env 
#closure->fv 
#env-make? 
#env-make->id 
#env-make->fields 
#env-make->values 
#env-get? 
#env-get->id 
#env-get->field 
#env-get->env
#set-cell!? 
#set-cell!->cell 
#set-cell!->value 
#cell? 
#cell->value 
#cell-get? 
#cell-get->cell 
#expand 
#expand-lambda-body
#let=>lambda 
#isolate-globals 
#has-global? 
#global-vars 
#filter-unused-variables 
#free-vars 
#clear-mutables
#mark-mutable
#is-mutable? 
#analyze-mutable-variables 
#wrap-mutables 
#alpha-convert 
#cps-convert 
#pos-in-list 
#closure-convert 
