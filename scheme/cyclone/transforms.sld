(define-library (scheme cyclone transforms)
  (import (scheme base)
          (scheme char)
          (scheme file)
          (scheme eval)
          (scheme read)
          (scheme write)
          (scheme cyclone common)
          (scheme cyclone util)
          (scheme cyclone libraries)
          (scheme cyclone macros)
  )
  (export
    ;*defined-macros* 
    *do-code-gen*
    *trace-level*
    *primitives*
    built-in-syms
    trace
    trace:error
    trace:warn
    trace:info
    trace:debug
    cyc:error
    basename
    list-index
    gensym
    symbol<? 
    insert
    remove 
    union 
    difference 
    reduce 
    azip 
    assq-remove-key 
    assq-remove-keys 
    const? 
    ref? 
    quote? 
    let? 
    let->bindings 
    let->exp 
    let->bound-vars 
    let->args 
    letrec? 
    letrec->bindings 
    letrec->exp 
    letrec->bound-vars 
    letrec->args 
    lambda? 
    lambda-varargs? 
    lambda->formals 
    lambda-varargs? 
    lambda-varargs-var 
    lambda-formals-type 
    lambda-formals->list 
    lambda-num-args
    list->lambda-formals 
    pair->list 
    list->pair 
    lambda->exp 
    if? 
    if->condition 
    if->then 
    if-else? 
    if->else 
    app? 
    app->fun 
    app->args 
    prim? 
    precompute-prim-app? 
    prim-call? 
    begin? 
    begin->exps 
    define? 
    define-lambda? 
    define->lambda 
    define->var 
    define->exp 
    set!? 
    set!->var 
    set!->exp 
    closure? 
    closure->lam 
    closure->env 
    closure->fv 
    env-make? 
    env-make->id 
    env-make->fields 
    env-make->values 
    env-get? 
    env-get->id 
    env-get->field 
    env-get->env
    set-cell!? 
    set-cell!->cell 
    set-cell!->value 
    cell? 
    cell->value 
    cell-get? 
    cell-get->cell 
    macro? 
    macro-expand 
    expand 
    let=>lambda 
    letrec=>lets+sets 
    begin=>let 
    isolate-globals 
    has-global? 
    global-vars 
    filter-unused-variables 
    free-vars 
    clear-mutables
    mark-mutable
    is-mutable? 
    analyze-mutable-variables 
    wrap-mutables 
    alpha-convert 
    cps-convert 
    pos-in-list 
    closure-convert 
  )
  (include "transforms.scm"))

