# Util Library

The `(scheme cyclone util`) library contains various utility functions.

tagged-list?
if?
begin?
lambda?
pair->list 
formals->list
lambda-formals->list
lambda-varargs?
lambda->formals
lambda->exp 
lambda-formals-type
lambda-varargs-var
pack-lambda-arguments
if->condition 
if->then 
if-else? 
if->else 
const? 
ref? 
quote? 
define-c?
set!? 
set!->var 
set!->exp 
define? 
define->var 
define->exp 
app?
;; Environments
env:enclosing-environment
env:first-frame
env:the-empty-environment
env:make-frame
env:frame-variables
env:frame-values 
env:add-binding-to-frame! 
env:all-variables
env:all-values
env:extend-environment 
env:lookup
env:lookup-variable-value 
env:_lookup-variable-value 
env:set-variable-value! 
env:define-variable! 
;; Syntactic closures
make-syntactic-closure
strip-syntactic-closures
identifier->symbol
identifier?
identifier=?
;; ER macro supporting functions
Cyc-er-rename
Cyc-er-compare?
;; Code generation
mangle
mangle-global
;; Scheme library functions
gensym
delete
delete-duplicates
flatten
length/obj
list-index2
list-insert-at!
list-prefix?
string-replace-all
take
filter

