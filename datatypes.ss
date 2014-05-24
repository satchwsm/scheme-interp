
;; Parsed expression datatypes

(define-datatype expression expression?  
  (lit-exp
    (id (lambda (x) #t)))
  (var-exp
    (id symbol?))
  (lambda-exp
    (id (lambda (x) (or (null? x) (symbol? x) (pair? x) (ref? x))))
    (body listed-expression?))
  (lambda-ref-exp
    (id (lambda (x) (or (null? x) (symbol? x) (pair? x) (ref? x))))
    (body listed-expression?))
  (app-exp
    (rator expression?)
    (rand listed-expression?))
  (if-exp
    (test expression?)
    (then expression?))
  (if-alt-exp
    (test expression?)
    (first expression?)
    (second expression?))
  (quote-exp
    (data (lambda (x) #t)))
  (set!-exp
    (id symbol?)
    (new-id expression?))
  (let-exp
    (vars list?)
    (values list?)
    (body listed-expression?))
  (let*-exp
    (vars list?)
    (values list?)
    (body listed-expression?))
  (letrec-exp
    (vars list?)
    (values list?)
    (body listed-expression?))
  (let-named-exp
    (let-name symbol?)
    (vars list?)
    (values list?)
    (expression listed-expression?))
  (begin-exp
    (body listed-expression?))
  (and-exp
    (conds listed-expression?))
  (or-exp
    (conds listed-expression?))
  (cond-exp
    (cases listed-expression?)
    (exps listed-expression?))
  (case-exp
    (test expression?)
    (cases listed-expression?)
    (vals listed-expression?))
  (while-exp
    (test expression?)
    (cases listed-expression?))
  (varassign-exp
    (id symbol?)
    (exp expression?))
  (define-exp
    (id symbol?)
    (exp expression?))
)

(define ref?
  (lambda (x)
    (and (list? x) (eqv? 'ref (car x)) (symbol? (cadr x)))))

(define listed-expression?
  (lambda (ls)
    (or (null? ls) (and (pair? ls) (expression? (car ls)) (listed-expression? (cdr ls))))))

;; environment type definitions

(define scheme-value?
  (lambda (x) #t))
   	
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
    (syms (list-of symbol?))
    (cells (list-of box?))
    (env environment?)))

; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [user-proc
   (vars (lambda (x) (or (list? x) (pair? x) (symbol? x))))
   (body listed-expression?)
   (env environment?)]
  [ref-proc
    (vars (lambda (x) (or (list? x) (pair? x) (symbol? x) (ref? x))))
    (body listed-expression?)
    (env environment?)]
  [continuation-proc
    (k continuation?)])
	
; Continuation datatype
(define-datatype continuation continuation?
  [repl-k]
  [lit-k 
    (datum scheme-value?)]
  [var-k 
    (datum symbol?)]
  [test-k
    (then-exp expression?)
    (else-exp expression?)
    (env environment?)
    (k continuation?)]
  [rator-k
    (rands (list-of expression?))
    (env environment?)
    (k continuation?)]
  [rands-k
    (proc-value scheme-value?)
    (env environment?)
    (k continuation?)]
  [let-rands-k 
    (body expression?)
    (vars (list-of symbol?))
    (env environment?)
    (k continuation?)]
  [env-k 
    (body expression?)
    (k continuation?)]
  [eval-rands-k
    (rands (list-of expression?))
    (env environment?)
    (k continuation?)]
  [cons-k
    (item scheme-value?)
    (k continuation?)]
)
