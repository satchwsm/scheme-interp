
;; Parsed expression datatypes

(define-datatype expression expression?  
  (lit-exp
    (id (lambda (x) #t)))
  (var-exp
    (id symbol?))
  (lambda-exp
    (id (lambda (x) (or (null? x) (symbol? x) (pair? x))))
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
    (body listed-expression))
  (let-named-exp
    (let-name symbol?)
    (vars expression?)
    (values expression?)
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
    (cases listed-expression?)
    (else expression?))
)

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
   (vals (list-of scheme-value?))
   (env environment?)))
   	
; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [user-proc
   (vars (lambda (x) (or (list? x) (pair? x) (symbol? x))))
   (body listed-expression?)
   (env environment?)])
	
