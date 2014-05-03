
;; Parsed expression datatypes

(define-datatype expression expression?  
  (lit-exp
    (id (lambda (x) (or (number? x) (symbol? x) (string? x) (boolean? x) (vector? x) (null? x)))))
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
    (data (lambda (x) (or (number? x) (symbol? x) (string? x) (boolean? x) (vector? x) (null? x)))))
  (set!-exp
    (id symbol?)
    (new-id expression?))
  (let-exp
    (let-type symbol?)
    (vars expression?)
    (expression listed-expression?))
  (let-named-exp
    (let-type symbol?)
    (let-name symbol?)
    (vars expression?)
    (expression listed-expression?))
)

(define listed-expression?
  (lambda (ls)
    (or (null? ls) (and (pair? ls) (expression? (car ls)) (listed-expression? (cdr ls))))))

(define-datatype expression expression?
  [var-exp        ; variable references
   (id symbol?)]
  [lit-exp        ; "Normal" data.  Did I leave out any types?
   (datum
    (lambda (x)
      (ormap 
       (lambda (pred) (pred x))
       (list number? vector? boolean? symbol? string? pair? null?))))]
  [app-exp        ; applications
   (rator expression?)
   (rands (list-of expression?))]  
  )

	
; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)])
	
;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))