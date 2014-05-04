; top-level-eval evaluates a form in the global environment

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x)
    ;(display (parse-exp x)) 
    (top-level-eval (parse-exp x))))

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form (empty-env-record))))

; eval-exp is the main component of the interpreter

(define eval-exp
  (lambda (exp env)
    ;(display exp)
    (cases expression exp
      [lit-exp (datum) datum]
      [var-exp (id)
        (apply-env env id; look up its value.
           (lambda (x) x) ; procedure to call if id is in the environment 
           (lambda () ; procedure to call if id not in env
            (apply-env global-env id identity-proc (lambda ()
              (eopl:error 'apply-env "variable not found in environment: ~s" id)))))]
      [if-exp (test then)
        (if (eval-exp test env)
          (eval-exp then env))]
      [if-alt-exp (test first second)
        (if (eval-exp test env)
          (eval-exp first env)      
          (eval-exp second env))]     
      [let-exp (vars values body)
        (apply begin-eval 
          (map (lambda (x) (eval-exp x (extend-env vars 
            (map (lambda (x) (eval-exp x env)) values) env)))
          body))] 
      [app-exp (rator rands)
        (let ([proc-value (eval-exp rator env)]
              [args (eval-rands rands env)])
          (apply-proc proc-value args env))]
      [lambda-exp (id body)
        (user-proc id body env)]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-exp x env)) rands)))

(define begin-eval
  (case-lambda
   [(x) x]
   [(x . rest)
      (apply begin-eval rest)]))

(define *prim-proc-names* 
  '(+ - * / add1 sub1 = < <= > >= zero? not cons car cdr list null? assq eq? equal?
  atom? length list->vector list? pair? procedure? vector->list vector make-vector
  make-list vector->ref list->ref vector? number? symbol? set-car! set-cdr! vector-set! 
  display newline caar cadr cdar caaar caadr cadar cdaar caddr cdadr cddar cdddr))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args env)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args env)]
      [user-proc (vars body env) 
        (cond
          ([symbol? vars] 
            (let ([new-env (extend-env (list vars) (list args) env)])
              (apply begin-eval (eval-rands body new-env))))
        ;([not (proper-list? vars)]
        ;  (let ([proper-list-vars (improper-lambda-vars vars)])
        ;    (let ([new-env (extend-env proper-list-vars (improper-lambda-args (length proper-list-vars) args) env)])
        ;      (apply begin-eval (eval-rands body new-env)))))
        (else
          (if (not (= (length args) (length vars)))
            (error 'apply-proc "Incorrect number of arguments for the given variables ~s" proc-value) 
            (let ([new-env (extend-env vars args env)])
              (apply begin-eval (eval-rands body new-env))))))]
      [else (error 'apply-proc "Attempt to apply bad procedure: ~s" proc-value)])))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (lambda (prim-proc args env)
    ;(display (list prim-proc args))
    (case prim-proc
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
      [(/) (apply / args)]
      [(add1) (+ (1st args) 1)]
      [(sub1) (- (1st args) 1)]
      [(=) (= (1st args) (2nd args))]
      [(<) (< (1st args) (2nd args))]
      [(<=) (<= (1st args) (2nd args))]
      [(>) (> (1st args) (2nd args))]
      [(>=) (>= (1st args) (2nd args))]
      [(zero?) (zero? (1st args))]
      [(not) (not (1st args))]
      [(cons) (cons (1st args) (2nd args))]
      [(car) (1st (1st args))]
      [(cdr) (cdr (1st args))]
      [(list) args]
      [(null?) (null? (1st args))]
      [(assq) (assq (1st args) (2nd args))]
      [(eq?) (eq? (1st args) (2nd args))]
      [(equal?) (equal? (1st args) (2nd args))]
      [(atom?) (atom? (1st args))]
      [(length) (length (1st args))]
      [(list->vector) (list->vector (1st args))]
      [(list?) (list? (1st args))]
      [(pair?) (pair? (1st args))]
      [(procedure?) (proc-val? (1st args))]
      [(vector->list) (vector->list (1st args))]
      [(vector) (apply vector args)]
      [(make-vector) (make-vector (1st args) (2nd args))]
      [(make-list) (make-list (1st args) (2nd args))]
      [(vector-ref) (vector-ref (1st args) (2nd args))]
      [(list-ref) (list-ref (1st args) (2nd args))]
      [(vector?) (vector? (1st args))]
      [(number?) (number? (1st args))]
      [(symbol?) (symbol? (1st args))]
      [(set-car!) (set-car! (1st args) (2nd args))]
      [(set-cdr!) (set-cdr! (1st args) (2nd args))]
      [(vector-set!) (vector-set! (1st args) (2nd args) (3rd args))]
      [(display) (display (1st args))]
      [(newline) (newline (1st args))]
      [(caar) (caar (1st args))]
      [(cadr) (cadr (1st args))]
      [(cdar) (cdar (1st args))]
      [(caaar) (caaar (1st args))]
      [(caadr) (caadr (1st args))]
      [(cadar) (cadar (1st args))]
      [(cdaar) (cdaar (1st args))]
      [(caddr) (caddr (1st args))]
      [(cdadr) (cdadr (1st args))]
      [(cddar) (cddar (1st args))]
      [(cdddr) (cdddr (1st args))]
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-op)])))

;; Helper functions

(define identity-proc (lambda (x) x))
(define void-proc (void))
(define global-env init-env)







