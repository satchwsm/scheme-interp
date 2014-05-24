; top-level-eval evaluates a form in the global environment

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([read (read)])
      (if (eqv? 'exit read)
        (void)
        (let ([answer (top-level-eval (syntax-expand (parse-exp (read))))])
          (eopl:pretty-print answer) (newline)
          (rep))))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x)
    ;(display (parse-exp x)) 
    (top-level-eval (syntax-expand (parse-exp x)))))

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form (empty-env-record) (repl-k)))) ;; TODO add k

; eval-exp is the main component of the interpreter

(define eval-exp
  (lambda (exp env k)
    (cases expression exp
      [lit-exp (datum) (apply-k k datum)]
      [var-exp (id) 
        (apply-env env id; look up its value.
           k ; procedure to call if id is in the environment 
           (lambda () ; procedure to call if id not in env
            (apply-env-ref global-env id (lambda ()
              (eopl:error 'apply-env-ref "variable not found in environment: ~s" id)))))]
      [if-exp (test then)
        (if (eval-exp test env k)
          (eval-exp then env k)
          (void))]
      [if-alt-exp (test first second)
        (if (eval-exp test env k)
          (eval-exp first env k)      
          (eval-exp second env k))]     
      [let-exp (vars values body) ; this is never used because lets are expanded into lambdas
        (apply begin-eval 
          (map (lambda (x) (eval-exp x (extend-env vars 
            (map box (map (lambda (x) (eval-exp x env k)) values)) env k)))
          body))] 
      [app-exp (rator rands)
        (eval-exp rator env (rator-k rands env k))]
      [lambda-exp (id body)
        (apply-k k (user-proc id body env))]
      [lambda-ref-exp (id body)
        (apply-k k (ref-proc id body env))]
      [while-exp (test cases)
        (if (not (eval-exp test env k))
          (void)
          (begin (eval-body cases env k) (eval-exp exp env k)))]
      [varassign-exp (id expr)
        (let ((ref (apply-env-ref env id (lambda () ; procedure to call if id not in env
            (apply-env-ref global-env id (lambda ()
              (eopl:error 'apply-env-ref "variable not found in environment: ~s" id))))))
            (res (eval-exp expr env)))
          (if (and (not (null? (unbox ref))) (list? (unbox ref)) (eqv? (car (unbox ref)) 'ref))
            (let ((r (apply-env-ref env (cadr (unbox ref)) (lambda () (apply-env-ref global-env (cadr (unbox ref))
                (eopl:error 'apply-env-ref "variable not found in environment: ~s" id))))))
              (if (ref? (unbox r))
                (set-ref! (apply-env-ref env (cadr (unbox r)) (lambda () (apply-env-ref global-env (cadr (unbox r))
                  (eopl:error 'apply-env-ref "variable not found in environment: ~s" id)))) res)
                (set-ref! r res)))
            (set-ref! ref res)))]
      [define-exp (id expr)
        (let ((ref-result (apply-env-ref env id (lambda () #f)))
              (e-result (eval-exp expr env k)))
          (if ref-result
            (set-ref! ref-result e-result)
            (set! global-env (define-new-cell id e-result))))]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list
(define eval-rands
  (lambda (rands env k)
    (if (null? rands)
      (apply-k k '())
      (eval-exp (car rands) env (eval-rands-k (cdr rands) env k)))))

; evaluates the body of a lambda exp in order and returns result in a list
(define eval-body
  (lambda (body env k)
    (let loop ([body body][env env][res '()])
      (if (null? body)
        res
        (loop (cdr body) env (append (list (eval-exp (car body) env k)) res))))))

(define *prim-proc-names* 
  '(+ - * / add1 sub1 = < <= > >= zero? not cons car cdr list null? assq eq? equal?
  atom? length list->vector list? pair? procedure? vector->list vector make-vector
  make-list vector-ref list->ref vector? number? symbol? set-car! set-cdr! vector-set! 
  display newline caar cadr cdar caaar caadr cadar cdaar caddr cdadr cddar cdddr
  void map apply quotient memq eqv? list-tail append set-box! unbox box))

(define init-env
  (extend-env
    *prim-proc-names*
    (map box (map prim-proc *prim-proc-names*))
    (empty-env)))

; Apply-k applies the k continuation
(define apply-k
  (lambda (k val)
    (cases continuation k
      [repl-k () val]
      [lit-k (datum) datum]
      [var-k (datum) datum]
      [test-k (then-exp else-exp env k)
        (if val
          (eval-exp then-exp env k)
          (eval-exp else-exp env k))]
      [rator-k (rands env k)
        (eval-rands rands env (rands-k val env k))]
      [rands-k (proc-value env k)
        (apply-proc proc-value val env k)]
      [let-rands-k (body vars env k)
        (extend-env vars (map box val) env (env-k body k))]
      [eval-rands-k (rands env k) (eval-rands rands env (cons-k val k))]
      [cons-k (item k) (apply-k k (cons item val))]
      [env-k (body k)
        (eval-exp body val k)]
      [else (eopl:error 'eval-exp "Bad continuation syntax: ~a" k)])))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args env k)
    ;(display proc-value)
    (cases proc-val proc-value
      [prim-proc (op) (apply-k k (apply-prim-proc op args env k))]
      [user-proc (vars body env)
        (cond
          ([symbol? vars]
            (let ([new-env (extend-env (list vars) (map box (list args)) env)])
              (apply begin-eval (eval-rands body new-env k))))
          ([not (proper-list? vars)]
            (let ([proper-list-vars (improper-lambda-vars vars)])
              (let ([new-env (extend-env proper-list-vars 
                  (map box (improper-lambda-args (length proper-list-vars) args)) env)])
                (apply begin-eval (eval-rands body new-env k)))))
          (else
            (if (not (= (length args) (length vars)))
              (error 'apply-proc "Incorrect number of arguments for the given variables ~s" proc-value) 
              (let ([new-env (extend-env vars (map box args) env)])
                (car (eval-body body new-env k))))))]
        [ref-proc (vars body env2)
          (if (not (= (length args) (length vars)))
              (error 'apply-proc "Incorrect number of arguments for the given variables ~s" proc-value)
              (let ([new-env (extend-env vars (map box args) env)])
                (car (eval-body body new-env k))))]
        [continuation-proc (k)
          (apply-k k (car args))]
      [else (error 'apply-proc "Attempt to apply bad procedure: ~s" proc-value)])))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (lambda (prim-proc args env k)
    ;(display (list prim-proc args))
    (case prim-proc
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
      [(/) (apply / args)]
      [(quotient) (apply quotient args)]
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
      [(number?) (number? (1st args))]
      [(symbol?) (symbol? (1st args))]
      [(list?) (list? (1st args))]
      [(pair?) (pair? (1st args))]
      [(procedure?) (proc-val? (1st args))]
      [(list->vector) (list->vector (1st args))]
      [(vector->list) (vector->list (1st args))]
      [(vector) (apply vector args)]
      [(vector?) (vector? (1st args))]
      [(make-vector) (make-vector (1st args) (2nd args))]
      [(make-list) (make-list (1st args) (2nd args))]
      [(vector-ref) (vector-ref (1st args) (2nd args))]
      [(list-ref) (list-ref (1st args) (2nd args))]
      [(vector-set!) (vector-set! (1st args) (2nd args) (3rd args))]
      [(set-car!) (set-car! (1st args) (2nd args))]
      [(set-cdr!) (set-cdr! (1st args) (2nd args))]
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
      [(void) (void)]
      [(memq) (memq (1st args) (2nd args))]
      [(map) (map (lambda (x) (apply-proc (car args) (list x) env)) (cadr args))]
      [(apply) (apply-proc (car args) (flatten (cdr args)) env)]
      [(eqv?) (eqv? (1st args) (2nd args))]
      [(list-tail) (list-tail (1st args) (2nd args))]
      [(append) (apply append args)]
      [(set-box!) (set-box! (1st args) (2nd args))]
      [(unbox) (unbox (1st args))]
      [(box) (box (1st args))]
      [(call/cc) (apply-proc (car args) (continuation-proc k) k)]
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-op)])))

;; Helper functions

(define begin-eval
  (case-lambda
   [(x) x]
   [(x . rest)
      (apply begin-eval rest)]))

(define improper-lambda-vars
  (lambda (vars)
    (cond
     ([symbol? vars] (list vars))
     (else (cons (car vars) (improper-lambda-vars (cdr vars)))))))

(define improper-lambda-args
  (lambda (length args)
    (if (= length 1)
  (list args)
  (cons (car args) (improper-lambda-args (- length 1) (cdr args))))))

(define flatten
 (lambda (list)
   (cond
    ([null? list] '())
    ([list? (car list)] (append (car list) (flatten (cdr list))))
    (else (cons (car list) (flatten (cdr list)))))))

(define identity-proc (lambda (x) x))
(define void-proc (void))
(define global-env init-env)
(define reset-global-env (lambda () (set! global-env init-env)))






