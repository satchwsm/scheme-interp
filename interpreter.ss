; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form)))

; eval-exp is the main component of the interpreter

(define eval-exp
  (lambda (exp)
    (cases expression exp
      [lit-exp (datum) datum]
      [var-exp (id)
				(apply-env init-env id; look up its value.
      	   (lambda (x) x) ; procedure to call if id is in the environment 
           (lambda () (eopl:error 'apply-env ; procedure to call if id not in env
		          "variable not found in environment: ~s"
			   id)))] 
      [app-exp (rator rands)
        (let ([proc-value (eval-exp rator)]
              [args (eval-rands rands)])
          (apply-proc proc-value args))]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands)
    (map eval-exp rands)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
			; You will add other cases
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

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

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

; TODO - tweak cases to handle when its not just 2 arguments
(define apply-prim-proc
  (lambda (prim-proc args)
    (case prim-proc
      [(+) (+ (1st args) (2nd args))]
      [(-) (- (1st args) (2nd args))]
      [(*) (* (1st args) (2nd args))]
      [(/) (/ (1st args) (2nd args))]
      [(add1) (+ (1st args) 1)]
      [(sub1) (- (1st args) 1)]
      [(=) (= (1st args) (2nd args))]
      [(<) (< (1st args) (2nd args))]
      [(<=) (<= (1st args) (2nd args))]
      [(>) (> (1st args) (2nd args))]
      [(>=) (>= (1st args) (2nd args))]
      [(zero?) (zero? args)]
      [(not) (not args)]
      [(cons) (cons (1st args) (2nd args))]
      [(car) (1st args)]
      [(cdr) (2nd args)]
      [(list) (list args)]
      [(null?) (null? args)]
      [(assq) (assq (1st args) (2nd args))]
      [(eq?) (eq? (1st args) (2nd args))]
      [(equal?) (equal? (1st args) (2nd args))]
      [(atom?) (atom? args)]
      [(length) (length args)]
      [(list->vector) (list->vector args)]
      [(list?) (list? args)]
      [(pair?) (pair? args)]
      [(procedure?) (procedure? args)]
      [(vector->list) (vector->list args)]
      [(vector) (vector args)]
      [(make-vector) (make-vector (1st args) (2nd args))]
      [(make-list) (make-list (1st args) (2nd args))]
      [(vector-ref) (vector-ref (1st args) (2nd args))]
      [(list-ref) (list-ref (1st args) (2nd args))]
      [(vector?) (vector? args)]
      [(number?) (number? args)]
      [(symbol?) (symbol? args)]
      [(set!-car) (set!-car (1st args) (2nd args))]
      [(set!-cdr) (set!-cdr (1st args) (2nd args))]
      [(vector-set!) (vector-set! (1st args) (2nd args) (3rd args))]
      [(display) (display args)]
      [(newline) (newline args)]
      [(caar) (caar args)]
      [(cadr) (cadr args)]
      [(cdar) (cdar args)]
      [(caaar) (caaar args)]
      [(caadr) (caadr args)]
      [(cadar) (cadar args)]
      [(cdaar) (cdaar args)]
      [(caddr) (caddr args)]
      [(cdadr) (cdadr args)]
      [(cddar) (cddar args)]
      [(cdddr) (cdddr args)]
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-op)])))

(define low-prim-proc
  (lambda (prim-proc args base-case)
    (if (null? args)
      base-case
      (prim-proc args (low-prim-proc prim-proc args base-case)))))

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
    (display (parse-exp x)) 
    (top-level-eval (parse-exp x))))










