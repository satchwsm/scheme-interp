; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define parse-exp
  (lambda (datum)
    (cond
      ((or (number? datum) (string? datum) (boolean? datum) (vector? datum) (null? datum)) (lit-exp datum))
      ((symbol? datum) (var-exp datum))
      ((not (list? datum)) (eopl:error 'parse-exp 
        "Error in parse-exp: application is not a proper list: ~s" datum))
      ((pair? datum)
        (cond
          ((eqv? (car datum) 'lambda)
            (cond
              ((not (> (length datum) 2)) (eopl:error 'parse-exp 
                "Error in parse-exp: lambda expression missing body: ~s" datum))
              ((or (and (not (list? (cadr datum))) (not (symbol? (cadr datum)))) 
                (and (list? (cadr datum)) (not (andmap symbol? (cadr datum))))) (eopl:error 'parse-exp
                "Error in parse-exp: lambda argument list: formals must be symbols: ~s" (cadr datum)))
              (else 
                (lambda-exp (cadr datum) (map parse-exp (cddr datum))))))
          ((eqv? (car datum) 'if) 
            (cond
              ((= 3 (length datum)) (if-exp (parse-exp (cadr datum)) (parse-exp (caddr datum))))
              ((= 4 (length datum)) (if-alt-exp (parse-exp (cadr datum)) (parse-exp (caddr datum)) (parse-exp (cadddr datum))))
              (else (eopl:error 'parse-exp
                "Error in parse-exp: if expression has incorrect number of arguments: ~s" datum))))
          ((eqv? (car datum) 'let)
            (if (not (symbol? (cadr datum))) ; Check for named let
              (begin (check-let datum) (let-exp (car datum) (parse-exp (cadr datum)) (map parse-exp (cddr datum))))
              (begin (check-named-let datum)
                (let-named-exp (car datum) (cadr datum) (parse-exp (caddr datum) (map parse-exp (cdddr datum)))))))
          ((or (eqv? (car datum) 'let*) (eqv? (car datum) 'letrec))
            (begin (check-let datum) (let-exp (car datum) (parse-exp (cadr datum)) (map parse-exp (cddr datum)))))
          ((eqv? (car datum) 'set!) 
            (begin (check-set! datum) (set!-exp (cadr datum) (parse-exp (caddr datum)))))
          ;; TODO - fix quotes defaulting to app expressions
          (else 
            (app-exp (parse-exp (car datum))
              (map parse-exp (cdr datum))))))
      (else (eopl:error 'parse-exp
              "Invalid concrete syntax ~s" datum)))))

(define unparse-exp
  (lambda (exp)
    (cases expression exp
      (lit-exp (id) id)
      (var-exp (id) id)
      (lambda-exp (id body) 
        (append (list 'lambda id) (map unparse-exp body)))
      (app-exp (rator rand)
        (append (list (unparse-exp rator)) (map unparse-exp rator)))
      (if-exp (test then)
        (list 'if (unparse-exp test) (unparse-exp rator)))
      (if-alt-exp (test first second)
        (list 'if (unparse-exp test) (unparse-exp first) (unparse-exp second)))
      (let-exp (let-type vars expression)
        (append (list let-type) (list (unparse-exp vars)) (map unparse-exp expression)))
      (let-named-exp (let-type let-name vars expression)
        (append (list let-type let-name) (list (unparse-exp vars)) (map unparse-exp expression)))
      (quote-exp (data)
        (list 'quote data))
      (set!-exp (id new-id)
        (list 'set! id (unparse-exp new-id))))))




