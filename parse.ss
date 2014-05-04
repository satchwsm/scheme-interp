; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

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
          ((eqv? (car datum) 'quote) (lit-exp (cadr datum)))
          ((eqv? (car datum) 'if) 
            (cond
              ((= 3 (length datum)) (if-exp (parse-exp (cadr datum)) (parse-exp (caddr datum))))
              ((= 4 (length datum)) (if-alt-exp (parse-exp (cadr datum)) (parse-exp (caddr datum)) (parse-exp (cadddr datum))))
              (else (eopl:error 'parse-exp
                "Error in parse-exp: if expression has incorrect number of arguments: ~s" datum))))
          ([eqv? (car datum) 'let]
            (cond
              ([null? (cdr datum)] (error 'parse-exp "let must have two or more arguments, given zero ~s" datum))
              ([symbol? (cadr datum)]
               (proper-let-syntax? datum caddr 4)
               (let-named-exp (cadr datum)
                  (map car (caddr datum))
                  (map parse-exp (map cadr (caddr datum)))
                  (map parse-exp (cdddr datum))))
              (else
               (proper-let-syntax? datum cadr 3)  
               (let-exp (map car (cadr datum))
                  (map parse-exp (map cadr (cadr datum)))
                  (map parse-exp (cddr datum))))))
          ([eqv? (car datum) 'let*]
           (proper-let-syntax? datum cadr 3)
           (let*-exp (map car (cadr datum))
               (map parse-exp (map cadr (cadr datum)))
               (map parse-exp (cddr datum))))
          ([eqv? (car datum) 'letrec]
           (proper-let-syntax? datum cadr 3)
           (letrec-exp (map car (cadr datum))
                 (map parse-exp (map cadr (cadr datum))) 
                 (map parse-exp (cddr datum))))
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

(define contains-duplicates?
  (lambda (list)
    (cond
     ([null? list] #f)
     ([symbol? list] #f)
     ([number? list] #f)
     ([list? list]
      (cond
       ([member (car list) (cdr list)] #t)
       (else (contains-duplicates? (cdr list)))))
     (else (or (contains? (car list) (cdr list)) (contains-duplicates? (cdr list)))))))

(define contains?
  (lambda (element list)
    (cond
     ([null? list] #f)
     ([symbol? list] (eqv? element list))
     ([number? list] (eqv? element list))
     ([list? (car list)] (contains? element (car list)))
     ([pair? list] (or (eqv? element (car list)) (contains? element (cdr list))))
     ([eqv? element (car list)] #t)
     (else (contains? element (cdr list))))))

(define proper-list?
  (lambda (exp)
    (cond
     ([null? exp] #t)
     ([pair? exp] (proper-list? (cdr exp)))
     (else #f))))

(define proper-let-syntax?
  (lambda (datum var-proc min-length)
    (cond
     ([< (length datum) min-length] (error 'parse-exp "if must have at least two arguments ~s" datum))
     ([not (proper-list? (var-proc datum))] (error 'parse-exp "let does not allow improper lists in variable declaration ~s" datum))
     ([not (andmap proper-list? (var-proc datum))] (error 'parse-exp "let does not allow improper lists in variable declaration ~s" datum))
     ([ormap null? (var-proc datum)] (error 'parse-exp "let does not allow empty lists in variable declaration ~s" datum))
     ([not (andmap (lambda (x) (symbol? (car x))) (var-proc datum))] (error 'parse-exp "let variables must be symbols ~s" datum))
     ([not (andmap (lambda (x) (eqv? '2 (length x))) (var-proc datum))] (error 'parse-exp "let variables must have a keyword and definition ~s" datum))
     ([contains-duplicates? (map car (var-proc datum))] (error 'parse-exp "let variables may only be defined once ~s" datum)))))

