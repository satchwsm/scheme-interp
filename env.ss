; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

;(define empty-env
;  (lambda ()
;    (empty-env-record)))

;(define extend-env
;  (lambda (syms vals env)
;    (extended-env-record syms vals env)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
	     (if (number? list-index-r)
		 (+ 1 list-index-r)
		 #f))))))

;(define apply-env
;  (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
;    (cases environment env
;      (empty-env-record ()
;        (fail))
;      (extended-env-record (syms vals e)
;        ;(display syms)
;	      (let ((pos (list-find-position sym syms)))
;          ;(display pos)
;          (if (number? pos)
;	         ;(begin (display (list-ref vals pos)) (succeed (list-ref vals pos)))
;           (succeed (list-ref vals pos))
;	         (apply-env e sym succeed fail)))))))

(define apply-env
  (lambda (env sym k fail)
    (let* ((ref (apply-env-ref env sym fail))(res (deref ref)))
      (if (and (not (null? res)) (list? res) (eqv? 'ref (car res)))
        (deref (apply-env-ref env (cadr res) fail))
      (apply-k k res)))))

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms cells env)
    (if (and (andmap symbol? syms) (andmap box? cells))
      (extended-env-record syms cells env)
      (let* ((s (build-syms syms))(positions (find-ref-in-syms syms))(c (build-cells s cells positions env)))
        (extended-env-record s c env)))))

(define find-ref-in-syms
  (lambda (ls)
    (let loop ((l ls)(pos 0))
      (if (null? l)
        '()
        (if (ref? (car l))
          (append (list pos) (loop (cdr l) (+ pos 1)))
          (append (list -1) (loop (cdr l) (+ pos 1))))))))

(define build-syms
  (lambda (ls)
    (if (null? ls)
      '()
      (if (ref? (car ls))
        (append (list (cadr (car ls))) (build-syms (cdr ls)))
        (append (list (car ls)) (build-syms (cdr ls)))))))

(define build-cells
  (lambda (syms cells lop env)
    (if (null? syms)
      '()
      (if (eqv? (car lop) -1)
        (append (list (car cells)) (build-cells (cdr syms) (cdr cells) (cdr lop) env))
      (append (list (box (list 'ref 
        (look-up-id (car cells) env)))) 
        (build-cells (cdr syms) (cdr cells) (cdr lop) env))))))

(define look-up-id
  (lambda (b-value env)
    (cases environment env
      (empty-env-record () (look-up-id b-value global-env))
      (extended-env-record (syms cells e)
        (let loop ((s syms)(c cells))
          (if (null? s)
            (look-up-id b-value e)
            (if (eqv? (unbox (car c)) (unbox b-value))
              (car s)
              (loop (cdr s) (cdr c)))))))))

(define define-new-cell
  (lambda (id value)
    (cases environment global-env
      (empty-env-record () (eopl:error 'add-new-cell "error occured in add-new-cell: ~s" old-env))
      (extended-env-record (syms cells e)
        (let ((s (cons id syms))(v (cons (box value) cells)))
          (extended-env-record s v e))))))

(define deref
  (lambda (ref)
    (unbox ref)))

(define set-ref!
  (lambda (ref value)
    (set-box! ref value)))

(define apply-env-ref
  (lambda (env sym fail)
    (cases environment env
      (empty-env-record () (fail))
      (extended-env-record (syms cells e)
        (let ((pos (list-find-position sym syms)))
          (if (number? pos)
            (list-ref cells pos)
            (apply-env-ref e sym fail)))))))
