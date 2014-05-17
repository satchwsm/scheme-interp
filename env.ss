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
  (lambda (env sym succeed fail)
    (let ((ref (apply-env-ref env sym fail)))
      (deref ref))))

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms cells env)
    (extended-env-record syms cells env)))

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
