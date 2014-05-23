;; Test file for personal tests primarily used in debugging.

(define (mytests)
		(list
		;(eval-one-exp '(let ([a (vector 3)]) 
	 ;     	(begin 
		;      	(vector-set! a 0 (+ 1 (vector-ref a 0))) 
		;		(= (vector-ref a 0) 4)) 
	 ; 		6))
		;(parse-exp '(let ([a (vector 3)]) 
	 ;     	(begin 
		;      	(vector-set! a 0 (+ 1 (vector-ref a 0))) 
		;		(= (vector-ref a 0) 4)) 
	 ; 		6))
		;(syntax-expand (parse-exp '(let ([a (vector 3)]) 
	 ;     	(begin 
		;      	(vector-set! a 0 (+ 1 (vector-ref a 0))) 
		;		(= (vector-ref a 0) 4)) 
	 ; 		6)))

		(eval-one-exp '((lambda (x) (set! x (+ x 1)) (list x)) 5))


	))