; evaluator for simple expressions.
; Possible starting point for first interpreter assignment.
;                  
; Claude Anderson.  Last modified April, 2014

(load "chez-init.ss") 


(define load-tests
    (lambda ()
        ;(load "t15.ss")
        ;(load "t16.ss")
        (load "t17.ss")
        (load "mytests.ss")))

(define load-all ; make it easy to reload the files
  (lambda ()
    (load "datatypes.ss")
    (load "parse.ss")
    (load "env.ss")
    (load "interpreter.ss")

	; odd fix
	(load "datatypes.ss")
    (load "parse.ss")
    (load "env.ss")
    (load "interpreter.ss")))

    (load-tests)
	
(load-all)

(define l load-all) ; even easier!

(define r2 mytests) ; runs my tests