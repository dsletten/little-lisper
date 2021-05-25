;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               ch09.lisp
;;;
;;;   STARTED:            Sat Jan 26 17:30:28 2002
;;;   MODIFICATIONS:
;;;
;;;   PURPOSE:
;;;
;;;
;;;
;;;   CALLING SEQUENCE:
;;;
;;;
;;;   INPUTS:
;;;
;;;   OUTPUTS:
;;;
;;;   EXAMPLE:
;;;
;;;   NOTES: Recursion with anonymous functions. An anonymous function cannot
;;;   contain its name within its own definition to allow it to call itself
;;;   recursively. Instead, its definition contains an invocation of the
;;;   function-making function that built it to create a duplicate of itself
;;;   and then calls that function--its clone. Is this strictly speaking
;;;   recursion? The function is not calling itself, which is the standard
;;;   definition of recursion. However, it is calling a function identical to
;;;   itself, so it seems reasonable to consider this recursion after all.
;;;
;;;   The basic requirements for such a master function are:
;;;   1. It is a function-making function.
;;;   2. The function it creates has embedded within it the behavior of the
;;;      target recursive function. (See FUNCTION-MAKER vs. MREMBER-CURRY
;;;      below.)
;;;      
(defun rember-f (test)
  #'(lambda (a l)
      (cond ((null l) ())
	    ((funcall test a (car l))
	     (cdr l))
	    (t (cons (car l)
		     (funcall (rember-f test) a (cdr l)))) )) )

;;;
;;;    When this function creates a closure, then invoking that closure may
;;;    repeatedly call the above function to resolve its own recursive calls:
;;;
; [5]> (setf (symbol-function 'x) (rember-f #'=))
; #<CLOSURE :LAMBDA (A L)
;   (COND ((NULL L) NIL) ((FUNCALL TEST A (CAR L)) (CDR L))
;    (T (CONS (CAR L) (FUNCALL (REMBER-F TEST) A (CDR L)))))>
; [6]> (x 7 '(8 7 2))
; (8 2)
; [7]> (trace rember-f)
; ;; Tracing function REMBER-F.
; (REMBER-F)
; [8]> (x 7 '(8 7 2))

; 1. Trace: (REMBER-F '#<SYSTEM-FUNCTION =>)
; 1. Trace: REMBER-F ==> 
; #<CLOSURE :LAMBDA (A L)
;   (COND ((NULL L) NIL) ((FUNCALL TEST A (CAR L)) (CDR L))
;    (T (CONS (CAR L) (FUNCALL (REMBER-F TEST) A (CDR L)))))>
; (8 2)
; [9]> (x 7 '(8 3 1 7 2))

; 1. Trace: (REMBER-F '#<SYSTEM-FUNCTION =>)
; 1. Trace: REMBER-F ==> 
; #<CLOSURE :LAMBDA (A L)
;   (COND ((NULL L) NIL) ((FUNCALL TEST A (CAR L)) (CDR L))
;    (T (CONS (CAR L) (FUNCALL (REMBER-F TEST) A (CDR L)))))>
; 1. Trace: (REMBER-F '#<SYSTEM-FUNCTION =>)
; 1. Trace: REMBER-F ==> 
; #<CLOSURE :LAMBDA (A L)
;   (COND ((NULL L) NIL) ((FUNCALL TEST A (CAR L)) (CDR L))
;    (T (CONS (CAR L) (FUNCALL (REMBER-F TEST) A (CDR L)))))>
; 1. Trace: (REMBER-F '#<SYSTEM-FUNCTION =>)
; 1. Trace: REMBER-F ==> 
; #<CLOSURE :LAMBDA (A L)
;   (COND ((NULL L) NIL) ((FUNCALL TEST A (CAR L)) (CDR L))
;    (T (CONS (CAR L) (FUNCALL (REMBER-F TEST) A (CDR L)))))>
; (8 3 1 2)

;;;
;;;    Or in this case, the function is never actually given a name:
;;;
(funcall (rember-f #'=) 5 '(1 2 5 4 5 9 0 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    Abstract common functionality out.
;;;    
(defun insert-g (side)
  #'(lambda (old new l)
      (cond ((null l) ())
	    ((eq (car l) old)
	     (funcall side old new (cdr l)))
	    (t (cons (car l) (funcall (insert-g side)
				      old new (cdr l)))) )) )

(defun left-sub (old new l)
  (cons new (cons old l)) )

(defun right-sub (old new l)
  (cons old (cons new l)) )

(defun subst-sub (old new l)
  (cons new l) )

(defun rem-sub (old x l)
  l)

;;;
;;;    (insert-g 'left-sub) VS. (insert-g #'left-sub) ??
;;;    (Both work...)
;;;    
; [4]> (funcall (insert-g 'left-sub) 'pung 'foo '(is this not pung))
; (IS THIS NOT FOO PUNG)
; [5]> (funcall (insert-g 'right-sub) 'pung 'foo '(is this not pung))
; (IS THIS NOT PUNG FOO)
; [6]> (funcall (insert-g 'subst-sub) 'pung 'foo '(is this not pung))
; (IS THIS NOT FOO)
; [7]> (funcall #'(lambda (a l) (funcall (insert-g 'rem-sub) a nil l)) 'pung '(is this not pung))
; (IS THIS NOT)

;;;
;;;    Using anonymous functions rather than left-sub/right-sub above:
;;;    
(setf (symbol-function 'insert-l) (insert-g #'(lambda (old new l)
						(cons new (cons old l)))) )
(setf (symbol-function 'insert-r) (insert-g #'(lambda (old new l)
						(cons old (cons new l)))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun atom-to-function (x)
  (cond ((eq x '+) #'+)
	((eq x '*) #'*)
	((eq x '^) #'expt)) ) ;??

(defun atom-to-function (x)
  (symbol-function x) )

(defun value (aexp)
  (cond ((numberp aexp) aexp)
	(t (funcall (atom-to-function (operator aexp))
		    (value (1st-sub-exp aexp))
		    (value (2nd-sub-exp aexp)))) ) )

(defun operator (exp)
  (car exp) )

(defun 1st-sub-exp (exp)
  (car (cdr exp)) )

(defun 2nd-sub-exp (exp)
  (car (cdr (cdr exp))) )

;;;
;;;    Another way
;;;    
(defstruct (aexp (:type list))
  operator
  op-1
  op-2)

(defun value (aexp)
  (cond ((numberp aexp) aexp)
	(t (funcall (atom-to-function (aexp-operator aexp))
		    (value (aexp-op-1 aexp))
		    (value (aexp-op-2 aexp)))) ) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    Line of reasoning:
;;;    1. AND/OR can't be passed as functions like we've done above with
;;;    other functions. AND/OR are special forms.
;;;    2. We can write our own versions of AND/OR which are functions.
;;;    However, these functions will not be short-circuit operators as the real
;;;    AND/OR are. If we use them in recursive functions, they may cause
;;;    clauses to be evaluated which don't need to be. We may have functions
;;;    that are correct, but they will be wasteful due to unnecessary work.
;;;    3. The new OR-PRIME and AND-PRIME simulate short-circuit evaluation by
;;;    only accepting one clause as an argument. The second clause is part of
;;;    the actual function definition.
;;;    4. However, these new functions will rely upon the very functions we are
;;;    trying to define in the first place: INTERSECT? and SUBSET?.
;;;
;;;    Aha! Recursion!
;;;    

(defun set-f? (logical? const)
  #'(lambda (s1 s2)
      (cond ((null s1) const)
	    (t (funcall logical? (member (car s1) s2)
			(funcall (set-f? logical? const)
				 (cdr s1) s2)))) ) )

;;;
;;;    Can't use FUNCALL on AND/OR
;;;    
;(setf (symbol-function 'subset?) (set-f? 'and t))
;(setf (symbol-function 'intersect?) (set-f? 'or nil))

(defun my-and (p q)
  (and p q) )

(defun my-or (p q)
  (or p q) )

(setf (symbol-function 'subset?) (set-f? 'my-and t))
(setf (symbol-function 'intersect?) (set-f? 'my-or nil))

(defun my-and (x s1 s2)
  (and x (subset? (cdr s1) s2)) )

(defun my-or (x s1 s2)
  (or x (intersect? (cdr s1) s2)) )

(defun set-f? (logical? const)
  #'(lambda (s1 s2)
      (cond ((null s1) const)
	    (t (funcall logical?
			(member (car s1) s2)
			s1
			s2)))) )

(setf (symbol-function 'subset?) (set-f? 'my-and t))
(setf (symbol-function 'intersect?) (set-f? 'my-or nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mrember-curry (l)
  (cond ((null l) ())
	((eq (car l) 'curry)
	 (mrember-curry (cdr l)))
	(t (cons (car l)
		 (mrember-curry (cdr l)))) ) )
;;;
;;;    CURRY-MAKER returns a function that "recursively" calls CURRY-MAKER to
;;;    create copies of itself which are then FUNCALL'ed in order to create
;;;    the behavior of MREMBER-CURRY. Whereas the named function MREMBER-CURRY
;;;    calls itself (by name) recursively, CURRY-MAKER creates an anonymous
;;;    function that can only call a duplicate of itself. It doesn't know its
;;;    own name.
;;;    
(defun curry-maker (future)
  #'(lambda (l)
      (cond ((null l) ())
	    ((eq (car l) 'curry)
	     (funcall (curry-maker future) (cdr l)))
	    (t (cons (car l)
		     (funcall (curry-maker future) (cdr l)))) )) )
;;;
;;;    When this function is used, the named function MREMBER-CURRY is only
;;;    used once. Recursive calls invoke anonymous lambda functions equivalent
;;;    to MREMBER-CURRY but created by CURRY-MAKER at runtime.
;;;    
(setf (symbol-function 'mrember-curry) (curry-maker 0))

;;;
;;;    In CURRY-MAKER the FUTURE parameter is arbitrary. However, in
;;;    FUNCTION-MAKER, FUTURE must refer to FUNCTION-MAKER in order to create
;;;    the duplicate functions to simulate MREMBER-CURRY. In other words, the
;;;    recursive call must tell FUNCTION-MAKER which function making function
;;;    should be used for the next recursive call.
;;;
;;;    (FUNCALL (FUNCALL FUTURE FUTURE) (CDR L)) suggests that the result of
;;;    (FUNCALL FUTURE FUTURE) must be a function in order to FUNCALL this on
;;;    (CDR L). Thus, FUTURE must be a function-making function. Furthermore,
;;;    to use FUNCTION-MAKER to successfully create MREMBER-CURRY, FUTURE must
;;;    return a function that performs the basic behavior of MREMBER-CURRY.
;;;    This means that FUTURE must be FUNCTION-MAKER itself.
;;;    
(defun function-maker (future)
  #'(lambda (l)
      (cond ((null l) ())
	    ((eq (car l) 'curry)
	     (funcall (funcall future future) (cdr l)))
	    (t (cons (car l)
		     (funcall (funcall future future) (cdr l)))) )) )

;;;
;;;    We can already produce MREMBER-CURRY anonymously now:
;;;
(funcall ((lambda (future)
	    #'(lambda (l)
		(cond ((null l) ())
		      ((eq (car l) 'curry)
		       (funcall (funcall future future) (cdr l)))
		      (t (cons (car l)
			       (funcall (funcall future future) (cdr l)))) )))
	  #'(lambda (future)
	      #'(lambda (l)
		  (cond ((null l) ())
			((eq (car l) 'curry)
			 (funcall (funcall future future) (cdr l)))
			(t (cons (car l)
				 (funcall
				  (funcall future future) (cdr l)))) ))))
	 '(a b c curry e curry g h curry i))

((lambda (x) (1+ x)) 2)  ;(1+ 2) -> ((lambda (x) (1+ x)) 2)

((lambda (y) ((lambda (x) (1+ x)) y)) 2)

((lambda (x) ((lambda (x) (1+ x)) x)) 2)
;                      ^------^
;         ^-----------------------^

;;;f -> (lambda (x) (f x))
;;;E.g. ((lambda (x) (car x)) '(a b c)) <=> (car '(a b c))

((lambda (f) (funcall f 1 2 3))
 #'(lambda (x y z)
     (+ x y z)))

;;;
;;;    Prepare to extract duplicate code.
;;;    
(defun function-maker (future)
  #'(lambda (l)
      (cond ((null l) ())
	    ((eq (car l) 'curry)
	     ((lambda (arg)
		(funcall (funcall future future) arg))
	      (cdr l)))
	    (t (cons (car l)
		     ((lambda (arg)
			(funcall (funcall future future) arg))
		      (cdr l)))) )) )

;;;
;;;    Extract the duplicate code.
;;;    
(defun function-maker (future)
  ((lambda (recur)
     #'(lambda (l)
	 (cond ((null l) ())
	       ((eq (car l) 'curry)
		(funcall recur (cdr l)))
	       (t (cons (car l)
			(funcall recur (cdr l)))) )))
   #'(lambda (arg)
       (funcall (funcall future future) arg))) )


(defun function-maker (future)
  (M #'(lambda (arg)
	 (funcall (funcall future future) arg))) )

(defun M (recur)
  #'(lambda (l)
      (cond ((null l) ())
	    ((eq (car l) 'curry)
	     (funcall recur (cdr l)))
	    (t (cons (car l)
		     (funcall recur (cdr l)))) )) )

;;;
;;;    Alternative definitions:
;;;    
(setf (symbol-function 'mrember-curry) (function-maker #'function-maker))
(defun mrember-curry (l)
  (funcall (function-maker #'function-maker) l))

;;;
;;;    How does this work?! FUTURE is never given a value in the LAMBDA
;;;    arg...
;;;    
(setf (symbol-function 'mrember-curry)
      ((lambda (future)
	 (M #'(lambda (arg)
		(funcall (funcall future future) arg))))
       #'(lambda (future)
	   (M #'(lambda (arg)
		  (funcall (funcall future future) arg)))) ))

;;;
;;;    It doesn't matter! FUTURE is just a placeholder. It defines a function
;;;    that takes a single arg and performs the specified action using it
;;;    (namely creating another function of one arg (whose value will be
;;;    (cdr l) in this example) and passing this function to M).
;;;    
(setf (symbol-function 'mrember-curry)
      ((lambda (future)
	 (M #'(lambda (arg)
		(funcall (funcall future future) arg))))
       #'(lambda (pung)
	   (M #'(lambda (arg)
		  (funcall (funcall pung pung) arg)))) ))

;;;
;;;    Rather than being tied to M, accept an arbitrary function in the
;;;    function maker. Y works with an auxiliary function F, which creates a
;;;    function that uses the function created by Y to perform its recursion.
;;;    Since Y creates functions of a single variable, the argument to F should
;;;    only be applied to a single value. Y is generic. F must define the
;;;    specific character of the recursive operation.
;;;    
(defun Y (F)
  ((lambda (future)
     (funcall F #'(lambda (arg)
		    (funcall (funcall future future) arg))))
   #'(lambda (future)
       (funcall F #'(lambda (arg)
		      (funcall (funcall future future) arg)))) ) )

(setf (symbol-function 'mrember-curry) (Y #'M))





;;  (defun Y (f)
;;    (funcall
;;      #'(lambda (rec)
;;          #'(lambda (arg) (funcall (funcall f (funcall rec rec)) arg)))
;;      #'(lambda (rec)
;;          #'(lambda (arg) (funcall (funcall f (funcall rec rec)) arg)))))

;; It can be made a little neater by rewriting:

;;  (defun Y (f)
;;    (let ((rec #'(lambda (rec)
;;                   #'(lambda (arg) 
;;                       (funcall (funcall f (funcall rec rec))
;;                                arg)))))
;;      (funcall rec rec)))
