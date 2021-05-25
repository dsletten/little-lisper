;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               ycomb.lisp
;;;
;;;   STARTED:            Wed Feb 20 17:23:44 2002
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
;;;   NOTES:
;;;
;;;

;;;
;;;    Create a function that makes functions that know how to perform the
;;;    basic factorial computation and know how to create identical functions
;;;    to handle factorial recursive computation. This function makes such
;;;    functions by passing itself as an argument.
;;;
;1. Named FACT-MAKER
(defun fact-maker (procedure)
  #'(lambda (n)
      (if (zerop n)
	  1
	  (* n (funcall (funcall procedure procedure)
			(1- n)))) ) )

(funcall (fact-maker #'fact-maker) 5)

; (fact-maker #'fact-maker) =>
; #'(lambda (n)
;     (if (zerop n)
; 	1
; 	(* n (funcall (funcall #'fact-maker #'fact-maker)
; 		      (1- n)))) )

; Compare to CURRY-MAKER in Little Lisper ch. 9
; (defun fact-maker (procedure)
;   #'(lambda (n)
;       (if (zerop n)
; 	  1
; 	  (* n (funcall (fact-maker procedure)
; 			(1- n)))) ) )

;2. Applying definition directly
(funcall ((lambda (procedure)
	    #'(lambda (n)
		(if (zerop n)
		    1
		    (* n (funcall (funcall procedure procedure)
				  (1- n)))) ))
	  #'(lambda (procedure)
	      #'(lambda (n)
		  (if (zerop n)
		      1
		      (* n (funcall (funcall procedure procedure)
				    (1- n)))) ))) 6)

;3. The above works. Now attempt to generalize for any recursive function.
;   Try to get recursive part to look like standard recursive FACT definition.
;   In particular, make (funcall (funcall procedure procedure) (1- n)) look
;   like (funcall f (1- n)).
(funcall ((lambda (procedure)
	    ((lambda (func-arg)
	       #'(lambda (n)
		   (if (zerop n)
		       1
		       (* n (funcall func-arg (1- n)))) ))
	     #'(lambda (arg)
		 (funcall (funcall procedure procedure) arg))))
	  #'(lambda (procedure)
	      ((lambda (func-arg)
		 #'(lambda (n)
		     (if (zerop n)
			 1
			 (* n (funcall func-arg (1- n)))) ))
	       #'(lambda (arg)
		   (funcall (funcall procedure procedure) arg)))) )
	 5)

;
; Compare FUNCTION-MAKER pg. 154 Little Lisper:
; 
; (defun fact-maker (procedure)
;   #'(lambda (n)
;       (if (zerop n)
; 	  1
; 	  (* n ((lambda (arg)
; 		  (funcall (funcall procedure procedure) arg))
; 		(1- n)))) ) )
;
; Pg. 155:
; 
; (defun fact-maker (procedure)
;   ((lambda (func-arg)
;      #'(lambda (n)
; 	 (if (zerop n)
; 	     1
; 	     (* n (funcall func-arg) (1- n)))) )
;    #'(lambda (arg)
;        (funcall (funcall procedure procedure) arg))) )

;4. Extract the duplicate factorial code.
(defun F* (func-arg)
  #'(lambda (n)
      (if (zerop n)
	  1
	  (* n (funcall func-arg (1- n)))) ) )

(funcall ((lambda (procedure)
	    (F* #'(lambda (arg)
		    (funcall (funcall procedure procedure) arg))))
	  #'(lambda (procedure)
	      (F* #'(lambda (arg)
		      (funcall (funcall procedure procedure) arg)))) )
	 6)

;5. Remove dependency on specific recursive function. Voila: Y-combinator!
(defun Y (X)
  ((lambda (procedure)
     (funcall X #'(lambda (arg)
		    (funcall (funcall procedure procedure) arg))))
   #'(lambda (procedure)
       (funcall X #'(lambda (arg)
		      (funcall (funcall procedure procedure) arg)))) ) )

(funcall (Y #'F*) 6)

