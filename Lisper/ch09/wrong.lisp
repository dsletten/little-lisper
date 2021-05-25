;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               wrong.lisp
;;;
;;;   STARTED:            Fri Mar  1 18:01:16 2002
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
;;;   NOTES: Wrong definition (* 13 *) from DazeY
;;;
;;;

;;;
;;;    wrong - expanded in style of ycomb
;;;
; (setf (symbol-function 'fact)
;       ((lambda (procedure)
; 	 ((lambda (func-arg)
; 	    #'(lambda (n)
; 		(if (zerop n)
; 		    1
; 		    (* n (funcall func-arg (1- n)))) ))
; 	  (funcall procedure procedure)))
; 	 #'(lambda (procedure)
; 	     ((lambda (func-arg)
; 		#'(lambda (n)
; 		    (if (zerop n)
; 			1
; 			(* n (funcall func-arg (1- n)))) ))
; 	      (funcall procedure procedure)))) )

; (setf (symbol-function 'fact)
;       ((lambda (func-arg)
; 	 #'(lambda (n)
; 	     (if (zerop n)
; 		 1
; 		 (* n (funcall func-arg (1- n)))) ))
;        (funcall #'(lambda (procedure)
; 		    ((lambda (func-arg)
; 		       #'(lambda (n)
; 			   (if (zerop n)
; 			       1
; 			       (* n (funcall func-arg (1- n)))) ))
; 		     (funcall procedure procedure)))
; 		#'(lambda (procedure)
; 		    ((lambda (func-arg)
; 		       #'(lambda (n)
; 			   (if (zerop n)
; 			       1
; 			       (* n (funcall func-arg (1- n)))) ))
; 		     (funcall procedure procedure)))) ))

(setf (symbol-function 'fact)
      #'(lambda (n)
	  (if (zerop n)
	      1
	      (* n (funcall (funcall
			     #'(lambda (procedure)
				 ((lambda (func-arg)
				    #'(lambda (n)
					(if (zerop n)
					    1
					    (* n (funcall func-arg (1- n)))) ))
				  (funcall procedure procedure)))
			     #'(lambda (procedure)
				 ((lambda (func-arg)
				    #'(lambda (n)
					(if (zerop n)
					    1
					    (* n (funcall func-arg (1- n)))) ))
				  (funcall procedure procedure)))) (1- n)))) ))
