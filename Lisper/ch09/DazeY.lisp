;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               DazeY.lisp
;;;
;;;   STARTED:            Tue Feb 19 17:34:50 2002
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
(defun pass-fact (f)
  #'(lambda (n)
      (if (zerop n)
	  1
	  (* n (funcall (funcall f f)
			(1- n)))) ) )

(setf (symbol-function 'fact)
      (let ((g #'(lambda (f)
		   #'(lambda (n)
		       (if (zerop n)
			   1
			   (* n (funcall (funcall f f)
					 (1- n)))) ))))
	(funcall g g)) )

(defun redundant-fact (n)
  (let ((g #'(lambda (f n)
	       (if (zerop n)
		   1
		   (* n (funcall f f (1- n)))) )))
    (funcall g g n)) )

(setf (symbol-function 'fact)
      (let ((g #'(lambda (f)
		   #'(lambda (n)
		       ((lambda (func)
			  (if (zerop n)
			      1
			      (* n (funcall func (1- n)))) )
			(funcall f f)))) ))
	(funcall g g)))
