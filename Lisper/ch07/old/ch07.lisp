;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               ch07.lisp
;;;
;;;   STARTED:            Sat Jan 26 16:28:42 2002
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
(defun numbered? (exp)
  (cond ((atom exp) (numberp exp))
	((member (second exp) '(+ * ^))
	 (and (numbered? (first exp))
	      (numbered? (third exp))))
	(t nil)) )

(defun value (exp)
  (cond ((numberp exp) exp)
	((atom exp) nil)
	((eq (second exp) '+)
	 (+ (value (first exp))
	    (value (third exp))))
	((eq (second exp) '*)
	 (* (value (first exp))
	    (value (third exp))))
	((eq (second exp) '^)
	 (if (zerop (value (third exp)))
	     1
	     (value (list (first exp)
			  '*
			  (list (first exp)
				'^
				(1- (value (third exp)))) )))) ) )

