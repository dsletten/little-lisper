;#!/opt/local/bin/sbcl.bin --core /opt/local/lib/sbcl/sbcl.core --noinform
;#!/usr/local/bin/sbcl --noinform
;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch07.lisp
;;;;
;;;;   Started:            Sun Jul 29 02:36:39 2007
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;
;;;;
;;;;
;;;;   Calling Sequence:
;;;;
;;;;
;;;;   Inputs:
;;;;
;;;;   Outputs:
;;;;
;;;;   Example:
;;;;
;;;;   Notes:
;;;;
;;;;
(load "/Users/dsletten/lisp/packages/test.fasl")

(defpackage ch07 (:use common-lisp test) (:shadow zerop 1+ 1- + numberp))

(in-package ch07)

;;;
;;;    This doesn't respect the required ordering of EXPR OP EXPR...
;;;    
(defun numbered (aexp)
  (cond ((null aexp) t)
	((cl:numberp aexp) t)
	((member aexp '(+ * ^)))
	((atom aexp) nil)
	((and (numbered (car aexp))
	      (numbered (cdr aexp)))) ))

(defun numbered (aexp)
  (cond ((atom aexp) (cl:numberp aexp))
	((member (second aexp) '(+ * ^))
	 (and (numbered (first aexp))
	      (numbered (third aexp)))) ))

;;;
;;;    Assuming AEXP is valid arithmetic expression:
;;;    
(defun numbered (aexp)
  (cond ((atom aexp) (cl:numberp aexp))
	(t (and (numbered (first aexp))
		(numbered (third aexp)))) ))

(defun value (aexp)
  (cond ((cl:numberp aexp) aexp)
	((eql (second aexp) '+)
	 (cl:+ (value (first aexp))
	    (value (third aexp))))
	((eql (second aexp) '*)
	 (* (value (first aexp))
	    (value (third aexp))))
	((eql (second aexp) '^)
	 (expt (value (first aexp))
	       (value (third aexp)))) ))

(defun value (aexp)
  (if (cl:numberp aexp)
      aexp
      (case (second aexp)
	(+ (cl:+ (value (first aexp))
	      (value (third aexp))))
	(* (* (value (first aexp))
	      (value (third aexp))))
	(^ (expt (value (first aexp))
		 (value (third aexp)))) )))

(defun value (aexp)
  (cond ((cl:numberp aexp) aexp)
	((eql (first aexp) 'plus)
	 (cl:+ (value (second aexp))
	    (value (third aexp))))
	((eql (first aexp) 'times)
	 (* (value (second aexp))
	    (value (third aexp))))
	((eql (first aexp) 'expt)
	 (expt (value (second aexp))
	       (value (third aexp)))) ))

(defun 1st-sub-exp (aexp)
  (second aexp))

(defun 2nd-sub-exp (aexp)
  (third aexp))

(defun operator (aexp)
  (first aexp))

(defun value (aexp)
  (cond ((cl:numberp aexp) aexp)
	((eql (operator aexp) 'plus)
	 (cl:+ (value (1st-sub-exp aexp))
	    (value (2nd-sub-exp aexp))))
	((eql (operator aexp) 'times)
	 (* (value (1st-sub-exp aexp))
	    (value (2nd-sub-exp aexp))))
	((eql (operator aexp) 'expt)
	 (expt (value (1st-sub-exp aexp))
	       (value (2nd-sub-exp aexp)))) ))

(defun value (aexp)
  (if (cl:numberp aexp)
      aexp
      (case (operator aexp)
	(plus (cl:+ (value (1st-sub-exp aexp))
		 (value (2nd-sub-exp aexp))))
	(times (* (value (1st-sub-exp aexp))
		  (value (2nd-sub-exp aexp))))
	(expt (expt (value (1st-sub-exp aexp))
		    (value (2nd-sub-exp aexp)))) )))

(defun 1st-sub-exp (aexp)
  (first aexp))

(defun operator (aexp)
  (second aexp))

(defun value (aexp)
  (cond ((cl:numberp aexp) aexp)
	((eql (operator aexp) '+)
	 (cl:+ (value (1st-sub-exp aexp))
	    (value (2nd-sub-exp aexp))))
	((eql (operator aexp) '*)
	 (* (value (1st-sub-exp aexp))
	    (value (2nd-sub-exp aexp))))
	((eql (operator aexp) '^)
	 (expt (value (1st-sub-exp aexp))
	       (value (2nd-sub-exp aexp)))) ))

;;;
;;;    ?!
;;;    
;; (defun null (s)
;;   (and (atom s)
;;        (eql s '())))

(defun zerop (n)
  (null n))

(defun 1+ (n)
  (cons '() n))

(defun 1- (n)
  (rest n))

;;;
;;;    Same as in ch. 4!
;;;    
(defun + (m n)
  (if (zerop n)
      m
      (1+ (+ m (1- n)))) )

(defun numberp (n)
  (cond ((zerop n) t)
	((eql (first n) '()) (numberp (1- n)))
	(t nil)))
