;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               anonymous.lisp
;;;
;;;   STARTED:            Sun Feb 24 16:11:47 2002
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
;;;   NOTES: Exercises in creating recursive functions without DEFUN.
;;;   (See also Y.lisp, which uses Y-combinator to do the same thing.)
;;;

;;;
;;;    Touretzky 8.2
;;;
; (defun anyoddp (num-list)
;   (if (null num-list)
;       nil
;       (if (oddp (first num-list))
; 	  t
; 	  (anyoddp (rest num-list)))) )
(defun anyoddp-maker (f)
  #'(lambda (num-list)
      (cond ((null num-list) nil)
	    ((oddp (first num-list)) t)
	    (t (funcall (anyoddp-maker #'anyoddp-maker) (rest num-list)))) ) )

(funcall ((lambda (f)
	    #'(lambda (num-list)
		(cond ((null num-list) nil)
		      ((oddp (first num-list)) t)
		      (t (funcall (funcall f f) (rest num-list)))) ))
	  #'(lambda (f)
	      #'(lambda (num-list)
		  (cond ((null num-list) nil)
			((oddp (first num-list)) t)
			(t (funcall (funcall f f) (rest num-list)))) )))
	 '(2 4 6 7 8))

;;;
;;;    Touretzky 8.5
;;;
; (defun add-up (num-list)
;   (cond ((null num-list) 0)
; 	(t (+ (first num-list) (add-up (rest num-list)))) ) )
(defun add-up-maker (f)
  #'(lambda (num-list)
      (cond ((null num-list) 0)
	    (t (+ (first num-list)
		  (funcall (add-up-maker #'add-up-maker)
			   (rest num-list)))) )) )

(funcall ((lambda (f)
	    #'(lambda (num-list)
		(cond ((null num-list) 0)
		      (t (+ (first num-list)
			    (funcall (funcall f f)
				     (rest num-list)))) )))
	  #'(lambda (f)
	      #'(lambda (num-list)
		  (cond ((null num-list) 0)
			(t (+ (first num-list)
			      (funcall (funcall f f)
				       (rest num-list)))) ))))
	 '(2 4 6 7 8))

;;;
;;;    Touretzky 8.6
;;;
; (defun alloddp (num-list)
;   (cond ((null num-list) t)
; 	((evenp (first num-list)) nil)
; 	(t (alloddp (rest num-list)))) )
(defun alloddp-maker (f)
  #'(lambda (num-list)
      (cond ((null num-list) t)
	    ((evenp (first num-list)) nil)
	    (t (funcall (alloddp-maker #'alloddp-maker)
			(rest num-list)))) ) )

(funcall ((lambda (f)
	    #'(lambda (num-list)
		(cond ((null num-list) t)
		      ((evenp (first num-list)) nil)
		      (t (funcall (funcall f f)
				  (rest num-list)))) ))
	  #'(lambda (f)
	      #'(lambda (num-list)
		  (cond ((null num-list) t)
			((evenp (first num-list)) nil)
			(t (funcall (funcall f f)
				    (rest num-list)))) )))
	 '(2 4 6 7 8))

;;;
;;;    8.7
;;;
; (defun my-member (elt l)
;   (cond ((null l) nil)
; 	((eql elt (first l)) l)
; 	(t (my-member elt (rest l)))) )
(defun member-maker (f)
  #'(lambda (elt l)
      (cond ((null l) nil)
	    ((eql elt (first l)) l)
	    (t (funcall (member-maker #'member-maker) elt (rest l)))) ) )

(funcall ((lambda (f)
	    #'(lambda (elt l)
		(cond ((null l) nil)
		      ((eql elt (first l)) l)
		      (t (funcall (funcall f f) elt (rest l)))) ))
	  #'(lambda (f)
	      #'(lambda (elt l)
		  (cond ((null l) nil)
			((eql elt (first l)) l)
			(t (funcall (funcall f f) elt (rest l)))) )))
	 'k
	 '(a h p o k q r t))

;;;
;;;    8.27
;;;
; (defun square-list (num-list)
;   (cond ((null num-list) nil)
; 	(t (cons (* (first num-list) (first num-list))
; 		 (square-list (rest num-list)))) ) )

;;;
;;;    8.32 (Compare 8.41)
;;;
; (defun sum-numeric-elements (l)
;   (cond ((null l) 0)
; 	((numberp (first l)) (+ (first l) (sum-numeric-elements (rest l))))
; 	(t (sum-numeric-elements (rest l)))) )

;;;
;;;    8.41 (Compare 8.32)
;;;
; (defun sum-tree (tree)
;   (cond ((numberp tree) tree)
; 	((atom tree) 0)
; 	(t (+ (sum-tree (first tree))
; 	      (sum-tree (rest tree)))) ) )
	

; (defun reverse-r (l)
;   (cond ((null (cdr l)) l)
; 	(t (cons (car (reverse-r (cdr l)))
; 		 (reverse-r (cons (car l)
; 				  (reverse-r
; 				   (cdr (reverse-r (cdr l)))) )))) ) )

