;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               Y.lisp
;;;
;;;   STARTED:            Mon Feb  4 01:14:55 2002
;;;   MODIFICATIONS:
;;;
;;;   PURPOSE: Examples of recursive functions rewritten to use the
;;;   "Applicative-order Y-combinator"!
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

(defun Y (F)
  ((lambda (future)
     (funcall F #'(lambda (arg)
		    (funcall (funcall future future) arg))))
   #'(lambda (future)
       (funcall F #'(lambda (arg)
		      (funcall (funcall future future) arg)))) ) )

;;;
;;;    Touretzky 8.2
;;;
; (defun anyoddp (num-list)
;   (if (null num-list)
;       nil
;       (if (oddp (first num-list))
; 	  t
; 	  (anyoddp (rest num-list)))) )

;1.
(defun anyoddp-maker (f)
  #'(lambda (l)
      (cond ((null l) nil)
	    ((oddp (first l)) t)
	    (t (funcall (funcall f f) (rest l)))) ) )

(funcall (anyoddp-maker #'anyoddp-maker) '(2 4 6 7 8))

;2.
(funcall ((lambda (f)
	    #'(lambda (l)
		(cond ((null l) nil)
		      ((oddp (first l)) t)
		      (t (funcall (funcall f f) (rest l)))) ))
	  #'(lambda (f)
	      #'(lambda (l)
		  (cond ((null l) nil)
			((oddp (first l)) t)
			(t (funcall (funcall f f) (rest l)))) )))
	 '(2 4 6 7 8))

;3.
(funcall ((lambda (f)
	    ((lambda (p)
	       #'(lambda (l)
		   (cond ((null l) nil)
			 ((oddp (first l)) t)
			 (t (funcall p (rest l)))) ))
	     #'(lambda (arg)
		 (funcall (funcall f f) arg))))
	  #'(lambda (f)
	      ((lambda (p)
		 #'(lambda (l)
		     (cond ((null l) nil)
			   ((oddp (first l)) t)
			   (t (funcall p (rest l)))) ))
	       #'(lambda (arg)
		   (funcall (funcall f f) arg)))) )
	 '(2 4 6 7 8))

;4.
(defun A* (p)
  #'(lambda (l)
      (cond ((null l) nil)
	    ((oddp (first l)) t)
	    (t (funcall p (rest l)))) ) )

(funcall ((lambda (f)
	    (A* #'(lambda (arg)
		    (funcall (funcall f f) arg))))
	  #'(lambda (f)
	      (A* #'(lambda (arg)
		      (funcall (funcall f f) arg)))) )
	 '(2 4 6 7 8))
	    
(setf (symbol-function 'anyoddp)
      (Y #'(lambda (recur)
	     #'(lambda (num-list)
		 (cond ((null num-list) nil)
		       ((oddp (first num-list)) t)
		       (t (funcall recur
				   (rest num-list)))) ))))

;;;
;;;    Touretzky 8.5
;;;
; (defun add-up (num-list)
;   (cond ((null num-list) 0)
; 	(t (+ (first num-list) (add-up (rest num-list)))) ) )

;1.
(defun add-up-maker (f)
  #'(lambda (num-list)
      (cond ((null num-list) 0)
	    (t (+ (first num-list)
		  (funcall (funcall f f) (rest num-list)))) )) )

(funcall (add-up-maker #'add-up-maker) '(2 4 6 7 8))

;2.
(funcall ((lambda (f)
	    #'(lambda (num-list)
		(cond ((null num-list) 0)
		      (t (+ (first num-list)
			    (funcall (funcall f f) (rest num-list)))) )))
	  #'(lambda (f)
	      #'(lambda (num-list)
		  (cond ((null num-list) 0)
			(t (+ (first num-list)
			      (funcall (funcall f f) (rest num-list)))) ))))
	 '(2 4 6 7 8))

;3.
(funcall ((lambda (f)
	    ((lambda (p)
	       #'(lambda (num-list)
		   (cond ((null num-list) 0)
			 (t (+ (first num-list)
			       (funcall p (rest num-list)))) )))
	     #'(lambda (arg)
		 (funcall (funcall f f) arg))))
	  #'(lambda (f)
	      ((lambda (p)
		 #'(lambda (num-list)
		     (cond ((null num-list) 0)
			   (t (+ (first num-list)
				 (funcall p (rest num-list)))) )))
	       #'(lambda (arg)
		   (funcall (funcall f f) arg)))) )
	 '(2 4 6 7 8))

;4.
(defun +* (p)
  #'(lambda (num-list)
      (cond ((null num-list) 0)
	    (t (+ (first num-list)
		  (funcall p (rest num-list)))) )) )

(funcall ((lambda (f)
	    (+* #'(lambda (arg)
		    (funcall (funcall f f) arg))))
	  #'(lambda (f)
	      (+* #'(lambda (arg)
		      (funcall (funcall f f) arg)))) )
	 '(2 4 6 7 8))


(setf (symbol-function 'add-up)
      (Y #'(lambda (recur)
	     #'(lambda (num-list)
		 (cond ((null num-list) 0)
		       (t (+ (first num-list)
			     (funcall recur (rest num-list)))) )))) )
;;;
;;;    Touretzky 8.6
;;;
; (defun alloddp (num-list)
;   (cond ((null num-list) t)
; 	((evenp (first num-list)) nil)
; 	(t (alloddp (rest num-list)))) )

(setf (symbol-function 'alloddp)
      (Y #'(lambda (recur)
	     #'(lambda (num-list)
		 (cond ((null num-list) t)
		       ((evenp (first num-list)) nil)
		       (t (funcall recur (rest num-list)))) ))))
(defun all-oddp (l)
  (cond ((null l) t)
	((and (integerp (first l))  ;Check whether element is even an integer
				    ;first.
	      (evenp (first l))) nil)
	(t (all-oddp (rest l)))) )

;;;
;;;    8.7
;;;
; (defun my-member (elt l)
;   (cond ((null l) nil)
; 	((eql elt (first l)) l)
; 	(t (my-member elt (rest l)))) )
(defun Y2 (F)
  ((lambda (future)
     (funcall F #'(lambda (&rest args)
		    (apply (funcall future future) args))))
   #'(lambda (future)
       (funcall F #'(lambda (&rest args)
		      (apply (funcall future future) args)))) ) )

(setf (symbol-function 'my-member)
      (Y2 #'(lambda (recur)
	      #'(lambda (elt l)
		  (cond ((null l) nil)
			((eql elt (first l)) l)
			(t (funcall recur elt (rest l)))) ))))


;;;
;;;    8.27
;;;
; (defun square-list (num-list)
;   (cond ((null num-list) nil)
; 	(t (cons (* (first num-list) (first num-list))
; 		 (square-list (rest num-list)))) ) )

(setf (symbol-function 'square-list)
      (Y #'(lambda (recur)
	     #'(lambda (num-list)
		 (cond ((null num-list) nil)
		       (t (cons (* (first num-list) (first num-list))
				(funcall recur (rest num-list)))) )))) )

;;;
;;;    8.32 (Compare 8.41)
;;;
; (defun sum-numeric-elements (l)
;   (cond ((null l) 0)
; 	((numberp (first l)) (+ (first l) (sum-numeric-elements (rest l))))
; 	(t (sum-numeric-elements (rest l)))) )

(setf (symbol-function 'sum-numeric-elements)
      (Y #'(lambda (recur)
	     #'(lambda (l)
		 (cond ((null l) 0)
		       ((numberp (first l))
			(+ (first l) (funcall recur (rest l))))
		       (t (funcall recur (rest l)))) ))))

;;;
;;;    8.41 (Compare 8.32)
;;;
; (defun sum-tree (tree)
;   (cond ((numberp tree) tree)
; 	((atom tree) 0)
; 	(t (+ (sum-tree (first tree))
; 	      (sum-tree (rest tree)))) ) )
	
(setf (symbol-function 'sum-tree)
      (Y #'(lambda (recur)
	     #'(lambda (tree)
		 (cond ((numberp tree) tree)
		       ((atom tree) 0)
		       (t (+ (funcall recur (first tree))
			     (funcall recur (rest tree)))) )))) )

; (defun reverse-r (l)
;   (cond ((null (cdr l)) l)
; 	(t (cons (car (reverse-r (cdr l)))
; 		 (reverse-r (cons (car l)
; 				  (reverse-r
; 				   (cdr (reverse-r (cdr l)))) )))) ) )

(setf (symbol-function 'my-reverse)
      (Y #'(lambda (recur)
	     #'(lambda (l)
		 (cond
		   ((null (cdr l)) l)
		   (t (cons (car (funcall recur (cdr l)))
			    (funcall
			     recur
			     (cons (car l)
				   (funcall
				    recur
				    (cdr (funcall recur
						  (cdr l)))) )))) )))) )
	     
(setf (symbol-function 'my-reverse)
      ((lambda (F)
	 ((lambda (future)
	    (funcall F #'(lambda (arg)
			   (funcall (funcall future future) arg))))
	  #'(lambda (future)
	      (funcall F #'(lambda (arg)
			     (funcall (funcall future future) arg)))) ))
       #'(lambda (recur)
	   #'(lambda (l)
	       (cond
		 ((null (cdr l)) l)
		 (t (cons (car (funcall recur (cdr l)))
			  (funcall recur
				   (cons (car l)
					 (funcall recur
						  (cdr (funcall
							recur
							(cdr l)))) )))) )))) )

