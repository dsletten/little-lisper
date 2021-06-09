;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               ch06.lisp
;;;
;;;   STARTED:            Thu Jan 24 00:13:26 2002
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
;;;   NOTES: These functions generally violate The Law of Eq? In the book, both
;;;          args must be atoms. 210527
;;;
;;;
(defun rember* (elt obj)
  (cond ((null obj) ())
	((atom obj) obj)
	((eq (car obj) elt)
	 (rember* elt (cdr obj)))
	(t (cons (rember* elt (car obj))
		 (rember* elt (cdr obj)))) ) )

; [8]> (rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))
; ((COFFEE) ((TEA)) (AND (HICK)))
; [9]> (rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))
; (((TOMATO)) ((BEAN)) (AND ((FLYING))))

;;;
;;;    Even more succinctly:
;;;    
; (defun rember* (elt obj)
;   (cond ((atom obj) obj)
; 	((eq (car obj) elt)
; 	 (rember* elt (cdr obj)))
; 	(t (cons (rember* elt (car obj))
; 		 (rember* elt (cdr obj)))) ) )

(defun occur* (a l)
  (cond ((eq a l) 1)
	((atom l) 0)
	(t (+ (occur* a (car l))
	      (occur* a (cdr l)))) ) )

; [2]> (occur* 'banana '((banana) (split ((((banana ice))) (cream (banana)) sherbet)) (banana) (bread) (banana brandy)))
; 5

(defun subst* (old new l)
  (cond ((eq old l) new)
	((atom l) l)
	(t (cons (subst* old new (car l))
		 (subst* old new (cdr l)))) ) )

(defun insert-l* (old new l)
  (cond ((null l) ())
	((atom l) l)
	((eq (car l) old)
	 (cons new (cons old (insert-l* old new (cdr l)))) )
	(t (cons (insert-l* old new (car l))
		 (insert-l* old new (cdr l)))) ) )

(defun member* (a l)
  (cond ((null l) nil)
	((eq a l) t)
	((atom l) nil)
	(t (or (member* a (car l))
	       (member* a (cdr l)))) ) )

(defun eqan (a1 a2)
  (cond ((numberp a1)
	 (and (numberp a2) (= a1 a2)))
	((numberp a2) nil)
	(t (eq a1 a2))) )

(defun eqlist? (l1 l2)
  (cond ((null l1) (null l2))
	((null l2) nil)
	((atom (car l1))
	 (and (atom (car l2))
	      (eqan? (car l1) (car l2))
	      (eqlist? (cdr l1) (cdr l2))))
	((atom (car l2)) nil)
	(t (and (eqlist? (car l1) (car l2))
		(eqlist? (cdr l1) (cdr l2)))) ) )

