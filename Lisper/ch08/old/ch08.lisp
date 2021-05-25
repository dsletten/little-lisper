;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               ch08.lisp
;;;
;;;   STARTED:            Sat Jan 26 16:25:59 2002
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
(defun set? (lat)
  (cond ((null lat) t)
	((member (car lat) (cdr lat)) nil)
	(t (set? (cdr lat)))) )

(defun makeset (lat)
  (cond ((null lat) ())
	((member (car lat) (cdr lat))
	 (makeset (cdr lat)))
	(t (cons (car lat) (makeset (cdr lat)))) ) )

(defun makeset (lat)
  (cond ((null lat) ())
	(t (cons (car lat)
		 (makeset (multirember (car lat) (cdr lat)))) )) )

(defun subset? (s1 s2)
  (cond ((null s1) t)
	((member (car s1) s2)
	 (subset? (cdr s1) s2))
	(t nil)) )

(defun intersect? (s1 s2)
  (cond ((null s1) nil)
	((member (car s1) s2) t)
	(t (intersect? (cdr s1) s2))) )

(defun intersect (s1 s2)
  (cond ((null s1) ())
	((member (car s1) s2)
	 (cons (car s1)
	       (intersect (cdr s1) s2)))
	(t (intersect (cdr s1) s2))) )

(defun revrel (rel)
  (cond ((null rel) ())
	(t (cons (reverse (car l))
		 (revrel (cdr l)))) ) )
			  