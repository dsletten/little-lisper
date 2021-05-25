;#!/opt/local/bin/sbcl.bin --core /opt/local/lib/sbcl/sbcl.core --noinform
;#!/usr/local/bin/sbcl --noinform
;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch05.lisp
;;;;
;;;;   Started:            Fri Jul 27 02:19:01 2007
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

(defpackage ch05 (:use common-lisp test) (:shadow member))

(in-package ch05)

(defun member (a lat)
  (cond ((endp lat) '())
	((eql (first lat) a) lat)
	(t (member a (rest lat)))) )

;;;
;;;    Theirs (sort of)
;;;    
;; (defun member (a lat)
;;   (if (endp lat)
;;       nil
;;       (or (eql (first lat) a)
;; 	  (member a (rest lat)))) )

(defun rember (a lat)
  (cond ((endp lat) '())
	((eql (first lat) a) (rest lat))
	(t (cons (first lat) (rember a (rest lat)))) ))

(defun multi-rember (a lat)
  (cond ((endp lat) '())
	((eql (first lat) a) (multi-rember a (rest lat)))
	(t (cons (first lat) (multi-rember a (rest lat)))) ))

(deftest test-multi-rember ()
  (check
   (equal (multi-rember 'pung '(foo pung bar pung baz pung)) '(foo bar baz))
   (equal (multi-rember 'sneep '(foo pung bar pung baz pung)) '(foo pung bar pung baz pung))))

(defun multi-insertr (old new lat)
  (cond ((endp lat) '())
	((eql (first lat) old) (cons old (cons new (multi-insertr old new (rest lat)))) )
	(t (cons (first lat) (multi-insertr old new (rest lat)))) ))

(deftest test-multi-insertr ()
  (check
   (equal (multi-insertr 'pung 'peach '(foo pung bar pung baz pung)) '(foo pung peach bar pung peach baz pung peach))
   (equal (multi-insertr 'sneep 'peach '(foo pung bar pung baz pung)) '(foo pung bar pung baz pung))))

(defun multi-insertl (old new lat)
  (cond ((endp lat) '())
	((eql (first lat) old) (cons new (cons old (multi-insertl old new (rest lat)))) )
	(t (cons (first lat) (multi-insertl old new (rest lat)))) ))

(defun multi-subst (old new lat)
  (cond ((endp lat) '())
	((eql (first lat) old) (cons new (multi-subst old new (rest lat))))
	(t (cons (first lat) (multi-subst old new (rest lat)))) ))

(defun occur (a lat)
  (cond ((endp lat) 0)
	((eql (first lat) a) (1+ (occur a (rest lat))))
	(t (occur a (rest lat)))) )

(deftest test-occur ()
  (check
   (= (occur 'pung '(foo pung bar pung baz pung)) 3)
   (= (occur 'sneep '(foo pung bar pung baz pung)) 0)))

(defun onep (n)
  (if (zerop (1- n))
      t
      nil))

;;;
;;;    Theirs guards against 0 -> negative.
;;;
(defun one? (n)
  (cond ((zerop n) nil)
	(t (zerop (1- n)))) )

(defun onep (n)
  (zerop (1- n)))

(defun onep (n)
  (= n 1))

(defun rempick (n lat)
  (cond ((endp lat) '())
	((onep n) (rest lat))
	(t (cons (first lat) (rempick (1- n) (rest lat)))) ))
