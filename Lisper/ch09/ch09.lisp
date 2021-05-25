;#!/opt/local/bin/sbcl.bin --core /opt/local/lib/sbcl/sbcl.core --noinform
;#!/usr/local/bin/sbcl --noinform
;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch09.lisp
;;;;
;;;;   Started:            Mon Jul 30 22:58:49 2007
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

(defpackage ch09 (:use common-lisp test))

(in-package ch09)

(defun rember-f (test a l)
  (cond ((endp l) '())
	((funcall test (first l) a) (rest l))
	(t (cons (first l) (rember-f test a (rest l)))) ))

(deftest test-rember-f ()
  (check
   (equal (rember-f #'= 5 '(6 2 5 3)) '(6 2 3))
   (equal (rember-f #'eql 'jelly '(jelly beans are good)) '(beans are good))
   (equal (rember-f #'equal '(pop corn) '(lemonade (pop corn) and (cake))) '(lemonade and (cake)))) )

(defun eq-c (a)
  #'(lambda (x)
      (eql x a)))

;; (defun rember-f (test)
;;   #'(lambda (a l)
;;       (cond ((endp l) '())
;; 	    ((funcall test (first l) a) (rest l))
;; 	    (t (cons (first l) ??? )))) )






(defun rember-f (test)
  #'(lambda (a l)
      (cond ((endp l) '())
	    ((funcall test (first l) a) (rest l))
	    (t (cons (first l) (funcall (rember-f test) a (rest l)))) )))

;; (funcall (rember-f #'=) 5 '(6 2 5 3)) => (6 2 3)
;; (funcall (rember-f #'eql) 'jelly '(jelly beans are good)) => (BEANS ARE GOOD)
;; (funcall (rember-f #'equal) '(pop corn) '(lemonade (pop corn) and (cake))) => (LEMONADE AND (CAKE))
;; (funcall (rember-f #'eql) 'tuna '(shrimp salad and tuna salad)) => (SHRIMP SALAD AND SALAD)

;; (defun insertl (old new lat)
;;   (cond ((endp lat) '())
;; 	((eql (first lat) old) (cons new lat))
;; 	(t (cons (first lat) (insertl old new (rest lat)))) ))

(defun insertl-f (test)
  #'(lambda (old new lat)
      (cond ((endp lat) '())
	    ((funcall test (first lat) old) (cons new lat))
	    (t (cons (first lat) (funcall (insertl-f test) old new (rest lat)))) )))

(deftest test-insertl-f ()
  (check
   (equal (funcall (insertl-f #'eql) 'd 'e '(a b c d f g d h)) '(a b c e d f g d h))
   (equal (funcall (insertl-f #'eql) 'd 'e '(a b c)) '(a b c))
   (equal (funcall (insertl-f #'eql) 'a 'e '(a b c d f g d h)) '(e a b c d f g d h))))

;; (defun insertr (old new lat)
;;   (cond ((endp lat) '())
;; 	((eql (first lat) old) (cons old (cons new (rest lat))))
;; 	(t (cons (first lat) (insertr old new (rest lat)))) ))

(defun insertr-f (test)
  #'(lambda (old new lat)
      (cond ((endp lat) '())
	    ((funcall test (first lat) old) (cons old (cons new (rest lat))))
	    (t (cons (first lat) (funcall (insertr-f test) old new (rest lat)))) )))

(deftest test-insertr-f ()
  (check
   (equal (funcall (insertr-f #'eql) 'fudge 'topping '(ice cream with fudge for dessert)) '(ice cream with fudge topping for dessert))
   (equal (funcall (insertr-f #'eql) 'and 'jalapeno '(tacos tamales and salsa)) '(tacos tamales and jalapeno salsa))
   (equal (funcall (insertr-f #'eql) 'd 'e '(a b c d f g d h)) '(a b c d e f g d h))))

(defun insert-g (f)
  #'(lambda (old new lat)
      (cond ((endp lat) '())
	    ((eql (first lat) old) (funcall f old new lat))
	    (t (cons (first lat) (funcall (insert-g f) old new (rest lat)))) )))

;; (funcall (insert-g #'(lambda (old new lat) (cons new lat))) 'd 'e '(a b c d f g d h)) => (A B C E D F G D H)
;; (funcall (insert-g #'(lambda (old new lat) (cons old (cons new (rest lat))))) 'd 'e '(a b c d f g d h)) => (A B C D E F G D H)
