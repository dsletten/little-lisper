;#!/opt/local/bin/sbcl.bin --core /opt/local/lib/sbcl/sbcl.core --noinform
;#!/usr/local/bin/sbcl --noinform
;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch03.lisp
;;;;
;;;;   Started:            Sun Jul 22 02:40:57 2007
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

(defpackage ch03 (:use common-lisp test) (:shadow substitute))

(in-package ch03)

(defun rember (a lat)
  (cond ((endp lat) '())
	((eql a (first lat)) (rest lat))
	(t (cons (first lat) (rember a (rest lat)))) ))

(deftest test-rember ()
  (check
   (equal (rember 'mint '(lamb chops and mint jelly)) '(lamb chops and jelly))
   (equal (rember 'mint '(lamb chops and mint flavored mint jelly)) '(lamb chops and flavored mint jelly))
   (equal (rember 'toast '(bacon lettuce and tomato)) '(bacon lettuce and tomato))
   (equal (rember 'cup '(coffee cup tea cup and hick cup)) '(coffee tea cup and hick cup))
   (equal (rember 'sauce '(soy sauce and tomato sauce)) '(soy and tomato sauce))))

(defun firsts (l)
  (if (endp l)
      '()
      (cons (first (first l)) (firsts (rest l)))) )

(deftest test-firsts ()
  (check
   (equal (firsts '((apple peach pumpkin) (plum pear cherry) (grape raisin pea) (bean carrot eggplant))) '(apple plum grape bean))
   (equal (firsts '((a b) (c d) (e f))) '(a c e))
   (equal (firsts '()) '())
   (equal (firsts '((five plums) (four) (eleven green oranges))) '(five four eleven))))

(defun firsts2 (l)
  (mapcar #'first l))

(deftest test-firsts2 ()
  (check
   (equal (firsts '((apple peach pumpkin) (plum pear cherry) (grape raisin pea) (bean carrot eggplant)))
	  (firsts2 '((apple peach pumpkin) (plum pear cherry) (grape raisin pea) (bean carrot eggplant))))
   (equal (firsts '((a b) (c d) (e f)))
	  (firsts2 '((a b) (c d) (e f))))
   (equal (firsts '())
	  (firsts2 '()))
   (equal (firsts '((five plums) (four) (eleven green oranges)))
	  (firsts2 '((five plums) (four) (eleven green oranges)))) ))

(defun insertr (old new lat)
  (cond ((endp lat) '())
	((eql (first lat) old) (cons old (cons new (rest lat))))
	(t (cons (first lat) (insertr old new (rest lat)))) ))

(deftest test-insertr ()
  (check
   (equal (insertr 'fudge 'topping '(ice cream with fudge for dessert)) '(ice cream with fudge topping for dessert))
   (equal (insertr 'and 'jalapeno '(tacos tamales and salsa)) '(tacos tamales and jalapeno salsa))
   (equal (insertr 'd 'e '(a b c d f g d h)) '(a b c d e f g d h))))

(defun insertl (old new lat)
  (cond ((endp lat) '())
	((eql (first lat) old) (cons new lat))
	(t (cons (first lat) (insertl old new (rest lat)))) ))

(deftest test-insertl ()
  (check
   (equal (insertl 'd 'e '(a b c d f g d h)) '(a b c e d f g d h))
   (equal (insertl 'd 'e '(a b c)) '(a b c))
   (equal (insertl 'a 'e '(a b c d f g d h)) '(e a b c d f g d h))))

(defun substitute (old new lat)
  (cond ((endp lat) '())
	((eql (first lat) old) (cons new (rest lat)))
	(t (cons (first lat) (substitute old new (rest lat)))) ))

(deftest test-substitute ()
  (check
   (equal (substitute 'a 'b '(a b c)) '(b b c))
   (equal (substitute 'a 'b '(b a a c)) '(b b a c))
   (equal (substitute 'a 'b '(d e f)) '(d e f))))

(defun substitute2 (o1 o2 new lat)
  (cond ((endp lat) '())
	((eql (first lat) o1) (cons new (rest lat)))
	((eql (first lat) o2) (cons new (rest lat)))
	(t (cons (first lat) (substitute2 o1 o2 new (rest lat)))) ))

(defun substitute2 (o1 o2 new lat)
  (cond ((endp lat) '())
	((or (eql (first lat) o1) (eql (first lat) o2))
	 (cons new (rest lat)))
	(t (cons (first lat) (substitute2 o1 o2 new (rest lat)))) ))