;#!/opt/local/bin/sbcl.bin --core /opt/local/lib/sbcl/sbcl.core --noinform
;#!/usr/local/bin/sbcl --noinform
;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch04.lisp
;;;;
;;;;   Started:            Wed Jul 25 00:23:23 2007
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

(defpackage ch04 (:use common-lisp test) (:shadow + - * > < = length) (:export eqan))

(in-package ch04)

(defun + (m n)
  (if (zerop n)
      m
      (1+ (+ m (1- n)))) )

(deftest test+ ()
  (check
   (cl:= (+ 46 12) 58)
   (cl:= (+ 2 3) (cl:+ 2 3))
   (cl:= (+ 5 0) (cl:+ 5 0))
   (cl:= (+ 0 5) (cl:+ 0 5))
   (cl:= (+ 0 5) (+ 5 0))))

(defun + (m n)
  (if (zerop n)
      m
      (+ (1+ m) (1- n))))

;;;
;;;    m - n = m - n + 0
;;;          = m - n + (1 - 1)
;;;          = (m - n + 1) - 1
;;;          = (m - (n - 1)) - 1
;;;          
(defun - (m n)
  (if (zerop n)
      m
      (1- (- m (1- n)))) )

(deftest test- ()
  (check
   (cl:= (- 8 3) 5)
   (cl:= (- 17 9) 8)
   (cl:= (- 6 2) (cl:- 6 2))
   (cl:= (- 3 0) (cl:- 3 0))))

;;;
;;;    m - n = m - n + 0
;;;          = m - n + (1 - 1)
;;;          = m - 1 - n + 1
;;;          = (m - 1) - (n - 1)
;;;          
(defun - (m n)
  (if (zerop n)
      m
      (- (1- m) (1- n))))

(defun addvec (vec)
  (if (endp vec)
      0
      (+ (first vec) (addvec (rest vec)))) )

(deftest test-addvec ()
  (check
   (cl:= (addvec '(3 5 2 8)) 18)
   (cl:= (addvec '(15 6 7 12 3)) 43)
   (cl:= (addvec '(1 2 3 4)) (apply #'cl:+ '(1 2 3 4)))
   (cl:= (addvec '()) (apply #'cl:+ '()))))

;;;
;;;    mn = m(n + 0)
;;;       = m(n + 1 - 1)
;;;       = m(n - 1 + 1)
;;;       = m(n - 1) + m
;;;       
(defun * (m n)
  (if (zerop n)
      0
      (+ (* m (1- n)) m)))

(deftest test* ()
  (check
   (cl:= (* 5 3) 15)
   (cl:= (* 13 4) 52)
   (cl:= (* 12 3) 36)
   (cl:= (* 2 3) (cl:* 2 3))
   (cl:= (* 2 3) (* 3 2))
   (cl:= (* 4 1) (cl:* 4 1))
   (cl:= (* 3 0) (cl:* 3 0))))

;;;
;;;    Assumes both VEC's are same length.
;;;    
(defun vec+ (vec1 vec2)
  (if (endp vec1)
      '()
      (cons (+ (first vec1) (first vec2)) (vec+ (rest vec1) (rest vec2)))) )

(deftest test-vec+ ()
  (check
   (equal (vec+ '(3 6 9 11 4) '(8 5 2 0 7)) '(11 11 11 11 11))
   (equal (vec+ '(2 3) '(4 6)) '(6 9))
   (equal (vec+ '(3 7) '(4 6)) '(7 13))))

(defun vec+ (vec1 vec2)
  (cond ((endp vec1) vec2)
	((endp vec2) vec1)
	(t (cons (+ (first vec1) (first vec2)) (vec+ (rest vec1) (rest vec2)))) ))

(deftest test-vec+ ()
  (check
   (equal (vec+ '(3 6 9 11 4) '(8 5 2 0 7)) '(11 11 11 11 11))
   (equal (vec+ '(2 3) '(4 6)) '(6 9))
   (equal (vec+ '(3 6 9 11 4 8) '(8 5 2 0 7)) '(11 11 11 11 11 8))
   (equal (vec+ '(2 3) '(4 6 7)) '(6 9 7))
   (equal (vec+ '(3 7) '(4 6)) '(7 13))))

(defun vec+ (vec1 vec2)
  (mapcar #'+ vec1 vec2))

(deftest test-vec+ ()
  (check
   (equal (vec+ '(3 6 9 11 4) '(8 5 2 0 7)) '(11 11 11 11 11))
   (equal (vec+ '(2 3) '(4 6)) '(6 9))
   (equal (vec+ '(3 6 9 11 4 8) '(8 5 2 0 7)) '(11 11 11 11 11))
   (equal (vec+ '(2 3) '(4 6 7)) '(6 9))
   (equal (vec+ '(3 7) '(4 6)) '(7 13))))

(defun > (m n)
  (cond ((zerop m) nil)
	((zerop n) t)
	(t (> (1- m) (1- n)))) )

(deftest test> ()
  (check
   (not (> 12 13))
   (> 12 11)
   (not (> 3 3))))

(defun < (m n)
  (cond ((zerop n) nil)
	((zerop m) t)
	(t (< (1- m) (1- n)))) )

(deftest test< ()
  (check
   (< 4 6)
   (not (< 8 3))
   (not (< 6 6))))

(defun = (m n)
  (cond ((zerop m) (zerop n))
	((zerop n) nil)
	(t (= (1- m) (1- n)))) )

(deftest test= ()
  (check
   (= 5 5)
   (not (= 2 3))
   (not (= 4 5))))

;; (defun = (m n)
;;   (cond ((< m n) nil)
;; 	((> m n) nil)
;; 	(t t)))

(defun ^ (m n)
  (if (zerop n)
      1
      (* m (^ m (1- n)))) )

(deftest test^ ()
  (check
   (cl:= (^ 1 1) 1)
   (cl:= (^ 2 3) 8)
   (cl:= (^ 5 3) 125)))

(defun length (lat)
  (if (endp lat)
      0
      (1+ (length (rest lat)))) )

(deftest test-length ()
  (check
   (cl:= (length '(hotdogs with mustard sauerkraut and pickles)) (cl:length '(hotdogs with mustard sauerkraut and pickles)))
   (cl:= (length '(ham and cheese on rye)) (cl:length '(ham and cheese on rye)))) )

;;;
;;;    1-based index.
;;;    (pick 0 '(a)) => No answer (Below returns NIL. Theirs in principle produces an error since (1- 0) is not defined.)
;;;    
(defun pick (n lat)
  (cond ((endp lat) nil)
	((= n 1) (first lat)) ; They use ((zerop (1- n)) (first lat))
	(t (pick (1- n) (rest lat)))) )

(deftest test-pick ()
  (check
   (equal (pick 4 '(lasagna spaghetti ravioli macaroni meatball)) 'macaroni)
   (equal (pick 0 '()) nil)))

(defun rempick (n lat)
  (cond ((null lat) '())
	((= n 1) (rest lat)) ; They use ((zerop (1- n)) (rest lat))
	(t (cons (first lat) (rempick (1- n) (rest lat)))) ))

(deftest test-rempick ()
  (check
   (equal (rempick 3 '(hotdogs with hot mustard)) '(hotdogs with mustard))
   (equal (rempick 0 '()) '())))

(defun no-nums (lat)
  (cond ((endp lat) '())
	((numberp (first lat)) (no-nums (rest lat)))
	(t (cons (first lat) (no-nums (rest lat)))) ))

(deftest test-no-nums ()
  (check
   (equal (no-nums '(5 pears 6 prunes 9 dates)) '(pears prunes dates))))

(defun all-nums (lat)
  (cond ((endp lat) '())
	((numberp (first lat)) (cons (first lat) (all-nums (rest lat))))
	(t (all-nums (rest lat)))) )

(deftest test-all-nums ()
  (check
   (equal (all-nums '(5 pears 6 prunes 9 dates)) '(5 6 9))))

(defun eqan (a1 a2)
  (cond ((numberp a1) (and (numberp a2) (= a1 a2)))
	((numberp a2) nil)
	(t (eql a1 a2))))

(deftest test-eqan ()
  (check
   (eqan 8 8)
   (not (eqan 8 9))
   (not (eqan 8 'foo))
   (eqan 'foo 'foo)
   (not (eqan 'foo 'bar))))

