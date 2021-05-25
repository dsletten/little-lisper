;#!/opt/local/bin/sbcl.bin --core /opt/local/lib/sbcl/sbcl.core --noinform
;#!/usr/local/bin/sbcl --noinform
;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch08.lisp
;;;;
;;;;   Started:            Mon Jul 30 10:42:10 2007
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

(defpackage ch08 (:use common-lisp test ch05) (:shadow subsetp union set-difference))

(in-package ch08)

(defun setp (lat)
  (cond ((endp lat) t)
	((member (first lat) (rest lat) :test #'equal) nil)
	(t (setp (rest lat)))) )

(deftest test-setp ()
  (check
   (not (setp '(apple peaches apple plum)))
   (setp '(apples peaches pears plums))
   (not (setp '(apple 3 pear 4 9 apple 3 4)))) )

(defun makeset (lat)
  (cond ((endp lat) '())
	((member (first lat) (rest lat) :test #'equal) (makeset (rest lat)))
	(t (cons (first lat) (makeset (rest lat)))) ))

(deftest test-makeset ()
  (check
   (equal (makeset '(apple peach pear peach plum apple lemon peach)) '(pear plum apple lemon peach))))

(defun makeset (lat)
  (cond ((endp lat) '())
	(t (cons (first lat) (makeset (ch05::multi-rember (first lat) (rest lat)))) )))

(deftest test-makeset ()
  (check
   (equal (makeset '(apple peach pear peach plum apple lemon peach)) '(apple peach pear plum lemon))))

(defun subsetp (set1 set2)
  (cond ((endp set1) t)
	((member (first set1) set2) (subsetp (rest set1) set2))
	(t nil)))

(deftest test-subsetp ()
  (check
   (subsetp '(5 chicken wings) '(5 hamburgers 2 pieces fried chicken and light duckling wings))
   (not (subsetp '(4 pounds of horseradish) '(fours pounds chicken and 5 ounces horseradish)))) )

(defun subsetp (set1 set2)
  (if (endp set1)
      t
      (and (member (first set1) set2)
	   (subsetp (rest set1) set2))))

(defun eqset (set1 set2)
  (and (subsetp set1 set2)
       (subsetp set2 set1)))

(deftest test-eqset ()
  (check
   (eqset '(6 large chickens with wings) '(6 chickens with large wings))))

(defun intersectp (set1 set2)
  (cond ((endp set1) nil)
	((member (first set1) set2) t)
	(t (intersectp (rest set1) set2))))

(deftest test-intersectp ()
  (check
   (intersectp '(tomatoes and macaroni) '(macaroni and cheese))))

(defun intersectp (set1 set2)
  (if (endp set1)
      nil
      (or (member (first set1) set2)
	  (intersectp (rest set1) set2))))

(defun intersect (set1 set2)
  (cond ((endp set1) '())
	((member (first set1) set2) (cons (first set1) (intersect (rest set1) set2)))
	(t (intersect (rest set1) set2))))

(deftest test-intersect ()
  (check
   (equal (intersect '(tomatoes and macaroni) '(macaroni and cheese)) '(and macaroni))))

(defun intersect (set1 set2)
  (cond ((endp set1) '())
	((not (member (first set1) set2)) (intersect (rest set1) set2))
	(t (cons (first set1) (intersect (rest set1) set2)))) )

(defun union (set1 set2)
  (cond ((endp set1) set2)
	((member (first set1) set2) (union (rest set1) set2))
	(t (cons (first set1) (union (rest set1) set2)))) )

(deftest test-union ()
  (check
   (equal (union '(tomatoes and macaroni casserole) '(macaroni and cheese)) '(tomatoes casserole macaroni and cheese))))

(defun set-difference (set1 set2)
  (cond ((endp set1) '())
	((member (first set1) set2) (set-difference (rest set1) set2))
	(t (cons (first set1) (set-difference (rest set1) set2)))) )

(defun intersectall (l-set)
  (cond ((endp l-set) '())
	((endp (rest l-set)) (first l-set))
	(t (intersect (first l-set) (intersectall (rest l-set)))) ))

(defun build (a1 a2)
  (list a1 a2))

;;;
;;;    Assumes REL is valid relation (i.e., set).
;;;    
(defun funp (rel)
  (cond ((endp rel) t)
	((member (first (first rel)) (rest rel) :key #'first) nil)
	(t (funp (rest rel)))) )

(deftest test-funp ()
  (check
   (funp '((8 3) (4 2) (7 6) (6 2) (3 4)))))

(defun funp (rel)
  (setp (ch03::firsts rel)))

;;;
;;;    Assumes REL is valid relation.
;;;    
(defun revrel (rel)
  (cond ((endp rel) '())
	((cons (build (second (first rel)) (first (first rel))) (revrel (rest rel)))) ))

(deftest test-revrel ()
  (check
   (equal (revrel '((8 a) (pumpkin pie) (got sick))) '((a 8) (pie pumpkin) (sick got)))) )

(defun revrel (rel)
  (mapcar #'reverse rel))

(defun fullfunp (fun)
  (cond ((endp fun) t)
	((member (second (first fun)) (rest fun) :key #'second) nil)
	(t (fullfunp (rest fun)))) )

(deftest test-fullfunp ()
  (check
   (not (fullfunp '((8 3) (4 2) (7 6) (6 2) (3 4)))) ))

(defun seconds (l)
  (mapcar #'second l))

(defun fullfunp (fun)
  (setp (seconds fun)))

(defun fullfunp (fun)
  (funp (revrel fun)))

	