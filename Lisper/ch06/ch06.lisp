;#!/opt/local/bin/sbcl.bin --core /opt/local/lib/sbcl/sbcl.core --noinform
;#!/usr/local/bin/sbcl --noinform
;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch06.lisp
;;;;
;;;;   Started:            Sat Jul 28 02:13:40 2007
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

(defpackage ch06 (:use common-lisp test ch04) (:shadow equal))

(in-package ch06)

(defun leftmost (l)
  (cond ((null l) '())
	((atom (car l)) (car l))
	(t (leftmost (car l)))) )

;;;
;;;    Theirs (paraphrased)
;;;    
(defun leftmost (l)
  (cond ((null l) '())
	((consp (car l)) (leftmost (car l)))
	(t (car l))))

(deftest test-leftmost ()
  (check
   (eql (leftmost '(((() four)) 17 (seventeen))) '())
   (eql (leftmost '((((4) four)) 17 (seventeen))) 4)
   (eql (leftmost '((hot) (tuna (and)) cheese)) 'hot)
   (eql (leftmost '(((hamburger) french) (fries (and a) coke))) 'hamburger)))

;;;
;;;    See "Little Schemer" Preface
;;;    
(defun non-atom (s)
  (consp s))

;; (defun not (b)
;;   (cond (b nil)
;; 	(t)))

(defun rember* (a l)
  (cond ((null l) '())
	((atom l) l)
	((eql (car l) a) (rember* a (cdr l)))
	(t (cons (rember* a (car l)) (rember* a (cdr l)))) ))

;;;
;;;    Theirs
;;;
(defun rember1* (a l)
  (cond ((null l) '())
	((non-atom (car l)) (cons (rember1* a (car l)) (rember1* a (cdr l))))
	(t (cond ((eql (car l) a) (rember1* a (cdr l)))
		 (t (cons (car l) (rember1* a (cdr l)))) ))))

;;;
;;;    In other words...
;;;    
(defun rember1a* (a l)
  (cond ((null l) '())                                                        ; Done
	((consp (car l)) (cons (rember1a* a (car l)) (rember1a* a (cdr l))))  ; Simplify/recur/rebuild
	((eql (car l) a) (rember1a* a (cdr l)))                               ; Remove
	(t (cons (car l) (rember1a* a (cdr l)))) ))                           ; Carry on

;;;
;;;    Theirs is actually elegant. Especially the second COND clause--Take the CONS apart, process CAR and CDR, then CONS these
;;;    results back together again.

;;;
;;;    Theirs is the same as mine assuming A is always an atom. Mine will remove (certain) top-level lists as well:
;; (let* ((l (list 'a 'b '(c)))
;;        (a (third l)))
;;   (push a l)
;;   (print l)
;;   (rember* a l))
;; This yields (A B), whereas using REMBER1* would yield ((C) A B (C)).
;; But (rember* '(c) '((c) a b (c))) => ((C) A B (C)) in any case.


(defun insertr* (old new l)
  (cond ((null l) '())
	((consp (car l)) (cons (insertr* old new (car l)) (insertr* old new (cdr l))))
	((eql (car l) old) (cons old (cons new (insertr* old new (cdr l)))) )
	(t (cons (car l) (insertr* old new (cdr l)))) ))

(deftest test-insertr* ()
  (check
   (cl:equal (insertr* 'chuck 'roast '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))
	  '((how much (wood)) could ((a (wood) chuck roast)) (((chuck roast))) (if (a) ((wood chuck roast))) could chuck roast wood))))

(defun occur* (a l)
  (cond ((null l) 0)
	((consp (car l)) (+ (occur* a (car l)) (occur* a (cdr l))))
	((eql (car l) a) (1+ (occur* a (cdr l))))
	(t (occur* a (cdr l)))) )

(deftest test-occur* ()
  (check
   (= (occur* 'banana '((banana) (split ((((banana ice))) (cream (banana)) sherbet)) (banana) (bread) (banana brandy))) 5)))

(defun occur* (a l)
  (cond ((eql a l) 1)
	((atom l) 0)
	(t (+ (occur* a (car l)) (occur* a (cdr l)))) ))

(defun subst* (old new l)
  (cond ((null l) '())
	((consp (car l)) (cons (subst* old new (car l)) (subst* old new (cdr l))))
	((eql (car l) old) (cons new (subst* old new (cdr l))))
	(t (cons (car l) (subst* old new (cdr l)))) ))

(deftest test-subst* ()
  (check
   (cl:equal (subst* 'banana 'orange '((banana) (split ((((banana ice))) (cream (banana)) sherbet)) (banana) (bread) (banana brandy)))
	  '((orange) (split ((((orange ice))) (cream (orange)) sherbet)) (orange) (bread) (orange brandy)))))

;;;
;;;    Weird because F is called in two different contexts? One returns a result, the other returns a function.
;;;
;;;    Broken...
;;;    
;; (defun tree-process (l f g)
;;   (cond ((null l) (funcall f l))
;; 	((consp (car l)) (funcall g (tree-process (car l) f) (tree-process (cdr l) f)))
;; 	(t (let ((result (funcall f l)))
;; 	     (if result
;; 		 (funcall result (tree-process (cdr l) f))
;; 		 (funcall g (car l) (tree-process (cdr l) f)))) )))

;; (defun occur* (a l)
;;   (tree-process l #'(lambda (tree)
;; 		      (cond ((null tree) 0)
;; 			    ((eql (car l) a) #'1+)))) )

(defun insertl* (old new l)
  (cond ((null l) '())
	((consp (car l)) (cons (insertl* old new (car l)) (insertl* old new (cdr l))))
	((eql (car l) old) (cons new (cons old (insertl* old new (cdr l)))) )
	(t (cons (car l) (insertl* old new (cdr l)))) ))

(deftest test-insertl* ()
  (check
   (cl:equal (insertl* 'chuck 'pecker '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))
	  '((how much (wood)) could ((a (wood) pecker chuck)) (((pecker chuck))) (if (a) ((wood pecker chuck))) could pecker chuck wood))))

(defun member* (a l)
  (cond ((null l) '())
	((consp (car l)) (or (member* a (car l)) (member* a (cdr l))))
	((eql (car l) a) t)
	(t (member* a (cdr l)))) )

(deftest test-member* ()
  (check
   (member* 'chips '((potato) (chips ((with) fish) (chips)))) ))

;;;
;;;    Theirs
;;;
(defun member* (a l)
  (cond ((null l) '())
	((consp (car l)) (or (member* a (car l)) (member* a (cdr l))))
	(t (or (eql (car l) a) (member* a (cdr l)))) ))

(defun eqlist (l1 l2)
  (cond ((null l1) (null l2))
	((null l2) nil)
	((consp (car l1)) (and (consp (car l2))
			       (eqlist (car l1) (car l2))
			       (eqlist (cdr l1) (cdr l2))))
	(t (and (ch04::eqan (car l1) (car l2))
		(eqlist (cdr l1) (cdr l2)))) ))

(deftest test-eqlist ()
  (check
   (eqlist '(strawberry ice cream) '(strawberry ice cream))
   (not (eqlist '(strawberry ice cream) '(strawberry cream ice)))
   (not (eqlist '(banana ((split))) '((banana) (split))))
   (not (eqlist '(beef ((sausage)) (and (soda))) '(beef ((salami)) (and (soda)))) )
   (eqlist '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda)))) ))

;;;
;;;    Theirs
;;;
(defun eqlist (l1 l2)
  (cond ((and (null l1) (null l2)) t)
	((or (null l1) (null l2)) nil)
	((and (consp (car l1)) (consp (car l2)))
	 (and (eqlist (car l1) (car l2)) (eqlist (cdr l1) (cdr l2))))
	((or (consp (car l1)) (consp (car l2))) nil)
	(t (and (ch04::eqan (car l1) (car l2)) (eqlist (cdr l1) (cdr l2)))) ))

;;;
;;;    Mutually recursive
;;;
(defun equal (s1 s2)
  (cond ((and (atom s1) (atom s2)) (ch04::eqan s1 s2))
	((and (consp s1) (consp s2)) (eqlist s1 s2))
	(t nil)))

(defun equal (s1 s2)
  (cond ((and (atom s1) (atom s2)) (ch04::eqan s1 s2))
	((or (atom s1) (atom s2)) nil)
	(t (eqlist s1 s2))))

(defun eqlist (l1 l2)
  (cond ((and (null l1) (null l2)) t)
	((or (null l1) (null l2)) nil)
	(t (and (equal (car l1) (car l2))
		(equal (cdr l1) (cdr l2)))) ))

;;;
;;;    multirember -> rember*
;;;    rember -> ?
(defun rember (s l)
  (cond ((endp l) '())
	((equal (first l) s) (rest l))
	(t (cons (first l) (rember s (rest l)))) ))

(deftest test-rember ()
  (check
   (cl:equal (rember '(a b) '(a b (a b) c d (a b))) '(A B C D (A B)))) )