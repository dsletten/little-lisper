;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               little.lisp
;;;;
;;;;   Started:            Sun Oct  3 02:49:38 2010
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
;;;;   Little Lisper Laws
;;;;   -CAR/CDR not defined for empty list.
;;;;   -2nd arg of CONS must be list.
;;;;   -NULL? only defined for lists.
;;;;   -EQ? 2 atomic args.
;;;;
;;;;
(load "/Users/dsletten/lisp/packages/test")

(defpackage little (:use common-lisp test))

(in-package little)

;;;
;;;    Ch. 2
;;;
(defun latp (l)
  (cond ((endp l) t)
        ((atom (first l)) (latp (rest l)))
        (t nil)))

(deftest test-latp ()
  (check
   (latp '(jack sprat could eat no chicken fat))
   (not (latp '((jack) sprat could eat no chicken fat)))
   (not (latp '(jack (sprat could) eat no chicken fat)))
   (latp '())
   (latp '(bacon and eggs))
   (not (latp '(bacon (and eggs))))
   (latp '(is this not pung?))
   (not (latp '(is (this) not pung?)))) )

;; (defun latp (l)
;;   (every #'atom l))

(defun memberp (a lat)
  (cond ((endp lat) nil)
        ((eql (first lat) a) t)
        (t (memberp a (rest lat)))) )

(defun memberp (a lat)
  (cond ((endp lat) nil)
        ((eql (first lat) a) lat)
        (t (memberp a (rest l)))) ) ; Oops! 210522

(deftest test-memberp ()
  (check
   (memberp 'tea '(coffee tea or milk))
   (not (memberp 'poached '(fried eggs and scrambled eggs)))
   (memberp 'meat '(mashed potatoes and meat gravy))
   (not (memberp 'liver '(bagels and lox)))) )

;;;
;;;    Ch. 3
;;;
(defun rember (a lat)
  (cond ((endp lat) '())
        ((eql (first lat) a) (rest lat))
        (t (cons (first lat) (rember a (rest lat)))) ))

;; (defun rember (a lat)
;;   (do ((l lat (rest l))
;;        (result '()))
;;       ((endp l) (nreverse result))
;;     (if (eql (first l) a)
;;         (return (nconc (nreverse result) (rest l)))
;;         (push (first l) result))))

;; (defun rember (a lat)
;;   (labels ((rember-aux (l result)
;;              (cond ((endp l) (nreverse result))
;;                    ((eql (first l) a) (nconc (nreverse result) (rest l)))
;;                    (t (rember-aux (rest l) (cons (first l) result)))) ))
;;     (rember-aux lat '())))

;; (defun rember (a lat)
;;   (remove a lat :count 1))

;;;
;;;    FLAG used simply to prevent more than 1 elt being removed.
;;;    
;; (defun rember (a lat)
;;   (let ((flag nil))
;;     (nreverse (reduce #'(lambda (result elt)
;;                           (if (and (eql elt a) (not flag))
;;                               (progn (setf flag t)
;;                                      result)
;;                               (cons elt result)))
;;                       lat
;;                       :initial-value '()))) )

(deftest test-rember ()
  (check
   (equal (rember 'mint '(lamb chops and mint jelly)) '(LAMB CHOPS AND JELLY))
   (equal (rember 'mint '(lamb chops and mint flavored mint jelly)) '(LAMB CHOPS AND FLAVORED MINT JELLY))
   (equal (rember 'toast '(bacon lettuce and tomato)) '(BACON LETTUCE AND TOMATO))
   (equal (rember 'cup '(coffee cup tea cup and hick cup)) '(COFFEE TEA CUP AND HICK CUP))
   (equal (rember 'bacon '(bacon lettuce and tomato)) '(lettuce and tomato))
   (equal (rember 'and '(bacon lettuce and tomato)) '(bacon lettuce tomato))
   (equal (rember 'sauce '(soy sauce and tomato sauce)) '(soy and tomato sauce))
   (equal (rember 'pung '(soy sauce and tomato sauce)) '(soy sauce and tomato sauce))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    Subtle lesson. The authors are trying to have their recursive
;;;    functions match the recursive structure of the data which
;;;    they process.
;;;
;;;    For example, their original definition of REMBER (pg. 38):
;;;    
(defun rember (a lat)
  (cond ((endp lat) '())
        (t (cond ((eql (first lat) a) (rest lat))
                 (t (cons (first lat) (rember a (rest lat)))) ))))

;;;
;;;    Their point (I assume) is that either a list is empty or not.
;;;    This decision is made by the outer COND. Then once we've
;;;    determined that we are dealing with a non-empty list we ask
;;;    the second question: Is the first elt the same as A or not?
;;;
;;;    In fact, on pg. 33 they write: "First we will test (null? lat)...
;;;    Is there any other question we should ask about the lat? No.
;;;    Either a lat is empty or it contains at least one atom. What
;;;    do we do if we know that the lat contains at least one atom?
;;;    We will ask whether /a/ is equal to (car lat)."
;;;
;;;    So each question asked in a function gets its own COND form.
;;;    Apparently the authors didn't want to mix things up by
;;;    using IF here since they change style on pg. 41 to use a
;;;    single COND (see first REMBER above). They would have to switch
;;;    from IF in one place to COND in another rather than just showing
;;;    two forms of COND. (Or maybe Scheme doesn't have IF too?)
;;;
;;;    On pg. 41 the authors summarize how REMBER works and ask if
;;;    this different description of the function's behavior suggests
;;;    a simplified definition.     ??????
;;;
;;;    The authors use a similar pattern with the function member? on
;;;    pg. 22:
;;;
(defun member? (a lat)
  (cond ((endp lat) nil)
        (t (or (eql (first lat) a)
               (member? a (rest lat)))) ))

;;;
;;;    Not sure why they didn't use _the same_ pattern:
;;;    
(defun member? (a lat)
  (cond ((endp lat) nil)
        (t (cond ((eql (first lat) a) t)
                 (t (member? a (rest lat)))) )))

;;;
;;;    But the authors state on pg. 23: "(null? lat) asks if
;;;    /lat/ is the null list...If it is not, then we ask the
;;;    next question...Now that we know that /lat/ is not null?,
;;;    we have to find out whether the car of /lat/ is the same
;;;    atom as /a/, or whether /a/ is somewhere in the rest of
;;;    the /lat/."
;;;
;;;    However, for whatever reason, the first function above in
;;;    ch. 2, LATP?, does not follow this methodology. They just
;;;    jump right in with a COND form containing 3 clauses...
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun firsts (lol)
  (if (endp lol)
      '()
      (cons (first (first lol)) (firsts (rest lol)))) )

;; (defun firsts (lol)
;;   (mapcar #'first lol))

(deftest test-firsts ()
  (check
   (equal (firsts '((apple peach pumpkin)
                    (plum pear cherry)
                    (grape raisin pea)
                    (bean carrot eggplant)))
          '(apple plum grape bean))
   (equal (firsts '((a b) (c d) (e f))) '(A C E))
   (equal (firsts '()) '())
   (equal (firsts '((five plums) (four) (eleven green oranges)))
                  '(five four eleven))))

(defun insert-r (old new lat)
  (cond ((endp lat) '())
        ((eql (first lat) old) (cons (first lat) (cons new (rest lat))))
        (t (cons (first lat) (insert-r old new (rest lat)))) ))

(deftest test-insert-r ()
  (check
   (equal (insert-r 'fudge 'topping '(ice cream with fudge for dessert))
          '(ICE CREAM WITH FUDGE TOPPING FOR DESSERT))
   (equal (insert-r 'and 'jalapeno '(tacos tamales and salsa))
          '(TACOS TAMALES AND JALAPENO SALSA))
   (equal (insert-r 'd 'e '(a b c d f g d h))
          '(A B C D E F G D H))))
