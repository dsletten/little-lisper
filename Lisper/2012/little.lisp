;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp isn't a language, it's a building material.
;;;;   -- Alan Kay
;;;;
;;;;   Name:               little.lisp
;;;;
;;;;   Started:            Tue Apr 24 23:10:06 2012
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
;;;;   Notes: See /Users/dsletten/lisp/Y\ Combinator/DazeY.html for discussion of mutually recursive functions!
;;;;
;;;;
(load "/Users/dsletten/lisp/packages/lang.lisp" :verbose nil)
(load "/Users/dsletten/lisp/packages/test.lisp" :verbose nil)

(defpackage :ch02
  (:use :common-lisp :test)
  (:shadow :member)
  (:export :member))

(in-package :ch02)

(defun latp (l)
  (cond ((endp l) t)
        ((atom (first l)) (latp (rest l)))
        (t nil)))

(deftest test-latp ()
  (check
   (latp '(jack sprat could eat no chicken fat))
   (not (latp '((jack) sprat could eat no chicken fat)))
   (not (latp '(jack (sprat could) eat no chicken fat)))
   (latp '())))

(defun latp-1 (l)
  (every #'atom l))

(deftest test-latp-1 ()
  (check
   (latp-1 '(jack sprat could eat no chicken fat))
   (not (latp-1 '((jack) sprat could eat no chicken fat)))
   (not (latp-1 '(jack (sprat could) eat no chicken fat)))
   (latp-1 '())))

(defun member (obj l)
  (cond ((endp l) nil)
        ((eql (first l) obj) l)
        (t (member obj (rest l)))) )

(deftest test-member ()
  (check
   (member 'tea '(coffee tea or milk))
   (member 'meat '(mashed potatoes and meat gravy))
   (not (member 'poached '(fried eggs and scrambled eggs)))
   (not (member 'liver '(bagels and lox)))) )

(defun member (obj l)
  (cond ((endp l) nil)
        (t (or (eql obj (first l))
               (member obj (rest l)))) ))

(defun member (obj l)
  (if (endp l)
      nil
      (or (eql obj (first l))
          (member obj (rest l)))) )

;;;
;;;    See pg. 123
;;;    
(defun member (obj l)
  (cond ((endp l) nil)
        ((equal (first l) obj) l)
        (t (member obj (rest l)))) )

(defsuite test-ch02 ()
  (test-member) 
  (test-latp-1) 
  (test-latp))

(defpackage :ch03
  (:use :common-lisp :test)
  (:export :firsts)
  (:shadow :subst))

(in-package :ch03)

(defun rember (a lat)
  (cond ((endp lat) '())
        ((eql a (first lat)) (rest lat))
        (t (cons (first lat) (rember a (rest lat)))) ))

(deftest test-rember ()
  (check
   (equal (rember 'mint '(lamb chops and mint jelly)) '(lamb chops and jelly))
   (equal (rember 'mint '(lamb chops and mint flavored mint jelly)) '(lamb chops and flavored mint jelly))
   (equal (rember 'toast '(bacon lettuce and tomato)) '(bacon lettuce and tomato))
   (equal (rember 'bacon '(bacon lettuce and tomato)) '(lettuce and tomato))
   (equal (rember 'and '(bacon lettuce and tomato)) '(bacon lettuce tomato))
   (equal (rember 'cup '(coffee cup tea cup and hick cup)) '(coffee tea cup and hick cup))
   (equal (rember 'sauce '(soy sauce and tomato sauce)) '(soy and tomato sauce))))

(defun rember (a lat)
  (if (endp lat)
      '()
      (if (eql (first lat) a)
          (rest lat)
          (cons (first lat) (rember a (rest lat)))) ))

(defun firsts (l)
  (if (endp l)
      '()
      (cons (first (first l)) (firsts (rest l)))) )

(deftest test-firsts ()
  (check
   (equal (firsts '((apple peach pumpkin) (plum pear cherry) (grape raisin pea) (bean carrot eggplant))) '(apple plum grape bean))
   (equal (firsts '((a b) (c d) (e f))) '(a c e))
   (equal (firsts '((five plums) (four) (eleven green oranges))) '(five four eleven))
   (equal (firsts '()) '())))

(defun firsts (lat)
  (mapcar #'first lat))

(defun insert-r (old new lat)
  (cond ((endp lat) '())
        ((eql (first lat) old) (cons old (cons new (rest lat))))
        (t (cons (first lat) (insert-r old new (rest lat)))) ))

(deftest test-insert-r ()
  (check
   (equal (insert-r 'pickle 'topping '(ice cream with fudge for dessert)) '(ice cream with fudge for dessert))
   (equal (insert-r 'fudge 'topping '(ice cream with fudge for dessert)) '(ice cream with fudge topping for dessert))
   (equal (insert-r 'and 'jalapeno '(tacos tamales and salsa)) '(tacos tamales and jalapeno salsa))
   (equal (insert-r 'd 'e '(a b c d f g d h)) '(a b c d e f g d h))))

(defun insert-l (old new lat)
  (cond ((endp lat) '())
        ((eql (first lat) old) (cons new lat))
        (t (cons (first lat) (insert-l old new (rest lat)))) ))

(deftest test-insert-l ()
  (check
   (equal (insert-l 'pickle 'topping '(ice cream with fudge for dessert)) '(ice cream with fudge for dessert))
   (equal (insert-l 'topping 'fudge '(ice cream with topping for dessert)) '(ice cream with fudge topping for dessert))
   (equal (insert-l 'jalapeno 'and '(tacos tamales jalapeno salsa)) '(tacos tamales and jalapeno salsa))
   (equal (insert-l 'e 'd '(a b c e f g e h)) '(a b c d e f g e h))))

;;;
;;;    Opposite order of OLD/NEW args compared to Common Lisp SUBST.
;;;    
(defun subst (old new lat)
  (cond ((endp lat) '())
        ((eql (first lat) old) (cons new (rest lat)))
        (t (cons (first lat) (subst old new (rest lat)))) ))

(deftest test-subst ()
  (check
   (equal (subst 'pickle 'topping '(ice cream with fudge for dessert)) '(ice cream with fudge for dessert))
   (equal (subst 'fudge 'topping '(ice cream with fudge for dessert)) '(ice cream with topping for dessert))
   (equal (subst 'and 'jalapeno '(tacos tamales and salsa)) '(tacos tamales jalapeno salsa))
   (equal (subst 'd 'e '(a b c d f g d h)) '(a b c e f g d h))))

(defun subst2 (old1 old2 new lat)
  (cond ((endp lat) '())
        ((or (eql (first lat) old1)
             (eql (first lat) old2))
         (cons new (rest lat)))
        (t (cons (first lat) (subst2 old1 old2 new (rest lat)))) ))

(deftest test-subst2 ()
  (check
   (equal (subst2 'pickle 'vinegar 'vanilla '(banana ice cream with chocolate topping))
          '(banana ice cream with chocolate topping))
   (equal (subst2 'chocolate 'banana 'vanilla '(banana ice cream with chocolate topping))
          '(vanilla ice cream with chocolate topping))
   (equal (subst2 'banana 'peach 'vanilla '(chocolate topping on banana ice cream))
          '(chocolate topping on vanilla ice cream))))

;;;
;;;    These LOOP versions process all OLD elts not just the first one. (See ch. 5 "multi" versions)
;;;    
(defun insert-l (old new l)
  (loop for elt in l
        when (eql old elt) collect new
        collect elt))

(defun insert-r (old new l)
  (loop for elt in l
        collect elt
        when (eql old elt) collect new))

(defun subst (old new l)
  (loop for elt in l
        unless (eql elt old) collect elt
        when (eql old elt) collect new))

(defun subst2 (old1 old2 new l)
  (loop for elt in l
        when (or (eql elt old1) (eql elt old2)) collect new
        else collect elt))

;;;
;;;    First
;;;    
(defun insert-r (old new l)
  (labels ((insert (l result)
             (if (endp l)
                 (nreverse result)
                 (destructuring-bind (head . tail) l
                   (if (eql head old)
                       (nconc (nreverse result) (list* old new tail))
                       (insert tail (cons head result)))) )))
    (insert l '())))

;;;
;;;    All
;;;    
(defun insert-r (old new l)
  (labels ((insert (l result)
             (if (endp l)
                 (nreverse result)
                 (destructuring-bind (head . tail) l
                   (if (eql head old)
                       (insert tail (list* new head result))
                       (insert tail (cons head result)))) )))
    (insert l '())))

;;;
;;;    First
;;;    
(defun insert-l (old new l)
  (labels ((insert (l result)
             (if (endp l)
                 (nreverse result)
                 (destructuring-bind (head . tail) l
                   (if (eql head old)
                       (nconc (nreverse result) (list* new old tail))
                       (insert tail (cons head result)))) )))
    (insert l '())))

;;;
;;;    All
;;;    
(defun insert-l (old new l)
  (labels ((insert (l result)
             (if (endp l)
                 (nreverse result)
                 (destructuring-bind (head . tail) l
                   (if (eql head old)
                       (insert tail (list* head new result))
                       (insert tail (cons head result)))) )))
    (insert l '())))

;;;
;;;    First
;;;    
(defun subst (old new l)
  (labels ((insert (l result)
             (if (endp l)
                 (nreverse result)
                 (destructuring-bind (head . tail) l
                   (if (eql head old)
                       (nconc (nreverse result) (cons new tail))
                       (insert tail (cons head result)))) )))
    (insert l '())))

;;;
;;;    All
;;;    
(defun subst (old new l)
  (labels ((insert (l result)
             (if (endp l)
                 (nreverse result)
                 (destructuring-bind (head . tail) l
                   (if (eql head old)
                       (insert tail (cons new result))
                       (insert tail (cons head result)))) )))
    (insert l '())))

;;;
;;;    First
;;;    
(defun subst2 (old1 old2 new l)
  (labels ((insert (l result)
             (if (endp l)
                 (nreverse result)
                 (destructuring-bind (head . tail) l
                   (if (or (eql head old1) (eql head old2))
                       (nconc (nreverse result) (cons new tail))
                       (insert tail (cons head result)))) )))
    (insert l '())))

;;;
;;;    All
;;;    
(defun subst2 (old1 old2 new l)
  (labels ((insert (l result)
             (if (endp l)
                 (nreverse result)
                 (destructuring-bind (head . tail) l
                   (if (or (eql head old1) (eql head old2))
                       (insert tail (cons new result))
                       (insert tail (cons head result)))) )))
    (insert l '())))

(defun insert-r (old new l)
  (insert old old new l '()))

(defun insert-l (old new l)
  (insert old new old l '()))

(defun insert (old first second l result)
  (if (endp l)
      (nreverse result)
      (destructuring-bind (head . tail) l
        (if (eql head old)
            (insert old first second tail (list* second first result))
            (insert old first second tail (cons head result)))) ))

(defun insert-r (old new l)
  (insert old #'(lambda (result) (list* new old result)) l '()))

(defun insert-l (old new l)
  (insert old #'(lambda (result) (list* old new result)) l '()))

(defun subst (old new l)
  (insert old #'(lambda (result) (cons new result)) l '()))

(defun insert (old next-result l result)
  (if (endp l)
      (nreverse result)
      (destructuring-bind (head . tail) l
        (if (eql head old)
            (insert old next-result tail (funcall next-result result))
            (insert old next-result tail (cons head result)))) ))

(defun insert-r (old new l)
  (insert #'(lambda (elt) (eql elt old))
          #'(lambda (result) (list* new old result))
          l
          '()))

(defun insert-l (old new l)
  (insert #'(lambda (elt) (eql elt old))
          #'(lambda (result) (list* old new result))
          l
          '()))

(defun subst (old new l)
  (insert #'(lambda (elt) (eql elt old))
          #'(lambda (result) (cons new result))
          l
          '()))

(defun subst2 (old1 old2 new l)
  (insert #'(lambda (elt) (or (eql elt old1)
                              (eql elt old2)))
          #'(lambda (result) (cons new result))
          l
          '()))

(defun insert (matchp next-result l result)
  (if (endp l)
      (nreverse result)
      (destructuring-bind (head . tail) l
        (if (funcall matchp head)
            (insert matchp next-result tail (funcall next-result result))
            (insert matchp next-result tail (cons head result)))) ))

;;;
;;;    Stop after first match.
;;;    
(defun insert (matchp next-result l result)
  (if (endp l)
      (nreverse result)
      (destructuring-bind (head . tail) l
        (if (funcall matchp head)
            (nreverse (revappend tail (funcall next-result result)))
            (insert matchp next-result tail (cons head result)))) ))

;;;
;;;    Tail-recursive function with single termination case is relatively straightforward translation to DO.
;;;    However, multiple terminations complicates the DO translation. Furthermore, DO lacks the destructuring
;;;    capabilities of Clojure's 'loop'.
;;;    
(defun insert-r (old new lat)
  (labels ((insert (lat result)
             (cond ((endp lat) (nreverse result))
                   ((eql (first lat) old) (nreverse (revappend (rest lat) (cons new (cons old result)))) )
                   (t (insert (rest lat) (cons (first lat) result)))) ))
    (insert lat '())))

(defun insert-r (old new lat)
  (labels ((insert (lat result)
             (if (endp lat)
                 (nreverse result)
                 (destructuring-bind (head . tail) lat
                   (if (eql head old)
                       (nreverse (revappend tail (cons new (cons old result))))
                       (insert tail (cons head result)))) )))
    (insert lat '())))

;;
;;    This destructuring works
;;    
(defun insert-r (old new lat)
  (labels ((insert (lat result)
             (destructuring-bind (&optional head &rest tail) lat
               (cond ((endp lat) (nreverse result))
                     ((eql head old) (nreverse (revappend tail (cons new (cons old result)))) )
                     (t (insert tail (cons head result)))) )))
    (insert lat '())))

(defun insert-r (old new lat)
  (do ((l lat (rest l))
       (result '() (cons (first l) result)))
      ((if (endp l)
           (return (nreverse result))
           (eql (first l) old))
       (nreverse (revappend (rest l) (list* new old result)))) ))

(defun insert-r (old new lat)
  (do ((l lat (rest l))
       (result '() (cons (first l) result)))
      ((endp l) (nreverse result))
    (when (eql (first l) old)
      (return (nreverse (revappend (rest l) (list* new old result)))) )))

;; (defun insert-r (old new l)
;;   (loop for elt in l
;;         collect elt into result
;;         when (eql old elt) (return (append

(defsuite test-ch03 ()
  (test-insert-r) 
  (test-insert-l) 
  (test-firsts) 
  (test-subst2) 
  (test-subst) 
  (test-rember))

(defpackage :ch04
  (:use :common-lisp :test)
  (:shadow :+ :- :* :> :< := :length)
  (:export :eqan :+ :* :^ :- :< :> :=))

(in-package :ch04)

(defun + (m n)
  (if (zerop n)
      m
      (+ (1+ m) (1- n))))

;;
;;    The book's version (Not tail-recursive!)
;;    
(defun + (m n)
  (if (zerop n)
      m
      (1+ (+ m (1- n)))) )

(deftest test+ ()
  (check
   (cl:= (+ 46 12) (cl:+ 46 12))))

(defun - (m n)
  (if (zerop n)
      m
      (- (1- m) (1- n))))

;;
;;    Book version
;;    
(defun - (m n)
  (if (zerop n)
      m
      (1- (- m (1- n)))) )

(deftest test- ()
  (check
   (cl:= (- 8 3) (cl:- 8 3))
   (cl:= (- 17 9) (cl:- 17 9))))

(defun addvec (vec)
  (if (endp vec)
      0
      (+ (first vec) (addvec (rest vec)))) )

(deftest test-addvec ()
  (check
   (let ((vec '(3 5 2 8)))
     (cl:= (addvec vec) (reduce #'cl:+ vec)))
   (let ((vec '(15 6 7 12 3)))
     (cl:= (addvec vec) (reduce #'cl:+ vec)))) )

(defun * (m n)
  (if (zerop n)
      0
      (+ (* m (1- n)) m)))

(deftest test* ()
  (check
   (cl:= (* 5 3) (cl:* 5 3))
   (cl:= (* 13 4) (cl:* 13 4))
   (cl:= (* 12 3) (cl:* 12 3))))

(defun vec+ (v1 v2)
  (cond ((endp v1) (if (endp v2) '() (error "Length mismatch")))
        ((endp v2) (error "Length mismatch"))
        (t (cons (+ (first v1) (first v2))
                 (vec+ (rest v1) (rest v2)))) ))

(defun vec+ (v1 v2)
  (cond ((and (endp v1) (endp v2)) '())
        ((or (endp v1) (endp v2)) (error "Length mismatch"))
        (t (cons (+ (first v1) (first v2))
                 (vec+ (rest v1) (rest v2)))) ))

(deftest test-vec+ ()
  (check
   (let ((v1 '(3 6 9 11 4))
         (v2 '(8 5 2 0 7)))
     (equal (vec+ v1 v2) (mapcar #'+ v1 v2)))
   (let ((v1 '(3 7))
         (v2 '(4 6)))
     (equal (vec+ v1 v2) (mapcar #'+ v1 v2)))
   (let ((v1 '(2 3))
         (v2 '(4 6)))
     (equal (vec+ v1 v2) (mapcar #'+ v1 v2)))) )

(defun vec+ (v1 v2)
  (cond ((endp v1) v2)
        ((endp v2) v1)
        (t (cons (+ (first v1) (first v2))
                 (vec+ (rest v1) (rest v2)))) ))

(deftest test-vec+ ()
  (check
   (let ((v1 '(3 6 9 11 4))
         (v2 '(8 5 2 0 7)))
     (equal (vec+ v1 v2) (mapcar #'+ v1 v2)))
   (let ((v1 '(3 7))
         (v2 '(4 6)))
     (equal (vec+ v1 v2) (mapcar #'+ v1 v2)))
   (let ((v1 '(2 3))
         (v2 '(4 6)))
     (equal (vec+ v1 v2) (mapcar #'+ v1 v2)))
   (let ((v1 '(3 7 8 1))
         (v2 '(4 6)))
     (equal (vec+ v1 v2) '(7 13 8 1)))
   (let ((v1 '(3 7 8 1))
         (v2 '(4 6)))
     (equal (vec+ v2 v1) '(7 13 8 1)))) )

(defun > (m n)
  (cond ((zerop n) (not (zerop m)))
        ((zerop m) nil)
        (t (> (1- m) (1- n)))) )

;;;
;;;    Book's version--better than mine!
;;;    
(defun > (m n)
  (cond ((zerop m) nil)
        ((zerop n) t)
        (t (> (1- m) (1- n)))) )

(deftest test> ()
  (check
   (> 12 11)
   (not (> 12 12))
   (not (> 12 13))))

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
  (cond ((and (zerop m) (zerop n)) t)
        ((or (zerop m) (zerop n)) nil)
        (t (= (1- m) (1- n)))) )

(defun = (m n)
  (cond ((zerop m) (zerop n))
        ((zerop n) nil)
        (t (= (1- m) (1- n)))) )

(deftest test= ()
  (check
   (= 3 3)
   (not (= 3 4))
   (not (= 3 2))))

(defun = (m n)
  (cond ((< m n) nil)
        ((> m n) nil)
        (t t)))

;;;
;;;    Exponentiation
;;;    
(defun ^ (m n)
  (if (zerop n)
      1
      (* m (^ m (1- n)))) )

(deftest test^ ()
  (check
   (cl:= (^ 1 1) 1)
   (cl:= (^ 2 3) 8)
   (cl:= (^ 5 3) 125)))

(defun length (l)
  (if (endp l)
      0
      (1+ (length (rest l)))) )

(defun length (l)
  (loop for elt in l
        counting elt))

(deftest test-length ()
  (check
   (let ((l '()))
     (cl:= (length l) (cl:length l)))
   (let ((l '(hotdogs with mustard sauerkraut and pickles)))
     (cl:= (length l) (cl:length l)))
   (let ((l '(ham and cheese on rye)))
     (cl:= (length l) (cl:length l)))) )

;;;
;;;    NTH (1-based index)
;;;
(defun pick (n l)
  (if (cl:= n 1)
      (first l)
      (pick (1- n) (rest l))))

;;
;;    Similar to book's version.
;;    
(defun pick (n l)
  (cond ((endp l) nil)
        ((zerop (1- n)) (first l)) ; ?! (zerop (1- n)) => (= n 1)
        (t (pick (1- n) (rest l)))) )

;;
;;    Only differs from above version since the book says (car '()) is undefined.
;;    This version: (pick 1 '()) => (first '()) => NIL
;;    
(defun pick (n l)
  (cond ((zerop (1- n)) (first l))
        ((endp l) nil)
        (t (pick (1- n) (rest l)))) )

(deftest test-pick ()
  (check
   (equal (pick 4 '(lasagna spaghetti ravioli macaroni meatball)) 'macaroni)
   (equal (pick 6 '(lasagna spaghetti ravioli macaroni meatball)) nil)
   (equal (pick 8 '(lasagna spaghetti ravioli macaroni meatball)) nil)
   (equal (pick 0 '()) nil)))

(defun rempick (n l)
  (cond ((endp l) '())
        ((cl:= n 1) (rest l))
        (t (cons (first l) (rempick (1- n) (rest l)))) ))

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

;; (defun eqan? (a1 a2)
;;   (cond ((and (atom a1) (atom a2))
;;          (cond ((and (numberp a1) (numberp a2)) (cl:= a1 a2))
;;                ((or (numberp a1) (numberp a2)) nil)
;;                (t (eq a1 a2))))
;;         ((or (atom a1) (atom a2)) nil)
;;         (t (equal a1 a2))))

(defun eqan (a1 a2)
  (cond ((and (numberp a1) (numberp a2)) (cl:= a1 a2))
        ((or (numberp a1) (numberp a2)) nil)
        (t (eq a1 a2))))

(deftest test-eqan ()
  (check
   (eqan 8 8)
   (eqan 'foo 'foo)
   (not (eqan 8 9))
   (not (eqan 'pung 'foo))
   (not (eqan 8 'pung))))

(defsuite test-ch04 ()
  (test-addvec) 
  (test-all-nums) 
  (test>) 
  (test<) 
  (test=) 
  (test-pick) 
  (test-no-nums) 
  (test*) 
  (test+) 
  (test-eqan) 
  (test-) 
  (test-vec+) 
  (test-rempick) 
  (test^) 
  (test-length))

(defpackage :ch05 (:use :common-lisp :test) (:export :multi-rember))

(in-package :ch05)

(defun multi-rember (obj l)
  (cond ((endp l) '())
        ((eql (first l) obj) (multi-rember obj (rest l)))
        (t (cons (first l) (multi-rember obj (rest l)))) ))

(deftest test-multi-rember ()
  (check
   (equal (multi-rember 'cup '(coffee cup tea cup and hick cup)) '(coffee tea and hick))
   (equal (multi-rember 'mint '(lamb chops and mint jelly)) '(lamb chops and jelly))
   (equal (multi-rember 'mint '(lamb chops and mint flavored mint jelly)) '(lamb chops and flavored jelly))
   (equal (multi-rember 'toast '(bacon lettuce and tomato)) '(bacon lettuce and tomato))
   (equal (multi-rember 'bacon '(bacon lettuce and tomato)) '(lettuce and tomato))
   (equal (multi-rember 'and '(bacon lettuce and tomato)) '(bacon lettuce tomato))
   (equal (multi-rember 'sauce '(soy sauce and tomato sauce)) '(soy and tomato))))

(defun multi-insert-r (old new lat)
  (cond ((endp lat) '())
        ((eql (first lat) old) (cons old (cons new (multi-insert-r old new (rest lat)))) )
        (t (cons (first lat) (multi-insert-r old new (rest lat)))) ))

(deftest test-multi-insert-r ()
  (check
   (equal (multi-insert-r 'pickle 'topping '(ice cream with fudge for dessert)) '(ice cream with fudge for dessert))
   (equal (multi-insert-r 'fudge 'topping '(ice cream with fudge for dessert)) '(ice cream with fudge topping for dessert))
   (equal (multi-insert-r 'and 'jalapeno '(tacos tamales and salsa)) '(tacos tamales and jalapeno salsa))
   (equal (multi-insert-r 'd 'e '(a b c d f g d h)) '(a b c d e f g d e h))))

(defun multi-insert-l (old new lat)
  (cond ((endp lat) '())
        ((eql (first lat) old) (cons new (cons old (multi-insert-l old new (rest lat)))) )
        (t (cons (first lat) (multi-insert-l old new (rest lat)))) ))

(deftest test-multi-insert-l ()
  (check
   (equal (multi-insert-l 'pickle 'topping '(ice cream with fudge for dessert)) '(ice cream with fudge for dessert))
   (equal (multi-insert-l 'topping 'fudge '(ice cream with topping for dessert)) '(ice cream with fudge topping for dessert))
   (equal (multi-insert-l 'jalapeno 'and '(tacos tamales jalapeno salsa)) '(tacos tamales and jalapeno salsa))
   (equal (multi-insert-l 'e 'd '(a b c e f g e h)) '(a b c d e f g d e h))))

(defun multi-subst (old new lat)
  (cond ((endp lat) '())
        ((eql (first lat) old) (cons new (multi-subst old new (rest lat))))
        (t (cons (first lat) (multi-subst old new (rest lat)))) ))

(deftest test-multi-subst ()
  (check
   (equal (multi-subst 'pickle 'topping '(ice cream with fudge for dessert)) '(ice cream with fudge for dessert))
   (equal (multi-subst 'fudge 'topping '(ice cream with fudge for dessert)) '(ice cream with topping for dessert))
   (equal (multi-subst 'and 'jalapeno '(tacos tamales and salsa)) '(tacos tamales jalapeno salsa))
   (equal (multi-subst 'd 'e '(a b c d f g d h)) '(a b c e f g e h))))

(defun multi-subst2 (old1 old2 new lat)
  (destructuring-bind (&optional head &rest tail) lat
    (cond ((endp lat) '())
          ((or (eql head old1) (eql head old2)) (cons new (multi-subst2 old1 old2 new tail)))
          (t (cons head (multi-subst2 old1 old2 new tail)))) ))

(deftest test-multi-subst2 ()
  (check
   (equal (multi-subst2 'pickle 'vinegar 'vanilla '(banana ice cream with chocolate topping))
          '(banana ice cream with chocolate topping))
   (equal (multi-subst2 'chocolate 'banana 'vanilla '(banana ice cream with chocolate topping))
          '(vanilla ice cream with vanilla topping))
   (equal (multi-subst2 'banana 'peach 'vanilla '(chocolate topping on banana ice cream))
          '(chocolate topping on vanilla ice cream))))

;;
;;    Count occurrences.
;;    
(defun occur (a lat)
  (cond ((endp lat) 0)
        ((eql (first lat) a) (1+ (occur a (rest lat))))
        (t (occur a (rest lat)))) )

(deftest test-occur ()
  (check
   (let ((lat '(a b b a c a d)))
     (= (occur 'a lat) (count 'a lat)))
   (let ((lat '(a b b a c a d)))
     (= (occur 'x lat) (count 'x lat)))
   (let ((lat '(a b b a c a d)))
     (= (occur 'd lat) (count 'd lat)))
   (let ((lat '(a a a a a a)))
     (= (occur 'a lat) (count 'a lat)))) )

(defun onep (n)
  (= n 1))

(defun rempick (n lat)
  (cond ((endp lat) '())
        ((onep n) (rest lat))
        (t (cons (first lat) (rempick (1- n) (rest lat)))) ))

(deftest test-rempick ()
  (check
   (equal (rempick 3 '(lemon meringue salty pie)) '(lemon meringue pie))))

(defsuite test-ch05 ()
  (test-multi-insert-r)
  (test-occur)
  (test-multi-subst2)
  (test-multi-subst)
  (test-multi-insert-l)
  (test-rempick)
  (test-multi-rember))

(defpackage :ch06
  (:use :common-lisp :lang :test)
  (:import-from :ch04 :eqan)
;  (:shadowing-import-from :cl :+ :- :< :> :=)
  (:shadow :split :equal))

(in-package :ch06)

(defun leftmost (l)
  (cond ((endp l) '())
        ((atom (first l)) (first l))
        (t (leftmost (first l)))) )

(defun leftmost (l)
  (cond ((endp l) '())
        ((consp (first l)) (leftmost (first l)))
        (t (first l))))

(defun leftmost (l)
  (if (atom l)
      l
      (leftmost (first l))))

(deftest test-leftmost ()
  (check
   (eql (leftmost '((hot) (tuna (and)) cheese)) 'hot)
   (eql (leftmost '(((hamburger) french) (fries (and a) coke))) 'hamburger)
   (eql (leftmost '((((4) four)) 17 (seventeen))) 4)
   (eql (leftmost '(((() four)) 17 (seventeen))) nil)))

(defun rember* (a l)
  (cond ((endp l) '())
        ((eql a (first l)) (rember* a (rest l)))
        ((atom (first l)) (cons (first l) (rember* a (rest l))))
        (t (cons (rember* a (first l))
                 (rember* a (rest l)))) ))

(defun rember* (a l)
  (cond ((endp l) '())
        ((consp (first l)) (cons (rember* a (first l))
                                 (rember* a (rest l))))
        ((eql a (first l)) (rember* a (rest l)))
        (t (cons (first l) (rember* a (rest l)))) ))

(deftest test-rember* ()
  (check
   (cl:equal (rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup)) '((coffee) ((tea)) (and (hick))))
   (cl:equal (rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce))) '(((tomato)) ((bean)) (and ((flying)))) )))

(defun insert-r* (old new l)
  (cond ((endp l) '())
        ((eql (first l) old) (cons old (cons new (insert-r* old new (rest l)))) )
        ((atom (first l)) (cons (first l) (insert-r* old new (rest l))))
        (t (cons (insert-r* old new (first l))
                 (insert-r* old new (rest l)))) ))

(defun insert-r* (old new l)
  (cond ((endp l) '())
        ((eql (first l) old) (list* old new (insert-r* old new (rest l))))
        ((atom (first l)) (cons (first l) (insert-r* old new (rest l))))
        (t (cons (insert-r* old new (first l))
                 (insert-r* old new (rest l)))) ))

(defun insert-r* (old new l)
  (destructuring-bind (&optional head &rest tail) l
    (cond ((endp l) '())
          ((eql head old) (list* old new (insert-r* old new tail)))
          ((atom head) (cons head (insert-r* old new tail)))
          (t (cons (insert-r* old new head)
                   (insert-r* old new tail)))) ))

;;
;;    Hybrid
;;    
(defun insert-r* (old new l)
  (loop for elt in l
        when (atom elt) collect elt
        when (eql elt old) collect new
        when (consp elt) collect (insert-r* old new elt)))

(deftest test-insert-r* ()
  (check
   (cl:equal (insert-r* 'chuck 'roast '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))
             '((how much (wood)) could ((a (wood) chuck roast)) (((chuck roast))) (if (a) ((wood chuck roast))) could chuck roast wood))))

;;;    See LOOP and INSERT versions above

(defun occur* (a l)
  (cond ((endp l) 0)
        ((eql (first l) a) (1+ (occur* a (rest l))))
        ((atom (first l)) (occur* a (rest l)))
        (t (+ (occur* a (first l))
              (occur* a (rest l)))) ))

(deftest test-occur* ()
  (check (= (occur* 'banana '((banana) (split ((((banana ice))) (cream (banana)) sherbet)) (banana) (bread) (banana brandy)))) ))

(defun subst* (old new l)
  (cond ((endp l) '())
        ((eql (first l) old) (cons new (subst* old new (rest l))))
        ((atom (first l)) (cons (first l) (subst* old new (rest l))))
        (t (cons (subst* old new (first l))
                 (subst* old new (rest l)))) ))

;;
;;    Hybrid
;;    
(defun subst* (old new l)
  (loop for elt in l
        unless (or (consp elt) (eql elt old)) collect elt
        when (eql elt old) collect new
        when (consp elt) collect (subst* old new elt)))

(deftest test-subst* ()
  (check
   (cl:equal (subst* 'banana 'orange '((banana) (split ((((banana ice))) (cream (banana)) sherbet)) (banana) (bread) (banana brandy)))
             '((orange) (split ((((orange ice))) (cream (orange)) sherbet)) (orange) (bread) (orange brandy)))) )

(defun insert-l* (old new l)
  (cond ((endp l) '())
        ((eql (first l) old) (list* new old (insert-l* old new (rest l))))
        ((atom (first l)) (cons (first l) (insert-l* old new (rest l))))
        (t (cons (insert-l* old new (first l))
                 (insert-l* old new (rest l)))) ))

(deftest test-insert-l* ()
  (check
   (cl:equal (insert-l* 'chuck 'pecker '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))
             '((how much (wood)) could ((a (wood) pecker chuck)) (((pecker chuck))) (if (a) ((wood pecker chuck))) could pecker chuck wood))))

(defun member* (a l)
  (cond ((endp l) nil)
        ((eql (first l) a) t)
        ((atom (first l)) (member* a (rest l)))
        (t (cond ((member* a (first l)) t)
                 (t (member* a (rest l)))) )))

(defun member* (a l)
  (cond ((endp l) nil)
        ((eql (first l) a) t)
        ((atom (first l)) (member* a (rest l)))
        (t (or (member* a (first l))
               (member* a (rest l)))) ))

(deftest test-member* ()
  (check
   (member* 'chips '((potato) (chips ((with) fish) (chips)))) ))

(defun eqlat (lat1 lat2)
  (cond ((and (endp lat1) (endp lat2)) t)
        ((or (endp lat1) (endp lat2)) nil)
        ((eqan (first lat1) (first lat2)) (eqlat (rest lat1) (rest lat2)))
        (t nil)))

(defun eqlat (lat1 lat2)
  (do ((l1 lat1 (rest l1))
       (l2 lat2 (rest l2)))
      ((or (endp l1) (endp l2)) (and (endp l1) (endp l2)))
    (unless (eqan (first l1) (first l2))
      (return nil))))

(deftest test-eqlat ()
  (check
   (eqlat '(strawberry ice cream) '(strawberry ice cream))
   (not (eqlat '(strawberry ice) '(strawberry ice cream)))
   (not (eqlat '(strawberry ice cream) '(strawberry ice)))
   (not (eqlat '(strawberry ice cream) '(strawberry cream ice)))) )
   
(defun eqlist (l1 l2)
  (cond ((and (endp l1) (endp l2)) t)
        ((or (endp l1) (endp l2)) nil)
        ((and (atom (first l1)) (atom (first l2)))
         (and (eqan (first l1) (first l2))
              (eqlist (rest l1) (rest l2))))
        ((or (atom (first l1)) (atom (first l2))) nil)
        (t (and (eqlist (first l1) (first l2))
                (eqlist (rest l1) (rest l2)))) ))

(defun eqlist (l1 l2)
  (destructure ((&optional h1 &rest t1) l1
                (&optional h2 &rest t2) l2)
    (cond ((and (endp l1) (endp l2)) t)
          ((or (endp l1) (endp l2)) nil)
          ((and (atom h1) (atom h2))
           (and (eqan h1 h2) (eqlist t1 t2)))
          ((or (atom h1) (atom h2)) nil)
          (t (and (eqlist h1 h2) (eqlist t1 t2)))) ))
;;
;;    Hybrid
;;    
;; (defun eqlist (l1 l2)
;;   (do ((l1 l1 (rest l1))
;;        (l2 l2 (rest l2)))
;;       ((or (endp l1) (endp l2)) (and (endp l1) (endp l2)))
;;     (if (or (atom (first l1)) (atom (first l2)))
;;         (unless (and (atom (first l1))
;;                      (atom (first l2))
;;                      (eqan (first l1) (first l2)))
;;           (return nil))
;;         (unless (eqlist (first l1) (first l2))
;;           (return nil)))) )

(deftest test-eqlist ()
  (check
   (eqlist '(strawberry ice cream) '(strawberry ice cream))
   (not (eqlist '(strawberry ice cream) '(strawberry cream ice)))
   (not (eqlist '(strawberry ice cream) '(strawberry ice)))
   (not (eqlist '(strawberry ice) '(strawberry ice cream)))
   (not (eqlist '(banana ((split))) '((banana) (split))))
   (not (eqlist '(beef ((sausage)) (and (soda))) '(beef ((salami)) (and (soda)))) )
   (eqlist '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda)))) ))

(defun equal (o1 o2)
  (cond ((and (atom o1) (atom o2)) (eqan o1 o2))
        ((or (atom o1) (atom o2)) nil)
        (t (and (equal (first o1) (first o2))
                (equal (rest o1) (rest o2)))) ))

;;;
;;;    Book's version -- nice
;;;    -Mutually recursive
;;;
(defun equal (o1 o2)
  (cond ((and (atom o1) (atom o2)) (eqan o1 o2))
        ((and (consp o1) (consp o2)) (eqlist o1 o2))
        (t nil)))

;;;
;;;    This assumes two proper lists, i.e., REST is always a list.
;;;    
(defun eqlist (l1 l2)
  (cond ((endp l1) (endp l2))
        ((endp l2) nil)
        (t (and (equal (first l1) (first l2))
                (eqlist (rest l1) (rest l2)))) ))

;;;
;;;    This handles dotted list args.
;;;    
(defun eqlist (l1 l2)
  (cond ((endp l1) (endp l2))
        ((endp l2) nil)
        (t (and (equal (first l1) (first l2))
                (equal (rest l1) (rest l2)))) ))

(deftest test-equal ()
  (check
   (equal 'a 'a)
   (not (equal 'a 'b))
   (not (equal 'a 1))
   (equal 1 1)
   (not (equal 1 2))
   (equal '(a b c) '(a b c))
   (not (equal '(a b c) '(a b c d)))
   (equal '(a (b c)) '(a (b c)))) )

;;;
;;;    This simply removes the first instance of an arbitrary (i.e., possibly non-atom) sequence element.
;;;    
(defun rember (obj l)
  (cond ((endp l) '())
        ((equal obj (first l)) (rest l))
        (t (cons (first l) (rember obj (rest l)))) ))

(deftest test-rember ()
  (check
   (cl:equal (rember '(a b) '(c d (a b) e)) '(c d e))
   (cl:equal (rember '(x y) '(c d (a b) e)) '(c d (a b) e))))

(defsuite test-ch06 ()
  (test-leftmost)
  (test-rember*)
  (test-occur*)
  (test-insert-r*)
  (test-insert-l*)
  (test-subst*)
  (test-member*)
  (test-eqlat)
  (test-eqlist)
  (test-equal)
  (test-rember))

(defpackage :ch07
  (:use :common-lisp :test); :ch04)
  (:shadow :1+ :1- :zerop :numberp))
;  (:shadowing-import-from :ch04 :+ :* :- :< :> := :^))  

(in-package :ch07)

;;;
;;;    Assumes EXPR is a legal (representation of an) arithmetic expression.
;;;    
(defun numberedp (expr)
  (cond ((atom expr) (numberp expr))
        (t (and (numberedp (first expr))
                (operatorp (second expr))
                (numberedp (third expr)))) ))

(defun operatorp (expr)
  (case expr
    ((+ * ^) t)
    (otherwise nil)))

(deftest test-numberedp ()
  (check
   (numberedp '(3 + (4 ^ 5)))
   (not (numberedp '(2 * sausage)))) )

(defun value (expr)
  (cond ((numberp expr) expr)
        ((numberedp expr) (funcall (second expr)
                                   (value (first expr))
                                   (value (third expr)))) ))

(defun value (expr)
  (cond ((numberp expr) expr)
        ((numberedp expr) (destructuring-bind (op1 operator op2) expr
                            (funcall operator (value op1) (value op2)))) ))

(deftest test-value ()
  (check
   (cl:= (value '(8 + (9 * 4))) 44)
   (cl:= (value '(3 + (2 ^ 5))) 35)))

(defun value (expr)
  (cond ((numberp expr) expr)
        (t (destructuring-bind (operator op1 op2) expr
             (funcall (operator-function operator) (value op1) (value op2)))) ))

(defun operator-function (operator)
  (case operator
    (plus #'+)
    (times #'*)
    (expt #'expt)))

(deftest test-value ()
  (check
   (cl:= (value '(plus 8 (times 9 4))) 44)
   (cl:= (value '(plus 3 (expt 2 5))) 35)))

(defpackage :ch07a
  (:use :common-lisp :test); :ch04)
  (:shadow :+ :- :* :^ := :> :< :1+ :1- :zerop :numberp))
;  (:shadowing-import-from :ch04 :+ :* :- :< :> := :^))  

(in-package :ch07a)

(defun zerop (n)
  (null n))

(defun 1+ (n)
  (list n))

(defun 1- (n)
  (first n))

(defun 1- (n)
  (destructuring-bind (m) n
    m))

(defun numberp (n)
  (or (null n)
      (numberp (first n))))

(defun numberp (n)
  (or (zerop n)
      (numberp (1- n))))

;;;
;;;    See ch. 4
;;;    
(defun + (m n)
  (if (zerop n)
      m
      (+ (1+ m) (1- n))))

(defun - (m n)
  (if (zerop n)
      m
      (- (1- m) (1- n))))

(defun * (m n)
  (if (zerop n)
      '()
      (+ (* m (1- n)) m)))

(defun ^ (m n)
  (if (zerop n)
      '(())
      (* m (^ m (1- n)))) )

(defun > (m n)
  (cond ((zerop m) nil)
        ((zerop n) t)
        (t (> (1- m) (1- n)))) )

(defun < (m n)
  (cond ((zerop n) nil)
        ((zerop m) t)
        (t (< (1- m) (1- n)))) )

(defun = (m n)
  (not (or (< m n) (> m n))))

(defun represent-number (n)
  (cond ((cl:zerop n) '())
        (t (list (represent-number (cl:1- n)))) ))

(deftest test-numberp ()
  (check
   (numberp '())
   (numberp '(()))
   (numberp (+ '(()) '((()))))
   (not (numberp 'a))))

(deftest test+ ()
  (check
   (= (+ '((((((((((((((((((((((((((((((((((((((((((((((NIL))))))))))))))))))))))))))))))))))))))))))))))
         '((((((((((((NIL)))))))))))))
      '((((((((((((((((((((((((((((((((((((((((((((((((((((((((((NIL)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(deftest test- ()
  (check
   (= (- '((((((((())))))))) '(((())))) '(((((()))))))
   (= (- '(((((((((((((((((NIL))))))))))))))))) '(((((((((NIL)))))))))) '((((((((NIL)))))))))))

(deftest test* ()
  (check
   (= (* '(((((NIL))))) '(((NIL)))) (+ (+ '(((((NIL))))) '(((((NIL)))))) '(((((NIL)))))))
   (= (* '(((((((((((((NIL))))))))))))) '((((NIL))))) (+ (+ (+ '(((((((((((((NIL))))))))))))) '(((((((((((((NIL)))))))))))))) '(((((((((((((NIL)))))))))))))) '(((((((((((((NIL)))))))))))))))
   (= (* '((((((((((((NIL)))))))))))) '(((NIL)))) (+ (+ '((((((((((((NIL)))))))))))) '((((((((((((NIL))))))))))))) '((((((((((((NIL)))))))))))))) ))

(deftest test> ()
  (check
   (> '((((((((((((NIL)))))))))))) '(((((((((((NIL))))))))))))
   (not (> '((((((((((((NIL)))))))))))) '((((((((((((NIL))))))))))))))
   (not (> '((((((((((((NIL)))))))))))) '(((((((((((((NIL)))))))))))))))))

(deftest test< ()
  (check
   (< '((((NIL)))) '((((((NIL)))))))
   (not (< '((((((((NIL)))))))) '(((NIL)))))
   (not (< '((((((NIL)))))) '((((((NIL))))))))))

(deftest test= ()
  (check
   (= '(((NIL))) '(((NIL))))
   (not (= '(((NIL))) '((((NIL))))))
   (not (= '(((NIL))) '((NIL))))))

(deftest test^ ()
  (check
   (= (^ '(NIL) '(NIL)) '(NIL))
   (= (^ '((NIL)) '(((NIL)))) (* (* '((NIL)) '((NIL))) '((NIL))))
   (= (^ '(((((NIL))))) '(((NIL)))) (* (* '(((((NIL))))) '(((((NIL)))))) '(((((NIL)))))))))

;;;
;;;    Whoops! I didn't use the book's representation...
;;;    
(defpackage :ch07b
  (:use :common-lisp :test)
  (:shadow :+ :- :* :^ := :> :< :1+ :1- :zerop :numberp))

(in-package :ch07b)

(defun zerop (n)
  (null n))

(defun 1+ (n)
  (cons '() n))

;;;
;;;    Assumes that N is positive.
;;;    
(defun 1- (n)
  (rest n))

(defun numberp (n)
  (or (null n)
      (and (null (first n))
           (numberp (rest n)))) )

(defun numberp (n)
  (or (zerop n)
      (numberp (1- n))))

(deftest test-numberp ()
  (check
   (numberp '())
   (numberp '(()))
   (numberp (+ '(()) '((()))))
   (not (numberp 'a))))

;;;
;;;    Same definitions as above...
;;;    
(defun + (m n)
  (if (zerop n)
      m
      (+ (1+ m) (1- n))))

(defun - (m n)
  (if (zerop n)
      m
      (- (1- m) (1- n))))

(defun * (m n)
  (if (zerop n)
      '()
      (+ (* m (1- n)) m)))

(defun ^ (m n)
  (if (zerop n)
      '(())
      (* m (^ m (1- n)))) )

(defun > (m n)
  (cond ((zerop m) nil)
        ((zerop n) t)
        (t (> (1- m) (1- n)))) )

(defun < (m n)
  (cond ((zerop n) nil)
        ((zerop m) t)
        (t (< (1- m) (1- n)))) )

(defun = (m n)
  (not (or (< m n) (> m n))))

(defun represent-number (n)
  (cond ((cl:zerop n) '())
        (t (cons '() (represent-number (cl:1- n)))) ))

(deftest test+ ()
  (check
   (= (+ '(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
         '(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL))
      '(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL))))

(deftest test- ()
  (check
   (cl:= (- 8 3) (cl:- 8 3))
   (cl:= (- 17 9) (cl:- 17 9))))

(defpackage :ch08
  (:use :common-lisp :test :ch02 :ch03 :ch05)
  (:shadow :subsetp :union :set-difference)
  (:shadowing-import-from :ch02 :member))

(in-package :ch08)

;;;
;;;    This doesn't actually require a new definition of MEMBER as discussed on pg. 123 (i.e., eq? vs. equal?)... EQL works in any case.
;;;    
(defun setp (lat)
  (cond ((endp lat) t)
        ((member (first lat) (rest lat)) nil)
        (t (setp (rest lat)))) )

(deftest test-setp ()
  (check
   (not (setp '(apple peaches apple plum)))
   (setp '(apples peaches pears plums))
   (not (setp '(apple 3 pear 4 9 apple 3 4)))) )

(defun makeset (lat)
  (cond ((endp lat) '())
        ((member (first lat) (rest lat)) (makeset (rest lat)))
        (t (cons (first lat) (makeset (rest lat)))) ))

(deftest test-makeset ()
  (check
   (let ((set (makeset '(apple peach pear peach plum apple lemon peach)))
         (expected '(PEAR PLUM APPLE LEMON PEACH)))
     (and (cl:subsetp set expected) (cl:subsetp expected set)))
   (let ((set (makeset '(apple 3 pear 4 9 apple 3 4)))
         (expected '(APPLE 3 PEAR 4 9)))
     (and (cl:subsetp set expected) (cl:subsetp expected set)))) )

;;;
;;;    Not new MULTI-REMBER
;;;    
(defun makeset (lat)
  (cond ((endp lat) '())
        (t (cons (first lat) (makeset (multi-rember (first lat) (rest lat)))) )))

(defun subsetp (set1 set2)
  (cond ((endp set1) t)
;        ((endp set2) nil) ; Unnecessary!
        ((member (first set1) set2) (subsetp (rest set1) set2))
        (t nil)))

(deftest test-subsetp ()
  (check
   (subsetp '() '(5 hamburgers 2 pieces fried chicken and light duckling wings))
   (subsetp '() '())
   (not (subsetp '(a b) '()))
   (subsetp '(5 chicken wings) '(5 hamburgers 2 pieces fried chicken and light duckling wings))
   (not (subsetp '(4 pounds of horseradish) '(four pounds chicken and 5 ounces horseradish)))) )

(defun subsetp (set1 set2)
  (cond ((endp set1) t)
        (t (and (member (first set1) set2) (subsetp (rest set1) set2)))) )

(defun set-equal (set1 set2)
  (cond ((subsetp set1 set2) (subsetp set2 set1))
        (t nil)))

(defun set-equal (set1 set2)
  (and (subsetp set1 set2) (subsetp set2 set1)))

(deftest test-set-equal ()
  (check
   (set-equal '(6 large chickens with wings)
              '(6 chickens with large wings))))

(defun intersectp (set1 set2)
  (cond ((endp set1) nil)
        ((member (first set1) set2) t)
        (t (intersectp (rest set1) set2))))

(defun intersectp (set1 set2)
  (if (endp set1)
      nil
      (or (member (first set1) set2)
          (intersectp (rest set1) set2))))

(deftest test-intersectp ()
  (check
   (intersectp '(tomatoes and macaroni) '(macaroni and cheese))
   (not (intersectp '() '(macaroni and cheese)))
   (not (intersectp '(tomatoes and macaroni) '()))) )

(defun intersect (set1 set2)
  (cond ((endp set1) '())
        ((member (first set1) set2) (cons (first set1) (intersect (rest set1) set2)))
        (t (intersect (rest set1) set2))))

(defun intersect (set1 set2)
  (cond ((endp set1) '())
        ((not (member (first set1) set2)) (intersect (rest set1) set2))
        (t (cons (first set1) (intersect (rest set1) set2)))) )

(deftest test-intersect ()
  (check
   (set-equal (intersect '(tomatoes and macaroni) '(macaroni and cheese)) '(and macaroni))))

(defun union (set1 set2)
  (cond ((endp set1) set2)
        ((member (first set1) set2) (union (rest set1) set2))
        (t (cons (first set1) (union (rest set1) set2)))) )

(deftest test-union ()
  (check
   (set-equal (union '(tomatoes and macaroni casserole) '(macaroni and cheese)) '(tomatoes and macaroni casserole cheese))))

(defun set-difference (set1 set2)
  (cond ((endp set1) '())
        ((member (first set1) set2) (set-difference (rest set1) set2))
        (t (cons (first set1) (set-difference (rest set1) set2)))) )

(deftest test-set-difference ()
  (check
   (set-equal (set-difference '(a b c d e) '(b d e)) '(a c))))

(defun intersectall (l-set)
  (cond ((endp l-set) '())
        ((endp (rest l-set)) (first l-set))
        (t (intersect (first l-set) (intersectall (rest l-set)))) ))

(deftest test-intersectall ()
  (check
   (set-equal (intersectall '((a b c) (c a d e) (e f g h a b))) '(a))
   (set-equal (intersectall '((6 pears and) (3 peaches and 6 peppers) (8 pears and 6 plums) (and 6 prunes with lots of apples))) '(6 and))))

(defun pairp (obj)
  (and (consp obj)
       (destructuring-bind (&optional (first nil first-p) (second nil second-p) &rest tail) obj
         (and first-p
              second-p
              (null tail)))) )

(deftest test-pairp ()
  (check
   (pairp '(1 2))
   (not (pairp '(1)))
   (not (pairp '(1 2 3)))) )

(defun build (first second)
  (list first second))

(defun relp (l)
  (cond ((endp l) t)
        (t (and (pairp (first l))
                (not (member (first l) (rest l))) ; Relies on updated version of MEMBER using EQUAL (See pg. 123)
                (relp (rest l)))) ))

(deftest test-relp ()
  (check
   (not (relp '(apples peaches pumpkin pie)))
   (not (relp '((apples peaches) (pumpkin pie) (apples peaches))))
   (relp '((apples peaches) (pumpkin pie)))
   (relp '((4 3) (4 2) (7 6) (6 2) (3 4)))
   (relp '((8 3) (4 2) (7 6) (6 2) (3 4)))) )

(defun member-key (obj l test)
  (cond ((endp l) nil)
        ((equal obj (funcall test (first l))) l)
        (t (member-key obj (rest l) test))))

(deftest test-member-key ()
  (check
   (not (member 'a '((c d) (a b) (e f))))
   (member-key 'a '((c d) (a b) (e f)) #'first)))

(defun funp (rel)
  (cond ((endp rel) t)
        (t (and (pairp (first rel))
                (not (member-key (first (first rel)) (rest rel) #'first))
                (funp (rest rel)))) ))

(defun funp (rel)
  (and (relp rel)
       (setp (firsts rel))))

(deftest test-funp ()
  (check
   (not (funp '(apples peaches pumpkin pie)))
   (not (funp '((apples peaches) (pumpkin pie) (apples peaches))))
   (funp '((apples peaches) (pumpkin pie)))
   (not (funp '((4 3) (4 2) (7 6) (6 2) (3 4))))
   (funp '((8 3) (4 2) (7 6) (6 2) (3 4)))) )

(defun revrel (rel)
  (if (endp rel)
      '()
      (cons (build (second (first rel)) (first (first rel)))
            (revrel (rest rel)))) )

(deftest test-revrel ()
  (check
   (set-equal (revrel '((8 a) (pumpkin pie) (got sick))) '((a 8) (pie pumpkin) (sick got)))) )

(defun injectivep (fun)
  (and (funp fun)
       (setp (firsts (revrel fun)))) )

;;;
;;;    D'oh!
;;;
(defun injectivep (fun)
  (funp (revrel fun)))

(deftest test-injectivep ()
  (check
   (not (injectivep '((8 3) (4 2) (7 6) (6 2) (3 4))))
   (not (injectivep '((8 3) (4 2) (7 6) (4 9) (3 4)))) ; This fails with simplified definition since reverse of original relation is a function although the original isn't.
   (injectivep '((8 3) (4 8) (7 6) (6 2) (3 4)))
   (not (injectivep '((grape raisin) (plum prune) (stewed prune))))
   (injectivep '((grape raisin) (plum prune) (stewed grape)))) )

(defsuite test-ch08 ()
  (test-funp)
  (test-intersect)
  (test-revrel)
  (test-subsetp)
  (test-union)
  (test-setp)
  (test-injectivep)
  (test-relp)
  (test-set-equal)
  (test-makeset)
  (test-pairp)
  (test-intersectp)
  (test-intersectall)
  (test-set-difference)
  (test-member-key))

(defpackage :ch08a
  (:use :common-lisp :collections :test :ch03)
  (:shadowing-import-from :collections :intersection :set :set-difference :subsetp :union))

(in-package :ch08a)

(defun pairp (obj)
  (and (consp obj)
       (destructuring-bind (&optional (first nil first-p) (second nil second-p) &rest tail) obj
         (and first-p
              second-p
              (null tail)))) )

(deftest test-pairp ()
  (check
   (pairp '(1 2))
   (not (pairp '(1)))
   (not (pairp '(1 2 3)))) )

(defun build (first second)
  (list first second))

(defun relp (set)
  (every #'pairp (elements set)))

(deftest test-relp ()
  (check
   (not (relp #{'apples 'peaches 'pumpkin 'pie}))
   (relp #{'(apples peaches) '(pumpkin pie) '(apples peaches)})
   (relp #{'(apples peaches) '(pumpkin pie)})
   (relp #{'(4 3) '(4 2) '(7 6) '(6 2) '(3 4)})
   (relp #{'(8 3) '(4 2) '(7 6) '(6 2) '(3 4)})))

(defun funp (set)
  (and (relp set)
       (setp (firsts (elements set)))) )

(deftest test-funp ()
  (check
   (not (funp #{'apples 'peaches 'pumpkin 'pie}))
   (funp #{'(apples peaches) '(pumpkin pie) '(apples peaches)})
   (funp #{'(apples peaches) '(pumpkin pie)})
   (not (funp #{'(4 3) '(4 2) '(7 6) '(6 2) '(3 4)}))
   (funp #{'(8 3) '(4 2) '(7 6) '(6 2) '(3 4)})))

(defun revrel (rel)
  (make-set :elements (mapcar #'reverse (elements rel)) :test #'equal))

(deftest test-revrel ()
  (check
   (set-equal-p (revrel #{'(8 a) '(pumpkin pie) '(got sick)}) #{'(a 8) '(pie pumpkin) '(sick got)})))

(defun injectivep (fun)
  (funp (revrel fun)))

(deftest test-injectivep ()
  (check
   (not (injectivep #{'(8 3) '(4 2) '(7 6) '(6 2) '(3 4)}))
   (not (injectivep #{'(8 3) '(4 2) '(7 6) '(4 9) '(3 4)})) ; Reverse of original relation is a function although the original isn't.
   (injectivep #{'(8 3) '(4 8) '(7 6) '(6 2) '(3 4)})
   (not (injectivep #{'(grape raisin) '(plum prune) '(stewed prune)}))
   (injectivep #{'(grape raisin) '(plum prune) '(stewed grape)})))

(defsuite test-ch08a ()
  (test-funp)
  (test-revrel)
  (test-injectivep)
  (test-relp)
  (test-pairp))

(defpackage :ch09 (:use :common-lisp :test) (:shadow :subst :subsetp :length))

(in-package :ch09)

;;;
;;;    Examine other list (sequence) functions for similarities like intersectp and subsetp
;;;    

(defun rember-f (test obj l)
  (cond ((endp l) '())
        ((funcall test obj (first l)) (rest l))
        (t (cons (first l) (rember-f test obj (rest l)))) ))

(deftest test-rember-f ()
  (check
   (equal (rember-f #'= 5 '(6 2 5 3)) '(6 2 3))
   (equal (rember-f #'eq 'jelly '(jelly beans are good)) '(beans are good))
   (equal (rember-f #'eql '(pop corn) '(lemonade (pop corn) and (cake))) '(lemonade (pop corn) and (cake)))
   (equal (rember-f #'equal '(pop corn) '(lemonade (pop corn) and (cake))) '(lemonade and (cake)))) )

(defun rember-f (test)
  #'(lambda (obj l)
      (cond ((endp l) '())
            ((funcall test obj (first l)) (rest l))
            (t (cons (first l) (funcall (rember-f test) obj (rest l)))) )))

(deftest test-rember-f ()
  (check
   (equal (funcall (rember-f #'=) 5 '(6 2 5 3)) '(6 2 3))
   (equal (funcall (rember-f #'eq) 'jelly '(jelly beans are good)) '(beans are good))
   (equal (funcall (rember-f #'eq) 'tuna '(tuna salad is good)) '(salad is good))
   (equal (funcall (rember-f #'eql) '(pop corn) '(lemonade (pop corn) and (cake))) '(lemonade (pop corn) and (cake)))
   (equal (funcall (rember-f #'equal) '(pop corn) '(lemonade (pop corn) and (cake))) '(lemonade and (cake)))) )

(defun insert-l-f (test)
  #'(lambda (old new lat)
      (cond ((endp lat) '())
            ((funcall test (first lat) old) (cons new lat))
            (t (cons (first lat) (funcall (insert-l-f test) old new (rest lat)))) )))

(deftest test-insert-l-f ()
  (check
   (equal (funcall (insert-l-f #'eql) 'pickle 'topping '(ice cream with fudge for dessert)) '(ice cream with fudge for dessert))
   (equal (funcall (insert-l-f #'eql) 'topping 'fudge '(ice cream with topping for dessert)) '(ice cream with fudge topping for dessert))
   (equal (funcall (insert-l-f #'eql) 'jalapeno 'and '(tacos tamales jalapeno salsa)) '(tacos tamales and jalapeno salsa))
   (equal (funcall (insert-l-f #'eql) 'e 'd '(a b c e f g e h)) '(a b c d e f g e h))))

(defun insert-r-f (test)
  #'(lambda (old new lat)
      (cond ((endp lat) '())
            ((funcall test (first lat) old) (cons old (cons new (rest lat))))
            (t (cons (first lat) (funcall (insert-r-f test) old new (rest lat)))) )))

(deftest test-insert-r-f ()
  (check
   (equal (funcall (insert-r-f #'eql) 'pickle 'topping '(ice cream with fudge for dessert)) '(ice cream with fudge for dessert))
   (equal (funcall (insert-r-f #'eql) 'fudge 'topping '(ice cream with fudge for dessert)) '(ice cream with fudge topping for dessert))
   (equal (funcall (insert-r-f #'eql) 'and 'jalapeno '(tacos tamales and salsa)) '(tacos tamales and jalapeno salsa))
   (equal (funcall (insert-r-f #'eql) 'd 'e '(a b c d f g d h)) '(a b c d e f g d h))))

(defun insert-g (old left right lat)
  (cond ((endp lat) '())
        ((eql (first lat) old) (cons left (cons right (rest lat))))
        (t (cons (first lat) (insert-g old left right (rest lat)))) ))

(deftest test-insert-r ()
  (check
   (equal (insert-g 'pickle 'pickle 'topping '(ice cream with fudge for dessert)) '(ice cream with fudge for dessert))
   (equal (insert-g 'fudge 'fudge 'topping '(ice cream with fudge for dessert)) '(ice cream with fudge topping for dessert))
   (equal (insert-g 'and 'and 'jalapeno '(tacos tamales and salsa)) '(tacos tamales and jalapeno salsa))
   (equal (insert-g 'd 'd 'e '(a b c d f g d h)) '(a b c d e f g d h))))

(deftest test-insert-l ()
  (check
   (equal (insert-g 'pickle 'topping 'pickle '(ice cream with fudge for dessert)) '(ice cream with fudge for dessert))
   (equal (insert-g 'topping 'fudge 'topping '(ice cream with topping for dessert)) '(ice cream with fudge topping for dessert))
   (equal (insert-g 'jalapeno 'and 'jalapeno '(tacos tamales jalapeno salsa)) '(tacos tamales and jalapeno salsa))
   (equal (insert-g 'e 'd 'e '(a b c e f g e h)) '(a b c d e f g e h))))

(defun seq-l (a b lat)
  (cons a (cons b lat)))

(defun seq-l (a b lat)
  (list* a b lat))

(defun seq-r (a b lat)
  (cons b (cons a lat)))

(defun seq-r (a b lat)
  (list* b a lat))

(defun seq-r (a b lat)
  (seq-l b a lat))

(defun insert-g-f (seq)
  #'(lambda (old new lat)
      (cond ((endp lat) '())
            ((eql (first lat) old) (funcall seq new old (rest lat)))
            (t (cons (first lat) (funcall (insert-g-f seq) old new (rest lat)))) )))

(defun insert-l (old new lat)
  (funcall (insert-g-f #'seq-l) old new lat))

(defun insert-r (old new lat)
  (funcall (insert-g-f #'seq-r) old new lat))

(deftest test-insert-r ()
  (check
   (equal (insert-r 'pickle 'topping '(ice cream with fudge for dessert)) '(ice cream with fudge for dessert))
   (equal (insert-r 'fudge 'topping '(ice cream with fudge for dessert)) '(ice cream with fudge topping for dessert))
   (equal (insert-r 'and 'jalapeno '(tacos tamales and salsa)) '(tacos tamales and jalapeno salsa))
   (equal (insert-r 'd 'e '(a b c d f g d h)) '(a b c d e f g d h))))

(deftest test-insert-l ()
  (check
   (equal (insert-l 'pickle 'topping '(ice cream with fudge for dessert)) '(ice cream with fudge for dessert))
   (equal (insert-l 'topping 'fudge '(ice cream with topping for dessert)) '(ice cream with fudge topping for dessert))
   (equal (insert-l 'jalapeno 'and '(tacos tamales jalapeno salsa)) '(tacos tamales and jalapeno salsa))
   (equal (insert-l 'e 'd '(a b c e f g e h)) '(a b c d e f g e h))))

(defun insert-l (old new lat)
  (funcall (insert-g-f #'(lambda (a b lat) (list* a b lat))) old new lat))

(defun insert-r (old new lat)
  (funcall (insert-g-f #'(lambda (a b lat) (list* b a lat))) old new lat))

(defun seq-sub (a b lat)
  (declare (ignore b))
  (cons a lat))

(defun subst (old new lat)
  (funcall (insert-g-f #'seq-sub) old new lat))

(deftest test-subst ()
  (check
   (equal (subst 'pickle 'topping '(ice cream with fudge for dessert)) '(ice cream with fudge for dessert))
   (equal (subst 'fudge 'topping '(ice cream with fudge for dessert)) '(ice cream with topping for dessert))
   (equal (subst 'and 'jalapeno '(tacos tamales and salsa)) '(tacos tamales jalapeno salsa))
   (equal (subst 'd 'e '(a b c d f g d h)) '(a b c e f g d h))))

(defun seq-rem (a b lat)
  (declare (ignore a b))
  lat)

(defun rember (old lat)
  (funcall (insert-g-f #'seq-rem) old nil lat))

(deftest test-rember ()
  (check
   (equal (rember 'mint '(lamb chops and mint jelly)) '(lamb chops and jelly))
   (equal (rember 'mint '(lamb chops and mint flavored mint jelly)) '(lamb chops and flavored mint jelly))
   (equal (rember 'toast '(bacon lettuce and tomato)) '(bacon lettuce and tomato))
   (equal (rember 'bacon '(bacon lettuce and tomato)) '(lettuce and tomato))
   (equal (rember 'and '(bacon lettuce and tomato)) '(bacon lettuce tomato))
   (equal (rember 'cup '(coffee cup tea cup and hick cup)) '(coffee tea cup and hick cup))
   (equal (rember 'sauce '(soy sauce and tomato sauce)) '(soy and tomato sauce))))

(defun value (expr)
  (cond ((numberp expr) expr)
        (t (destructuring-bind (operator op1 op2) expr
             (funcall (operator-function operator) (value op1) (value op2)))) ))

(defun operator-function (operator)
  (case operator
    (plus #'+)
    (times #'*)
    (expt #'expt)))

(defun subsetp (set1 set2)
  (if (endp set1)
      t
      (and (member (first set1) set2)
           (subsetp (rest set1) set2))))

;;
;;    A.
;;    
(defun subsetp (set1 set2)
  (or (null set1)
      (and (member (first set1) set2)
           (subsetp (rest set1) set2))))

(defun intersectp (set1 set2)
  (if (endp set1)
      nil
      (or (member (first set1) set2)
          (intersectp (rest set1) set2))))

(defun intersectp (set1 set2)
  (if (not (endp set1))
      (or (member (first set1) set2)
          (intersectp (rest set1) set2))
      nil))

(defun intersectp (set1 set2)
  (or (and (not (endp set1))
           (or (member (first set1) set2)
               (intersectp (rest set1) set2)))
      (and (endp set1) nil)))

;;
;;    B.
;;    
(defun intersectp (set1 set2)
 (and (not (null set1))
      (or (member (first set1) set2)
          (intersectp (rest set1) set2))))

(defun intersectp (set1 set2)
  (not (or (null set1)
           (and (not (member (first set1) set2))
                (not (intersectp (rest set1) set2)))) ))

;;
;;    Based on A. and B. above:
;;
(defun set-f-p (outer)
  #'(lambda (set1 set2)
      (case outer
        (or (or (null set1)
                (and (member (first set1) set2)
                     (funcall (set-f-p outer)
                              (rest set1)
                              set2))))
        (and (and (not (null set1))
                  (or (member (first set1) set2)
                      (funcall (set-f-p outer)
                               (rest set1)
                               set2)))) )))

(defun subsetp (set1 set2)
  (funcall (set-f-p 'or) set1 set2))

(defun intersectp (set1 set2)
  (funcall (set-f-p 'and) set1 set2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Macro experiments;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    Stack overflow!!
;;;    
;; (defmacro set-f-p-m (outer when-null inner)
;;   `#'(lambda (s1 s2)
;;        (,outer (,when-null (null s1))
;;                (,inner (member (first s1) s2)
;;                        (funcall (set-f-p-m ,outer ,when-null ,inner) (rest s1) s2)))) )

;; (defun subsetp (set1 set2)
;;   (funcall (set-f-p-m or identity and) set1 set2))

;; (defun intersectp (set1 set2)
;;   (funcall (set-f-p-m and not or) set1 set2))

(defmacro set-f-p-m (outer when-null inner)
  `#'(lambda (s1 s2)
       (,outer (,when-null (null s1))
               (,inner (member (first s1) s2)
                       (funcall (set-f-p ',outer ',when-null ',inner) (rest s1) s2)))) )

(defun set-f-p (outer when-null inner)
  (set-f-p-m outer when-null inner))

(defun subsetp (set1 set2)
  (funcall (set-f-p-m or identity and) set1 set2))

(defun intersectp (set1 set2)
  (funcall (set-f-p-m and not or) set1 set2))

(defmacro set-f-p-m (f outer when-null inner)
  `#'(lambda (s1 s2)
       (,outer (,when-null (null s1))
               (,inner (member (first s1) s2)
                       (,f (rest s1) s2)))) )

(defun subsetp (set1 set2)
  (funcall (set-f-p-m subsetp or identity and) set1 set2))

(defun intersectp (set1 set2)
  (funcall (set-f-p-m intersectp and not or) set1 set2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun set-f-p (logicalp const)
  #'(lambda (set1 set2)
      (if (endp set1)
          const
          (if (funcall logicalp set1 set2)
              t
              nil))))

(defun subsetp (set1 set2)
  (funcall (set-f-p #'(lambda (s1 s2)
                        (and (member (first s1) s2)
                             (subsetp (rest s1) s2)))
                    t)
           set1
           set2))

(deftest test-subsetp ()
  (check
   (subsetp '() '(5 hamburgers 2 pieces fried chicken and light duckling wings))
   (subsetp '() '())
   (not (subsetp '(a b) '()))
   (subsetp '(5 chicken wings) '(5 hamburgers 2 pieces fried chicken and light duckling wings))
   (not (subsetp '(4 pounds of horseradish) '(four pounds chicken and 5 ounces horseradish)))) )

(defun intersectp (set1 set2)
  (funcall (set-f-p #'(lambda (s1 s2)
                        (or (member (first s1) s2)
                            (intersectp (rest s1) s2)))
                    nil)
           set1
           set2))

(deftest test-intersectp ()
  (check
   (intersectp '(tomatoes and macaroni) '(macaroni and cheese))
   (not (intersectp '() '(macaroni and cheese)))
   (not (intersectp '(tomatoes and macaroni) '()))) )

(defun and-prime (expr set1 set2)
  (and expr (subsetp (rest set1) set2)))

(defun or-prime (expr set1 set2)
  (or expr (intersectp (rest set1) set2)))

(defun set-f-p (logicalp const)
  #'(lambda (set1 set2)
      (if (endp set1)
          const
          (funcall logicalp (member (first set1) set2) set1 set2))))

(defun subsetp (set1 set2)
  (funcall (set-f-p #'and-prime t) set1 set2))

(defun intersectp (set1 set2)
  (funcall (set-f-p #'or-prime nil) set1 set2))

;;
;;    Based on A. and B. again
;;    
;; (defun intersectp (set1 set2)
;;  (and (not (null set1))
;;       (or (member (first set1) set2)
;;           (intersectp (rest set1) set2))))

;; (defun subsetp (set1 set2)
;;   (or (null set1)
;;       (and (member (first set1) set2)
;;            (subsetp (rest set1) set2))))

(defun set-f-p (logical1 logical2 when-null)
  #'(lambda (set1 set2)
      (funcall logical1
               (funcall when-null (null set1))
               #'(lambda () 
                   (funcall logical2 (member (first set1) set2)
                            (set-f-p logical1 logical2 when-null)
                            (rest set1)
                            set2)))) )

;; (defun and1 (boolean f)
;;   (and boolean
;;        (funcall f)))

;; (defun or1 (boolean f)
;;   (or boolean
;;       (funcall f)))

;; (defun and2 (boolean f s1 s2)
;;   (and boolean
;;        (funcall f s1 s2)))

;; (defun or2 (boolean f s1 s2)
;;   (or boolean
;;       (funcall f s1 s2)))

;; (defun subsetp (set1 set2)
;;   (funcall (set-f-p #'or1 #'and2 #'identity) set1 set2))

;; (defun intersectp (set1 set2)
;;   (funcall (set-f-p #'and1 #'or2 #'not) set1 set2))

(defun and-f (boolean f &rest args)
  (and boolean
       (apply f args)))

(defun or-f (boolean f &rest args)
  (or boolean
      (apply f args)))

(defun subsetp (set1 set2)
  (funcall (set-f-p #'or-f #'and-f #'identity) set1 set2))

(defun intersectp (set1 set2)
  (funcall (set-f-p #'and-f #'or-f #'not) set1 set2))

;;;
;;;    On the road to the Y-Combinator
;;;    
(defun multi-rember (obj l)
  (cond ((endp l) '())
        ((eql (first l) obj) (multi-rember obj (rest l)))
        (t (cons (first l) (multi-rember obj (rest l)))) ))

(defun mrember-curry (l)
  (multi-rember 'curry l))

(defun mrember-curry (l)
  ((lambda (obj) (multi-rember obj l)) 'curry))

(defun mrember-curry (l)
  (cond ((endp l) '())
        ((eql (first l) 'curry) (mrember-curry (rest l)))
        (t (cons (first l) (mrember-curry (rest l)))) ))

(deftest test-mrember-curry ()
  (check
   (equal (mrember-curry '(a b c curry e curry g curry)) '(a b c e g))))

(defun curry-maker (future)
  #'(lambda (l)
      (cond ((endp l) '())
            ((eql (first l) 'curry) (funcall (curry-maker future) (rest l)))
            (t (cons (first l) (funcall (curry-maker future) (rest l)))) )))

(defun mrember-curry (l)
  (funcall (curry-maker #'curry-maker) l))

;;;
;;;    This works too...
;;;    But the FUTURE argument is needed below.
;;;    
;; (defun curry-maker ()
;;   #'(lambda (l)
;;       (cond ((endp l) '())
;;             ((eql (first l) 'curry) (funcall (curry-maker) (rest l)))
;;             (t (cons (first l) (funcall (curry-maker) (rest l)))) )))

;; (defun mrember-curry (l)
;;   (funcall (curry-maker) l))

(setf (symbol-function 'mrember-curry) (curry-maker #'curry-maker))

;; (defun function-maker ()
;;   #'(lambda (future)
;;       #'(lambda (l)
;;           (cond ((endp l) '())
;;                 ((eql (first l) 'curry) (funcall (funcall future future) (rest l)))
;;                 (t (cons (first l) (funcall (funcall future future) (rest l)))) ))))
;; (let ((f (function-maker))) (funcall (funcall f f) '(a b c curry e curry g curry)))

;;    I.
(defun function-maker (future)
  #'(lambda (l)
      (cond ((endp l) '())
            ((eql (first l) 'curry) (funcall (funcall future future) (rest l)))
            (t (cons (first l) (funcall (funcall future future) (rest l)))) )))

(setf (symbol-function 'mrember-curry) (function-maker #'function-maker))





(funcall ((lambda (future)
            #'(lambda (l)
                (cond ((endp l) '())
                      ((eql (first l) 'curry) (funcall (funcall future future) (rest l)))
                      (t (cons (first l) (funcall (funcall future future) (rest l)))) )))
          #'(lambda (future)
              #'(lambda (l)
                  (cond ((endp l) '())
                        ((eql (first l) 'curry) (funcall (funcall future future) (rest l)))
                        (t (cons (first l) (funcall (funcall future future) (rest l)))) ))))
         '(a b c curry e curry g curry))

(funcall ((lambda (future)
            #'(lambda (obj l)
                (cond ((endp l) '())
                      ((eql (first l) obj) (funcall (funcall future future) obj (rest l)))
                      (t (cons (first l) (funcall (funcall future future) obj (rest l)))) )))
          #'(lambda (future)
              #'(lambda (obj l)
                  (cond ((endp l) '())
                        ((eql (first l) obj) (funcall (funcall future future) obj (rest l)))
                        (t (cons (first l) (funcall (funcall future future) obj (rest l)))) ))))
         'pung
         '(a b c pung e pung g pung))

;; (funcall (f f) y)
;; ((lambda (x) (funcall (f f) x)) y)

#|
Common Lisp

(f (cdr l)) => ((lambda (x) (f x)) (cdr l))
(funcall (funcall future future) (cdr l)) =>
((lambda (arg) (funcall (funcall future future) arg)) (cdr l))

Scheme
(f (cdr l)) => ((lambda (x) (f x)) (cdr l))
((future future) (cdr l)) =>
((lambda (arg) ((future future) arg)) (cdr l))

Clojure
(f (rest l)) => ((fn [x] (f x)) (rest l))
((future future) (rest l)) =>
((fn [arg] ((future future) arg)) (rest l))
|#

;;    I.
(defun function-maker (future)
  #'(lambda (l)
      (cond ((endp l) '())
            ((eql (first l) 'curry) (funcall (funcall future future) (rest l)))
            (t (cons (first l) (funcall (funcall future future) (rest l)))) )))

;;    II.
(defun function-maker (future)
  #'(lambda (l)
      (cond ((endp l) '())
            ((eql (first l) 'curry) ((lambda (arg) (funcall (funcall future future) arg)) (rest l)))
            (t (cons (first l) ((lambda (arg) (funcall (funcall future future) arg)) (rest l)))) )))

;;    III.
(defun function-maker (future)
  ((lambda (recur)
     #'(lambda (l)
         (cond ((endp l) '())
               ((eql (first l) 'curry) (funcall recur (rest l)))
               (t (cons (first l) (funcall recur (rest l)))) )))
   #'(lambda (arg)
       (funcall (funcall future future) arg))))

;;    IIIa.
(defun function-maker (future)
  (flet ((recur (arg)
           (funcall (funcall future future) arg)))
    #'(lambda (l)
        (cond ((endp l) '())
              ((eql (first l) 'curry) (recur (rest l)))
              (t (cons (first l) (recur (rest l)))) ))))

;;    IV.
(defun function-maker (future)
  (foo #'(lambda (arg)
           (funcall (funcall future future) arg))))

(defun foo (recur)
  #'(lambda (l)
      (cond ((endp l) '())
            ((eql (first l) 'curry) (funcall recur (rest l)))
            (t (cons (first l) (funcall recur (rest l)))) )))

(setf (symbol-function 'mrember-curry) (function-maker #'function-maker))

(defun M (recur)
  #'(lambda (l)
      (cond ((endp l) '())
            ((eql (first l) 'curry) (funcall recur (rest l)))
            (t (cons (first l) (funcall recur (rest l)))) )))

(defun mrember-curry (l)
  (funcall (M ((lambda (future) #'(lambda (arg) (funcall (funcall future future) arg)))
               #'(lambda (future) (M #'(lambda (arg) (funcall (funcall future future) arg)))) ))
           l))

(defun mrember-curry (l)
  (funcall ((lambda (future) (M #'(lambda (arg) (funcall (funcall future future) arg))))
            #'(lambda (future) (M #'(lambda (arg) (funcall (funcall future future) arg)))))
           l))

(setf (symbol-function 'mrember-curry)
      ((lambda (future) (M #'(lambda (arg) (funcall (funcall future future) arg))))
       #'(lambda (future) (M #'(lambda (arg) (funcall (funcall future future) arg)))) ))

(setf (symbol-function 'mrember-curry)
      (funcall #'(lambda (m)
                   ((lambda (future) (funcall m #'(lambda (arg) (funcall (funcall future future) arg))))
                    #'(lambda (future) (funcall m #'(lambda (arg) (funcall (funcall future future) arg)))) ))
               #'M))

(defun Y (M)
  ((lambda (future) (funcall M #'(lambda (arg) (funcall (funcall future future) arg))))
   #'(lambda (future) (funcall M #'(lambda (arg) (funcall (funcall future future) arg)))) ))

(setf (symbol-function 'mrember-curry)
      (Y #'M))

(defun L (recur)
  #'(lambda (l)
      (if (endp l)
          0
          (1+ (funcall recur (rest l)))) ))

(setf (symbol-function 'length)
      (Y #'L))

(deftest test-length ()
  (check
   (= (length '(a b c)) 3)
   (= (length '()) 0)))

(setf (symbol-function 'length)
      (Y #'(lambda (recur)
             #'(lambda (l)
                 (if (endp l)
                     0
                     (1+ (funcall recur (rest l)))) ))))

(setf (symbol-function 'length)
      ((lambda (M)
         ((lambda (future) (funcall M #'(lambda (arg) (funcall (funcall future future) arg))))
          #'(lambda (future) (funcall M #'(lambda (arg) (funcall (funcall future future) arg)))) ))
       #'(lambda (recur)
           #'(lambda (l)
               (if (endp l)
                   0
                   (1+ (funcall recur (rest l)))) ))))

(funcall ((lambda (M)
            ((lambda (future) (funcall M #'(lambda (arg) (funcall (funcall future future) arg))))
             #'(lambda (future) (funcall M #'(lambda (arg) (funcall (funcall future future) arg)))) ))
          #'(lambda (recur)
              #'(lambda (l)
                  (if (endp l)
                      0
                      (1+ (funcall recur (rest l)))) )))
         '(a b c))

(defpackage :ch10
  (:use :common-lisp :test)
  (:shadow :apply))

(in-package :ch10)

(defun make-entry (keys vals)
  (list keys vals))

(defun lookup-in-entry (name entry)
  (cond ((endp (first entry)) nil)
        ((eql name (first (first entry))) (first (second entry)))
        (t (lookup-in-entry name (make-entry (rest (first entry)) (rest (second entry)))) )))

(defun lookup-in-entry (name entry missing)
  (lookup-in-entry-aux name (first entry) (second entry) missing))

(defun lookup-in-entry-aux (name keys vals missing)
  (cond ((endp keys) (funcall missing name))
        ((eql (first keys) name) (first vals))
        (t (lookup-in-entry-aux name (rest keys) (rest vals) missing))))

(defun extend-table (entry table)
  (cons entry table))

(defun lookup-in-table (name table missing)
  (if (endp table)
      (funcall missing name)
      (or (lookup-in-entry name (first table) (constantly nil))
          (lookup-in-table name (rest table) missing))))

(defun lookup-in-table (name table missing)
  (if (endp table)
      (funcall missing name)
      (lookup-in-entry name
                       (first table)
                       #'(lambda (name)
                           (lookup-in-table name (rest table) missing)))) )

;; (defun value (expr)
;;   (cond ((numberp expr) expr)
;;         ((numberedp expr) (funcall (second expr)
;;                                    (value (first expr))
;;                                    (value (third expr)))) ))

;; (defun value (expr)
;;   (cond ((numberp expr) expr)
;;         ((numberedp expr) (destructuring-bind (op1 operator op2) expr
;;                             (funcall operator (value op1) (value op2)))) ))

;; (deftest test-value ()
;;   (check
;;    (cl:= (value '(8 + (9 * 4))) 44)
;;    (cl:= (value '(3 + (2 ^ 5))) 35)))

;; (defun value (expr)
;;   (cond ((numberp expr) expr)
;;         (t (destructuring-bind (operator op1 op2) expr
;;              (funcall (operator-function operator) (value op1) (value op2)))) ))

;; (defun operator-function (operator)
;;   (case operator
;;     (plus #'+)
;;     (times #'*)
;;     (expt #'expt)))

;; (deftest test-value ()
;;   (check
;;    (cl:= (value '(plus 8 (times 9 4))) 44)
;;    (cl:= (value '(plus 3 (expt 2 5))) 35)))

(defun expression-to-action (e)
  (cond ((atom e) (atom-to-action e))
        (t (list-to-action e))))

(defun atom-to-action (atom)
  (cond ((numberp atom) #'*self-evaluating)
        (t #'*identifier)))

(defun list-to-action (list)
  (case (first list)
    (quote #'*quote)
    (lambda #'*lambda)
    (cond #'*cond)
    (otherwise (if (eql (first (first list)) 'lambda)
                   #'*application
                   (error "Unrecognized operator.")))) )

(defun list-to-action (e)
  (cond ((atom (first e)) (first-action (first e)))
        (t #'*application)))

(defun first-action (e)
  (case e
    (quote #'*quote)
    (lambda #'*lambda)
    (cond #'*cond)
    (otherwise #'*application)))

(defun value (e)
  (meaning e '()))

(defun meaning (e table)
  (funcall (expression-to-action e) e table))

(defun *self-evaluating (e table)
  (declare (ignore table))
  e)

(defun *quote (e table)
  (declare (ignore table))
  (text-of-quotation e))

(defun text-of-quotation (e)
  (second e))

(defun *identifier (e table)
  (lookup-in-table e
                   table
                   #'(lambda (name)
                       (case name
                         ((t) t)
                         ((nil) nil)
                         (otherwise (list 'primitive name)))) ))

(defun *lambda (e table)
  (list 'non-primitive (cons table (rest e))))

(defun table (e)
  (first e))

(defun params (e)
  (second e))

(defun body (e)
  (third e))

(defun evcon (lines table)
  (cond ((meaning (question (first lines)) table)
         (meaning (answer (first lines)) table))
        (t (evcon (rest lines) table))))

(defun question (e)
  (first e))

(defun answer (e)
  (second e))

(defun *cond (e table)
  (evcon (rest e) table))

;(*cond '(cond (coffee klatsch) (t party)) '(((coffee) (t)) ((klatsch party) (5 (6))))) => 5

(defun evlis (args table)
  (cond ((endp args) '())
        (t (cons (meaning (first args) table) (evlis (rest args) table)))) )

(defun *application (e table)
  (apply (meaning (function-of e) table)
         (evlis (args e) table)))

(defun function-of (e)
  (first e))

(defun args (e)
  (rest e))

(defun primitivep (f)
  (eql (first f) 'primitive))

(defun non-primitivep (f)
  (eql (first f) 'non-primitive))

(defun apply (f args)
  (cond ((primitivep f) (apply-primitive (second f) args))
        ((non-primitivep f) (apply-closure (second f) args))))

(defun apply-primitive (f args)
  (case f
    (car (car (first args)))
    (cdr (cdr (first args)))
    (cons (cons (first args) (second args)))
    (eql (eql (first args) (second args)))
    (atom (atom (first args)))
    (not (not (first args)))
    (null (null (first args)))
    (numberp (numberp (first args)))
    (zerop (zerop (first args)))
    (1+ (1+ (first args)))
    (1- (1- (first args)))) )

(defun apply-closure (f args)
  (meaning (body f) (extend-table (make-entry (params f) args) (table f))))