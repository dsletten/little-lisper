;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Pascal is for building pyramids—imposing, breathtaking, static structures built by armies pushing heavy blocks into place. Lisp is for building organisms...
;;;;   -- Alan Perlis
;;;;
;;;;   Name:               little.lisp
;;;;
;;;;   Started:            Fri Apr 30 11:20:59 2021
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
(load "/home/slytobias/lisp/packages/lang.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :little
  (:use :common-lisp :lang :test)
  (:export :build)
  (:shadow :member :subst + - * > < = :expt :length :not :equal :subsetp :intersection :union :complement))

(in-package :little)

;;;
;;;    Chapter 2
;;;    

;;;
;;;    (15 页) lat: A list of atoms.
;;;    
(defun latp (l)
  "Does the list L contains only atoms? In other words, if L contains any elements are they all atoms?"
  (cond ((null l) t)
        ((atom (first l)) (latp (rest l)))
        (t nil)))

;;;
;;;    For completeness...
;;;    Although it could simply be considered an error to pass a non-list arg to LATP.
;;;    
;; (defun latp (l)
;;   "Does the list L contains only atoms? In other words, if L contains any elements are they all atoms?"
;;   (cond ((not (listp l)) nil)
;;         ((null l) t)
;;         ((atom (first l)) (latp (rest l)))
;;         (t nil)))

(deftest test-latp ()
  (check
   (latp '(jack sprat could eat no chicken fat))
   (not (latp '((jack) sprat could eat no chicken fat)))
   (not (latp '(jack (sprat could) eat no chicken fat)))
   (latp '())
   (latp '(bacon and eggs))
   (not (latp '(bacon (and eggs)))) ))

;;;
;;;    Book's version (essentially)
;;;    
(defun member (a lat &key (test #'eql))
  (cond ((null lat) nil)
        (t (or (funcall test (first lat) a)
               (member a (rest lat) :test test))) ))

(defun member (a lat &key (test #'eql))
  (cond ((null lat) nil)
        ((funcall test (first lat) a) t)
        (t (member a (rest lat) :test test))) )

(deftest test-member ()
  (check
   (member 'tea '(coffee tea or milk))
   (not (member 'poached '(fried eggs and scrambled eggs)))
   (member 'meat '(mashed potatoes and meat gravy))
   (not (member 'liver '(bagels and lox)))) )

;;;
;;;    Chapter 3
;;;    

;;;
;;;    REMBER  - Remove target atom from list
;;;    INSERTR - Insert atom to right of target atom
;;;    INSERTL - Insert atom to left of target atom
;;;    SUBST   - Replace target atom with new atom.
;;;

;; (defun rember (a lat)
;;   (cond ((null lat) '())
;;         (t (cond ((eql (first lat) a) (rest lat))
;;                  (t (cons (first lat) (rember a (rest lat)))) ))))

(defun rember (a lat)
  (cond ((null lat) '())
        ((eql (first lat) a) (rest lat))
        (t (cons (first lat) (rember a (rest lat)))) ))

(deftest test-rember ()
  (check
   (equal (rember 'mint '(lamb chops and mint jelly)) '(lamb chops and jelly))
   (equal (rember 'mint '(lamb chops and mint flavored mint jelly)) '(lamb chops and flavored mint jelly))
   (equal (rember 'toast #1='(bacon lettuce and tomato)) #1#)
   (equal (rember 'cup '(coffee cup tea cup and hick cup)) '(coffee tea cup and hick cup))
   (equal (rember 'bacon '(bacon lettuce and tomato)) '(lettuce and tomato))
   (equal (rember 'and '(bacon lettuce and tomato)) '(bacon lettuce tomato))
   (equal (rember 'sauce '(soy sauce and tomato sauce)) '(soy and tomato sauce))))

(defun firsts (lol)
  (cond ((null lol) '())
        (t (cons (first (first lol)) (firsts (rest lol)))) ))

(defun firsts (lol)
  (if (null lol)
      '()
      (destructuring-bind ((first . more) . lists) lol
        (declare (ignore more))
        (cons first (firsts lists)))) )

(deftest test-firsts ()
  (check
   (equal (firsts '((apple peach pumpkin) (plum pear cherry) (grape raisin pea) (bean carrot eggplant))) '(apple plum grape bean))
   (equal (firsts '((a b) (c d) (e f))) '(a c e))
   (equal (firsts '()) '())
   (equal (firsts '((five plums) (four) (eleven green oranges))) '(five four eleven))))

(defun insertr (old new lat)
  (cond ((null lat) '())
        ((eql (first lat) old) (cons old (cons new (rest lat))))
        (t (cons (first lat) (insertr old new (rest lat)))) ))

(defun insertr (old new lat)
  (cond ((null lat) '())
        ((eql (first lat) old) (list* old new (rest lat)))
        (t (cons (first lat) (insertr old new (rest lat)))) ))

(deftest test-insertr ()
  (check
   (equal (insertr 'fudge 'topping '(ice cream with fudge for dessert)) '(ice cream with fudge topping for dessert))
   (equal (insertr 'and 'jalapeño '(tacos tamales and salsa)) '(tacos tamales and jalapeño salsa))
   (equal (insertr 'd 'e '(a b c d f g d h)) '(a b c d e f g d h))))

(defun insertl (old new lat)
  (cond ((null lat) '())
        ((eql (first lat) old) (cons new lat))
        (t (cons (first lat) (insertl old new (rest lat)))) ))

(deftest test-insertl ()
  (check
   (equal (insertl 'topping 'fudge '(ice cream with topping for dessert)) '(ice cream with fudge topping for dessert))
   (equal (insertl 'salsa 'jalapeño '(tacos tamales and salsa)) '(tacos tamales and jalapeño salsa))
   (equal (insertl 'f 'e '(a b c d f g d h)) '(a b c d e f g d h))))

(defun subst (old new lat)
  (cond ((null lat) '())
        ((eql (first lat) old) (cons new (rest lat)))
        (t (cons (first lat) (subst old new (rest lat)))) ))

(deftest test-subst ()
  (check
   (equal (subst 'fudge 'topping '(ice cream with fudge for dessert)) '(ice cream with topping for dessert))
   (equal (subst 'jalapeño 'and '(tacos tamales jalapeño salsa)) '(tacos tamales and salsa))
   (equal (subst 'd 'e '(a b c d f g d h)) '(a b c e f g d h))))

;;;
;;;    Refactor! (Compare INSERT-G below)
;;;    
(defun skeleton (f target lat)
  (cond ((null lat) '())
        ((eql (first lat) target) (funcall f lat))
        (t (cons (first lat) (skeleton f target (rest lat)))) ))

;; (defun rember (a lat)
;;   (skeleton #'(lambda (lat) (rest lat)) a lat))

(defun rember (a lat)
  (skeleton #'rest a lat))

(defun insertr (old new lat)
  (skeleton #'(lambda (lat) (list* old new (rest lat))) old lat))

(defun insertl (old new lat)
  (skeleton #'(lambda (lat) (cons new lat)) old lat))

(defun subst (old new lat)
  (skeleton #'(lambda (lat) (cons new (rest lat))) old lat))

(defun subst2 (o1 o2 new lat)
  "Substitute for the first instance of either O1 or O2 with NEW."
  (cond ((null lat) '())
        ((or (eql (first lat) o1)
             (eql (first lat) o2))
         (cons new (rest lat)))
        (t (cons (first lat) (subst2 o1 o2 new (rest lat)))) ))

(defun subst2 (o1 o2 new lat)
  (cond ((null lat) '())
        ((member (first lat) (list o1 o2)) (cons new (rest lat)))
        (t (cons (first lat) (subst2 o1 o2 new (rest lat)))) ))

(deftest test-subst2 ()
  (check
   (equal (subst2 'chocolate 'banana 'vanilla '(banana ice cream with chocolate topping)) '(vanilla ice cream with chocolate topping))))

;;;
;;;    Refactor again! (Previous change cannot accomodate SUBST2)
;;;    
(defun skeleton2 (match action lat)
  (cond ((null lat) '())
        ((funcall match (first lat)) (funcall action lat))
        (t (cons (first lat) (skeleton2 match action (rest lat)))) ))

(defun rember (a lat)
  (skeleton2 #'(lambda (elt) (eql elt a)) #'rest lat))

(defun insertr (old new lat)
  (skeleton2 #'(lambda (elt) (eql elt old)) #'(lambda (lat) (list* old new (rest lat))) lat))

(defun insertl (old new lat)
  (skeleton2 #'(lambda (elt) (eql elt old)) #'(lambda (lat) (cons new lat)) lat))

(defun subst (old new lat)
  (skeleton2 #'(lambda (elt) (eql elt old)) #'(lambda (lat) (cons new (rest lat))) lat))

(defun subst2 (o1 o2 new lat)
  (skeleton2 #'(lambda (elt) (or (eql elt o1) (eql elt o2))) #'(lambda (lat) (cons new (rest lat))) lat))

;;;
;;;    Chapter 4
;;;

(defun + (n m)
  (cond ((zerop n) m)
        (t (+ (1- n) (1+ m)))) )

(deftest test-+ ()
  (check
   (cl:= (+ 0 0) (cl:+ 0 0))
   (cl:= (+ 0 1) (cl:+ 0 1))
   (cl:= (+ 1 0) (cl:+ 1 0))
   (cl:= (+ 1 1) (cl:+ 1 1))
   (cl:= (+ 2 3) (cl:+ 2 3))))
   
(defun - (n m)
  (cond ((zerop m) n)
        ((zerop n) (error "Negative numbers are not supported."))
        (t (- (1- n) (1- m)))) )

(deftest test-- ()
  (check
   (cl:= (- 0 0) (cl:- 0 0))
   (cl:= (- 1 0) (cl:- 1 0))
   (cl:= (- 1 1) (cl:- 1 1))
   (cl:= (- 8 3) (cl:- 8 3))
   (cl:= (- 17 9) (cl:- 17 9))
   (handler-case (- 3 5)
     (error (e)
       (format t "Got expected error: ~A~%" e)
       t)
     (:no-error (obj)
       (declare (ignore obj))
       (error "Can't subtract larger number from smaller.~%")))) )

(defun addvec (vec)
  (cond ((null vec) 0)
        (t (+ (first vec) (addvec (rest vec)))) ))

(deftest test-addvec ()
  (check
   (cl:= (addvec #1='()) (reduce #'cl:+ #1#))
   (cl:= (addvec #2='(3 5 2 8)) (reduce #'cl:+ #2#))
   (cl:= (addvec #3='(15 6 7 12 3)) (reduce #'cl:+ #3#))))

(defun * (n m)
  (cond ((zerop m) 0)
        (t (+ n (* n (1- m)))) ))

(deftest test-* ()
  (check
   (cl:= (* 5 3) (cl:* 5 3))
   (cl:= (* 13 4) (cl:* 13 4))
   (cl:= (* 12 3) (cl:* 12 3))))

(defun vec+ (vec1 vec2)
  (cond ((null vec1) '())
        (t (cons (+ (first vec1) (first vec2))
                 (vec+ (rest vec1) (rest vec2)))) ))

(defun vec+ (xs ys)
  (cond ((null xs) '())
        (t (destructuring-bind (x . morex) xs
             (destructuring-bind (y . morey) ys
               (cons (+ x y) (vec+ morex morey)))) )))

(defun vec+ (xs ys)
  (mapcar #'cl:+ xs ys))

(defun vec+ (xs ys)
  (loop for x in xs
        for y in ys
        collect (+ x y)))

(deftest test-vec+ ()
  (check
   (equal (vec+ #1='(3 6 9 11 4) #2='(8 5 2 0 7)) (mapcar #'cl:+ #1# #2#))
   (equal (vec+ #3='(2 3) #4='(4 6)) (mapcar #'cl:+ #3# #4#))
   (equal (vec+ #5='(3 7) #6='(4 6)) (mapcar #'cl:+ #5# #6#))))

(defun vec+ (vec1 vec2)
  (cond ((null vec1) vec2)
        ((null vec2) vec1)
        (t (cons (+ (first vec1) (first vec2))
                 (vec+ (rest vec1) (rest vec2)))) ))

(deftest test-vec+ ()
  (check
   (equal (vec+ #1='(3 6 9 11 4) #2='(8 5 2 0 7)) (mapcar #'cl:+ #1# #2#))
   (equal (vec+ #3='(2 3) #4='(4 6)) (mapcar #'cl:+ #3# #4#))
   (equal (vec+ #5='(3 7) #4#) (mapcar #'cl:+ #5# #4#))
   (equal (vec+ #5# '(4 6 8 1)) '(7 13 8 1))
   (equal (vec+ '(3 7 8 1) #4#) '(7 13 8 1))))

(defun > (n m)
  (cond ((zerop n) nil)
        ((zerop m) t)
        (t (> (1- n) (1- m)))) )

(deftest test-> ()
  (check
   (not (> 0 1))
   (> 1 0)
   (not (> 12 13))
   (not (> 12 12))
   (> 12 11)))

(defun < (n m)
  (> m n))

(defun < (n m)
  (cond ((zerop m) nil)
        ((zerop n) t)
        (t (< (1- n) (1- m)))) )

(deftest test-< ()
  (check
   (< 0 1)
   (not (< 1 0))
   (< 4 6)
   (not (< 8 3))
   (not (< 6 6))))

(defun = (n m)
  (cond ((zerop n) (zerop m))
        ((zerop m) nil)
        (t (= (1- n) (1- m)))) )

(defun = (n m)
  (cond ((> n m) nil)
        ((< n m) nil)
        (t t)))

(deftest test-= ()
  (check
   (= 0 0)
   (not (= 0 1))
   (not (= 1 0))
   (= 2 2)
   (not (= 2 3))
   (not (= 3 2))))

(defun expt (n m)
  (cond ((zerop m) 1)
        (t (* n (expt n (1- m)))) ))

(deftest test-expt ()
  (check
   (cl:= (expt 1 1) (cl:expt 1 1))
   (cl:= (expt 2 3) (cl:expt 2 3))
   (cl:= (expt 5 3) (cl:expt 5 3))))

(defun length (lat)
  (cond ((null lat) 0)
        (t (1+ (length (rest lat)))) ))

(deftest test-length ()
  (check
   (cl:= (length #1='(hotdogs with mustard sauerkraut and pickles)) (cl:length #1#))
   (cl:= (length #2='(ham and cheese on rye)) (cl:length #2#))))

;;;
;;;    1-based index!
;;;    
(defun pick (n lat)
  (cond ((null lat) nil)
        ((= n 1) (first lat))
        (t (pick (1- n) (rest lat)))) )

(deftest test-pick ()
  (check
   (equal (pick 4 '(lasagna spaghetti ravioli macaroni meatball)) 'macaroni)
   (equal (pick 0 '()) nil)))

;;;
;;;    1-based index!
;;;    
(defun rempick (n lat)
  (cond ((null lat) '())
        ((= n 1) (rest lat))
        (t (cons (first lat) (rempick (1- n) (rest lat)))) ))

(deftest test-rempick ()
  (check
   (equal (rempick 3 '(hotdogs with hot mustard)) '(hotdogs with mustard))
   (equal (rempick 0 '()) '())))

(defun no-nums (lat)
  (cond ((null lat) '())
        ((numberp (first lat)) (no-nums (rest lat)))
        (t (cons (first lat) (no-nums (rest lat)))) ))

(deftest test-no-nums ()
  (check
   (equal (no-nums #1='(5 pears 6 prunes 9 dates)) (remove-if #'numberp #1#))))

(defun all-nums (lat)
  (cond ((null lat) '())
        ((numberp (first lat)) (cons (first lat) (all-nums (rest lat))))
        (t (all-nums (rest lat)))) )

(deftest test-all-nums ()
  (check
   (equal (all-nums #1='(5 pears 6 prunes 9 dates)) (remove-if-not #'numberp #1#))))

(defun eqatom (a1 a2)
  (cond ((numberp a1) (and (numberp a2) (= a1 a2)))
        ((numberp a2) nil)
        ((atom a1) (and (atom a2) (eql a1 a2)))
        (t nil)))

;;;
;;;    Book assumes that A1 and A2 are both atoms.
;;;    Otherwise their version is simpler (ignoring their nested COND)...
;;;    
(defun eqatom (a1 a2)
  (cond ((numberp a1) (and (numberp a2) (= a1 a2)))
        ((numberp a2) nil)
        (t (eql a1 a2))))

(deftest test-eqatom ()
  (check
   (eqatom 3 3)
   (eqatom 'pung 'pung)
   (not (eqatom 3 4))
   (not (eqatom 'pung 'foo))
   (not (eqatom 3 'pung))
   (not (eqatom 'pung 3))))

;;;
;;;    Chapter 5
;;;

(defun multirember (a lat)
  "Remove all instances of A from LAT."
  (cond ((null lat) '())
        ((eql (first lat) a) (multirember a (rest lat)))
        (t (cons (first lat) (multirember a (rest lat)))) ))

(deftest test-multirember ()
  (check
   (equal (multirember 'mint '(lamb chops and mint jelly)) '(lamb chops and jelly))
   (equal (multirember 'mint '(lamb chops and mint flavored mint jelly)) '(lamb chops and flavored jelly))
   (equal (multirember 'toast #1='(bacon lettuce and tomato)) #1#)
   (equal (multirember 'cup '(coffee cup tea cup and hick cup)) '(coffee tea and hick))
   (equal (multirember 'bacon '(bacon lettuce and tomato)) '(lettuce and tomato))
   (equal (multirember 'and '(bacon lettuce and tomato)) '(bacon lettuce tomato))
   (equal (multirember 'sauce '(soy sauce and tomato sauce)) '(soy and tomato))))

(defun multiinsertr (old new lat)
  (cond ((null lat) '())
        ((eql (first lat) old) (cons old (cons new (multiinsertr old new (rest lat)))) )
        (t (cons (first lat) (multiinsertr old new (rest lat)))) ))

(defun multiinsertr (old new lat)
  (cond ((null lat) '())
        ((eql (first lat) old) (list* old new (multiinsertr old new (rest lat))))
        (t (cons (first lat) (multiinsertr old new (rest lat)))) ))

(deftest test-multiinsertr ()
  (check
   (equal (multiinsertr 'fudge 'topping '(ice cream with fudge for dessert)) '(ice cream with fudge topping for dessert))
   (equal (multiinsertr 'and 'jalapeño '(tacos and tamales and salsa)) '(tacos and jalapeño tamales and jalapeño salsa))
   (equal (multiinsertr 'd 'e '(a b c d f g d h)) '(a b c d e f g d e h))))

(defun multiinsertl (old new lat)
  (cond ((null lat) '())
        ((eql (first lat) old) (cons new (cons old (multiinsertl old new (rest lat)))) )
        (t (cons (first lat) (multiinsertl old new (rest lat)))) ))

(defun multiinsertl (old new lat)
  (cond ((null lat) '())
        ((eql (first lat) old) (list* new old (multiinsertl old new (rest lat))))
        (t (cons (first lat) (multiinsertl old new (rest lat)))) ))

(deftest test-multiinsertl ()
  (check
   (equal (multiinsertl 'topping 'fudge '(ice cream with topping for dessert)) '(ice cream with fudge topping for dessert))
   (equal (multiinsertl 'salsa 'jalapeño '(tacos tamales and salsa)) '(tacos tamales and jalapeño salsa))
   (equal (multiinsertl 'f 'e '(a b c d f g d f h)) '(a b c d e f g d e f h))
   (equal (multiinsertl 'fish 'fried '(chips and fish or fish and fried)) '(chips and fried fish or fried fish and fried))))

(defun multisubst (old new lat)
  (cond ((null lat) '())
        ((eql (first lat) old) (cons new (multisubst old new (rest lat))))
        (t (cons (first lat) (multisubst old new (rest lat)))) ))

(deftest test-multisubst ()
  (check
   (equal (multisubst 'fudge 'topping '(ice cream with fudge for dessert)) '(ice cream with topping for dessert))
   (equal (multisubst 'jalapeño 'and '(tacos tamales jalapeño salsa)) '(tacos tamales and salsa))
   (equal (multisubst 'd 'e '(a b c d f g d h)) '(a b c e f g e h))))

(defun occur (a lat)
  "How many times does the atom A occur in the list LAT?"
  (cond ((null lat) 0)
        ((eql (first lat) a) (1+ (occur a (rest lat))))
        (t (occur a (rest lat)))) )

(deftest test-occur ()
  (check
   (zerop (occur 'a '()))
   (= (occur 'a '(a b c)) 1)
   (= (occur 'a '(a a a)) 3)
   (= (occur 'a '(a b a c a)) 3)))

(defun onep (n)
  (cond ((zerop n) nil)
        (t (zerop (1- n)))) )

(defun onep (n)
  (= n 1))

(deftest test-onep ()
  (check
   (onep 1)
   (not (onep 0))
   (not (onep 2))))

;;;
;;;    1-based index!
;;;    
(defun rempick (n lat)
  (cond ((null lat) '())
        ((onep n) (rest lat))
        (t (cons (first lat) (rempick (1- n) (rest lat)))) ))

(defun multisubst2 (o1 o2 new lat)
  "Substitute for all instances of either O1 or O2 with NEW."
  (cond ((null lat) '())
        ((or (eql (first lat) o1)
             (eql (first lat) o2))
         (cons new (multisubst2 o1 o2 new (rest lat))))
        (t (cons (first lat) (multisubst2 o1 o2 new (rest lat)))) ))

(defun multisubst2 (o1 o2 new lat)
  (let ((olds (list o1 o2)))
    (labels ((subst (lat)
               (cond ((null lat) '())
                     ((member (first lat) olds) (cons new (subst (rest lat))))
                     (t (cons (first lat) (subst (rest lat)))) )))
      (subst lat))))

(deftest test-multisubst2 ()
  (check
   (equal (multisubst2 'chocolate 'banana 'vanilla '(banana ice cream with chocolate topping)) '(vanilla ice cream with vanilla topping))))

(defun multiskeleton (match action lat)
  (cond ((null lat) '())
        ((funcall match (first lat)) (funcall action (multiskeleton match action (rest lat))))
        (t (cons (first lat) (multiskeleton match action (rest lat)))) ))

(defun multirember (a lat)
  (multiskeleton #'(lambda (elt) (eql elt a)) #'identity lat))

(defun multiinsertr (old new lat)
  (multiskeleton #'(lambda (elt) (eql elt old)) #'(lambda (rest) (list* old new rest)) lat))

(defun multiinsertl (old new lat)
  (multiskeleton #'(lambda (elt) (eql elt old)) #'(lambda (rest) (list* new old rest)) lat))

(defun multisubst (old new lat)
  (multiskeleton #'(lambda (elt) (eql elt old)) #'(lambda (rest) (cons new rest)) lat))

(defun multisubst2 (o1 o2 new lat)
  (multiskeleton #'(lambda (elt) (or (eql elt o1) (eql elt o2))) #'(lambda (rest) (cons new rest)) lat))

;;;
;;;    Chapter 6
;;;

(defun leftmost (l)
  (cond ((null l) '())
        ((atom (first l)) (first l))
        (t (leftmost (first l)))) )

(deftest test-leftmost ()
  (check
   (eql (leftmost '((hot) (tuna (and)) cheese)) 'hot)
   (eql (leftmost '(((hamburger) french) (fries (and a) coke))) 'hamburger)
   (eql (leftmost '((((4) four)) 17 (seventeen))) 4)
   (eql (leftmost '(((() four)) 17 (seventeen))) '())))

(defun non-atom (obj)
  (not (atom obj)))

(defun non-atom (obj)
  (consp obj))

(defun not (obj)
  (cond (obj nil)
        (t t)))

(defun rember* (a l)
  "Remove all instances of A from the tree L."
  (cond ((null l) '())
        ((atom (car l))
         (cond ((eql (car l) a) (rember* a (cdr l)))
               (t (cons (car l) (rember* a (cdr l)))) ))
        (t (cons (rember* a (car l))
                 (rember* a (cdr l)))) ))

(defun rember* (a l)
  (cond ((null l) '())
        (t (destructuring-bind (car . cdr) l
             (cond ((atom car) (if (eql car a)
                                   (rember* a cdr)
                                   (cons car (rember* a cdr))))
                   (t (cons (rember* a car)
                            (rember* a cdr)))) ))))

;;;
;;;    My solution from 2002 (!!) handles this more elegantly.
;;;    (Consistent with semantics of "The Little Lisper" where () is both list/atom.
;;;     Different in "The Little Schemer")
;;;
;;;    OBJ is either an atom or a CONS:
;;;    1a If we've reached the end of the top-level list or any nested sublist, then OBJ is (),
;;;       which is an atom. Return it as the foundation of rebuilding the tree structure.
;;;     b Otherwise, we have the pathological case where REMBER* was called with an atomic tree OBJ.
;;;     c Or the atom OBJ does not match ELT and slipped the through the following test. Just return it.
;;;    2a OBJ is a CONS whose first element matches ELT. Remove it by simply recursively processing the
;;;       rest of the tree.
;;;     b The first element of OBJ does not match ELT (Although it may be either an atom or a CONS.)
;;;       Process both the CAR and CDR of OBJ and then reassemble those results with CONS.
;;;
;;;    To be fair, this violates The Law of Eq?. In the book both args to eq? must be atoms. We don't know
;;;    for sure that (car obj) is an atom in the 2nd COND clause.
;;;
;;;    In the book's style (Although this also violates The Fourth Commandment!)
;;;    
(defun rember* (elt obj)
  (cond ((atom obj) obj)
	(t (cond ((eq (car obj) elt) (rember* elt (cdr obj)))
	         (t (cons (rember* elt (car obj))
		          (rember* elt (cdr obj)))) ))))

;;;
;;;    My actual version.
;;;    
(defun rember* (elt obj)
  (cond ((atom obj) obj)
	((eq (car obj) elt) (rember* elt (cdr obj)))
	(t (cons (rember* elt (car obj))
		 (rember* elt (cdr obj)))) ) )

(deftest test-rember* ()
  (check
   (equal (rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup)) '((coffee) ((tea)) (and (hick))))
   (equal (rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce))) '(((tomato)) ((bean)) (and ((flying)))) )))

(defun insertr* (old new l)
  (cond ((null l) '())
        ((atom (car l))
         (cond ((eql (car l) old) (cons old (cons new (insertr* old new (cdr l)))) )
               (t (cons (car l) (insertr* old new (cdr l)))) ))
        (t (cons (insertr* old new (car l))
                 (insertr* old new (cdr l)))) ))

(defun insertr* (old new l)
  (cond ((null l) '())
        (t (destructuring-bind (car . cdr) l
             (cond ((atom car) (if (eql car old)
                                   (list* old new (insertr* old new cdr))
                                   (cons car (insertr* old new cdr))))
                   (t (cons (insertr* old new car)
                            (insertr* old new cdr)))) ))))

(deftest test-insertr* ()
  (check
   (equal (insertr* 'chuck 'roast '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))
          '((how much (wood)) could ((a (wood) chuck roast)) (((chuck roast))) (if (a) ((wood chuck roast))) could chuck roast wood))))

(defun occur* (a l)
  (cond ((null l) 0)
        ((atom (car l))
         (cond ((eql (car l) a) (1+ (occur* a (cdr l))))
               (t (occur* a (cdr l)))) )
        (t (+ (occur* a (car l))
              (occur* a (cdr l)))) ))

;;;
;;;    2002. See notes regarding REMBER* above.
;;;    
(defun occur* (a l)
  (cond ((eq a l) 1)
	((atom l) 0)
	(t (+ (occur* a (car l))
	      (occur* a (cdr l)))) ) )

(deftest test-occur* ()
  (check
   (= (occur* 'banana '((banana) (split ((((banana ice))) (cream (banana)) sherbet)) (banana) (bread) (banana brandy))) 5)))

(defun subst* (old new l)
  (cond ((null l) '())
        ((atom (car l))
         (cond ((eql (car l) old) (cons new (subst* old new (cdr l))))
               (t (cons (car l) (subst* old new (cdr l)))) ))
        (t (cons (subst* old new (car l))
                 (subst* old new (cdr l)))) ))

(deftest test-subst* ()
  (check
   (equal (subst* 'banana 'orange '((banana) (split ((((banana ice))) (cream (banana)) sherbet)) (banana) (bread) (banana brandy)))
          '((orange) (split ((((orange ice))) (cream (orange)) sherbet)) (orange) (bread) (orange brandy)))) )

(defun insertl* (old new l)
  (cond ((null l) '())
        ((atom (car l))
         (cond ((eql (car l) old) (cons new (cons old (insertl* old new (cdr l)))) )
               (t (cons (car l) (insertl* old new (cdr l)))) ))
        (t (cons (insertl* old new (car l))
                 (insertl* old new (cdr l)))) ))

(defun insertl* (old new l)
  (cond ((null l) '())
        ((atom (car l))
         (cond ((eql (car l) old) (list* new old (insertl* old new (cdr l))))
               (t (cons (car l) (insertl* old new (cdr l)))) ))
        (t (cons (insertl* old new (car l))
                 (insertl* old new (cdr l)))) ))

;;;
;;;    (103 页)
;;;    
(defun insertl* (old new l)
  (cond ((null l) '())
        ((consp (car l)) (cons (insertl* old new (car l))
                               (insertl* old new (cdr l))))
        ((eql (car l) old) (list* new old (insertl* old new (cdr l))))
        (t (cons (car l) (insertl* old new (cdr l)))) ))

(deftest test-insertl* ()
  (check
   (equal (insertl* 'chuck 'pecker '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))
          '((how much (wood)) could ((a (wood) pecker chuck)) (((pecker chuck))) (if (a) ((wood pecker chuck))) could pecker chuck wood))))

(defun member* (a l)
  (cond ((null l) nil)
        ((atom (car l))
         (cond ((eql (car l) a))
               (t (member* a (cdr l)))) )
        (t (or (member* a (car l))
               (member* a (cdr l)))) ))

(defun member* (a l)
  (cond ((null l) nil)
        ((atom (car l)) (or (eql (car l) a) (member* a (cdr l))))
        (t (or (member* a (car l))
               (member* a (cdr l)))) ))

(deftest test-member* ()
  (check
   (member* 'chips '((potato) (chips ((with) fish) (chips)))) ))

(defun skeleton* (base match fail action branch tree)
  (cond ((null tree) base)
        ((atom (car tree)) (if (funcall match (car tree))
                               (funcall action (skeleton* base match fail action branch (cdr tree)))
                               (funcall fail (car tree) (skeleton* base match fail action branch (cdr tree)))) )
        (t (funcall branch
                    (skeleton* base match fail action branch (car tree))
                    (skeleton* base match fail action branch (cdr tree)))) ))

(defun rember* (a l)
  (skeleton* '() #'(lambda (elt) (eql elt a)) #'cons #'identity #'cons l))

(defun insertr* (old new l)
  (skeleton* '() #'(lambda (elt) (eql elt old)) #'cons #'(lambda (rest) (list* old new rest)) #'cons l))

(defun occur* (a l)
  (skeleton* 0 #'(lambda (elt) (eql elt a)) #'(lambda (elt rest) (declare (ignore elt)) rest) #'1+ #'+ l))

(defun subst* (old new l)
  (skeleton* '() #'(lambda (elt) (eql elt old)) #'cons #'(lambda (rest) (cons new rest)) #'cons l))

(defun insertl* (old new l)
  (skeleton* '() #'(lambda (elt) (eql elt old)) #'cons #'(lambda (rest) (list* new old rest)) #'cons l))

(defun member* (a l)
  (skeleton* nil #'(lambda (elt) (eql elt a)) #'(lambda (elt rest) (declare (ignore elt)) rest) (constantly t) #'(lambda (car cdr) (or car cdr)) l))

(defun eqlist (l1 l2)
  (cond ((null l1) (null l2))
        ((null l2) nil)
        ((atom (car l1)) (and (atom (car l2))
                              (eqatom (car l1) (car l2))
                              (eqlist (cdr l1) (cdr l2))))
        ((atom (car l2)) nil)
        (t (and (eqlist (car l1) (car l2))
                (eqlist (cdr l1) (cdr l2)))) ))

(defun eqlist (l1 l2)
  (cond ((null l1) (null l2))
        ((null l2) nil)
        ((and (atom (car l1)) (atom (car l2)))
         (and (eqatom (car l1) (car l2)) (eqlist (cdr l1) (cdr l2))))
        ((or (atom (car l1)) (atom (car l2))) nil)
        (t (and (eqlist (car l1) (car l2))
                (eqlist (cdr l1) (cdr l2)))) ))

(deftest test-eqlist ()
  (check
   (eqlist '(strawberry ice cream) '(strawberry ice cream))
   (not (eqlist '(strawberry ice cream) '(strawberry cream ice)))
   (not (eqlist '(banana ((split))) '((banana) (split))))
   (not (eqlist '(beef ((sausage)) (and (soda))) '(beef ((salami)) (and (soda)))) )
   (eqlist '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda))))
   (not (eqlist '((a) b) '(a b)))) )
   
(defun equal (o1 o2)
  (cond ((and (atom o1) (atom o2)) (eqatom o1 o2))
        ((or (atom o1) (atom o2)) nil)
        (t (eqlist o1 o2))))

(deftest test-equal ()
  (check
   (equal 3 3)
   (equal 'a 'a)
   (not (equal 2 3))
   (not (equal 'a 3))
   (not (equal 3 '(3)))
   (equal '(a b c) '(a b c))
   (equal '((a) b ((c))) '((a) b ((c))))
   (not (equal '(a b ((c))) '((a) b ((c)))) )))

;;;
;;;    Mutually recursive.
;;;    
(defun eqlist (l1 l2)
  (cond ((null l1) (null l2))
        ((null l2) nil)
        (t (and (equal (car l1) (car l2))
                (equal (cdr l1) (cdr l2)))) ))

;; (defun rember (s l)
;;   (cond ((null l) '())
;;         ((equal (first l) s) (rest l))
;;         (t (cons (first l) (rember s (rest l)))) ))

;;;
;;;    Chapter 7
;;;

;;;
;;;    The book assumes that AEXP is a valid representation of an arithmetic expression.
;;;    
(defun numberedp (aexp)
  (cond ((numberp aexp) t)
        ((atom aexp) nil)
;(null aexp) t)
        ((or (eql (second aexp) '+)
             (eql (second aexp) '*)
             (eql (second aexp) '^))
         (and (numberedp (first aexp)) (numberedp (third aexp))))
        (t nil)))

(defun numberedp (aexp)
  (cond ((atom aexp) (numberp aexp))
        ((or (eql (second aexp) '+)
             (eql (second aexp) '*)
             (eql (second aexp) '^))
         (and (numberedp (first aexp)) (numberedp (third aexp))))
        (t nil)))

(defun numberedp (aexp)
  (cond ((atom aexp) (numberp aexp))
        (t (destructuring-bind (op1 operator op2) aexp
             (case operator
               ((+ * ^) (and (numberedp op1) (numberedp op2)))
               (otherwise nil)))) ))

(deftest test-numberedp ()
  (check
   (numberedp '(3 + (4 * 5)))
   (not (numberedp '(2 * sausage)))) )

(defun value (aexp)
  (cond ((numberp aexp) aexp)
        ((eql (second aexp) '+) (+ (value (first aexp)) (value (third aexp))))
        ((eql (second aexp) '*) (* (value (first aexp)) (value (third aexp))))
        ((eql (second aexp) '^) (expt (value (first aexp)) (value (third aexp)))) ))

(defun value (aexp)
  (cond ((numberp aexp) aexp)
        (t (destructuring-bind (op1 operator op2) aexp
             (ecase operator
               (+ (+ (value op1) (value op2)))
               (* (* (value op1) (value op2)))
               (^ (expt (value op1) (value op2)))) ))))

;;;
;;;    See ATOM-TO-FUNCTION below.
;;;    
(defun operation (operator)
  (ecase operator
    (+ #'+)
    (* #'*)
    (^ #'expt)))

(defun value (aexp)
  (cond ((numberp aexp) aexp)
        (t (destructuring-bind (op1 operator op2) aexp
             (funcall (operation operator) (value op1) (value op2)))) ))

(deftest test-value ()
  (check
   (= (value '(1 + 3)) 4)
   (= (value '(1 + (3 * 4))) 13)
   (= (value '(1 + (3 ^ 4))) 82)))

;;;
;;;    New representation for arithmetic expressions.
;;;
(defun value (aexp)
  (cond ((numberp aexp) aexp)
        ((eql (first aexp) 'plus) (+ (value (second aexp)) (value (third aexp))))
        ((eql (first aexp) 'times) (* (value (second aexp)) (value (third aexp))))
        ((eql (first aexp) 'expt) (expt (value (second aexp)) (value (third aexp)))) ))
        
(deftest test-value ()
  (check
   (= (value '(plus 1 3)) 4)
   (= (value '(plus 1 (times 3 4))) 13)
   (= (value '(plus 1 (expt 3 4))) 82)))

;;;
;;;    Refactoring
;;;
(defun 1st-sub-exp (aexp)
  (second aexp))

(defun 2nd-sub-exp (aexp)
  (third aexp))

(defun operator (aexp)
  (first aexp))

(defun value (aexp)
  (cond ((numberp aexp) aexp)
        ((eql (operator aexp) 'plus) (+ (value (1st-sub-exp aexp)) (value (2nd-sub-exp aexp))))
        ((eql (operator aexp) 'times) (* (value (1st-sub-exp aexp)) (value (2nd-sub-exp aexp))))
        ((eql (operator aexp) 'expt) (expt (value (1st-sub-exp aexp)) (value (2nd-sub-exp aexp)))) ))

(defun operation (operator)
  (ecase operator
    (plus #'+)
    (times #'*)
    (expt #'expt)))

(defun value (aexp)
  (cond ((numberp aexp) aexp)
        (t (destructuring-bind (operator op1 op2) aexp
             (funcall (operation operator) (value op1) (value op2)))) ))

;;;
;;;    Returning to first representation!
;;;
(defun 1st-sub-exp (aexp)
  (first aexp))

;; (defun 2nd-sub-exp (aexp)
;;   (third aexp))

(defun operator (aexp)
  (second aexp))

;;;
;;;    Cosmetic changes... plus -> +, times -> *, expt -> ^
;;;    
(defun value (aexp)
  (cond ((numberp aexp) aexp)
        ((eql (operator aexp) '+) (+ (value (1st-sub-exp aexp)) (value (2nd-sub-exp aexp))))
        ((eql (operator aexp) '*) (* (value (1st-sub-exp aexp)) (value (2nd-sub-exp aexp))))
        ((eql (operator aexp) '^) (expt (value (1st-sub-exp aexp)) (value (2nd-sub-exp aexp)))) ))

(deftest test-value ()
  (check
   (= (value '(1 + 3)) 4)
   (= (value '(1 + (3 * 4))) 13)
   (= (value '(1 + (3 ^ 4))) 82)))

;;;
;;;    New representation for numbers:
;;;    0 ()
;;;    1 (())
;;;    2 (() ())
;;;
(defpackage :unary
  (:use :common-lisp :lang :test)
  (:shadow + :zerop :1+ :1- :numberp))

(in-package :unary)

(defconstant unit '())

(defun zerop (n)
  (null n))

(defun 1+ (n)
  (cons unit n))

(defun 1- (n)
  (rest n))

(defun + (n m)
  (cond ((zerop n) m)
        (t (+ (1- n) (1+ m)))) )

(deftest test-+ ()
  (check
   (zerop (+ '() '()))
   (equal (+ '() '(())) '(()))
   (equal (+ '(()) '()) '(()))
   (equal (+ '(()) '(())) '(()()))
   (equal (+ '(()()) '(()()())) '(()()()()()))) )

(defun numberp (n)
  (cond ((zerop n) t)
        ((eql (first n) unit) (numberp (1- n)))
        (t nil)))

(deftest test-numberp ()
  (check
   (numberp '())
   (numberp (+ '(()) '(()())))
   (not (numberp '(a b c)))) )

;;;
;;;    Chapter 8
;;;

(in-package :little)

(defun setp (l)
  (cond ((null l) t)
        ((member (first l) (rest l)) nil)
        (t (setp (rest l)))) )

(deftest test-setp ()
  (check
   (setp '(apples peaches pears plums))
   (not (setp '(apple peaches apple plum)))
   (not (setp '(apple 3 pear 4 9 apple 3 4)))) )

;;;
;;;    REMOVE-DUPLICATES
;;;    
(defun make-set (lat)
  (cond ((null lat) '())
        ((member (first lat) (rest lat)) (make-set (rest lat)))
        (t (cons (first lat) (make-set (rest lat)))) ))

(defun set-equal-p (s1 s2 &key (test #'eql))
  (and (subsetp s1 s2 :test test) (subsetp s2 s1 :test test)))

(deftest test-make-set ()
  (check
   (set-equal-p (make-set '(apple peach pear peach plum apple lemon peach)) '(apple peach pear plum lemon))
   (set-equal-p (make-set '(apple 3 pear 4 9 apple 3 4)) '(apple 3 pear 4 9))))

(defun make-set (lat)
  (cond ((null lat) '())
        (t (cons (first lat) (make-set (multirember (first lat) (rest lat)))) )))

(defun subsetp (s1 s2 &key (test #'eql))
  (cond ((null s1) t)
        ((member (first s1) s2 :test test) (subsetp (rest s1) s2 :test test))
        (t nil)))

(deftest test-subsetp ()
  (check
   (subsetp '(5 chicken wings) '(5 hamburgers 2 pieces fried chicken and light duckling wings))
   (not (subsetp '(4 pounds of horseradish) '(four pounds chicken and 5 ounces horseradish)))))

(defun subsetp (s1 s2 &key (test #'eql))
  (or (null s1)
      (and (member (first s1) s2 :test test)
           (subsetp (rest s1) s2 :test test))))

(defun intersectp (s1 s2)
  (cond ((null s1) nil)
        ((member (first s1) s2) t)
        (t (intersectp (rest s1) s2))))

(deftest test-intersectp ()
  (check
   (intersectp '(tomatoes and macaroni) '(macaroni and cheese))))

(defun intersectp (s1 s2)
  (cond ((null s1) nil)
        (t (or (member (first s1) s2)
               (intersectp (rest s1) s2)))) )

(defun intersection (s1 s2)
  (cond ((null s1) '())
        ((member (first s1) s2) (cons (first s1) (intersection (rest s1) s2)))
        (t (intersection (rest s1) s2))))

(deftest test-intersection ()
  (check
   (set-equal-p (intersection '(tomatoes and macaroni) '(macaroni and cheese)) '(and macaroni))))

(defun intersection (s1 s2)
  (cond ((null s1) '())
        ((not (member (first s1) s2)) (intersection (rest s1) s2))
        (t (cons (first s1) (intersection (rest s1) s2)))) )

(defun union (s1 s2)
  (cond ((null s1) s2)
        ((member (first s1) s2) (union (rest s1) s2))
        (t (cons (first s1) (union (rest s1) s2)))) )

(deftest test-union ()
  (check
   (set-equal-p (union '(tomatoes and macaroni casserole) '(macaroni and cheese)) '(tomatoes and macaroni casserole and cheese))))

(defun complement (s1 s2)
  (cond ((null s1) '())
        ((member (first s1) s2) (complement (rest s1) s2))
        (t (cons (first s1) (complement (rest s1) s2)))) )

(deftest test-complement ()
  (check
   (set-equal-p (complement '(a b) '(a b)) '())
   (set-equal-p (complement '(a b c) '(d e f)) '(a b c))
   (set-equal-p (complement '(b c a) '(a d e)) '(b c))))

(defun intersectall (l-set)
  (cond ((null l-set) '())
        ((null (rest l-set)) (first l-set))
        (t (intersection (first l-set) (intersectall (rest l-set)))) ))

(deftest test-intersectall ()
  (check
   (set-equal-p (intersectall '((a b c) (c a d e) (e f g h a b))) '(a))
   (set-equal-p (intersectall '((6 pears and) (3 peaches and 6 peppers) (8 pears and 6 plums) (and 6 prunes with lots of apples))) '(6 and))))

(defun build (a1 a2)
  (cons a1 (cons a2 '())))

(defun funp (rel)
  (setp (firsts rel)))

(deftest test-funp ()
  (check
   (not (funp '((4 3) (4 2) (7 6) (6 2) (3 4))))
   (funp '((8 3) (4 2) (7 6) (6 2) (3 4)))) )

(defun revrel (rel)
  (cond ((null rel) '())
        (t (cons (build (second (first rel)) (first (first rel))) (revrel (rest rel)))) )) ; Ugh!!!

(defun revrel (rel)
  (cond ((null rel) '())
        (t (destructuring-bind ((a b) . more) rel
             (cons (build b a) (revrel more)))) ))

(deftest test-revrel ()
  (check
   (set-equal-p (revrel #1='((8 a) (pumpkin pie) (got sick))) (mapcar #'reverse #1#) :test #'equal)))

(defun injectivep (fun)
  (setp (firsts (revrel fun))))

;;;
;;;    Book's version
;;;    
(defun injectivep (fun)
  (funp (revrel fun)))

(deftest test-injectivep ()
  (check
   (not (injectivep '((8 3) (4 2) (7 6) (6 2) (3 4))))
   (injectivep '((8 3) (4 8) (7 6) (6 2) (3 4)))
   (not (injectivep '((grape raisin) (plum prune) (stewed prune))))
   (injectivep '((grape raisin) (plum prune) (stewed grape)))) )

;;;
;;;    Chapter 9
;;;

(defun rember-f (test a l)
  (cond ((null l) '())
        ((funcall test (first l) a) (rest l))
        (t (cons (first l) (rember-f test a (rest l)))) ))

(deftest test-rember-f ()
  (check
   (equal (rember-f #'eql 'mint '(lamb chops and mint jelly)) '(lamb chops and jelly))
   (equal (rember-f #'eql 'mint '(lamb chops and mint flavored mint jelly)) '(lamb chops and flavored mint jelly))
   (equal (rember-f #'eql 'toast #1='(bacon lettuce and tomato)) #1#)
   (equal (rember-f #'eql 'cup '(coffee cup tea cup and hick cup)) '(coffee tea cup and hick cup))
   (equal (rember-f #'eql 'bacon '(bacon lettuce and tomato)) '(lettuce and tomato))
   (equal (rember-f #'eql 'and '(bacon lettuce and tomato)) '(bacon lettuce tomato))
   (equal (rember-f #'eql 'sauce '(soy sauce and tomato sauce)) '(soy and tomato sauce))
   (equal (rember-f #'= 5 '(6 2 5 3)) '(6 2 3))
   (equal (rember-f #'eql 'jelly '(jelly beans are good)) '(beans are good))
   (equal (rember-f #'equal '(pop corn) '(lemonade (pop corn) and (cake))) '(lemonade and (cake)))) )

(defun rember-fn (test)
  #'(lambda (a l)
      (cond ((null l) '())
            ((funcall test (first l) a) (rest l))
            (t (cons (first l) (funcall (rember-fn test) a (rest l)))) )))

(defun rember-fn (test)
  (labels ((rember (a l)
             (cond ((null l) '())
                   ((funcall test (first l) a) (rest l))
                   (t (cons (first l) (rember a (rest l)))) )))
    #'rember))

(setf (symbol-function 'rember-=) (rember-fn #'=))

(deftest test-rember-= ()
  (check
   (equal (rember-= 5 '(6 2 5 3)) '(6 2 3))))

(deftest test-rember-eql ()
  (check
   (equal (funcall (rember-fn #'eql) 'tuna '(shrimp salad and tuna salad)) '(shrimp salad and salad))))

(defun insertl-f (test)
  #'(lambda (old new l)
      (cond ((null l) '())
            ((funcall test (first l) old) (cons new l))
            (t (cons (first l) (funcall (insertl-f test) old new (rest l)))) )))

(defun insertl-f (test)
  (labels ((insert (old new l)
             (cond ((null l) '())
                   ((funcall test (first l) old) (cons new l))
                   (t (cons (first l) (insert old new (rest l)))) )))
    #'insert))

(setf (symbol-function 'insertl-eql) (insertl-f #'eql))

(deftest test-insertl-eql ()
  (check
   (equal (insertl-eql 'topping 'fudge '(ice cream with topping for dessert)) '(ice cream with fudge topping for dessert))
   (equal (insertl-eql 'salsa 'jalapeño '(tacos tamales and salsa)) '(tacos tamales and jalapeño salsa))
   (equal (insertl-eql 'f 'e '(a b c d f g d h)) '(a b c d e f g d h))))

(defun insertr-f (test)
  #'(lambda (old new l)
      (cond ((null l) '())
            ((funcall test (first l) old) (cons old (cons new (rest l))))
            (t (cons (first l) (funcall (insertr-f test) old new (rest l)))) )))

(defun insertr-f (test)
  (labels ((insert (old new l)
             (cond ((null l) '())
                   ((funcall test (first l) old) (cons old (cons new (rest l))))
                   (t (cons (first l) (insert old new (rest l)))) )))
    #'insert))

(setf (symbol-function 'insertr-eql) (insertr-f #'eql))

(deftest test-insertr-eql ()
  (check
   (equal (insertr-eql 'fudge 'topping '(ice cream with fudge for dessert)) '(ice cream with fudge topping for dessert))
   (equal (insertr-eql 'and 'jalapeño '(tacos tamales and salsa)) '(tacos tamales and jalapeño salsa))
   (equal (insertr-eql 'd 'e '(a b c d f g d h)) '(a b c d e f g d h))))

(defun insert-g (test insert)
  #'(lambda (old new l)
      (cond ((null l) '())
            ((funcall test (first l) old) (funcall insert old new l))
            (t (cons (first l) (funcall (insert-g test insert) old new (rest l)))) )))

(defun insert-g (test seq)
  (labels ((insert (old new l)
             (cond ((null l) '())
                   ((funcall test (first l) old) (funcall seq old new l))
                   (t (cons (first l) (insert old new (rest l)))) )))
    #'insert))

(setf (symbol-function 'insertl-eql) (insert-g #'eql #'(lambda (old new l) (declare (ignore old)) (cons new l))))
(setf (symbol-function 'insertr-eql) (insert-g #'eql #'(lambda (old new l) (cons old (cons new (rest l)))) ))

;;;
;;;    The book flips the order of NEW and OLD for SEQL and SEQR!
;;;    
(defun seql (a b l)
  "Insert B to the left of A at the head of L."
  (list* b a l))

(defun seqr (a b l)
  "Insert B to the right of A at the head of L."
  (list* a b l))

;;;
;;;    Compare SKELETON/SKELETON2 above.
;;;    
(defun insert-g (seq)
  #'(lambda (old new l)
      (cond ((null l) '())
            ((eql (first l) old) (funcall seq old new (rest l)))
            (t (cons (first l) (funcall (insert-g seq) old new (rest l)))) )))

(defun insert-g (seq)
  (labels ((insert (old new l)
             (cond ((null l) '())
                   ((eql (first l) old) (funcall seq old new (rest l)))
                   (t (cons (first l) (insert old new (rest l)))) )))
    #'insert))

(setf (symbol-function 'insertl) (insert-g #'seql))
(setf (symbol-function 'insertr) (insert-g #'seqr))

(setf (symbol-function 'insertl) (insert-g #'(lambda (a b l) (list* b a l))))
(setf (symbol-function 'insertr) (insert-g #'(lambda (a b l) (list* a b l))))

(defun seqs (a b l)
  (declare (ignore a))
  (cons b l))

(setf (symbol-function 'subst) (insert-g #'seqs))
(setf (symbol-function 'subst) (insert-g #'(lambda (a b l) (declare (ignore a)) (cons b l))))

(defun seqrem (a b l)
  (declare (ignore a b))
  l)

;(setf (symbol-function 'rember) (insert-g #'seqrem))
;;;
;;;    Have to package the function! REMBER takes 2 args.
;;;    But INSERT-G is called at runtime!
;;;    
(setf (symbol-function 'rember) #'(lambda (a l) (funcall (insert-g #'seqrem) a nil l)))

(setf (symbol-function 'rember)
      (let ((insert (insert-g #'seqrem)))
        #'(lambda (a l) (funcall insert a nil l))))

;;;
;;;    See OPERATION above.
;;;    Book uses new Lisp syntax here: (+ 5 3)
;;;    
(defun atom-to-function (operator)
  (ecase operator
    (+ #'+)
    (* #'*)
    (^ #'expt)))

(defun value (aexp)
  (cond ((numberp aexp) aexp)
        (t (destructuring-bind (operator op1 op2) aexp
             (funcall (atom-to-function operator) (value op1) (value op2)))) ))

(deftest test-value ()
  (check
   (= (value '(+ 1 3)) 4)
   (= (value '(+ 1 (* 3 4))) 13)
   (= (value '(+ 1 (^ 3 4))) 82)))


(defun set-fp (logical result)
  #'(lambda (s1 s2)
      (cond ((null s1) result)
            (t (funcall logical (member (first s1) s2) (set-fp logical result) s1 s2)))) )

;;;
;;;    This breaks earlier SUBSETP which takes :TEST arg.
;;;    
(setf (symbol-function 'subsetp) (set-fp #'(lambda (bool continue s1 s2) (and bool (funcall continue (rest s1) s2))) t))
(setf (symbol-function 'intersectp) (set-fp #'(lambda (bool continue s1 s2) (or bool (funcall continue (rest s1) s2))) nil))

;;;
;;;    Book's approach...
;;;
(defun and* (p s1 s2)
  (and p (subsetp (rest s1) s2))) ; Mutual recursion

(defun or* (p s1 s2)
  (or p (intersectp (rest s1) s2)))

(defun set-fp (logical result)
  #'(lambda (s1 s2)
      (cond ((null s1) result)
            (t (funcall logical (member (first s1) s2) s1 s2)))) )

(setf (symbol-function 'subsetp) (set-fp #'and* t))
(setf (symbol-function 'intersectp) (set-fp #'or* nil))

;;;
;;;    Y-combinator (Curry's paradoxical combinator: https://en.wikipedia.org/wiki/Fixed-point_combinator)
;;;    https://viksit.com/blog/practical-applications-y-combinator-clojure
;;;    http://rosettacode.org/wiki/Y_combinator
;;;
;;;    /home/slytobias/lisp/Y Combinator
;;;    ./RonGarret/flownet/Y-combinator.lisp
;;;    
(defun multirember (a lat)
  "Remove all instances of A from LAT."
  (cond ((null lat) '())
        ((eql (first lat) a) (multirember a (rest lat)))
        (t (cons (first lat) (multirember a (rest lat)))) ))

;;;
;;;    CLHS §3.1.2.1.2.4
;;;    http://www.lispworks.com/documentation/HyperSpec/Body/03_ababd.htm
;;;    A lambda form is equivalent to using funcall of a lexical closure of the lambda expression on the given arguments.
;;;    ((lambda (x) (+ x 3)) 9) == (funcall #'(lambda (x) (+ x 3)) 9)
;;;    
;; ((lambda (l) (multirember 'curry l)) '(a b c curry e curry g curry)) => (A B C E G)
;; ((lambda (l) ((lambda (a) (multirember a l)) 'curry)) '(a b c curry e curry g curry)) => (A B C E G)

(defun mrember-curry (l)
  (multirember 'curry l))

(defun mrember-curry (l)
  (cond ((null l) '())
        ((eql (first l) 'curry) (mrember-curry (rest l)))
        (t (cons (first l) (mrember-curry (rest l)))) ))

(deftest test-mrember-curry ()
  (check
   (equal (mrember-curry '(a b c curry e curry g curry)) '(a b c e g))))

;; (defun curry-maker ()
;;   #'(lambda (l)
;;       (cond ((null l) '())
;;             ((eql (first l) 'curry) (funcall (curry-maker) (rest l)))
;;             (t (cons (first l) (funcall (curry-maker) (rest l)))) )))

;; (funcall (curry-maker) '(a b c curry e curry g curry)) => (A B C E G)

;;;
;;;    FUTURE is just a dummy value.
;;;    CURRY-MAKER returns a function which is a closure over that value, so that it is captured in the recursive calls.
;;;    
(defun curry-maker (future)
  #'(lambda (l)
      (cond ((null l) '())
            ((eql (first l) 'curry) (funcall (curry-maker future) (rest l)))
            (t (cons (first l) (funcall (curry-maker future) (rest l)))) )))

(defun curry-maker (future)
  (declare (ignore future)) ; !!
  (labels ((f (l)
             (cond ((null l) '())
                   ((eql (first l) 'curry) (f (rest l)))
                   (t (cons (first l) (f (rest l)))) )))
    #'f))

(setf (symbol-function 'mrember-curry) (curry-maker 0))
(setf (symbol-function 'mrember-curry) (curry-maker #'curry-maker))

(defun function-maker (future)
  #'(lambda (l)
      (cond ((null l) '())
            ((eql (first l) 'curry) (funcall (funcall future future) (rest l)))
            (t (cons (first l) (funcall (funcall future future) (rest l)))) )))

(setf (symbol-function 'mrember-curry) (function-maker #'function-maker))

(setf (symbol-function 'mrember-curry) ((lambda (future)
                                          #'(lambda (l)
                                              (cond ((null l) '())
                                                    ((eql (first l) 'curry) (funcall (funcall future future) (rest l)))
                                                    (t (cons (first l) (funcall (funcall future future) (rest l)))) )))
                                        #'(lambda (future)
                                            #'(lambda (l)
                                                (cond ((null l) '())
                                                      ((eql (first l) 'curry) (funcall (funcall future future) (rest l)))
                                                      (t (cons (first l) (funcall (funcall future future) (rest l)))) )))) )

(funcall ((lambda (future)
            #'(lambda (l)
                (cond ((null l) '())
                      ((eql (first l) 'curry) (funcall (funcall future future) (rest l)))
                      (t (cons (first l) (funcall (funcall future future) (rest l)))) )))
          #'(lambda (future)
              #'(lambda (l)
                  (cond ((null l) '())
                        ((eql (first l) 'curry) (funcall (funcall future future) (rest l)))
                        (t (cons (first l) (funcall (funcall future future) (rest l)))) ))))
         '(a b c curry e curry g curry))

;;;
;;;    (f x) -> ((lambda (x) (f x)) x)
;;;    
(defun function-maker (future)
  #'(lambda (l)
      (cond ((null l) '())
            ((eql (first l) 'curry) ((lambda (arg) (funcall (funcall future future) arg)) (rest l)))
            (t (cons (first l) ((lambda (arg) (funcall (funcall future future) arg)) (rest l)))) )))

(defun function-maker (future)
  ((lambda (recur)
     #'(lambda (l)
         (cond ((null l) '())
               ((eql (first l) 'curry) (funcall recur (rest l)))
               (t (cons (first l) (funcall recur (rest l)))) )))
   #'(lambda (arg)
       (funcall (funcall future future) arg))))

(defun function-maker (future)
  (helper #'(lambda (arg)
              (funcall (funcall future future) arg))))

(defun helper (recur)
  #'(lambda (l)
      (cond ((null l) '())
            ((eql (first l) 'curry) (funcall recur (rest l)))
            (t (cons (first l) (funcall recur (rest l)))) )))

(setf (symbol-function 'mrember-curry) ((lambda (future)
                                          (helper #'(lambda (arg)
                                                      (funcall (funcall future future) arg))))
                                        #'(lambda (future)
                                            (helper #'(lambda (arg)
                                                        (funcall (funcall future future) arg)))) ))

(setf (symbol-function 'mrember-curry) ((lambda (future)
                                          ((lambda (helper)
                                             (funcall helper #'(lambda (arg)
                                                                 (funcall (funcall future future) arg)))) #'helper))
                                        ((lambda (helper)
                                           #'(lambda (future)
                                               (funcall helper #'(lambda (arg)
                                                                   (funcall (funcall future future) arg)))) ) #'helper)))
(defun y (m)
  ((lambda (future)
     (funcall m #'(lambda (arg)
                    (funcall (funcall future future) arg))))
   #'(lambda (future)
       (funcall m #'(lambda (arg)
                      (funcall (funcall future future) arg)))) ))

(setf (symbol-function 'mrember-curry) (y #'helper))

(setf (symbol-function 'length) (y #'(lambda (recur) #'(lambda (l) (cond ((null l) 0) (t (1+ (funcall recur (rest l)))) )))) )

(defun l (recur)
  #'(lambda (l)
      (cond ((null l) 0)
            (t (1+ (funcall recur (rest l)))) )))

(setf (symbol-function 'length) (y #'l))

(setf (symbol-function 'length) ((lambda (m)
                                   ((lambda (future)
                                      (funcall m #'(lambda (arg)
                                                     (funcall (funcall future future) arg))))
                                    #'(lambda (future)
                                        (funcall m #'(lambda (arg)
                                                       (funcall (funcall future future) arg)))) ))
                                 #'(lambda (recur) #'(lambda (l) (cond ((null l) 0) (t (1+ (funcall recur (rest l)))) )))) )

;;;
;;;    This is a beast that used to be called LENGTH.
;;;    
(funcall ((lambda (m)
            ((lambda (future)
               (funcall m #'(lambda (arg)
                              (funcall (funcall future future) arg))))
             #'(lambda (future)
                 (funcall m #'(lambda (arg)
                                (funcall (funcall future future) arg)))) ))
          #'(lambda (recur) #'(lambda (l) (cond ((null l) 0) (t (1+ (funcall recur (rest l)))) ))))
         '(a b c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    Derive Y again from a different starting point.
;;;    
(defun + (n m)
  (cond ((zerop n) m)
        (t (+ (1- n) (1+ m)))) )

;;;
;;;    Need modified version that doesn't rely on 2nd arg...
;;;
(defun + (n m)
  (cond ((zerop n) m)
        (t (1+ (+ (1- n) m)))) )

;;;
;;;    Partial application - Simpler with function of one variable.
;;;
(defun plus5 (n)
  (+ n 5))

(deftest test-plus5 ()
  (check
   (= (plus5 8) 13)
   (= (plus5 0) 5)))

;;;
;;;    Redefine recursive general function as recursive partial application (one arg)
;;;    The alternative is to curry the original function???
;;;    
(defun plus5 (n)
  (cond ((zerop n) 5)
        (t (1+ (plus5 (1- n)))) ))

;;;
;;;    Sever recursive reference (Of course this doesn't work "as is".)
;;;
(defun plus5 (n f)
  (cond ((zerop n) 5)
        (t (1+ (funcall f (1- n)))) ))

;;;    Inside out?? See PLUS below
(defun plus5 (n)
  #'(lambda (f)
      (cond ((zerop n) 5)
            (t (1+ (funcall f (1- n)))) )))

;;;
;;;    Dummy FUTURE arg...
;;;    
(defun plus-maker (future)
  #'(lambda (n)
      (cond ((zerop n) 5)
            (t (1+ (funcall (plus-maker future) (1- n)))) )))

(setf (symbol-function 'plus5) (plus-maker #'plus-maker))

;;;
;;;    Abstract out reference to factory function
;;;
(defun plus-maker (future)
  #'(lambda (n)
      (cond ((zerop n) 5)
            (t (1+ (funcall (funcall future future) (1- n)))) )))

(setf (symbol-function 'plus5) (plus-maker #'plus-maker))

;;;
;;;    Prepare to separate the general factory from the specific function.
;;;
(defun plus-maker (future)
  #'(lambda (n)
      (cond ((zerop n) 5)
            (t (1+ ((lambda (arg) (funcall (funcall future future) arg)) (1- n)))) )))

;;;
;;;    Initial split
;;;
(defun plus-maker (future)
  ((lambda (recur)
     #'(lambda (n)               ; <--- This function is the value returned by PLUS-MAKER. It is a closure over RECUR, which is a closure over FUTURE.
         (cond ((zerop n) 5)
               (t (1+ (funcall recur (1- n)))) )))
   #'(lambda (arg)
       (funcall (funcall future future) arg))))

(setf (symbol-function 'plus5) (plus-maker #'plus-maker))

;;;
;;;    Full split, but dependency still exists.
;;;
(defun plus-maker (future)
  (plus #'(lambda (arg) (funcall (funcall future future) arg))))

(defun plus (recur)
  #'(lambda (n)    ; <--- Value returned by PLUS-MAKER
      (cond ((zerop n) 5)
            (t (1+ (funcall recur (1- n)))) )))

(setf (symbol-function 'plus5) (plus-maker #'plus-maker))

;;;
;;;    Final refactoring of PLUS-MAKER. Takes itself as FUTURE arg.
;;;    Still hard-wired with helper function.
;;;
(defun plus-maker ()
  ((lambda (future)
     (plus #'(lambda (arg) (funcall (funcall future future) arg))))
   #'(lambda (future)
       (plus #'(lambda (arg) (funcall (funcall future future) arg)))) ))

(setf (symbol-function 'plus5) (plus-maker))

;;;
;;;    Y is abstracted from PLUS-MAKER. Helper function is passed as arg.
;;;    
(defun y (m)
  ((lambda (future)
     (funcall m #'(lambda (arg) ; Value returned by Y is closure created by M. Captures values of M, FUTURE, and 2nd arg passed to M as RECUR.
                    (funcall (funcall future future) arg)))) ; Captures function below as FUTURE.
   #'(lambda (future)           ; Captures M
       (funcall m #'(lambda (arg)
                      (funcall (funcall future future) arg)))) ))

(setf (symbol-function 'plus5) (y #'plus))

;;;
;;;    Parameterized
;;;
(defun plus-x (x recur)
  #'(lambda (n)
      (cond ((zerop n) x)
            (t (1+ (funcall recur (1- n)))) )))

(setf (symbol-function 'plus9) (y #'(lambda (recur) (plus-x 9 recur))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    Derive Y again from a different starting point. One more time.
;;;    
(defun collatz (n)
  (cond ((= n 1) t)
        ((evenp n) (collatz (/ n 2)))
        (t (collatz (+ (* 3 n) 1)))) )

(deftest test-collatz ()
  (check
   (collatz 10)
   (collatz 9)
   (collatz 150)))

;;;
;;;    COLLATZ is already function of one arg.
;;;    Sever recursive reference.
;;;
(defun collatz (n f)
  (cond ((= n 1) t)
        ((evenp n) (funcall f (/ n 2)))
        (t (funcall f (+ (* 3 n) 1)))) )

;;;
;;;    Dummy FUTURE arg...
;;;
(defun collatz-maker (future)
  #'(lambda (n)
      (cond ((= n 1) t)
            ((evenp n) (funcall (collatz-maker future) (/ n 2)))
            (t (funcall (collatz-maker future) (+ (* 3 n) 1)))) ))

(setf (symbol-function 'collatz) (collatz-maker 'foo))

;;;
;;;    Abstract out reference to factory function
;;;
(defun collatz-maker (future)
  #'(lambda (n)
      (cond ((= n 1) t)
            ((evenp n) (funcall (funcall future future) (/ n 2)))
            (t (funcall (funcall future future) (+ (* 3 n) 1)))) ))

(setf (symbol-function 'collatz) (collatz-maker #'collatz-maker))

;;;
;;;    Prepare to separate general/specific
;;;
(defun collatz-maker (future)
  #'(lambda (n)
      (cond ((= n 1) t)
            ((evenp n) ((lambda (arg) (funcall (funcall future future) arg)) (/ n 2)))
            (t ((lambda (arg) (funcall (funcall future future) arg)) (+ (* 3 n) 1)))) ))

(setf (symbol-function 'collatz) (collatz-maker #'collatz-maker))

;;;
;;;    Initial split
;;;
(defun collatz-maker (future)
  ((lambda (recur)
     #'(lambda (n)
         (cond ((= n 1) t)
               ((evenp n) (funcall recur (/ n 2)))
               (t (funcall recur (+ (* 3 n) 1)))) ))
   #'(lambda (arg) (funcall (funcall future future) arg))))

(setf (symbol-function 'collatz) (collatz-maker #'collatz-maker))

;;;
;;;    Full split, with dependency
;;;
(defun collatz-maker (future)
  (collatz-m #'(lambda (arg) (funcall (funcall future future) arg))))

(defun collatz-m (recur)
  #'(lambda (n)
      (cond ((= n 1) t)
            ((evenp n) (funcall recur (/ n 2)))
            (t (funcall recur (+ (* 3 n) 1)))) ))

(setf (symbol-function 'collatz) (collatz-maker #'collatz-maker))

;;;
;;;    Final refactoring
;;;
(defun collatz-maker ()
  ((lambda (future)
     (collatz-m #'(lambda (arg) (funcall (funcall future future) arg))))
   #'(lambda (future)
       (collatz-m #'(lambda (arg) (funcall (funcall future future) arg)))) ))

(setf (symbol-function 'collatz) (collatz-maker))

(setf (symbol-function 'collatz) (y #'collatz-m))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    Derive Y again from a different starting point. Last time.
;;;

;;;
;;;    Basic MEMBER. No TEST keyword...
;;;    
(defun member (a lat)
  (cond ((null lat) nil)
        ((eql a (first lat)) lat) ; This is the correct order of args to EQL. Doesn't matter for equality, but it does for other tests. See below #'<
        (t (member a (rest lat)))) )

;;;
;;;    Partial application
;;;
(defun member-pung (lat)
  (cond ((null lat) nil)
        ((eql 'pung (first lat)) lat)
        (t (member-pung (rest lat)))) )

(deftest test-member-pung ()
  (check
   (equal (member-pung '(is this not pung)) '(pung))
   (not (member-pung '(foo bar baz)))
   (equal (member-pung '(foo pung baz bar)) '(pung baz bar))))

;;;
;;;    Dummy FUTURE
;;;
(defun member-maker (future)
  #'(lambda (lat)
      (cond ((null lat) nil)
            ((eql 'pung (first lat)) lat)
            (t (funcall (member-maker future) (rest lat)))) ))

(setf (symbol-function 'member-pung) (member-maker :baz))

;;;
;;;    Abstract out factory function
;;;
(defun member-maker (future)
  #'(lambda (lat)
      (cond ((null lat) nil)
            ((eql 'pung (first lat)) lat)
            (t (funcall (funcall future future) (rest lat)))) ))

(setf (symbol-function 'member-pung) (member-maker #'member-maker))

;;;
;;;    Prepare to separate
;;;
(defun member-maker (future)
  #'(lambda (lat)
      (cond ((null lat) nil)
            ((eql 'pung (first lat)) lat)
            (t ((lambda (arg) (funcall (funcall future future) arg)) (rest lat)))) ))

(setf (symbol-function 'member-pung) (member-maker #'member-maker))

;;;
;;;    Initial split
;;;
(defun member-maker (future)
  ((lambda (recur)
     #'(lambda (lat)
         (cond ((null lat) nil)
               ((eql 'pung (first lat)) lat)
               (t (funcall recur (rest lat)))) ))
   #'(lambda (arg)
       (funcall (funcall future future) arg))))

(setf (symbol-function 'member-pung) (member-maker #'member-maker))

;;;
;;;    Full split (Dependency still)
;;;
(defun member-maker (future)
  (member-m #'(lambda (arg) (funcall (funcall future future) arg))))

(defun member-m (recur)
  #'(lambda (lat)
      (cond ((null lat) nil)
            ((eql 'pung (first lat)) lat)
            (t (funcall recur (rest lat)))) ))

(setf (symbol-function 'member-pung) (member-maker #'member-maker))

;;;
;;;    Final refactoring
;;;
(defun member-maker ()
  ((lambda (future)
     (member-m #'(lambda (arg) (funcall (funcall future future) arg))))
   #'(lambda (future)
       (member-m #'(lambda (arg) (funcall (funcall future future) arg)))) ))

(setf (symbol-function 'member-pung) (member-maker))

(setf (symbol-function 'member-pung) (y #'member-m))

;;;
;;;    Parameterized
;;;    
(defun member-x (target recur &key (test #'eql))
  #'(lambda (lat)
      (cond ((null lat) nil)
            ((funcall test target (first lat)) lat)
            (t (funcall recur (rest lat)))) ))

(setf (symbol-function 'member-foo) (y #'(lambda (recur) (member-x 'foo recur))))
(setf (symbol-function 'member-pung-list) (y #'(lambda (recur) (member-x '(pung) recur :test #'equal))))

(deftest test-y-combinator ()
  (check
   (equal (funcall ((lambda (m)
                      ((lambda (future)
                         (funcall m #'(lambda (arg)
                                        (funcall (funcall future future) arg))))
                       #'(lambda (future)
                           (funcall m #'(lambda (arg)
                                          (funcall (funcall future future) arg)))) ))
                    #'(lambda (recur)
                        (funcall #'(lambda (target recur &key (test #'eql))
                                     #'(lambda (lat)
                                         (cond ((null lat) nil)
                                               ((funcall test target (first lat)) lat)
                                               (t (funcall recur (rest lat)))) ))
                                 '(pung) recur :test #'equal)))
                   '(a b (pung) d))
          (cl:member '(pung) '(a b (pung) d) :test #'equal))
   (equal (funcall ((lambda (m)
                      ((lambda (future)
                         (funcall m #'(lambda (arg)
                                        (funcall (funcall future future) arg))))
                       #'(lambda (future)
                           (funcall m #'(lambda (arg)
                                          (funcall (funcall future future) arg)))) ))
                    #'(lambda (recur)
                        (funcall #'(lambda (target recur &key (test #'eql))
                                     #'(lambda (lat)
                                         (cond ((null lat) nil)
                                               ((funcall test target (first lat)) lat)
                                               (t (funcall recur (rest lat)))) ))
                                 3 recur :test #'<)))
                   '(1 2 3 4 5))
          (cl:member 3 '(1 2 3 4 5) :test #'<))))

;;;
;;;    Chapter 10
;;;

(defpackage :interpreter
  (:use :common-lisp :lang :test)
  (:shadow :apply :cons :car :cdr))

(in-package :interpreter)

(defconstant primitive :primitive)
(defconstant non-primitive :non-primitive)

(setf (symbol-function 'new-entry) #'little:build)

(deftest test-new-entry ()
  (check
   (equal (new-entry '(appetizer entrée beverage) '(paté boeuf vin)) '((appetizer entrée beverage) (paté boeuf vin)))
   (equal (new-entry '(beverage dessert) '((food is) (number one with us))) '((beverage dessert) ((food is) (number one with us)))) ))

(defun lookup-in-entry (name entry entry-f)
  (lookup-in-entry-aux name (first entry) (second entry) entry-f))

(defun lookup-in-entry-aux (name names values entry-f)
  (cond ((null names) (funcall entry-f name))
        ((eql (first names) name) (first values))
        (t (lookup-in-entry-aux name (rest names) (rest values) entry-f))))

(deftest test-lookup-in-entry ()
  (check
   (eql (lookup-in-entry 'entrée '((appetizer entrée beverage) (food tastes good)) #'print) 'tastes)))

(setf (symbol-function 'extend-table) #'cl:cons)

(defun lookup-in-table (name table table-f)
  (cond ((null table) (funcall table-f name))
        (t (lookup-in-entry name (first table) #'(lambda (name) (lookup-in-table name (rest table) table-f)))) ))

(deftest test-lookup-in-table ()
  (check
   (eql (lookup-in-table 'entrée '(((entrée dessert) (spaghetti spumoni)) ((appetizer entrée beverage) (food tastes good))) #'print) 'spaghetti)
   (eql (lookup-in-table 'beverage '(((entrée dessert) (spaghetti spumoni)) ((appetizer entrée beverage) (food tastes good))) #'print) 'good)))

(defun expression-to-action (e)
  (cond ((atom e) (atom-to-action e))
        (t (list-to-action e))))

(defun atom-to-action (e)
  (cond ((numberp e) #'*self-evaluating)
        (t #'*identifier)))

(defun list-to-action (e)
  (cond ((atom (first e))
         (cond ((eql (first e) 'quote) #'*quote)
               ((eql (first e) 'lambda) #'*lambda)
               ((eql (first e) 'cond) #'*cond)
               (t #'*application)))
        (t #'*application)))

(defun list-to-action (e)
  (cond ((atom (first e))
         (case (first e)
           (quote #'*quote)
           (lambda #'*lambda)
           (cond #'*cond)
           (otherwise #'*application)))
        (t #'*application)))

(defun value (e)
  (meaning e '()))

(defun meaning (e table)
  (funcall (expression-to-action e) e table))

(deftest test-meaning ()
  (check
   (eql (meaning 'entrée '(((entrée dessert) (spaghetti spumoni)) ((appetizer entrée beverage) (food tastes good)))) 'spaghetti)
   (equal (meaning '(lambda (x) (cons x y)) '(((y z) ((8) 9)))) '(:NON-PRIMITIVE ((((Y Z) ((8) 9))) (X) (CONS X Y)))) ))
   
(defun *self-evaluating (e table)
  (declare (ignore table))
  e)

(defun *quote (e table)
  (declare (ignore table))
  (text-of-quotation e))

(setf (symbol-function 'text-of-quotation) #'second)

(defun *identifier (e table)
  (lookup-in-table e table #'(lambda (name)
                               (case name
                                 ((t) t)
                                 ((nil) nil)
                                 (otherwise (little:build primitive name)))) ))

;;;
;;;    Builds a closure: ordered triple of environment, params, body
;;;    ((lambda (x y) (cons x y)) 1 '(2)) => (:non-primitive (((x y) (1 (2))) (x y) (cons x y)))
;;;    
(defun *lambda (e table)
  (little:build non-primitive (cl:cons table (rest e))))

(setf (symbol-function 'table-of) #'first)
(setf (symbol-function 'formals-of) #'second)
(setf (symbol-function 'body-of) #'third)

;;;
;;;    Uses COND to define COND!
;;;    Assumes that at least final QUESTION will evaluate true! (Violates First Commandment)
;;;    
(defun evcon (lines table)
  "Evaluate COND form."
  (cond ((meaning (question-of (first lines)) table) (meaning (answer-of (first lines)) table))
        (t (evcon (rest lines) table))))

(setf (symbol-function 'question-of) #'first)
(setf (symbol-function 'answer-of) #'second)

(defun *cond (e table)
  (evcon (cond-lines e) table))

(setf (symbol-function 'cond-lines) #'rest)

;(*cond '(cond (coffee klatsch) (t party)) '(((coffee) (t)) ((klatsch party) (5 (6))))) => 5

(defun evlis (args table)
  (cond ((null args) '())
        (t (cl:cons (meaning (first args) table) (evlis (rest args) table)))) )

(deftest test-evlis ()
  (check
   (equal (evlis '(x y) '(((x y) (1 (2)))) ) '(1 (2)))
   (equal (evlis '(z x) '(((x y) ((a b c) (d e f))) ((u v w) (1 2 3)) ((x y z) (4 5 6)))) '(6 (a b c)))) )

(defun *application (e table)
  (apply (meaning (function-of e) table)
         (evlis (arguments-of e) table)))

(setf (symbol-function 'function-of) #'first)
(setf (symbol-function 'arguments-of) #'rest)

(defun primitivep (f)
  (eql (first f) primitive))

(defun non-primitive-p (f)
  (eql (first f) non-primitive))

(defun apply (f args)
  (cond ((primitivep f) (apply-primitive (second f) args))
        ((non-primitive-p f) (apply-closure (second f) args))))

(defun apply-primitive (name args)
  (ecase name
    ((first car) (first (first args))) ; Self-reference
    ((rest cdr) (rest (first args)))
    (cons (cl:cons (first args) (second args)))
    (eql (eql (first args) (second args)))
    (atom (atom (first args)))
    (not (not (first args)))
    (null (null (first args)))
    (numberp (numberp (first args)))
    (zerop (zerop (first args)))
    (1+ (1+ (first args)))
    (1- (1- (first args)))) )

(defun apply-closure (closure args)
  (meaning (body-of closure) (extend-table (new-entry (formals-of closure) args) (table-of closure))))

(deftest test-apply-closure ()
  (check
   (equal (apply-closure '((((u v w) (1 2 3)) ((x y z) (4 5 6))) (x y) (cons z x)) '((a b c) (d e f))) '(6 a b c))))

;;;
;;;    Radical refactoring
;;;
(defun cons (u v)
  #'(lambda (b)
      (cond (b u)
            (t v))))

(defun car (l)
  (funcall l t))

(defun cdr (l)
  (funcall l nil))

(defvar *lunch* (cons 'apple '()))

;;;
;;;    Building up a "list" does not produce recognizable CDRs. But operationally CAR works as expected.
;;;    
;; (car *lunch*) => APPLE
;; (cdr *lunch*) => NIL
;; (cons 'banana *lunch*) => #<CLOSURE (LAMBDA (B) :IN CONS) {10026C861B}>
;; (car *) => BANANA
;; (cdr **) => #<CLOSURE (LAMBDA (B) :IN CONS) {100241F38B}>
;; (car (cdr ***)) => APPLE


;; (defun apply-primitive (name args)
;;   (ecase name
;;     (eql (eql (first args) (second args)))
;;     (atom (atom (first args)))) )

    ;; (not (not (first args)))
    ;; (null (null (first args)))
    ;; (numberp (numberp (first args)))
    ;; (zerop (zerop (first args)))
    ;; (1+ (1+ (first args)))
    ;; (1- (1- (first args)))) )

;;;
;;;    PERSON as closure rather than structure/object...
;;;    - Parameters as fields
;;;    - All of this could be bundled into a macro to define accessors.
;;;    
(defun defperson (name age)
  #'(lambda (command)
      (case command
        (name name)
        (age age))))

(defun age (person)
  (funcall person 'age))

(defun name (person)
  (funcall person 'name))
