;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Of all the languages I know, I like Lisp the best, simply because it's the most beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               interpreter.lisp
;;;;
;;;;   Started:            Sun Jun 17 01:43:37 2012
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;   Ch. 10 of the Little Lisper
;;;;
;;;;   Notes:
;;;;
;;;;
(load "/Users/dsletten/lisp/packages/lang.lisp")
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :interpreter
  (:use :common-lisp :lang :test)
  (:shadow :apply))

(in-package :interpreter)

;;;
;;;    Entry: a pair of lists, the first of which is a set. An entry represents (part of?) an environment.
;;;    The first list is a set of variable (names), and the second is a list of corresponding values.
;;;
(defun make-entry (vars vals)
  (list vars vals))

;(defclass entry ())?

;Entry as hash-table, table as list of entries?

;;
;;    MISSING is a function that takes NAME as an arg and performs
;;    the correct action if NAME is not found in the entry.
;;    
(defun lookup-in-entry (name entry missing)
  (labels ((lookup-aux (vars vals)
             (cond ((endp vars) (funcall missing name))
                   ((eql (first vars) name) (first vals))
                   (t (lookup-aux (rest vars) (rest vals)))) ))
    (lookup-aux (first entry) (second entry))))

(defun make-entry (vars vals)
  (let ((entry (make-hash-table)))
    (loop for var in vars
          for val in vals
          do (setf (gethash var entry) val))
    entry))

(defun lookup-in-entry (name entry missing)
  (multiple-value-bind (value found) (gethash name entry)
    (if (null found)
        (funcall missing name)
        value)))

;;;
;;;    Table: a list of entries. An environment.
;;;
(defun extend-table (entry table)
  (cons entry table))

(defun lookup-in-table (name table missing)
  (cond ((endp table) (funcall missing name))
        (t (lookup-in-entry name
                            (first table)
                            #'(lambda (name)
                                (lookup-in-table name (rest table) missing)))) ))

;;;
;;;    Types are represented by functions, which are called "actions".
;;;    

;;
;;    An S-expression represents either an atom or a list.
;;
;; (defun expression-to-action (e)
;;   (if (atom e)
;;       (atom-to-action e)
;;       (list-to-action e)))

(defun expression-to-action (e)
  (etypecase e
    (atom (atom-to-action e))
    (list (list-to-action e))))

;;
;;    An atom is either self-evaluating or is an identifier (symbolic atom).
;;    
;; (defun atom-to-action (e)
;;   (if (numberp e)
;;       #'*self-evaluating
;;       #'*identifier))

(defun atom-to-action (e)
  (etypecase e
    (number #'*self-evaluating)
    (symbol #'*identifier)))

;;
;;    The CAR of a compound form is either an atom (function/macro/special operator)
;;    or a list (lambda expression).
;;    
;; (defun list-to-action (e)
;;   (cond ((atom (first e))
;;          (cond ((eql (first e) 'quote) #'*quote)
;;                ((eql (first e) 'lambda) #'*lambda)
;;                ((eql (first e) 'cond) #'*cond)
;;                (t #'*application)))
;;         (t #'*application)))

(defun list-to-action (e)
  (case (first e)
    (quote #'*quote)
    (lambda #'*lambda)
    (cond #'*cond)
    (otherwise #'*application))) ;?

;;
;;    Top-level interpreter function. Starts with empty table (null lexical environment).
;;
(defun value (e)
  (meaning e '()))

;;
;;    Determine the appropriate action for the expression E, then call this function
;;    with E and TABLE (the environment) as args.
;;    
(defun meaning (e table)
  (funcall (expression-to-action e) e table))

(defun *self-evaluating (e table)
  (declare (ignore table))
  e)

(defun *quote (e table)
  (declare (ignore table))
  (second e))

(defun *identifier (e table)
  (lookup-in-table e
                   table
                   #'(lambda (name)
                       (case name
                         ((t) t)
                         ((nil) nil)
                         (otherwise (list 'primitive name)))) ))

;;
;;    Creates a closure. Environment replaces LAMBDA in the list.
;;    
(defun *lambda (e table)
  (list 'non-primitive (cons table (rest e))))

(defun table (closure)
  (first closure))

(defun lambda-list (closure)
  (second closure))

(defun body (closure)
  (third closure))

;;
;;    Views COND as a sequence of question/answer pairs.
;;    (Violates "The First Commandment" (no initial test for empty LINES arg))
;;    
(defun eval-cond (lines table)
  (cond ((meaning (question (first lines)) table)
         (meaning (answer (first lines)) table))
        (t (eval-cond (rest lines) table))))

(defun question (line)
  (first line))

(defun answer (line)
  (second line))

(defun *cond (e table)
  (eval-cond (cond-lines e) table))

(defun cond-lines (e)
  (rest e))

;(meaning '(cond (coffee klatsch) (t party)) '(((coffee) (t)) ((klatsch party) (5 (6)))) )
;(meaning '(cond (coffee klatsch) (t party)) (list {'coffee t} {'klatsch 5 'party '(6)}))

;;
;;    Application: a list of expressions whose CAR position contains an expression whose value is a function.
;;

;;
;;    Maps list of representations of args to list of meaning of args in the context of a given environment.
;;    May result in recursive calls to MEANING.
;;
(defun eval-list (args table)
  (if (endp args)
      '()
      (cons (meaning (first args) table) (eval-list (rest args) table))))

(defun *application (e table)
  (apply (meaning (get-function e) table)
         (eval-list (args e) table)))

(defun get-function (e)
  (first e))

(defun args (e)
  (rest e))

(defun primitivep (l)
  (eql (first l) 'primitive))

(defun non-primitivep (l)
  (eql (first l) 'non-primitive))

(defun apply (f vals)
  (cond ((primitivep f) (apply-primitive (second f) vals))
        ((non-primitivep f) (apply-closure (second f) vals))))

(defun apply-primitive (name vals)
  (case name
    (car (car (first vals)))
    (cdr (cdr (first vals)))
    (cons (cons (first vals) (second vals)))
    (eql (eql (first vals) (second vals)))
    (atom (atom (first vals)))
    (not (not (first vals)))
    (null (null (first vals)))
    (numberp (numberp (first vals)))
    (zerop (zerop (first vals)))
    (+ (+ (first vals) (second vals)))
    (- (- (first vals) (second vals)))
    (* (* (first vals) (second vals)))
    (/ (/ (first vals) (second vals)))
    (1+ (1+ (first vals)))
    (1- (1- (first vals)))) )

(defun apply-closure (closure vals)
  (meaning (body closure)
           (extend-table (make-entry (lambda-list closure) vals)
                         (table closure))))

(deftest test-value ()
  (check
   (equal (value '((lambda (x y) (cons x y)) 2 '(3))) '(2 3))
   (equal (value '((lambda (x y) (car (cons x y))) 2 '(3))) 2)
   (equal (value '(eql (1- 1) 0)) T)
   (equal (value '((lambda (x) (+ (* 3 x) 4)) 9)) 31)
   (equal (value '(/ 10 2)) 5)
   (equal (value '((lambda (b) (cond (b 2) (t 3))) t)) 2)
   (equal (value '((lambda (b) (cond (b 2) (t 3))) nil)) 3)
   (equal (value '((lambda (b) (cond ((not b) 2) (t 3))) t)) 3)
   (equal (value '((lambda (b) (cond ((not b) 2) (t 3))) nil)) 2)
   (equal (value '((lambda (x y) (car (cons x y))) 2 '(3))) 2)))
