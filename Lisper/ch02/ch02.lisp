;#!/opt/local/bin/sbcl.bin --core /opt/local/lib/sbcl/sbcl.core --noinform
;#!/usr/local/bin/sbcl --noinform
;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch02.lisp
;;;;
;;;;   Started:            Sun Jul 15 05:15:31 2007
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

(defpackage ch02
  (:use common-lisp test)
  (:shadow member))

(in-package ch02)

(defun latp (l)
  (cond ((endp l) t)
	((atom (first l)) (latp (rest l)))
	(t nil)))

(deftest test-latp ()
  (check
   (latp '(a b c))
   (latp '())
   (not (latp '(a b (c))))
   (latp '(jack sprat could eat no chicken fat))
   (not (latp '((jack) sprat could eat no chicken fat)))
   (not (latp '(jack (sprat could) eat no chicken fat)))
   (latp '(bacon and eggs))
   (not (latp '(bacon (and eggs)))) ))

(defun member (obj lat)
  (cond ((endp lat) nil)
	((eql (first lat) obj) t)
	(t (member obj (rest lat)))) )

(deftest test-member ()
  (check
   (member 'a '(b a c d))
   (not (member 'a '(b c d)))
   (not (member 'a '()))) )
