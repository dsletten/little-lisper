;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               WhyY.lisp
;;;
;;;   STARTED:            Mon Feb  4 16:23:44 2002
;;;   MODIFICATIONS:
;;;
;;;   PURPOSE:
;;;
;;;
;;;
;;;   CALLING SEQUENCE:
;;;
;;;
;;;   INPUTS:
;;;
;;;   OUTPUTS:
;;;
;;;   EXAMPLE:
;;;
;;;   NOTES:
;;;
;;;
#|
(defun anyoddp (l)
  (cond ((null l) nil)
	((oddp (car l)) t)
	(t (anyoddp (cdr l)))) )
;;;
;;;    VS.
;;;
(defun anyoddp (l)
  (cond ((null l) nil)
	((oddp (car l)) t)
	(t ((lambda (l)
	      (cond ((null l) nil)
		    ((oddp (car l)) t)
		    (t ((lambda (l)
			  (cond ((null l) nil)
				((oddp (car l)) t)
				(t ((lambda (l)
				      .
				      .
				      .)
				    (cdr l)))) )
			(cdr l)))) )
	    (cdr l)))) )
|#
(defun hukairs ()
  'hukairs)

(setf (symbol-function 'length0)
      ((lambda (length)
	 #'(lambda (l)
	     (if (null l)
		 0
		 (1+ (funcall length (cdr l)))) ))
       #'hukairs))

(setf (symbol-function 'length1)
      ((lambda (length)
	 #'(lambda (l)
	     (if (null l)
		 0
		 (1+ (funcall length (cdr l)))) ))
       ((lambda (length)
	  #'(lambda (l)
	      (if (null l)
		  0
		  (1+ (funcall length (cdr l)))) ))
	#'hukairs)))

(setf (symbol-function 'length2)
      ((lambda (length)
	 #'(lambda (l)
	     (if (null l)
		 0
		 (1+ (funcall length (cdr l)))) ))
       ((lambda (length)
	  #'(lambda (l)
	      (if (null l)
		  0
		  (1+ (funcall length (cdr l)))) ))
	((lambda (length)
	   #'(lambda (l)
	       (if (null l)
		   0
		   (1+ (funcall length (cdr l)))) ))
	 #'hukairs))))

(setf (symbol-function 'length0)
      ((lambda (mk-length)
	 (funcall mk-length #'hukairs))
       #'(lambda (length)
	   #'(lambda (l)
	       (if (null l)
		   0
		   (1+ (funcall length (cdr l)))) ))))

(setf (symbol-function 'length1)
      ((lambda (mk-length)
	 (funcall mk-length (funcall mk-length #'hukairs)))
       #'(lambda (length)
	   #'(lambda (l)
	       (if (null l)
		   0
		   (1+ (funcall length (cdr l)))) ))))

(setf (symbol-function 'length3)
      ((lambda (mk-length)
	 (funcall mk-length
		  (funcall mk-length
			   (funcall mk-length
				    (funcall mk-length #'hukairs)))) )
       #'(lambda (length)
	   #'(lambda (l)
	       (if (null l)
		   0
		   (1+ (funcall length (cdr l)))) ))))


(setf (symbol-function 'my-length)
      ((lambda (mk-length)
	 (funcall mk-length mk-length))
       #'(lambda (mk-length)
	   #'(lambda (l)
	       (if (null l)
		   0
		   (1+ (funcall (funcall mk-length mk-length)
				(cdr l)))) ))))


(setf (symbol-function 'length2)
      ((lambda (mk-length)
	 (funcall mk-length mk-length))
       #'(lambda (mk-length)
	   #'(lambda (l)
	       (if (null l)
		   0
		   (1+ (funcall (funcall mk-length
					 (funcall mk-length #'hukairs))
				(cdr l)))) ))))

(setf (symbol-function 'length2)
      ((lambda (mk-length)
	 (funcall mk-length mk-length))
       #'(lambda (mk-length)
	   #'(lambda (l)
	       (if (null l)
		   0
		   (1+ (funcall (funcall (funcall mk-length mk-length)
					 #'hukairs)
				(cdr l)))) ))))

(setf (symbol-function 'length2)
      #'(lambda (l)
	  (if (null l)
	      0
	      (1+ (funcall #'(lambda (l)
			       (if (null l)
				   0
				   (1+ (funcall #'(lambda (l)
						    (if (null l)
							0
							(1+ (funcall
							     (funcall #'hukairs
								      #'hukairs)
							     (cdr l)))) )
						(cdr l)))) )
			   (cdr l)))) ))


(setf (symbol-function 'length2)
      ((lambda (mk-length)
	 #'(lambda (l)
	     (if (null l)
		 0
		 (1+ (funcall (funcall mk-length mk-length)
			      (cdr l)))) ))
       #'(lambda (mk-length)
	   #'(lambda (l)
	       (if (null l)
		   0
		   (1+ (funcall (funcall mk-length #'hukairs)
				(cdr l)))) ))))
