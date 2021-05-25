;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               Stages.lisp
;;;
;;;   STARTED:            Fri Feb 15 19:45:22 2002
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

;;;
;;;    This file looks at the LENGTH functions developed in Felleisen's essay
;;;    'A Lecture on the Why of Y'. (This discussion closely mirrors the
;;;    development of the Y combinator in ch. 9 of 'The Little Schemer'
;;;    beginning on pg. 160.) The lecture develops various versions of a group
;;;    of LENGTH-i functions eventually leading to the Y combinator. Here we
;;;    analyze the development of these LENGTH-i functions.
;;;
;;;    The base version of each (length0, length1, ...) is version 0. This
;;;    version is a literal specification of the function:
;;;
;;;    length0
;;;    
#'(lambda (l)
    (if (null l)
	0
	(1+ (hukairs (cdr l)))) )

;;;
;;;    length1
;;;
#'(lambda (l)
    (if (null l)
	0
	(1+ ((lambda (l)                    ;<--
	       (if (null l)                 ;  | length0
		   0                        ;  |
		   (1+ (hukairs (cdr l)))) );<--
	     (cdr l)))) )

;;;
;;;    length2
;;;
#'(lambda (l)
    (if (null l)
	0
	(1+ ((lambda (l)
	       (if (null l)
		   0
		   (1+ ((lambda (l)
			  (if (null l)
			      0
			      (1+ (hukairs (cdr l)))) )
			(cdr l)))) )
	     (cdr l)))) )

;;;
;;;    Version I is the next version of these functions. At each step it
;;;    represents the literal definition of the previous function passed as an
;;;    argument to a function-making function which generates the successor.
;;;    Basically the version 0 function is wrapped in lambda.
;;;
;;;    length0
;;;
((lambda (length)
   #'(lambda (l)
       (if (null l)
	   0
	   (1+ (funcall length (cdr l)))) ))
 #'hukairs)

; This yields: (Essentially length0 Version 0)
; #'(lambda (l)
;     (if (null l)
; 	0
; 	(1+ (funcall #'hukairs (cdr l)))) )

;;;
;;;    length1
;;;
((lambda (length)                            ;<--
   #'(lambda (l)                             ;  |  Same as length0.
       (if (null l)                          ;  |  Only arg below differs.
	   0                                 ;  |
	   (1+ (funcall length (cdr l)))) )) ;<--
 ((lambda (length)                            ;<--
    #'(lambda (l)                             ;  |  This produces length0
	(if (null l)                          ;  |  as an arg to the
	    0                                 ;  |  function-making function
	    (1+ (funcall length (cdr l)))) )) ;  |  above.
  #'hukairs))                                 ;<--

;This yields: (Essentialy length1 Version 0)
; #'(lambda (l)
;     (if (null l)
; 	0
; 	(1+ (funcall #'(lambda (l)
; 			 (if (null l)
; 			     0
; 			     (1+ (funcall #'hukairs (cdr l)))) )
; 		     (cdr l)))) )

;;;
;;;    length2
;;;
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
   #'hukairs)))

;;;
;;;    Version II of the functions eliminates much of the redundancy of the
;;;    Version I functions. Here the function-making function is itself passed
;;;    as an argument and applied repeatedly via a lambda wrapper.
;;;
;;;    length0
;;;
((lambda (mk-length)
   (funcall mk-length #'hukairs))
 #'(lambda (length)
     #'(lambda (l)
	 (if (null l)
	     0
	     (1+ (funcall length (cdr l)))) )))

;This yields:
; (funcall #'(lambda (length)
; 	     #'(lambda (l)
; 		 (if (null l)
; 		     0
; 		     (1+ (funcall length (cdr l)))) ))
; 	 #'hukairs)
;Which in turn yields:
; #'(lambda (l)
;     (if (null l)
; 	0
; 	(1+ (funcall #'hukairs (cdr l)))) )

;;;
;;;    length1
;;;
((lambda (mk-length)
   (funcall mk-length
	    (funcall mk-length #'hukairs)))
 #'(lambda (length)
     #'(lambda (l)
	 (if (null l)
	     0
	     (1+ (funcall length (cdr l)))) )))

;This yields:
; (funcall #'(lambda (length)
; 	     #'(lambda (l)
; 		 (if (null l)
; 		     0
; 		     (1+ (funcall length (cdr l)))) ))
; 	 (funcall #'(lambda (length)
; 		      #'(lambda (l)
; 			  (if (null l)
; 			      0
; 			      (1+ (funcall length (cdr l)))) ))
; 		  #'hukairs))
;Which results in:
; #'(lambda (l)
;     (if (null l)
; 	0
; 	(1+ (funcall #'(lambda (l)
; 			 (if (null l)
; 			     0
; 			     (1+ (funcall #'hukairs (cdr l)))) )
; 		     (cdr l)))) )

;;;
;;;    length2
;;;
((lambda (mk-length)
   (funcall mk-length
	    (funcall mk-length
		     (funcall mk-length #'hukairs))))
 #'(lambda (length)
     #'(lambda (l)
	 (if (null l)
	     0
	     (1+ (funcall length (cdr l)))) )))

;;;
;;;    Let L be the function-making function:
;;; #'(lambda (length)
;;;     #'(lambda (l)
;;; 	(if (null l)
;;; 	    0
;;; 	    (1+ (funcall length (cdr l)))) ))
;;;    Then the version III functions pass L (or a modified version of L)
;;;    to the wrapper and apply it to itself.
;;;
;;;    length0
;;;
((lambda (mk-length)
   (funcall mk-length mk-length)) ; The second MK-LENGTH here is the only
				  ; difference between this and version II.
 #'(lambda (mk-length) ; Version II calls this parameter LENGTH.
     #'(lambda (l)
	 (if (null l)
	     0
	     (1+ (funcall mk-length (cdr l)))) )))

;This yields:
; (funcall #'(lambda (mk-length)
; 	     #'(lambda (l)
; 		 (if (null l)
; 		     0
; 		     (1+ (funcall mk-length (cdr l)))) ))
; 	 #'(lambda (mk-length)
; 	     #'(lambda (l)
; 		 (if (null l)
; 		     0
; 		     (1+ (funcall mk-length (cdr l)))) )))
;Which yields:
; #'(lambda (l)
;     (if (null l)
; 	0
; 	(1+ (funcall #'(lambda (mk-length)
; 			 #'(lambda (l)
; 			     (if (null l)
; 				 0
; 				 (1+ (funcall mk-length (cdr l)))) ))
; 		     (cdr l)))) )
;Note that if the original arg to this function is not (), then the function
;reaches a dead end. FUNCALL applies (CDR L) as the arg to the LAMBDA
;expression, which results in (FUNCALL (CDR L) (CDR L)).

;;;
;;;    length1
;;;
;;;
;;;    We could describe an intermediate Version IIa for length1 based on
;;;    Version III length0 above. The MK-LENGTH parameter is applied to itself
;;;    rather than to HUKAIRS.
;;;
((lambda (mk-length)
   (funcall mk-length
	    (funcall mk-length mk-length))) ; <-- Differs from version II here.
 #'(lambda (mk-length)
     #'(lambda (l)
	 (if (null l)
	     0
	     (1+ (funcall mk-length (cdr l)))) )))

;This expands to:
; (funcall #'(lambda (mk-length)
; 	     #'(lambda (l)
; 		 (if (null l)
; 		     0
; 		     (1+ (funcall mk-length (cdr l)))) ))
; 	 #'(lambda (l)
; 	     (if (null l)
; 		 0
; 		 (1+ (funcall #'(lambda (mk-length)
; 				  #'(lambda (l)
; 				      (if (null l)
; 					  0
; 					  (1+ (funcall mk-length (cdr l)))) ))
; 			      (cdr l)))) ))
;
;Which yields:
; #'(lambda (l)
;     (if (null l)
; 	0
; 	(1+ (funcall #'(lambda (l)
; 			 (if (null l)
; 			     0
; 			     (1+ (funcall #'(lambda (mk-length)
; 					      #'(lambda (l)
; 						  (if (null l)
; 						      0
; 						      (1+ (funcall mk-length
; 								   (cdr l))))))
; 					  (cdr l)))) )
; 		     (cdr l)))) )
; This behaves the same as Version III below; they both blow up if the list
; has more than 1 element.

;;;
;;;    Here is Version III of length1.
;;;    
((lambda (mk-length)
   (funcall mk-length mk-length))
 #'(lambda (mk-length)
     #'(lambda (l)
	 (if (null l)
	     0
	     (1+ (funcall (funcall mk-length #'hukairs) ; This differs from a
							; working LENGTH only
							; with the HUKAIRS
							; here.
			  (cdr l)))) )))

;This yields:
; #'(lambda (l)
;     (if (null l)
; 	0
; 	(1+ (funcall (funcall #'(lambda (mk-length)
; 				  #'(lambda (l)
; 				      (if (null l)
; 					  0
; 					  (1+ (funcall (funcall mk-length
; 								#'hukairs)
; 						       (cdr l)))) ))
; 			      #'hukairs)
; 		     (cdr l)))) )
;Which results in:
; #'(lambda (l)
;     (if (null l)
; 	0
; 	(1+ (funcall #'(lambda (l)       ; This is a dead end here:
; 			 (if (null l)    ; ___________________________
; 			     0           ; v                         v
; 			     (1+ (funcall (funcall #'hukairs #'hukairs)
; 					  (cdr l)))) )
; 		     (cdr l)))) )

;;;
;;;    The question now arises how to define version III of length2. I have
;;;    tried several options and have not been able to find one that works.
;;;    The failures are summarized below.
;;;
;;;    First I tried modifying the function-making function passed as an
;;;    argument. This is where length0 and length1 differ, so it seemed like
;;;    the correct strategy:
;;;
;;;    length0--
;;;    	     (1+ (funcall mk-length (cdr l)))) )))
;;;    length1--
;;;    	     (1+ (funcall (funcall mk-length #'hukairs) (cdr l)))) )))
;;;
;;;    There appeared to be two obvious choices:
;;;    Either--
;;;    	     (1+ (funcall (funcall mk-length
;;;                                (funcall mk-length #'hukairs))
;;;                       (cdr l)))) )))
;;;    or--
;;;    	     (1+ (funcall (funcall (funcall mk-length #'hukairs)
;;;                                #'hukairs)
;;;                       (cdr l)))) )))
;;;    Unfortunately, modifying the function-making function like this won't
;;;    work since it is applied to itself later. The first version expands to
;;;    this:
; #'(lambda (l)
;     (if (null l)
; 	0
; 	(1+ (funcall (funcall #'(lambda (mk-length)
; 				  #'(lambda (l)
; 				      (if (null l)
; 					  0
; 					  (1+ (funcall (funcall mk-length
; 								(funcall
; 								 mk-length
; 								 #'hukairs))
; 						       (cdr l)))) ))
; 			      (funcall
; 			       #'(lambda (mk-length)
; 				   #'(lambda (l)
; 				       (if (null l)
; 					   0
; 					   (1+ (funcall (funcall mk-length
; 								 (funcall
; 								  mk-length
; 								  #'hukairs))
; 							(cdr l)))) ))
; 			       #'hukairs))
; 		     (cdr l)))) )
;;;    This causes HUKAIRS to be applied to itself for lists of length 2 or
;;;    more. Consequently, this does not work as LENGTH2.
;;;
;;;    The second version results in this:
; #'(lambda (l)
;     (if (null l)
; 	0
; 	(1+ (funcall (funcall (funcall #'(lambda (mk-length)
; 					   #'(lambda (l)
; 					       (if (null l)
; 						   0
; 						   (1+ (funcall
; 							(funcall
; 							 (funcall mk-length
; 								  #'hukairs)
; 							 #'hukairs)
; 							(cdr l)))) ))
; 				       #'hukairs)
; 			      #'hukairs)
; 		     (cdr l)))) )
;;;    Once again, for lists of length 2 or more this causes HUKAIRS to be
;;;    applied to itself and fails.
;;;
;;;    I tried a different approach next. I thought that the expansion of
;;;    LENGTH2 should look very similar to LENGTH1 with merely an additional
;;;    recursive call available before the function failed. So I took the
;;;    expanded LENGTH1 version III above and modified it:
; #'(lambda (l)
;     (if (null l)
; 	0
; 	(1+ (funcall #'(lambda (l)
; 			 (if (null l)
; 			     0
; 			     (1+ (funcall
; 				  #'(lambda (l)
; 				      (if (null l)
; 					  0
; 					  (1+ (funcall (funcall #'hukairs
; 								#'hukairs)
; 						       (cdr l)))) )
; 				  (cdr l)))) )
; 		     (cdr l)))) )
;;;    This function does work properly for lists of length 2 or less.
;;;
;;;    Next, I worked backwards to determine the original expression that would
;;;    result in this function. Each step is indicated below:
;The above comes from this:
; #'(lambda (l)
;     (if (null l)
; 	0
; 	(1+ (funcall
; 	     #'(lambda (l)
; 		 (if (null l)
; 		     0
; 		     (1+ (funcall (funcall #'(lambda (mk-length)
; 					       #'(lambda (l)
; 						   (if (null l)
; 						       0
; 						       (1+ (funcall
; 							    (funcall mk-length
; 								     #'hukairs)
; 							    (cdr l)))) ))
; 					   #'hukairs)
; 				  (cdr l)))) )
; 	     (cdr l)))) )
;Which comes from this:
; #'(lambda (l)
;     (if (null l)
; 	0
; 	(1+ (funcall (funcall #'(lambda (mk-length)
; 				  #'(lambda (l)
; 				      (if (null l)
; 					  0
; 					  (1+ (funcall (funcall mk-length
; 								#'hukairs)
; 						       (cdr l)))) ))
; 			      #'(lambda (mk-length)
; 				  #'(lambda (l)
; 				      (if (null l)
; 					  0
; 					  (1+ (funcall (funcall mk-length
;                                                               #'hukairs)
; 						       (cdr l)))) )))
; 		     (cdr l)))) )
;Which comes from this:
; ((lambda (mk-length)
;    #'(lambda (l)
;        (if (null l)
; 	   0
; 	   (1+ (funcall (funcall mk-length mk-length)
; 			(cdr l)))) ))
;  #'(lambda (mk-length)
;      #'(lambda (l)
; 	 (if (null l)
; 	     0
; 	     (1+ (funcall (funcall mk-length #'hukairs)
; 			  (cdr l)))) )))
;;;
;;;    Of course, this is version III of length1 with the lambda wrapper
;;;    modified rather than the function-making function modified. I don't
;;;    know where to proceed from here.