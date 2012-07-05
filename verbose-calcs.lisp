;;;; verbose-calcs.lisp

(in-package #:verbose-calcs)
(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun binding+echo (binding)
    "From a three element string, do two things:

Return first two elements as a binding.  This will be use to build a
let form.

Unless the doc value is :QUIET, push a format string into special
variable ACC.

If DOC is a string, use it.  Otherwise, if it is NIL, use the symbol name"
    (declare (special acc))
    (destructuring-bind (var init-form &optional doc)
	binding
      (unless (eql doc :quiet)
	(let ((msg (format nil
			   "~a: ~~a~~%" (or doc var))))
	  (push `(format t ,msg ,var) acc)))
      `(,var ,init-form))))


(defmacro vlet ((title &rest bindings) &body body)
  "Establish a LET environment for the execution of BODY, printing
out a verbose message on the values of the bindings after all of them
have been established, and prior to executing BODY

TITLE -- title text
BINDINGS -- a list of two-to-three element lists, to be processed by 
            ``binding+echo''

Each binding consists of 
VAR -- a symbol
INIT-FORM -- a form
DOC -- optional documentation string

The macro expands into
 <generate title>
 (let ((var init-form)
      ...)
  <generate var=xyz for each var>
  <body>)

Thus (vlet* (\"some title\" (a 1) (b 2) (c (+ a b))) c)
will generate output of the form
some title
a: 1
b: 2
c: 3

and return the value of c
  "
  (let (acc)
    (declare (special acc))
    (let ((lexicals
	   (mapcar #'binding+echo
		   bindings)))
      `(progn
	 (format t "------------~%")
	 (format t "~a~%" ,title)
	 ,(if lexicals
	      `(let (,@lexicals)
		 ,@(nreverse acc)
		 ,@body)
	      `(progn ,@body))))))

(defmacro vlet* ((title &rest bindings) &body body)
  "Establish a LET* environment for the execution of BODY, printing
out a verbose message on the values of the bindings after all of them
have been established, and prior to executing BODY

TITLE -- title text
BINDINGS -- a list of two-to-three element lists, to be processed by 
            ``binding+echo''

Each binding consists of 
VAR -- a symbol
INIT-FORM -- a form
DOC -- optional documentation string

The macro expands into
 <generate title>
 (let ((var init-form)
      ...)
  <generate var=xyz for each var>
  <body>)

Thus (vlet* (\"some title\" (a 1) (b 2) (c (+ a b))) c)
will generate output of the form
some title
a: 1
b: 2
c: 3

and return the value of c
  "
  (let (acc)
    (declare (special acc))
    (let ((lexicals
	   (mapcar #'binding+echo
		   bindings)))
      `(progn
	 (format t "------------~%")
	 (format t "~a~%" ,title)
	 ,(if lexicals
	      `(let* (,@lexicals)
		 ,@(nreverse acc)
		 ,@body)
	      `(progn ,@body))))))


(define-test vlet*
  (assert-expands
   '(progn
     (format t "------------~%")
     (format t "~a~%" 'example)
     (let* ((a 3)
	    (b 4)
	    (c (+ a b)))
       (format t "A: ~a~%" a)
       (format t "result: ~a~%" c)
       t))
   (vlet*
       ('example
	(a 3)
	(b 4 :quiet)
	(c (+ a b) "result"))
     t))
  (assert-expands
   '(progn
     (format t "------------~%")
     (format t "~a~%" 'example1)
     (let* ((a 3))
       a))
   (vlet*
       ('example1
	(a 3 :quiet))
     a))
  (assert-expands
   '(progn
     (format t "------------~%")
     (format t "~a~%" 'example2)
     (progn t))
   (vlet*
       ('example2)
     t)))



(defmacro defvfun (name (&rest args) doc &body body)
  (let (acc)
    (declare (special acc))
    (let ((kwd-args
	   (mapcar (lambda (arg-def)
		     (subseq arg-def 0 2))
		   args))
	  (input-bindings (mapcar #'binding+echo args)))
      (declare (ignore input-bindings))
      (let ((input-echo (nreverse acc)))
	`(defun ,name (&key ,@kwd-args)
	   (format t "~%~%~a:~%~%" ,doc)
	   ,@input-echo
	   ,@body)))))

