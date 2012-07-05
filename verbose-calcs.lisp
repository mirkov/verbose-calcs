;;;; verbose-calcs.lisp

(in-package #:verbose-calcs)
(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun binding+echo (binding)
    "Reform the three element list (VAR INIT-FORM DOC) into
 (var (prog1 init-form
          (format t \"~a: ~a~%\" doc init-form)))

If DOC is nil, use the VAR's symbol-name

If DOC is :quiet, do not create the format form, return (var
init-form)
"
    (destructuring-bind (var init-form &optional (doc nil))
	binding
      (with-gensyms (it)
	(if (eql doc :quiet)
	    `(,var ,init-form)
	    `(,var (let ((,it ,init-form)) 
		     (format t "~a: ~a~%" ,(or doc (symbol-name var)) ,it)
		     ,it)))))))


(define-test binding+echo
  (assert-equal '(foo 3)
		(binding+echo '(foo 3 :quiet)))
  (assert-equal '(foo (let ((it 3))
			(format t "~a: ~a~%" "bar" it)
			it))
		(binding+echo '(foo 3 "bar")))
  (assert-equal '(foo (let ((it 3))
			(format t "~a: ~a~%" 'foo it)
			it))
		(binding+echo '(foo 3))))

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
  (let ((lexicals
	 (mapcar #'binding+echo
		 bindings)))
    `(progn
       (format t "------------~%")
       (format t "~a~%" ,title)
       ,(if lexicals
	    `(let (,@lexicals)
	       ,@body)
	    `(progn ,@body)))))

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
  (let ((lexicals
	 (mapcar #'binding+echo
		 bindings)))
    `(progn
       (format t "------------~%")
       (format t "~a~%" ,title)
       ,(if lexicals
	    `(let* (,@lexicals)
	       ,@body)
	    `(progn ,@body)))))


(define-test vlet*
  (assert-expands
   '(progn
     (format t "------------~%")
     (format t "~a~%" 'example)
     (let* ((a (let ((it 3))
		 (format t "~a: ~a~%" "Aaa" it)
		 it))
	    (b (let ((it 4))
		 (format t "~a: ~a~%" "B" it)
		 it))
	    (c 5)
	    (d (+ a b c)))
       d))
   (vlet*
       ('example
	(a 3 "Aaa")
	(b 4)
	(c 5 :quiet)
	(d (+ a b c) :quiet))
     d))
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



(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun binding+deferred-echo (binding)
    "Reform the three element list (VAR INIT-FORM DOC) into
 (var (prog1 init-form
          (format t \"~a: ~a~%\" doc init-form)))

If DOC is nil, use the VAR's symbol-name

If DOC is :quiet, do not create the format form, return (var
init-form)
"
    (declare (special acc))
    (destructuring-bind (var init-form &optional (doc nil))
	binding
	(if (eql doc :quiet)
	    `(,var ,init-form)
	    (progn
	      (push `(format t "~a: ~a~%" ,(or doc (symbol-name var)) ,var)
		    acc)
	      `(,var ,init-form))))))


(define-test binding+deferred-echo
  (let (acc)
    (declare (special acc))
    (assert-equal '(var 5)
		  (binding+deferred-echo '(var 5))))
  (let (acc)
    (declare (special acc))
    (assert-equal '(var 5)
		  (binding+deferred-echo '(var 5 :quiet))))
  (let (acc)
    (declare (special acc))
    (assert-equal '(var 5)
		  (binding+deferred-echo '(var 5)))
    (assert-equal '((format t "~a: ~a~%" "VAR" var)) acc))
  (let (acc)
    (declare (special acc))
    (assert-equal '(var 5)
		  (binding+deferred-echo '(var 5 "doc")))
    (assert-equal '((format t "~a: ~a~%" "doc" var)) acc)))
  
(defmacro defvfun (name (&rest args) doc+opt-header &body body)
  (let (acc)
    (declare (special acc))
    (let ((kwd-args
	   (mapcar (lambda (arg-def)
		     (subseq arg-def 0 2))
		   args))
	  (input-bindings (mapcar #'binding+deferred-echo args)))
      (declare (ignore input-bindings))
      (let ((input-echo (nreverse acc)))
	`(defun ,name (&key ,@kwd-args)
	   ,@(if (consp doc+opt-header)
		 (destructuring-bind (doc header) doc+opt-header
		   `(,(format nil "~a" doc)
		      (format t "~a~%" ,header)))
		 (list (format nil "~a" doc+opt-header)))
	   ,@input-echo
	   ,@body)))))


(define-test defvfun
  (assert-expands
   '(defun foo (&key (arg1 5) (arg2 5) (arg3 4))
     "function documentation"
     (format t "~a: ~a~%" "ARG1" arg1)
     (format t "~a: ~a~%" "arg2doc" arg2)
     'function-body)
   (defvfun foo ((arg1 5) (arg2 5 "arg2doc") (arg3 4 :quiet))
       "function documentation"
     'function-body))
  (assert-expands
   '(defun foo (&key (arg1 5) (arg2 5) (arg3 4))
     "function documentation"
     (format t "~a~%" "run documentation")
     (format t "~a: ~a~%" "ARG1" arg1)
     (format t "~a: ~a~%" "arg2doc" arg2)
     'function-body)
   (defvfun foo ((arg1 5) (arg2 5 "arg2doc") (arg3 4 :quiet))
       ("function documentation" "run documentation")
     'function-body)))