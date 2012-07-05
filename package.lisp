;;;; package.lisp

(defpackage #:verbose-calcs
  (:use #:cl #:lisp-unit)
  (:import-from #:alexandria
		#:with-gensyms)
  (:export #:verbose-calc #:defvfun))

