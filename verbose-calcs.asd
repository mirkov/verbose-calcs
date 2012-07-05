;;;; verbose-calcs.asd

(asdf:defsystem #:verbose-calcs
  :serial t
  :depends-on (#:alexandria
	       #:lisp-unit)
  :components ((:file "package")
               (:file "verbose-calcs")))

