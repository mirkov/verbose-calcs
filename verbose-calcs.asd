;;;; verbose-calcs.asd

(asdf:defsystem #:verbose-calcs
  :serial t
  :depends-on (#:lisp-unit)
  :components ((:file "package")
               (:file "verbose-calcs")))

