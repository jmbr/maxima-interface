(defpackage :maxima-interface
  (:documentation "Simple interface between Common Lisp and Maxima")
  (:use :common-lisp :maxima)
  (:export #:maxima-init
           #:maxima-read
           #:maxima-eval
           #:maxima-print
           #:maxima-run
           #:i
           #:inf
           #:diff
           #:integrate
           #:limit
           #:simplify
           #:expand
           #:evaluate))

(defpackage :maxima-interface-user
  (:use :common-lisp :maxima-interface))
