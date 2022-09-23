(defpackage :maxima-interface
  (:documentation "Simple interface between Common Lisp and Maxima")
  (:use :common-lisp :maxima)
  (:export #:maxima-init                ; Low level interface
           #:maxima-read
           #:maxima-eval
           #:maxima-print
           #:maxima-run
           #:i                          ; High level interface
           #:inf
           #:diff
           #:integrate
           #:limit
           #:simplify
           #:expand
           #:evaluate
           #:*latex-output*             ; LaTeX output
           #:latex))

(defpackage :maxima-interface-user
  (:use :common-lisp :maxima-interface))
