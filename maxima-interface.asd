(defsystem :maxima-interface
  :description "Simple interface between Common Lisp and Maxima"
  :author "Juan M. Bello Rivas <jmbr@superadditive.com>"
  :licence "X11"
  :depends-on ((:version "asdf" "3.1.2")
               :uiop
               :maxima)
  :serial t
  :components ((:file "package")
               (:file "low-level-interface")
               (:file "high-level-interface")
               (:file "latex"))
  :in-order-to ((test-op (test-op "maxima-interface/tests"))))

(defsystem :maxima-interface/tests
  :description "Test suite for maxima-interface."
  :depends-on ((:version "asdf" "3.1.2")
               :maxima-interface
               :fiasco)
  :perform (test-op (o s) (symbol-call '#:fiasco '#:run-package-tests :package
                                       '#:maxima-interface-test))
  :components ((:file "test-suite")))
