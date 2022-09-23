(in-package :maxima-interface)

(defvar *latex-output* :console
  "Outlet for LaTeX output. Can be one of :console, :emacs, or :jupyter")

(defgeneric lisp-to-latex (object)
  (:documentation "Return LaTeX representation of OBJECT as a string.")
  (:method (x)
    (maxima-eval (lisp-to-maxima (cons 'maxima::$tex1 (list x)))))
  (:method ((x string))
    x)
  (:method ((x number))
    ;; Enclosing in an mbox is a work-around for tex2svg.
    (format nil "\\mbox{~D}" x))
  (:method ((x symbol))
    (or (get x 'maxima::texword)
        (concatenate 'string "\\textrm{" (symbol-name x) "}")))
  (:method ((x array))
    (with-output-to-string (stream)
      (let ((dimensions (array-dimensions x)))
        (if (= (length dimensions) 2)
            (destructuring-bind (m n) (array-dimensions x)
              (format stream "\\begin{bmatrix}~%")
              (loop :for i :below m :do
                (loop :for j :below (1- n) :do
                  (format stream "~A &" (lisp-to-latex (aref x i j)))
                      :finally (format stream "~A"
                                       (lisp-to-latex (aref x i (1- n)))))
                (format stream "\\\\~%"))
              (format stream "\\end{bmatrix}~%"))
            (error "Unable to render arrays of more than two-dimensions.")))))
  (:method ((x vector))
    (with-output-to-string (stream)
      (format stream "\\begin{bmatrix}~%~{~A~^\\\\~%~}\\end{bmatrix}~%"
              (loop :for v :across x :collect (lisp-to-latex v))))))

(defun latex (object)
  "Render LaTeX representation of object in Emacs."
  (case *latex-output*
    (:emacs
     (uiop:symbol-call :swank '#:send-to-emacs `(:write-latex ,(lisp-to-latex object)))
     (uiop:symbol-call :swank '#:send-to-emacs `(:write-string ,(string #\Newline) :repl-result)))
    (:jupyter
     (uiop:symbol-call :jupyter '#:latex (concatenate 'string "$$" (lisp-to-latex object) "$$")))
    (:console
     (princ (lisp-to-latex object))
     (terpri))))
