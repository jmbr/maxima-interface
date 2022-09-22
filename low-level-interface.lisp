(in-package :maxima-interface)

(defun maxima-init ()
  "Set up Maxima path names for autoload."
  (maxima::set-pathnames))

(eval-when (:load-toplevel)
  (maxima-init))

(defun maxima-read (expression)
  "Parse Maxima EXPRESSION."
  (with-input-from-string (input-stream expression)
    (caddr (maxima::dbm-read input-stream nil))))

(defun maxima-eval (&rest expressions)
  "Evaluate Maxima EXPRESSION in Maxima."
  (loop :for expression :in expressions
        :for result := (maxima::meval expression)
        :finally (return result)))

(defun maxima-print (expression &key (output-stream *standard-output*)
                                  (display2d t) (return-expression nil))
  "Print Maxima EXPRESSION to OUTPUT-STREAM."
  (let ((*standard-output* output-stream)
        (maxima::$display2d display2d))
    (maxima::displa expression)
    (if return-expression
        expression
        (values))))

(defun maxima-run (expression &rest arguments)
  "Parse, evaluate, and print Maxima EXPRESSION to OUTPUT-STREAM."
  (let ((evaluated-expression (maxima-eval (maxima-read expression))))
    (apply #'maxima-print evaluated-expression arguments)))
