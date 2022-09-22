(in-package :maxima-interface)

;;; Some of the operators used by Maxima are listed in comm.lisp.
(defparameter *lisp-to-maxima*
  '((=      . (maxima::mequal))
    (/=     . (maxima::mnotequal))
    (+      . (maxima::mplus))
    (-      . (maxima::mminus))
    (*      . (maxima::mtimes))
    (sqrt   . (maxima::%sqrt))
    (expt   . (maxima::mexpt))
    (sin    . (maxima::%sin))
    (cos    . (maxima::%cos))
    (tan    . (maxima::%tan))
    (asin   . (maxima::%asin))
    (acos   . (maxima::%acos))
    (sinh   . (maxima::%sinh))
    (cosh   . (maxima::%cosh))
    (tanh   . (maxima::%tanh))
    (asinh  . (maxima::%asinh))
    (acosh  . (maxima::%acosh))
    (atanh  . (maxima::%atanh))
    (abs    . (maxima::mabs))
    (signum . (maxima::%signum))
    (mod    . (maxima::$mod))
    (pi     . maxima::$%pi)
    (i      . maxima::$%i)
    (inf    . maxima::$inf)
    (diff   . (maxima::$diff))
    (diff   . (maxima::%derivative)))
  "Association list for translating Lisp to Maxima")

(defparameter *maxima-to-lisp-operations*
  '((maxima::mequal      . =)
    (maxima::mnotequal   . /=)
    (maxima::mplus       . +)
    (maxima::mminus      . -)
    (maxima::mtimes      . *)
    (maxima::mquotient   . /)
    (maxima::mrat        . /)
    (maxima::rat         . /)
    (maxima::%sqrt       . sqrt)
    (maxima::mexpt       . expt)
    (maxima::%log        . log)
    (maxima::%sin        . sin)
    (maxima::%cos        . cos)
    (maxima::%tan        . tan)
    (maxima::%asin       . asin)
    (maxima::%acos       . acos)
    (maxima::%atan       . atan)
    (maxima::$atan2      . atan)
    (maxima::%sinh       . sinh)
    (maxima::%cosh       . cosh)
    (maxima::%tanh       . tanh)
    (maxima::%asinh      . asinh)
    (maxima::%acosh      . acosh)
    (maxima::%atanh      . atanh)
    (maxima::mabs        . abs)
    (maxima::%signum     . signum)
    (maxima::$mod        . mod)
    (maxima::$diff       . diff)
    (maxima::%derivative . diff)
    (maxima::%limit      . limit))
  "Association list for translating operations from Maxima to Lisp")

(defparameter *maxima-to-lisp-constants*
  '((maxima::$%pi   . pi)
    (maxima::$%e    . (exp 1))
    (maxima::$%i    . (complex 0 1))
    (maxima::$inf   . inf)
    (maxima::$plus  . +)
    (maxima::$minus . -))
  "Association list for translating constants from Maxima to Lisp")

(defun lisp-to-maxima (sexp)
  "Translate s-expression to Maxima."
  (cond
    ((atom sexp)
     (or (rest (assoc sexp *lisp-to-maxima*)) sexp))
    ((consp sexp)
     (destructuring-bind (x . xs) sexp
       (let ((entry (assoc x *lisp-to-maxima*)))
         (if entry
             (cons (rest entry) (mapcar #'lisp-to-maxima xs))
             (case x                    ; Deal with special cases.
               (1+
                `((maxima::mplus) ,(lisp-to-maxima (first xs)) 1))
               (1-
                `((maxima::mplus) ,(lisp-to-maxima (first xs)) ((maxima::mminus) 1)))
               (exp
                `((maxima::mexpt) maxima::$%e ,(lisp-to-maxima (first xs))))
               (log
                (if (= (length xs) 2)
                    `((maxima::mquotient)
                      ((maxima::%log) ,(lisp-to-maxima (first xs)))
                      ((maxima::%log) ,(lisp-to-maxima (second xs))))
                    `((maxima::%log) ,(lisp-to-maxima (first xs)))))
               (atan
                (if (= (length xs) 2)
                    `((maxima::$atan2) ,@(mapcar #'lisp-to-maxima xs))
                    `((maxima::%atan) ,(lisp-to-maxima (first xs)))))
               (complex
                (if (= (length xs) 1)
                    (lisp-to-maxima (first xs))
                    `((maxima::mplus) ,(lisp-to-maxima (first xs))
                      ((maxima::mtimes) ,(lisp-to-maxima (second xs)) maxima::$%i))))
               (/
                (if (= (length xs) 1)
                    `((maxima::mquotient) 1 ,(lisp-to-maxima (first xs)))
                    `((maxima::mquotient) ,@(mapcar #'lisp-to-maxima xs))))
               (otherwise
                (cons (list (lisp-to-maxima x)) (mapcar #'lisp-to-maxima xs))))))))))

(defun maxima-to-lisp (sexp)
  "Translate s-expression from Maxima."
  (flet ((translate-op (x)
           (rest (assoc x *maxima-to-lisp-operations*)))
         (translate-const (x)
           (rest (assoc x *maxima-to-lisp-constants*))))
    (cond
      ((atom sexp)
       (or (translate-const sexp) sexp))
      ((consp sexp)
       (destructuring-bind (x . xs) sexp
         (or (translate-op x)
             (if (member 'maxima::simp xs :test #'eq)
                 (maxima-to-lisp x)   ; Omit tags added by Maxima.
                 (cons (maxima-to-lisp x) (maxima-to-lisp xs)))))))))

(defmethod evaluate (sexp)
  "Evaluate an expression in Maxima and return its translation to Lisp."
  (maxima-to-lisp (maxima-eval (lisp-to-maxima sexp))))

(defun diff (sexp var &optional (n 1))
  "Differentiate s-expression with respect to VAR, N times."
  (evaluate `(maxima::$diff ,sexp ,var ,n)))

(defun integrate (sexp var &rest integration-limits)
  "Integrate s-expression with regard to variable VAR, optionally using INTEGRATION-LIMITS."
  (if integration-limits
      (destructuring-bind (a b) integration-limits
        (evaluate `(maxima::$integrate ,sexp ,var ,a ,b)))
      (evaluate `(maxima::$integrate ,sexp ,var))))

(defun expand (sexp)
  "Expand s-expression."
  (evaluate `(maxima::$expand ,sexp)))

(defun simplify (sexp)
  "Simplify s-expression."
  (evaluate `(maxima::$ratsimp (maxima::$trigsimp ,sexp))))

(defun limit (sexp &optional var value dir)
  "Compute the limit of an s-expression as the variable VAR approaches VALUE
from the direction specified by DIR, which is either one of the + or - symbols."
  (declare (type (or null (member + -))))
  (evaluate `(maxima::$limit ,@(cond
                                 (dir (list sexp (lisp-to-maxima var) (lisp-to-maxima value)
                                            (if (eql '+ dir)
                                                'maxima::$plus
                                                'maxima::$minus)))
                                 (value (list sexp (lisp-to-maxima var) (lisp-to-maxima value)))
                                 (t (list sexp))))))

#| Minimal Maxima REPL (no pun intended)
(maxima-init)                           ;
(unwind-protect                         ;
(catch 'to-lisp                         ;
(loop                                   ;
(with-simple-restart (maxima::macsyma-quit "Maxima top-level") ;
(maxima::macsyma-top-level))))          ;
(maxima::delete-temp-files))            ;
|#                                      ;
