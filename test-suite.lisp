(fiasco:define-test-package :maxima-interface-test
  (:use :maxima-interface))

(in-package :maxima-interface-test)

(deftest test-evaluate ()
  (macrolet ((is-identity (&body body)
               "Auxiliary macro for TEST-EVALUATE."
               (let ((sexp (gensym)))
                 `(loop :for ,sexp :in ',body :do
                   (is (equalp (evaluate ,sexp) ,sexp))))))
    (is-identity
      (= a b)
      (/= a b)
      (+ a b)
      (* a b)
      (expt x n)
      (log x)
      (sin x)
      (cos x)
      (tan x)
      (asin x)
      (acos x)
      (atan y)
      (atan y x)
      (sinh x)
      (cosh x)
      (tanh x)
      (asinh x)
      (acosh x)
      (atanh y)
      (abs x)
      (signum x)
      (mod x p)
      pi
      e
      inf
      (f x y)))
  (is (equalp (evaluate '(- a b)) '(+ a (* -1 b))))
  (is (zerop (evaluate '(- a a))))
  (is (equalp (evaluate '(1+ x)) '(+ 1 x)))
  (is (equalp (evaluate '(1- x)) '(+ -1 x)))
  (is (equalp (evaluate '(exp x)) '(expt (exp 1) x)))
  (is (equalp (evaluate '(log x b)) '(* (expt (log b) -1) (log x))))
  (is (equalp (evaluate '(/ a b)) '(* a (expt b -1))))
  (is (equalp (evaluate '(sqrt x)) '(expt x (/ 1 2))))
  (is (equalp (evaluate 'i) '(complex 0 1)))
  (is (equalp (evaluate '(diff (f x y) x)) '(diff (f x y) x 1))))

(deftest test-expand ()
  (is (equalp (expand '(expt (+ x y) 2))
              '(+ (expt x 2) (* 2 x y) (expt y 2)))))

(deftest test-simplify ()
  (is (= 1 (simplify '(+ (expt (cos x) 2) (expt (sin x) 2)))))
  (is (eql (simplify '(/ (1- (1+ x)) 1)) 'x)))

(deftest test-diff ()
  (is (equalp (evaluate '(diff (cos x) x)) '(* -1 (sin x))))
  (is (equalp (evaluate '(diff '(expt (exp 1) x) 'x)) '(expt (exp 1) x)))
  (is (equalp (diff '(expt (exp 1) x) 'x) '(expt (exp 1) x))))

(deftest test-integrate ()
  (is (equalp (integrate 'x 'x)
              '(* (/ 1 2) (expt x 2))))
  (is (equalp (integrate 'x 'x 0 1)
              '(/ 1 2)))
  (is (equalp (integrate '(exp (- (expt x 2))) 'x '(- inf) 'inf)
              '(expt pi (/ 1 2)))))

(deftest test-limit ()
  (is (eql (limit '(- inf 1)) 'inf))
  (is (= 1 (limit '(/ (sin x) x) 'x 0 '+)))
  (is (equalp (limit '(f x) 'x 0 '+) '(limit (f x) x 0 +))))
