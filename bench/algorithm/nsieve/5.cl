;; Ported to Common Lisp by Daniel Spangberg
;; Converted to be used in Programming Language and compiler Benchmarks by Bela Pecsek
;;   Changed CLOS sieve-state class to struct
(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defun binarize (expr)
  (if (and (nthcdr 3 expr) (member (car expr) '(+ - * / ash abs sin cos tan cot
                                                ceiling isqrt signum log exp expt)))
      (destructuring-bind (op a1 a2 . rest) expr
        (binarize `(,op (,op ,a1 ,a2) ,@rest))) expr))

(defun expand-call (type expr)
  `(,(car expr) ,@(mapcar #'(lambda (a) `(with-type ,type ,a)) (cdr expr))))

(defmacro with-type (type expr)
  `(#+sbcl sb-ext:truly-the #-sbcl the ,type
           ,(if (and (consp expr) (member (car expr) '(1+ 1- + - * / ash abs sin cos
                                           tan cot ceiling isqrt signum log exp expt)
                     :test #'eq)) (expand-call type (binarize expr)) expr)))


(declaim (type '(simple-array fixnum 1) +steps+))
(defconstant +steps+ (coerce
#(8 1 2 3 1 3 2 1 2 3 3 1 3 2 1 3 2 3 4 2 1 2 1 2 7 
  2 3 1 5 1 3 3 2 3 3 1 5 1 2 1 6 6 2 1 2 3 1 5 3 3 
  3 1 3 2 1 3 2 7 2 1 2 3 4 3 5 1 2 3 1 3 3 3 2 3 1 
  3 2 4 5 1 5 1 2 1 2 3 4 2 1 2 6 4 2 1 3 2 3 6 1 2 
  1 6 3 2 3 3 3 1 3 5 1 2 3 1 3 3 2 1 5 1 5 1 2 3 3 
  1 3 3 2 3 4 3 2 1 3 2 3 4 2 1 3 2 4 3 2 4 2 3 4 5 
  1 5 1 3 2 1 2 1 5 1 5 1 2 1 2 7 2 1 2 3 3 1 3 2 4 
  5 4 2 1 2 3 4 3 2 3 3 3 1 3 3 2 1 2 3 1 5 1 2 1 5 
  1 5 1 3 2 4 3 2 1 2 3 3 4 2 1 3 5 4 2 1 3 2 4 5 3 
  1 2 4 3 3 2 1 2 3 1 3 2 3 1 5 6 1 2 1 2 3 1 3 2 1 
  2 6 1 3 3 5 3 4 2 1 2 1 2 4 3 6 2 3 1 6 2 1 2 3 4 
  2 1 2 1 6 5 1 2 1 2 3 1 5 1 2 3 4 3 2 1 3 2 3 4 2 
  3 1 2 4 3 2 3 1 2 3 1 3 3 2 3 3 4 3 2 1 5 1 5 1 2 
  1 5 1 3 2 1 5 3 1 3 2 1 3 2 3 4 3 2 1 6 5 3 1 2 3 
  1 6 2 1 2 4 3 2 1 2 1 5 1 5 3 1 2 3 1 3 2 1 5 3 1 
  3 2 6 3 4 3 2 1 2 4 3 2 3 1 2 3 4 3 3 2 3 1 3 2 1 
  2 1 5 6 1 2 6 1 3 2 1 2 3 3 1 6 3 2 9 1 2 1 2 4 3 
  2 3 1 2 4 3 3 2 1 2 3 1 3 2 1 2 6 1 6 3 2 3 1 3 2 
  3 3 3 1 3 2 1 3 2 3 4 2 1 2 1 2 7 2 3 1 5 1 3 3 2 
  1 5 1 5 1 2 7 5 1 2 1 2 3 1 3 5 3 3 1 5 1 3 2 3 4 
  2 1 2 3 4 3 5 1 2 3 1 3 3 2 1 2 3 1 3 2 1 3 5 1 5 
  3 1 2 3 4 2 1 2 6 1 3 2 1 3 2 3 6 1 2 1 2 4 3 2 3 
  1 5 1 3 5 3 3 1 3 2 1 2 1 5 1 6 2 3 3 1 6 2 3 3 1 
  3 2 1 3 2 7 2 1 3 2 4 3 2 3 1 2 3 4 3 3 5 1 3 2 3 
  1 5 1 5 1 2 1 2 4 3 2 1 2 3 3 4 2 4 2 3 4 2 1 2 1 
  6 3 2 3 3 3 1 3 3 2 1 2 3 1 3 3 2 1 5 1 5 1 3 2 3 
  1 3 2 1 2 3 7 2 1 3 5 4 2 1 2 1 2 4 5 4 2 4 3 5 1 
  2 3 1 3 2 3 1 5 1 5 1 2 1 2 3 4 2 1 2 3 3 1 3 3 3 
  5 4 2 1 2 3 4 3 2 4 2 3 1 3 3 2 1 2 3 6 1 2 1 5 1 
  5 1 2 1 2 4 5 1 2 3 4 3 2 1 3 2 3 4 2 4 2 4 3 2 3 
  1 2 3 1 3 3 2 3 3 1 3 3 2 1 5 6 1 2 1 2 3 1 3 2 1 
  8 1 3 2 1 5 3 4 2 1 2 1 6 3 5 1 2 3 1 6 2 1 2 4 3 
  2 1 2 1 6 5 3 1 2 3 1 3 2 1 2 3 3 1 3 2 1 5 3 4 5 
  1 2 4 3 2 3 1 2 3 1 3 3 3 2 3 4 2 1 2 1 5 6 1 2 1 
  5 1 3 2 1 2 3 3 1 5 1 3 2 7 3 2 1 2 4 5 3 1 2 3 1 
  3 3 2 1 2 4 3 2 1 2 6 1 6 2 1 2 3 1 3 2 1 5 3 1 3 
  2 4 2 3 4 2 1 2 1 2 7 2 3 1 5 4 3 2 1 2 3 1 5 1 2 
  1 6 5 1 2 3 3 1 3 2 3 3 3 1 3 3 3 2 3 6 1 2 3 4 3 
  5 1 2 4 3 3 2 1 2 3 1 3 2 1 3 5 1 5 1 3 2 3 4 2 3 
  6 1 3 2 1 3 2 3 6 1 2 1 2 7 2 3 1 2 3 1 3 5 1 5 1 
  3 2 1 2 6 1 5 1 2 3 3 1 3 3 2 3 3 1 5 1 3 2 3 4 2 
  1 3 2 4 3 2 3 1 2 3 4 3 2 1 5 1 3 2 1 3 5 1 5 3 1 
  2 4 3 2 1 2 3 3 1 3 2 4 2 3 4 2 1 2 1 2 4 3 2 3 6 
  1 3 3 2 3 3 1 3 2 1 2 1 5 1 6 3 2 3 1 5 1 2 3 3 4 
  2 1 3 9 2 1 2 1 2 4 5 3 1 2 4 3 3 3 2 3 1 3 2 3 1 
  5 1 5 1 2 1 2 3 1 3 2 1 2 3 3 4 3 3 2 3 4 2 1 2 1 
  6 3 2 6 3 1 3 3 2 1 2 3 4 3 2 1 5 1 5 1 2 1 2 3 1 
  5 1 2 3 4 3 2 1 3 2 3 4 2 3 1 2 4 3 2 4 2 3 1 3 5 
  3 3 1 3 3 2 1 5 1 5 1 2 1 2 3 4 2 1 5 3 1 3 2 1 3 
  5 4 2 1 2 7 3 2 3 1 2 3 1 6 2 1 2 4 5 1 2 1 5 1 5 
  3 1 2 4 3 2 1 2 3 3 1 3 2 1 5 3 4 3 3 2 4 3 2 3 1 
  2 3 1 3 3 3 2 3 1 3 2 1 2 1 5 6 1 2 1 5 1 3 2 1 2 
  6 1 5 1 5 7 2 1 2 1 2 4 3 5 1 2 3 1 6 2 1 2 3 1 3 
  2 1 2 7 6 2 1 2 3 1 3 2 1 2 3 3 1 3 2 1 3 2 3 4 2 
  3 1 2 7 2 3 1 5 1 3 3 2 1 2 3 6 1 2 1 6 5 1 2 1 5 
  1 3 2 3 3 3 1 3 2 1 3 2 3 4 3 2 3 4 8 1 2 3 1 3 3 
  2 1 2 4 3 2 1 3 5 1 5 1 2 1 2 3 4 2 1 8 1 3 2 4 2 
  3 6 1 2 1 2 4 3 2 3 1 2 3 4 5 1 2 3 1 3 2 1 2 1 5 
  1 5 1 2 3 3 1 3 3 2 3 3 1 3 3 3 2 3 6 1 3 2 4 3 2 
  3 1 2 7 3 2 1 5 1 3 2 1 2 1 5 1 5 1 3 2 4 3 2 3 3 
  3 1 3 2 4 2 3 4 2 1 2 1 2 7 2 3 3 3 1 3 3 2 1 5 1 
  3 2 1 2 6 1 5 1 3 2 3 1 3 3 2 3 3 6 1 3 5 4 2 1 2 
  1 2 4 5 3 1 2 4 3 3 2 1 2 3 1 3 2 4 5 1 5 3 1 2 3 
  1 3 2 1 2 3 3 1 3 3 3 2 3 4 2 1 2 1 2 4 3 2 4 5 1 
  3 3 2 3 3 4 2 1 2 1 5 1 6 2 1 2 3 1 5 1 2 3 4 3 2 
  1 3 2 7 2 3 1 2 4 3 2 3 1 2 3 1 3 3 5 3 1 3 5 1 5 
  1 5 1 2 1 2 3 1 3 2 1 5 3 4 2 1 3 2 3 4 2 1 2 1 6 
  3 2 3 3 3 1 6 2 1 2 4 3 3 2 1 5 1 5 3 1 2 3 1 3 2 
  1 2 3 4 3 2 1 5 3 4 3 2 1 2 4 3 2 4 2 3 1 3 6 2 3 
  1 3 2 1 2 1 5 6 1 2 1 5 4 2 1 2 3 3 1 5 1 3 9 2 1 
  2 3 4 3 2 3 1 2 3 1 3 3 2 1 2 3 1 5 1 2 6 1 6 2 1 
  2 4 3 2 1 2 3 3 1 3 2 1 3 2 3 4 2 1 3 2 7 2 3 1 5 
  1 3 3 2 1 2 3 1 5 1 2 1 11 1 2 1 2 3 1 3 2 3 6 1 3 
  2 1 5 3 4 2 1 2 3 4 3 5 1 2 3 1 6 2 1 2 3 1 3 2 1 
  3 6 5 1 2 1 2 3 4 2 1 2 6 1 3 2 1 3 2 3 6 3 1 2 4 
  3 2 3 1 2 3 1 3 5 1 2 3 4 2 1 2 1 5 1 5 1 2 6 1 3 
  3 2 3 3 1 3 2 1 3 2 3 4 3 3 2 4 5 3 1 2 3 4 3 2 1 
  6 3 2 1 2 1 5 1 5 1 2 1 2 4 3 2 1 5 3 1 3 2 4 2 3 
  4 2 1 2 1 2 4 3 2 3 3 3 4 3 2 1 2 3 1 3 2 1 2 1 5 
  1 5 1 5 3 1 3 2 1 2 3 3 4 3 3 5 6 1 2 1 2 4 5 3 1 
  2 4 3 3 2 1 2 3 1 3 2 3 1 5 1 5 1 3 2 3 1 3 2 3 3 
  3 1 3 3 3 2 3 4 2 1 2 1 2 7 2 4 2 3 1 3 3 2 1 5 4 
  2 1 2 6 1 5 1 2 1 2 3 1 6 2 3 4 5 1 3 2 3 4 2 3 1 
  2 4 3 2 3 1 2 3 1 3 3 2 3 3 1 3 3 3 5 1 5 3 1 2 3 
  1 3 2 1 5 3 1 3 2 1 3 2 3 4 2 1 2 1 6 3 2 3 1 5 1 
  6 2 3 4 3 2 1 2 1 5 1 8 1 2 3 1 5 1 2 3 3 1 3 2 1 
  5 7 3 2 1 2 4 3 2 3 1 2 3 1 3 3 3 2 3 1 3 2 3 1 5 
  6 1 2 1 5 1 3 2 1 2 3 3 6 1 3 2 7 2 1 2 1 6 3 2 3 
  3 3 1 3 3 2 1 2 3 1 3 3 2 6 1 6 2 1 2 3 1 3 2 1 2 
  3 4 3 2 1 3 2 3 4 2 1 2 1 2 7 2 4 5 1 3 5 1 2 3 1 
  5 1 2 1 6 5 1 2 1 2 3 4 2 3 3 3 1 3 2 1 3 5 4 2 1 
  2 3 4 3 5 1 2 3 1 3 3 2 1 2 3 1 5 1 3 5 1 5 1 2 1 
  2 7 2 1 2 6 1 3 2 1 3 2 3 6 1 3 2 4 3 2 3 1 2 3 1 
  3 5 1 2 3 1 3 2 1 2 1 5 6 1 2 3 3 1 3 3 2 6 1 3 2 
  1 5 3 4 2 1 3 2 4 3 5 1 2 3 7 2 1 5 1 3 2 1 2 1 6 
  5 1 2 1 2 4 3 2 1 2 3 3 1 3 2 4 2 3 4 2 3 1 2 4 3 
  2 3 3 3 1 3 3 2 1 2 3 4 2 1 2 1 5 1 5 1 3 5 1 3 2 
  1 2 3 3 4 2 1 3 5 4 3 2 1 2 4 5 3 1 2 4 3 3 2 1 2 
  4 3 2 3 1 5 1 5 1 2 1 2 3 1 3 2 1 5 3 1 3 6 2 3 4 
  2 1 2 1 2 4 3 2 4 2 3 4 3 2 1 2 3 4 2 1 2 1 5 1 5 
  1 2 3 3 1 5 1 2 3 4 3 3 3 2 3 6 3 1 2 4 3 2 3 1 2 
  4 3 3 2 3 3 1 3 3 2 1 5 1 5 1 3 2 3 1 3 2 6 3 1 3 
  2 1 3 2 3 4 2 1 2 1 9 2 3 1 2 3 1 6 2 1 6 3 2 1 2 
  6 1 5 3 1 2 3 1 3 3 2 3 3 1 5 1 5 3 4 3 2 1 2 4 3 
  2 3 1 2 3 1 3 3 3 2 3 1 3 2 1 3 5 6 3 1 5 1 3 2 1 
  2 3 3 1 5 1 3 2 7 2 1 2 1 2 4 3 2 3 1 5 1 3 3 2 3 
  3 1 3 2 1 2 6 1 6 2 1 2 3 1 5 1 2 3 3 1 3 2 1 3 2 
  7 2 1 2 1 2 7 2 3 1 5 1 3 3 3 2 3 1 5 3 1 6 5 1 2 
  1 2 3 1 3 2 3 3 3 4 2 1 3 2 3 4 2 1 2 7 3 5 3 3 1 
  3 3 2 1 2 3 1 3 3 3 5 1 5 1 2 1 2 3 4 2 1 2 7 3 2 
  1 3 2 3 6 1 2 1 2 4 3 2 4 2 3 1 3 5 1 2 3 1 3 2 1 
  2 1 5 1 5 1 2 3 3 4 3 2 3 3 1 3 2 1 3 5 4 2 1 5 4 
  3 2 3 1 2 3 4 3 2 1 5 1 5 1 2 1 5 1 5 1 2 1 2 4 3 
  2 1 2 3 3 1 3 2 4 2 3 4 2 1 3 2 4 3 2 3 3 3 1 3 3 
  2 1 2 3 1 3 2 1 2 1 5 6 1 3 2 3 1 3 2 1 2 6 4 2 1 
  8 4 2 1 2 1 2 4 8 1 2 4 6 2 1 2 3 1 3 2 3 1 6 5 1 
  2 1 2 3 1 3 2 1 2 3 3 1 3 3 3 2 3 4 2 3 1 2 4 3 2 
  4 2 3 1 3 3 2 1 2 3 4 2 1 2 1 5 1 5 1 2 1 5 1 5 1 
  2 3 4 3 2 1 3 2 3 4 5 1 2 4 5 3 1 2 3 1 3 3 2 3 4 
  3 3 2 1 5 1 5 1 2 1 2 3 1 3 2 1 5 3 1 3 2 4 2 3 4 
  2 1 2 1 6 3 2 3 1 2 3 7 2 1 2 4 3 2 1 2 1 5 1 5 3 
  3 3 1 3 2 1 2 3 3 1 3 3 5 3 7 2 1 2 4 3 2 3 1 2 4 
  3 3 3 2 3 1 3 2 1 2 1 5 6 1 3 5 1 3 2 3 3 3 1 5 1 
  3 2 7 2 1 2 1 2 7 2 3 1 2 3 1 3 3 2 1 5 1 3 2 1 2 
  6 1 6 2 1 2 3 1 3 3 2 3 3 1 5 1 3 2 3 4 2 1 2 1 2 
  7 2 3 1 5 1 3 3 2 1 2 3 1 5 1 3 6 5 3 1 2 3 1 3 2 
  3 3 3 1 3 2 1 3 2 3 4 2 1 2 3 4 3 5 1 5 1 3 3 2 3 
  3 1 3 2 1 3 5 1 6 2 1 2 3 6 1 2 6 1 3 2 1 3 2 9 1 
  2 1 2 4 3 2 3 1 2 3 1 3 6 2 3 1 3 2 3 1 5 1 5 1 2 
  3 3 1 3 3 2 3 3 4 2 1 3 2 3 4 2 1 3 6 3 2 3 3 3 4 
  3 2 1 5 1 3 3 2 1 5 1 5 1 2 1 2 4 3 2 1 2 3 4 3 2 
  4 2 3 4 2 1 2 1 2 4 3 2 6 3 1 3 5 1 2 3 1 3 2 1 2 
  1 5 1 5 1 3 2 3 4 2 1 2 3 3 4 2 1 3 5 4 2 1 2 3 4 
  5 3 1 2 4 3 3 2 1 2 3 1 5 3 1 5 1 5 1 2 1 2 4 3 2 
  1 2 3 3 1 3 3 3 2 3 4 2 1 3 2 4 3 2 4 2 3 1 3 3 2 
  1 2 3 4 2 1 2 1 5 6 1 2 1 2 3 1 5 1 2 7 3 2 1 5 3 
  4 2 3 1 2 4 3 5 1 2 3 1 6 2 3 3 1 3 3 2 1 6 5 1 2 
  1 2 3 1 3 2 1 5 3 1 3 2 1 3 2 3 4 2 3 1 6 3 2 3 1 
  2 3 1 6 2 1 2 7 2 1 2 1 5 1 5 3 1 5 1 3 2 1 2 3 3 
  1 3 2 1 5 3 4 3 2 1 2 4 5 3 1 2 3 1 3 3 3 2 4 3 2 
  1 2 1 5 6 1 2 1 5 1 3 2 1 5 3 1 5 4 2 7 2 1 2 1 2 
  4 3 2 3 1 2 3 4 3 2 1 2 3 1 3 2 1 2 6 1 6 2 3 3 1 
  3 2 1 2 3 3 1 3 3 3 2 3 6 1 2 1 2 7 2 3 1 6 3 3 2 
  1 2 3 1 5 1 2 1 6 5 1 3 2 3 1 3 2 3 3 3 1 3 2 1 3 
  2 3 4 2 1 2 3 7 5 1 2 3 1 3 3 2 1 5 1 3 2 1 8 1 5 
  1 2 1 2 3 4 3 2 6 1 5 1 3 2 3 6 1 2 1 2 4 3 2 3 1 
  2 3 1 3 5 1 2 3 1 3 2 1 3 5 1 5 3 3 3 1 3 3 2 3 3 
  1 3 2 1 3 2 3 4 2 1 3 2 4 3 2 3 1 5 4 3 2 6 1 3 2 
  1 2 1 5 1 6 2 1 2 4 5 1 2 3 3 1 3 2 4 2 7 2 1 2 1 
  2 4 3 2 3 3 3 1 3 3 3 2 3 1 3 2 3 1 5 1 5 1 3 2 3 
  1 3 2 1 2 3 3 4 2 1 3 5 4 2 1 2 1 6 5 3 3 4 3 3 2 
  1 2 3 1 3 5 1 5 1 5 1 2 1 2 3 1 3 2 1 2 3 4 3 3 3 
  2 3 4 2 1 2 1 2 4 3 2 4 2 3 1 3 5 1 2 3 4 2 1 2 1 
  5 1 5 1 2 1 2 3 6 1 2 3 4 3 2 1 3 5 4 2 3 3 4 3 2 
  3 1 2 3 1 3 3 2 3 3 1 6 2 1 5 1 5 1 2 1 2 4 3 2 1 
  5 3 1 3 2 1 3 2 3 4 2 1 3 6 3 2 3 1 2 3 1 6 2 1 2 
  4 3 2 1 2 1 5 6 3 1 2 3 1 3 2 1 2 6 1 3 2 1 5 3 4 
  3 2 1 2 4 3 5 1 2 3 1 6 3 2 3 1 3 2 1 2 1 11 1 2 1 
  5 1 3 2 1 2 3 3 1 5 1 3 2 7 2 3 1 2 4 3 2 3 1 2 3 
  1 3 3 2 1 2 3 4 2 1 2 6 1 6 2 1 5 1 3 2 1 2 3 3 1 
  3 2 1 3 2 3 4 3 2 1 2 9 3 1 5 1 3 3 2 1 2 4 5 1 2 
  1 6 5 1 2 1 2 3 1 3 2 6 3 1 3 2 4 2 3 4 2 1 2 3 4 
  3 5 1 2 3 4 3 2 1 2 3 1 3 2 1 3 5 1 5 1 2 3 3 4 2 
  1 2 6 1 3 3 3 2 3 6 1 2 1 2 4 3 2 3 1 2 4 3 5 1 2 
  3 1 3 2 1 2 1 5 1 5 1 5 3 1 3 5 3 3 1 3 2 1 3 2 3 
  4 2 1 3 2 7 2 3 1 2 3 4 3 2 1 5 1 3 2 1 2 6 1 5 1 
  2 1 2 4 3 3 2 3 3 1 5 4 2 3 4 2 1 2 1 2 4 3 2 3 3 
  3 1 3 3 2 1 2 3 1 3 2 1 3 5 1 5 4 2 3 1 3 2 1 2 3 
  3 4 2 1 3 5 4 2 1 2 1 2 4 5 3 1 6 3 3 2 3 3 1 3 2 
  3 1 5 1 6 2 1 2 3 1 5 1 2 3 3 1 3 3 3 2 7 2 1 2 1 
  2 4 3 2 4 2 3 1 3 3 3 2 3 4 2 3 1 5 1 5 1 2 1 2 3 
  1 5 1 2 3 7 2 1 3 2 3 4 2 3 1 6 3 2 3 3 3 1 3 3 2 
  3 3 1 3 3 2 1 5 1 5 1 2 1 2 3 1 3 2 1 5 4 3 2 1 3 
  2 3 4 2 1 2 1 6 3 2 4 2 3 1 8 1 2 4 3 2 1 2 1 5 1 
  5 3 1 2 3 4 2 1 2 3 3 1 3 2 1 8 4 3 2 3 4 3 2 3 1 
  2 3 1 3 3 3 2 3 1 5 1 2 1 5 6 1 2 1 6 3 2 1 2 3 3 
  1 5 1 3 2 7 2 1 3 2 4 3 2 3 1 2 3 1 3 3 2 1 2 3 1 
  3 2 1 2 6 7 2 1 2 3 1 3 2 1 2 6 1 3 2 1 5 3 4 2 1 
  2 1 2 7 5 1 5 1 6 2 1 2 3 1 5 1 2 1 6 5 1 2 1 2 3 
  1 3 2 3 3 3 1 3 2 1 3 2 3 4 2 3 3 4 3 5 1 2 3 1 3 
  3 2 1 2 3 4 2 1 3 5 1 5 1 2 1 5 4 2 1 2 6 1 3 2 1 
  3 2 3 7 2 1 2 4 5 3 1 2 3 1 3 5 1 2 4 3 2 1 2 1 5 
  1 5 1 2 3 3 1 3 3 5 3 1 3 2 4 2 3 4 2 1 3 2 4 3 2 
  3 1 2 3 4 3 2 1 5 1 3 2 1 2 1 5 1 5 1 2 3 4 3 2 1 
  2 3 3 1 3 6 2 3 6 1 2 1 2 4 3 2 3 3 4 3 3 2 1 2 3 
  1 3 2 1 2 1 5 1 5 1 3 2 3 1 3 2 3 3 3 4 2 1 3 5 4 
  2 1 2 1 2 9 3 1 2 4 3 3 2 1 5 1 3 2 3 6 1 5 1 2 1 
  2 3 1 3 3 2 3 3 1 6 3 2 3 4 2 1 2 1 2 4 3 2 4 2 3 
  1 3 3 2 1 2 3 4 2 1 3 5 1 5 3 1 2 3 1 5 1 2 3 4 3 
  2 1 3 2 3 4 2 3 1 2 4 3 2 3 1 5 1 3 3 2 3 3 1 3 3 
  2 1 5 1 6 2 1 2 3 1 5 1 5 3 1 3 2 1 3 2 7 2 1 2 1 
  6 3 2 3 1 2 3 1 6 3 2 4 3 2 3 1 5 1 5 3 1 2 3 1 3 
  2 1 2 3 3 4 2 1 5 3 4 3 2 1 6 3 2 3 3 3 1 3 3 3 2 
  3 1 3 3 2 1 5 6 1 2 1 5 1 3 2 1 2 3 4 5 1 3 2 7 2 
  1 2 1 2 4 3 2 4 2 3 1 3 5 1 2 3 1 3 2 1 2 6 1 6 2 
  1 2 3 4 2 1 2 3 3 1 3 2 1 3 5 4 2 1 2 3 7 2 3 1 5 
  1 3 3 2 1 2 3 1 5 1 2 1 6 5 1 2 1 2 4 3 2 3 3 3 1 
  3 2 1 3 2 3 4 2 1 5 4 3 5 1 2 3 1 3 3 2 1 2 3 1 3 
  2 1 3 5 6 1 2 1 2 3 4 2 1 2 6 1 3 2 1 5 3 6 1 2 1 
  2 4 3 5 1 2 3 1 8 1 2 3 1 3 2 1 2 1 6 5 1 2 3 3 1 
  3 3 2 3 3 1 3 2 1 3 2 3 4 2 4 2 4 3 2 3 1 2 3 4 3 
  2 1 5 4 2 1 2 1 5 1 5 1 2 1 6 3 2 1 2 3 3 1 3 2 4 
  2 3 4 3 2 1 2 4 5 3 3 3 1 3 3 2 1 2 4 3 2 1 2 1 5 
  1 5 1 3 2 3 1 3 2 1 5 3 4 2 4 5 4 2 1 2 1 2 4 5 3 
  1 2 7 3 2 1 2 3 1 3 2 3 1 5 1 5 1 2 3 3 1 3 2 1 2 
  3 3 1 3 3 3 2 3 6 1 2 1 2 4 3 2 4 2 4 3 3 2 1 2 3 
  4 2 1 2 1 5 1 5 1 3 2 3 1 5 3 3 4 3 2 1 3 2 3 4 2 
  3 1 2 7 2 3 1 2 3 1 3 3 2 6 1 3 3 2 6 1 5 1 2 1 2 
  3 1 3 3 5 3 1 5 1 3 2 3 4 2 1 2 1 6 3 2 3 1 2 3 1 
  6 2 1 2 4 3 2 1 3 5 1 5 3 1 2 3 1 3 2 1 2 3 3 1 3 
  2 1 5 3 4 3 2 1 2 4 3 2 3 1 5 1 3 3 5 3 1 3 2 1 2 
  1 5 7 2 1 5 1 5 1 2 3 3 1 5 1 3 2 7 2 1 2 1 2 4 3 
  2 3 1 2 3 1 3 3 3 2 3 1 3 2 3 6 1 6 2 1 2 3 1 3 2 
  1 2 3 3 4 2 1 3 2 3 4 2 1 2 1 9 2 3 6 1 3 3 2 1 2 
  3 1 6 2 1 6 5 1 2 1 2 3 1 3 2 3 3 4 3 2 1 3 2 3 4 
  2 1 2 3 4 3 6 2 3 1 3 5 1 2 3 1 3 2 1 3 5 1 5 1 2 
  1 2 3 4 2 1 2 6 1 3 2 1 3 5 6 1 2 3 4 3 2 3 1 2 3 
  1 3 5 1 2 3 1 5 1 2 1 5 1 5 1 2 3 4 3 3 2 3 3 1 3 
  2 1 3 2 3 4 2 1 3 2 4 3 2 3 1 2 3 4 3 2 1 5 1 3 2 
  1 2 1 5 6 1 2 1 2 4 3 2 1 2 6 1 3 2 6 3 4 2 1 2 1 
  2 4 3 5 3 3 1 6 2 1 2 3 1 3 2 1 2 1 6 5 1 3 2 3 1 
  3 2 1 2 3 3 4 2 1 3 5 4 2 3 1 2 4 5 3 1 2 4 3 3 2 
  1 2 3 4 2 3 1 5 1 5 1 2 1 5 1 3 2 1 2 3 3 1 3 3 3 
  2 3 4 3 2 1 2 4 5 4 2 3 1 3 3 2 1 2 7 2 1 2 1 5 1 
  5 1 2 1 2 3 1 5 1 5 4 3 2 4 2 3 4 2 3 1 2 4 3 2 3 
  1 2 3 4 3 2 3 3 1 3 3 2 1 5 1 5 1 2 3 3 1 3 2 1 5 
  3 1 3 3 3 2 3 6 1 2 1 6 3 2 3 1 2 4 6 2 1 2 4 3 2 
  1 2 1 5 1 5 4 2 3 1 3 2 3 3 3 1 3 2 1 5 3 4 3 2 1 
  2 7 2 3 1 2 3 1 3 3 3 5 1 3 2 1 2 6 6 1 2 1 5 1 3 
  3 2 3 3 1 5 1 3 2 7 2 1 2 1 2 4 3 2 3 1 2 3 1 3 3 
  2 1 2 3 1 3 2 1 8 1) '(simple-array fixnum 1)))

(declaim (inline make-sieve-state maxints (setf maxints) sieve (setf sieve)))
(defstruct (sieve-state (:conc-name nil)
                        (:constructor make-sieve-state (maxints sieve)))
  (maxints nil :type fixnum)
  (sieve   nil :type simple-bit-vector))

(declaim (ftype (function (fixnum) (values sieve-state &optional)) create-sieve))
(defun create-sieve (maxints)
  (make-sieve-state maxints (make-array (/ maxints 2) :element-type 'bit :initial-element 0)))

(declaim (ftype (function (sieve-state (simple-array fixnum (*)))
                          (values sieve-state &optional)) run-sieve))
(defun run-sieve (sieve-state steps)
  (do* ((maxints  (maxints sieve-state))
        (qh       (with-type fixnum (ash (ceiling (isqrt maxints)) -1)))
        (maxintsh (ash maxints -1))
        (sieve    (sieve sieve-state))
        (step 1   (if (>= step 5759) 0 (with-type fixnum (1+ step))))
        (factorh  (with-type fixnum (ash 17 -1))))
       ((> factorh qh) sieve-state)
    (declare (type fixnum maxints maxintsh qh step factorh)
             (type simple-bit-vector sieve))
    (when (zerop (sbit sieve factorh))
      (do* ((istep  step (if (>= istep 5759) 0 (with-type fixnum (1+ istep))))
            (ninc   (aref steps istep) (aref steps istep))
            (factor (with-type fixnum (1+ (* factorh 2))))
            (i      (with-type fixnum (ash (* factor factor) -1))))
           ((>= i   maxintsh))
        (declare (fixnum istep ninc factor i))
        (setf (sbit sieve i) 1)
        (incf i (with-type fixnum (* factor ninc)))))
    (setq factorh (+ factorh (aref steps step)))))

(declaim (ftype (function (sieve-state) (values fixnum &optional)) count-primes))
(defun count-primes (sieve-state)
  (let ((maxints (maxints sieve-state)))
    (do* ((sieve  (sieve sieve-state))
          (ncount (if (<= maxints 10) 4 6))
          (factor 17)
          (step 1 (if (>= step 5759) 0 (with-type fixnum (1+ step))))
          (inc    (* (aref +steps+ step) 2) (* (aref +steps+ step) 2)))
         ((> factor maxints) ncount)
      (declare (type fixnum maxints ncount factor inc)
               (type simple-bit-vector sieve))
      (when (zerop (sbit sieve (ash factor -1)))
        (incf ncount))
      (incf factor inc))))

(declaim (ftype (function (&optional (integer 0 16)) null) main))
(defun main (&optional n-supplied)
  (let* ((n (or n-supplied (parse-integer (car (last sb-ext:*posix-argv*))))))
    (declare ((integer 0 16) n))
    (loop for k below 3 
          for m = (ash 10000 (- n k))
          for sieve-state of-type sieve-state = (create-sieve m)
          do (format t "Primes up to~T~8<~d~>~T~8<~d~>~%"
                     m (count-primes (run-sieve sieve-state +steps+))))))
