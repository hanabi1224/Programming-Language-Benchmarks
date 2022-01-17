;;    The Computer Language Benchmarks Game
;;    https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
;;
;;    Adapted from the C (gcc) code by Sebastien Loisel
;;
;;    Contributed by Christopher Neufeld
;;    Modified by Juho Snellman 2005-10-26
;;      * Use SIMPLE-ARRAY instead of ARRAY in declarations
;;      * Use TRUNCATE instead of / for fixnum division
;;      * Rearrange EVAL-A to make it more readable and a bit faster
;;    Modified by Andy Hefner 2008-09-18
;;      * Eliminate array consing
;;      * Clean up type declarations in eval-A
;;      * Distribute work across multiple cores on SBCL
;;    Modified by Witali Kusnezow 2008-12-02
;;      * use right shift instead of truncate for division in eval-A
;;      * redefine eval-A as a macro
;;    Optimize declaration and get-thread-count added by Bela Pecsek
;;      * eval-A macro slightly improved
;;      * uint31 type defined
;;      * double-float values are always positive
;;      * threading slightly modified - 2021-09-19
;;      * eval-A inlines function is optimised for ultimate speed - 2021-12-27
;;      * Simplified eval-A-times-u code using serapeum with-boolean compile
;;        time macro and using the -> macro for function type declarations
(declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :serapeum :silent t)
  (use-package :serapeum))

(deftype uint31   () '(unsigned-byte 31))
(deftype d+       () '(double-float 0d0))
(deftype array-d+ () '(simple-array d+ (*)))

(defmacro eval-A (i j)
  `(let ((i+1 (1+ ,i)))
     (+ (ash (the uint31 (* (the uint31 (+ ,i ,j))
                            (the uint31 (+ i+1 ,j)))) -1) i+1)))


(-> eval-a-times-u (boolean array-d+ uint31 array-d+ uint31 uint31) null)
(defun eval-A-times-u (transpose u n Au start end)
  (with-boolean (transpose)
    (loop for i from start below end
          do (setf (aref Au i)
                   (loop for j below n
                         sum (/ (aref u j) (if transpose (eval-A j i)
                                                 (eval-A i j))) of-type d+)))))

#+sb-thread
(defun get-thread-count ()
  (progn (define-alien-routine sysconf long (name int))
         (sysconf 84)))

(-> execute-parallel (uint31 uint31 function) null)
#+sb-thread
(defun execute-parallel (start end function)
  (declare (optimize (speed 1)))
  (mapc #'sb-thread:join-thread
          (loop with step = (truncate (- end start) (get-thread-count))
                for index from start below end by step
                collecting (let ((start index)
                                 (end (min end (+ index step))))
                             (sb-thread:make-thread
                              (lambda () (funcall function start end)))))))

#-sb-thread
(defun execute-parallel (start end function )
  (funcall function start end))

(-> eval-AtA-times-u (array-d+ array-d+ array-d+ uint31 uint31 uint31) null)
(defun eval-AtA-times-u (u AtAu v n start end)
  (execute-parallel start end
                    (lambda (start end) (eval-A-times-u nil u n v start end)))
  (execute-parallel start end
                    (lambda (start end) (eval-A-times-u t v n AtAu start end))))


(-> spectralnorm (uint31) null)
(defun main (&optional n-supplied)
  (let* ((n (or n-supplied (parse-integer (or (car (last sb-ext:*posix-argv*))
                                             "8000"))))
        (u   (make-array n :element-type 'd+ :initial-element 1d0))
        (v   (make-array n :element-type 'd+))
        (tmp (make-array n :element-type 'd+)))
    (declare (type array-d+ u v tmp))
    (loop repeat 10 do
      (eval-AtA-times-u u v tmp n 0 n)
      (eval-AtA-times-u v u tmp n 0 n))
    (loop with uv of-type d+ = 0d0
          with vv of-type  d+ = 0d0
          for i below n
          for vi of-type d+ = (aref v i)
          do (incf uv (* (the d+ (aref u i)) (the d+ vi)))
             (incf vv  (* (the d+ vi) (the d+ vi)))
          finally (format t "~11,9F~%" (sqrt (the d+ (/ (the d+ uv)
                                                        (the d+ vv))))))))
