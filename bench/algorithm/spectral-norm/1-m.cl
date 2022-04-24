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
(declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))

(deftype uint31  () '(unsigned-byte 31))
(deftype d+      () '(double-float 0d0))
(deftype d+array () '(simple-array d+ (*)))

(declaim (ftype (function (uint31 uint31) uint31) eval-A)
         (inline eval-A))
(defun eval-A (i j)
  (let ((i+1 (1+ i)))
    (the uint31 (+ (ash (the uint31 (* (+ i j) (+ i+1 j))) -1) i+1))))

(declaim (ftype (function (d+array uint31 d+array uint31 uint31) null)
                eval-A-times-u eval-At-times-u))
(defun eval-A-times-u (u n Au start end)
  (loop for i of-type uint31 from start below end do
    (setf (aref Au i) (loop for j of-type uint31 below n
                            summing (/ (aref u j) (eval-A i j)) of-type d+))))

(defun eval-At-times-u (u n Au start end)
  (loop for i of-type uint31 from start below end do
    (setf (aref Au i) (loop for j of-type uint31 below n
                            summing (/ (aref u j) (eval-A j i)) of-type d+))))

#+sb-thread
(defun get-thread-count ()
  (progn (define-alien-routine sysconf long (name int))
         (sysconf 84)))

(declaim (ftype (function (uint31 uint31 function) null) execute-parallel))
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

(defun eval-AtA-times-u (u AtAu v n start end)
  (execute-parallel start end
                    (lambda (start end) (eval-A-times-u u n v start end)))
  (execute-parallel start end
                    (lambda (start end) (eval-At-times-u v n AtAu start end))))

(declaim (ftype (function (&optional uint31) null) main))
(defun main (&optional n-supplied)
  (let ((n (or n-supplied (parse-integer (or (car (last sb-ext:*posix-argv*))
                                             "5000")))))
    (or (typep (* (- (* 2 n) 1) (- (* 2 n) 2)) 'fixnum)
        (error "The supplied value of 'n' breaks the optimizations in EVAL-A"))
    (let ((u   (make-array n :element-type 'd+ :initial-element 1d0))
          (v   (make-array n :element-type 'd+))
          (tmp (make-array n :element-type 'd+)))
      (declare (type d+array u v tmp))
      (loop repeat 10 do
        (eval-AtA-times-u u v tmp n 0 n)
        (eval-AtA-times-u v u tmp n 0 n))
      (let ((vBv 0d0)
            (vv  0d0))
        (loop for i of-type uint31 below n do
          (let ((vi (aref v i)))
            (incf vBv (* (the d+ (aref u i)) (the d+ vi)))
            (incf vv  (* (the d+ vi) (the d+ vi)))))
        (format t "~11,9F~%" (sqrt (the d+ (/ (the d+ vBv) (the d+ vv)))))))))
