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
(declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))
(setf *block-compile-default* t)

(deftype uint31 (&optional (bits 31)) `(unsigned-byte ,bits))
(deftype d+ () '(double-float 0d0))
(deftype array-d+ () '(simple-array d+))

(defmacro eval-A (i j)
  `(let* ((i+1   (1+ ,i))
          (i+j   (+ ,i ,j))
          (i+j+1 (+ i+1 ,j)))
     (declare (type uint31 i+1 i+j i+j+1))
     (/ (float (+ (ash (* i+j i+j+1) -1) i+1) 0d0))))

(declaim (ftype (function (array-d+ uint31 array-d+ uint31 uint31) null)
                eval-At-times-u eval-A-times-u))
(defun eval-At-times-u (u n Au start end)
  (loop for i from start below end do
    (setf (aref Au i)
          (loop for j below n
                summing (* (aref u j) (eval-A j i)) of-type d+))))

(defun eval-A-times-u (u n Au start end)
  (loop for i from start below end do
    (setf (aref Au i)
          (loop for j below n
                summing (* (aref u j) (eval-A i j)) of-type d+))))

#+sb-thread
(defun get-thread-count ()
  (progn (define-alien-routine sysconf long (name int))
         (sysconf 84)))

#+sb-thread
(defun execute-parallel (start end function)
  (declare (optimize (speed 0)))
  (let* ((num-threads (get-thread-count)))
    (mapcar #'sb-thread:join-thread
            (loop with step = (truncate (- end start) num-threads)
                  for index from start below end by step
                  collecting (let ((start index)
                                   (end (min end (+ index step))))
                               (sb-thread:make-thread
                                (lambda () (funcall function start end))))))))

#-sb-thread
(defun execute-parallel (start end function )
  (funcall function start end))

(defun eval-AtA-times-u (u AtAu v n start end)
  (execute-parallel start end
                    (lambda (start end)
                      (eval-A-times-u u n v start end)))
  (execute-parallel start end
                    (lambda (start end)
                      (eval-At-times-u v n AtAu start end))))

(defun main (&optional n-supplied)
  (let ((n (or n-supplied
               (parse-integer (or (car (last sb-ext:*posix-argv*)) "3000")))))
    (declare (type uint31 n))
    (or (typep (* (- (* 2 n) 1) (- (* 2 n) 2)) 'fixnum)
        (error "The supplied value of 'n' breaks the optimizations in EVAL-A"))
    (let ((u   (make-array n :element-type 'd+ :initial-element 1d0))
          (v   (make-array n :element-type 'd+))
          (tmp (make-array n :element-type 'd+)))
      (declare (type array-d+ u v tmp))
      (loop repeat 10 do
        (eval-AtA-times-u u v tmp n 0 n)
        (eval-AtA-times-u v u tmp n 0 n))
      (let ((vBv 0d0)
            (vv 0.0d0))
        (loop for i below n do
          (let ((aref-v (aref v i)))
            (incf vBv (* (aref u i) aref-v))
            (incf vv  (* aref-v aref-v))))
        (format t "~11,9F~%" (sqrt (the d+ (/ vBv vv))))))))
