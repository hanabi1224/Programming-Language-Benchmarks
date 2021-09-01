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


;; This is our most expensive function.  Optimized with the knowledge
;; that 'n' will never be "huge".  This will break if 'n' exceeds
;; approximately half of the square root of the largest fixnum
;; supported by the implementation.  On 32-bit sbcl,
;; 'most-positive-fixnum' is 536870911, and we can support values of
;; 'n' above 11000.
(declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))

(deftype uint31 (&optional (bits 31))
  `(unsigned-byte ,bits))

(defmacro eval-A (i j)
  `(let* ((i+1   (1+ ,i))
          (i+j   (+ ,i ,j))
          (i+j+1 (+ i+1 ,j)))
     (declare (type uint31 i+1 i+j i+j+1))
     (/ (float (+ (ash (* i+j i+j+1) -1) i+1) 0d0))))

(defun eval-At-times-u (u n Au start end)
  (declare (type fixnum n start end)
           (type (simple-array double-float) u Au))
  (loop for i from start below end do
        (setf (aref Au i)
              (loop for j below n
                    summing (* (aref u j) (eval-A j i))
                    of-type double-float))))

(defun eval-A-times-u (u n Au start end)
  (declare (type fixnum n start end)
           (type (simple-array double-float) u Au))
  (loop for i from start below end do
        (setf (aref Au i)
              (loop for j below n
                    summing (* (aref u j) (eval-A i j))
                    of-type double-float))))

#+sb-thread
(defun get-thread-count ()
  (progn (define-alien-routine sysconf long (name int))
         (sysconf 84)))

#+sb-thread
(defun execute-parallel (start end function)
  (declare (optimize (speed 0)))
  (let* ((num-threads (get-thread-count)))
    (loop with step = (truncate (- end start) num-threads)
          for index from start below end by step
          collecting (let ((start index)
                           (end (min end (+ index step))))
                       (sb-thread:make-thread
                        (lambda () (funcall function start end))))
          into threads
          finally (mapcar #'sb-thread:join-thread threads))))

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
               (parse-integer (or (car (last #+sbcl sb-ext:*posix-argv*
                                             #+clisp ext:*args*
                                             #+cmu extensions:*command-line-strings*
                                             #+gcl  si::*command-args*))
                                  "3000")))))
    (declare (type fixnum n))
    (or (typep (* (- (* 2 n) 1) (- (* 2 n) 2)) 'fixnum)
        (error "The supplied value of 'n' breaks the optimizations in EVAL-A"))
    (let ((u (make-array n :element-type 'double-float :initial-element 1.0d0))
          (v (make-array n :element-type 'double-float))
          (tmp (make-array n :element-type 'double-float)))
      (declare (type (simple-array double-float) u v tmp))
      (dotimes (i 10)
        (eval-AtA-times-u u v tmp n 0 n)
        (eval-AtA-times-u v u tmp n 0 n))
      (let ((vBv 0.0d0)
            (vv 0.0d0))
        (dotimes (i n)
          (incf vBv (* (aref u i) (aref v i)))
          (incf vv (* (aref v i) (aref v i))))
        (format t "~11,9F~%" (sqrt (the (double-float 0d0) (/ vBv vv))))))))
