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
;;    Modified by Bela Pecsek
;;      * Substantially rewritten using AVX calculations
;;      * Improvement in type declarations
;;      * Changed code to be compatible with sb-simd
;;      * Eliminated mixing VEX and non-VEX instructions as far as possible
;;        in the hot loops
;;      * Simplified eval-A-times-u code using serapeum with-boolean macro
;;        and using the -> macro for function type declarations
;;      * execute-parallel function refactorred - 2021-12-20
(declaim (optimize (speed 3) (safety 0) (debug 0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:sb-simd :serapeum) :silent t)
  (use-package  '(:sb-simd-avx2 :serapeum)))

(-> eval-A (f64.4 f64.4) f64.4)
(define-inline eval-A (i j)
  (let ((i+1   (f64.4+ i 1)))
    (f64.4+ (f64.4* (f64.4+ i j) (f64.4+ i+1 j) 0.5) i+1)))

(-> eval-A-times-u (boolean f64vec f64vec u32 u32 u32) null)
(defun eval-A-times-u (transpose src dst begin end length)
  (with-boolean (transpose)
    (loop with src-0 of-type f64 = (f64-aref src 0)
          for i of-type index from begin below end by 4
          do (let* ((ti  (if transpose (f64.4+ i (make-f64.4 1 2 3 4))
                                       (f64.4+ i (make-f64.4 0 1 2 3))))
                    (eA  (if transpose (eval-A (f64.4 0) (f64.4- ti 1))
                                       (eval-A ti (f64.4 0))))
		                (sum (f64.4/ src-0 eA)))
	             (loop for j of-type index from 1 below length
		                 do (let ((src-j (f64-aref src j))
                              (idx (f64.4+ eA ti j)))
			                    (setf eA idx)
			                    (f64.4-incf sum (f64.4/ src-j idx))))
	             (setf (f64.4-aref dst i) sum)))))

#+sb-thread
(defun get-thread-count ()
  (progn (define-alien-routine sysconf long (name int))
         (sysconf 84)))

(-> execute-parallel (u32 u32 function) null)
#+sb-thread
(defun execute-parallel (start end function)
  (declare (optimize (speed 0)))
  (mapc #'sb-thread:join-thread
          (loop with step = (truncate (- end start) (get-thread-count))
                for index from start below end by step
                collecting (let ((start index)
                                 (end (min end (+ index step))))
                             (sb-thread:make-thread
                              (lambda () (funcall function start end)))))))
#-sb-thread
(defun execute-parallel (start end function)
  (funcall function start end))

(-> eval-AtA-times-u (f64vec f64vec f64vec u32 u32 u32) null)
(defun eval-AtA-times-u (src dst tmp start end n)
      (progn
	(execute-parallel start end (lambda (start end)
				      (eval-A-times-u t src tmp start end n)))
	(execute-parallel start end (lambda (start end)
				      (eval-A-times-u nil tmp dst start end n)))))

(-> spectralnorm (u32) f64)
(defun spectralnorm (n)
  (let ((u   (make-array (+ n 3) :element-type 'f64 :initial-element 1d0))
        (v   (make-array (+ n 3) :element-type 'f64))
        (tmp (make-array (+ n 3) :element-type 'f64)))
    (declare (type f64vec u v tmp))
    (loop repeat 10 do
      (eval-AtA-times-u u v tmp 0 n n)
      (eval-AtA-times-u v u tmp 0 n n))
    (sqrt (f64/ (f64.4-vdot u v) (f64.4-vdot v v)))))

(defun main (&optional n-supplied)
  (let ((n (or n-supplied (parse-integer (or (car (last sb-ext:*posix-argv*))
                                             "5000")))))
    (declare (type u32 n))
    (if (< n 16)
        (error "The supplied value of 'n' must be at least 16")
        (format t "~11,9F~%" (spectralnorm n)))))
