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
;;      * Using AVX calculations
;;      * Improvement in type declarations
;;      * Changed code to be compatible with sb-simd
;;      * Eliminated mixing VEX and non-VEX instructions as far as possible
;;        in the hot loops
(declaim (optimize (speed 3) (safety 0) (debug 0)))
(setf *block-compile-default* t)

(eval-when (:compile-toplevel :load-toplevel :execute)
 (ql:quickload :sb-simd)
 (use-package :sb-simd-avx))

(declaim (ftype (function (f64.4 f64.4) f64.4) eval-A)
         (inline eval-A))
(defun eval-A (i j)
  (let* ((i+1   (f64.4+ i (f64.4 1)))
         (i+j   (f64.4+ i j))
         (i+j+1 (f64.4+ i+1 j)))
    (f64.4+ (f64.4* i+j i+j+1 (f64.4 0.5)) i+1)))

(declaim (ftype (function (f64vec f64vec u32 u32 u32) null)
                eval-A-times-u eval-At-times-u))
(defun eval-A-times-u (src dst begin end length)
  (loop for i of-type fixnum from begin below end by 4
        do (let* ((src-0 (f64.4 (aref src 0)))
                  (ti    (f64.4+ (f64.4 i) (make-f64.4 0 1 2 3)))
                  (eA    (eval-A ti (f64.4 0)))
		  (sum   (f64.4/ src-0 eA)))
	     (loop for j from 1 below length
		   do (let ((src-j (aref src j))
                            (idx (f64.4+ eA ti (f64.4 j))))
			(setf eA idx)
			(f64.4-incf sum (f64.4/ (f64.4 src-j) idx))))
	     (setf (f64.4-aref dst i) sum))))

(defun eval-At-times-u (src dst begin end length)
  (loop for i from begin below end by 4
        do (let* ((src-0 (f64.4 (aref src 0)))
                  (ti    (f64.4+ (f64.4 i) (make-f64.4 1 2 3 4)))
                  (eAt   (eval-A (f64.4 0) (f64.4+ ti (f64.4 -1))))
		  (sum   (f64.4/ src-0 eAt)))
	     (loop for j from 1 below length
                   do (let ((src-j (aref src j))
                            (idx (f64.4+ eAt ti (f64.4 j))))
			(setf eAt idx)
			(f64.4-incf sum (f64.4/ (f64.4 src-j) idx))))
	     (setf (f64.4-aref dst i) sum))))

#+sb-thread
(defun get-thread-count ()
  (progn (define-alien-routine sysconf long (name int))
         (sysconf 84)))

#+sb-thread
(defun execute-parallel (start end function)
  (declare (optimize (speed 0)))
  (let* ((n    (truncate (- end start) (get-thread-count)))
         (step (- n (mod n 2))))
    (loop for i from start below end by step
          collecting (let ((start i)
                           (end (min end (+ i step))))
                       (sb-thread:make-thread
			(lambda () (funcall function start end))))
            into threads
          finally (mapcar #'sb-thread:join-thread threads))))

#-sb-thread
(defun execute-parallel (start end function)
  (funcall function start end))

(defun eval-AtA-times-u (src dst tmp start end n)
      (progn
	(execute-parallel start end (lambda (start end)
				      (eval-A-times-u src tmp start end n)))
	(execute-parallel start end (lambda (start end)
				      (eval-At-times-u tmp dst start end n)))))

(defun spectralnorm (n)
  (let ((u   (make-array (+ n 3) :element-type 'f64 :initial-element 1d0))
        (v   (make-array (+ n 3) :element-type 'f64))
        (tmp (make-array (+ n 3) :element-type 'f64)))
    (declare (type f64vec u v tmp))
    (loop repeat 10 do
      (eval-AtA-times-u u v tmp 0 n n)
      (eval-AtA-times-u v u tmp 0 n n))
    (sqrt (/ (f64.4-vdot u v) (f64.4-vdot v v)))))

(defun main (&optional n-supplied)
  (let ((n (or n-supplied (parse-integer (or (car (last #+sbcl sb-ext:*posix-argv*))
                                  "5000")))))
    (if (< n 16)
        (error "The supplied value of 'n' must be at least 16")
        (format t "~11,9F~%" (spectralnorm n)))))
