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
;;      * Using SSE registers but AVX2 VEX vector instruction sets
;;      * Improvement in type declarations
;;      * Redefine eval-A as inlined function using sse simd
;;      * Changed code to be compatible with sb-simd
(declaim (optimize (speed 3) (safety 0) (debug 0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :sb-simd)
  (use-package :sb-simd-avx2))

(declaim (ftype (function (f64.2 f64.2) f64.2) eval-A)
         (inline eval-A))
(defun eval-A (i j)
  (let* ((i+1   (f64.2+ i 1))
         (i+j   (f64.2+ i j))
         (i+j+1 (f64.2+ i+1 j)))
    (f64.2+ (f64.2* i+j i+j+1 0.5) i+1)))

(declaim (ftype (function (f64vec f64vec u32 u32 u32) null)
                eval-A-times-u eval-At-times-u))
(defun eval-A-times-u (src dst begin end length)
  (loop for i from begin below end by 2
	do (let* ((ti   (f64.2+ i (make-f64.2 0 1)))
		  (eA   (eval-A ti (f64.2 0)))
		  (sum  (f64.2/ (aref src 0) eA)))
	     (loop for j from 1 below length
		   do (let* ((idx (f64.2+ eA ti j)))
			(setf eA idx)
			(f64.2-incf sum (f64.2/ (aref src j) idx))))
             (setf (f64.2-aref dst i) sum))))

(defun eval-At-times-u (src dst begin end length)
  (loop for i from begin below end by 2
	do (let* ((ti   (f64.2+ i (make-f64.2 1 2)))
                  (eAt  (eval-A (f64.2 0) (f64.2- ti 1)))
                  (sum  (f64.2/ (aref src 0) eAt)))
	     (loop for j from 1 below length
                   do (let* ((idx  (f64.2+ eAt ti j)))
			(setf eAt idx)
			(f64.2-incf sum (f64.2/ (aref src j) idx))))
	     (setf (f64.2-aref dst i) sum))))

#+sb-thread
(defun get-thread-count ()
  (progn (define-alien-routine sysconf long (name int))
         (sysconf 84)))

(declaim (ftype (function (u32 u32 function) null) execute-parallel))
#+sb-thread
(defun execute-parallel (start end function)
  (declare (optimize (speed 0)))
  (let* ((n    (truncate (- end start) (get-thread-count)))
         (step (- n (mod n 2))))
    (declare (type u32 n step))
    (mapcar #'sb-thread:join-thread
            (loop for i from start below end by step
                  collecting (let ((start i)
                                   (end (min end (+ i step))))
                               (sb-thread:make-thread
			        (lambda () (funcall function start end))))))))

#-sb-thread
(defun execute-parallel (start end function)
  (funcall function start end))

(declaim (ftype (function (f64vec f64vec f64vec u32 u32 u32) null)
                eval-AtA-times-u))
(defun eval-AtA-times-u (src dst tmp start end N)
      (progn
	(execute-parallel start end (lambda (start end)
				      (eval-A-times-u src tmp start end N)))
	(execute-parallel start end (lambda (start end)
				      (eval-At-times-u tmp dst start end N)))))

(declaim (ftype (function (u32) f64) spectralnorm))
(defun spectralnorm (n)
  (let ((u   (make-array (+ n 1) :element-type 'f64 :initial-element 1d0))
        (v   (make-array (+ n 1) :element-type 'f64))
        (tmp (make-array (+ n 1) :element-type 'f64)))
    (declare (type f64vec u v tmp))
    (loop repeat 10 do
      (eval-AtA-times-u u v tmp 0 n n)
      (eval-AtA-times-u v u tmp 0 n n))
    (let ((vBv 0d0)
          (vv  0d0))
      (loop for i below n do
        (let ((aref-vi (aref v i)))
          (incf vBv (* (aref u i) aref-vi))
          (incf vv  (* aref-vi aref-vi))))
      (sqrt (/ vBv vv)))))

(defun main (&optional n-supplied)
  (let ((n (or n-supplied (parse-integer (or (car (last sb-ext:*posix-argv*))
                                             "5000")))))
    (declare (type u32 n))
    (if (< n 16)
        (error "The supplied value of 'n' must be at least 16")
        (format t "~11,9F~%" (spectralnorm n)))))
