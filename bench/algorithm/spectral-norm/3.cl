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
;;      * Using AVX calculation in two lanes
;;      * Improvement in type declarations
;;      * Changed code to be compatible with sb-simd
;;      * Eliminated mixing VEX and non-VEX instructions as far as possible
;;        in the hot loops
;;      * Using s32.8 in eval-A and returning 2 f64.4 

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(eval-when (:compile-toplevel :load-toplevel :execute)  
  
  (ql:quickload :sb-simd)
  (use-package :sb-simd-avx2))

(declaim (ftype (function (s32.8 s32.8) (values f64.4 f64.4)) eval-A)
         (inline eval-A))
(defun eval-A (i j)
  (let* ((i+1   (s32.8+ i (s32.8 1)))
         (i+j   (s32.8+ i j))
         (i+j+1 (s32.8+ i+1 j))
         (evala  (s32.8+ (s32.8-shiftr (s32.8-mullo i+j i+j+1) 1) i+1)))
    (values (f64.4-from-s32.4 (s32.8-extract128 evala 0))
            (f64.4-from-s32.4 (s32.8-extract128 evala 1)))))

(declaim (ftype (function (f64vec f64vec u32 u32 u32) null)
                eval-A-times-u eval-At-times-u))
(defun eval-A-times-u (src dst begin end length)
  (loop for i of-type u32 from begin below end by 8
        with src-0 of-type f64 = (aref src 0)
        do (multiple-value-bind (eA0 eA1)
               (eval-A (s32.8+ (s32.8 i) (make-s32.8 0 1 2 3 4 5 6 7)) (s32.8 0))
             (let* ((ti0  (f64.4+ (f64.4 i) (make-f64.4 0 1 2 3)))
	            (ti1  (f64.4+ (f64.4 i) (make-f64.4 4 5 6 7)))
                    (sum0 (f64.4/ (f64.4 src-0) eA0))
		    (sum1 (f64.4/ (f64.4 src-0) eA1)))
	       (loop for j of-type u32 from 1 below length
		     do (let* ((src-j  (aref src j))
                               (j     (f64.4 j))
			       (idx0  (f64.4+ eA0 ti0 j))
			       (idx1  (f64.4+ eA1 ti1 j)))
			  (setf eA0 idx0
                                eA1 idx1)
			  (f64.4-incf sum0 (f64.4/ (f64.4 src-j) idx0))
			  (f64.4-incf sum1 (f64.4/ (f64.4 src-j) idx1))))
               (setf (f64.4-aref dst i) sum0)
               (setf (f64.4-aref dst (+ i 4)) sum1)))))

(defun eval-At-times-u (src dst begin end length)
  (loop for i of-type u32 from begin below end by 8
        with src-0 of-type f64 = (aref src 0)
	do  (multiple-value-bind (eAt0 eAt1)
                (eval-A (s32.8 0) (s32.8+ (s32.8 i) (make-s32.8 0 1 2 3 4 5 6 7)))
              (let* ((ti0   (f64.4+ (f64.4 i) (make-f64.4 1 2 3 4)))
		     (ti1   (f64.4+ (f64.4 i) (make-f64.4 5 6 7 8)))
                     (sum0  (f64.4/ (f64.4 src-0) eAt0))
		     (sum1  (f64.4/ (f64.4 src-0) eAt1)))
	        (loop for j of-type u32 from 1 below length
		      do (let* ((src-j  (aref src j))
                                (j     (f64.4 j))
                                (idx0  (f64.4+ eAt0 ti0 j))
			        (idx1  (f64.4+ eAt1 ti1 j)))
			   (setf eAt0 idx0
                                 eAt1 idx1)
			   (f64.4-incf sum0 (f64.4/ (f64.4 src-j) idx0))
			   (f64.4-incf sum1 (f64.4/ (f64.4 src-j) idx1))))
                (setf (f64.4-aref dst i) sum0)
                (setf (f64.4-aref dst (+ i 4)) sum1)))))

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
    (mapcar #'sb-thread:join-thread
            (loop for i from start below end by step
                  collecting (let ((start i)
                                   (end (min end (+ i step))))
                               (sb-thread:make-thread
			        (lambda () (funcall function start end))))
                  of-type thread))))

#-sb-thread
(defun execute-parallel (start end function)
  (funcall function start end))

(defun eval-AtA-times-u (src dst tmp start end N)
      (progn
	(execute-parallel start end (lambda (start end)
				      (eval-A-times-u src tmp start end N)))
	(execute-parallel start end (lambda (start end)
				      (eval-At-times-u tmp dst start end N)))))

(declaim (ftype (function (u32) f64) spectralnorm))
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
  (let ((n (or n-supplied (parse-integer (or (car (last sb-ext:*posix-argv*))
                                             "5000")))))
    (declare (type u32 n))
    (if (< n 16)
        (error "The supplied value of 'n' must be at least 16")
        (format t "~11,9F~%" (spectralnorm n)))))