;;;   The Computer Language Benchmarks Game
;;;   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
;;;
;;;   by Jon Smith from GCC Mandelbrot version
;;;   does computation nearly the same as the GCC #4 version of the code.
;;;   
;;;   modified by Bela Pecsek
;;;
;;;   The code was based on GCC #4, however the threading was not as of gcc
;;;   resulting in severely under-utilised processes/CPU cores.
;;;   Threading changed to interleaved as of the gcc code to fully utilise all
;;;   8 logical CPU cores/processes during execution, resulting in some 50%
;;;   speedup in the calculation phase and some 30% reduction in the overall
;;;   exacution time of the program.
;;;
;;;   Modified by Bela Pecsek for the Programming Language and compiler Benchmarks
(declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))
(sb-int:set-floating-point-modes :traps (list :divide-by-zero))

(eval-when (:compile-toplevel :load-toplevel :execute)  
  (require "sb-md5")
  (defpackage :vops
    (:use :cl :sb-md5)))

(in-package :sb-vm)
(eval-when (:load-toplevel :compile-toplevel :execute)
  (handler-bind ((simple-error (lambda (condition)
				 condition
				 (invoke-restart 'continue))))
    
    (defknown complex-double-float/sse-* ((complex double-float)
					  (complex double-float))
	(complex double-float))
    (defknown cmple-movmskpd ((complex double-float)
			      (complex double-float))
	(unsigned-byte 32))

    (define-vop (complex-double-float/sse-*)
      (:policy :fast-safe)
      (:translate complex-double-float/sse-*)
      (:args (x :scs (complex-double-reg))
	     (y :scs (complex-double-reg)))
      (:arg-types complex-double-float complex-double-float)
      (:results (r :scs (complex-double-reg)))
      (:result-types complex-double-float)
      (:generator 2
		  (flet ((get-constant (tn)
			   (register-inline-constant
			    (tn-value tn))))
		    (cond ((location= x r)
			   (inst mulpd x y))
			  ((location= y r)
			   (inst mulpd y x))
			  ((not (location= r y))
			   (if (sc-is x fp-complex-double-immediate)
			       (inst movapd r (get-constant x))
			       (move r x))
			   (inst mulpd r y))))))

    (macrolet ((generate (opinst test movmsk constant-sc load-inst)
		 `(flet ((get-constant (tn)
			   (register-inline-constant
			    ,@(and (eq constant-sc 'fp-single-immediate)
				   '(:aligned))
			    (tn-value tn))))
		    (declare (ignorable #'get-constant))
		    (cond
		      ((location= x r)
		       (when (sc-is y ,constant-sc)
			 (setf y (get-constant y)))
		       (inst ,opinst ,test x y)
		       (inst ,movmsk r x))
		      ((not (location= r y))
		       (if (sc-is x ,constant-sc)
			   (inst ,load-inst r (get-constant x))
			   (move tmp x))
		       (when (sc-is y ,constant-sc)
			 (setf y (get-constant y)))
		       (inst ,opinst ,test tmp y)
		       (inst ,movmsk r tmp))
		      (t
		       (if (sc-is x ,constant-sc)
			   (inst ,load-inst tmp (get-constant x))
			   (move tmp x))
		       (when (sc-is y ,constant-sc)
			 (setf y (get-constant y)))
		       (inst ,opinst ,test tmp y)
		       (inst ,movmsk r tmp)
		       ))))
	       (frob (test cdinst cdname cdcost)
		 `(progn
		    (define-vop (,cdname)
		      (:translate ,cdname)
		      (:policy :fast-safe)
		      (:args (x :scs (complex-double-reg))
			     (y :scs (complex-double-reg)))
		      (:arg-types complex-double-float
				  complex-double-float)
		      (:results (r :scs (unsigned-reg)))
		      (:result-types unsigned-num)
		      (:temporary (:sc complex-double-reg) tmp)
		      (:info)
		      (:generator ,cdcost
				  (generate ,cdinst ,test movmskpd
					    fp-complex-double-immediate
					    movapd))))))
      (frob :le cmppd cmple-movmskpd 3)))

  (declaim (inline complex-double-float/sse-*))
  (declaim (inline cmple-movmskpd))
  (declaim (inline vops::calc-row vops::main))
  (declaim (inline vops::complex-double-float/sse-*))
  (declaim (inline vops::cmple-movmskpd)))

(defun vops::complex-double-float/sse-* (numbera numberb)
  (declare (type (complex double-float) numbera numberb))
  (complex-double-float/sse-* numbera numberb))

(defun vops::cmple-movmskpd (numbera numberb)
  (declare (type (complex double-float) numbera numberb))
  (cmple-movmskpd numbera numberb))

(in-package :vops)
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defmacro cbyte (form)
    (cond ((stringp form)
	   (map '(simple-array (unsigned-byte 8) (*)) #'char-code form))
	  ((characterp form)
	   (char-code form))
	  ((listp form)
	   `(map '(simple-array (unsigned-byte 8) (*)) #'char-code ,form)))))

#+sb-thread
(defun get-thread-count ()
  (progn (define-alien-routine sysconf long (name int))
         (sysconf 84)))

(defconstant +zero+ (complex 0.0d0 0.0d0))
(defconstant +four+ (complex 4.0d0 4.0d0))
(defconstant +workers+ (get-thread-count))

(defmacro escapes? (n two-pixels  crv civ)
  (let ((escaped (gensym "escaped"))
	(temp (gensym "temp"))
	(temp2 (gensym "temp2"))
	(zrv (gensym))
	(ziv (gensym))
	(trv (gensym))
	(tiv (gensym)))
    `(let ((,zrv vops::+zero+)
	   (,ziv vops::+zero+)
	   (,trv vops::+zero+)
	   (,tiv vops::+zero+))
       (block ,escaped 
	 ,@(nreverse
            (loop for i of-type fixnum from 0 below n
		  collecting `(progn
				(let* ((,temp (complex-double-float/sse-*
					       ,zrv ,ziv)))
				  (setf ,zrv (+ (- ,trv ,tiv) ,crv))
				  (setf ,trv (complex-double-float/sse-*
					      ,zrv ,zrv))
				  (setf ,ziv (+ ,temp ,temp ,civ))
				  (setf ,tiv (complex-double-float/sse-*
					      ,ziv ,ziv)))
				(let ((,temp2 (+ ,trv ,tiv)))
				  (setf ,two-pixels (cmple-movmskpd
						     ,temp2 ,vops::+four+)))
				(when (= ,two-pixels 0)
				  (return-from ,escaped)))))))))

(defun vops::calc-row (y n bitmap bytes-per-row crvs inverse-h)
  (declare (type fixnum y n bytes-per-row)
	   (type double-float inverse-h)
	   (type (simple-array (unsigned-byte 8) (*)) bitmap)
	   (type (simple-array (complex double-float) (*)) crvs))
  (let ((index (the fixnum (* bytes-per-row y)))
	(civ-init (complex (the double-float (- (* y inverse-h) 1.0d0))
		           (the double-float (- (* y inverse-h) 1.0d0))))
	(bit 0)
	(code 0))
    (declare (type fixnum index bit)
	     (type (unsigned-byte 8) code))
    (loop for x of-type fixnum from 0 below n by 2
          do (let ((two-pixels 3)
                   (crv (aref crvs (ash x -1)))
	           (civ civ-init))
	       (declare (type (complex double-float) crv civ)
		        (type fixnum two-pixels))
	       (escapes? 50 two-pixels crv civ)
	       (setf code (logior (ash code 2) two-pixels))
	       (when (= (incf bit) 4)
	         (setf (aref bitmap index) code
		       bit 0
		       code 0)
	         (incf index))))))

  (defun vops::main ()
    (let* ((args sb-ext:*posix-argv*)
	   (n-input (parse-integer (or (second args) "16000")))
           (n (* (floor (+ n-input 7) 8) 8))
	   (bytes-per-row (ash (the fixnum (+ n 7)) -3))
	   (inverse-w (/ 2.0d0 (the fixnum (ash bytes-per-row 3))))
	   (inverse-h (/ 2.0d0 (the fixnum n)))
	   (crvs
	     (make-array (ash n -1) :element-type '(complex double-float))))
      (declare (type fixnum n bytes-per-row)
	       (type double-float inverse-h inverse-w)
	       (type (simple-array (complex double-float) (*)) crvs))
      (let ((bitmap (make-array (* bytes-per-row n)
			        :initial-element 0
			        :element-type '(unsigned-byte 8))))
	(declare (type (simple-array (unsigned-byte 8) (*)) bitmap))
	(loop for i of-type fixnum from 0 below n by 2 
	   do (setf (aref crvs (ash i -1))
		    (complex (- (* (+ i 1.0d0) inverse-w) 1.5d0)
			     (- (* i inverse-w) 1.5d0))))
      	#-sb-thread
	(loop for y of-type from 0 below n
	   do (calc-row y n bitmap bytes-per-row crvs inverse-h))
	#+sb-thread ;; threaded inner loop changed to inderleaved from block
	(mapcar #'sb-thread:join-thread
		(loop for i of-type fixnum from 0 below +workers+
		      collecting (sb-thread:make-thread
				  (let ((i i))
				    (lambda ()
				      (loop for y of-type fixnum
					    from i below n by +workers+
					    do (calc-row y n
							 bitmap bytes-per-row
							 crvs inverse-h)))))))
        
	(let ((digest (sb-md5:md5sum-sequence bitmap)))
          (with-open-file (stream #p"/dev/stdout" :direction :output
				                  :if-exists :append
				                  :element-type '(unsigned-byte 8))
	    (write-sequence (cbyte (format nil "P4~%~d ~d~%" n n)) stream)
	    (write-sequence (cbyte (format nil "~(~{~2,'0X~}~)~%"
                                           (coerce digest 'list))) stream))))))
      
(in-package :cl-user)
(defun main ()
  (vops::main))
