;;   The Computer Language Benchmarks Game
;;   http://benchmarksgame.alioth.debian.org/
;;;
;;; contributed by Patrick Frankenberger
;;; modified by Juho Snellman 2005-11-18
;;;   * About 40% speedup on SBCL, 90% speedup on CMUCL
;;;   * Represent a body as a DEFSTRUCT with (:TYPE VECTOR DOUBLE-FLOAT), a
;;;     not as a structure that contains vectors
;;;   * Inline APPLYFORCES
;;;   * Replace (/ DT DISTANCE DISTANCE DISTANCE) with
;;;     (/ DT (* DISTANCE DISTANCE DISTANCE)), as is done in the other
;;;     implementations of this test.
;;;   * Add a couple of declarations
;;;   * Heavily rewritten for style (represent system as a list instead of
;;;     an array to make the nested iterations over it less clumsy, use
;;;     INCF/DECF where appropriate, break very long lines, etc)
;;; modified by Marko Kocic 
;;;   * add optimization declarations
;;; modified by Joszef Csanyi - 08/08/2020
;;;   * advance function written using AVX

(declaim (optimize (speed 3) (safety 0) (debug 0)))
(setf *block-compile-default* t)
(setf sb-ext:*inline-expansion-limit* 1000)
(sb-int:set-floating-point-modes :traps (list :divide-by-zero))
(declaim (sb-ext:muffle-conditions style-warning))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AVX inicialisation                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package #:sb-vm)
(eval-when (:compile-toplevel :load-toplevel :execute)
   (defmacro define-double-binary-vop-operation (name avx-operation)
     `(eval-when (:compile-toplevel :load-toplevel :execute)
        (defknown (,name) ((simd-pack-256 double-float)
			   (simd-pack-256 double-float))
	    (simd-pack-256 double-float)
            (movable flushable always-translatable)
          :overwrite-fndb-silently t)
        (define-vop (,name)
          (:translate ,name)
          (:policy :fast-safe)
          (:args (a :scs (double-avx2-reg))
                 (b :scs (double-avx2-reg) :to :save))
          (:arg-types simd-pack-256-double simd-pack-256-double)
          (:results (dest :scs (double-avx2-reg)))
          (:result-types simd-pack-256-double)
          (:generator 4 (inst ,avx-operation dest a b)))))
   (define-double-binary-vop-operation %d4+ vaddpd)
   (define-double-binary-vop-operation %d4* vmulpd)
   (define-double-binary-vop-operation %d4/ vdivpd)

  (defmacro define-double-unary-vop-operation (name avx-operation)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defknown (,name) ((simd-pack-256 double-float))
	   (simd-pack-256 double-float)
           (movable flushable always-translatable)
         :overwrite-fndb-silently t)
       (define-vop (,name)
         (:translate ,name)
         (:policy :fast-safe)
         (:args (a :scs (double-avx2-reg)))
         (:arg-types simd-pack-256-double)
         (:results (dest :scs (double-avx2-reg)))
         (:result-types simd-pack-256-double)
         (:generator 4 (inst ,avx-operation dest a)))))
  (define-double-unary-vop-operation  %d4sqrt vsqrtpd)

  (defknown %d4-ref ((simple-array double-float (*))
                     (integer 0 #.most-positive-fixnum))
      (simd-pack-256 double-float)
      (movable foldable flushable always-translatable)
    :overwrite-fndb-silently t)
  (define-vop (%d4-ref)
    (:translate %d4-ref)
    (:args (v :scs (descriptor-reg))
           (i :scs (any-reg)))
    (:arg-types simple-array-double-float
                tagged-num)
    (:results (dest :scs (double-avx2-reg)))
    (:result-types simd-pack-256-double)
    (:policy :fast-safe)
    (:generator 8 (inst vmovupd dest (float-ref-ea v i 0 8 :scale 4))))

  (defknown %d4-set ((simple-array double-float (*))
		     (integer 0 #.most-positive-fixnum)
		     (simd-pack-256 double-float))
	(simd-pack-256 double-float)
      (always-translatable)
    :overwrite-fndb-silently t)
  (define-vop (%d4-set)
    (:translate %d4-set)
    (:args (v :scs (descriptor-reg))
           (i :scs (any-reg))
           (x :scs (double-avx2-reg) :target dest))
    (:arg-types simple-array-double-float
                tagged-num
                simd-pack-256-double)
    (:results (dest :scs (double-avx2-reg) :from (:argument 2)))
    (:result-types simd-pack-256-double)
    (:policy :fast-safe)
    (:generator 8 (inst vmovupd (float-ref-ea v i 0 8 :scale 4) x)))

  (defknown %vzeroupper () (integer)
      (always-translatable)
    :overwrite-fndb-silently t)
  (define-vop (%vzeroupper)
    (:translate %vzeroupper)
    (:policy :fast-safe)
    (:generator 1 (inst vzeroupper))))

(in-package #:cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-double-binary-operation (name avx-operation)
    `(progn
      (declaim (ftype (function ((simd-pack-256 double-float)
			         (simd-pack-256 double-float))
			        (simd-pack-256 double-float)) ,name)
               (inline ,name))
      (defun ,name (x y)
        (declare (optimize (speed 3) (safety 0) (debug 0))
	         ((simd-pack-256 double-float) x y))
        (,avx-operation x y))))
  (define-double-binary-operation d4+ sb-vm::%d4+)
  (define-double-binary-operation d4* sb-vm::%d4*)
  (define-double-binary-operation d4/ sb-vm::%d4/)

  (defmacro define-double-unary-operation (name avx-operation)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (declaim (ftype (function ((simd-pack-256 double-float))
				 (simd-pack-256 double-float)) ,name)
                (inline ,name))
       (defun ,name (x)
	 (declare (optimize (speed 3) (safety 0) (debug 0))
		  ((simd-pack-256 double-float) x))
	 (,avx-operation x))))
  (define-double-unary-operation d4sqrt sb-vm::%d4sqrt)
  (declaim (ftype (function ((simple-array double-float (*))
			     (integer 0 #.most-positive-fixnum))
			    (simd-pack-256 double-float)) d4-aref)
           (inline d4-aref))
  (defun d4-aref (v i)
    (declare (optimize (speed 3) (safety 0) (debug 0))
	     ((simple-array double-float (*)) v)
	     ((integer 0 #.most-positive-fixnum) i))
    (sb-vm::%d4-ref v i))

  (declaim (ftype (function ((simd-pack-256 double-float)
			     (simple-array double-float (*))
			     (integer 0 #.most-positive-fixnum))
			    (simd-pack-256 double-float)) (setf d4-aref))
           (inline (setf d4-aref)))
  (defun (setf d4-aref) (new-value v i)
    (declare (optimize (speed 3) (safety 0) (debug 0))
	     ((simple-array double-float (*)) v)
	     ((integer 0 #.most-positive-fixnum) i)
	     ((simd-pack-256 double-float) new-value))
    (sb-vm::%d4-set v i new-value))

  (declaim (ftype (function (double-float double-float
			                  double-float double-float)
			    (simd-pack-256 double-float)) %make-avx-double)
           (inline %make-avx-double))
  (defun %make-avx-double (a b c d)
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (sb-vm::%make-simd-pack-256-double a b c d))

  (declaim (ftype (function () integer) vzeroupper)
           (inline vzeroupper))
  (defun vzeroupper ()
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (sb-vm::%vzeroupper))

  (defmacro define-constant (name value &optional doc)
    `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
       ,@(when doc (list doc)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package #:cl-user)

(defconstant +days-per-year+ 365.24d0)
(defconstant +solar-mass+ (* 4d0 pi pi))

(defstruct (body (:type (vector double-float))
                 (:conc-name nil)
                 (:constructor make-body (x y z vx vy vz mass)))
  x y z vx vy vz mass)

(deftype body () '(vector double-float 7))

(defparameter *jupiter*
  (make-body 4.84143144246472090d0
             -1.16032004402742839d0
             -1.03622044471123109d-1
             (* 1.66007664274403694d-3 +days-per-year+)
             (* 7.69901118419740425d-3 +days-per-year+)
             (* -6.90460016972063023d-5  +days-per-year+)
             (* 9.54791938424326609d-4 +solar-mass+)))

(defparameter *saturn*
  (make-body 8.34336671824457987d0
             4.12479856412430479d0
             -4.03523417114321381d-1
             (* -2.76742510726862411d-3 +days-per-year+)
             (* 4.99852801234917238d-3 +days-per-year+)
             (* 2.30417297573763929d-5 +days-per-year+)
             (* 2.85885980666130812d-4 +solar-mass+)))

(defparameter *uranus*
  (make-body 1.28943695621391310d1
             -1.51111514016986312d1
             -2.23307578892655734d-1
             (* 2.96460137564761618d-03 +days-per-year+)
             (* 2.37847173959480950d-03 +days-per-year+)
             (* -2.96589568540237556d-05 +days-per-year+)
             (* 4.36624404335156298d-05 +solar-mass+)))

(defparameter *neptune*
  (make-body 1.53796971148509165d+01
             -2.59193146099879641d+01
             1.79258772950371181d-01
             (* 2.68067772490389322d-03 +days-per-year+)
             (* 1.62824170038242295d-03 +days-per-year+)
             (* -9.51592254519715870d-05 +days-per-year+)
             (* 5.15138902046611451d-05 +solar-mass+)))

(defparameter *sun* (make-body 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 +solar-mass+))

(defconstant +bodies_count+ 5)
(defconstant +interactions_count+ (/ (* +bodies_count+ (1- +bodies_count+)) 2))
(defconstant +ric+ (+ +interactions_count+ (mod +interactions_count+ 4)))
(defconstant +dt+  0.01d0)
(defconstant +recip_dt+ (/ 1.0d0 +dt+))

(deftype magnitudes () `(simple-array double-float (,+ric+)))
(deftype position_deltas () `(simple-array double-float (,+ric+)))

(declaim (inline advance))
(defun advance (system times)
  (declare (list system)
	   (fixnum times))
  (let ((pdx (make-array +ric+ :element-type 'double-float :initial-element 1.0d0))
	(pdy (make-array +ric+ :element-type 'double-float :initial-element 1.0d0))
	(pdz (make-array +ric+ :element-type 'double-float :initial-element 1.0d0))
        (mag (make-array +ric+ :element-type 'double-float :initial-element 1.0d0)))
    (declare (position_deltas pdx pdy pdz)
             (magnitudes mag))
    (loop repeat times do
      (loop with i of-type fixnum = 0
	    for (a . rest) on system
	    do (dolist (b rest)
		 (declare (position_deltas pdx pdy pdz))
		 (setf (aref pdx i) (- (x a) (x b))
		       (aref pdy i) (- (y a) (y b))
		       (aref pdz i) (- (z a) (z b)))
		 (incf i)))
      (loop for i of-type fixnum below +ric+ by 4
	    do (let* ((%dx (d4-aref pdx i))
		      (%dy (d4-aref pdy i))
		      (%dz (d4-aref pdz i))
		      (%posdelta_sq (d4+ (d4+ (d4* %dx %dx) (d4* %dy %dy))
					 (d4* %dz %dz)))
		      (%dist (d4sqrt %posdelta_sq))
		      (%mag (d4/ (%make-avx-double 1.0d0 1.0d0 1.0d0 1.0d0)
                                 (d4* %dist %posdelta_sq))))
                 (setf (d4-aref mag i) %mag)
                 (vzeroupper))) 
      (loop with i of-type fixnum = 0
	    for (a . rest) on system
	    do (dolist (b rest)
		 (declare (magnitudes mag)
			  (position_deltas pdx pdy pdz))
		 (let* ((pdx-i (aref pdx i))
			(pdy-i (aref pdy i))
			(pdz-i (aref pdz i))
			(mag-i (aref mag i))
			(massmag-a (* (mass a) mag-i))  
			(massmag-b (* (mass b) mag-i)))
		   (declare (double-float pdx-i pdy-i pdz-i)
			    ((double-float 0.0d0) massmag-a massmag-b))
		   (decf (vx a) (* pdx-i massmag-b)) 
	           (decf (vy a) (* pdy-i massmag-b))
	           (decf (vz a) (* pdz-i massmag-b))
	           (incf (vx b) (* pdx-i massmag-a))
	           (incf (vy b) (* pdy-i massmag-a)) 
	           (incf (vz b) (* pdz-i massmag-a)))	               
		 (incf i)))
      (dolist (a system)
	(declare (body a))
	(incf (x a) (vx a))
	(incf (y a) (vy a))
	(incf (z a) (vz a))))))

(defun energy (system)
  (declare (list system))
    (let ((e 0.0d0))
      (declare (double-float e))
    (loop for (a . rest) on system do
          (incf e (* 0.5d0 (mass a)
                     (+ (* (vx a) (vx a))
                        (* (vy a) (vy a))
                        (* (vz a) (vz a)))))
          (dolist (b rest)
            (let* ((dx (- (x a) (x b)))
                   (dy (- (y a) (y b)))
                   (dz (- (z a) (z b)))
                   (dist (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))
	      (declare ((double-float 0.0d0) dist dx dy dz))
              (decf e (/ (* (mass a) (mass b)) dist)))))
    e))

(defun offset-momentum (system)
  (let ((px 0.0d0)
	(py 0.0d0)
	(pz 0.0d0)
        (sun (car system)))
    (declare (body sun)
	     ((double-float 0.0d0) px py pz))
    (dolist (p system)
      (incf px (* (vx p) (mass p)))
      (incf py (* (vy p) (mass p)))
      (incf pz (* (vz p) (mass p))))
    (setf (vx sun) (/ (- px) +solar-mass+)
          (vy sun) (/ (- py) +solar-mass+)
          (vz sun) (/ (- pz) +solar-mass+))
    nil))

(defun body-scale (system scale)
  (declare (list system)
	   ((double-float 0.0d0)  scale))
  (dolist (p system)
    (declare (body p))
    (setf (mass p) (* (mass p) (* scale scale))
	  (vx p) (* (vx p) scale)
	  (vy p) (* (vy p) scale)
	  (vz p) (* (vz p) scale))
    nil))

(defun nbody (n)
  (let ((system (list *sun* *jupiter* *saturn* *uranus* *neptune*)))
    (declare (list system)
	     (fixnum n))
    (offset-momentum system)
    (format t "~,9f~%" (energy system))
    (body-scale system +dt+)
    (advance system n)
    (body-scale system +recip_dt+)
    (format t "~,9f~%" (energy system))))

(defun main ()
  (let ((n (parse-integer (or (car (last #+sbcl sb-ext:*posix-argv*)) "1"))))
    (nbody n)))
