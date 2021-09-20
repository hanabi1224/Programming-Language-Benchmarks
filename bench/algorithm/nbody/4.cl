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
;;; modified by Béla Pecsek - 2021-09-11
;;;   * converted to use sb-simd
;;;   * further optimization using sb-simd f64 scalar operations

(declaim (optimize (speed 3) (safety 0) (debug 0)))
(setf *block-compile-default* t)

(in-package #:cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :sb-simd :silent t)
  (use-package :sb-simd-avx2)

  (defconstant +DAYS-PER-YEAR+ 365.24d0)
  (defconstant +SOLAR-MASS+ (* 4d0 pi pi))

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

  (defparameter *sun* (make-body 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0
			         +solar-mass+))
  (defparameter *system* (list *sun* *jupiter* *saturn* *uranus* *neptune*)))

;; Declarations and iteration of 'advance' function is based on GCC#8 and GCC#2
;; Figure out how many total different interactions there are between each
;; body and every other body. Some of the calculations for these
;; interactions will be calculated two at a time by using x86 SSE
;; instructions and because of that it will also be useful to have a
;; ROUNDED_INTERACTIONS_COUNT that is equal to the next highest even number
;; which is equal to or greater than INTERACTIONS_COUNT.
(defconstant +BODIES_COUNT+ (length *system*))
(defconstant +INTERACTIONS_COUNT+ (/ (* +BODIES_COUNT+ (1- +BODIES_COUNT+)) 2))
(defconstant +ROUNDED_INTERACTIONS_COUNT+ (+ +INTERACTIONS_COUNT+
					     (mod +INTERACTIONS_COUNT+ 4)))
;; Based on GCC #2
(defconstant +DT+ 0.01d0)
(defconstant +RECIP_DT+ (/ +dt+))

;; Type declarations
(deftype position_deltas () `(simple-array f64 (,+ROUNDED_INTERACTIONS_COUNT+)))
(deftype magnitudes      () `(simple-array f64 (,+ROUNDED_INTERACTIONS_COUNT+)))

;; Based on GCC #8 and GCC #2
;; Advances with timestem dt = 1.0d0
;; Advance all the bodies in the system by one timestep. Calculate the
;; interactions between all the bodies, update each body's velocity based on
;; those interactions, and update each body's position by the distance it
;; travels in a timestep of 1.0d0 at it's updated velocity.
;; 3 simple arrays are used to stope position delta components for speed
;; Force magnitudes are stores in array magnitude
(defun advance (system n)
  (declare (type list system)
	   (type fixnum n))
  (let ((position_Deltas-x (make-array +ROUNDED_INTERACTIONS_COUNT+
			 :element-type 'f64 :initial-element 1.0d0))
	(position_Deltas-y (make-array +ROUNDED_INTERACTIONS_COUNT+
			 :element-type 'f64 :initial-element 1.0d0))
	(position_Deltas-z (make-array +ROUNDED_INTERACTIONS_COUNT+
			 :element-type 'f64 :initial-element 1.0d0))
        (magnitudes (make-array +ROUNDED_INTERACTIONS_COUNT+
			 :element-type 'f64 :initial-element 1.0d0)))
    (flet ((position-deltas ()
	     ;; Calculate the position_Deltas between the bodies for each
	     ;; interaction.
	     (loop with i of-type fixnum = 0 for (a . rest) on system 
		   do (dolist (b rest)
			;; (declare (type position_deltas pdx pdy pdz))
			(setf (aref position_Deltas-x i) (f64- (x a) (x b))
			      (aref position_Deltas-y i) (f64- (y a) (y b))
			      (aref position_Deltas-z i) (f64- (z a) (z b)))
			(incf i))))
	   (force-magnitudes ()
	     ;; Calculate the magnitudes of force between the bodies for each
	     ;; interaction. This loop processes two interactions at a time
	     ;; which is why ROUNDED_INTERACTIONS_COUNT/2 iterations are done.
	     (loop for i of-type fixnum below +ROUNDED_INTERACTIONS_COUNT+ by 4
		   do (let* ((pdx (f64.4-aref position_Deltas-x i))
			     (pdy (f64.4-aref position_Deltas-y i))
			     (pdz (f64.4-aref position_Deltas-z i))
			     (distance_Squared (f64.4+ (f64.4* pdx pdx)
                                                       (f64.4* pdy pdy)
						       (f64.4* pdz pdz)))
                             (distance_Reciprocal (f64.4-sqrt distance_Squared))
			     (magnitude (f64.4/ (f64.4* distance_Reciprocal
						        distance_Squared))))
			(setf (f64.4-aref magnitudes i) magnitude)))
             (vzeroupper))
	   (velocities ()
	     ;; Use the calculated magnitudes of force to update the velocities
	     ;; for all of the bodies.
	     (loop with i of-type fixnum = 0 for (a . rest) on system
		   do (dolist (b rest)
			(let* ((pdx-i (aref position_Deltas-x i))
			       (pdy-i (aref position_Deltas-y i))
			       (pdz-i (aref position_Deltas-z i))
			       (mag-i (aref magnitudes i))
			       ;; Precompute products of mass and magnitude
			       ;; to be reused a couple of times
			       (mass*mag-a (f64* (mass a) mag-i))  
			       (mass*mag-b (f64* (mass b) mag-i)))
			  (decf (vx a) (f64* pdx-i mass*mag-b)) 
			  (decf (vy a) (f64* pdy-i mass*mag-b))
			  (decf (vz a) (f64* pdz-i mass*mag-b))
			  (incf (vx b) (f64* pdx-i mass*mag-a))
			  (incf (vy b) (f64* pdy-i mass*mag-a)) 
			  (incf (vz b) (f64* pdz-i mass*mag-a)))	               
			(incf i))))
	   (positions ()
	     ;; Use the updated velocities to update the positions for all
	     ;; bodies.
	     (dolist (a system)
	       (incf (x a) (vx a))
	       (incf (y a) (vy a))
	       (incf (z a) (vz a)))))
      (declare (inline position-deltas force-magnitudes velocities positions))
      (loop repeat n do (progn (position-deltas) (force-magnitudes)
			       (velocities) (positions))))))

;; Output the total energy of the system.
(defun output_Energy (system)
    (let ((e 0.0d0))
      (declare (type f64 e))
      (loop for (a . rest) on system do
	;; Add the kinetic energy for each body.
        (incf e (f64* 0.5d0 (mass a)
                      (f64+ (f64* (vx a) (vx a))
                            (f64* (vy a) (vy a))
                            (f64* (vz a) (vz a)))))
        (dolist (b rest)
	  ;; Add the potential energy between this body and every other bodies
          (let* ((dx (f64- (x a) (x b)))
                 (dy (f64- (y a) (y b)))
                 (dz (f64- (z a) (z b)))
                 (dist (sqrt (f64+ (f64* dx dx) (f64* dy dy) (f64* dz dz)))))
            (declare (f64 dx dy dz dist))
            (decf e (f64/ (the f64 (f64* (mass a) (mass b))) dist)))))
      (format t "~,9f~%" e))) ;; Output the total energy of the system.

;; Calculate the momentum of each body and conserve momentum of the system by
;; adding to the Sun's velocity the appropriate opposite velocity needed in
;; order to offset that body's momentum.
(defun offset-momentum (system)
  (let ((px 0.0d0)
	(py 0.0d0)
	(pz 0.0d0)
        (sun (car system)))
    (declare (type f64 px py pz))
    (dolist (p system)
      (incf px (f64* (vx p) (mass p)))
      (incf py (f64* (vy p) (mass p)))
      (incf pz (f64* (vz p) (mass p))))
    (setf (vx sun) (f64/ (- px) +solar-mass+)
          (vy sun) (f64/ (- py) +solar-mass+)
          (vz sun) (f64/ (- pz) +solar-mass+))
    nil))

;; Rescale certain properties of bodies. That allows doing
;; consequential advances as if dt were equal to 1.0d0.
;; When all advances done, rescale bodies back to obtain correct energy.
(defun scale_bodies (system scale)
  (declare (type list system)
	   (type f64 scale))
  (dolist (p system)
    (declare (type body p))
    (setf (mass p) (f64* (mass p) (f64* scale scale))
	  (vx p)   (f64* (vx p) scale)
	  (vy p)   (f64* (vy p) scale)
	  (vz p)   (f64* (vz p) scale))
    nil))

(defun nbody (n)
  (let ((system *system*))
    (offset-momentum system)         ;; Output initial energy of the system
    (output_Energy system)           ;; Scale bodies to use time step of 1.0d0
    (scale_bodies system +DT+)       ;; Advance system with time step of 1.0d0
    (advance system n)               ;; Advance system n times
    (scale_bodies system +RECIP_DT+) ;; Rescale bodies
    (output_Energy system)))         ;; Output final energy of the system

(defun main ()
  (let ((n (parse-integer (or (car (last #+sbcl sb-ext:*posix-argv*
                                         #+cmu  extensions:*command-line-strings*
					 #+gcl  si::*command-args*)) "1"))))
    (nbody n)))
