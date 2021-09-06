;;;   The Computer Language Benchmarks Game
;;;   http://benchmarksgame.alioth.debian.org/
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
;;; modified by Bela Pecsek
;;;   * code modification useing scaling method and use dt 1.0d0
;;;   * calculate magnitude by (/ 1.0d0 (* dist_sq distance))
;;;   * precompute mass * mag instead of dx,y,z * mag 
;;; sbcl --load nbody3.lisp --no-userinit --eval "(setf *block-compile-default* t)" --eval "(save-lisp-and-die \"nbody3.core\" :executable t :purify t :toplevel (lambda () (main) (quit)))"

(declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))
(setf *block-compile-default* t)
(setf sb-ext:*inline-expansion-limit* 1000)
(sb-int:set-floating-point-modes :traps (list :divide-by-zero))
(declaim (sb-ext:muffle-conditions style-warning))

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

(defparameter *sun*
  (make-body 0d0 0d0 0d0 0d0 0d0 0d0 +solar-mass+))

(declaim (inline applyforces))
(defun applyforces (a b)
  (let* ((dx (- (x a) (x b)))
         (dy (- (y a) (y b)))
         (dz (- (z a) (z b)))
	 (posdelta_sq (+ (* dx dx) (* dy dy) (* dz dz)))
	 (distance (sqrt posdelta_sq))
	 (mag (/ (* posdelta_sq distance)))
	 (massmag-b (* mag (mass b)))
	 (massmag-a (* mag (mass a))))
    (declare (body a b)(double-float dx dy dz)
	     ((double-float 0d0) posdelta_sq distance
	      mag massmag-a massmag-b))
    (decf (vx a) (* dx massmag-b))
    (decf (vy a) (* dy massmag-b))
    (decf (vz a) (* dz massmag-b))
    (incf (vx b) (* dx massmag-a))
    (incf (vy b) (* dy massmag-a))
    (incf (vz b) (* dz massmag-a))))

(declaim (inline advance))
(defun advance (system)
  (loop for (a . rest) on system do
    (dolist (b rest)
          (applyforces a b)))
  (dolist (a system)
    (incf (x a) (vx a))
    (incf (y a) (vy a))
    (incf (z a) (vz a)))
  nil)

(declaim (inline energy))
(defun energy (system)
    (let ((e 0d0))
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
              (decf e (/ (* (mass a) (mass b)) dist)))))
    e))

(defun offset-momentum (system)
  (let ((px 0d0)
	(py 0d0)
	(pz 0d0)
	(sun (car system)))
    (dolist (p system)
      (incf px (* (vx p) (mass p)))
      (incf py (* (vy p) (mass p)))
      (incf pz (* (vz p) (mass p))))
    (setf (vx sun) (/ (- px) +solar-mass+)
          (vy sun) (/ (- py) +solar-mass+)
          (vz sun) (/ (- pz) +solar-mass+))
    nil))

(defun body-scale (system scale)
  (dolist (p system)
    (declare (type body p)
	     (type list system))
    (setf (mass p) (* (mass p) (* scale scale))
	  (vx p) (* (vx p) scale)
	  (vy p) (* (vy p) scale)
	  (vz p) (* (vz p) scale))))

(defconstant +DT+ 0.01d0)
(defconstant +RECIP_DT+ (/ 1d0 +DT+))

(defun nbody (n)
  (declare (fixnum n))
  (let ((system (list *sun* *jupiter* *saturn* *uranus* *neptune*)))
    (offset-momentum system)
    (format t "~,9F~%" (energy system))
    (body-scale system +DT+)
    (dotimes (i n)
      (advance system))
    (body-scale system +RECIP_DT+)
    (format t "~,9F~%" (energy system))))

(defun main ()
  (let ((n (parse-integer (or (car (last #+sbcl sb-ext:*posix-argv*
                                         #+cmu  extensions:*command-line-strings*
					 #+gcl  si::*command-args*)) "1"))))
    (nbody n)))
