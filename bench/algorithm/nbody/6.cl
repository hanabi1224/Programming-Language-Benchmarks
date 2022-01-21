;;; Based on 2.zig
;;; Converted to Common Lisp by Bela Pecsek - 2021-12-04
;;;   * Code cleanup and refactoring - 2021-12-06
;;;   * Slight optimization - 2021-12-07
(declaim (optimize (speed 3) (safety 0) (debug 0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :sb-simd :silent t)
  (use-package :sb-simd-fma))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +DAYS-PER-YEAR+ 365.24d0)
  (defconstant +SOLAR-MASS+ (* 4d0 pi pi))

  (deftype body () '(simple-array f64 (9)))
  (declaim (inline x y z vx vy vz mas make-body))
  (defstruct (body (:type (vector f64))
                   (:conc-name nil)
                   (:constructor make-body (x y z fill1 vx vy vz fill2 mas)))
    x y z fill1 vx vy vz fill2 mas)

  (declaim (ftype (function (body) f64.4) pos vel)
           (inline pos vel mass set-pos (setf pos) set-vel (setf vel) set-mass (setf mass)))
  (defun pos (body)
    (f64.4-aref body 0))
  (defun vel (body)
    (f64.4-aref body 4))
  (declaim (ftype (function (body f64.4) f64.4) set-pos (setf pos) set-vel (setf vel)))
  (defun set-pos (body new-value)
    (setf (f64.4-aref body 0) new-value))
  (defsetf pos set-pos)
  (defun set-vel (body new-value)
    (setf (f64.4-aref body 4) new-value))
  (defsetf vel set-vel)
  (declaim (ftype (function (body) f64) mass))
  (defun mass (body)
    (f64-aref body 8))
  (declaim (ftype (function (body f64) f64) set-mass (setf mass)))
  (defun set-mass (body new-value)
    (setf (f64-aref body 8) new-value))
  (defsetf mass set-mass)

  (defparameter *jupiter*
    (make-body 4.84143144246472090d0
               -1.16032004402742839d0
               -1.03622044471123109d-1 0d0
               (* 1.66007664274403694d-3 +days-per-year+)
               (* 7.69901118419740425d-3 +days-per-year+)
               (* -6.90460016972063023d-5  +days-per-year+) 0d0
               (* 9.54791938424326609d-4 +solar-mass+)))

  (defparameter *saturn*
    (make-body 8.34336671824457987d0
               4.12479856412430479d0
               -4.03523417114321381d-1 0d0
               (* -2.76742510726862411d-3 +days-per-year+)
               (* 4.99852801234917238d-3 +days-per-year+)
               (* 2.30417297573763929d-5 +days-per-year+) 0d0
               (* 2.85885980666130812d-4 +solar-mass+)))

  (defparameter *uranus*
    (make-body 1.28943695621391310d1
               -1.51111514016986312d1
               -2.23307578892655734d-1 0d0
               (* 2.96460137564761618d-03 +days-per-year+)
               (* 2.37847173959480950d-03 +days-per-year+)
               (* -2.96589568540237556d-05 +days-per-year+) 0d0
               (* 4.36624404335156298d-05 +solar-mass+)))

  (defparameter *neptune*
    (make-body 1.53796971148509165d+01
               -2.59193146099879641d+01
               1.79258772950371181d-01 0d0
               (* 2.68067772490389322d-03 +days-per-year+)
               (* 1.62824170038242295d-03 +days-per-year+)
               (* -9.51592254519715870d-05 +days-per-year+) 0d0
               (* 5.15138902046611451d-05 +solar-mass+)))

  (defparameter *sun* (make-body 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0d0 0d0 0.0d0
                                 +solar-mass+))
  (defparameter *system* (list *sun* *jupiter* *saturn* *uranus* *neptune*)))

;; Helper functions
(declaim (ftype (function (f64.4 f64.4) f64) dot)
         (inline dot length-sq length_))
(defun dot (a b)
  (f64.4-hsum (f64.4* a b)))

(declaim (ftype (function (f64.4) f64) length-sq  length_))
(defun length-sq (a)
  (dot a a))

(defun length_ (a)
  (sqrt (length-sq a)))

;; Calculate the momentum of each body and conserve momentum of the system by
;; adding to the Sun's velocity the appropriate opposite velocity needed in
;; order to offset that body's momentum.
(declaim (ftype (function (list) null) offset-momentum))
(defun offset-momentum (system)
  (loop for bi in system
        with pos = (f64.4 0)
        with sun = (car system) do
          (setf pos (f64.4-fmadd213 (vel bi) (mass bi) pos)
                (vel sun) (f64.4* pos (/ (- +SOLAR-MASS+))))))

;; Advances with timestem dt = 1.0d0
;; Advance all the bodies in the system by one timestep. Calculate the
;; interactions between all the bodies, update each body's velocity based on
;; those interactions, and update each body's position by the distance it
;; travels in a timestep of 1.0d0 at it's updated velocity.
(declaim (ftype (function (list u32) null) advance))
(defun advance (system n)
  (loop repeat n do
    (loop for (bi . rest) on system do
      (dolist (bj rest)
        (let* ((pd  (f64.4- (pos bi) (pos bj)))
               (dsq (f64.4  (length-sq pd)))
               (dst (f64.4-sqrt dsq))
               (mag (f64.4/ (f64.4* dsq dst)))
               (pd-mag (f64.4* pd mag)))
          (setf (vel bi) (f64.4-fnmadd213 pd-mag (mass bj) (vel bi))
                (vel bj) (f64.4-fmadd213  pd-mag (mass bi) (vel bj))))))
    (loop for b in system do
      (f64.4-incf (pos b) (vel b)))))

;; Output the total energy of the system.
(declaim (ftype (function (list) null) energy))
(defun energy (system)
  (loop for (bi . rest) on system
        with e of-type f64 = 0d0
          ;; Add the kinetic energy for each body.
        do (f64-incf e (f64* 0.5d0 (mass bi) (length-sq (vel bi))))
           (dolist (bj rest)
             (declare (type body bj))
             ;; Add the potential energy between this body and every other bodies
            (f64-decf e (f64/ (f64* (mass bi) (mass bj))
                              (length_ (f64.4- (pos bi) (pos bj))))))
        finally (format t "~,9f~%" e)))

;; Rescale certain properties of bodies. That allows doing
;; consequential advances as if dt were equal to 1.0d0.
;; When all advances done, rescale bodies back to obtain correct energy.
(defconstant +DT+ 0.01d0)
(defconstant +RECIP-DT+ #.(f64/ +dt+))
(declaim (ftype (function (list f64) null) scale-bodies))
(defun scale-bodies (system scale)
  (dolist (bi system)
    (declare (type body bi))
    (setf (mass bi) (f64*   (mass bi) (f64* scale scale))
          (vel  bi) (f64.4* (vel bi) scale))))

(declaim (ftype (function (u32) null) nbody))
(defun nbody (n-times)
  (let ((system *system*))
    (offset-momentum system)
    (energy system)                     ;; Output initial energy of the system
    (scale-bodies system +DT+)          ;; Scale bodies to use unity time step
    (advance system n-times)            ;; Advance system n times
    (scale-bodies system +RECIP-DT+)    ;; Rescale bodies back to original
    (energy system)))                   ;; Output final energy of the system

(defun main (&optional n-supplied)
  (let ((n (or n-supplied (parse-integer (or (car (last #+sbcl sb-ext:*posix-argv*))
                                             "10000")))))
    (nbody n)))
