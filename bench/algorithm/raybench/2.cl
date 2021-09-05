(declaim (optimize (speed 3) (safety 0) (space 0) (debug 0) (compilation-speed 0)))

;(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))

(defconstant +width+ 1280)
(defconstant +height+ 720)
(defconstant +samples+ 50)
(defconstant +max-depth+ 5)
;; float type can be specified externally by defining this on command line
;; with something like --eval "(defconstant +float-type+ 'double-float)"
(defconstant +float-type+ (if (boundp '+float-type+)
                              +float-type+
                              'single-float))
(setf *read-default-float-format* +float-type+)
(deftype float-type () +float-type+)
(deftype vec-type () '(simple-array float-type (3)))


(declaim (inline v v-x v-y v-z))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (vec (:conc-name v-)
                  (:constructor v (x y z))
                  (:type (vector float-type)))
    x y z))

(defconstant +zero+ (if (boundp '+zero+)
                        +zero+
                        (v 0.0 0.0 0.0)))

(defmacro define-v-op (name (a b) op &optional scalar)
  `(progn
     (declaim (inline ,name))
     (defun ,name (,a ,b)
       (declare (type vec-type ,a)
                (type ,(if scalar 'float-type 'vec-type) ,b))
       (v ,@(loop for acc in '(v-x v-y v-z)
                  collect `(,op (,acc ,a)
                                ,(if scalar b `(,acc ,b ))))))))

(define-v-op v-add (v1 v2) +)
(define-v-op v-sub (v1 v2) -)
(define-v-op v-mul (v1 v2) *)
(define-v-op v-mul-s (v1 s) * t)
(define-v-op v-div (v1 v2) /)
(define-v-op v-div-s (v1 s) / t)

(declaim (inline v-dot v-norm v-unit))
(defun v-dot (v1 v2)
  (declare (type vec-type v1 v2))
  (+ (* (v-x v1) (v-x v2))
     (* (v-y v1) (v-y v2))
     (* (v-z v1) (v-z v2))))

(defun v-norm (v1)
  (declare (type vec-type v1))
  (let ((d (v-dot v1 v1)))
    (if (minusp d) 0.0 (sqrt d))))

(defun v-unit (v1)
  (declare (type vec-type v1))
  (v-div-s v1 (v-norm v1)))

(defstruct (ray
            (:conc-name ray-)
            (:constructor ray-new (origin direction)))
  (origin +zero+ :type vec-type)
  (direction +zero+ :type vec-type))

(declaim (inline ray-point))
(defun ray-point (ray dist)
  (declare (type ray)
           (type float-type dist))
  (v-add (ray-origin ray)
         (v-mul-s (ray-direction ray)
                  dist)))

(declaim (inline sphere-radius))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (sphere
              (:conc-name sphere-)
              (:constructor sphere-new (center radius color is-light)))
    (center +zero+ :type vec-type)
    (radius 0.0 :type float-type)
    (color +zero+ :type vec-type)
    (is-light nil :type (or t nil))))


(declaim (inline hit-distance))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (hit
              (:conc-name hit-)
              (:constructor hit-new (distance point normal sphere)))
    (distance 0.0 :type float-type)
    (point +zero+ :type vec-type)
    (normal +zero+ :type vec-type)
    (sphere (sphere-new) :type sphere)))

(defstruct (camera
            (:conc-name camera-)
            (:constructor camera-new (eye lt rt lb)))
  (eye +zero+ :type vec-type)
  (lt (v -1.0 1.0 1.0) :type vec-type)
  (rt (v 1.0 1.0 1.0) :type vec-type)
  (lb (v -1.0 0.0 1.0) :type vec-type))


(defconstant +no-hit+ (if (boundp '+no-hit+)
                          +no-hit+
                          (hit-new 1e16 +zero+ +zero+
                                   (sphere-new +zero+ 0.0 +zero+ nil))))

(defun sphere-hit (sphere ray)
  (declare (type sphere sphere)
           (type ray ray))
  (let* ((oc (v-sub (ray-origin ray) (sphere-center sphere)))
         (dir (ray-direction ray))
         (a (v-dot dir dir))
         (b (v-dot oc dir))
         (c (- (v-dot oc oc)
               (* (sphere-radius sphere) (sphere-radius sphere))))
         (dis (- (* b b) (* a c))))
    (if (> dis 0.0)
        (let* ((e (sqrt dis))
               (t1 (/ (- (- b) e) a)))
          (if (> t1 0.007)
              (let ((point (ray-point ray t1)))
                (hit-new t1 point
                         (v-unit (v-sub point (sphere-center sphere)))
                         sphere))
              (let ((t2 (/ (+ (- b) e) a)))
                (if (> t2 0.007)
                    (let ((point (ray-point ray t2)))
                      (hit-new t2 point
                               (v-unit (v-sub point (sphere-center sphere)))
                               sphere))
                    +no-hit+))))
        +no-hit+)))


(defun world-new ()
  (list (camera-new (v 0.0 4.5 75.0)
                    (v -8.0 9.0 50.0)
                    (v 8.0 9.0 50.0)
                    (v -8.0 0.0 50.0))
        (list (sphere-new (v 0.0 -10002.0 0.0) 9999.0 (v 1.0 1.0 1.0) nil)
              (sphere-new (v -10012.0 0.0 0.0) 9999.0 (v 1.0 0.0 0.0) nil)
              (sphere-new (v 10012.0 0.0 0.0) 9999.0 (v 0.0 1.0 0.0) nil)
              (sphere-new (v 0.0 0.0 -10012.0) 9999.0 (v 1.0 1.0 1.0) nil)
              (sphere-new (v 0.0 10012.0 0.0) 9999.0 (v 1.0 1.0 1.0) t)
              (sphere-new (v -5.0 0.0 2.0) 2.0 (v 1.0 1.0 0.0) nil)
              (sphere-new (v 0.0 5.0 -1.0) 4.0 (v 1.0 0.0 0.0) nil)
              (sphere-new (v 8.0 5.0 -1.0) 2.0 (v 0.0 0.0 1.0) nil))))

(defun world-camera (world)
  (first world))

(defun world-spheres (world)
  (second world))

;;https://codingforspeed.com/using-faster-psudo-random-generator-xorshift/
(declaim (ftype (function () (values (unsigned-byte 32) &optional)) xor128))
(let ((x 123456789)
      (y 362436069)
      (z 521288629)
      (w 88675123))
  (declare (type (unsigned-byte 32) x y z w))
  (defun xor128 ()
    (let ((temp (ldb (byte 32 0) (logxor x (ash x 11)))))
      (declare (type (unsigned-byte 32) temp))
      (shiftf x y z w
              (logxor w
                      (ldb (byte 13 19) w)
                      (logxor temp
                              (ldb (byte 24 8) temp))))
      w)))
(declaim (inline randf))
(defun randf ()
  (/ (float (xor128) 1.0) #.(float #xffffffff 1.0)))

(defun rnd-dome (normal)
  (declare (type vec-type normal))
  (loop for p = (v-unit (v (- (* 2.0 (randf)) 1.0)
                           (- (* 2.0 (randf)) 1.0)
                           (- (* 2.0 (randf)) 1.0)))
        unless (<= (v-dot p normal) 0.0)
          return p))

(defun trace-ray (world ray depth)
  (declare (type ray ray) (type fixnum depth))
  (let* ((hit (loop with hit = +no-hit+
                    for sp in (world-spheres world)
                    for res = (sphere-hit sp ray)
                    when (and (not (eq res +no-hit+))
                              (> (hit-distance res) 0.0001)
                              (< (hit-distance res)
                                 (hit-distance hit)))
                      do (setf hit res)
                    finally (return hit)))
         (color (sphere-color (hit-sphere hit))))
    (cond
      ((or (eq hit +no-hit+)
           (>= depth +max-depth+))
       +zero+)
      ((sphere-is-light (hit-sphere hit))
       color)
      (t
       (let* ((nray (ray-new (hit-point hit) (rnd-dome (hit-normal hit))))
              (ncolor (trace-ray world nray (1+ depth)))
              (at (v-dot (ray-direction nray) (hit-normal hit))))
         (v-mul color (v-mul-s ncolor at)))))))

(declaim (inline to-255))
(defun to-255 (color)
  (map 'list #'floor (v-mul-s color 255.99)))

(defun writeppm (data)
  (format *standard-output* "P3~%~A ~A~%255~%" +width+ +height+)
  (loop for row in data
        do (loop for color in row
                 do (format *standard-output* "~{~A ~}" (to-255 color)))
           (format *standard-output* "~%")))

;; (defun writeppm (data)
;;   (with-open-file (ppm "lisprb-opt0.ppm" :direction :output :if-exists :supersede)
;;     (format ppm "P3~%~A ~A~%255~%" +width+ +height+)
;;     (loop for row in data
;;           do (loop for color in row
;;                    do (format ppm "~{~A ~}" (to-255 color)))
;;              (format ppm "~%"))))

(defun main ()
  (let* ((world (world-new))
         (camera (world-camera world))
         (lt (camera-lt camera))
         (vdu (v-div-s (v-sub (camera-rt camera) (camera-lt camera))
                       (float +width+ 1.0)))
         (vdv (v-div-s (v-sub (camera-lb camera) (camera-lt camera))
                       (float +height+ 1.0)))
         (data (loop
                 for y from 0 below +height+
                 collect
                 (loop
                   for x from 0 below +width+
                   collect
                   (let ((color +zero+)
                         (ray (ray-new (camera-eye camera) +zero+))
                         (dir nil))
                     (loop
                       repeat +samples+
                       do (setf dir (v-add
                                     (v-add lt
                                            (v-mul-s vdu (+ (float x 1.0)
                                                            (randf))))
                                     (v-mul-s vdv (+ (float y 1.0)
                                                     (randf)))))
                          (setf (ray-direction ray)
                                (v-unit (v-sub dir (ray-origin ray))))
                          (setf color (v-add color
                                             (trace-ray world ray 0))))
                     (v-div-s color (float +samples+ 1.0)))))))
    data))

(defun main ()
  (let ((data (produce-data)))
    (writeppm data)))

(defun dump ()
  (progn (sb-ext:disable-debugger)
         (sb-ext:save-lisp-and-die "lisprb-opt0" :purify t :toplevel #'main :executable t)))
