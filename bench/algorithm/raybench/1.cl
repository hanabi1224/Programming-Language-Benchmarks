(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defconstant +width+ 1280)
(defconstant +height+ 720)
(defconstant +samples+ 50)
(defconstant +max-depth+ 6)
(defconstant +float-type+ 'single-float)

(setf *read-default-float-format* +float-type+)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype float-type (&optional low high) `(,+float-type+ ,low ,high))
  (deftype vec-type () '(simple-array float-type (3)))

  (declaim (inline zero))
  (defun zero ()
    (ecase +float-type+
      (single-float 0.0)
      (double-float 0.0d0)))

  (declaim (inline %v v-x v-y v-z))
  (defstruct (vec (:conc-name v-)
                  (:constructor %v (x y z))
                  (:type (vector float-type)))
    (x (zero) :type float-type)
    (y (zero) :type float-type)
    (z (zero) :type float-type))

  (declaim (inline v))
  (defun v (x y z &optional output)
    (declare (type (or vec-type null)))
    (if output
        (progn
          (setf (v-x output) (coerce x +float-type+)
                (v-y output) (coerce y +float-type+)
                (v-z output) (coerce z +float-type+))
          output)
        (%v (coerce x +float-type+)
            (coerce y +float-type+)
            (coerce z +float-type+)))))

(defmacro define-v-op (name (a b) op &optional scalar)
  (let ((destructive-name (intern (format nil "~A!" name))))
    `(progn
       (declaim (ftype (function (vec-type ,(if scalar 'float-type 'vec-type))
                                 vec-type) ,name ,destructive-name)
                (inline ,name ,destructive-name))
       (defun ,name (,a ,b)
         (v ,@(loop for acc in '(v-x v-y v-z)
                    collect `(,op (,acc ,a)
                                  ,(if scalar b `(,acc ,b ))))))
       (defun ,destructive-name (,a ,b)
         "destructively modify the first argument, then return it."
         ,@(loop for acc in '(v-x v-y v-z)
                 collect `(setf (,acc ,a)
                                (,op (,acc ,a)
                                     ,(if scalar b `(,acc ,b )))))
         ,a))))

(define-v-op v-add (v1 v2) +)
(define-v-op v-sub (v1 v2) -)
(define-v-op v-mul (v1 v2) *)
(define-v-op v-mul-s (v1 s) * t)
(define-v-op v-div (v1 v2) /)
(define-v-op v-div-s (v1 s) / t)

(eval-when (:compile-toplevel)
  (sb-c:defknown (%sqrt)
      ((single-float 0.0)) (single-float 0.0)
      (sb-c:movable sb-c:foldable sb-c:flushable sb-c:always-translatable))
  (sb-c:defknown (%fma231ss)
      (single-float single-float single-float) single-float
      (sb-c:movable sb-c:foldable sb-c:flushable sb-c:always-translatable))
  (sb-c:define-vop (fsqrt/s)
    (:translate %sqrt)
    (:policy :fast-safe)
    (:args (x :scs (sb-vm::single-reg)))
    (:results (y :scs (sb-vm::single-reg)))
    (:arg-types single-float)
    (:result-types single-float)
    (:note "inline float arithmetic")
    (:generator 1
                (SB-C::inst SB-X86-64-ASM::sqrtss y x)))
  (sb-c:define-vop (fma231ss)
    (:translate %fma231ss)
    (:policy :fast-safe)
    (:args (a :scs (sb-vm::single-reg) :target d)
           (b :scs (sb-vm::single-reg))
           (c :scs (sb-vm::single-reg)))
    (:results (d :scs (sb-vm::single-reg) :from (:argument 0)))
    (:arg-types single-float single-float single-float)
    (:result-types single-float)
    (:note "inline fused multiply add")
    (:generator 1
                (SB-C::inst SB-X86-64-ASM::vfmadd231ss d b c))))

(declaim (ftype (function ((single-float 0.0)) (single-float 0.0)) fsqrt)
         (ftype (function (single-float single-float single-float) single-float) fma231ss)
         (ftype (function (vec-type vec-type) single-float) v-dot)
         (ftype (function (vec-type) (single-float 0.0)) v-norm)
         (ftype (function (vec-type) vec-type) v-unit)
         (ftype (function (vec-type) vec-type) v-unit!)
         (inline fsqrt fma231ss v-dot v-norm v-unit v-unit!))
(defun fsqrt (x) (%sqrt x))
(defun fma231ss (a b c) (%fma231ss a b c))

(defun v-dot (v1 v2)
  (let* ((a (fma231ss 0.0 (v-x v1) (v-x v2)))
         (b (fma231ss a   (v-y v1) (v-y v2))))
    (fma231ss b (v-z v1) (v-z v2))))

(defun v-norm (v1)
  (fsqrt (v-dot v1 v1)))

(defun v-unit (v1)
  (let ((d (v-dot v1 v1)))
    (declare (type (single-float 0.0)))
    (fsqrt d)))

(defun v-unit! (v1)
  (v-div-s! v1 (v-norm v1)))

(declaim (inline ray-new))
(defstruct (ray
            (:constructor ray-new (origin direction)))
  (origin    #.(v 0 0 0) :type vec-type)
  (direction #.(v 0 0 0) :type vec-type))

(declaim (inline ray-point))
(defun ray-point (ray dist &optional (v (v 0 0 0)))
  (declare (type ray)
           (type float-type dist)
           (type vec-type v))
  (replace v (ray-direction ray))
  (v-add! (v-mul-s! v dist)
          (ray-origin ray)))

(declaim (inline sphere-new sphere-radius))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (sphere
              (:constructor sphere-new (center radius color is-light)))
    (center #.(v 0 0 0) :type vec-type)
    (radius 0.0 :type float-type)
    (color  #.(v 0 0 0) :type vec-type)
    (is-light nil :type (or t nil))))

(declaim (inline hit-new hit-distance))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (hit
              (:constructor hit-new (distance point normal sphere)))
    (distance 0.0 :type float-type)
    (point    #.(v 0 0 0) :type vec-type)
    (normal   #.(v 0 0 0) :type vec-type)
    (sphere (make-sphere) :type sphere)))

(declaim (inline camera-new))
(defstruct (camera
            (:constructor camera-new (eye lt rt lb)))
  (eye #.(v 0.0 0.0 0.0)  :type vec-type)
  (lt  #.(v -1.0 1.0 1.0) :type vec-type)
  (rt  #.(v 1.0 1.0 1.0)  :type vec-type)
  (lb  #.(v -1.0 0.0 1.0) :type vec-type))

(defconstant +no-hit+ (if (boundp '+no-hit+)
                          +no-hit+
                          (hit-new 1e16 #.(v 0 0 0) #.(v 0 0 0)
                                   (sphere-new #.(v 0 0 0) 0.0 #.(v 0 0 0) nil))))

(defun sphere-hit (sphere ray output)
  (declare (type sphere sphere)
           (type ray ray)
           (type hit output))
  (let* ((oc (v-sub (ray-origin ray) (sphere-center sphere)))
         (dir (ray-direction ray))
         (a (v-dot dir dir))
         (b (v-dot oc dir))
         (c (- (v-dot oc oc)
               (* (sphere-radius sphere) (sphere-radius sphere))))
         (dis (- (* b b) (* a c))))
    (declare (dynamic-extent oc))
    (if (> dis 0.0)
        (let* ((e (fsqrt dis))
               (t1 (/ (- (- b) e) a)))
          (if (> t1 0.007)
              (let ((point (ray-point ray t1 (hit-point output)))
                    (normal (hit-normal output)))
                (replace normal point)
                (v-unit! (v-sub! normal (sphere-center sphere)))
                (setf (hit-distance output) t1
                      (hit-sphere output) sphere)
                output)
              (let ((t2 (/ (+ (- b) e) a)))
                (if (> t2 0.007)
                    (let ((point (ray-point ray t2 (hit-point output)))
                          (normal (hit-normal output)))
                      (replace normal point)
                      (v-unit! (v-sub! normal (sphere-center sphere)))
                      (setf (hit-distance output) t2
                            (hit-sphere output) sphere)
                      output)
                    nil))))
        nil)))

(defun world-new ()
  (list (camera-new #.(v 0.0 4.5 75.0)
                    #.(v -8.0 9.0 50.0)
                    #.(v 8.0 9.0 50.0)
                    #.(v -8.0 0.0 50.0))
        (list (sphere-new #.(v 0.0 -10002.0 0.0) 9999.0 #.(v 1.0 1.0 1.0) nil)
              (sphere-new #.(v -10012.0 0.0 0.0) 9999.0 #.(v 1.0 0.0 0.0) nil)
              (sphere-new #.(v 10012.0 0.0 0.0) 9999.0 #.(v 0.0 1.0 0.0) nil)
              (sphere-new #.(v 0.0 0.0 -10012.0) 9999.0 #.(v 1.0 1.0 1.0) nil)
              (sphere-new #.(v 0.0 10012.0 0.0) 9999.0 #.(v 1.0 1.0 1.0) t)
              (sphere-new #.(v -5.0 0.0 2.0) 2.0 #.(v 1.0 1.0 0.0) nil)
              (sphere-new #.(v 0.0 5.0 -1.0) 4.0 #.(v 1.0 0.0 0.0) nil)
              (sphere-new #.(v 8.0 5.0 -1.0) 2.0 #.(v 0.0 0.0 1.0) nil))))

(declaim (inline world-camera world-spheres xorg128 randf rnd-dome))
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

(defun randf ()
  (/ (float (xor128) 1.0) #.(float #xffffffff 1.0)))

(defun rnd-dome (normal &optional (v (v 0.0 0.0 0.0)))
  (declare (type vec-type normal))
  (loop for p = (v-unit! (v (- (* 2.0 (randf)) 1.0)
                            (- (* 2.0 (randf)) 1.0)
                            (- (* 2.0 (randf)) 1.0)
                            v))
        unless (<= (v-dot p normal) 0.0)
          return p))

(defun trace-ray (world ray depth)
  (declare (type ray ray) (type fixnum depth))
  (let* ((v1 (%v 0.0 0.0 0.0))
         (v2 (%v 0.0 0.0 0.0))
         (v3 (%v 0.0 0.0 0.0))
         (v4 (%v 0.0 0.0 0.0))
         (tmp (hit-new 1e16 v1 v2
                       (sphere-new #.(v 0.0 0.0 0.0) 0.0 #.(v 0.0 0.0 0.0) nil)))
         (hit (hit-new 1e16 v3 v4
                       (sphere-new #.(v 0.0 0.0 0.0) 0.0 #.(v 0.0 0.0 0.0) nil)))
         (nohit t))
    (declare (dynamic-extent tmp hit v1 v2 v3 v4))
    (loop for sp in (world-spheres world)
          for res = (sphere-hit sp ray tmp)
          when (and res
                    (> (hit-distance res) 0.0001)
                    (< (hit-distance res)
                       (hit-distance hit)))
            do (progn
                 (setf nohit nil
                       (hit-distance hit) (hit-distance res)
                       (hit-sphere hit)   (hit-sphere res))
                 (replace (hit-point hit) (hit-point res))
                 (replace (hit-normal hit) (hit-normal res))))
    (cond
      ;; base case : ensure new vector is returned
      ((or nohit
           (>= depth +max-depth+))
       (v 0 0 0))
      ;; base case : ensure new vector is returned
      ((sphere-is-light (hit-sphere hit))
       (copy-vec (sphere-color (hit-sphere hit))))
      (t (let* ((r (v 0 0 0))
                (nray (ray-new (hit-point hit) (rnd-dome (hit-normal hit) r)))
                (ncolor (trace-ray world nray (1+ depth)))
                (at (v-dot (ray-direction nray) (hit-normal hit))))
           (declare (dynamic-extent r nray))
           (v-mul! (v-mul-s! ncolor at)
                   (sphere-color (hit-sphere hit))))))))

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

(defun produce-data ()
  (let* ((world (world-new))
         (camera (world-camera world))
         (lt (camera-lt camera))
         (vdu (v-div-s! (v-sub (camera-rt camera) (camera-lt camera))
                        (coerce +width+ +float-type+)))
         (vdv (v-div-s! (v-sub (camera-lb camera) (camera-lt camera))
                        (coerce +height+ +float-type+)))
         (data (loop for y from 0 below +height+
                     collect (loop for x fixnum from 0 below +width+
                                   collect (let ((color (v 0.0 0.0 0.0))
                                                 (ray (ray-new (camera-eye camera) #.(v 0.0 0.0 0.0)))
                                                 (dir nil))
                                             (loop repeat +samples+ do
                                               (setf dir (v-add!
                                                          (v-add! (v-mul-s vdu (+ (coerce x +float-type+)
                                                                                  (randf)))
                                                                  lt)
                                                          (v-mul-s vdv (+ (coerce y +float-type+)
                                                                          (randf)))))
                                               (setf (ray-direction ray)
                                                     (v-unit! (v-sub! dir (ray-origin ray))))
                                               (setf color (v-add! color
                                                                   (trace-ray world ray 0))))
                                             (v-div-s! color (coerce +samples+ +float-type+)))))))
    data))

(defun main ()
  (let ((data (produce-data)))
    (writeppm data)))

(defun dump ()
  (progn (sb-ext:disable-debugger)
         (sb-ext:save-lisp-and-die "lisprb" :purify t :toplevel #'main :executable t)))
