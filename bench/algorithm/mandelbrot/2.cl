(declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))
(sb-int:set-floating-point-modes :traps (list :divide-by-zero))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "sb-md5")
  (ql:quickload '(:sb-simd) :silent t)
  (defpackage :v8
    (:use #:common-lisp :sb-simd-avx2)
    (:export #:make-f64v8 #:f64v8+ #:f64v8- #:f64v8* #:f64v8 #:f64 #:f64vec
             #:u32 #:u8 #:u8vec #:u8-aref #:f64-aref #:f64.4-aref #:f64- #:f64*
             #:vzeroupper))

  (defpackage :mandelbrot
    (:nicknames :mb)
    (:use :cl :sb-md5 :v8)))

(in-package #:v8)
(defconstant +VLEN+ 8)
(deftype f64v8 () `(simple-array f64 (,+VLEN+)))

(declaim (ftype (function (&key (:initial-element f64)
                                (:initial-contents list)) f64v8) make-f64v8)
         (inline make-f64v8))
(defun make-f64v8 (&key initial-element initial-contents)
  (cond (initial-element  (make-array +VLEN+ :element-type 'f64
                                             :initial-element initial-element))
        (initial-contents (make-array +VLEN+ :element-type 'f64
                                             :initial-contents initial-contents))
        (t (make-array +VLEN+ :element-type 'f64))))

(macrolet ((define (name inst)
              `(progn (declaim (ftype (function (f64v8 f64v8 f64v8) f64v8) ,name)
                               (inline ,name))
                      (defun ,name (u v result)
                        (setf (f64.4-aref result 0) (,inst (f64.4-aref u 0)
                                                           (f64.4-aref v 0))
                              (f64.4-aref result 4) (,inst (f64.4-aref u 4)
                                                           (f64.4-aref v 4)))
                        result))))
  (define f64v8+ f64.4+)
  (define f64v8- f64.4-)
  (define f64v8* f64.4*))

(in-package :mandelbrot)

(defconstant +MAX-ITER+ 50)
(defconstant +VLEN+ 8)

(declaim (ftype (function (f64v8 f64v8) u8) mbrot8)
         (inline mbrot8))
(defun mbrot8 (cr ci)
  (let ((zr   (make-f64v8))
        (zi   (make-f64v8))
        (tr   (make-f64v8))
        (ti   (make-f64v8))
        (absz (make-f64v8))
        (tmp  (make-f64v8)))
    (declare (dynamic-extent absz))
    (loop repeat (/ +MAX-ITER+ 5)
          do (loop repeat 5
                   do (f64v8+ zr zr tmp)
                      (f64v8* tmp zi tmp)
                      (f64v8+ tmp ci zi)
                      (f64v8- tr ti tmp)
                      (f64v8+ tmp cr zr)
                      (f64v8* zr zr tr)
                      (f64v8* zi zi ti))
             (f64v8+ tr ti absz)
             (loop with terminate of-type boolean = t
                   for i below +VLEN+
                   when (<= (aref absz i) 4d0)
                     do (setf terminate nil)
                        (loop-finish)
                   finally (if terminate (return-from mbrot8 0))))
    (loop with accu of-type u8 = 0
          for i of-type u8 below +VLEN+
          do (setf accu (logior accu (if (<= (aref absz i) 4d0) (ash #x80 (- i)) 0)))
          finally (return accu))))

(declaim (ftype (function (u32) null) main))
(defun main (n-input)
  (let* ((size (* (ceiling n-input +VLEN+) +VLEN+)) ;size to be multiple of 8
         (chunk-size (ash size -3))
         (inv    (/ 2d0 size))
         (xloc   (make-array (* chunk-size +VLEN+) :element-type 'f64))
         (bitmap (make-array (* size chunk-size)   :element-type 'u8)))
    (declare (type f64vec xloc)
             (type u8vec bitmap))
    (loop for i below size
          do (setf (aref xloc i) (- (* i inv) 1.5d0)))
    (flet ((array-slice (array row slice)
             (declare (type f64vec array)
                      (type u32 row)
                      (type f64v8 slice))
             (setf (f64.4-aref slice 0) (f64.4-aref array row)
                   (f64.4-aref slice 4) (f64.4-aref array (+ row 4)))
             slice))
      (declare (inline array-slice))
      (loop with slice of-type f64v8 = (make-f64v8)
            for chunk-id of-type u32 below size
            for ci of-type f64v8 = (make-f64v8 :initial-element (- (* chunk-id inv) 1d0))
            do (loop for i below chunk-size
                     for r of-type u8 = (mbrot8 (array-slice xloc (* i +VLEN+) slice) ci)
                     unless (zerop r)
                       do (setf (u8-aref bitmap (+ (* chunk-id chunk-size) i)) r))))
    (format t "P4~%~d ~d~%" size size)
    (format t "~(~{~2,'0X~}~)~%" (coerce (sb-md5:md5sum-sequence bitmap) 'list))))

(in-package :cl-user)
(defun main (&optional n-supplied)
  (let* ((args sb-ext:*posix-argv*)
         (n-input (or n-supplied (parse-integer (or (cadr args) "8000")))))
    (mb::main n-input)))
