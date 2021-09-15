;;; The Great Computer Language Shootout
;;; http://shootout.alioth.debian.org/
;;; contributed by Nicolas Neuss, 2005
;;; - improved by Bela Pecsek, 2021-09-13
;;; - improvement in type declarations by Bela Pecsek, 2021-09-15
;;;   resulting in further 10-12% speed gain
(declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))

(deftype uint62 (&optional (bits 62)) `(unsigned-byte ,bits))

(declaim (ftype (function (uint62) uint62) nsieve))
(defun nsieve (m)
  (let ((a (make-array m :initial-element 1 :element-type 'bit)))
    (declare (type simple-bit-vector a))
    (loop for i from 2 below m
          when (= (sbit a i) 1)
            do (loop for j of-type uint62 from (ash i 1) below m by i
                     do (setf (sbit a j) 0))
            and count t)))

(defun main (&optional n-supplied)
  (let* ((n (or n-supplied (parse-integer (car (last sb-ext:*posix-argv*))))))
    (declare ((integer 0 32) n))
    (loop for i below 3
          for m = (ash 10000 (- n i))
          do (format t "Primes up to~T~8<~d~>~T~8<~d~>~%" m (nsieve m)))))
