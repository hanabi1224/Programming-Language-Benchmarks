;;; The Great Computer Language Shootout
;;; http://shootout.alioth.debian.org/
;;; contributed by Nicolas Neuss, 2005
;;; - improved by Bela Pecsek, 2021-09-13
;;; - improvement in type declarations by Bela Pecsek, 2021-09-15
;;;   resulting in further 10-12% speed gain
(declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))

(deftype uint31 () '(unsigned-byte 31))

(declaim (ftype (function (uint31) (values uint31 &optional)) nsieve))
(defun nsieve (m)
  (let ((a (make-array m :element-type 'bit :initial-element 0)))
    (declare (type simple-bit-vector a))
    (loop for i from 2 below m
          when (zerop (sbit a i))
            do (loop for j from (ash i 1) below m by i
                     do (setf (sbit a j) 1))
            and count t)))

(declaim (ftype (function (&optional (integer 0 16)) null) main))
(defun main (&optional n-supplied)
  (let* ((n (or n-supplied (parse-integer (car (last sb-ext:*posix-argv*))))))
    (declare ((integer 0 16) n))
    (loop for k of-type (integer 0 16) from n downto (- n 2) 
          for m = (* 10000 (expt 2 k))
          do (format t "Primes up to~T~8<~d~>~T~8<~d~>~%" m (nsieve m)))))
