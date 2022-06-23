;;; The Great Computer Language Shootout
;;; http://shootout.alioth.debian.org/
;;; contributed by Nicolas Neuss, 2005
;;; improved by Bela Pecsek, 2021-09-13
(declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))

(deftype uint () '(unsigned-byte 62))

(declaim (ftype (function (uint) uint) nsieve))
(defun nsieve (m)
  (loop with sieve of-type simple-bit-vector
          = (make-array m :initial-element 1 :element-type 'bit)
        for i of-type uint from 2 below m
        unless (zerop (sbit sieve i))
          do (loop for j of-type uint from (ash i 1) below m by i
                   do (setf (sbit sieve j) 0))
          and count t))

(defun main (&optional n-supplied)
  (let ((n (or n-supplied (parse-integer (car (last sb-ext:*posix-argv*))))))
    (declare ((integer 0 16) n))
    (loop for k of-type (integer 0 16) from n downto (- n 2) 
          for m = (* 10000 (expt 2 k))
          do (format t "Primes up to~T~8<~d~>~T~8<~d~>~%" m (nsieve m)))))
