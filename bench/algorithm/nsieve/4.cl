;;; The Great Computer Language Shootout
;;; http://shootout.alioth.debian.org/
;;; contributed by Nicolas Neuss, 2005
;;; - improved by Bela Pecsek, 2021-09-13
;;; - improvement in type declarations by Bela Pecsek, 2021-09-15
;;;   resulting in further 10-12% speed gain
;;; - further optimisation resulting in some 35% speed increase
;;;   by Bela Pecsek, 2021-09-22;
(declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))

(deftype uint31 () '(unsigned-byte 31))

(declaim (ftype (function (uint31) (values uint31 &optional)) nsieve))
(defun nsieve (m)
  (let ((sieve (make-array m :element-type 'bit :initial-element 0)))
    (declare (type simple-bit-vector sieve))
    ;; 0 and 1 aren't prime
    (setf (aref sieve 0) 1
          (aref sieve 1) 1)
    ;; eliminate even numbers that never prime
    (loop for i of-type uint31 from 4 below m by 2
          do (setf (aref sieve i) 1))
    (loop for i of-type uint31 from 3 to (isqrt m) by 2
          when (zerop (aref sieve i))
            do (loop for j of-type uint31 from (* i 3) below m by (* i 2)
                     do (setf (aref sieve j) 1)))
  (loop for i of-type uint31 from 2 below m
        count (zerop (aref sieve i)))))

(declaim (ftype (function (&optional (integer 0 16)) null) main))
(defun main (&optional n-supplied)
  (let* ((n (or n-supplied (parse-integer (car (last sb-ext:*posix-argv*))))))
    (declare ((integer 0 16) n))
    (loop for k of-type (integer 0 16) from n downto (- n 2) 
          for m = (* 10000 (expt 2 k))
          do (format t "Primes up to~T~8<~d~>~T~8<~d~>~%" m (nsieve m)))))
