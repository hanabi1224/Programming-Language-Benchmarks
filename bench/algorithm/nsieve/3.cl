;;; The Great Computer Language Shootout
;;; http://shootout.alioth.debian.org/
;;; contributed by Nicolas Neuss, 2005
;;; - improved by Bela Pecsek, 2021-09-13
;;; - improvement in type declarations by Bela Pecsek, 2021-09-15
;;;   resulting in further 10-12% speed gain
;;; - further optimization resulting in some 35% speed increase
;;;   by Bela Pecsek, 2021-09-22;
;;; - further optimization of 4.cl resulting in some 30% speed increase
;;;   by Bela Pecsek
;;; - changed count from using loop to (count 0 sieve) resulting in substantial speedup
;;;   also changed type declaration by Bela Pecsek, 2022-03-13
(declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))

(deftype uint () '(unsigned-byte 62))

(declaim (ftype (function (uint) (values uint &optional)) nsieve))
(defun nsieve (m)
  (declare (optimize speed (safety 0) (debug 0)))
  (let ((sieve (make-array m :element-type 'bit :initial-element 0)))
    (declare (type simple-bit-vector sieve))
    ;; eliminate even numbers that never prime
    (when (> m 2)
      (dotimes (i (ceiling m 64))
        (setf (sb-kernel:%vector-raw-bits sieve i) #xAAAAAAAAAAAAAAAA)))
    ;; 1 aren't prime but 2 is for this problem
    (setf (sbit sieve 1) 0
          (sbit sieve 2) 1)
    (loop for i of-type uint from 3 to (isqrt m) by 2
          when (= 1 (sbit sieve i))
            do (loop for j of-type uint from (* i 3) below m by (* i 2)
                     do (setf (sbit sieve j) 0))
          finally (return (count 1 sieve)))))

(declaim (ftype (function (&optional (integer 0 16)) null) main))
(defun main (&optional n-supplied)
  (let ((n (or n-supplied (parse-integer (car (last sb-ext:*posix-argv*))))))
    (declare ((integer 0 16) n))
    (loop for k of-type (integer 0 16) from n downto (- n 2) 
          for m = (* 10000 (expt 2 k))
          do (format t "Primes up to~T~8<~d~>~T~8<~d~>~%" m (nsieve m)))))
