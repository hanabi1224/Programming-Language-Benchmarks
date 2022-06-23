;;; The Great Computer Language Shootout
;;; http://shootout.alioth.debian.org/
;;; contributed by Nicolas Neuss, 2005
;;; - improved by Bela Pecsek, 2021-09-13
;;; - improvement in type declarations by Bela Pecsek, 2021-09-15
;;;   resulting in further 10-12% speed gain
;;; - re-write nsieve based on an algorithm resambling Robert Smith' used in factorial.lisp
;;; - changed count from using loop to (count 0 sieve) resulting in substantial speedup
;;;   also changed type declaration by Bela Pecsek, 2022-03-13
(declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))

(deftype uint () '(unsigned-byte 62))

(declaim (ftype (function (uint) (values uint &optional)) nsieve))
(defun nsieve (limit)
  "Compute the number of primes less than or equal to LIMIT."
  (if (< limit 2) 0
      (loop with len of-type uint = (1- (+ (ash limit -1) (mod limit 2)))
            with sieve of-type simple-bit-vector = (make-array (1+ len) :element-type 'bit
                                                                        :initial-element 0)
            for i of-type uint below (ash (isqrt limit) -1)
            for offset of-type uint = (+ 3 (* 2 i (+ 3 i)))
            for delta  of-type uint = (+ 3 (* 2 i))
            when (zerop (sbit sieve i))
              do (loop for j of-type uint from offset below len by delta
                       do (setf (sbit sieve j) 1))
            finally (return (count 0 sieve)))))

(declaim (ftype (function (&optional (integer 0 16)) null) main))
(defun main (&optional n-supplied)
  (let* ((n (or n-supplied (parse-integer (car (last sb-ext:*posix-argv*))))))
    (declare ((integer 0 16) n))
    (loop for k below 3
          for m = (ash 10000 (- n k))
          do (format t "Primes up to~T~8<~d~>~T~8<~d~>~%" m (nsieve m)))))
