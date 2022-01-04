;;; The Great Computer Language Shootout
;;; http://shootout.alioth.debian.org/
;;; contributed by Nicolas Neuss, 2005
;;; - improved by Bela Pecsek, 2021-09-13
;;; - improvement in type declarations by Bela Pecsek, 2021-09-15
;;;   resulting in further 10-12% speed gain
;;; - re-write nsieve based on an algorithm resambling Robert Smith' used in factorial.lisp
(declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))

(deftype uint31 () '(unsigned-byte 31))

(declaim (ftype (function (uint31) (values uint31 &optional)) nsieve))
(defun nsieve (limit)
  "Compute the number of primes less than or equal to LIMIT."
  (if (< limit 2) 0
      (loop with len of-type uint31 = (+ (ash limit -1) (mod limit 2) -1)
            with sieve of-type simple-bit-vector = (make-array (1+ len) :element-type 'bit
                                                                        :initial-element 0)
            for i of-type uint31 below (ash (isqrt limit) -1)
            for offset of-type uint31 = (+ 3 (* 2 i (+ 3 i)))
            for delta  of-type uint31 = (+ 3 (* 2 i))
            when (zerop (aref sieve i))
              do (loop for j of-type uint31 from offset below len by delta
                       do (setf (aref sieve j) 1))
            finally (return (1+ (loop for i of-type uint31 below len
                                      count (zerop (aref sieve i))))))))

(declaim (ftype (function (&optional (integer 0 16)) null) main))
(defun main (&optional n-supplied)
  (let* ((n (or n-supplied (parse-integer (car (last sb-ext:*posix-argv*))))))
    (declare ((integer 0 16) n))
    (loop for k of-type (integer 0 16) from n downto (- n 2) 
          for m = (* 10000 (expt 2 k))
          do (format t "Primes up to~T~8<~d~>~T~8<~d~>~%" m (nsieve m)))))
