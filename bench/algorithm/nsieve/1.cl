;;; The Great Computer Language Shootout
;;; http://shootout.alioth.debian.org/
;;; contributed by Nicolas Neuss, 2005
;;; optimize declaration added by Bela Pecsek, 2021-09-13
(declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))

(defun nsieve (m)
  (declare (type fixnum m))
  (let ((sieve (make-array m :initial-element t :element-type 'boolean)))
    (loop for i of-type fixnum from 2 below m
          when (aref sieve i)
            do (loop for j of-type fixnum from (* 2 i) below m by i
                     do (setf (aref sieve j) nil))
            and count t)))

(defun main (&optional n-supplied)
  (let* ((args #+sbcl sb-ext:*posix-argv*
               #+cmu  extensions:*command-line-strings*
	             #+gcl  si::*command-args*)
	       (n (or n-supplied (parse-integer (car (last args))))))
    (loop for k from n downto (- n 2) 
          for m = (* 10000 (expt 2 k))
          do (format t "Primes up to~T~8<~d~>~T~8<~d~>~%" m (nsieve m)))))
