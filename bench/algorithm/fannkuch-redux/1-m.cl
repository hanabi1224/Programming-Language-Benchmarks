;;   The Computer Language Benchmarks Game
;;   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
;;;
;;; By Jon Smith (rev 4)
;;; 
;;; This is a multi core implementation. It should be quite fast.
;;; It is now a mix of multiple techniques stolen from a number of other implementations.
;;; (It is essentially based on every other implementation available)
;;;
;;; To compile
;;; sbcl --load fannkuch.lisp --eval "(save-lisp-and-die \"fannkuch.core\" :purify t :toplevel (lambda () (main) (quit)))"
;;; To run
;;; sbcl --noinform --core fannkuch.core %A
                                        ;(setf *efficiency-note-cost-threshold* 1)
                                        ;(setf *efficiency-note-limit* 8)

(declaim (optimize (speed 3) (safety 0) (space 1) (debug 0)))

(defmacro sb (a) `(the fixnum ,a))
(deftype sb () 'fixnum)

(defmacro setlambda(n)
  (declare (type fixnum n))
  (let ((copy (gensym))
	      (perm (gensym)))
    `(lambda (,perm ,copy)
       (declare (optimize (speed 3) (safety 0) (space 0) (debug 0))
                (type (simple-array sb (,n)) ,copy ,perm))
       ,@(loop for i of-type fixnum from 0 below n collect
	             `(setf (aref ,copy ,i) (aref ,perm ,i))))))

(defmacro countfliplambda (n)
  (declare (type fixnum n))
  (let ((copy (gensym))
	      (c (gensym))
	      (z (gensym)))
    `(lambda (,copy &aux (,c 0))
       (declare (optimize (speed 3) (safety 0) (space 0) (debug 0))
		            (type sb ,c)
		            (type (simple-array sb (,n)) ,copy))
       (let ((,z (aref ,copy 0)))
	       (loop until (= ,z 0) do
	         (progn
		         (case ,z 
		           ,@(loop for i of-type sb from 1 to (- n 1) collect
			                 `(,i
			                   ,@(loop for j of-type sb from 0 to (ash i -1)
				                         if (not (= j (- i j)))
				                           collect `(rotatef (aref ,copy ,j) 
						                                         (aref ,copy ,(- i j)))))))
		         (incf ,c)
		         (setf ,z (aref ,copy 0)))))
       ,c)))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun factorial (n)
    (declare (type fixnum n))
    (if (> n 2)
	      (* n (factorial (- n 1)))
	      n)))

(let ((factorial-array (apply #'vector (loop for i from 1 to 16 collect (factorial i)))))
  (declare (type (simple-array fixnum (*))))
  (defun pre-computed-factorial (n)
    (declare (type fixnum n))
    (aref factorial-array (- n 1))))


(defun nth-permutation (perm copy count idx &optional (len (length perm)))
  (declare (type (simple-array fixnum (*)) perm copy count)
	         (type fixnum idx len))
  (loop for i from 0 below len do (setf (aref perm i) i))
  (loop for i of-type fixnum from (- len 1) above 0 do
    (let ((d (truncate idx (the fixnum (pre-computed-factorial i)))))
	    (declare (type fixnum d))
	    (setf (aref count i) d)
	    (setf idx (the fixnum (mod idx (the fixnum(pre-computed-factorial i)))))
	    (loop for m of-type fixnum from 0 to i do (setf (aref copy m) (aref perm m)))
	    (loop for j of-type fixnum from 0 to i do
	      (if (<= (+ j d) i)
		        (setf (aref perm j) (aref copy (+ j d)))
		        (setf (aref perm j) (aref copy (- (sb (+ j d)) i 1))))))))

(defun next-permutation (perm count)
  (declare (type (simple-array sb (*)) perm count))
  (let ((first (aref perm 1)))
    (setf (aref perm 1) (aref perm 0))
    (setf (aref perm 0) first)
    (let ((i 1))
      (incf (aref count i))
      (do ()
	        ((not (> (aref count i) i)))
	      (setf (aref count i) 0)
	      (incf i)
	      (setf (aref perm 0) (aref perm 1))
	      (let ((next-first (aref perm 1)))
	        (loop for j from 1 below i do
	          (setf (aref perm j) (aref perm (+ j 1))))
	        (setf (aref perm i) first)
	        (setf first next-first))
	      (incf (aref count i))))))

(defun fannkuch (n)  
  (declare (type fixnum n))
  (flet ((fannkuch-sub (n cflip copyfn start end)
	         (declare (type sb n start end)
		                (type (function ((simple-array sb (*)))) cflip)
		                (type (function ((simple-array sb (*)) (simple-array sb (*)))) copyfn))
	         (let ((csum 0)
		             (fmax 0)
		             (count (make-array n :element-type 'fixnum))
		             (copy (make-array n :element-type 'fixnum))
		             (perm (make-array n :element-type 'fixnum)))
	           (declare (type (simple-array sb (*)) perm copy count)
		                  (type sb  csum fmax))
	           (nth-permutation perm copy count start n)
	           (dotimes (i (- end start))
	             (funcall copyfn perm copy)
	             (let ((c (funcall cflip copy)))
		             (declare (type sb c))
		             (setf csum (sb (+ csum  (sb (if (evenp i) c (- c))))))
		             (when (> c fmax)
		               (setf fmax c)))
	             (when (< (+ 1 i) end)
		             (next-permutation perm count)))
	           (values csum fmax))))


    (let* ((cflip (the (function ((simple-array sb (*)))) (eval `(countfliplambda ,n))))
	         (copyfn (the (function ((simple-array sb (*)) (simple-array sb (*)))) (eval `(setlambda ,n))))
	         (csum 0) (fmax 0))
      (declare (type sb csum fmax))
      #-sb-thread (multiple-value-setq (csum fmax) (fannkuch-sub n cflip copyfn 0 (pre-computed-factorial n)))

      #+sb-thread
      (let* ((cores 4)
	           (index 0)
	           (index-step (truncate (the fixnum (+ (the fixnum (pre-computed-factorial n)) (- cores 1))) cores))
	           (threads (loop for i from 0 below cores
			                      collecting  (sb-thread:make-thread (let ((start index) (end (+ index index-step)))
							                                                   (declare (fixnum start end))
							                                                   (lambda () (fannkuch-sub n cflip copyfn start end))))
			                      do (The fixnum (incf index index-step)))))
	      (declare (type fixnum cores index index index-step))
	      (dolist (thread threads) 
	        (multiple-value-bind (sum max) (sb-thread:join-thread thread)
	          (declare (type fixnum sum max))
	          (incf csum sum)
	          (when (> max fmax)
	            (setf fmax max)))))
      (format t "~s~%Pfannkuchen(~s) = ~s~%" csum n fmax))))

(defun main ()  
  (let* ((args (cdr sb-ext:*posix-argv*))
         (n (parse-integer (car args))))
    (declare (type fixnum n))
    (fannkuch n)))
