;;   The Computer Language Benchmarks Game
;;   http://benchmarksgame.alioth.debian.org/
;;;
;;; contributed by Manuel Giraud
;;; modified by Nicolas Neuss
;;; modified by Juho Snellman 2005-10-26
;;;
;;; modified by Witali Kusnezow 2009-01-20
;;;  * simplified structure of leaf nodes
;;;  * optimize GC usage
;;;  * optimize all functions
;;;
;;; modified by Witali Kusnezow 2009-08-20
;;;  * remove GC hacks to satisfy new versions of the sbcl
;;;
;;; modified by Marko Kocic 2011-02-18
;;;  * add declaim to optimize for speed
;;;
;;; modified by Kieran Grant 2017-10-21
;;;   * Update declaim. Never good idea to do (safety 0)
;;;   * Optimize build-btree and check-node to remove useless item
;;;     (saves RAM and makes it *much* faster)
;;;   * Use multi-threading like Java version
;;;
;;; modified by Tomas Wain 2021-03-20
;;;   * code refactoring
;;;   * several changes in declarations

(declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))
  
  (define-constant min-depth 4 "Minimal depth of the binary tree.")
  (define-constant num-workers 4 "Number of concurrent workers.")
  (deftype uint () '(unsigned-byte 62))
  (deftype index () 'sb-int:index))
  

(declaim (ftype (function (uint) list) build-tree))
(defun build-tree (depth)
  (declare (type uint depth))
  (if (zerop depth) (list)
      (let ((depth-1 (1- depth)))
	(cons (build-tree depth-1)
	      (build-tree depth-1)))))

(declaim (ftype (function (list) uint) check-node))
(defun check-node (node)
  (if node (+ 1 (check-node (car node)) (check-node (cdr node)))
      1))

(declaim (ftype (function (uint) null) loop-depths-async))
(defun loop-depths-async (max-depth)
  (declare (type uint max-depth)
	   (optimize (speed 1)))
  #+sb-thread
  (let* ((counter (1+ (ash (- max-depth min-depth) -1)))
	 (results (make-array counter :initial-element t))
	 (lock (sb-thread:make-mutex))
	 (waitqueue (sb-thread:make-waitqueue)))
    (declare (type (simple-array list (*)) results))
    (loop for depth of-type index from min-depth by 2 upto max-depth do
      (sb-thread:make-thread
       (lambda (max-depth depth)
	 (declare (type uint max-depth depth))
	 (loop with iterations of-type index
		 = (ash 1 (+ max-depth min-depth (- depth)))
	       for i of-type uint from 1 upto iterations
	       summing (the uint (check-node (build-tree depth)))
		 into result of-type uint
	       finally
		  (sb-thread:with-mutex (lock)
		    (setf (aref results (ash (- depth min-depth) -1))
			  `(:iterations ,iterations
			    :depth      ,depth
			    :result     ,result))
		    (decf counter)
		    (sb-thread:condition-notify waitqueue))))
       :arguments `(,max-depth ,depth)))
    (sb-thread:with-mutex (lock)
      (loop while (> counter 0) do
	(sb-thread:condition-wait waitqueue lock)))
    (loop for result across results do
      (format t "~D	 trees of depth ~D	 check: ~D~%"
	      (getf result :iterations)
	      (getf result :depth)
	      (getf result :result)))))

(declaim (ftype (function (uint) null) binary-trees-upto-size))
(defun binary-trees-upto-size (n)
  (declare (type (integer 0 255) n))
  (format t "stretch tree of depth ~d~c check: ~d~%" (1+ n) #\Tab
          (check-node (build-tree (1+ n))))
  (let ((long-lived-tree (build-tree n)))
    (loop-depths-async n)
    (format t "long lived tree of depth ~d~c check: ~d~%" n #\Tab
            (check-node long-lived-tree))))

(defun main ()
  (let ((n (parse-integer (or (car (last #+sbcl sb-ext:*posix-argv*
                                         #+cmu  extensions:*command-line-strings*
					 #+gcl  si::*command-args*)) "21"))))
    (binary-trees-upto-size n)))
