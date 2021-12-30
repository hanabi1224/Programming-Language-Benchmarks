;;   The Computer Language Benchmarks Game
;;   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
;;;
;;; contributed by Roman Kashitsyn
;;; added eval-when by Bela Pecsek to run in container
(declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-concurrency)
  (defun get-thread-count ()
    (progn (define-alien-routine sysconf long (name int))
           (sysconf 84)))
  (defconstant min-depth    4 "Minimal depth of the binary tree.")
  (defconstant num-workers (get-thread-count) "Number of concurrent workers.")
  (deftype uint () '(unsigned-byte 31)))

(declaim (ftype (function (uint) list) build-tree)
         (maybe-inline build-tree check-node))
(defun build-tree (depth)
    "Build a binary tree of the specified DEPTH. Leaves are represented by NIL,
     branches are represented by a cons cell."
  (declare (type uint depth))
  (cond ((zerop depth) (cons nil nil))
        (t (cons (build-tree (- depth 1)) (build-tree (- depth 1))))))

(declaim (ftype (function (list) uint) check-node))
(defun check-node (node)
  (declare (type list node))
  (cond ((car node) (the uint (+ 1 (check-node (car node)) (check-node (cdr node)))))
        (t 1)))

(declaim (ftype (function (uint) null) loop-depths))
(defun loop-depths (max-depth)
  (declare (type uint max-depth))
  (labels ((build-tree (depth)
             (declare (type uint max-depth))
             (cond ((zerop depth) (cons nil nil))
                   (t (cons (build-tree (- depth 1)) (build-tree (- depth 1))))))
           (check-node (node)
             (declare (type list node))
             (cond ((car node) (truly-the uint (+ 1 (check-node (car node))
                                                    (check-node (cdr node)))))
                   (t 1))))
    (declare (inline build-tree check-node))
    (loop for depth of-type uint from min-depth by 2 upto max-depth
          do (let ((iterations (ash 1 (+ max-depth min-depth (- depth)))) (check 0))
               (loop for i of-type uint from 1 upto iterations
                     do (incf check (check-node (build-tree depth))))
               (format t "~D	 trees of depth ~D	 check: ~D~%" iterations
                       depth check)))))


(defun binary-trees-upto-size (n)
  (declare (type (integer 0 255) n))
  (format t "stretch tree of depth ~d~c check: ~d~%" (1+ n) #\Tab
          (check-node (build-tree (1+ n))))
  (let ((long-lived-tree (build-tree n)))
    (loop-depths n)
    (format t "long lived tree of depth ~d~c check: ~d~%" n #\Tab
            (check-node long-lived-tree))))

(defun main (&optional n-supplied)
  (let ((n (or n-supplied (parse-integer (or (car (last sb-ext:*posix-argv*)) "18")))))
  (binary-trees-upto-size n)))
