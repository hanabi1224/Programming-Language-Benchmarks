;;   The Computer Language Benchmarks Game
;;   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
;;;
;;; contributed by Roman Kashitsyn
;;; added eval-when by Bela Pecsek to run in container
(declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))

(defconstant min-depth 4 "Minimal depth of the binary tree.")
(deftype uint () '(unsigned-byte 31))

(declaim (ftype (function (uint) list) build-tree)
         (maybe-inline build-tree check-node))
(defun build-tree (depth)
  "Build a binary tree of the specified DEPTH. Leaves are represented by NIL,
   branches are represented by a cons cell."
  (declare (type uint depth))
  (cond ((zerop depth) (cons nil nil))
        (t (cons (build-tree (1- depth)) (build-tree (1- depth))))))

(declaim (ftype (function (list) uint) check-node))
(defun check-node (node)
  (declare (type list node))
  (cond ((car node) (the uint (+ 1 (check-node (car node)) (check-node (cdr node)))))
        (t 1)))

(declaim (ftype (function (uint) null) loop-depths))
(defun loop-depths (max-depth)
  (declare (type uint max-depth))
  (labels ((build-tree (depth)
             (declare (type uint depth))
             (cond ((zerop depth) (cons nil nil))
                   (t (cons (build-tree (1- depth)) (build-tree (1- depth))))))
           (check-node (node)
             (declare (type list node))
             (cond ((car node) (the uint (+ 1 (check-node (car node))(check-node (cdr node)))))
                   (t 1))))
    (declare (inline build-tree check-node))
    (loop for depth of-type uint from min-depth by 2 upto max-depth do
      (loop with iterations of-type uint = (the uint (ash 1 (+ max-depth min-depth (- depth))))
            for i of-type uint from 1 upto iterations
            sum (check-node (build-tree depth)) into result of-type uint
            finally (return (format t "~D	 trees of depth ~D	 check: ~D~%"
                                    iterations depth result))))))

(declaim (ftype (function (uint) null) binary-trees-upto-size))
(defun binary-trees-upto-size (n)
  (declare (type uint n))
  (format t "stretch tree of depth ~d~c check: ~d~%" (1+ n) #\Tab
          (check-node (build-tree (1+ n))))
  (let ((long-lived-tree (build-tree n)))
    (loop-depths n)
    (format t "long lived tree of depth ~d~c check: ~d~%" n #\Tab
            (check-node long-lived-tree))))

(declaim (ftype (function (&optional uint) null) main))
(defun main (&optional n-supplied)
  (let ((n (or n-supplied (parse-integer (or (car (last sb-ext:*posix-argv*)) "18")))))
    (binary-trees-upto-size n)))
