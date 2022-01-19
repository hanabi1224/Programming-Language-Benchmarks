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
        (t (cons (build-tree (1- depth)) (build-tree (1- depth))))))

(declaim (ftype (function (list) uint) check-node))
(defun check-node (node)
  (declare (type list node))
  (cond ((car node) (the uint (+ 1 (check-node (car node)) (check-node (cdr node)))))
        (t 1)))

(declaim (ftype (function (uint) null) loop-depths-async))
(defun loop-depths-async (max-depth)
  (declare (type uint max-depth))
  (labels ((build-tree (depth)
             (declare (type uint depth))
             (cond ((zerop depth) (cons nil nil))
                   (t (cons (build-tree (1- depth)) (build-tree (1- depth))))))
           (check-node (node)
             (declare (type list node))
             (cond ((car node) (the uint (+ 1 (check-node (car node))
                                              (check-node (cdr node)))))
                   (t 1)))
           (check-trees-of-depth (depth max-depth)
             (declare (uint depth max-depth))
             (loop with iterations of-type uint = (the uint (ash 1 (+ max-depth min-depth
                                                                      (- depth))))
                   for i of-type uint from 1 upto iterations
                   summing (check-node (build-tree depth)) into result of-type uint
                   finally (return (format nil "~d~c trees of depth ~d~c check: ~d~%"
                                           iterations #\Tab depth #\Tab result)))))
    (declare (inline build-tree check-node check-trees-of-depth))
    (let* ((tasks (sb-concurrency:make-queue
                   :initial-contents (loop for depth from min-depth by 2 upto max-depth
                                           collect depth)))
           (outputs (sb-concurrency:make-queue))
           (threads (loop for i of-type fixnum from 1 to num-workers
                          collect (sb-thread:make-thread
                                   #'(lambda ()
                                       (loop as task = (sb-concurrency:dequeue tasks)
                                             while task do
                                               (sb-concurrency:enqueue
                                                (cons task
                                                      (check-trees-of-depth task max-depth))
                                                outputs)))))))
      (mapc #'sb-thread:join-thread threads)
      (let ((results (sort (sb-concurrency:list-queue-contents outputs) #'< :key #'car)))
        (loop for (k . v) in results
              do (format t "~a" v))))))

(declaim (ftype (function (uint) null) binary-trees-upto-size))
(defun binary-trees-upto-size (n)
  (declare (type uint n))
  (format t "stretch tree of depth ~d~c check: ~d~%" (1+ n) #\Tab
          (check-node (build-tree (1+ n))))
  (let ((long-lived-tree (build-tree n)))
    (loop-depths-async n)
    (format t "long lived tree of depth ~d~c check: ~d~%" n #\Tab
            (check-node long-lived-tree))))

(declaim (ftype (function (&optional uint) null) main))
(defun main (&optional n-supplied)
  (let ((n (or n-supplied (parse-integer (or (car (last sb-ext:*posix-argv*)) "18")))))
  (binary-trees-upto-size n)))
