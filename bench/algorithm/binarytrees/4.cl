;;;  Code cleanup and formating by Bela Pecsek
;;;   * Local functions for speed
;;;   * MV lookup to improce slot access speed
;;;   * Some declaration improvement
(declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-concurrency)
  (defun get-thread-count ()
    (progn (define-alien-routine sysconf long (name int))
           (sysconf 84)))
  (defconstant min-depth   4 "Minimal depth of the binary tree.")
  (defconstant num-workers (get-thread-count) "Number of concurrent workers.")
  (deftype uint () '(unsigned-byte 31)))

(defclass node ()
   ((left  :type node :accessor left  :initarg :left)
    (right :type node :accessor right :initarg :right)))

(declaim (maybe-inline make-node build-tree values-for-node check-node))
(defun make-node (left right)
  (declare (type (or node null) left right))
  (make-instance 'node :left left :right right))

(defun values-for-node (node)
  (declare (type (or node null) node))
  (values (slot-value node 'left) (slot-value node 'right)))

(defun check-node (node)
  (declare (type node node))
  (multiple-value-bind (l r) (values-for-node node)
    (cond (l (the uint (+ 1 (check-node l) (check-node r)))) 
          (t 1))))

(declaim (ftype (function (uint) node) build-tree))
(defun build-tree (depth)
  (declare (type uint depth))
  (cond ((zerop depth) (make-node nil nil))
        (t (make-node (build-tree (- depth 1)) (build-tree (- depth 1))))))

(declaim (ftype (function (uint) null) loop-depths-async))
(defun loop-depths-async (max-depth)
  (declare (fixnum max-depth))
  (flet ((build-tree (depth)
           (declare (type uint depth))
           (cond ((zerop depth) (make-node nil nil))
                 (t (make-node (build-tree (- depth 1)) (build-tree (- depth 1))))))
         (check-node (node)
           (declare (type node node))
           (multiple-value-bind (l r) (values-for-node node)
             (cond (l (the uint (+ 1 (check-node l) (check-node r)))) 
                   (t 1))))
         (check-trees-of-depth (depth max-depth)
           (declare (type uint depth max-depth))
           (loop with iterations of-type uint = (ash 1 (+ max-depth min-depth (- depth)))
                 for i of-type uint from 1 upto iterations
                 summing (check-node (build-tree depth)) into result of-type uint
                 finally (return (format nil "~d~c trees of depth ~d~c check: ~d~%"
                                         iterations #\Tab depth #\Tab result)))))
    (declare (inline build-tree check-node check-trees-of-depth))
    (let* ((tasks (sb-concurrency:make-queue
                   :initial-contents
                   (loop for depth from min-depth by 2 upto max-depth
                         collect depth)))
           (outputs (sb-concurrency:make-queue))
           (threads
             (loop for i of-type fixnum from 1 to num-workers
                   collect (sb-thread:make-thread
                            #'(lambda ()
                                (loop as task = (sb-concurrency:dequeue tasks)
                                      while task
                                      do (sb-concurrency:enqueue
                                          (cons task
                                                (check-trees-of-depth task max-depth))
                                          outputs)))))))
      (mapc #'sb-thread:join-thread threads)
      (let ((results (sort (sb-concurrency:list-queue-contents outputs)
                           #'< :key #'car)))
        (loop for (k . v) in results
              do (format t "~a" v))))))

(declaim (ftype (function (uint) null) binary-trees-upto-size))
(defun binary-trees-upto-size (n)
  (declare (type (integer 0 255) n))
  (format t "stretch tree of depth ~d~c check: ~d~%" (1+ n) #\Tab
          (check-node (build-tree (1+ n))))
  (let ((long-lived-tree (build-tree n)))
    (loop-depths-async n)
    (format t "long lived tree of depth ~d~c check: ~d~%" n #\Tab
            (check-node long-lived-tree))))

(declaim (ftype (function (&optional uint) null) main))
(defun main (&optional n-supplied)
  (let ((n (or n-supplied (parse-integer (or (car (last #+sbcl sb-ext:*posix-argv*)) "18")))))
    (binary-trees-upto-size n)))
