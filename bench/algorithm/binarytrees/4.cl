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
  (deftype uint  () '(unsigned-byte 31))
  (deftype index () 'sb-int:index))

(defclass node ()
   ((left  :accessor left  :initarg :left)
    (right :accessor right :initarg :right)))

(defmethod values-for-node ((x node))
  (values (slot-value x 'left)
          (slot-value x 'right)))

(defmethod check-node ((n node))
  (multiple-value-bind (l r) (values-for-node n)
    (cond (l (truly-the uint (+ 1 (check-node l) (check-node r)))) 
          (t 1))))

(defun build-tree (depth)
  (cond ((zerop depth) (make-instance 'node :left nil :right nil))
        (t (make-instance 'node :left  (build-tree (- depth 1))
                                :right (build-tree (- depth 1))))))

(declaim (ftype (function (uint) null) loop-depths-async))
(defun loop-depths-async (max-depth)
  (declare (fixnum max-depth))
  (flet ((build-tree (depth)
           (cond ((zerop depth) (make-instance 'node :left nil :right nil))
                 (t (make-instance 'node :left  (build-tree (- depth 1))
                                         :right (build-tree (- depth 1))))))
         (check-node (n)
           (multiple-value-bind (l r) (values-for-node n)
             (cond  (l (truly-the uint (+ 1 (check-node l) (check-node r)))) 
                    (t 1))))
         (check-trees-of-depth (depth max-depth)
           (declare (uint depth max-depth))
           (loop with iterations of-type uint = (ash 1 (+ max-depth min-depth (- depth)))
                 for i of-type uint from 1 upto iterations
                 sum (check-node (build-tree depth))
                   into result of-type uint
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

(defun main (&optional n-supplied)
  (let ((n (or n-supplied (parse-integer (or (car (last #+sbcl sb-ext:*posix-argv*)) "18")))))
    (binary-trees-upto-size n)))
