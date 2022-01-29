;;; created by Bela Pecsek 2022-01-22
;;;   based on 6-m.cl binary tree code 
(declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-concurrency)
  #+sb-thread
  (defun get-thread-count ()
    (progn (define-alien-routine sysconf long (name int))
           (sysconf 84)))
  (defconstant min-depth   4 "Minimal depth of the binary tree.")
  (defconstant num-workers (get-thread-count) "Number of concurrent workers.")
  (deftype uint  () '(unsigned-byte 31))
  (deftype int64 () '(signed-byte 64)))

(defconstant min-depth 4 "Minimal depth of the binary tree.")

(declaim (inline make-node hash (setf hash) value (setf value)
                 left (setf left) right (setf right)))
(defstruct (node (:conc-name nil)
                 (:constructor make-node (value left right)))
  (hash  nil :type (or int64 null))
  (value nil :type (or int64 null))
  (left  nil :type (or node null))
  (right nil :type (or node null)))

(declaim (ftype (function (uint) node) build-tree)
         (inline build-tree get-hash check cal-hash))
(defun build-tree (depth)
  (let ((depth-1 (1- depth)))
    (cond ((zerop depth) (make-node 1 nil nil))
          (t (make-node nil (build-tree depth-1) (build-tree depth-1))))))

(declaim (ftype (function (node) int64) get-hash))
(defun get-hash (node)
  (with-accessors ((hash hash)) node
    (if hash hash (the int64 -1))))

(declaim (ftype (function (node) boolean) check))
(defun check (node)
  (with-accessors ((hash hash)(value value)(left left)(right right)) node
    (cond ((null hash) nil)
          (value t)
          ((and left right) (and (check left) (check right))))))

(declaim (ftype (function (node) int64) check-node))
(defun cal-hash (node)
  (with-accessors ((hash hash)(value value)(left left)(right right)) node
    (unless hash
      (if value (setf hash value)
          (when (and left right)
              (progn (cal-hash left) (cal-hash right)
                     (setf hash (+ (get-hash left)(get-hash right)))))))))

(declaim (ftype (function (uint) null) loop-depths-async))
(defun loop-depths-async (max-depth)
  (labels ((build-tree (depth)
             (declare (type uint depth))
             (let ((depth-1 (1- depth)))
               (cond ((zerop depth) (make-node 1 nil nil))
                     (t (make-node nil (build-tree depth-1) (build-tree depth-1))))))
           (check (node)
             (declare (type node node))
             (with-accessors ((hash hash)(value value)(left left)(right right)) node
               (cond ((null hash) nil)
                     (value t)
                     ((and left right) (and (check left) (check right))))))
           (cal-hash (node)
             (declare (type node node))
             (with-accessors ((hash hash)(value value)(left left)(right right)) node
               (unless hash
                 (if value (setf hash value)
                     (when (and left right)
                         (progn (cal-hash left) (cal-hash right)
                                (setf hash (+ (get-hash left)(get-hash right)))))))))
           (check-trees-of-depth (depth max-depth)
             (declare (uint depth max-depth))
             (loop with iterations of-type uint = (the uint (ash 1 (+ max-depth min-depth (- depth))))
                   for i of-type uint from 1 upto iterations
                   with sum of-type int64 = 0
                   do (let ((tree (build-tree depth)))
                        (progn (cal-hash tree)
                               (incf sum (the int64 (get-hash tree)))))
                   finally (return (format nil "~d~c trees of depth ~d~c root hash sum: ~D~%"
                                           iterations #\Tab depth #\Tab sum)))))
    (let* ((tasks (sb-concurrency:make-queue
                   :initial-contents
                   (loop for depth from min-depth by 2 upto max-depth collect depth)))
           (outputs (sb-concurrency:make-queue)))
      (mapc #'sb-thread:join-thread
            (loop for i of-type fixnum from 1 to num-workers
                  collect (sb-thread:make-thread
                           #'(lambda ()
                               (loop as task = (sb-concurrency:dequeue tasks)
                                     while task
                                     do (sb-concurrency:enqueue
                                         (cons task
                                               (check-trees-of-depth task max-depth))
                                         outputs))))))
      (loop for (k . v) in (sort (sb-concurrency:list-queue-contents outputs) #'< :key #'car)
            do (format t "~a" v)))))

(declaim (ftype (function (uint) null) binary-trees-upto-size))
(defun binary-trees-upto-size (n)
  (let* ((max-depth (max 6 n))
         (strech-depth (1+ max-depth))
         (strech-tree (build-tree strech-depth)))
    (cal-hash strech-tree)
    (format t "stretch tree of depth ~d~c root hash: ~d check: ~:[false~;true~]~%"
            strech-depth #\Tab (get-hash strech-tree) (check strech-tree))
    (let ((long-lived-tree (build-tree max-depth)))
      (loop-depths-async max-depth)
      (cal-hash long-lived-tree)
      (format t "long lived tree of depth ~d~c root hash: ~d check: ~:[false~;true~]~%"
              max-depth #\Tab (get-hash long-lived-tree) (check long-lived-tree)))))

(declaim (ftype (function (&optional uint) null) main))
(defun main (&optional n-supplied)
  (let ((n (or n-supplied (parse-integer (or (car (last #+sbcl sb-ext:*posix-argv*)) "18")))))
    (binary-trees-upto-size n)))
