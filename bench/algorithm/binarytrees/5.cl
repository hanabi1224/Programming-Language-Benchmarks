;;; modified by Bela Pecsek 2021-12-28
;;;   * CLOSS class changed to struct
;;;   * optional n-supplied added to main
;;;   * defmethod changed to defun for check-node
;;;   * Local functions for more speed
;;;   * Code cleanup
(declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sb-thread
  (defun get-thread-count ()
    (progn (define-alien-routine sysconf long (name int))
           (sysconf 84)))
  (defconstant min-depth   4 "Minimal depth of the binary tree.")
  (defconstant num-workers (get-thread-count) "Number of concurrent workers.")
  (deftype uint () '(unsigned-byte 31))
  (deftype index () 'sb-int:index))

(declaim (inline make-node left (setf left) right (setf right)))
(defstruct (node (:conc-name nil)
                 (:constructor make-node (left right)))
  (left  nil :type (or node null))
  (right nil :type (or node null)))

(declaim (maybe-inline build-tree check-node))
(defun build-tree (depth)
  (declare (type uint depth))
  (cond ((zerop depth) (make-node nil nil))
        (t (make-node (build-tree (- depth 1)) (build-tree (- depth 1))))))

(defun check-node (node)
  (declare (type node node)
           (dynamic-extent node))
  (cond ((left node) (+ 1 (check-node (left node)) (check-node (right node))))
        (t 1)))

(declaim (ftype (function (uint) null) loop-depths))
(defun loop-depths (max-depth)
  (declare (type uint max-depth))
  (labels ((build-tree (depth)
             (declare (type uint max-depth))
             (cond ((zerop depth) (make-node nil nil))
                   (t (make-node (build-tree (- depth 1)) (build-tree (- depth 1))))))
           (check-node (node)
             (declare (type node node))
             (cond ((left node) (truly-the uint (+ 1 (check-node (left node))
                                                     (check-node (right node)))))
                   (t 1))))
    (declare (inline build-tree check-node))
    (loop for depth of-type index from min-depth by 2 upto max-depth
          do (let ((iterations (ash 1 (+ max-depth min-depth (- depth)))) (check 0))
               (loop for i of-type uint from 1 upto iterations
                     do (incf check (check-node (build-tree depth))))
               (format t "~D	 trees of depth ~D	 check: ~D~%" iterations
                       depth check)))))

(declaim (ftype (function (uint) null) binary-trees-upto-size))
(defun binary-trees-upto-size (n)
  (declare (type (integer 0 255) n))
  (format t "stretch tree of depth ~d~c check: ~d~%" (1+ n) #\Tab
          (check-node (build-tree (1+ n))))
  (let ((long-lived-tree (build-tree n)))
    (loop-depths n)
    (format t "long lived tree of depth ~d~c check: ~d~%" n #\Tab
            (check-node long-lived-tree))))

(defun main (&optional n-supplied)
  (let ((n (or n-supplied (parse-integer (or (car (last #+sbcl sb-ext:*posix-argv*)) "18")))))
    (binary-trees-upto-size n)))
