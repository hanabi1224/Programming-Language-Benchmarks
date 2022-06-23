;;;  Code cleanup and formating by Bela Pecsek
;;;   * Local functions for speed
;;;   * MV lookup to improce slot access speed
;;;   * Some declaration improvement
(declaim (optimize speed (safety 0) (space 0) (debug 0)))

(defconstant min-depth 4 "Minimal depth of the binary tree.")
(deftype uint () '(unsigned-byte 31))

(defclass node ()
   ((left  :type node :accessor left  :initarg :left)
    (right :type node :accessor right :initarg :right)))

(declaim (inline make-node))
(defun make-node (left right)
  (declare (type (or node null) left right))
  (make-instance 'node :left left :right right))

(declaim (ftype (function (node) uint) check-node))
(defun check-node (node)
  (declare (type node node))
  (with-accessors ((l left) (r right)) node 
    (cond (l (the uint (+ 1 (check-node l) (check-node r)))) 
          (t 1))))

(declaim (ftype (function (uint) node) build-tree))
(defun build-tree (depth)
  (declare (type uint depth))
  (cond ((zerop depth) (make-node nil nil))
        (t (make-node (build-tree (1- depth)) (build-tree (1- depth))))))

(declaim (ftype (function (uint) null) loop-depths))
(defun loop-depths (max-depth)
  (declare (type uint max-depth))
  (labels ((build-tree (depth)
             (declare (type uint depth))
             (cond ((zerop depth) (make-node nil nil))
                   (t (make-node (build-tree (1- depth))(build-tree (1- depth))))))
           (check-node (node)
             (declare (type node node))
             (with-accessors ((l left) (r right)) node 
               (cond (l (the uint (+ 1 (check-node l) (check-node r)))) 
                     (t 1)))))
    (declare (inline build-tree check-node))
    (loop for depth of-type uint from min-depth by 2 upto max-depth do
      (loop with iterations of-type uint = (ash 1 (+ max-depth min-depth (- depth)))
            for i of-type uint from 1 upto iterations
            summing (check-node (build-tree depth)) into result of-type uint
            finally (format t "~D	 trees of depth ~D	 check: ~D~%"
                                    iterations depth result)))))

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
