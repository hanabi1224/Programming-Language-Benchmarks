;;;  Code cleanup and formating by Bela Pecsek
;;;   * Local functions for speed
;;;   * MV lookup to improce slot access speed
;;;   * Some declaration improvement
(declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-constant (name value &optional doc)
    `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
       ,@(when doc (list doc))))
  (defun get-thread-count ()
    (progn (define-alien-routine sysconf long (name int))
           (sysconf 84)))
  (define-constant min-depth   4 "Minimal depth of the binary tree.")
  (define-constant num-workers (get-thread-count) "Number of concurrent workers.")
  (deftype uint () '(unsigned-byte 31))
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

(declaim (ftype (function (uint) null) loop-depths))
(defun loop-depths (max-depth)
  (declare (type uint max-depth))
  (labels ((build-tree (depth)
             (cond ((zerop depth) (make-instance 'node :left nil :right nil))
                   (t (make-instance 'node :left  (build-tree (- depth 1))
                                           :right (build-tree (- depth 1))))))
           (check-node (n)
             (multiple-value-bind (l r) (values-for-node n)
               (cond (l (truly-the uint (+ 1 (check-node l) (check-node r)))) 
                     (t 1)))))
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
  (let ((n (or n-supplied (parse-integer (or (car (last sb-ext:*posix-argv*)) "18")))))
    (binary-trees-upto-size n)))
