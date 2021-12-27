(declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))
  
  (define-constant min-depth 4 "Minimal depth of the binary tree.")
  (define-constant num-workers 4 "Number of concurrent workers.")
  (deftype uint () '(unsigned-byte 62))
  (deftype index () 'sb-int:index))

(defclass node ()
   (
       (left :accessor left :initarg :left)
       (right :accessor right :initarg :right)
   )
)

(defmethod check-node ((n node))
    (if (null (left n)) 
        1 
        (+ 1 (check-node (left n)) (check-node (right n)))
    )
)

(defun build-tree (depth)
    (if (zerop depth) 
        (make-instance 'node 
                        :left nil
                        :right nil
        )
        (make-instance 'node 
                        :left (build-tree (- depth 1))
                        :right (build-tree (- depth 1))
        )
    )
)

(declaim (ftype (function (uint) null) loop-depths))
(defun loop-depths (max-depth)
  (declare (type uint max-depth)
	   (optimize (speed 1)))
    (loop for depth of-type index from min-depth by 2 upto max-depth do
        (let ((iterations (ash 1 (+ max-depth min-depth (- depth)))) (check 0))
            (loop for i of-type uint from 1 upto iterations do
                (setq check (+ check (check-node (build-tree depth))))
            )
            (            
                format t "~D	 trees of depth ~D	 check: ~D~%"
                    iterations
                    depth
                    check
            )
        )
    )
)

(declaim (ftype (function (uint) null) binary-trees-upto-size))
(defun binary-trees-upto-size (n)
  (declare (type (integer 0 255) n))
  (format t "stretch tree of depth ~d~c check: ~d~%" (1+ n) #\Tab
          (check-node (build-tree (1+ n))))
  (let ((long-lived-tree (build-tree n)))
    (loop-depths n)
    (format t "long lived tree of depth ~d~c check: ~d~%" n #\Tab
            (check-node long-lived-tree))))

(defun main ()
  (let ((n (parse-integer (or (car (last #+sbcl sb-ext:*posix-argv*
                                         #+cmu  extensions:*command-line-strings*
					 #+gcl  si::*command-args*)) "21"))))
    (binary-trees-upto-size n)))
