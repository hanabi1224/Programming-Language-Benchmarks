(declaim (optimize (speed 3)(safety 0)(space 0)(debug 0)))

(defun main ()
(let ((n (or (car (last #+sbcl sb-ext:*posix-argv*
                        #+cmu  extensions:*command-line-strings*
					    #+gcl  si::*command-args*)) "")))
    (format t (concatenate 'string "Hello world " n "!"))))
