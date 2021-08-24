(declaim (optimize (speed 3)(safety 0)(space 0)(debug 0)))

(load (compile-file "app.cl"))
(sb-ext:save-lisp-and-die 
  "app"
  ;; http://www.sbcl.org/manual/#Function-sb_002dext-save_002dlisp_002dand_002ddie
  ;; :purify t
  ;; :compression t
  ;; this is the main function:
  :toplevel (lambda () 
              (main)                                      
              0)
  :executable t)
