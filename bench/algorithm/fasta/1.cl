;; The Computer Language Benchmarks Game
;; https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
;;
;; Contributed by Lorenzo Bolla
;; Modified by Jason Miller

(declaim (optimize (speed 3) (debug 0) (safety 0)))

(declaim (fixnum +line-length+ +buffer-size+ +im+))
(defconstant +line-length+ 60)
(defconstant +buffer-size+ 3000)
(defconstant +im+ 139968)


(declaim (simple-base-string *alu*))
(defparameter *alu* (concatenate 'simple-base-string
                                 "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG"
                                 "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA"
                                 "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT"
                                 "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA"
                                 "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG"
                                 "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC"
                                 "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"))

(defun cumsum (lst)
  (let ((c 0.0))
    (declare (type single-float c))
    (declare (type (vector float) lst))
    (map 'vector #'(lambda (x) 
                     (declare (type single-float x)
                              (values fixnum))
                     (the fixnum (ceiling (the (single-float #.(float most-negative-fixnum)
                                                             #.(float most-positive-fixnum))
                                               (* +im+ (incf c x))))))
         lst)))

(defun make-cprob (probs)
  (declare (type vector probs))
  (make-array (length probs)
              :element-type 'fixnum
              :initial-contents (cumsum probs)))

(defparameter *amino-acids-syms* "acgtBDHKMNRSVWY")
(defparameter *amino-acids-cprobs* 
  (make-cprob #(0.27 0.12 0.12 0.27 0.02 0.02 0.02 0.02 0.02 0.02 0.02 0.02 0.02 0.02 0.02)))

(defparameter *homo-sapiens-syms* "acgt")
(defparameter *homo-sapiens-cprobs* 
  (make-cprob #(0.3029549426680 0.1979883004921 0.1975473066391 0.3015094502008)))

(let ((r 42)
      (ia 3877)
      (ic 29573))
  (declare (type fixnum r ia ic))
  (defun reset-random () (setf r (the fixnum 42)))
  (declaim (inline next-random))
  (defun next-random ()
    (declare (values fixnum))
    (setf r (mod (+ (the (integer 0 542655936) (* r ia)) ic) +im+))))

(declaim (inline find-amino-acid next-random repeat))
(defun find-amino-acid (amino-acids-syms amino-acids-cprobs p)
  (declare (type (simple-array fixnum (*)) amino-acids-cprobs)
           (type simple-string amino-acids-syms)
           (type fixnum p))
  (let ((i (position-if (lambda (x) (< p x)) amino-acids-cprobs)))
    (if i
        (aref amino-acids-syms i)
        (aref amino-acids-syms (1- (length amino-acids-syms))))))

(declaim (inline output-line flush))
(defun output-line (line &key (start 0) (end nil))
  (write-line line *standard-output* :start start :end end))
(defun flush ()
  (finish-output *standard-output*))

(defun randomize (amino-acids-syms amino-acids-cprobs title n)
  (declare (type fixnum n))
  (declare (type (simple-array fixnum (*)) amino-acids-cprobs))
  (output-line title)
  (loop with buf of-type simple-base-string = (make-string +buffer-size+ :element-type 'base-char)
        with i of-type fixnum = 0
        with max-j of-type fixnum = (1- +buffer-size+)
        for j of-type fixnum from 0
        for k of-type fixnum from 0
        while (< i n)
        if (= k +line-length+) do 
          (setf (aref buf j) #\Newline) 
          (setf k -1)
        else do 
          (incf i)
          (setf (aref buf j) 
                (find-amino-acid amino-acids-syms amino-acids-cprobs (next-random)))
        end
        when (= j max-j) do 
          (write-string buf *standard-output*)
          (setf j -1)
        finally 
           (output-line buf :start 0 :end j)
                                        ;(flush)
        ))

(defun repeat (alu title n)
  (declare (type simple-base-string alu) 
           (type fixnum n))
  (let ((len (length alu))
        (buf (concatenate 'simple-base-string 
                          alu 
                          (subseq alu 0 +line-length+))))
    (declare (type fixnum len) 
             (type simple-base-string buf))
    (output-line title)
    (do* ((pos-start 0 (mod pos-end len))
          (m n (- m bytes))
          (bytes (min n +line-length+) (min m +line-length+))
          (pos-end (+ pos-start bytes) (+ pos-start bytes)))
         ((<= m 0) (flush))
      (declare (type fixnum pos-start pos-end m bytes))
      (output-line buf :start pos-start :end pos-end))))

(defun main (&optional in-n)
  #+sbcl(setq *standard-output*
              (sb-impl::make-fd-stream 1
                                       :output t
                                       :buffering :full
                                       :external-format :ascii))
  (let ((n (or in-n
               (ignore-errors
                (parse-integer
                 (car
                  (last #+sbcl sb-ext:*posix-argv*
                        #+cmu  extensions:*command-line-strings*
                        #+gcl  si::*command-args*
                        #+clisp nil))))
               1000)))
    (declare (fixnum n))
    (reset-random)
    (repeat *alu* ">ONE Homo sapiens alu" (the fixnum (* n 2)))
    (randomize *amino-acids-syms*
               *amino-acids-cprobs*
               ">TWO IUB ambiguity codes" (the fixnum (* n 3)))
    (randomize *homo-sapiens-syms*
               *homo-sapiens-cprobs*
               ">THREE Homo sapiens frequency" (the fixnum (* n 5)))))


(eval-when (:compile-toplevel :load-toplevel :execute) (require :sb-sprof))
(defun mainp (n)
  (sb-sprof:with-profiling (:loop nil :report :graph)
    (main n)))
