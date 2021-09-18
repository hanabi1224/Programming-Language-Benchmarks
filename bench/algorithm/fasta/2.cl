;; The Computer Language Benchmarks Game
;; https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
;;
;; Contributed by Lorenzo Bolla
;; Modified by Jason Miller
;; Optimized by Bela Pecsek, 2021-09-17

(declaim (optimize (speed 3) (debug 0) (safety 0)))

(deftype uint31 () '(unsigned-byte 31))
(declaim (uint31 +line-length+ +buffer-size+ +im+))
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

(declaim (ftype (function ((simple-array float (*))) (simple-array float (*))) cumsum))
(defun cumsum (lst)
  (let ((c 0.0))
    (declare (type single-float c))
    (map 'vector #'(lambda (x) 
                     (declare (type single-float x)
                              (values uint31))
                     (the uint31 (ceiling (the (single-float #.(float most-negative-fixnum)
                                                             #.(float most-positive-fixnum))
                                               (* +im+ (incf c x))))))
         lst)))

(declaim (ftype (function (simple-vector) (simple-array uint31 (*))) make-cprob))
(defun make-cprob (probs)
  (make-array (length probs)
              :element-type 'uint31
              :initial-contents (cumsum probs)))

(defparameter *amino-acids-syms* "acgtBDHKMNRSVWY")
(defparameter *amino-acids-cprobs* 
  (make-cprob #(0.27 0.12 0.12 0.27 0.02 0.02 0.02 0.02 0.02 0.02 0.02 0.02 0.02 0.02 0.02)))

(defparameter *homo-sapiens-syms* "acgt")
(defparameter *homo-sapiens-cprobs* 
  (make-cprob #(0.3029549426680 0.1979883004921 0.1975473066391 0.3015094502008)))

(declaim (inline find-amino-acid reset-random next-random))
(let ((r 42)
      (ia 3877)
      (ic 29573))
  (declare (type uint31 r ia ic)
           (inline reset-random next-random))
  (defun reset-random () (setf r (the uint31 42)))
  (defun next-random ()
    (declare (values uint31))
    (setf r (mod (logand #.(1- (ash 1 (integer-length most-positive-fixnum)))
                         (+ (the uint31 (* r ia)) ic)) +im+))))

(defun find-amino-acid (amino-acids-syms amino-acids-cprobs p)
  (declare (type (simple-array uint31 (*)) amino-acids-cprobs)
           (type simple-string amino-acids-syms)
           (type uint31 p))
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
  (declare (type simple-string amino-acids-syms)
           (type (simple-array uint31 (*)) amino-acids-cprobs)
           (type uint31 n))
  (output-line title)
  (loop with buf of-type simple-base-string = (make-string +buffer-size+ :element-type 'base-char)
        with i of-type uint31 = 0
        with max-j of-type uint31 = (1- +buffer-size+)
        for j of-type fixnum from 0
        for k of-type fixnum from 0
        while (< i n)
        if (= k +line-length+)
          do (setf (aref buf j) #\Newline) 
             (setf k -1)
        else do (incf i)
                (setf (aref buf j) 
                      (find-amino-acid amino-acids-syms amino-acids-cprobs (next-random)))
        end
        when (= j max-j)
          do (write-string buf *standard-output*)
             (setf j -1)
        finally (output-line buf :start 0 :end j)))

(defun repeat (alu title n)
  (declare (type simple-base-string alu) 
           (type uint31 n))
  (let ((len (length alu))
        (buf (concatenate 'simple-base-string 
                          alu 
                          (subseq alu 0 +line-length+))))
    (declare (type uint31 len) 
             (type simple-base-string buf))
    (output-line title)
    (do* ((pos-start 0 (mod pos-end len))
          (m n (- m bytes))
          (bytes (min n +line-length+) (min m +line-length+))
          (pos-end (+ pos-start bytes) (+ pos-start bytes)))
         ((<= m 0) (flush))
      (declare (type uint31 pos-start pos-end m bytes))
      (output-line buf :start pos-start :end pos-end))))

(defun main (&optional in-n)
  #+sbcl(setq *standard-output*
              (sb-impl::make-fd-stream 1 :output t
                                         :buffering :full
                                         :external-format :ascii))
  (let ((n (or in-n
               (ignore-errors
                (parse-integer
                 (car (last #+sbcl sb-ext:*posix-argv*
                            #+cmu  extensions:*command-line-strings*
                            #+gcl  si::*command-args*
                            #+clisp nil)))) 1000)))
    (declare (uint31 n))
    (reset-random)
    (repeat *alu* ">ONE Homo sapiens alu" (* n 2))
    (randomize *amino-acids-syms*
               *amino-acids-cprobs*
               ">TWO IUB ambiguity codes" (* n 3))
    (randomize *homo-sapiens-syms*
               *homo-sapiens-cprobs*
               ">THREE Homo sapiens frequency" (* n 5))))
