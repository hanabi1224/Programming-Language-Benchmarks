#lang racket/base

(require racket/cmdline)

(define (hello n)
  (printf (string-append "Hello world " n "!"))
)

(hello (command-line #:args (n) n))
