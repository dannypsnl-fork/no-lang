#lang racket

(require "parser.rkt"
         "eval.rkt")

(define/match (handle args)
  [((list "run" file))
   (define p (make-parser file (open-input-file file)))
   (eval-module (parse-module p))]
  [((list "debug" file))
   (define p (make-parser file (open-input-file file)))
   (for ([s (parse-module p)])
     (printf "statement: ~a\n" s))])

(module+ main
  (require racket/cmdline)

  (command-line
    #:program "no"
    #:args args
    (handle args)))
