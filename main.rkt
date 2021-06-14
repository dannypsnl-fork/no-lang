#lang racket

(module+ main
  (require racket/cmdline
           "parser.rkt"
           "eval.rkt")

  (command-line
    #:program "no"
    #:args (file)
    (define p (make-parser file (open-input-file file)))
    (eval-module (parse-module p))))
