#lang racket

(require "parser.rkt"
         "eval.rkt")

(define/match (handle args)
  [((list "run" file))
   (parameterize ([current-parser (make-parser file (open-input-file file))])
     (eval-module (parse-module)))]
  [((list "debug" file))
   (parameterize ([current-parser (make-parser file (open-input-file file))])
     (for ([s (parse-module)])
       (printf "statement: ~a\n" s)))])

(module+ main
  (require racket/cmdline)

  (command-line
    #:program "no"
    #:args args
    (handle args)))
