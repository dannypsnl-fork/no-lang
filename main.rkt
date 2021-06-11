#lang racket

(module+ main
  (require racket/cmdline)

  (command-line
    #:program "no"
    #:args (file)
    (void)))
