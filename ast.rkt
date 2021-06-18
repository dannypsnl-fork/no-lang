#lang racket

(provide (all-defined-out))

(struct pos (filename line column) #:transparent)

(struct statement () #:transparent)
(struct vardef statement
  (name expr)
  #:transparent)
(struct fndef statement
  (name params stmts)
  #:transparent)
(struct ret statement (expr) #:transparent)

(struct expr () #:transparent)
(struct binary expr (op left right) #:transparent)
(struct func-call expr (fn args) #:transparent)
