#lang racket

(struct statement () #:transparent)
(struct vardef statement
  (name expr)
  #:transparent)
(struct fndef statement
  (name stmts)
  #:transparent)

(struct expr () #:transparent)
(struct binary expr (op left right) #:transparent)
