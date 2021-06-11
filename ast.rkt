#lang racket

(provide (struct-out statement)
         (struct-out vardef)
         (struct-out fndef)
         (struct-out expr)
         (struct-out binary))

(struct statement () #:transparent)
(struct vardef statement
  (name expr)
  #:transparent)
(struct fndef statement
  (name stmts)
  #:transparent)

(struct expr () #:transparent)
(struct binary expr (op left right) #:transparent)
