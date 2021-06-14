#lang racket

(provide eval-module)

(require "ast.rkt"
         "env.rkt")

(define (eval-module ss)
  (for ([s ss])
    (match s
      [(vardef name expr)
       (env/bind name expr)]
      [(fndef name params body)
       (env/bind name body)]
      [else (displayln (eval-expr s))])))

(define (eval-expr e)
  (match e
    [(binary op l r)
     (case op
       [(+) (+ (eval-expr l) (eval-expr r))]
       [(-) (- (eval-expr l) (eval-expr r))]
       [(*) (* (eval-expr l) (eval-expr r))]
       [(/) (/ (eval-expr l) (eval-expr r))])]
    [else e]))
