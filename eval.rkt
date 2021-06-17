#lang racket

(provide eval-module)

(require "ast.rkt"
         "lexer.rkt"
         "env.rkt")

(define (eval-module ss)
  (for ([s ss])
    (cond
      [(statement? s)
       (eval-stmt s)]
      [else (displayln (eval-expr s))])))

(define (eval-stmt s [return #f])
  (match s
    [(vardef name expr)
     (env/bind (token-val name) expr)]
    [(fndef name params body)
     (env/bind (token-val name)
         (lambda (args)
           (let/cc return
             (parameterize ([cur-env (make-env)])
               (for ([p params]
                     [a args])
                 (env/bind p (eval-expr a)))
                 (for ([s body])
               (eval-stmt s return))))))]
    [(ret e)
     (unless return
       (error 'cannot-return "You're not in a context can return, e.g. function"))
     (return (eval-expr e))]
    [(? expr?) (eval-expr s)]))

(define (eval-expr e)
  (match e
    [(binary op l r)
     (case op
       [(+) (+ (eval-expr l) (eval-expr r))]
       [(-) (- (eval-expr l) (eval-expr r))]
       [(*) (* (eval-expr l) (eval-expr r))]
       [(/) (/ (eval-expr l) (eval-expr r))])]
    [(func-call fn args) ((env/lookup fn) args)]
    [(? string?) (env/lookup e)]
    [else e]))
