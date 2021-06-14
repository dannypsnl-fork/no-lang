#lang racket

(provide make-env cur-env
         env/bind env/lookup)

(struct env (cur-map parent) #:transparent)
(define (make-env [parent (cur-env)])
  (env (make-hash) parent))
(define cur-env (make-parameter (make-env #f)))

(define (env/bind id val)
  (let* ([binding* (env-cur-map (cur-env))]
         [bound? (hash-ref binding* id #f)])
    (unless (not bound?)
      (error 'env "rebound: `~a`" id))
    (hash-set! binding* id val)))
(define (env/lookup id)
  (let* ([binding* (env-cur-map (cur-env))]
         [parent (env-parent (cur-env))])
    (hash-ref binding* id
              (λ () (if parent
                        (parameterize ([cur-env parent])
                          (env/lookup id))
                        #f)))))

(module+ test
  (require rackunit)

  (parameterize ([cur-env (make-env #f)])
    (env/bind 'a 1)
    (check-equal? (env/lookup 'a) 1)))
