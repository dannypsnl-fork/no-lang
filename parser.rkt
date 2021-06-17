#lang racket

(provide make-parser
         parse-module)

(require "lexer.rkt"
         "ast.rkt")

; parse
(define (parse-module p)
  (define ss '())
  (let loop ()
    (set! ss (append ss (list (parse-stmt p))))
    (unless (predict? p 'EOF)
      (loop)))
  ss)

(define (parse-stmt p)
  (cond
    [(predict? p 'identifier ':=)
     (parse-vardef p)]
    [(predict? p 'identifier 'lparens)
     (with-handlers ([(lambda (e) #t)
                      (lambda (e)
                        (put-back p 3)
                        (parse-expr p #f 1))])
       (parse-fndef p))]
    [(predict? p 'return)
     (consume p 'return)
     (ret (parse-expr p #f 1))]
    [else (parse-expr p #f 1)]))

(define (parse-vardef p)
  (define name (consume p 'identifier))
  (consume p ':=)
  (define expr (parse-expr p #f 1))
  (vardef name expr))

(define (parse-fndef p)
  (define name (consume p 'identifier))
  (consume p 'lparens)
  (define params '())
  (let loop ()
    (set! params (append params (list (take p))))
    (unless (predict? p 'rparens)
      (loop)))
  (consume p 'rparens 'lbraces)
  (define ss '())
  (let loop ()
    (set! ss (append ss (list (parse-stmt p))))
    (unless (predict? p 'rbraces)
      (loop)))
  (consume p 'rbraces)
  (fndef name params ss))

(define (parse-expr p left-hand-side previous-primary)
  (define lhs (if left-hand-side
                  left-hand-side
                  (parse-primary p (parse-unary p))))

  (let loop ([lookahead (peek p)])
    (when (>= (precedence lookahead) previous-primary)
      (define operator lookahead)
      (take p)
      (define rhs (parse-primary p (parse-unary p)))
      (set! lookahead (peek p))
      (let loop ()
        (when (or (> (precedence lookahead) (precedence operator))
                  (and (right-assoc? lookahead)
                       (= (precedence lookahead) (precedence operator))))
          (set! rhs (parse-expr p rhs (precedence lookahead)))
          (set! lookahead (peek p))
          (loop)))
      (set! lhs (binary (token-typ operator)
                        lhs rhs))
      (loop lookahead)))

  lhs)

(define (parse-primary p unary)
  (cond
    [(predict? p 'lparens)
     (consume p 'lparens)
     (let loop ([args '()])
       (cond
         [(predict? p '(comma rparens))
          (consume p 'comma 'rparens)
          (func-call unary args)]
         [(predict? p 'rparens)
          (consume p 'rparens)
          (func-call unary args)]
         [else
          (when (predict? p 'comma)
            (consume p 'comma))
          (define expr (parse-expr p #f 1))
          (loop (append args (list expr)))]))]
    [else unary]))

(define (parse-unary p)
  (define tok (peek p))
  (case (token-typ tok)
    [(number) (take p)
              (string->number (token-val tok))]
    [(true) (take p)
            'true]
    [(false) (take p)
             'false]
    [(identifier) (take p)
                  (token-val tok)]
    [else (error 'unknown "~a" tok)]))

; helper
(struct parser (name lexer tokens offset)
  #:mutable
  #:transparent)

(define (make-parser name input)
  (define lexer (lex name input))
  (parser name lexer (stream) 0))

(define (peek p [n 0])
  (get-token p (+ (parser-offset p) n)))
(define (put-back p [n 1])
  (set-parser-offset! p (- (parser-offset p) n)))
(define (take p [n 1])
  (define origin (parser-offset p))
  (set-parser-offset! p (+ origin n))
  (get-token p origin))
(define (consume p . wants)
  (apply predict (cons p wants))
  (take p (length wants)))
(define (predict p . wants)
  (for ([i (length wants)]
        [want wants])
    (define tok (peek p i))
    (unless (eq? (token-typ tok) want)
      (error 'unexpected-token "want ~a, got ~a" want tok))))
(define (predict? p . wants)
  (let/cc return
    (with-handlers ([(λ (e) #t)
                     (λ (e) (return #f))])
      (apply predict (cons p wants)))
    #t))

(define (get-token p fixed-offset)
  (when (stream-empty? (parser-tokens p))
    (increase-token-stream p))
  (define tokens (parser-tokens p))
  (if (>= fixed-offset (stream-length tokens))
      (let ([last-token (stream-ref tokens (sub1 (stream-length tokens)))])
        (case (token-typ last-token)
          [(EOF) last-token]
          [else (increase-token-stream p)
                (get-token p fixed-offset)]))
      (stream-ref tokens fixed-offset)))
(define (increase-token-stream p)
  (define l (parser-lexer p))
  (define new-last-token (channel-get (lexer-tokens l)))
  (set-parser-tokens! p
                      (stream-append (parser-tokens p) (stream new-last-token))))

(define (right-assoc? token)
  (case (token-typ token)
    [(^) #t]
    [else #f]))
(define (precedence token)
  (define op** '((eq)
                 (and or)
                 (add sub)
                 (mul div ^)))
  (define m (make-hash))
  (for ([i (length op**)]
        [op* op**])
    (for ([op op*])
      (hash-set! m op (+ 2 i))))
  (hash-ref m (token-typ token) 0))

(module+ test
  (require rackunit)

  (define (parse name input)
    (define p (make-parser name input))
    (parse-expr p #f 1))

  (check-equal? (parse "parsing" (open-input-string "12 + 23 * 34"))
                (binary 'add 12 (binary 'mul 23 34)))

  (test-case "increase token stream automatically"
             (define (test-pos l c)
               (pos "" l c))
             (define lexer (lex "" (open-input-string "12 + 23 * 34")))
             (define p (parser "" lexer (stream) 0))
             (check-equal? (get-token p 4)
                           (token 'number "34" (test-pos 1 10) (test-pos 1 12))))

  (test-case "right assoc"
             (check-equal? (parse "parsing" (open-input-string "12 ^ 23 ^ 34"))
                           (binary '^ 12 (binary '^ 23 34))))

  (check-equal? (parse "parsing" (open-input-string "true and true = true or false"))
                (binary 'eq
                        (binary 'and 'true 'true)
                        (binary 'or 'true 'false))))
