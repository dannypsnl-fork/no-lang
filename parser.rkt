#lang racket

(provide current-parser
         make-parser
         parse-module)

(require "lexer.rkt"
         "ast.rkt")

(define current-parser (make-parameter #f))

; parse
(define (parse-module)
  (define ss '())
  (let loop ()
    (set! ss (append ss (list (parse-stmt))))
    (unless (predict? 'EOF)
      (loop)))
  ss)

(define (parse-stmt)
  (cond
    [(predict? 'identifier ':=)
     (parse-vardef)]
    [(predict? 'identifier 'lparens)
     (with-handlers ([(lambda (e) #t)
                      (lambda (e)
                        (parse-expr #f 1))])
       (parse-fndef))]
    [(predict? 'return)
     (consume 'return)
     (ret (parse-expr #f 1))]
    [else (parse-expr #f 1)]))

(define (parse-vardef)
  (define name (consume 'identifier))
  (consume ':=)
  (define expr (parse-expr #f 1))
  (vardef name expr))

(define (parse-fndef)
  (define name (consume 'identifier))
  (consume 'lparens)
  (define params '())
  (let loop ()
    (unless (predict? 'rparens)
      (set! params (append params (list (consume 'identifier
                                                 #:put-back 2))))
      (loop)))
  (consume 'rparens 'lbraces
           #:put-back (+ 2 (length params)))
  (define ss '())
  (let loop ()
    (set! ss (append ss (list (parse-stmt))))
    (unless (predict? 'rbraces)
      (loop)))
  (consume 'rbraces)
  (fndef name params ss))

(define (parse-expr left-hand-side previous-primary)
  (define lhs (if left-hand-side
                  left-hand-side
                  (parse-primary (parse-unary))))

  (let loop ([lookahead (peek)])
    (when (>= (precedence lookahead) previous-primary)
      (define operator lookahead)
      (take)
      (define rhs (parse-primary (parse-unary)))
      (set! lookahead (peek))
      (let loop ()
        (when (or (> (precedence lookahead) (precedence operator))
                  (and (right-assoc? lookahead)
                       (= (precedence lookahead) (precedence operator))))
          (set! rhs (parse-expr rhs (precedence lookahead)))
          (set! lookahead (peek))
          (loop)))
      (set! lhs (binary (token-typ operator)
                        lhs rhs))
      (loop lookahead)))

  lhs)

(define (parse-primary unary)
  (cond
    [(predict? 'lparens)
     (consume 'lparens)
     (let loop ([args '()])
       (cond
         [(predict? '(comma rparens))
          (consume 'comma 'rparens)
          (func-call unary args)]
         [(predict? 'rparens)
          (consume 'rparens)
          (func-call unary args)]
         [else
          (when (predict? 'comma)
            (consume 'comma))
          (define expr (parse-expr #f 1))
          (loop (append args (list expr)))]))]
    [else unary]))

(define (parse-unary)
  (define tok (peek))
  (case (token-typ tok)
    [(number) (take)
              (string->number (token-val tok))]
    [(true) (take)
            'true]
    [(false) (take)
             'false]
    [(identifier) (take)
                  (token-val tok)]
    [else (error 'unknown "~a" tok)]))

; helper
(struct parser (name lexer tokens offset)
  #:mutable
  #:transparent)

(define (make-parser name input)
  (define lexer (lex name input))
  (parser name lexer (stream) 0))

(define (peek [n 0])
  (get-token (+ (parser-offset (current-parser)) n)))
(define (put-back [n 1])
  (set-parser-offset! (current-parser) (- (parser-offset (current-parser)) n)))
(define (take [n 1])
  (define origin (parser-offset (current-parser)))
  (set-parser-offset! (current-parser) (+ origin n))
  (get-token origin))
(define (consume #:put-back [n 0] . wants)
  (with-handlers ([(lambda (e) #t)
                   (lambda (e)
                     (put-back n)
                     (raise e))])
    (apply predict wants))
  (take (length wants)))
(define (predict . wants)
  (for ([i (length wants)]
        [want wants])
    (define tok (peek i))
    (unless (eq? (token-typ tok) want)
      (error 'unexpected-token "want ~a, got ~a" want tok))))
(define (predict? . wants)
  (let/cc return
    (with-handlers ([(λ (e) #t)
                     (λ (e) (return #f))])
      (apply predict wants))
    #t))

(define (get-token fixed-offset)
  (define p (current-parser))
  (when (stream-empty? (parser-tokens p))
    (increase-token-stream p))
  (define tokens (parser-tokens p))
  (if (>= fixed-offset (stream-length tokens))
      (let ([last-token (stream-ref tokens (sub1 (stream-length tokens)))])
        (case (token-typ last-token)
          [(EOF) last-token]
          [else (increase-token-stream p)
                (get-token fixed-offset)]))
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
    (parameterize ([current-parser (make-parser name input)])
      (parse-expr #f 1)))

  (check-equal? (parse "parsing" (open-input-string "12 + 23 * 34"))
                (binary 'add 12 (binary 'mul 23 34)))

  (test-case "increase token stream automatically"
             (define (test-pos l c)
               (pos "" l c))
             (define lexer (lex "" (open-input-string "12 + 23 * 34")))
             (parameterize ([current-parser (parser "" lexer (stream) 0)])
               (check-equal? (get-token 4)
                             (token 'number "34" (test-pos 1 10) (test-pos 1 12)))))

  (test-case "right assoc"
             (check-equal? (parse "parsing" (open-input-string "12 ^ 23 ^ 34"))
                           (binary '^ 12 (binary '^ 23 34))))

  (check-equal? (parse "parsing" (open-input-string "true and true = true or false"))
                (binary 'eq
                        (binary 'and 'true 'true)
                        (binary 'or 'true 'false))))
