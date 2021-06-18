#lang racket

(provide run-server)

(require lsp
         web-server/servlet-env
         web-server/http/request-structs
         web-server/http/json
         json
         "../lexer.rkt"
         "../parser.rkt"
         "../ast.rkt")

(define (convert-pos p)
  (match-define (pos path line col) p)
  (Pos #:line line #:char col))
(define (range start end)
  (Range #:start (convert-pos start)
         #:end (convert-pos end)))

(define/match (user-handler method params succ err)
  [("initialize" params s e)
   (initialize params)]
  [("textDocument/documentHighlight" params s e)
   (define path (uri->path (hash-ref (hash-ref params 'textDocument) 'uri)))
   (define l (lex path (open-input-file path)))
   (define highlights '())
   (succ (let loop ([tok (channel-get (lexer-tokens l))])
     (match (token-typ tok)
       ['EOF highlights]
       ['identifier
        (set! highlights
              (append
               highlights
               (list (DocumentHighlight
                      (range (token-start-pos tok)
                             (token-end-pos tok))
                      DocumentHighlightKind.Read))))
        (loop (channel-get (lexer-tokens l)))]
       [typ
        (set! highlights
              (append
               highlights
               (list (DocumentHighlight
                      (range (token-start-pos tok)
                             (token-end-pos tok))
                      #f))))
        (loop (channel-get (lexer-tokens l)))])))]
  [(method params s e)
   (error 'fail "not implement ~a" method)])

(define (run-server)
  (serve/servlet
   (jsonrpc-start user-handler)
   #:port 4389
   #:launch-browser? #f
   #:servlet-path "/jsonrpc"))

(define (jsonrpc-start jsonrpc-handler)
  (λ (request)
    (define json-str (request-post-data/raw request))
    (match json-str
      [#f
       (response/jsexpr
        (hasheq 'jsonrpc "2.0"
                'id "0"
                'result '()))]
      [json-str
       (define json (bytes->jsexpr json-str))
       (response/jsexpr
         (jsonrpc-handler (hash-ref json 'method)
                          (hash-ref json 'params)
                          (succ json) (err json)))])))

(define (succ json)
  (lambda (result)
    (hasheq 'jsonrpc "2.0"
            'id (hash-ref json 'id)
            'result result)))
(define (err json)
  (lambda (e)
    (hasheq 'jsonrpc "2.0"
            'id (hash-ref json 'id)
            'error e)))
