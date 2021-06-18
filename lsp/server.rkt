#lang racket

(provide run-server)

(require jsonrpc
         lsp
         web-server/servlet-env
         "../lexer.rkt"
         "../parser.rkt"
         "../ast.rkt")

(define/match (user-handler method params)
  [("initialize" params)
   (initialize params)]
  [("textDocument/documentHighlight" params)
   (define path (uri->path (hash-ref (hash-ref params 'textDocument) 'uri)))
   (define l (lex path (open-input-file path)))
   (define highlights '())
   (let loop ([tok (channel-get (lexer-tokens l))])
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
        (loop (channel-get (lexer-tokens l)))]))]
  [(method params)
   (printf "not implement ~a" method)
   params])

(define (run-server)
  (serve/servlet
   (jsonrpc-start user-handler)
   #:port 4389
   #:launch-browser? #f
   #:servlet-path "/jsonrpc"))
