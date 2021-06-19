#lang racket

(provide run-server)

(require lsp
         web-server/servlet-env
         web-server/http/request-structs
         web-server/http/json
         json
         mzlib/cml
         racket/exn
         "../lexer.rkt"
         "../parser.rkt"
         "../ast.rkt")

(define (convert-pos p)
  (match-define (pos path line col) p)
  (Pos #:line line #:char col))
(define (range start end)
  (Range #:start (convert-pos start)
         #:end (convert-pos end)))

;; https://www.cs.utah.edu/plt/publications/pldi04-ff.pdf

(struct Q (in-ch out-ch mgr-t))

(define (queue)
  (define in-ch (channel))
  (define out-ch (channel))
  (define (serve msgs)
    (cond [(empty? msgs)
           (serve (list (sync (channel-recv-evt in-ch))))]
          [else
           (sync (choice-evt
                  (wrap-evt
                   (channel-recv-evt in-ch)
                   (λ (m)
                     (serve (append msgs (list m)))))
                  (wrap-evt
                   (channel-send-evt out-ch (first msgs))
                   (thunk*
                    (serve (rest msgs))))))]))
  (define mgr-t (spawn (λ () (serve empty))))
  (Q in-ch out-ch mgr-t))

(define (queue-send-evt q v)
  (guard-evt
   (λ ()
     (thread-resume (Q-mgr-t q) (current-thread))
     (channel-send-evt (Q-in-ch q) v))))

(define (queue-recv-evt q)
  (guard-evt
   (λ ()
     (thread-resume (Q-mgr-t q) (current-thread))
     (channel-recv-evt (Q-out-ch q)))))

(define (report-error exn)
  (eprintf "\nCaught exn:\n~a\n" (exn->string exn)))

(define already-initialized? #f)
(define already-shutdown? #f)
(define (shutdown id)
  (set! already-shutdown? #t)
  (success-response id (json-null)))
(define ((report-request-error id method) exn)
  (eprintf "Caught exn in request ~v\n~a\n" method (exn->string exn))
  (define err (format "internal error in method ~v" method))
  (error-response id INTERNAL-ERROR err))

(define (process-notification method params)
  (match method
    ["exit"
     (exit (if already-shutdown? 0 1))]
    ["textDocument/didOpen"
     (error 'fixme)]
    ["textDocument/didClose"
     (error 'fixme)]
    ["textDocument/didChange"
     (error 'fixme)]
    [_ (void)]))

(define (process-message msg)
  (match msg
    ;; Request
    [(hash-table ['id (? (or/c number? string?) id)]
                 ['method (? string? method)])
     (define params (hash-ref msg 'params hasheq))
     (define response (process-request id method params))
     (display-message/flush response)]
    ;; Notification
    [(hash-table ['method (? string? method)])
     (define params (hash-ref msg 'params hasheq))
     (process-notification method params)]
    ;; Batch Request
    [(? (non-empty-listof (and/c hash? jsexpr?)))
     (for-each process-message msg)]
    ;; Invalid Message
    [_
     (define id-ref (hash-ref msg 'id void))
     (define id (if ((or/c number? string?) id-ref) id-ref (json-null)))
     (define err "The JSON sent is not a valid request object.")
     (display-message/flush (error-response id INVALID-REQUEST err))]))
;; Processes a request. This procedure should always return a jsexpr
;; which is a suitable response object.
(define (process-request id method params)
  (with-handlers ([exn:fail? (report-request-error id method)])
    (match method
      ["initialize"
       (initialize id params)]
      ["shutdown"
       (shutdown id)]
      ["textDocument/hover"
       (error 'unimplement)]
      ["textDocument/completion"
       (error 'unimplement)]
      ["textDocument/signatureHelp"
       (error 'unimplement)]
      ["textDocument/definition"
       (error 'unimplement)]
      ["textDocument/documentHighlight"
       (error 'unimplement)]
      ["textDocument/references"
       (error 'unimplement)]
      ["textDocument/documentSymbol"
       (error 'unimplement)]
      ["textDocument/rename"
       (error 'unimplement)]
      ["textDocument/prepareRename"
       (error 'unimplement)]
      ["textDocument/formatting"
       (error 'unimplement) ]
      [_
       (eprintf "invalid request for method ~v\n" method)
       (define err (format "The method ~v was not found" method))
       (error-response id METHOD-NOT-FOUND err)])))

(define (run-server)
  (define q (queue))
  (define (consume)
    (define msg (sync (queue-recv-evt q)))
    (match msg
      ['parse-json-error
       (define err "Invalid JSON was received by the server.")
       (display-message/flush (error-response (json-null) PARSE-ERROR err))]
      [_
       (with-handlers ([exn:fail? report-error])
         (process-message msg))])
    (consume))
  (spawn consume)
  (for ([msg (in-port read-message)])
    (sync (queue-send-evt q msg)))
  (eprintf "Unexpected EOF\n")
  (exit 1))
