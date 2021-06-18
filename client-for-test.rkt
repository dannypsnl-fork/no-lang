#lang racket

(require jsonrpc
         lsp)

(jsonrpc-send!
 "http://localhost:4389/jsonrpc"
 (hasheq 'jsonrpc "2.0"
         'id "0"
         'method "initialize"
         'params (hasheq 'processId 0
                         'rootUri "file:///Users/linzizhuan/dannypsnl/no/test.no"
                         'capabilities (hasheq 'hoverProvider #t
                                               'definitionProvider #t
                                               'documentSymbolProvider #t
                                               'documentLinkProvider #t
                                               'documentFormattingProvider #t
                                               'documentRangeFormattingProvider #t))))

(jsonrpc-send!
 "http://localhost:4389/jsonrpc"
 (hasheq 'jsonrpc "2.0"
         'id "1"
         'method "textDocument/documentHighlight"
         'params (hasheq 'textDocument
                         (hasheq 'uri "file:///Users/linzizhuan/dannypsnl/no/test.no")
                         'position
                         (hasheq 'line 1
                                 'character 0))))