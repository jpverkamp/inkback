#lang racket

(provide plist->jsexpr
         read-plist/jsexpr)

(require json
         xml/plist)

; Convert a plist into a JSON expression
(define (plist->jsexpr data)
  (match data
    [(? string?) data]
    [`(true) #t]
    [`(false) #f]
    [`(integer ,v) v]
    [`(real ,v) v]
    [`(data ,v) v] ; Should we special case these?
    [`(date ,v) v] ; Ditto 
    [`(array . ,v*)
     (map plist->jsexpr v*)]
    [`(dict . ,kv*)
     (for/hash ([kv (in-list kv*)])
       (values (string->symbol (second kv)) (plist->jsexpr (third kv))))]))

; Read a plist file as a JSON expression from a file
(define (read-plist/jsexpr [in (current-input-port)])
  (plist->jsexpr (read-plist in)))



