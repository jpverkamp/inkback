#lang racket

(provide (all-defined-out))

(require db)

(define MESSAGES-DB "3d0d7e5fb2ce288813306e4d4636395e047a3d28")
(define CONTACTS-DB "31bb7ba8914766d4ba40d6dfb6113c8b614be442")

; Process a phone number or email address into a common format
(define (normalize-contact value)
  (cond
    [(sql-null? value)
     #f]
    ; Standard phone numbers
    ; TODO: Figure out international numbers
    [(regexp-match #px"^\\+?1? ?[\\(\\.]?(\\d\\d\\d)[\\)\\.-]? ?(\\d\\d\\d)[ \\.-]?(\\d\\d\\d\\d)$" value)
     => (λ (match) (string-join (map ~a (rest match)) "."))]
    ; Email addresses
    [(regexp-match #px"^[^@]+@[^@]+$" value)
     value]
    ; Short phone numbers
    [(regexp-match #px"^\\d{,6}$" value)
     value]
    ; No idea...
    [else #f]))