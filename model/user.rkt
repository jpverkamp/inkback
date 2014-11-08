#lang racket

(provide (struct-out user)
         current-users
         load-users!
         name->user
         contact->user)

(require db
         memoize
         "../constants.rkt"
         "../model/backup.rkt")

(struct user (name contacts) #:transparent)

(define current-users (make-parameter #f))

; Load all users stored in a specific backup
(define (load-users!)
  (define contacts-db (sqlite3-connect #:database (build-path (backup-path (current-backup)) CONTACTS-DB)))

  (current-users
   (for/list ([(user-id first-name middle-name last-name organization)
               (in-query contacts-db "SELECT ROWID, First, Middle, Last, Organization FROM ABPerson")])
     
     (define (fix str) (if (sql-null? str) "" str))
     
     (define name
       (let* ([name (~a (fix first-name) " " (fix middle-name) " " (fix last-name) " (" (fix organization) ")")]
              [name (regexp-replace* #px"\\(\\)" name "")]
              [name (regexp-replace* #px"\\s+" name " ")]
              [name (string-trim name)]
              [name (regexp-replace* #px"^\\((.*)\\)$" name "\\1")])
         name))
     
     (define contacts
       (for*/list ([raw-value (in-list (query-list contacts-db "SELECT value FROM ABMultiValue WHERE record_id = $1" user-id))]
                   [value (in-value (normalize-contact raw-value))]
                   #:when value)
         value))
     
     (user name contacts))))

; Load a user by name
(define/memo (name->user name)
  (when (not (current-users)) (load-users!))
  (for/first ([user (in-list (current-users))]
              #:when (equal? name (user-name user)))
    user))

; Load a user by one of their contact addresses
(define/memo (contact->user contact)
  (when (not (current-users)) (load-users!))
  (for/first ([user (in-list (current-users))]
              #:when (member contact (user-contacts user)))
    user))
