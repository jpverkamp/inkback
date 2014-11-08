#lang racket

(provide (struct-out chat)
         (struct-out message)
         current-chats
         load-chats!)

(require db
         file/sha1
         racket/date
         "../constants.rkt"
         "../model/backup.rkt"
         "../model/user.rkt")

(struct chat (users messages) #:transparent)
(struct message (date service sender subject text attachments) #:transparent)
(struct attachment (name path) #:transparent)

(define current-chats (make-parameter #f))

; Hash attachments so that we can find the local path 
(define (get-attachment-hash path [domain "MediaDomain"])
  (for*/first ([prefix (in-list (list "/var/mobile/" 
                                      "~/"))]
               #:when (and (> (string-length path) (string-length prefix))
                           (equal? (substring path 0 (string-length prefix)) prefix)))
    (define path-w/o-prefix (substring path (string-length prefix)))
    (call-with-input-string (~a domain "-" path-w/o-prefix) sha1)))

; Load all chats from a backup directory
(define (load-chats!)
  (parameterize ([date-display-format 'iso-8601])  
    ; Connect to the correct DB
    (define sms-db (sqlite3-connect #:database (build-path (backup-path (current-backup)) MESSAGES-DB)))
    
    ; Loop over the individual chat ids 
    (current-chats
     (for/list ([(chat-id) (in-query sms-db "SELECT ROWID FROM chat")])
       ; Determine which users were involved in the conversation by contact
       ; Use models/users.rkt to figure out who belongs to contact information
       (define user-query "SELECT id FROM chat_handle_join, handle WHERE chat_id = $1 AND handle_id = ROWID ORDER BY handle_id ASC")
       (define contacts
         (for/list ([(contact) (in-query sms-db user-query chat-id)])
           (normalize-contact contact)))
       
       ; Load the individual messages
       (define msg-query "
SELECT
  message.ROWID as message_id,
  date,
  message.service,
  is_from_me,
  handle.id as them,
  (CASE WHEN subject IS NULL THEN '' ELSE subject END),
  (CASE WHEN text IS NULL THEN '' ELSE text END),
  (SELECT group_concat(filename) FROM message_attachment_join, attachment WHERE message_id = message.ROWID AND attachment_id = attachment.ROWID)
FROM
  chat_message_join,
  message,
  handle
WHERE 
  chat_id = $1
  AND message_id = message.ROWID 
  AND handle_id = handle.ROWID
ORDER BY date ASC")
       (define messages
         (for/list ([(message-id raw-date service from-me? other-party subject text raw-attachments)
                     (in-query sms-db msg-query chat-id)])
           ; Correct dates from Apple time to unix time
           ; TODO: Account for timezones?
           (define date (date->string (seconds->date (+ raw-date 978336000)) #t))
           
           (define sender 
             (if (= 1 from-me?)
                 (backup-phone-number (current-backup))
                 (normalize-contact other-party)))
           
           ; Load attachments, 
           (define attachments 
             (if (sql-null? raw-attachments)
                 '()
                 (for/list ([path (in-list (string-split raw-attachments ","))])
                   (attachment 
                    (path->string (last (explode-path path)))
                    (build-path (backup-path (current-backup)) (get-attachment-hash path))))))
           
           (message date service sender subject text attachments)))
       
       ; Create the chat object
       (chat contacts messages)))))