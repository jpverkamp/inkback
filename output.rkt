#lang racket

; Generate the output directory
(define output-dir (build-path "output" (backup-hash current-backup) (date->string (current-date))))
(make-directory* output-dir)

(define current-blob #f)

(make-directory* (build-path output-dir "chats"))
(for ([chat (in-list chats)])
  (define chat-output-dir 
    (build-path output-dir "chats"
                (~a (string-join (map (λ (user) (string-trim
                                                 (regexp-replace* #px"[ _/\\\\]+"
                                                                  (or (user-name user) (user-value user) (~a "unknown-" (gensym)))
                                                                  " ")))
                                      (chat-users chat))
                                 ", "))))
  
  (make-directory* chat-output-dir)
  (printf "Writing ~a\n" chat-output-dir)
    
  (with-output-to-file (build-path chat-output-dir "messages.json")
    #:exists 'replace
    (λ ()
      (define blob
       (hash 'users
             (for/list ([each (in-list (chat-users chat))])
               (match-define (user name value) each)
               (hash 'name name 'contact-information value))
             'messages
             (for/list ([each (in-list (chat-messages chat))])
               (match-define (message date service sender subject text attachments) each)
               (match-define (user sender-name sender-value) sender)
               
               ; TODO: map attachments to local filenames
               (hash 'date date
                     'service service
                     'sender (hash 'name sender-name 'contact-information sender-value)
                     'subject subject
                     'text text
                     'attachments attachments))))
      (set! current-blob blob)
      (write-json blob))))