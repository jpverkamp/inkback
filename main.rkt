#lang racket

(require "model/backup.rkt"
         "model/chat.rkt"
         "model/user.rkt")

(define name #f)
(define hash #f)

(command-line
 #:program "inkback"
 #:once-any 
 [("-n" "--name") param
                  "Specify the backup to load by name"
                  (set! name param)]
 [(     "--hash") param
                  "Specify the backup to load by hash"
                  (set! hash param)])

(cond
  [name (load-backup! #:by-name name)]
  [hash (load-backup! #:by-hash hash)]
  [else
   (error 'inkback "todo: implement interactive mode")])

(load-users!)
(load-chats!)

(current-users)
