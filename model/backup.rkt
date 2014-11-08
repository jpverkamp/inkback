#lang racket

(provide (struct-out backup)
         current-backup
         list-backups
         load-backup!)

(require xml/plist
         "../constants.rkt"
         "../lib/plist-jsexpr.rkt")

; Represents an iPhone backup on disk
(struct backup (name hash date phone-number path) #:transparent)

; Store the most recently used backup for other modules
(define current-backup (make-parameter #f))

; Load all backups on disk into a list
(define (list-backups)
  ; Automatically find the backup path
  (define backup-root
    (for*/first ([path-parts (in-list '(("AppData" "Roaming" "Apple Computer" "MobileSync" "Backup")
                                        ("Library" "Application Support" "MobileSync" "Backup")))]
                 [path (in-value (apply build-path (cons (find-system-path 'home-dir) path-parts)))]
                 #:when (directory-exists? path))
      path))
  
  ; Determine which backup we are parsing
  (for/list ([dir (in-list (directory-list backup-root))])
    (define info-file (call-with-input-file (build-path backup-root dir "Info.plist") read-plist/jsexpr))
    (backup (dict-ref info-file '|Device Name|)
            (path->string dir)
            (dict-ref info-file '|Last Backup Date|)
            (normalize-contact (dict-ref info-file '|Phone Number|))
            (build-path backup-root dir))))

; Load a specific backup by either name or hash 
(define (load-backup! #:by-name [name #f] #:by-hash [hash #f])
  (when (and name hash) (error 'load-backup "cannot specify both name and hash"))
  (when (not (or name hash)) (error 'load-backup "must specify either name or hash"))
  
  (for/first ([backup (in-list (list-backups))]
              #:when (or (and name (equal? name (backup-name backup)))
                         (and hash (equal? hash (backup-hash backup)))))
    (current-backup backup)))
