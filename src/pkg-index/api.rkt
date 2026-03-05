#lang racket/base
;; Business logic for package management, extracted from dynamic.rkt.
;; This module contains no HTTP handlers or side effects at load time.
;; site.rkt calls these functions directly; dynamic.rkt uses them in HTTP handlers.

(provide curation-administrator?
         superuser?
         current-user
         package-exists-as
         package-remove!
         save-package!
         curate-packages!
         package-author?
         packages-of
         ensure-authenticate/email+passwd
         authenticate-user
         delete-package!/authorized
         update-user-packages!
         valid-versions-list-entry?
         ensure-package-author
         tags-normalize)

(require racket/list
         racket/match
         racket/set
         racket/string
         version/utils
         (only-in infrastructure-userdb user-password-correct? lookup-user)
         "common.rkt"
         "update.rkt"
         "static.rkt")

(define (curation-administrator? u)
  (member u
          '("jay.mccarthy@gmail.com" "mflatt@cs.utah.edu"
                                     "samth@ccs.neu.edu"
                                     "stamourv@racket-lang.org"
                                     "tonygarnockjones@gmail.com"
                                     "clements@racket-lang.org")))

;; This predicate means "Can `u` edit or delete arbitrary packages?"
;; For now, it's the same set of people as can curate packages, but we
;; can think about how we want to do this in future.
(define (superuser? u)
  (curation-administrator? u))

(define current-user (make-parameter #f))

(define (package-remove! pkg-name)
  (delete-file (build-path^ pkgs-path pkg-name)))

;; returns package in its registered case or #f if no such package
(define (package-exists-as pkg-name)
  ;; check for a case-insensitive match, which on a case-sensitive
  ;; filesystem helps avoid creating problems for a
  ;; case-insensitive filesystem
  (for/or ([p (in-list (directory-list pkgs-path))])
    (define name (path-element->string p))
    (and (string-ci=? name pkg-name) name)))

(define (ensure-authenticate/email+passwd email passwd body-fun)
  (log! "Checking credentials of user ~v" email)
  (if (user-password-correct? (lookup-user userdb email) passwd)
      (parameterize ([current-user email])
        (body-fun))
      "not-authenticated"))

;; Authenticate user and return curator/superuser info.
;; Returns #f on failure, or (hasheq 'curation ... 'superuser ...) on success.
(define (authenticate-user email password)
  (and (string? email)
       (string? password)
       (match (ensure-authenticate/email+passwd email password (λ () #t))
         ["not-authenticated" #f]
         [#t
          (hasheq 'curation
                  (and (curation-administrator? email) #t)
                  'superuser
                  (and (superuser? email) #t))])))

(define (valid-versions-list-entry? entry)
  (and (pair? entry)
       (pair? (cdr entry))
       (null? (cddr entry))
       (valid-version? (car entry))
       (string? (cadr entry))))

(define (tags-normalize ts)
  (remove-duplicates (sort ts string-ci<?)))

(define (ensure-package-author pkg f)
  (cond
    [(package-author? pkg (current-user)) (f)]
    [(superuser? (current-user))
     (log! "user ~v invoked their superpowers to modify package ~v" (current-user) pkg)
     (f)]
    [else
     (log! "attempt to modify package ~v by ~v failed because they are not an author of that package"
           pkg
           (current-user))
     #f]))

;; Call ONLY within scope of an ensure-authenticate! (because depends on non-#f current-user))
(define (save-package! #:old-name old-name
                       #:new-name new-name
                       #:description description
                       #:source source
                       #:tags tags0
                       #:authors authors0
                       #:versions versions0)
  (when (not (current-user))
    (error 'save-package! "No current-user"))
  (define new-package? (equal? old-name ""))
  (define (do-save! base-hash)
    (let* ([h base-hash]
           [h (cond
                [authors0
                 (define authors1
                   (if (superuser? (current-user))
                       authors0
                       (set->list (set-add (list->set authors0) (current-user)))))
                 (hash-set h 'author (string-join authors1))]
                [new-package? (hash-set h 'author (current-user))]
                [else h])]
           [h (if tags0
                  (hash-set h 'tags (tags-normalize tags0))
                  h)]
           [h (if versions0
                  (hash-set h
                            'versions
                            (for/hash ([v versions0])
                              (values (car v) (hasheq 'source (cadr v) 'checksum ""))))
                  h)]
           [h (hash-set h 'name new-name)]
           [h (hash-set h 'source source)]
           [h (hash-set h 'description description)]
           [h (if (hash-has-key? h 'date-added)
                  h
                  (hash-set h 'date-added (current-seconds)))]
           [h (hash-set h 'last-edit (current-seconds))])
      (package-info-set! new-name h)))
  (cond
    [(not (andmap valid-author? (or authors0 '())))
     (log! "package ~v/~v: some bad author" old-name new-name)
     #f]
    [(not (andmap valid-tag? (or tags0 '())))
     (log! "package ~v/~v: some bad tag" old-name new-name)
     #f]
    [(not (andmap valid-versions-list-entry? (or versions0 '())))
     (log! "package ~v/~v: some version list entry" old-name new-name)
     #f]
    [new-package?
     (cond
       [(or (package-exists-as new-name) (not (valid-name? new-name)))
        (log! "attempt to create package ~v failed" new-name)
        #f]
       [else
        (log! "creating package ~v" new-name)
        (do-save! (hasheq))
        (signal-update! (list new-name))
        #t])]
    [else
     (ensure-package-author old-name
                            (λ ()
                              (cond
                                [(not (equal? old-name (package-exists-as old-name)))
                                 (log! "case mismatch for package name ~s" old-name)
                                 #f]
                                [(equal? new-name old-name)
                                 (log! "updating package ~v" old-name)
                                 (do-save! (package-info old-name))
                                 (signal-update! (list new-name))
                                 #t]
                                [(and (valid-name? new-name) (not (package-exists-as new-name)))
                                 (log! "updating and renaming package ~v to ~v" old-name new-name)
                                 (do-save! (package-info old-name))
                                 (package-remove! old-name)
                                 (signal-update! (list new-name))
                                 #t]
                                [else
                                 (log! "attempt to rename package ~v to ~v failed" old-name new-name)
                                 #f])))]))

(define (curate-packages! package-name-strings proposed-new-ring)
  (cond
    [(and (curation-administrator? (current-user))
          (list? package-name-strings)
          (andmap string? package-name-strings)
          (integer? proposed-new-ring)
          (>= proposed-new-ring 0)
          (<= proposed-new-ring 2))
     (for ([pkg (in-list package-name-strings)])
       (define i (package-info pkg))
       (package-info-set! pkg (hash-set i 'ring proposed-new-ring)))
     (signal-static! package-name-strings)
     #t]
    [else #f]))

(define (package-author? p u)
  (define i (package-info p))
  (cond
    [(hash-has-key? i 'author) (member u (author->list (package-ref i 'author)))]
    [else
     (log! "WARNING: Package ~a is missing an author field" p)
     #f]))

(define (packages-of u)
  (filter (λ (p) (package-author? p u)) (package-list)))

;; Delete a package with authorization check.
;; Returns #t on success, #f if not authorized.
(define (delete-package!/authorized email pkg-name)
  (parameterize ([current-user email])
    (ensure-package-author pkg-name
                           (λ ()
                             (package-remove! pkg-name)
                             (signal-static! empty)
                             #t))))

;; Signal update for all of a user's packages.
(define (update-user-packages! email)
  (define user-packages (packages-of email))
  (log! "Packages of ~a: ~v" email user-packages)
  (signal-update! user-packages)
  #t)
